# Author: B Blake / J Carley (EMC), T Alcott / C Alexander / I Jankov (GSD)
# Optimized Version - Vectorized EAS PQPF Computation

import os, sys, time
import numpy as np
import math as m
from datetime import datetime, timedelta
from scipy import ndimage, signal
from scipy.io import FortranFile 
import multiprocessing as mp
from functools import partial

# CRITICAL: Prevent numpy/scipy from fighting for threads inside multiprocessing
os.environ["OMP_NUM_THREADS"] = "1"

#--------------------------------------------------------------------------------
#### FUNCTIONS AND ROUTINES ####

def simplewgrib2(txtfile):
    """Fast file parser using np.loadtxt."""
    with open(txtfile, 'r') as f:
        header = f.readline().split()
        if not header:
            raise ValueError(f"Empty grib txt file: {txtfile}")
        nx, ny = int(header[0]), int(header[1])
        data = np.loadtxt(f, dtype=np.float32).reshape((ny, nx))
        return data, nx, ny

def get_footprint(r, dx):
    """Compute structural element/footprint for a given radius."""
    r_idx = int(r / dx)
    size = r_idx * 2 + 1
    footprint = np.ones((size, size), dtype=int)
    footprint[int(m.ceil(r / dx)), int(m.ceil(r / dx))] = 0
    dist = ndimage.distance_transform_edt(footprint, sampling=[dx, dx])
    return np.where(dist <= r, 1, 0)

def process_radius(rlist_i, exceed3d, nm_use, isize, jsize, pr1, pr2, alpha, dx, active_mask=None):
    """
    Memory-efficient process_radius:
    Uses algebraic identity to avoid allocating giant 91-pair 3D arrays.
    """
    r = rlist_i
    footprint = get_footprint(r, dx)
    footprint_sum = float(np.sum(footprint))

    frac = np.zeros((nm_use, isize, jsize), dtype=np.float32)
    for mem in range(nm_use):
        frac[mem] = np.around(signal.fftconvolve(exceed3d[mem], footprint, mode='same')) / footprint_sum

    num_pairs = len(pr1)
    sim_sum = np.zeros((isize, jsize), dtype=np.float32)

    # Accumulate similarity in-place using only 2D slices (no 3D allocation)
    for m1, m2 in zip(pr1, pr2):
        f1 = frac[m1]
        f2 = frac[m2]
        pair_mask = (f1 > 0) & (f2 > 0)
        if active_mask is not None:
            pair_mask &= active_mask

        if not np.any(pair_mask):
            continue

        num = 2.0 * (f1 * f2)
        den = f1 * f1 + f2 * f2
        sim_sum += np.where(pair_mask, num / np.maximum(den, 1e-10), 0.0)

    # dijmean = 1.0 - mean(similarity)
    dijmean = 1.0 - (sim_sum / float(num_pairs))
    meets_criteria = dijmean <= alpha

    return r, meets_criteria


def calculate_eas_probability_para(ensemble_qpf, t, rlist, alpha, dx, p_smooth, pool=None):
    """
    Optimized EAS probability calculation:
    - Skips redundant max-radius convolution (Tweak B)
    - Uses an external persistent process pool (Tweak C)
    - Restricts pairwise math to points with active precipitation
    """
    t_mm = t * 25.4
    exceed3d = (ensemble_qpf >= t_mm).astype(np.float32)
    print(f'sum of exceed3d for t: {t} is : {np.sum(exceed3d)}')

    nm_use, isize, jsize = exceed3d.shape
    pr1, pr2 = np.triu_indices(nm_use, k=1)

    rlist_sorted = sorted(rlist, reverse=True)
    max_r = rlist_sorted[0]
    remaining_radii = rlist_sorted[1:]  # e.g., [85, 70, 55, 40, 25, 10]

    # --- TWEAK B: Convolve max radius ONCE for fracsum & optrad default ---
    initial_footprint = get_footprint(max_r, dx)
    footprint_sum = float(np.sum(initial_footprint))
    frac_initial = np.zeros((nm_use, isize, jsize), dtype=np.float32)

    for mem in range(nm_use):
        frac_initial[mem] = np.around(
            signal.fftconvolve(exceed3d[mem], initial_footprint, mode='same')
        ) / footprint_sum

    fracsum = np.sum(frac_initial, axis=0)
    active_mask = fracsum > 0  # Only compute similarity where precip is present

    # Initialize optrad to the max radius
    optrad = np.full((isize, jsize), float(max_r), dtype=np.float32)

    # --- TWEAK C: Use the persistent pool on remaining_radii ---
    partial_process = partial(
        process_radius,
        exceed3d=exceed3d,
        nm_use=nm_use,
        isize=isize,
        jsize=jsize,
        pr1=pr1,
        pr2=pr2,
        alpha=alpha,
        dx=dx,
        active_mask=active_mask
    )

    if pool is not None:
        # pool.map preserves order: results correspond to remaining_radii in descending order
        results = pool.map(partial_process, remaining_radii)
    else:
        results = [partial_process(r) for r in remaining_radii]

    # Update optrad: as smaller radii meet criteria, they overwrite larger ones
    for r, meets_criteria in results:
        optrad = np.where(meets_criteria, r, optrad)

    if p_smooth > 0:
        optrad = ndimage.gaussian_filter(optrad, p_smooth)

    # Zero out completely dry areas
    optrad = np.where(fracsum == 0, 0, optrad)

    return optrad

def get_footprint_flexi(r, i, j, nx, ny, dx):
    """Compute flexible footprint for boundary locations."""
    footprint = get_footprint(r, dx)
    rdx2 = int(r / dx)
    nx1, ny1 = nx - 1, ny - 1

    if i < rdx2:
        footprint[0:rdx2 - i, :] = 0
    if j < rdx2:
        footprint[:, 0:rdx2 - j] = 0
    if i > nx1 - rdx2:
        footprint[nx1 - rdx2 - i:, :] = 0
    if j > ny1 - rdx2:
        footprint[:, ny1 - rdx2 - j:] = 0

    return footprint

def optimize_footprint_calculation(optrad, prob, t, nm_use, nlats, nlons, dx, filter_footprints):
    """Vectorized replacement for the pixel-by-pixel footprint loop."""
    optrad_int = optrad.astype(int)
    valid_mask = optrad > 0

    probfinal = np.zeros((nlats, nlons), dtype=np.float32)

    # Threshold mapping
    bins = [2.5, 17.5, 32.5, 47.5, 62.5, 77.5, 92.5]
    sizes = [10, 25, 40, 55, 70, 85, 100]

    footprint_size_grid = np.select(
        [optrad_int < b for b in bins[1:]],
        sizes[:-1],
        default=100
    )

    rows, cols = np.ogrid[:nlats, :nlons]
    rdx_grid = (optrad_int / dx).astype(int)

    # Flag boundary points
    near_boundary = (
        (cols < rdx_grid) |
        (rows < rdx_grid) |
        (cols > nlons - 1 - rdx_grid) |
        (rows > nlats - 1 - rdx_grid)
    ) & valid_mask

    # Fast processing for domain interior points
    for sz in sizes:
        sz_mask = valid_mask & (~near_boundary) & (footprint_size_grid == sz)
        if not np.any(sz_mask):
            continue
        fp_sum = float(np.sum(filter_footprints[sz]))
        probfinal[sz_mask] = (100.0 * prob[t, sz][sz_mask]) / (fp_sum * nm_use)

    # Process boundary points individually
    boundary_indices = np.argwhere(near_boundary)
    for row, col in boundary_indices:
        rad = optrad_int[row, col]
        sz = footprint_size_grid[row, col]
        fp_orig = float(np.sum(filter_footprints[sz]))
        rdx = rdx_grid[row, col]

        fp_use = float(np.sum(get_footprint_flexi(rad, col, row, nlons, nlats, dx)))
        ratio = fp_use / fp_orig

        interior_col = (col > rdx) and (col < nlons - 1 - rdx)
        interior_row = (row > rdx) and (row < nlats - 1 - rdx)

        cond_col = (col < rdx) or (col > nlons - 1 - rdx)
        cond_row = (row < rdx) or (row > nlats - 1 - rdx)

        if ratio < 0.5:
            if cond_col and interior_row:
                fp_use = int(0.51 * fp_orig)
            elif cond_row and interior_col:
                fp_use = int(0.51 * fp_orig)

        if ratio < 0.25:
            fp_use = int(0.26 * fp_orig)

        probfinal[row, col] = 100.0 * prob[t, sz][row, col] / (fp_use * nm_use)

        if rad > 100:
            optrad[row, col] = 0

    probfinal = np.clip(probfinal, 0.0, 100.0)
    return probfinal, optrad

def calculate_pnt_probability(ensemble_qpf, t, p_smooth):
    """Point probability computation vectorized across ensemble members."""
    exceed3d = (ensemble_qpf / 25.4 >= t).astype(np.float32)
    p_smooth_loc = p_smooth + 2
    
    pnt_prob = np.mean(exceed3d, axis=0) * 100.0
    return ndimage.gaussian_filter(pnt_prob, p_smooth_loc)

#--------------------------------------------------------------------------------
#### REAL START OF SCRIPT ####

starttime = time.time()
print('Processing probabilistic QPF')

# Verify required environment variables
required_env = ["WGRIB2", "HOMErefs", "COMIN", "COMINhrrr", "COMOUT", "PDY", "cyc", "dom", "DATA"]
for env in required_env:
    if env not in os.environ:
        print(f"NEED TO DEFINE {env}")
        sys.exit(1)

WGRIB2 = os.environ["WGRIB2"]
HOMErefs = os.environ["HOMErefs"]
COMIN = os.environ["COMIN"]
COMINhrrr = os.environ["COMINhrrr"]
COMOUT = os.environ["COMOUT"]
PDY = os.environ["PDY"]
cyc = os.environ["cyc"]
dom = os.environ["dom"]
DATA = os.environ["DATA"]

sys.path.append(HOMErefs)
from eas_config import *

template = os.path.join(HOMErefs, f'fix/pqpf_rrfs{dom}template.grib2')
print('template file is: ', template)

fcst_hour = int(sys.argv[1])
qpf_interval = int(sys.argv[2])
start_hour = int(fcst_hour - qpf_interval)

os.chdir(DATA)

fhr_range = f"{start_hour}-{fcst_hour}"
slim = max(rlist)
alpha = 0.5

os.system(f'{WGRIB2} {template} -rpn rcl_lat -text lat.txt -rpn rcl_lon -text lon.txt')

lons, nx, ny = simplewgrib2('lon.txt')
lats, nx, ny = simplewgrib2('lat.txt')
nlats, nlons = lats.shape

maskregion = None
if dom == 'ak':
    maskfile = os.path.join(HOMErefs, 'fix/akrrfs_mask.grib2')
    tmp_mask_bin = f'./mask_{os.getpid()}.bin'
    os.system(f'{WGRIB2} {maskfile} -no_header -bin {tmp_mask_bin}')
    if os.path.exists(tmp_mask_bin) and os.path.getsize(tmp_mask_bin) > 0:
        undefmask = np.fromfile(tmp_mask_bin, dtype=np.float32).reshape((nlats, nlons))
        maskregion = np.where(undefmask > 9.0e20, -9999, undefmask)
        try:
            os.remove(tmp_mask_bin) 
        except OSError:
            pass
        print('Alaska maskregion defined via binary dump')
    else:
        raise RuntimeError(f"Failed to extract mask from {maskfile}")

os.makedirs(COMOUT, exist_ok=True)

if dom == 'conus':
    nm_use = 14
    members = ['rrfs01','rrfs02','rrfs03','rrfs04','rrfs05','rrfs06','hrrr']
elif dom == 'ak':
    nm_use = 14
    members = ['rrfs01','rrfs02','rrfs03','rrfs04','rrfs05','rrfs06','hrrrak']
else:
    nm_use = 12
    members = ['rrfs01','rrfs02','rrfs03','rrfs04','rrfs05','rrfs06']

cy, cm, cd, ch = int(PDY[0:4]), int(PDY[4:6]), int(PDY[6:8]), int(cyc[0:2])
d0 = datetime(cy, cm, cd, ch, 0)
starttime_dt = d0 + timedelta(hours=start_hour)
endtime = d0 + timedelta(hours=start_hour + qpf_interval)

memfiles = {}
itimes = []
fhours = []
latency = min_latency
stop = max_latency
wgribdate = PDY + cyc

if qpf_interval == 1:
    incr = 1
    thresh_use = pqpf_1h_thresh
    outbase = f'refs.t{cyc[0:2]}z.pqpf01_easfrac.f{start_hour+qpf_interval:02d}.{dom}.grib2'
elif qpf_interval == 3:
    incr = 3
    thresh_use = pqpf_3h_thresh
    outbase = f'refs.t{cyc[0:2]}z.pqpf03_easfrac.f{start_hour+qpf_interval:02d}.{dom}.grib2'
elif qpf_interval == 6:
    incr = 3
    thresh_use = pqpf_6h_thresh
    outbase = f'refs.t{cyc[0:2]}z.pqpf06_easfrac.f{start_hour+qpf_interval:02d}.{dom}.grib2'
elif qpf_interval == 12:
    incr = 3
    thresh_use = pqpf_12h_thresh
    outbase = f'refs.t{cyc[0:2]}z.pqpf12_easfrac.f{start_hour+qpf_interval:02d}.{dom}.grib2'
elif qpf_interval == 24:
    incr = 3
    thresh_use = pqpf_24h_thresh
    outbase = f'refs.t{cyc[0:2]}z.pqpf24_easfrac.f{start_hour+qpf_interval:02d}.{dom}.grib2'

outfile = os.path.join(DATA, outbase)
if os.path.exists(outfile):
    os.remove(outfile)

prob = {}
qpf = {}
memcount = 0

for mem in members:
    memname = mem[0:4]
    memnum = mem[4:6]

    while (len(itimes) < memcount + 2) and (latency <= stop) and (start_hour + qpf_interval + latency <= 60):
        itime = starttime_dt - timedelta(hours=start_hour + latency)
        itime_alt = starttime_dt - timedelta(hours=start_hour + latency + 6)

        if memname == 'rrfs':
            files = [f"{COMIN}.{itime:%Y%m%d}/{itime:%H}/preproc/fv3s.t{itime:%H}z.{dom}.m{memnum}.f{start_hour+latency+i*incr:02d}.grib2" for i in range(9)]
            files_alt = [f"{COMIN}.{itime_alt:%Y%m%d}/{itime_alt:%H}/preproc/fv3s.t{itime_alt:%H}z.{dom}.m{memnum}.f{start_hour+latency+i*incr+6:02d}.grib2" for i in range(9)]
        else:
            files = [f"{COMINhrrr}.{itime:%Y%m%d}/{itime:%H}/preproc/hrrr.t{itime:%H}z.{dom}.f{start_hour+latency+i*incr:02d}.grib2" for i in range(9)]
            files_alt = ['garb'] * 9

        alt_fhrinc = 6

        if qpf_interval == 1 and os.path.exists(files[1]):
            fhours.append(start_hour + incr + latency)
            itimes.append(itime)
            memfiles[itime] = files
        elif qpf_interval == 1 and os.path.exists(files_alt[1]):
            fhours.append(start_hour + incr + latency + alt_fhrinc)
            itimes.append(itime_alt)
            memfiles[itime_alt] = files_alt
        elif qpf_interval == 3 and os.path.exists(files[1]):
            fhours.append(start_hour + incr + latency)
            itimes.append(itime)
            memfiles[itime] = files[1:]
        elif qpf_interval == 3 and os.path.exists(files_alt[1]):
            fhours.append(start_hour + incr + latency + alt_fhrinc)
            itimes.append(itime_alt)
            memfiles[itime_alt] = files_alt[1:]
        elif qpf_interval == 6 and os.path.exists(files[2]):
            fhours.append(start_hour + incr + latency)
            itimes.append(itime)
            memfiles[itime] = files[1:]
        elif qpf_interval == 6 and os.path.exists(files_alt[2]):
            fhours.append(start_hour + incr + latency + alt_fhrinc)
            itimes.append(itime_alt)
            memfiles[itime_alt] = files_alt[1:]
        elif qpf_interval == 12 and os.path.exists(files[4]):
            fhours.append(start_hour + incr + latency)
            itimes.append(itime)
            memfiles[itime] = files[1:]
        elif qpf_interval == 12 and os.path.exists(files_alt[4]):
            fhours.append(start_hour + incr + latency + alt_fhrinc)
            itimes.append(itime_alt)
            memfiles[itime_alt] = files_alt[1:]
        elif qpf_interval == 24 and os.path.exists(files[8]):
            fhours.append(start_hour + incr + latency)
            itimes.append(itime)
            memfiles[itime] = files[1:]
        elif qpf_interval == 24 and os.path.exists(files_alt[8]):
            fhours.append(start_hour + incr + latency + alt_fhrinc)
            itimes.append(itime_alt)
            memfiles[itime_alt] = files_alt[1:]

        latency += 6

# before

#### READ IN QPF ####
    for itime in itimes[memcount:memcount + 2]:
        mfiles = memfiles[itime]

        # Use RAM disk (/dev/shm) for temporary files if available; fallback to DATA
        shm_dir = '/dev/shoom' if os.path.exists('/dev/shoom') and os.access('/dev/shoom', os.W_OK) else DATA

        def read_qpf_step(file_path, s_hr, f_hr, step_tag):
         """
         Fast binary reader:
         Uses wgrib2 -no_header -bin to dump raw IEEE 754 floats to shm_dir,
         then loads instantly via np.fromfile.
         """
         bin_filename = f'{shm_dir}/qpf_{step_tag}_{memcount}_{os.getpid()}.bin'

         # Dump raw single-precision IEEE binary floats (no text formatting)
         cmd = f'{WGRIB2} {file_path} -match "APCP:surface:{s_hr}-{f_hr}" -end -no_header -bin {bin_filename}'
         os.system(cmd)

         if not os.path.exists(bin_filename) or os.path.getsize(bin_filename) == 0:
           print(f"WARNING: wgrib2 produced no data for {file_path} with APCP:surface:{s_hr}-{f_hr}")
           data = np.zeros((nlats, nlons), dtype=np.float32)
         else:
           # np.fromfile reads raw float32 bytes in ~10 milliseconds
           data = np.fromfile(bin_filename, dtype=np.float32).reshape((nlats, nlons))
           # Zero out wgrib2 undefined value (9.999e20)
           data = np.where(data > 9.0e20, 0, data)
           try:
             os.remove(bin_filename)
           except OSError:
             pass

         return data


        base_fhr = fhours[memcount]

        if qpf_interval in (6, 12, 24):
            # First 6 hours (two 3-hr blocks)
            q1 = read_qpf_step(mfiles[0], base_fhr - 3, base_fhr, 'q1')
            q2 = read_qpf_step(mfiles[1], base_fhr, base_fhr + incr, 'q2')
            qpf12 = q1 + q2
            if qpf_interval == 6:
                qpf[itime] = qpf12

        if qpf_interval in (12, 24):
            # Second 6 hours (two 3-hr blocks)
            q3 = read_qpf_step(mfiles[2], base_fhr + incr, base_fhr + incr * 2, 'q3')
            q4 = read_qpf_step(mfiles[3], base_fhr + incr * 2, base_fhr + incr * 3, 'q4')
            qpf34 = q3 + q4
            if qpf_interval == 12:
                qpf[itime] = qpf12 + qpf34

        if qpf_interval == 24:
            # Third and fourth 6-hr blocks
            q5 = read_qpf_step(mfiles[4], base_fhr + incr * 3, base_fhr + incr * 4, 'q5')
            q6 = read_qpf_step(mfiles[5], base_fhr + incr * 4, base_fhr + incr * 5, 'q6')
            q7 = read_qpf_step(mfiles[6], base_fhr + incr * 5, base_fhr + incr * 6, 'q7')
            q8 = read_qpf_step(mfiles[7], base_fhr + incr * 6, base_fhr + incr * 7, 'q8')
            qpf[itime] = qpf12 + qpf34 + (q5 + q6) + (q7 + q8)

        if qpf_interval == 3:
            qpf[itime] = read_qpf_step(mfiles[0], base_fhr - 3, base_fhr, 'q3h')

        if qpf_interval == 1:
            qpf[itime] = read_qpf_step(mfiles[1], base_fhr - 1, base_fhr, 'q1h')

        if dom == 'ak' and maskregion is not None:
            qpf[itime] = np.where(maskregion == -9999, 0, qpf[itime])

        qpf[memcount] = qpf[itime]


        memcount += 1

    latency = min_latency

# Final Probabilities execution
nm_use = len(itimes)
ensemble_qpf = np.zeros((nm_use, nlats, nlons), dtype=np.float32)
for mem in range(nm_use):
    ensemble_qpf[mem] = qpf[mem]

    # Pre-compute footprint filters once
sizes = [10, 25, 40, 55, 70, 85, 100]
filter_footprints = {sz: get_footprint(sz, dx) for sz in sizes}

# Cap workers: remaining_radii has 6 tasks, so 4 to 6 workers is optimal
n_workers = min(4, len(rlist) - 1, mp.cpu_count())
print(f"Starting persistent process pool with {n_workers} workers...")

with mp.Pool(processes=n_workers) as pool:
    for t in thresh_use:
        t3 = time.time()
        t_mm = t * 25.4
        exceed3d = (ensemble_qpf >= t_mm).astype(np.float32)
        exceed_sum = np.sum(exceed3d)

        # Pre-sum exceedance across all members (cuts 784 convolutions down to 56)
        sum_exceed_members = np.sum(exceed3d, axis=0)
        for sz in sizes:
            if exceed_sum > 0:
                prob[t, sz] = signal.fftconvolve(sum_exceed_members, filter_footprints[sz], mode='same')
            else:
                prob[t, sz] = np.zeros((nlats, nlons), dtype=np.float32)

        # Skip all heavy computation if there is no rain anywhere in domain
        if exceed_sum > 0:
            optrad = calculate_eas_probability_para(
                ensemble_qpf, t, rlist, alpha, dx, p_smooth, pool=pool
            )
            t4 = time.time()
            print(f'Time for optrad routine: {t4 - t3:.2f}s')

            pnt_prob = calculate_pnt_probability(ensemble_qpf, t, p_smooth)

            probfinal, optrad = optimize_footprint_calculation(
                optrad, prob, t, nm_use, nlats, nlons, dx,
                filter_footprints
            )
        else:
            t4 = time.time()
            print(f'Time for optrad routine (bypassed, dry domain): {t4 - t3:.4f}s')
            optrad = np.zeros((nlats, nlons), dtype=np.float32)
            probfinal = np.zeros((nlats, nlons), dtype=np.float32)

        # Smoothing and masking
        probfinal = ndimage.gaussian_filter(probfinal, 1)

        if dom == 'ak' and maskregion is not None:
            probfinal = np.where(maskregion == -9999, 0, probfinal)

        probfinal = np.clip(probfinal, 0.0, 100.0)

        t5 = time.time()
        print(f'Time for get final probability routine for {t} inch threshold: {t5 - t4:.2f}s')

        # --- GRIB2 writing section continues as normal below ---
        probstr = str(t * 25.4)
        byte = int(m.ceil(t * 25.4 * 1000))
        byte45 = byte // 65536
        byte45rem = byte % 65536
        byte46 = byte45rem // 256
        byte47 = byte45rem % 256


        tmp_rec = f'{shm_dir}/record_out_{os.getpid()}.bin'
        tmp_grb = f'{shm_dir}/premod_{os.getpid()}.grb'

        with FortranFile(tmp_rec, mode='w') as myfort:
            myfort.write_record(probfinal.astype(np.float32))

        string = f"0:0:d={wgribdate}:APCP:surface:{fhr_range} hour acc fcst:prob >{probstr}:"

        os.system(f'{WGRIB2} {template} -import_bin {tmp_rec} -set_metadata_str "{string}" -set_grib_type c3 -grib_out {tmp_grb}')

        os.system(f'{WGRIB2}  {tmp_grb} -set_byte 4 14 136 -set_byte 4 12 197 -set_byte 4 17 0 -set_byte 4 24:35 0:0:0:0:0:255:0:0:0:0:0:0 -set_byte 4 36 {nm_use} -set_byte 4 38:42 0:0:0:0:0 -set_byte 4 43 3 -set_byte 4 44 0 -set_byte 4 45 {byte45} -set_byte 4 46 {byte46} -set_byte 4 47 {byte47} -set_byte 4 67 1 -append -set_grib_type c3 -grib_out {outfile}')

        if os.path.exists(tmp_rec):
            os.remove(tmp_rec)
        if os.path.exists(tmp_grb):
            os.remove(tmp_grb)

        print(f'Wrote {qpf_interval} PQPF to:{outfile} for {t} inch threshold')
