# Author: B Blake / J Carley (EMC), T Alcott / C Alexander / I Jankov (GSD)
# Script to calculate and plot 6-h HREFv2 PQPF using the EAS Fractional Coverage method
# Optimized Version - Fast Binary I/O & Vectorized Computation

import os, sys, time
import numpy as np
import math as m
from datetime import datetime, timedelta
from scipy import ndimage, signal
from scipy.io import FortranFile
import multiprocessing as mp
from functools import partial

# Prevent numpy/scipy from fighting for threads inside multiprocessing
os.environ["OMP_NUM_THREADS"] = "1"

# Identify RAM disk (/dev/shm) if writable; fallback to DATA if unavailable
shm_dir = '/dev/shoom' if os.path.exists('/dev/shoom') and os.access('/dev/shoom', os.W_OK) else None
#--------------------------------------------------------------------------------
#### FUNCTIONS AND ROUTINES ####

def simplewgrib2(txtfile):
    """Fast file parser using np.loadtxt for coordinate and mask text files."""
    with open(txtfile, 'r') as f:
        header = f.readline().split()
        if not header:
            raise ValueError(f"Empty grib txt file: {txtfile}")
        nx, ny = int(header[0]), int(header[1])
        array2d = np.loadtxt(f, dtype=np.float32).reshape((ny, nx))
        return array2d, nx, ny

def read_asnow_step(file_path, s_hr, f_hr, step_tag, memcount, nlats, nlons, wgrib2_cmd, tmp_dir):
    """
    Fast binary reader:
    Dumps raw single-precision IEEE 754 floats to RAM via wgrib2 -no_header -bin,
    then loads instantly via np.fromfile in ~10 milliseconds.
    """
    bin_filename = f'{tmp_dir}/asnow_{step_tag}_{memcount}_{os.getpid()}.bin'
    cmd = f'{wgrib2_cmd} {file_path} -match "ASNOW:surface:{s_hr}-{f_hr}" -end -no_header -bin {bin_filename}'
    os.system(cmd)

    if not os.path.exists(bin_filename) or os.path.getsize(bin_filename) == 0:
        print(f"WARNING: wgrib2 produced no data for {file_path} with ASNOW:surface:{s_hr}-{f_hr}")
        return np.zeros((nlats, nlons), dtype=np.float32)

    data = np.fromfile(bin_filename, dtype=np.float32).reshape((nlats, nlons))
    # Undefined values in wgrib2 are 9.999e20
    data = np.where(data > 9.0e20, 0, data)

    try:
        os.remove(bin_filename)
    except OSError:
        pass

    return data

def get_footprint(r, dx):
    """Calculate circular footprint."""
    r_idx = int(r / dx)
    size = r_idx * 2 + 1
    footprint = np.ones((size, size), dtype=int)
    footprint[int(m.ceil(r / dx)), int(m.ceil(r / dx))] = 0
    dist = ndimage.distance_transform_edt(footprint, sampling=[dx, dx])
    return np.where(dist <= r, 1, 0)

def get_footprint_flexi(r, i, j, nx, ny, dx):
    """Calculate footprint truncated near domain boundaries."""
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

def process_radius(rlist_i, exceed3d, nm_use, isize, jsize, pr1, pr2, alpha, dx, active_mask=None):
    """
    Memory-lean similarity routine:
    Uses algebraic identity [dij = 1 - 2*f1*f2 / (f1^2 + f2^2)] in a 2D accumulator.
    """
    r = rlist_i
    footprint = get_footprint(r, dx)
    footprint_sum = float(np.sum(footprint))

    frac = np.zeros((nm_use, isize, jsize), dtype=np.float32)
    for mem in range(nm_use):
        frac[mem] = np.around(signal.fftconvolve(exceed3d[mem], footprint, mode='same')) / footprint_sum

    num_pairs = len(pr1)
    sim_sum = np.zeros((isize, jsize), dtype=np.float32)

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

    dijmean = 1.0 - (sim_sum / float(num_pairs))
    meets_criteria = dijmean <= alpha

    return r, meets_criteria

def calculate_eas_probability_para(ensemble_qpf, t, rlist, alpha, dx, p_smooth, pool=None):
    """
    Optimized EAS probability calculation:
    - Pre-calculates 100 km radius once for default & active precip mask
    - Dispatches remaining 6 radii to worker pool
    """
    exceed3d = (ensemble_qpf >= t).astype(np.float32)
    print(f'sum of exceed3d for t: {t} is : {np.sum(exceed3d)}')

    nm_use, isize, jsize = exceed3d.shape
    pr1, pr2 = np.triu_indices(nm_use, k=1)

    rlist_sorted = sorted(rlist, reverse=True)
    max_r = rlist_sorted[0]
    remaining_radii = rlist_sorted[1:]

    # Convolve max radius ONCE to determine active footprint
    initial_footprint = get_footprint(max_r, dx)
    footprint_sum = float(np.sum(initial_footprint))
    frac_initial = np.zeros((nm_use, isize, jsize), dtype=np.float32)

    for mem in range(nm_use):
        frac_initial[mem] = np.around(
            signal.fftconvolve(exceed3d[mem], initial_footprint, mode='same')
        ) / footprint_sum

    fracsum = np.sum(frac_initial, axis=0)
    active_mask = fracsum > 0

    optrad = np.full((isize, jsize), float(max_r), dtype=np.float32)

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
        results = pool.map(partial_process, remaining_radii)
    else:
        results = [partial_process(r) for r in remaining_radii]

    for r, meets_criteria in results:
        optrad = np.where(meets_criteria, r, optrad)

    if p_smooth > 0:
        optrad = ndimage.gaussian_filter(optrad, p_smooth)

    optrad = np.where(fracsum == 0, 0, optrad)
    return optrad

def optimize_footprint_calculation(optrad, prob, t, nm_use, nlats, nlons, dx, filter_footprints):
    """Vectorized calculation of probability grid with boundary-only fallback."""
    optrad_int = optrad.astype(int)
    valid_mask = optrad > 0

    probfinal = np.zeros((nlats, nlons), dtype=np.float32)

    bins = [2.5, 17.5, 32.5, 47.5, 62.5, 77.5, 92.5]
    sizes = [10, 25, 40, 55, 70, 85, 100]

    footprint_size_grid = np.select(
        [optrad_int < b for b in bins[1:]],
        sizes[:-1],
        default=100
    )

    rows, cols = np.ogrid[:nlats, :nlons]
    rdx_grid = (optrad_int / dx).astype(int)

    near_boundary = (
        (cols < rdx_grid) |
        (rows < rdx_grid) |
        (cols > nlons - 1 - rdx_grid) |
        (rows > nlats - 1 - rdx_grid)
    ) & valid_mask

    # Process standard interior points fully vectorized
    for sz in sizes:
        sz_mask = valid_mask & (~near_boundary) & (footprint_size_grid == sz)
        if not np.any(sz_mask):
            continue
        fp_sum = float(np.sum(filter_footprints[sz]))
        probfinal[sz_mask] = (100.0 * prob[t, sz][sz_mask]) / (fp_sum * nm_use)

    # Process boundary points
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
    """Vectorized ensemble-mean point probability."""
    exceed3d = (ensemble_qpf >= t).astype(np.float32)
    pnt_prob = np.mean(exceed3d, axis=0) * 100.0
    return ndimage.gaussian_filter(pnt_prob, p_smooth + 2)

#--------------------------------------------------------------------------------
#### REAL START OF SCRIPT ####

starttime = time.time()
print("Processing probabilistic snow")

# Validate required environment variables
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

if shm_dir is None:
    shm_dir = DATA

sys.path.append(HOMErefs)
from eas_config import *

template = os.path.join(HOMErefs, f'fix/asnow_rrfs{dom}template.grib2')
print('template file is: ', template)

fcst_hour = int(sys.argv[1])
qpf_interval = int(sys.argv[2])
start_hour = int(fcst_hour - qpf_interval)

os.chdir(DATA)

fhr_range = f"{start_hour}-{fcst_hour}"
slim = max(rlist)
alpha = 0.5

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

# Grid coordinates
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

if qpf_interval == 1:
    outbase = f'refs.t{cyc[0:2]}z.snow01_easfrac.f{start_hour+qpf_interval:02d}.{dom}.grib2'
    incr = 1
    thresh_use = snow_1h_thresh
elif qpf_interval == 3:
    outbase = f'refs.t{cyc[0:2]}z.snow03_easfrac.f{start_hour+qpf_interval:02d}.{dom}.grib2'
    incr = 3
    thresh_use = snow_3h_thresh
elif qpf_interval == 6:
    outbase = f'refs.t{cyc[0:2]}z.snow06_easfrac.f{start_hour+qpf_interval:02d}.{dom}.grib2'
    incr = 3
    thresh_use = snow_6h_thresh

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
            file3 = f"{COMIN}.{itime:%Y%m%d}/{itime:%H}/preproc/fv3s.t{itime:%H}z.{dom}.m{memnum}.f{start_hour+latency+incr:02d}.grib2"
            file6 = f"{COMIN}.{itime:%Y%m%d}/{itime:%H}/preproc/fv3s.t{itime:%H}z.{dom}.m{memnum}.f{start_hour+latency+2*incr:02d}.grib2"
            file3alt = f"{COMIN}.{itime_alt:%Y%m%d}/{itime_alt:%H}/preproc/fv3s.t{itime_alt:%H}z.{dom}.m{memnum}.f{start_hour+latency+incr+6:02d}.grib2"
            file6alt = f"{COMIN}.{itime_alt:%Y%m%d}/{itime_alt:%H}/preproc/fv3s.t{itime_alt:%H}z.{dom}.m{memnum}.f{start_hour+latency+2*incr+6:02d}.grib2"
        else:
            file3 = f"{COMINhrrr}.{itime:%Y%m%d}/{itime:%H}/preproc/hrrr.t{itime:%H}z.{dom}.f{start_hour+latency+incr:02d}.grib2"
            file6 = f"{COMINhrrr}.{itime:%Y%m%d}/{itime:%H}/preproc/hrrr.t{itime:%H}z.{dom}.f{start_hour+latency+2*incr:02d}.grib2"
            file3alt = 'garb'
            file6alt = 'garb'

        if qpf_interval != 6:
            if os.path.exists(file3):
                fhours.append(start_hour + qpf_interval + latency)
                itimes.append(itime)
                memfiles[itime] = file3
            elif os.path.exists(file3alt):
                fhours.append(start_hour + qpf_interval + latency + 6)
                itimes.append(itime_alt)
                memfiles[itime_alt] = file3alt
        else:
            if os.path.exists(file3) and os.path.exists(file6):
                itimes.append(itime)
                fhours.append(start_hour + qpf_interval + latency)
                memfiles[itime] = [file3, file6]
            elif os.path.exists(file3alt) and os.path.exists(file6alt):
                itimes.append(itime_alt)
                fhours.append(start_hour + qpf_interval + latency + 6)
                memfiles[itime_alt] = [file3alt, file6alt]

        latency += 6

#### READ IN SNOW ####
    for itime in itimes[memcount:memcount + 2]:
        base_fhr = fhours[memcount]

        if qpf_interval == 6:
            file3, file6 = memfiles[itime]
            # Two consecutive 3-hr blocks
            q3 = read_asnow_step(file3, base_fhr - 6, base_fhr - 3, 's3', memcount, nlats, nlons, WGRIB2, shm_dir)
            q6 = read_asnow_step(file6, base_fhr - 3, base_fhr, 's6', memcount, nlats, nlons, WGRIB2, shm_dir)
            qpf[itime] = (q3 + q6) * 39.370079  # meters to inches
        elif qpf_interval == 3:
            file3 = memfiles[itime]
            q3 = read_asnow_step(file3, base_fhr - 3, base_fhr, 's3h', memcount, nlats, nlons, WGRIB2, shm_dir)
            qpf[itime] = q3 * 39.370079
        elif qpf_interval == 1:
            file3 = memfiles[itime]
            q1 = read_asnow_step(file3, base_fhr - 1, base_fhr, 's1h', memcount, nlats, nlons, WGRIB2, shm_dir)
            qpf[itime] = q1 * 39.370079

        if dom == 'ak' and maskregion is not None:
            qpf[itime] = np.where(maskregion == -9999, 0, qpf[itime])

        qpf[memcount] = qpf[itime]
        memcount += 1

    latency = min_latency

# Final Probabilities Setup
nm_use = len(itimes)
ensemble_qpf = np.zeros((nm_use, nlats, nlons), dtype=np.float32)
for mem in range(nm_use):
    ensemble_qpf[mem] = qpf[mem]

# Precompute circular footprint kernels
sizes = [10, 25, 40, 55, 70, 85, 100]
filter_footprints = {sz: get_footprint(sz, dx) for sz in sizes}

# Cap worker processes: remaining_radii has 6 tasks, so up to 6 workers is optimal
n_workers = max(1, min(4, len(rlist) - 1, mp.cpu_count()))
print(f"Starting persistent worker pool with {n_workers} processes...")

with mp.Pool(processes=n_workers) as pool:
    for t in thresh_use:
        t3 = time.time()
        exceed3d = (ensemble_qpf >= t).astype(np.float32)
        exceed_sum = np.sum(exceed3d)

        # Pre-sum across ensemble members: collapses 14 convolutions per radius to 1
        sum_exceed_members = np.sum(exceed3d, axis=0)
        for sz in sizes:
            if exceed_sum > 0:
                prob[t, sz] = signal.fftconvolve(sum_exceed_members, filter_footprints[sz], mode='same')
            else:
                prob[t, sz] = np.zeros((nlats, nlons), dtype=np.float32)

        # Bypass all heavy calculations if there is zero snow in domain
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
            print(f'Time for optrad routine (bypassed, zero snow): {t4 - t3:.4f}s')
            optrad = np.zeros((nlats, nlons), dtype=np.float32)
            probfinal = np.zeros((nlats, nlons), dtype=np.float32)

        probfinal = ndimage.gaussian_filter(probfinal, 1)

        if dom == 'ak' and maskregion is not None:
            probfinal = np.where(maskregion == -9999, 0, probfinal)

        probfinal = np.clip(probfinal, 0.0, 100.0)

        t5 = time.time()
        print(f'Time for get final probability routine for {t} inch threshold: {t5 - t4:.2f}s')

        # --- GRIB2 Export ---
        probstr = str(t * 0.0254)
        byte = int(m.floor(0.5 + t * 25.4))
        byte45 = byte // 65536
        byte45rem = byte % 65536
        byte46 = byte45rem // 256
        byte47 = byte45rem % 256

        tmp_rec = f'{shm_dir}/record_out_{os.getpid()}.bin'
        tmp_grb = f'{shm_dir}/premod_{os.getpid()}.grb'

        with FortranFile(tmp_rec, mode='w') as myfort:
            myfort.write_record(probfinal.astype(np.float32))

        string = f"0:0:d={wgribdate}:ASNOW:surface:{fhr_range} hour acc fcst:prob >{probstr}:"

        os.system(f'{WGRIB2} {template} -import_bin {tmp_rec} -set_metadata_str "{string}" -set_grib_type c3 -grib_out {tmp_grb}')
        os.system(f'{WGRIB2} {tmp_grb} -set_byte 4 14 136 -set_byte 4 12 197 -set_byte 4 17 0 -set_byte 4 24:35 0:0:0:0:0:255:0:0:0:0:0:0 -set_byte 4 36 {nm_use} -set_byte 4 38:42 0:0:0:0:0 -set_byte 4 43 3 -set_byte 4 44 0 -set_byte 4 45 {byte45} -set_byte 4 46 {byte46} -set_byte 4 47 {byte47} -set_byte 4 67 1 -append -set_grib_type c3 -grib_out {outfile}')

        if os.path.exists(tmp_rec):
            os.remove(tmp_rec)
        if os.path.exists(tmp_grb):
            os.remove(tmp_grb)

        print(f'Wrote {qpf_interval} PSNOW to: {outfile} for {t} inch threshold')
