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

def process_radius(rlist_i, exceed3d, nm_use, isize, jsize, pr1, pr2, alpha, dx):
    """Process a single radius from rlist without redundant allocations."""
    r = rlist_i
    footprint = get_footprint(r, dx)
    footprint_sum = float(np.sum(footprint))

    # Vectorized convolution for all members at once
    frac = np.zeros((nm_use, isize, jsize), dtype=np.float32)
    for mem in range(nm_use):
        frac[mem] = np.around(signal.fftconvolve(exceed3d[mem], footprint, mode='same')) / footprint_sum

    # Broadcast calculations across all member pairs simultaneously
    f1 = frac[pr1]  # Shape: (num_pairs, isize, jsize)
    f2 = frac[pr2]  # Shape: (num_pairs, isize, jsize)

    mask = (f1 > 0) & (f2 > 0)
    diff_sq = (f1 - f2) ** 2
    sum_sq = np.maximum(f1 ** 2 + f2 ** 2, 1e-10)

    ratio = np.ones_like(diff_sq)
    np.putmask(ratio, mask, diff_sq / sum_sq)

    dijmean = np.mean(ratio, axis=0)
    meets_criteria = dijmean <= alpha

    return r, meets_criteria

def calculate_eas_probability_para(ensemble_qpf, t, rlist, alpha, dx, p_smooth, n_processes=None):
    """Optimized parallel EAS probability calculation function."""
    exceed3d = (ensemble_qpf / 25.4 >= t).astype(np.float32)
    print('sum of exceed3d for t: ', t, ' is : ', np.sum(exceed3d))

    nm_use, isize, jsize = exceed3d.shape

    # Precompute pairwise combinations using NumPy indices
    pr1, pr2 = np.triu_indices(nm_use, k=1)

    optrad = np.full((isize, jsize), float(max(rlist)), dtype=np.float32)
    rlist_sorted = sorted(rlist, reverse=True)

    # Initial check on maximum radius footprint
    initial_footprint = get_footprint(rlist_sorted[0], dx)
    footprint_sum = float(np.sum(initial_footprint))
    frac_initial = np.zeros((nm_use, isize, jsize), dtype=np.float32)

    for mem in range(nm_use):
        frac_initial[mem] = np.around(signal.fftconvolve(exceed3d[mem], initial_footprint, mode='same')) / footprint_sum

    fracsum = np.sum(frac_initial, axis=0)

    # Parallel process pool setup
    if n_processes is None:
        n_processes = mp.cpu_count()
    n_processes = max(1, min(n_processes, len(rlist_sorted), mp.cpu_count()))

    if n_processes > 1:
        partial_process = partial(
            process_radius,
            exceed3d=exceed3d,
            nm_use=nm_use,
            isize=isize,
            jsize=jsize,
            pr1=pr1,
            pr2=pr2,
            alpha=alpha,
            dx=dx
        )
        with mp.Pool(processes=n_processes) as pool:
            results = pool.map(partial_process, rlist_sorted)

        for r, meets_criteria in results:
            optrad = np.where(meets_criteria, r, optrad)
    else:
        for r in rlist_sorted:
            r, meets_criteria = process_radius(r, exceed3d, nm_use, isize, jsize, pr1, pr2, alpha, dx)
            optrad = np.where(meets_criteria, r, optrad)

    if p_smooth > 0:
        optrad = ndimage.gaussian_filter(optrad, p_smooth)

    # Zero out dry areas
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
    os.system(f'{WGRIB2} {maskfile} -text mask.txt')
    undefmask, nx, ny = simplewgrib2('mask.txt')
    maskregion = np.where(undefmask > 9.0e20, -9999, undefmask)

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

#### READ IN QPF ####
    for itime in itimes[memcount:memcount + 2]:
        mfiles = memfiles[itime]

        def read_qpf_step(file_path, shour, fhour, out_txt='qpf.txt'):
            os.system(f'{WGRIB2} {file_path} -match "APCP:surface:{shour}-{fhour}" -end -text {out_txt}')
            data, _, _ = simplewgrib2(out_txt)
            if os.path.exists(out_txt):
                os.remove(out_txt)
            return data

        if qpf_interval in (6, 12, 24):
            fhour = fhours[memcount]
            shour = fhour - 3
            q1 = read_qpf_step(mfiles[0], shour, fhour)
            q2 = read_qpf_step(mfiles[1], shour, fhour + incr)
            qpf12 = q1 + q2
            if qpf_interval == 6:
                qpf[itime] = qpf12

        if qpf_interval in (12, 24):
            q3 = read_qpf_step(mfiles[2], shour, fhours[memcount] + incr * 2)
            q4 = read_qpf_step(mfiles[3], shour, fhours[memcount] + incr * 3)
            qpf34 = q3 + q4
            if qpf_interval == 12:
                qpf[itime] = qpf12 + qpf34

        if qpf_interval == 24:
            q5 = read_qpf_step(mfiles[4], shour, fhours[memcount] + incr * 4)
            q6 = read_qpf_step(mfiles[5], shour, fhours[memcount] + incr * 5)
            q7 = read_qpf_step(mfiles[6], shour, fhours[memcount] + incr * 6)
            q8 = read_qpf_step(mfiles[7], shour, fhours[memcount] + incr * 7)
            qpf[itime] = qpf12 + qpf34 + (q5 + q6) + (q7 + q8)

        if qpf_interval == 3:
            fhour = fhours[memcount]
            shour = fhour - 3
            qpf[itime] = read_qpf_step(mfiles[0], shour, fhour)

        if qpf_interval == 1:
            fhour = fhours[memcount]
            shour = fhour - 1
            qpf[itime] = read_qpf_step(mfiles[1], shour, fhour)

        if dom == 'ak' and maskregion is not None:
            qpf[itime] = np.where(maskregion == -9999, 0, qpf[itime])

        qpf[memcount] = qpf[itime]

        # Calculate exceedance per threshold & radius footprint
        for t in thresh_use:
            exceed = (qpf[itime] / 25.4 >= t).astype(np.float32)
            for size in rlist:
                if memcount == 0:
                    prob[t, size] = np.zeros((nlats, nlons), dtype=np.float32)
                fp = get_footprint(size, dx)
                prob[t, size] += signal.fftconvolve(exceed, fp, mode='same')

        memcount += 1

    latency = min_latency

# Final Probabilities execution
nm_use = len(itimes)
ensemble_qpf = np.zeros((nm_use, nlats, nlons), dtype=np.float32)
for mem in range(nm_use):
    ensemble_qpf[mem] = qpf[mem]

# Pre-compute footprint filters
sizes = [10, 25, 40, 55, 70, 85, 100]
filter_footprints = {sz: get_footprint(sz, dx) for sz in sizes}

for t in thresh_use:
    t3 = time.time()
    exceed3d = (ensemble_qpf / 25.4 >= t).astype(np.float32)
    exceed_sum = np.sum(exceed3d)

    optrad = calculate_eas_probability_para(ensemble_qpf, t, rlist, alpha, dx, p_smooth)
    t4 = time.time()
    pnt_prob = calculate_pnt_probability(ensemble_qpf, t, p_smooth)

    t8a = time.time()
    if exceed_sum > 0:
        probfinal, optrad = optimize_footprint_calculation(
            optrad, prob, t, nm_use, nlats, nlons, dx,
            filter_footprints
        )
    else:
        probfinal = np.zeros((nlats, nlons), dtype=np.float32)

    probfinal = ndimage.gaussian_filter(probfinal, 1)

    if dom == 'ak' and maskregion is not None:
        probfinal = np.where(maskregion == -9999, 0, probfinal)

    probfinal = np.clip(probfinal, 0.0, 100.0)

    probstr = str(t * 25.4)
    byte = int(m.ceil(t * 25.4 * 1000))
    byte45 = byte // 65536
    byte45rem = byte % 65536
    byte46 = byte45rem // 256
    byte47 = byte45rem % 256

    with FortranFile('record_out.bin', mode='w') as myfort:
        myfort.write_record(probfinal.astype(np.float32))

    string = f"0:0:d={wgribdate}:APCP:surface:{fhr_range} hour acc fcst:prob >{probstr}:"

    os.system(f'{WGRIB2} {template} -import_bin record_out.bin -set_metadata_str "{string}" -set_grib_type c3 -grib_out premod.grb')
    os.system(f'{WGRIB2} premod.grb -set_byte 4 14 136 -set_byte 4 12 197 -set_byte 4 17 0 -set_byte 4 24:35 0:0:0:0:0:255:0:0:0:0:0:0 -set_byte 4 36 {nm_use} -set_byte 4 38:42 0:0:0:0:0 -set_byte 4 43 3 -set_byte 4 44 0 -set_byte 4 45 {byte45} -set_byte 4 46 {byte46} -set_byte 4 47 {byte47} -set_byte 4 67 1 -append -set_grib_type c3 -grib_out {outfile}')

    if os.path.exists('record_out.bin'):
        os.remove('record_out.bin')
    if os.path.exists('premod.grb'):
        os.remove('premod.grb')

    print(f'Wrote {qpf_interval} PQPF to:{outfile} for {t} inch threshold')
