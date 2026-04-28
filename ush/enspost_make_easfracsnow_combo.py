# Author: B Blake / J Carley (EMC), T Alcott / C Alexander / I Jankov (GSD)
#
# Script to calculate and plot 6-h HREFv2 PQPF using the EAS Fractional Coverage method
# Apply variable radii to probability field
#
# Script History Log:
# 2017-03-02  B Blake   new script for HREFv2
# 2017-03-22  B Blake   6-hr time lagged NAM member added
# 2017-05-31  T Alcott  calculate_eas_probability function added
#                       computational resources reduced from 16 cores to 1 core
# 2017-06-01  B Blake   modified script containing loops over each ensemble member type
# 2018-01-05  M Pyle    3-hr only version, no bias correction
# 2018-01-18  M Pyle    Make a version for weasd
# 2018-02-08  M Pyle    Attempt to unify all EAS snow scripts into one
# 2018-03-21  M Pyle    replace pygrib with wgrib2
# 2021-06     M Pyle    python3 changes
# 2024-03     M Pyle    WEASD --> ASNOW

import os, sys, time
import numpy as np
import math as m
from datetime import datetime, timedelta
from scipy import ndimage, optimize, signal
from scipy.io import FortranFile 
import multiprocessing as mp
from functools import partial

# CRITICAL: Prevent numpy/scipy from fighting for threads inside multiprocessing
os.environ["OMP_NUM_THREADS"] = "1"

#--------------------------------------------------------------------------------
#### FUNCTIONS AND ROUTINES ####

def simplewgrib2(txtfile):
  tmps= []
  with open(txtfile) as F1:
    i=1
    nx,ny=[int(x) for x in next(F1).split()]
    ilim=nx*ny
    while i <= ilim:
      tmp=[float(x) for x in next(F1).split()]
      tmps.append(tmp)
      i=i+1
    array2d = np.asarray(tmps,dtype=np.float32)
    array2d.shape = (ny,nx)
    return array2d,nx,ny
  F1.close()

# calculate footprint routine
def get_footprint(r):
#    footprint = np.ones(((r/dx)*2+1,(r/dx)*2+1),dtype=int)
    footprint = np.ones((int((r/dx)*2+1),int((r/dx)*2+1)),dtype=int)
    footprint[int(m.ceil(r/dx)),int(m.ceil(r/dx))] = 0
    dist = ndimage.distance_transform_edt(footprint,sampling=[dx,dx])
    footprint = np.where(np.greater(dist,r),0,1)
    return footprint

def process_radius(rlist_i, exceed3d, nm_use, isize, jsize, pr1, pr2, alpha, fracsum):
    """
    Process a single radius from rlist.
    """
    r = rlist_i
    dcrit = alpha
    footprint = get_footprint(r)

    # Pre-calculate the denominator for the footprint sum
    footprint_sum = float(np.sum(footprint))

    # Pre-allocate arrays
    frac = np.zeros((nm_use, isize, jsize), dtype=float)
    dijmean = np.zeros((isize, jsize), dtype=float)

    # Vectorized convolution for all members at once
    for mem in range(nm_use):
        frac[mem] = np.around(signal.fftconvolve(exceed3d[mem], footprint, mode='same')) / footprint_sum

    # Process pairs efficiently
    for pair in range(len(pr1)):
        m1, m2 = pr1[pair], pr2[pair]
        mask = np.logical_and(frac[m1] > 0, frac[m2] > 0)

        # Only compute where mask is True
        diff_squared = np.zeros((isize, jsize), dtype=float)
        sum_squared = np.zeros((isize, jsize), dtype=float)

        np.putmask(diff_squared, mask, (frac[m1] - frac[m2])**2)
        np.putmask(sum_squared, mask, (frac[m1]**2 + frac[m2]**2))

        # Avoid division by zero
        ratio = np.ones((isize, jsize), dtype=float)
        np.putmask(ratio, mask, diff_squared / np.maximum(sum_squared, 1e-10))

        dijmean += ratio / float(len(pr1))

    # Check which points meet the similarity criteria
    meets_criteria = np.less_equal(dijmean, dcrit)

    return r, meets_criteria


def calculate_eas_probability_para(ensemble_qpf, t, rlist, alpha, dx, p_smooth, n_processes=None):
    """
    Optimized version of the EAS probability calculation function.

    Parameters:
    -----------
    ensemble_qpf: 3-D QPF array (members x lat x lon)
    t: QPF threshold in inches
    rlist: list of potential EAS radii (km)
    alpha: similarity criteria parameter
    dx: model grid spacing (km)
    p_smooth: width of Gaussian filter for smoothing radius field (grid points)
    n_processes: number of processes to use for parallel computation (default: CPU count)

    Returns:
    --------
    optrad: Optimized radius array
    """
    # Convert inches to mm
    exceed3d = np.where(np.greater_equal(ensemble_qpf, t), 1, 0)
    print('sum of exceed3d for t: ', t, ' is : ', np.sum(exceed3d))

    nm_use, isize, jsize = np.shape(exceed3d)
    print('nm_use within get_footprint: ', nm_use)
    print('isize, jsize within get_footprint: ', isize, jsize)

    # Create lists of ensemble member pairs - do this once
    pr1, pr2 = [], []
    for m1 in range(nm_use-1):
        for m2 in range(m1+1, nm_use):
            pr1.append(m1)
            pr2.append(m2)

    # Initialize with maximum radius
    optrad = np.full((isize, jsize), float(max(rlist)), dtype=float)

    # Sort radii from largest to smallest
    rlist = sorted(rlist, reverse=True)

    # Calculate initial fractions for the largest radius
    initial_footprint = get_footprint(rlist[0])
    footprint_sum = float(np.sum(initial_footprint))
    frac_initial = np.zeros((nm_use, isize, jsize), dtype=float)

    for mem in range(nm_use):
        frac_initial[mem] = np.around(signal.fftconvolve(exceed3d[mem], initial_footprint, mode='same')) / footprint_sum

    # Identify points where QPF threshold is not met within the maximum radius in any ensemble member
    fracsum = np.sum(frac_initial, axis=0)

    # Set up parallel processing
    if n_processes is None:
        n_processes = mp.cpu_count()

    # Use at most the number of radii or CPUs available
    n_processes = min(n_processes, len(rlist), mp.cpu_count())

    if n_processes > 1:
        print(f"Using {n_processes} processes for parallel computation")

        # Create a partial function with fixed arguments

        partial_process = partial(
            process_radius,
            exceed3d=exceed3d,
            nm_use=nm_use,
            isize=isize,
            jsize=jsize,
            pr1=pr1,
            pr2=pr2,
            alpha=alpha,
            fracsum=fracsum
        )

        # Process each radius in parallel
        with mp.Pool(processes=n_processes) as pool:
            results = pool.map(partial_process, rlist)

        # Update optrad based on results
        for r, meets_criteria in results:
            optrad = np.where(meets_criteria, r, optrad)
    else:
        # Sequential processing
        for r in rlist:
            r, meets_criteria = process_radius(
                r, exceed3d, nm_use, isize, jsize, pr1, pr2, alpha, fracsum
            )
            optrad = np.where(meets_criteria, r, optrad)

    # Smooth radius grid and zero out any dry areas
    if p_smooth > 0:
        optrad = ndimage.gaussian_filter(optrad, p_smooth)

    # Zero out dry areas
    optrad = np.where(np.equal(fracsum, 0), 0, optrad)

    return optrad


def get_footprint_flexi(r,i,j,nx,ny):

    footprint = np.ones((int((r/dx)*2+1),int((r/dx)*2+1)),dtype=int)
    footprint[int(m.ceil(r/dx)),int(m.ceil(r/dx))] = 0
    dist = ndimage.distance_transform_edt(footprint,sampling=[dx,dx])
    footprint = np.where(np.greater(dist,r),0,1)

    rdx2 = int(r/dx)
    nx1=nx-1
    ny1=ny-1

# if test is on domain grid indices.  Indices for footprint_flexi are footprint relative

    if i < rdx2:
      footprint[0:rdx2-i,:]=0
    if j < rdx2:
      footprint[:,0:rdx2-j]=0
    if i > nx1-rdx2:
      footprint[nx1-rdx2-i:,:]=0
    if j > ny1-rdx2:
      footprint[:,ny1-rdx2-j:]=0

    return footprint


def optimize_footprint_calculation(optrad, prob, t, nm_use, nlats, nlons, dx,
                                  filter_footprint_10, filter_footprint_25,
                                  filter_footprint_40, filter_footprint_55,
                                  filter_footprint_70, filter_footprint_85,
                                  filter_footprint_100, get_footprint_flexi):
    """
    Optimized version of the footprint calculation.

    Parameters:
    ----------
    optrad : numpy.ndarray
        The radius array
    prob : dict of numpy.ndarray
        Probability data indexed by threshold and radius
    t : float
        Current threshold value
    nm_use : int
        Number of ensemble members
    nlats, nlons : int
        Grid dimensions
    dx : float
        Grid spacing
    filter_footprint_* : numpy.ndarray
        Pre-computed footprint filters
    get_footprint_flexi : function
        Function to compute flexible footprint

    Returns:
    -------
    probfinal : numpy.ndarray
        Final probability array
    optrad : numpy.ndarray
        Updated radius array
    """
    # Pre-compute footprint sums
    footprint_sums = {
        10: float(np.sum(filter_footprint_10)),
        25: float(np.sum(filter_footprint_25)),
        40: float(np.sum(filter_footprint_40)),
        55: float(np.sum(filter_footprint_55)),
        70: float(np.sum(filter_footprint_70)),
        85: float(np.sum(filter_footprint_85)),
        100: float(np.sum(filter_footprint_100))
    }

    # Pre-allocate output array
    probfinal = np.zeros_like(optrad)

    # Create a mapping of radius ranges to footprint sizes and probability arrays
    radius_map = [
        ((2.5, 17.5), 10),
        ((17.5, 32.5), 25),
        ((32.5, 47.5), 40),
        ((47.5, 62.5), 55),
        ((62.5, 77.5), 70),
        ((77.5, 92.5), 85),
        ((92.5, float('inf')), 100)
    ]

    # Convert optrad to integer once
    optrad_int = optrad.astype(int)

    # Get all indices where the sum of exceed3d is not zero (assumed from context)
    # This is just a placeholder - replace with your actual condition
    valid_indices = np.where(optrad > 0)

    # Process only valid indices
    for row, column in zip(valid_indices[0], valid_indices[1]):
        rad = optrad_int[row, column]

        # Find the appropriate footprint size
        footprint_size = 100  # Default to largest footprint
        for (min_rad, max_rad), size in radius_map:
            if min_rad <= rad < max_rad:
                footprint_size = size
                break

        # Get initial footprint sum
        footprint_use = footprint_sums[footprint_size]

        # Calculate rdx once
        rdx = int(rad / dx)

        # Check boundaries and adjust footprint if needed
        boundary_conditions = [
            column < rdx,
            row < rdx,
            column > nlons - 1 - rdx,
            row > nlats - 1 - rdx
        ]

        if any(boundary_conditions):
            footprint_orig = footprint_use
            footprint_use = np.sum(get_footprint_flexi(rad, column, row, nlons, nlats))

            ratio = float(footprint_use) / float(footprint_orig)

            # Common condition checks
            interior_column = column > rdx and column < nlons - 1 - rdx
            interior_row = row > rdx and row < nlats - 1 - rdx

            if ratio < 0.5:
                if (boundary_conditions[0] or boundary_conditions[2]) and interior_row:
                    footprint_use = int(0.51 * footprint_orig)
                elif (boundary_conditions[1] or boundary_conditions[3]) and interior_column:
                    footprint_use = int(0.51 * footprint_orig)

            if ratio < 0.25:
                footprint_use = int(0.26 * footprint_orig)

        # Calculate final probability
        probfinal[row, column] = 100.0 * prob[t, footprint_size][row, column] / float(footprint_use * nm_use)
        # Debug output for high probabilities
        if probfinal[row, column] > 100.1:
            print('row, column, probfinal[row, column]: ', row, column, probfinal[row, column])
            print(f'prob[t,{footprint_size}][row,column], footprint_use: ',
                 prob[t, footprint_size][row, column], footprint_use)

        # Special case for large radii
        if rad > 100:
            optrad[row, column] = 0

    # cap probfinal at 100
    probfinal = np.where(probfinal > 100.0,100.0,probfinal)
    return probfinal, optrad


# ENSEMBLE AGREEMENT SCALES PROBABILITY FUNCTION
#
#   ensemble_qpf: 3-D QPF array (members x lat x lon)
#   t:            QPF threshold in inches
#   rlist:        list of potential EAS radii (km)
#   alpha:        similarity criteria parameter
#   dx:           model grid spacing (km)
#   p_smooth:     width of Gaussian filter for smoothing radius field (grid points)
#
def calculate_eas_probability(ensemble_qpf,t,rlist,alpha,dx,p_smooth):
    exceed3d = np.where(np.greater_equal(ensemble_qpf/1.0,t),1,0)
    print('sum of exceed3d for t: ', t, ' is : ', np.sum(exceed3d))
    nm_use, isize, jsize = np.shape(exceed3d)
    print('nm_use within get_footprint: ', nm_use)
    print('isize, jsize within get_footprint: ', isize, jsize)
    pr1, pr2 = [], []  # create lists of ens member pairs
    for m1 in range(nm_use-1):
      for m2 in range(m1+1,nm_use):
        pr1.append(m1)
        pr2.append(m2)
    optrad = np.zeros((isize,jsize),dtype=float) + float(max(rlist)) # set initial radius to max for better smoothing
    rlist = sorted(rlist,reverse=True)
    for i in range(len(rlist)):  # loop through rlist to find smallest radius over which member pairs meet similarity criteria
      dcrit = alpha
#      dcrit = alpha + ((1 - alpha) * (rlist[i] / float(max(rlist))))
      footprint = get_footprint(rlist[i])
      dijmean = np.zeros((isize,jsize),dtype=float)
      frac = np.zeros((nm_use,isize,jsize),dtype=float)
      for mem in range(nm_use):
        frac[mem,:,:] = np.around(signal.fftconvolve(exceed3d[mem,:,:],footprint,mode='same'))/float(np.sum(footprint))
      if i == 0:  # identify points where QPF threshold is not met within the maximum radius in any ens member
        fracsum = np.sum(frac,axis=0)
      for pair in range(len(pr1)):
        dijmean = dijmean + np.where(np.logical_and(np.greater(frac[pr1[pair]],0),np.greater(frac[pr2[pair]],0)),(frac[pr1[pair]]-frac[pr2[pair]])**2/(frac[pr1[pair]]**2+frac[pr2[pair]]**2),1)/float(len(pr1))
      optrad=np.where(np.less_equal(dijmean,dcrit),rlist[i],optrad)
#      p = np.where(np.less_equal(dijmean,dcrit),100.0*np.sum(frac,axis=0)/float(nm_use),p)
# smooth radius grid and zero out any dry areas
#    p = np.where(np.equal(fracsum,0),0,ndimage.filters.gaussian_filter(p,p_smooth))
    optrad = np.where(np.equal(fracsum,0),slim+5,ndimage.filters.gaussian_filter(optrad,p_smooth))
    return optrad

def calculate_pnt_probability(ensemble_qpf,t,p_smooth):
#?    exceed3d = np.where(np.greater_equal(ensemble_qpf/25.4,t),1,0)
    exceed3d = np.where(np.greater_equal(ensemble_qpf,t),1,0)
    p_smooth_loc=p_smooth+2

    nm_use, isize, jsize = np.shape(exceed3d)
    pnt_prob = np.zeros((isize,jsize),dtype=float)

    for mem in range(nm_use):
        pnt_prob[:,:] = pnt_prob[:,:]+(exceed3d[mem,:,:]/float(nm_use))

    pnt_prob = 100.0 * pnt_prob
    pnt_prob = ndimage.filters.gaussian_filter(pnt_prob,p_smooth_loc)

    return pnt_prob



#--------------------------------------------------------------------------------
#### REAL START OF SCRIPT ####

starttime = time.time()

print ("Processing probabilistic snow")

try:
  os.environ["WGRIB2"]
except KeyError:
  print("NEED module loaded to define WGRIB2")
  exit(1)

WGRIB2=os.environ.get('WGRIB2','trash')
print('found WGRIB2 as ', WGRIB2)



try:
  os.environ["HOMErefs"]
except KeyError:
  print("NEED TO DEFINE HOMErefs")
  exit(1)
HOMErefs=os.environ.get('HOMErefs','trash')
print('found HOMErefs as ', HOMErefs)

try:
  os.environ["COMIN"]
except KeyError:
  print("NEED TO DEFINE COMIN")
  exit(1)
COMIN=os.environ.get('COMIN','trash')
print ('found COMIN as ', COMIN)

try:
  os.environ["COMINhrrr"]
except KeyError:
  print("NEED TO DEFINE COMINhrrr")
  exit(1)
COMINhrrr=os.environ.get('COMINhrrr','trash')
print ('found COMINhrrr as ', COMINhrrr)

try:
  os.environ["COMOUT"]
except KeyError:
  print("NEED TO DEFINE COMOUT")
  exit(1)
COMOUT=os.environ.get('COMOUT','trash')
print('found COMOUT as ', COMOUT)

try:
  os.environ["PDY"]
except KeyError:
  print("NEED TO DEFINE PDY")
  exit(1)
PDY=os.environ.get('PDY','trash')
print('found PDY as ', PDY)

try:
  os.environ["cyc"]
except KeyError:
  print("NEED TO DEFINE cyc")
  exit(1)
cyc=os.environ.get('cyc','trash')
print('found cyc as ', cyc)

try:
  os.environ["dom"]
except KeyError:
  print("NEED TO DEFINE dom")
  exit(1)
dom=os.environ.get('dom','trash')
print('found dom as ', dom)

try:
  os.environ["DATA"]
except KeyError:
  print("NEED TO DEFINE DATA")
  exit(1)
DATA=os.environ.get('DATA','trash')
print('found DATA as ', DATA)

sys.path.append(HOMErefs)
from eas_config import *

# output directory
# grib-2 template 
template = HOMErefs + '/fix/asnow_rrfs'+dom+'template.grib2'
record = 1		# SNOW from SREF pgrb212 file

print('template file is: ', template)

# get latest run times and create output directory if it doesn't already exist

# accumulation interval (hours)
fcst_hour = int(sys.argv[1])
qpf_interval = int(sys.argv[2])

os.system("cd "+DATA)

start_hour = int(fcst_hour - qpf_interval)
fhr=fcst_hour
fhr_range=str(start_hour)+'-'+str(fcst_hour)

print('fhr_range: ', fhr_range)

print('fcst_hour, qpf_interval, start_hour: ', fcst_hour, qpf_interval, start_hour)

# maximum radius (km)
slim = max(rlist)
alpha = 0.5


if not os.path.exists(COMOUT):
  os.system("mkdir -p " + COMOUT)

#------------------------------------------------------------------------------------------

if dom == 'conus':
 nm_use = 14
 members = ['rrfs01','rrfs02','rrfs03','rrfs04','rrfs05','rrfs06','hrrr']
elif dom == 'ak':
 nm_use = 14
 members = ['rrfs01','rrfs02','rrfs03','rrfs04','rrfs05','rrfs06','hrrrak']
else:
 nm_use = 12
 members = ['rrfs01','rrfs02','rrfs03','rrfs04','rrfs05','rrfs06']

pqpf_6h_calibrate = 'no'
pqpf_3h_calibrate = 'no'
# print 'Calibration coefficients for '+mem+' members not found. Using raw model QPF'

cy, cm, cd, ch = int(PDY[0:4]), int(PDY[4:6]), int(PDY[6:8]), int(cyc[0:2])
d0 = datetime(cy,cm,cd,ch,0)
starttime = d0+timedelta(start_hour/24.0)
endtime = d0+timedelta((start_hour+qpf_interval)/24.0)
memfiles = {}
memfiles4 = {}
itimes = []
fhours = []

latency = min_latency
stop = max_latency

wgribdate=PDY+cyc

# create grib messages from template (only need to do this once)

os.system(WGRIB2+' '+template+' -rpn rcl_lat -text lat.txt  -rpn rcl_lon -text lon.txt')

lons,nx,ny=simplewgrib2('lon.txt')
lats,nx,ny=simplewgrib2('lat.txt')

nlats, nlons = np.shape(lats)

if dom == 'ak':
  print('defining ak maskfile')
  maskfile = HOMErefs + '/fix/akrrfs_mask.grib2'
  print('maskfile is: ', maskfile)
  os.system(WGRIB2+' '+maskfile+'  -text mask.txt ')
  undefmask,nx,ny=simplewgrib2('mask.txt')
  undefmask=np.ma.masked_greater(undefmask,9.0e+20)
  maskregion = np.ma.filled(undefmask,-9999)


if qpf_interval == 1:
  outbase = 'refs.t'+cyc[0:2]+'z.snow01_easfrac.f%02d.'%(start_hour+qpf_interval)+dom+'.grib2'
  print('defined outbase: ', outbase)
  incr = 1
  thresh_use=snow_1h_thresh
if qpf_interval == 3:
  outbase = 'refs.t'+cyc[0:2]+'z.snow03_easfrac.f%02d.'%(start_hour+qpf_interval)+dom+'.grib2'
  incr = 3
  thresh_use=snow_3h_thresh
if qpf_interval == 6:
  outbase = 'refs.t'+cyc[0:2]+'z.snow06_easfrac.f%02d.'%(start_hour+qpf_interval)+dom+'.grib2'
  incr = 3
  thresh_use=snow_6h_thresh
  print('thresh_use: ', thresh_use)

outfile = DATA + '/' + outbase

if os.path.exists(outfile):
  os.system('rm -f '+outfile)


# qpf is a dictionary - need unique key.  itime values are repeated, so is a bad choice.

prob = {}
qpf = {}
memcount = 0

print('members: ', members)

for mem in members:
  memname=mem[0:4]
  memnum=mem[4:6]
  print('memname, memnum: ', memname, memnum)

  while (len(itimes) < memcount+2) and (latency <= stop) and (start_hour+qpf_interval+latency <= 60):
    print('len(itimes), memcount+2: ', len(itimes), memcount+2)
    itime = starttime-timedelta((start_hour+latency)/24.0)
    itime_alt = starttime-timedelta((start_hour+latency+6)/24.0)
    print('itime for this member: ', itime)

    if memname == 'rrfs':
      file3 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+incr)+'.grib2'
      print('file3 is: ', file3)
      file6 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+incr+incr)+'.grib2'
      print('file6 is: ', file6)
      file3alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+incr+6)+'.grib2'
      print('file3alt is: ', file3alt)
      file6alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+2*incr+6)+'.grib2'
      print('file6alt is: ', file6alt)
    elif memname == 'hrrr':
      file3 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/'+'%02d'%itime.hour+'/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+latency+incr)+'.grib2'
      file6 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/'+'%02d'%itime.hour+'/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+latency+incr+incr)+'.grib2'
      file3alt = 'garb'
      file6alt = 'garb'
    elif memname == 'hrrrak':
      file3 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/'+'%02d'%itime.hour+'/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+latency+incr)+'.grib2'
      file6 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/'+'%02d'%itime.hour+'/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+latency+incr+incr)+'.grib2'
      file3alt = 'garb'
      file6alt = 'garb'






    if qpf_interval != 6:
      if os.path.exists(file3):
        print ('Found:',itime,'forecast hour',(start_hour+qpf_interval+latency))
        fhours.append(start_hour+qpf_interval+latency)
        itimes.append(itime)
        memfiles[itime] = file3
        print('utilizing file3 in memfiles: ', file3)
      else:
#        print('Missing:',itime,'forecast hour',(start_hour+qpf_interval+latency))
        if os.path.exists(file3alt):
          print('alt Found:',itime_alt,'forecast hour',(start_hour+qpf_interval+latency))
          fhours.append(start_hour+qpf_interval+latency+6)
          print('defined file3 fhours: ', start_hour+qpf_interval+latency+6)
          itimes.append(itime_alt)
          memfiles[itime_alt] = file3alt
          print('using file3alt which is: ', file3alt)
        else:
          print('Completely missing:',itime,'forecast hour',(start_hour+qpf_interval+latency))
    else:
      print('using 6 h block portion')
      if os.path.exists(file3) and os.path.exists(file6):
        print('Found:',itime,'forecast hour',(start_hour+qpf_interval+latency))
        itimes.append(itime)
        fhours.append(start_hour+qpf_interval+latency)
        print('defined file3 for 6h fhours: ', start_hour+qpf_interval+latency)
        memfiles[itime] = [file3,file6]
      else:
        print('Missing:',itime,'forecast hour',(start_hour+qpf_interval+latency))
        if os.path.exists(file3alt) and os.path.exists(file6alt):
          print('alt Found:',itime_alt,'forecast hour',(start_hour+qpf_interval+latency))
          itimes.append(itime_alt)
          fhours.append(start_hour+qpf_interval+latency+6)
          print('defined fhours in alt 6h block: ', start_hour+qpf_interval+latency+6)
          memfiles[itime_alt] = [file3alt,file6alt]

    if mem == 'nam' or mem == 'hrrr' or mem == 'hrrrak':
      latency = latency + 6
    else:
      latency = latency + 6

  if len(itimes) == (memcount+2):
    print('Found 2 '+mem+' members valid:',starttime,'-',endtime)
  else:
    print('Could not find 2 '+mem+' members valid for start hour',start_hour)
#    sys.exit(1)

#### READ IN QPF ####
  print('here a memcount: ', memcount)
  for itime in itimes[memcount:memcount+2]:
    if qpf_interval == 6:
      file3,file6 = memfiles[itime]
    else:
      file3 = memfiles[itime]

    print('Processing member',(1+memcount),'of',nm)
    print('file3 near read is ', file3)

    if qpf_interval != 1:
      if qpf_interval == 3:
        fhour=fhours[memcount]
        shour=fhour-3
      if qpf_interval == 6:
        fhour=fhours[memcount]-3
        shour=fhour-3
      print('for file3 shour fhour: ', shour,fhour)
      os.system(WGRIB2+' '+file3+' -match "ASNOW:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf.txt')
      qpf3,nx,ny=simplewgrib2('qpf.txt')

      undefsnow,nx,ny=simplewgrib2('qpf.txt')
      undefsnow=np.ma.masked_greater(undefsnow,9.0e+20)
      snowmaskregion = np.ma.filled(undefsnow,-9999)
      qpf3 = np.where(np.equal(snowmaskregion,-9999),0,qpf3)

    else:

      print('ready from file3 in 1h: ', file3)
      fhour=fhours[memcount]
      shour=fhour-1
      print('ASNOW accum from : ', shour, ' to : ', fhour)
      os.system(WGRIB2+' '+file3+' -match "ASNOW:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf.txt')
      qpf1,nx,ny=simplewgrib2('qpf.txt')

      undefsnow,nx,ny=simplewgrib2('qpf.txt')
      undefsnow=np.ma.masked_greater(undefsnow,9.0e+20)
      snowmaskregion = np.ma.filled(undefsnow,-9999)
      qpf1 = np.where(np.equal(snowmaskregion,-9999),0,qpf1)

      print('defining qpf from qpf1')
      qpf[itime] = qpf1*39.370079 # meters to inches

    if qpf_interval == 6:
      fhour=fhours[memcount]
      shour=fhour-3
      print('6h shour fhour: ', shour,fhour)
      os.system(WGRIB2+' '+file6+' -match "ASNOW:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf.txt')
      qpf6,nx,ny=simplewgrib2('qpf.txt')
      undefsnow,nx,ny=simplewgrib2('qpf.txt')
      undefsnow=np.ma.masked_greater(undefsnow,9.0e+20)
      snowmaskregion = np.ma.filled(undefsnow,-9999)
      qpf6 = np.where(np.equal(snowmaskregion,-9999),0,qpf6)
      qpf[itime] = (qpf3 + qpf6) * 39.370079
    if qpf_interval == 3:
      qpf[itime] = qpf3*39.370079   # convert meters to inches


    print('max of qpf as snow: ', np.max(qpf[itime]))
    print('dom is: ', dom)
    if dom == 'ak':
      print('qpf shape: ', np.shape(qpf))
      print('maskregion shape: ', np.shape(maskregion))
      qpf[itime] = np.where(np.equal(maskregion,-9999),0,qpf[itime])

    # calculate threshold exceedance
    if qpf_interval == 1:
      thresh_use=snow_1h_thresh
    if qpf_interval == 3:
      thresh_use=snow_3h_thresh
    if qpf_interval == 6:
      thresh_use=snow_6h_thresh



# prob is okay as is based on local qpf[itime]
# but save to qpf[memcount] for use below

    # calculate threshold exceedance on QPF
    for t in thresh_use:
      for size in rlist:
        if memcount == 0:
          prob[t,size] = np.zeros((nlats,nlons))
        exceed = np.where(np.greater_equal(qpf[itime],t),1,0)
        filter_footprint = get_footprint(size)
#        print 'np.shape(filter_footprint): ', np.shape(filter_footprint)
#        print 'np.shape(exceed): ', np.shape(exceed)
## exceed varies by member, so the fftconvolve will be sensitive to the order in which the members are done
        prob[t,size] = prob[t,size] + signal.fftconvolve(exceed,filter_footprint,mode='same')

# possible to compute number of valid points here?

    print('working memcount: ', memcount)

    qpf[memcount]=qpf[itime]
    memcount = memcount + 1

  latency = min_latency


#-------------------------------------------------------------------#

# Get final probabilities
# redefine number of members (in case a TL member is missing)
nm = len(itimes)
nm_use=nm
print('settled on nm_use for final probabilities: ', nm_use)
ensemble_qpf = np.zeros((nm_use,nlats,nlons),dtype=float)
for mem in range(0,len(itimes)):
  print('mem, itimes[mem],max(qpf): ',mem, itimes[mem], np.max(qpf[mem]))
  ensemble_qpf[mem,:,:] = qpf[mem]
  print('max of member: ', np.max(ensemble_qpf[mem,:,:]))

# Get final probabilities
probfinal = np.zeros((nlats,nlons))

filter_footprint_10 = get_footprint(10)
filter_footprint_25 = get_footprint(25)
filter_footprint_40 = get_footprint(40)
filter_footprint_55 = get_footprint(55)
filter_footprint_70 = get_footprint(70)
filter_footprint_85 = get_footprint(85)
filter_footprint_100 = get_footprint(100)

for t in thresh_use:
  t3 = time.time()
  exceed3d = np.where(np.greater_equal(ensemble_qpf/1.0,t),1,0)
  exceed_sum=np.sum(exceed3d)
  print('t, dx, max(ensemble_qpf): ', t, dx, np.max(ensemble_qpf))
  optrad = calculate_eas_probability_para(ensemble_qpf,t,rlist,alpha,dx,p_smooth)
  t4 = time.time()
  print('Time for optrad routine:', t4-t3)
  pnt_prob = calculate_pnt_probability (ensemble_qpf, t, p_smooth)

# how know the filter_footprint size for near boundary points?


## need something to account for reduced portion of filter_footprint actually within domain
#
#
  print('nm_use for final prob: ', nm_use)

  if exceed_sum > 0:
    probfinal, optrad = optimize_footprint_calculation(
    optrad, prob, t, nm_use, nlats, nlons, dx,
    filter_footprint_10, filter_footprint_25, filter_footprint_40,
    filter_footprint_55, filter_footprint_70, filter_footprint_85,
    filter_footprint_100, get_footprint_flexi)
  else:
    print('sum of exceed3d was zero, so setting zero probfinal')
    probfinal[:,:] = 0.0

# slight smoothing of probfinal
  probfinal = ndimage.filters.gaussian_filter(probfinal,1)

  if dom == 'ak':
    print('working final probability with mask')
    probfinal = np.where(np.equal(maskregion,-9999),0,probfinal)  # set to 0 for mask 

  t5 = time.time()

  print('Time for get final probability routine for ',t, 'inch threshold: ',t5-t4)
#  print('max of probfinal pre cap: ', np.max(probfinal))
#  probfinal = np.where(probfinal > 100.0,100.0,probfinal)
  print('max of probfinal post cap: ', np.max(probfinal))
  print('mean of probfinal: ', np.mean(probfinal))
  print('probfinal dims: ', np.shape(probfinal))


# can we keep as doing in mm - might be confusing if kept in meters?
# probably should stick to GRIB2 standard and provide in meters

  probstr=str(t*.0254)
  print('probstr defined: ', probstr)
  byte=int(m.floor(0.5+t*25.4))
  byte44=0
  byte45=int(byte/65536)
  byte45rem=byte%65536
  byte46=int(byte45rem/256)
  byte47=byte45rem%256

  myfort = FortranFile('record_out.bin',mode='w')

  probwrite=np.float32(probfinal)
  myfort.write_record(probwrite)
  print('max of probwrite: ', np.max(probwrite))
#  myfort.write_record(probfinal)

#  myfort = F.FortranFile('record_out.bin',mode='w')
#  myfort.writeReals(probfinal)
  myfort.close()

  string="0:0:d="+wgribdate+":ASNOW:surface:"+fhr_range+" hour acc fcst:prob >"+probstr+":"

  os.system(WGRIB2+' '+template+' -import_bin record_out.bin -set_metadata_str "'+string+'" -set_grib_type c3 -grib_out premod.grb')
  os.system(WGRIB2+' premod.grb -set_byte 4 14 136 -set_byte 4 12 197 -set_byte 4 17 0 -set_byte 4 24:35 0:0:0:0:0:255:0:0:0:0:0:0 -set_byte 4 36 '+str(nm_use)+' -set_byte 4 38:42 0:0:0:0:0 -set_byte 4 43 3 -set_byte 4 44 0 -set_byte 4 45 '+str(byte45)+' -set_byte 4 46 '+str(byte46)+' -set_byte 4 47 '+str(byte47)+' -set_byte 4 67 1 -append  -set_grib_type c3 -grib_out '+outfile)
  print('Wrote ', qpf_interval, ' PSNOW to:',outfile, 'for ',t, 'inch threshold')
  os.system('rm record_out.bin')
  os.system('rm premod.grb')
  print('exiting ush/enspost_make_easfracsnow_combo.py ...')
