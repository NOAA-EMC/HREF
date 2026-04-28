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
# 2018-03-20  M Pyle    replace pygrib with wgrib2
# 2021-06     M Pyle    Modifications to use python3
# 2022-03     M Pyle    Modifications to limit to just FV3/RRFS members
# 2023-10     M PYle    Adds back HRRR members for CONUS/AK
# 2025-12     M Pyle    COMINrefs --> COMIN

import os, sys, time
import numpy as np
import math as m
from datetime import datetime, timedelta
from scipy import ndimage, optimize, signal
from scipy.io import FortranFile 
import multiprocessing as mp
from functools import partial

# import fortranfile as F

# CRITICAL: Prevent numpy/scipy from fighting for threads inside multiprocessing
os.environ["OMP_NUM_THREADS"] = "1"

def optimized_wgrib2(txtfile):
    """
    Reads grid dimensions and data from a text file more efficiently.

    Args:
        txtfile (str): The path to the input text file.

    Returns:
        tuple: A tuple containing the 2D NumPy array, nx, and ny.
    """
    # Open the file to read just the header line
    with open(txtfile) as f:
        nx, ny = [int(x) for x in f.readline().split()]

    # Use NumPy's highly optimized loadtxt to read the remaining data
    # This is much faster than iterating line-by-line in Python
    data = np.loadtxt(txtfile, dtype=np.float32, skiprows=1)

    # Directly reshape the loaded data into the final 2D array
    # An error will be raised if the number of elements doesn't match nx * ny
    array2d = data.reshape(ny, nx)

    return array2d, nx, ny

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

#--------------------------------------------------------------------------------
#### FUNCTIONS AND ROUTINES ####

# calculate footprint routine
def get_footprint(r):
#    footprint = np.ones(((r/dx)*2+1,(r/dx)*2+1),dtype=int)
    footprint = np.ones((int((r/dx)*2+1),int((r/dx)*2+1)),dtype=int)
    footprint[int(m.ceil(r/dx)),int(m.ceil(r/dx))] = 0
    dist = ndimage.distance_transform_edt(footprint,sampling=[dx,dx])
    footprint = np.where(np.greater(dist,r),0,1)
    return footprint


def get_footprint_new(radius):
    """
    Creates a circular footprint with the specified radius.
    """
    # Estimate grid size based on radius
    size = int(2 * radius + 1)
    y, x = np.ogrid[-size//2:size//2+1, -size//2:size//2+1]
    mask = x*x + y*y <= radius*radius
    footprint = np.zeros((size, size))
    footprint[mask] = 1
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
    exceed3d = np.where(np.greater_equal(ensemble_qpf/25.4, t), 1, 0)
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
    
    return probfinal, optrad

#
#   ensemble_qpf: 3-D QPF array (members x lat x lon)
#   t:            QPF threshold in inches
#   rlist:        list of potential EAS radii (km)
#   alpha:        similarity criteria parameter
#   dx:           model grid spacing (km)
#   p_smooth:     width of Gaussian filter for smoothing radius field (grid points)
#
def calculate_eas_probability(ensemble_qpf,t,rlist,alpha,dx,p_smooth):
    exceed3d = np.where(np.greater_equal(ensemble_qpf/25.4,t),1,0)
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
    exceed3d = np.where(np.greater_equal(ensemble_qpf/25.4,t),1,0)
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

print('Processing probabilistic QPF')

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
print('found COMIN  as ', COMIN)

try: 
  os.environ["COMINhrrr"]
except KeyError:
  print("NEED TO DEFINE COMINhrrr")
  exit(1)
COMINhrrr=os.environ.get('COMINhrrr','trash')
print('found COMINhrrr as ', COMINhrrr)


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
template = HOMErefs + '/fix/pqpf_rrfs'+dom+'template.grib2'
record = 1		# PQPF from SREF pgrb212 file

print('template file is: ', template)

# get latest run times and create output directory if it doesn't already exist

# accumulation interval (hours)
fcst_hour = int(sys.argv[1])

qpf_interval = int(sys.argv[2])
start_hour = int(fcst_hour - qpf_interval)

os.system("cd "+DATA)

fhr=fcst_hour
fhr_range=str(start_hour)+'-'+str(fcst_hour)

# maximum radius (km)
slim = max(rlist)
alpha = 0.5

os.system(WGRIB2+' '+template+' -rpn rcl_lat -text lat.txt  -rpn rcl_lon -text lon.txt')

lons,nx,ny=simplewgrib2('lon.txt')
lats,nx,ny=simplewgrib2('lat.txt')

nlats, nlons = np.shape(lats)

print('nlons, nlats: ', nlons, nlats)

if dom == 'ak':
  maskfile = HOMErefs + '/fix/akrrfs_mask.grib2'

if dom == 'ak':
  os.system(WGRIB2+' '+maskfile+' -text mask.txt ')
  undefmask,nx,ny=simplewgrib2('mask.txt')

  undefmask=np.ma.masked_greater(undefmask,9.0e+20)
  maskregion = np.ma.filled(undefmask,-9999)
  print('maskregion defined')


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
 nm_use=12
 members = ['rrfs01','rrfs02','rrfs03','rrfs04','rrfs05','rrfs06']

pqpf_6h_calibrate = 'no'


cy, cm, cd, ch = int(PDY[0:4]), int(PDY[4:6]), int(PDY[6:8]), int(cyc[0:2])
d0 = datetime(cy,cm,cd,ch,0)
starttime = d0+timedelta(start_hour/24.0)
endtime = d0+timedelta((start_hour+qpf_interval)/24.0)
memfiles = {}
itimes = []
fhours = []
latency = min_latency
stop = max_latency

# create grib messages from template (only need to do this once)

wgribdate=PDY+cyc

if qpf_interval == 1:
  incr = 1
  thresh_use=pqpf_1h_thresh
  outbase = 'refs.t'+cyc[0:2]+'z.pqpf01_easfrac.f%02d.'%(start_hour+qpf_interval)+dom+'.grib2'

if qpf_interval == 3:
  incr = 3
  thresh_use=pqpf_3h_thresh
  outbase = 'refs.t'+cyc[0:2]+'z.pqpf03_easfrac.f%02d.'%(start_hour+qpf_interval)+dom+'.grib2'

if qpf_interval == 6:
  incr = 3
  thresh_use=pqpf_6h_thresh
  outbase = 'refs.t'+cyc[0:2]+'z.pqpf06_easfrac.f%02d.'%(start_hour+qpf_interval)+dom+'.grib2'

if qpf_interval == 12:
  incr = 3
  thresh_use=pqpf_12h_thresh
  outbase = 'refs.t'+cyc[0:2]+'z.pqpf12_easfrac.f%02d.'%(start_hour+qpf_interval)+dom+'.grib2'

if qpf_interval == 24:
  incr = 3
  thresh_use=pqpf_24h_thresh
  outbase = 'refs.t'+cyc[0:2]+'z.pqpf24_easfrac.f%02d.'%(start_hour+qpf_interval)+dom+'.grib2'

outfile = DATA + '/' + outbase

if os.path.exists(outfile):
  os.system('rm -f '+outfile)


# qpf is a dictionary - need unique key.  itime values are repeated, so is a bad choice.

prob = {}
qpf = {}
memcount = 0

for mem in members:
  memname=mem[0:4]
  memnum=mem[4:6]
  print('memname, memnum: ', memname, memnum)
  print('latency: ',latency)
  print('stop: ', stop)
  print('start_hour+qpf_interval+latency: ', start_hour+qpf_interval+latency)

  while (len(itimes) < memcount+2) and (latency <= stop) and (start_hour+qpf_interval+latency <= 60):
    print('len(itimes), memcount+2: ', len(itimes), memcount+2)
    itime = starttime-timedelta((start_hour+latency)/24.0)
    print('itime for this member: ', itime)
    itime_alt = starttime-timedelta((start_hour+latency+6)/24.0)
    if memname == 'rrfs':
      file0 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency)+'.grib2'
      file1 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+incr)+'.grib2'
      file2 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+2*incr)+'.grib2'
      file3 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+3*incr)+'.grib2'
      file4 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+4*incr)+'.grib2'
      file5 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+5*incr)+'.grib2'
      file6 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+6*incr)+'.grib2'
      file7 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+7*incr)+'.grib2'
      file8 = COMIN + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/fv3s.t%02d'%itime.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+8*incr)+'.grib2'

      file0alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+6)+'.grib2'
      file1alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+incr+6)+'.grib2'
      file2alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+2*incr+6)+'.grib2'
      file3alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+3*incr+6)+'.grib2'
      file4alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+4*incr+6)+'.grib2'
      file5alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+5*incr+6)+'.grib2'
      file6alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+6*incr+6)+'.grib2'
      file7alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+7*incr+6)+'.grib2'
      file8alt = COMIN + '.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day +'/'+'%02d'%itime_alt.hour+ '/fv3s.t%02d'%itime_alt.hour+'z.'+dom+'.m'+memnum+'.f%02d'%(start_hour+latency+8*incr+6)+'.grib2'

    elif mem == 'hrrr':
      file0 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+latency)+'.grib2'
      file1 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+incr+latency)+'.grib2'
      file2 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+2*incr+latency)+'.grib2'
      file3 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+3*incr+latency)+'.grib2'
      file4 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+4*incr+latency)+'.grib2'
      file5 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+5*incr+latency)+'.grib2'
      file6 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+6*incr+latency)+'.grib2'
      file7 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+7*incr+latency)+'.grib2'
      file8 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+8*incr+latency)+'.grib2'

    elif mem == 'hrrrak': 
      file0 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+latency)+'.grib2'
      file1 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+incr+latency)+'.grib2'
      file2 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+2*incr+latency)+'.grib2'
      file3 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+3*incr+latency)+'.grib2'
      file4 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+4*incr+latency)+'.grib2'
      file5 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+5*incr+latency)+'.grib2'
      file6 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+6*incr+latency)+'.grib2'
      file7 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+7*incr+latency)+'.grib2'
      file8 = COMINhrrr + '.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day +'/'+'%02d'%itime.hour+ '/hrrr.t%02d'%itime.hour+'z.'+dom+'.f%02d'%(start_hour+8*incr+latency)+'.grib2'


    elif memname == 'fv3n':
      print('no fv3nc here')  
    elif memname == 'fv3s':
      print('no fv3s here')
    elif memname == 'arw2':
      print('no arw2 here')

    alt_fhrinc = 6

    if qpf_interval == 1:
      if os.path.exists(file1):
        print('Found:',itime,'forecast hour',(start_hour+1*incr+latency))
        fhours.append(start_hour+1*incr+latency)
        itimes.append(itime)
# define fully just in case
        memfiles[itime] = [file0,file1,file2,file3,file4,file5,file6,file7,file8]
      else:
#        print('did not find file1: ', file1)
        print('trying to work file1alt: ', file1alt)
        if (os.path.exists(file1alt)):
          fhours.append(start_hour+1*incr+latency+alt_fhrinc)
          itimes.append(itime_alt)
          memfiles[itime_alt] = [file0alt,file1alt,file2alt,file3alt,file4alt,file5alt,file6alt,file7alt,file8alt]
        else:
          print('Alt cycle is missing as well')

    if qpf_interval == 3:
      if os.path.exists(file1):
        print('Found:',itime,'forecast hour',(start_hour+1*incr+latency))
        fhours.append(start_hour+1*incr+latency)
        itimes.append(itime)
# define fully just in case
        memfiles[itime] = [file1,file2,file3,file4,file5,file6,file7,file8]
      else:
#        print('did not find file1: ', file1)
        print('trying to work file1alt: ', file1alt)
        if (os.path.exists(file1alt)):
          fhours.append(start_hour+1*incr+latency+alt_fhrinc)
          itimes.append(itime_alt)
          memfiles[itime_alt] = [file1alt,file2alt,file3alt,file4alt,file5alt,file6alt,file7alt,file8alt]
#        else:
#          print('Alt cycle is missing as well')

    if qpf_interval == 6:
      if os.path.exists(file2):
        print('Found:',itime,'forecast hour',(start_hour+2*incr+latency))
        itimes.append(itime)
        fhours.append(start_hour+1*incr+latency)
# define fully just in case
        memfiles[itime] = [file1,file2,file3,file4,file5,file6,file7,file8]
      else:
#        print('did not find file2 for qpf_6: ', file2)
        print('trying to work file2alt: ', file2alt)
        if (os.path.exists(file2alt)):
          fhours.append(start_hour+1*incr+latency+alt_fhrinc)
          itimes.append(itime_alt)
          memfiles[itime_alt] = [file1alt,file2alt,file3alt,file4alt,file5alt,file6alt,file7alt,file8alt]
#        else:
#          print('Alt cycle is missing as well')

    if qpf_interval == 12:
      if os.path.exists(file4):
        print('Found:',itime,'forecast hour',(start_hour+4*incr+latency))
        itimes.append(itime)
        fhours.append(start_hour+1*incr+latency)
# define fully just in case
        memfiles[itime] = [file1,file2,file3,file4,file5,file6,file7,file8]
        print('defined memfiles for qpf_interval12')
      else:
#        print('did not find file4 for qpf_12: ', file4)
        print('trying to work file4alt: ', file4alt)
        if (os.path.exists(file4alt)):
          itimes.append(itime_alt)
          fhours.append(start_hour+1*incr+latency+alt_fhrinc)
          memfiles[itime_alt] = [file1alt,file2alt,file3alt,file4alt,file5alt,file6alt,file7alt,file8alt]
#        else:
#          print('Alt cycle is missing as well')



    if qpf_interval == 24:
      if os.path.exists(file8):
        print('Found:',itime,'forecast hour',(start_hour+8*incr+latency))
        itimes.append(itime)
        fhours.append(start_hour+1*incr+latency)
        memfiles[itime] = [file1,file2,file3,file4,file5,file6,file7,file8]
      else:
#        print('did not find file8 for qpf_24: ', file8)
        print('trying to work file8alt: ', file8alt)
        if (os.path.exists(file8alt)):
          itimes.append(itime_alt)
          fhours.append(start_hour+1*incr+latency+alt_fhrinc)
          memfiles[itime_alt] = [file1alt,file2alt,file3alt,file4alt,file5alt,file6alt,file7alt,file8alt]
#        else:
#          print('Alt cycle is missing as well')



    if mem == 'nam' or mem == 'hrrr' or mem == 'hrrrak' or mem == 'rrfs':
      latency = latency + 6
    else:
      print('using alt latency')
      latency = latency + 6

  if len(itimes) == (memcount+2):
    print('Found 2 '+mem+' members valid:',starttime,'-',endtime)
  else:
    print('Could not find 2 '+mem+' members valid for start hour',start_hour)
#    sys.exit(1)

#### READ IN QPF ####
  for itime in itimes[memcount:memcount+2]:
    if qpf_interval == 24 or qpf_interval == 6 or qpf_interval == 12:

      file1,file2,file3,file4,file5,file6,file7,file8 = memfiles[itime]

# Process first 6 hours
      print('Processing member',(1+memcount),'of',nm_use)
      fhour=fhours[memcount]
      print('fhour is: ', fhour)
      shour=fhour-3
 
      wgribzero=time.time()
      print('dealing with file1: ', file1)
      os.system(WGRIB2+' '+file1+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf1.txt ')
      qpf1,nx,ny=simplewgrib2('qpf1.txt')

## how get the proper hours for each file??
      fhour=fhours[memcount]+incr
      shour=fhour-3
      os.system(WGRIB2+' '+file2+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf2.txt ')
      qpf2,nx,ny=simplewgrib2('qpf2.txt')

      qpf12 = qpf1 + qpf2    

      print('max of qpf12: ', np.max(qpf12))

      if qpf_interval == 6:
         print('defined qpf[itime] from qpf12')
         qpf[itime]=qpf12
         print('itime assigned: ', itime)
         print('used memcount instead: ', memcount)

    if qpf_interval == 24 or qpf_interval == 12 :

## figure out fhour for two pieces here

      print('looking for file 3: ', file3)
      fhour=fhr+6
      fhour=fhours[memcount]+incr*2
      shour=fhour-3
      os.system(WGRIB2+' '+file3+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf3.txt ')
      qpf3,nx,ny=simplewgrib2('qpf3.txt')

      fhour=fhr+9
      fhour=fhours[memcount]+incr*3
      shour=fhour-3
      os.system(WGRIB2+' '+file4+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf4.txt ')
      qpf4,nx,ny=simplewgrib2('qpf4.txt')


# Process second 6 hours

      qpf34 = qpf3 + qpf4    

#### 12 h sum of the first two 6 h periods
      if qpf_interval == 12 :
        qpf[itime]= qpf12 + qpf34

    if qpf_interval == 24 :

# Process third 6 hours
      fhour=fhr+12
      fhour=fhours[memcount]+incr*4
      shour=fhour-3
      os.system(WGRIB2+' '+file5+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf5.txt ')
      qpf5,nx,ny=simplewgrib2('qpf5.txt')

      fhour=fhr+15
      fhour=fhours[memcount]+incr*5
      shour=fhour-3
      os.system(WGRIB2+' '+file6+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf6.txt ')
      qpf6,nx,ny=simplewgrib2('qpf6.txt')

      qpf56 = qpf5 + qpf6

# Process last 6 hours
## figure out fhour for two pieces here

      fhour=fhr+18
      fhour=fhours[memcount]+incr*6
      shour=fhour-3
      os.system(WGRIB2+' '+file7+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf7.txt ')
      qpf7,nx,ny=simplewgrib2('qpf7.txt')


      fhour=fhr+21
      fhour=fhours[memcount]+incr*7
      shour=fhour-3
      os.system(WGRIB2+' '+file8+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf8.txt ')
      qpf8,nx,ny=simplewgrib2('qpf8.txt')

      qpf78 = qpf7 + qpf8

      qpf[itime] = qpf12 + qpf34 + qpf56 + qpf78

######### 3 h APCP

    if qpf_interval == 3:

      file1,file2,file3,file4,file5,file6,file7,file8 = memfiles[itime]
      print('from memfiles file1 for 3 h qpf: ', file1)

# Process first 3 hours
      print('Processing member',(1+memcount),'of',nm_use)
      print('fhours of mem: ', fhours[memcount])

      fhour=fhours[memcount]
      shour=fhour-3

      print('shour: ', shour)
      print('fhour: ', fhour)

      print('nx, ny: ', nx, ny)
      print('for file1: ', file1)
      os.system(WGRIB2+' '+file1+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf.txt ')
      qpfhere,nx,ny=simplewgrib2('qpf.txt')
      os.system('rm -f qpf.txt')
      qpf[itime]=qpfhere
      print('from file1: ', file1)
      print('max 3 h qpf: ', np.max(qpf[itime]))
      print('mean 3 h qpf: ', np.mean(qpf[itime]))

######### 1 h APCP

    if qpf_interval == 1:

      file0,file1,file2,file3,file4,file5,file6,file7,file8 = memfiles[itime]

# Process first 1 hour
      print('Processing member',(1+memcount),'of',nm_use)
      print('from memfiles file1 for 1 h qpf: ', file1)

      fhour=fhours[memcount]
      shour=fhour-1
      print('from memfiles file1 for 1 h qpf: ', file1)
      print('fhour is: ', fhour)
      os.system(WGRIB2+' '+file1+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf.txt ')
      qpf1,nx,ny=simplewgrib2('qpf.txt')
      qpf[itime] = qpf1

      print('max of 1 h APCP: ', np.max(qpf[itime]))

###############

    print('here past qpf_interval tests')
    print('here with itime: ', itime)
    if dom == 'ak':
      qpf[itime] = np.where(np.equal(maskregion,-9999),0,qpf[itime])

# prob is okay as is based on local qpf[itime]

# but save to qpf[memcount] for use below

    qpf[memcount]=qpf[itime]

    # calculate threshold exceedance on QPF
    for t in thresh_use:
      for size in rlist:
        if memcount == 0:
          prob[t,size] = np.zeros((nlats,nlons))
        exceed = np.where(np.greater_equal(qpf[itime]/25.4,t),1,0)
        dontexceed = np.where(np.less(qpf[itime]/25.4,t),1,0)
        filter_footprint = get_footprint(size)

#        print('np.shape(filter_footprint): ', np.shape(filter_footprint))
#        print('np.shape(exceed): ', np.shape(exceed))
## exceed varies by member, so the fftconvolve will be sensitive to the order in which the members are done
        prob[t,size] = prob[t,size] + signal.fftconvolve(exceed,filter_footprint,mode='same')
#        print('t, size, sum(exceed),sum(dontexceed), mean prob: ', t, size, np.sum(exceed),np.sum(dontexceed), np.mean(prob[t,size]))

# possible to compute number of valid points here?

    print('working memcount: ', memcount)

    memcount = memcount + 1

  latency = min_latency


#-------------------------------------------------------------------#

# Get final probabilities
# redefine number of members (in case a TL member is missing)
nm_use = len(itimes)

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

# print('sums of the filters (10,25,40): ', np.sum(filter_footprint_10),np.sum(filter_footprint_25),np.sum(filter_footprint_40))
# print('sums of the filters (55,70,85,100): ', np.sum(filter_footprint_55),np.sum(filter_footprint_70),np.sum(filter_footprint_85),np.sum(filter_footprint_100))

for t in thresh_use:
  t3 = time.time()
  exceed3d = np.where(np.greater_equal(ensemble_qpf/25.4,t),1,0)
  exceed_sum=np.sum(exceed3d)
  optrad = calculate_eas_probability_para(ensemble_qpf,t,rlist,alpha,dx,p_smooth)
  t4 = time.time()
  print('Time for optrad routine:', t4-t3)
  pnt_prob = calculate_pnt_probability (ensemble_qpf, t, p_smooth)


# how know the filter_footprint size for near boundary points?


## need something to account for reduced portion of filter_footprint actually within domain
#
#
  print('nm_use for final prob: ', nm_use)
  t8a = time.time()

  if exceed_sum > 0:
    probfinal, optrad = optimize_footprint_calculation(
    optrad, prob, t, nm_use, nlats, nlons, dx,
    filter_footprint_10, filter_footprint_25, filter_footprint_40, 
    filter_footprint_55, filter_footprint_70, filter_footprint_85,
    filter_footprint_100, get_footprint_flexi)
  else:
    print('sum of exceed3d was zero, so setting zero probfinal')
    probfinal[:,:] = 0.0



  t8 = time.time()
# slight smoothing of probfinal
  print('Time just over loops for ',t,' threshold: ', t8-t8a)
  probfinal = ndimage.filters.gaussian_filter(probfinal,1)

  if dom == 'ak':
    probfinal = np.where(np.equal(maskregion,-9999),0,probfinal)  # set to 0 for mask 

  t5 = time.time()

# cap at 100
  print('max of probfinal pre cap: ', np.max(probfinal))
  probfinal = np.where(probfinal > 100.0,100.0,probfinal)
  print('Time for get final probability routine for ',t, 'inch threshold: ',t5-t4)
  print('max of probfinal post cap: ', np.max(probfinal))
  print('mean of probfinal: ', np.mean(probfinal))
  print('probfinal dims: ', np.shape(probfinal))


  probstr=str(t*25.4)
  byte=int(m.ceil(t*25.4*1000))
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

  string="0:0:d="+wgribdate+":APCP:surface:"+fhr_range+" hour acc fcst:prob >"+probstr+":"

  os.system(WGRIB2+' '+template+' -import_bin record_out.bin -set_metadata_str "'+string+'" -set_grib_type c3 -grib_out premod.grb')
  os.system(WGRIB2+' premod.grb -set_byte 4 14 136 -set_byte 4 12 197 -set_byte 4 17 0 -set_byte 4 24:35 0:0:0:0:0:255:0:0:0:0:0:0 -set_byte 4 36 '+str(nm_use)+' -set_byte 4 38:42 0:0:0:0:0 -set_byte 4 43 3 -set_byte 4 44 0 -set_byte 4 45 '+str(byte45)+' -set_byte 4 46 '+str(byte46)+' -set_byte 4 47 '+str(byte47)+' -set_byte 4 67 1 -append  -set_grib_type c3 -grib_out '+outfile)

# can we add something to define REFS 136 generation code?

  print('Wrote ', qpf_interval, ' PQPF to:',outfile, 'for ',t, 'inch threshold')
  os.system('rm record_out.bin')
  os.system('rm premod.grb')
  print('exiting ush/enspost_make_easfracqpf_combo.py ...')
