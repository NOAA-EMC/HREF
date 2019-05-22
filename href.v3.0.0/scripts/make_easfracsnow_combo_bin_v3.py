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
# 2018-03-21  M Pyle    pygrib free version

import os, sys, time
import numpy as np
import math as m
from datetime import datetime, timedelta
from scipy import ndimage, optimize, signal
# from scipy.stats import threshold
# from netCDF4 import Dataset
import fortranfile as F


WGRIB2 = '/nwprod2/grib_util.v1.0.0/exec/wgrib2'
WGRIB2 = '/gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/git_repo/EMC_hrw/grib_util.v1.0.6/exec/wgrib2'

starttime = time.time()

print 'Processing probabilistic snow'

try:
  os.environ["HOMEhref"]
except KeyError:
  print "NEED TO DEFINE HOMEhref"
  exit(1)
HOMEhref=os.environ.get('HOMEhref','trash')
print 'found HOMEhref as ', HOMEhref

try:
  os.environ["COMINnam"]
except KeyError:
  print "NEED TO DEFINE COMINnam"
  exit(1)
COMINnam=os.environ.get('COMINnam','trash')
print 'found COMINnam as ', COMINnam

try:
  os.environ["COMINhiresw"]
except KeyError:
  print "NEED TO DEFINE COMINhiresw"
  exit(1)
COMINhiresw=os.environ.get('COMINhiresw','trash')
print 'found COMINhiresw as ', COMINhiresw

try:
  os.environ["COMINhrrr"]
except KeyError:
  print "NEED TO DEFINE COMINhrrr"
  exit(1)
COMINhrrr=os.environ.get('COMINhrrr','trash')
print 'found COMINhrrr as ', COMINhrrr

try:
  os.environ["COMINfv3"]
except KeyError:
  print "NEED TO DEFINE COMINfv3"
  exit(1)
COMINfv3=os.environ.get('COMINfv3','trash')
print 'found COMINfv3 as ', COMINfv3

try:
  os.environ["COMOUT"]
except KeyError:
  print "NEED TO DEFINE COMOUT"
  exit(1)
COMOUT=os.environ.get('COMOUT','trash')
print 'found COMOUT as ', COMOUT

try:
  os.environ["PDY"]
except KeyError:
  print "NEED TO DEFINE PDY"
  exit(1)
PDY=os.environ.get('PDY','trash')
print 'found PDY as ', PDY

try:
  os.environ["cyc"]
except KeyError:
  print "NEED TO DEFINE cyc"
  exit(1)
cyc=os.environ.get('cyc','trash')
print 'found cyc as ', cyc

try:
  os.environ["dom"]
except KeyError:
  print "NEED TO DEFINE dom"
  exit(1)
dom=os.environ.get('dom','trash')
print 'found dom as: ', dom


try:
  os.environ["DATA"]
except KeyError:
  print "NEED TO DEFINE DATA"
  exit(1)
DATA=os.environ.get('DATA','trash')
print 'found DATA as ', DATA

# input directory and config file
sys.path.append(HOMEhref)
from eas_config import *
# grib-2 template 
template = HOMEhref + '/fix/weasd_' + dom + 'template.grib2'

record = 1		# SNOW from SREF pgrb212 file

# get latest run times and create output directory for plots if it doesn't already exist
if not os.path.exists(COMOUT):
  os.system("mkdir -p " + COMOUT)

# accumulation interval (hours)
fcst_hour = int(sys.argv[1])
qpf_interval = int(sys.argv[2])

DATArun=DATA+'/snow_'+str(fcst_hour)
if not os.path.exists(DATArun):
  os.system("mkdir -p "+DATArun)
os.system("cd "+DATArun)

start_hour = int(fcst_hour - qpf_interval)

fhr_range=str(start_hour)+'-'+str(fcst_hour)


print 'fcst_hour, qpf_interval, start_hour: ', fcst_hour, qpf_interval, start_hour

# maximum radius (km)
slim = max(rlist)
alpha = 0.5




#------------------------------------------------------------------------------------------
# read in calibration coefficients

if dom == 'conus':
#  members = ['arw','fv3s','nssl','hrrr','nam']
  members = ['arw','nssl','hrrr','nam']
elif dom == 'ak':
  members = ['arw','nmmb','nssl','hrrrak']
else:
  members = ['arw','nmmb','nssl']

pqpf_6h_calibrate = 'no'
pqpf_3h_calibrate = 'no'
# print 'Calibration coefficients for '+mem+' members not found. Using raw model QPF'
   
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


def process_nam_qpf(file3,file4,fhr):


    fhour=fhr
    if fhr%3 is 1:
      shour=fhour-1
      print 'process_nam_qpf remainder 1'
      os.system(WGRIB2+' '+file3+' -match "WEASD:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf.txt ')
      qpf1,nx,ny=simplewgrib2('qpf.txt')

#      idx = pygrib.index(file3,'name','lengthOfTimeRange')
#      grb = idx(name='Water equivalent of accumulated snow depth', lengthOfTimeRange=1)[0]
#      qpf1 = grb.values
#      idx.close()

    if fhr%3 is 2:
      shour1=fhour-2
      shour2=fhour-1
      print 'process_nam_qpf remainder 2 - f02 minus f01'
      os.system(WGRIB2+' '+file3+' -match "WEASD:surface:%i'%shour1+'-%i'%fhour+'" -end -text qpf.txt')
      qpfa,nx,ny=simplewgrib2('qpf.txt')
      os.system('rm -f qpf.txt')

      print 'shour1: ', shour1
      print 'shour2: ', shour2
      print 'file4: ', file4
      os.system(WGRIB2+' '+file4+' -match "WEASD:surface:%i'%shour1+'-%i'%shour2+'" -end -text qpf.txt')
      qpfb,nx,ny=simplewgrib2('qpf.txt')
      os.system('rm -f qpf.txt')

#      idx = pygrib.index(file3,'name','lengthOfTimeRange')
#      grb = idx(name='Water equivalent of accumulated snow depth', lengthOfTimeRange=2)[0]
#      qpfa = grb.values
#      idx.close()
#      idx = pygrib.index(file4,'name','lengthOfTimeRange')
#      grb = idx(name='Water equivalent of accumulated snow depth', lengthOfTimeRange=1)[0]
#      qpfb = grb.values
#      idx.close()

      qpf1=qpfa-qpfb

    if fhr%3 is 0:
      shour1=fhour-3
      shour2=fhour-2
      fhourm1=fhour-1
      print 'process_nam_qpf remainder 3 - f03 minus f02'
      os.system(WGRIB2+' '+file3+' -match "WEASD:surface:%i'%shour1+'-%i'%fhour+'" -end -text qpf.txt')
      qpfa,nx,ny=simplewgrib2('qpf.txt')
      os.system('rm -f qpf.txt')

      os.system(WGRIB2+' '+file4+' -match "WEASD:surface:%i'%shour1+'-%i'%fhourm1+'" -end -text qpf.txt')
      qpfb,nx,ny=simplewgrib2('qpf.txt')
      os.system('rm -f qpf.txt')

#      idx = pygrib.index(file3,'name','lengthOfTimeRange')
#      grb = idx(name='Water equivalent of accumulated snow depth', lengthOfTimeRange=3)[0]
#      qpfa = grb.values
#      idx.close()
#      idx = pygrib.index(file4,'name','lengthOfTimeRange')
#      grb = idx(name='Water equivalent of accumulated snow depth', lengthOfTimeRange=2)[0]
#      qpfb = grb.values
#      idx.close()
      qpf1=qpfa-qpfb

    return qpf1


# calculate footprint routine
def get_footprint(r):
    footprint = (np.ones(((r/dx)*2+1,(r/dx)*2+1))).astype(int)
    footprint[int(m.ceil(r/dx)),int(m.ceil(r/dx))] = 0
    dist = ndimage.distance_transform_edt(footprint,sampling=[dx,dx])
    footprint = np.where(np.greater(dist,r),0,1)
    return footprint

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
    nm, isize, jsize = np.shape(exceed3d)
    print 'np.shape: ', nm, isize, jsize
    pr1, pr2 = [], []  # create lists of ens member pairs
    for m1 in range(nm-1):
      for m2 in range(m1+1,nm):
        pr1.append(m1)
        pr2.append(m2)
    optrad = np.zeros((isize,jsize)).astype(float) + float(max(rlist)) # set initial radius to max for better smoothing
    rlist = sorted(rlist,reverse=True)
    for i in range(len(rlist)):  # loop through rlist to find smallest radius over which member pairs meet similarity criteria
      dcrit = alpha
#      dcrit = alpha + ((1 - alpha) * (rlist[i] / float(max(rlist))))
      footprint = get_footprint(rlist[i])
      dijmean = np.zeros((isize,jsize)).astype(float)
      frac = np.zeros((nm,isize,jsize)).astype(float)
      for mem in range(nm):
        frac[mem,:,:] = np.around(signal.fftconvolve(exceed3d[mem,:,:],footprint,mode='same'))/float(np.sum(footprint))
      if i == 0:  # identify points where QPF threshold is not met within the maximum radius in any ens member
        fracsum = np.sum(frac,axis=0)
      for pair in range(len(pr1)):
        dijmean = dijmean + np.where(np.logical_and(np.greater(frac[pr1[pair]],0),np.greater(frac[pr2[pair]],0)),(frac[pr1[pair]]-frac[pr2[pair]])**2/(frac[pr1[pair]]**2+frac[pr2[pair]]**2),1)/float(len(pr1))
      optrad=np.where(np.less_equal(dijmean,dcrit),rlist[i],optrad)
#      p = np.where(np.less_equal(dijmean,dcrit),100.0*np.sum(frac,axis=0)/float(nm),p)
# smooth radius grid and zero out any dry areas
#    p = np.where(np.equal(fracsum,0),0,ndimage.filters.gaussian_filter(p,p_smooth))
    optrad = np.where(np.equal(fracsum,0),slim+5,ndimage.filters.gaussian_filter(optrad,p_smooth))
    return optrad


#--------------------------------------------------------------------------------
#### START OF SCRIPT ####

# for start_hour in pqpf_6h_starthours:
cy, cm, cd, ch = int(PDY[0:4]), int(PDY[4:6]), int(PDY[6:8]), int(cyc[0:2])
d0 = datetime(cy,cm,cd,ch,0)
starttime = d0+timedelta(start_hour/24.0)
endtime = d0+timedelta((start_hour+qpf_interval)/24.0)
memfiles = {}
memfiles4 = {}
latency4 = {}
itimes = []
fhours = []

latency = min_latency
stop = max_latency
stopnam = max_latency_nam

wgribdate=PDY+cyc

# get dimensions and message from template file
# grbs = pygrib.open(template)
# grbtmp = grbs[record]
# lats, lons = grbtmp.latlons()
# grbs.close()

os.system(WGRIB2+' '+template+' -rpn rcl_lat -text lat.txt  -rpn rcl_lon -text lon.txt')

lons,nx,ny=simplewgrib2('lon.txt')
lats,nx,ny=simplewgrib2('lat.txt')

nlats, nlons = np.shape(lats)

# define mask - NAM nest grid interpolated to grid 227 has undefined values
print 'dom here at decision point: ', dom
if dom == 'conus':
  print 'defining conus maskfile'
  maskfile = HOMEhref + '/fix/nam_mask.grib2'

print 'dom here at decision point2: ', dom
if dom == 'ak':
  print 'defining ak maskfile'
  maskfile = HOMEhref + '/fix/akhref_mask.grib2'

print 'dom here at decision point3: ', dom
if dom == 'conus' or dom == 'ak':
  print 'opening the maskregion stuff'
  print 'maskfile is: ', maskfile
#  grbs2 = pygrib.open(maskfile)
#  undefmask = grbs2[1].values

  os.system(WGRIB2+' '+maskfile+'  -text mask.txt ')
  undefmask,nx,ny=simplewgrib2('mask.txt')
  undefmask=np.ma.masked_greater(undefmask,9.0e+20)
  maskregion = np.ma.filled(undefmask,-9999)
#  grbs2.close()


# create grib messages from template (only need to do this once)
# grbtmp['dataDate']=int('%i'%d0.year+'%02d'%d0.month+'%02d'%d0.day)
# grbtmp['dataTime']=int('%02d'%d0.hour+'00')
# grbtmp['startStep']=int(start_hour)
# grbtmp['endStep']=int(start_hour+qpf_interval)
# grbtmp['yearOfEndOfOverallTimeInterval']=endtime.year
# grbtmp['monthOfEndOfOverallTimeInterval']=endtime.month
# grbtmp['dayOfEndOfOverallTimeInterval']=endtime.day
# grbtmp['hourOfEndOfOverallTimeInterval']=endtime.hour
# grbtmp['scaleFactorOfUpperLimit']=3

if qpf_interval == 1:
  print 'defined 1 h outbase'
  outbase = 'href.t'+cyc[0:2]+'z.'+dom+'.snow01_easfrac.f%02d'%(start_hour+qpf_interval)+'.grib2'
#  outbase_low = 'href.t'+cyc[0:2]+'z.'+dom+'.snow01_easlowfrac.f%02d'%(start_hour+qpf_interval)+'.grib2'
#  outbase_high = 'href.t'+cyc[0:2]+'z.'+dom+'.snow01_eashighfrac.f%02d'%(start_hour+qpf_interval)+'.grib2'
  incr =  1
if qpf_interval == 3:
  print 'defined 3 h outbase'
  outbase = 'href.t'+cyc[0:2]+'z.'+dom+'.snow03_easfrac.f%02d'%(start_hour+qpf_interval)+'.grib2'
#  outbase_low = 'href.t'+cyc[0:2]+'z.'+dom+'.snow03_easlowfrac.f%02d'%(start_hour+qpf_interval)+'.grib2'
#  outbase_high = 'href.t'+cyc[0:2]+'z.'+dom+'.snow03_eashighfrac.f%02d'%(start_hour+qpf_interval)+'.grib2'
  incr =  3
if qpf_interval == 6:
  print 'defined 6 h outbase'
  outbase = 'href.t'+cyc[0:2]+'z.'+dom+'.snow06_easfrac.f%02d'%(start_hour+qpf_interval)+'.grib2'
#  outbase_low = 'href.t'+cyc[0:2]+'z.'+dom+'.snow06_easlowfrac.f%02d'%(start_hour+qpf_interval)+'.grib2'
#  outbase_high = 'href.t'+cyc[0:2]+'z.'+dom+'.snow06_eashighfrac.f%02d'%(start_hour+qpf_interval)+'.grib2'
  incr =  3

outfile = DATA  + '/' + outbase
# outfile_low = DATA  + '/' + outbase_low
# outfile_high = DATA  + '/' + outbase_high

if os.path.exists(outfile):
  os.system('rm -f '+outfile)
# if os.path.exists(outfile_low):
#  os.system('rm -f '+outfile_low)
# if os.path.exists(outfile_high):
#   os.system('rm -f '+outfile_high)

prob = {}
qpf = {}
memcount = 0
check = 0
check_hrrr = 0

print 'members: ', members

for mem in members:
  while (len(itimes) < memcount+2) and (latency <= stop) and (start_hour+qpf_interval+latency <= 48):
    print len(itimes), memcount+2
    itime = starttime-timedelta((start_hour+latency)/24.0)
    itime_alt = starttime-timedelta((start_hour+latency+6)/24.0)
    if mem == 'arw':
      file3 = COMINhiresw + '/hiresw.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day + '/hiresw.t%02d'%itime.hour+'z.arw_5km.f%02d'%(start_hour+latency+incr)+'.'+dom+'.grib2'
      file3alt = COMINhiresw + '/hiresw.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day + '/hiresw.t%02d'%itime_alt.hour+'z.arw_5km.f%02d'%(start_hour+latency+incr+6)+'.'+dom+'.grib2'
      file6 = COMINhiresw + '/hiresw.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day + '/hiresw.t%02d'%itime.hour+'z.arw_5km.f%02d'%(start_hour+latency+incr+incr)+'.'+dom+'.grib2'
      file6alt = COMINhiresw + '/hiresw.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day + '/hiresw.t%02d'%itime_alt.hour+'z.arw_5km.f%02d'%(start_hour+latency+incr+incr+6)+'.'+dom+'.grib2'

    elif mem == 'nmmb':
      file3 = COMINhiresw + '/hiresw.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day + '/hiresw.t%02d'%itime.hour+'z.nmmb_5km.f%02d'%(start_hour+latency+incr)+'.'+dom+'.grib2'
      file3alt = COMINhiresw + '/hiresw.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day + '/hiresw.t%02d'%itime_alt.hour+'z.nmmb_5km.f%02d'%(start_hour+latency+incr+6)+'.'+dom+'.grib2'
      file6 = COMINhiresw + '/hiresw.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day + '/hiresw.t%02d'%itime.hour+'z.nmmb_5km.f%02d'%(start_hour+latency+incr+incr)+'.'+dom+'.grib2'
      file6alt = COMINhiresw + '/hiresw.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day + '/hiresw.t%02d'%itime_alt.hour+'z.nmmb_5km.f%02d'%(start_hour+latency+incr+incr+6)+'.'+dom+'.grib2'

    elif mem == 'nssl':
      file3 = COMINhiresw + '/hiresw.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day + '/hiresw.t%02d'%itime.hour+'z.arw_5km.f%02d'%(start_hour+latency+incr)+'.'+dom+'mem2.grib2'
      file3alt = COMINhiresw + '/hiresw.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day + '/hiresw.t%02d'%itime_alt.hour+'z.arw_5km.f%02d'%(start_hour+latency+incr+6)+'.'+dom+'mem2.grib2'
      file6 = COMINhiresw + '/hiresw.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day + '/hiresw.t%02d'%itime.hour+'z.arw_5km.f%02d'%(start_hour+latency+incr+incr)+'.'+dom+'mem2.grib2'
      file6alt = COMINhiresw + '/hiresw.%02d'%itime_alt.year+'%02d'%itime_alt.month+'%02d'%itime_alt.day + '/hiresw.t%02d'%itime_alt.hour+'z.arw_5km.f%02d'%(start_hour+latency+incr+incr+6)+'.'+dom+'mem2.grib2'

    elif mem == 'fv3s':
      file3 = COMINfv3 + '/fv3.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/fv3s.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency+incr)+'.grib2'
      file4 = COMINfv3 + '/fv3.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/fv3s.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency)+'.grib2'
      memfiles4[itime] = file4
      file6 = COMINfv3 + '/fv3.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/fv3s.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency+incr+incr)+'.grib2'
      file3alt = 'garb'
      file6alt = 'garb'

    elif mem == 'nam':
      file3 = COMINnam + '/nam.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/nam.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency+incr)+'.grib2'
      file4 = COMINnam + '/nam.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/nam.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency)+'.grib2'
      memfiles4[itime] = file4
      file6 = COMINnam + '/nam.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/nam.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency+incr+incr)+'.grib2'
      file3alt = 'garb'
      file6alt = 'garb'

    elif mem == 'hrrr':
      file3 = COMINhrrr + '/hrrr.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/hrrr.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency+incr)+'.grib2'
      file6 = COMINhrrr + '/hrrr.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/hrrr.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency+incr+incr)+'.grib2'
      file3alt = 'garb'
      file6alt = 'garb'

    elif mem == 'hrrrak':
      file3 = COMINhrrr + '/hrrr.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/hrrr.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency+incr)+'.ak.grib2'
      file6 = COMINhrrr + '/hrrr.%02d'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'/hrrr.t%02d'%itime.hour+'z.f%02d'%(start_hour+latency+incr+incr)+'.ak.grib2'
      file3alt = 'garb'
      file6alt = 'garb'

    if qpf_interval != 6:
      if os.path.exists(file3):
        print 'Found:',itime,'forecast hour',(start_hour+qpf_interval+latency)
        fhours.append(start_hour+qpf_interval+latency)
        itimes.append(itime)
        memfiles[itime] = file3
        print 'utilizing file3 in memfiles: ', file3
      else:
        print 'Missing:',itime,'forecast hour',(start_hour+qpf_interval+latency)
        if os.path.exists(file3alt):
          print 'alt Found:',itime_alt,'forecast hour',(start_hour+qpf_interval+latency)
          fhours.append(start_hour+qpf_interval+latency+6)
          print 'defined file3 fhours: ', start_hour+qpf_interval+latency+6
          itimes.append(itime_alt)
          memfiles[itime_alt] = file3alt
          print 'using file3alt which is: ', file3alt
        else:
          print 'Completely missing:',itime,'forecast hour',(start_hour+qpf_interval+latency)
    else:
      print 'using 6 h block portion'
      if os.path.exists(file3) and os.path.exists(file6):
        print 'Found:',itime,'forecast hour',(start_hour+qpf_interval+latency)
        itimes.append(itime)
        fhours.append(start_hour+qpf_interval+latency)
        print 'defined file3 for 6h fhours: ', start_hour+qpf_interval+latency
        memfiles[itime] = [file3,file6]
      else:
        print 'Missing:',itime,'forecast hour',(start_hour+qpf_interval+latency)
        if os.path.exists(file3alt) and os.path.exists(file6alt):
          print 'alt Found:',itime_alt,'forecast hour',(start_hour+qpf_interval+latency)
          itimes.append(itime_alt)
          fhours.append(start_hour+qpf_interval+latency+6)
          print 'defined fhours in alt 6h block: ', start_hour+qpf_interval+latency+6
          memfiles[itime_alt] = [file3alt,file6alt]
          print 'using file3alt which is: ', file3alt
          print 'using file6alt which is: ', file6alt
        else:
          print 'Even alt is missing:',itime,'forecast hour',(start_hour+qpf_interval+latency)

    if mem == 'nam' or mem == 'hrrr' or mem == 'hrrrak' :
      latency = latency + 6
    else:
      print '12 hour latency member'
      latency = latency + 12

  if len(itimes) == (memcount+2):
    print 'Found 2 '+mem+' members valid:',starttime,'-',endtime
  else:
    print 'Could not find 2 '+mem+' members valid for start hour',start_hour
#    sys.exit(1)

#### READ IN QPF ####
  print 'here a memcount: ', memcount
  for itime in itimes[memcount:memcount+2]:	# Hi Res Window ARW members
    if qpf_interval == 6:
      file3,file6 = memfiles[itime]
    else:
      file3 = memfiles[itime]

    if dom == 'conus':
      print 'Processing member',(1+memcount),'of',nm
    if dom != 'conus':
      print 'Processing member',(1+memcount),'of',nm_nonconus
    print 'file3 near read is ', file3

    if qpf_interval != 1:
      if qpf_interval == 3:
        fhour=fhours[memcount]
        shour=fhour-3
      if qpf_interval == 6:
        fhour=fhours[memcount]-3
        shour=fhour-3
      print 'for file3 shour fhour: ', shour,fhour
      os.system(WGRIB2+' '+file3+' -match "WEASD:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf.txt')
      qpf3,nx,ny=simplewgrib2('qpf.txt')
    else:

### HERE 

      if mem == 'nam':
        fcst_hour=fhours[memcount]
        file4 = memfiles4[itime]

        print 'call process_nam_qpf with file3: ', file3
        print 'call process_nam_qpf with file4: ', file4
        print 'call process_nam_qpf with fcst_hour: ', fcst_hour
        qpf1=process_nam_qpf(file3,file4,fcst_hour)

      else:
        print 'ready from file3 in 1h: ', file3
        fhour=fhours[memcount]
        shour=fhour-1
        os.system(WGRIB2+' '+file3+' -match "WEASD:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf.txt')
        qpf1,nx,ny=simplewgrib2('qpf.txt')

#        idx = pygrib.index(file3,'name','lengthOfTimeRange')
#        grb = idx(name='Water equivalent of accumulated snow depth', lengthOfTimeRange=1)[0]
#        qpf1 = grb.values
#        idx.close()
      
      print 'defining qpf from qpf1'
      qpf[itime] = qpf1*0.39370079

    if qpf_interval == 6:
#      idx = pygrib.index(file6,'name','lengthOfTimeRange')
#      grb = idx(name='Water equivalent of accumulated snow depth',lengthOfTimeRange=3)[0]
#      qpf6 = grb.values
#      idx.close()
      fhour=fhours[memcount]
      shour=fhour-3
      print '6h shour fhour: ', shour,fhour
      os.system(WGRIB2+' '+file6+' -match "WEASD:surface:%i'%shour+'-%i'%fhour+'" -end -text qpf.txt')
      qpf6,nx,ny=simplewgrib2('qpf.txt')
      qpf[itime] = (qpf3 + qpf6) * 0.39370079
    if qpf_interval == 3:
      qpf[itime] = qpf3*0.39370079   # Apply 10:1 SLR, convert to inches

    
    print 'max of qpf as snow: ', np.max(qpf[itime])
    print 'dom is: ', dom
    if dom == 'conus' or dom == 'ak':
      print 'qpf shape: ', np.shape(qpf)
      print 'maskregion shape: ', np.shape(maskregion)
      qpf[itime] = np.where(np.equal(maskregion,-9999),0,qpf[itime])

    # calculate threshold exceedance
    if qpf_interval == 1:
      thresh_use=snow_1h_thresh
    if qpf_interval == 3:
      thresh_use=snow_3h_thresh
    if qpf_interval == 6:
      thresh_use=snow_6h_thresh

    for t in thresh_use:
      for size in rlist:
        if memcount == 0:
          prob[t,size] = np.zeros((nlats,nlons))
        exceed = np.where(np.greater_equal(qpf[itime],t),1,0)
        filter_footprint = get_footprint(size)
        prob[t,size] = prob[t,size] + signal.fftconvolve(exceed,filter_footprint,mode='same')

    memcount = memcount + 1
  latency = min_latency


#-------------------------------------------------------------------#

# Get final probabilities

# redefine number of members (in case a TL member is missing)
nm = len(itimes)
nm_use=nm
print 'nm members is: ', nm
ensemble_qpf = np.zeros((nm,nlats,nlons)).astype(float)
for mem in range(0,len(itimes)):
  print 'mem in range is: ', mem
  ensemble_qpf[mem,:,:] = qpf[itimes[mem]]

# Get final probabilities
probfinal = np.zeros((nlats,nlons))
# probfinal_low = np.zeros((nlats,nlons))
# probfinal_high = np.zeros((nlats,nlons))

filter_footprint_10 = get_footprint(10)
filter_footprint_25 = get_footprint(25)
filter_footprint_40 = get_footprint(40)
filter_footprint_55 = get_footprint(55)
filter_footprint_70 = get_footprint(70)
filter_footprint_85 = get_footprint(85)
filter_footprint_100 = get_footprint(100)

for t in thresh_use:
  t3 = time.time()
  optrad = calculate_eas_probability(ensemble_qpf,t,rlist,alpha,dx,p_smooth)
#  optrad_low = calculate_eas_probability(ensemble_qpf,t,rlist,alpha,dx,p_smooth_low)
#  optrad_high = calculate_eas_probability(ensemble_qpf,t,rlist,alpha,dx,p_smooth_high)
  t4 = time.time()
  print 'Time for optrad routine:', t4-t3

  for row in range((slim/dx),nlats - (slim/dx)):
    for column in range((slim/dx),nlons - (slim/dx)):
      rad = (optrad[row,column]).astype(int)
#      rad_low = (optrad_low[row,column]).astype(int)
#      rad_high = (optrad_high[row,column]).astype(int)


      if (2.5 <= rad < 17.5):
        probfinal[row,column] = prob[t,10][row,column]
        probfinal[row,column] = 100.0*probfinal[row,column] / float(np.sum(filter_footprint_10)*nm)
      elif (17.5 <= rad < 32.5):
        probfinal[row,column] = prob[t,25][row,column]
        probfinal[row,column] = 100.0*probfinal[row,column] / float(np.sum(filter_footprint_25)*nm)    
      elif (32.5 <= rad < 47.5):
        probfinal[row,column] = prob[t,40][row,column]
        probfinal[row,column] = 100.0*probfinal[row,column] / float(np.sum(filter_footprint_40)*nm)
      elif (47.5 <= rad < 62.5):
        probfinal[row,column] = prob[t,55][row,column]
        probfinal[row,column] = 100.0*probfinal[row,column] / float(np.sum(filter_footprint_55)*nm)
      elif (62.5 <= rad < 77.5):
        probfinal[row,column] = prob[t,70][row,column]
        probfinal[row,column] = 100.0*probfinal[row,column] / float(np.sum(filter_footprint_70)*nm)
      elif (77.5 <= rad < 92.5):
        probfinal[row,column] = prob[t,85][row,column]
        probfinal[row,column] = 100.0*probfinal[row,column] / float(np.sum(filter_footprint_85)*nm)
      elif (92.5 <= rad <= 100):
        probfinal[row,column] = prob[t,100][row,column]
        probfinal[row,column] = 100.0*probfinal[row,column] / float(np.sum(filter_footprint_100)*nm)
      elif (rad > 100):
        probfinal[row,column] = prob[t,100][row,column]
        probfinal[row,column] = 100.0*probfinal[row,column] / float(np.sum(filter_footprint_100)*nm)
        optrad[row,column] = 0

## probfinal_low
#      if (2.5 <= rad_low < 17.5):
#        probfinal_low[row,column] = prob[t,10][row,column]
#        probfinal_low[row,column] = 100.0*probfinal_low[row,column] / float(np.sum(filter_footprint_10)*nm)
#      elif (17.5 <= rad_low < 32.5):
#        probfinal_low[row,column] = prob[t,25][row,column]
#        probfinal_low[row,column] = 100.0*probfinal_low[row,column] / float(np.sum(filter_footprint_25)*nm)
#      elif (32.5 <= rad_low < 47.5):
#        probfinal_low[row,column] = prob[t,40][row,column]
#        probfinal_low[row,column] = 100.0*probfinal_low[row,column] / float(np.sum(filter_footprint_40)*nm)
#      elif (47.5 <= rad_low < 62.5):
#        probfinal_low[row,column] = prob[t,55][row,column]
#        probfinal_low[row,column] = 100.0*probfinal_low[row,column] / float(np.sum(filter_footprint_55)*nm)
#      elif (62.5 <= rad_low < 77.5):
#        probfinal_low[row,column] = prob[t,70][row,column]
#        probfinal_low[row,column] = 100.0*probfinal_low[row,column] / float(np.sum(filter_footprint_70)*nm)
#      elif (77.5 <= rad_low < 92.5):
#        probfinal_low[row,column] = prob[t,85][row,column]
#        probfinal_low[row,column] = 100.0*probfinal_low[row,column] / float(np.sum(filter_footprint_85)*nm)
#      elif (92.5 <= rad_low <= 100):
#        probfinal_low[row,column] = prob[t,100][row,column]
#        probfinal_low[row,column] = 100.0*probfinal_low[row,column] / float(np.sum(filter_footprint_100)*nm)
#      elif (rad_low > 100):
#        probfinal_low[row,column] = prob[t,100][row,column]
#        probfinal_low[row,column] = 100.0*probfinal_low[row,column] / float(np.sum(filter_footprint_100)*nm)
#        optrad_low[row,column] = 0


## probfinal_high
#      if (2.5 <= rad_high < 17.5):
#        probfinal_high[row,column] = prob[t,10][row,column]
#        probfinal_high[row,column] = 100.0*probfinal_high[row,column] / float(np.sum(filter_footprint_10)*nm)
#      elif (17.5 <= rad_high < 32.5):
#        probfinal_high[row,column] = prob[t,25][row,column]
#        probfinal_high[row,column] = 100.0*probfinal_high[row,column] / float(np.sum(filter_footprint_25)*nm)
#      elif (32.5 <= rad_high < 47.5):
#        probfinal_high[row,column] = prob[t,40][row,column]
#        probfinal_high[row,column] = 100.0*probfinal_high[row,column] / float(np.sum(filter_footprint_40)*nm)
#      elif (47.5 <= rad_high < 62.5):
#        probfinal_high[row,column] = prob[t,55][row,column]
#        probfinal_high[row,column] = 100.0*probfinal_high[row,column] / float(np.sum(filter_footprint_55)*nm)
#      elif (62.5 <= rad_high < 77.5):
#        probfinal_high[row,column] = prob[t,70][row,column]
#        probfinal_high[row,column] = 100.0*probfinal_high[row,column] / float(np.sum(filter_footprint_70)*nm)
#      elif (77.5 <= rad_high < 92.5):
#        probfinal_high[row,column] = prob[t,85][row,column]
#        probfinal_high[row,column] = 100.0*probfinal_high[row,column] / float(np.sum(filter_footprint_85)*nm)
#      elif (92.5 <= rad_high <= 100):
#        probfinal_high[row,column] = prob[t,100][row,column]
#        probfinal_high[row,column] = 100.0*probfinal_high[row,column] / float(np.sum(filter_footprint_100)*nm)
#      elif (rad_high > 100):
#        probfinal_high[row,column] = prob[t,100][row,column]
#        probfinal_high[row,column] = 100.0*probfinal_high[row,column] / float(np.sum(filter_footprint_100)*nm)
#        optrad_high[row,column] = 0


# slight smoothing of probfinal?
  probfinal = ndimage.filters.gaussian_filter(probfinal,1)
#  probfinal_low = ndimage.filters.gaussian_filter(probfinal_low,1)
#  probfinal_high = ndimage.filters.gaussian_filter(probfinal_high,1)

  if dom == 'conus' or dom == 'ak':
    print 'working final probability with mask'
    probfinal = np.where(np.equal(maskregion,-9999),0,probfinal)  # set to 0 for mask 
#    probfinal_low = np.where(np.equal(maskregion,-9999),0,probfinal_low)  # set to 0 for mask 
#    probfinal_high = np.where(np.equal(maskregion,-9999),0,probfinal_high)  # set to 0 for mask 
  t5 = time.time()
  print 'Time for get final probability routine for ',t, 'inch threshold: ',t5-t4
  print 'max of probfinal: ', np.max(probfinal)


  
  probstr=str(t*2.54)
  print 'probstr is: ', probstr

  byte=int(t*2.54*1000)
  byte44=0
  byte45=int(byte/65536)
  byte45rem=byte%65536
  byte46=int(byte45rem/256)
  byte47=byte45rem%256

# write binary file out of probfinal array, then import it into grib file using WGRIB2

  myfort = F.FortranFile('record_out.bin',mode='w')
  myfort.writeReals(probfinal)
  myfort.close()

  string="0:0:d="+wgribdate+":WEASD:surface:"+fhr_range+" hour acc fcst:prob > "+probstr
  print 'string used: ', string
  os.system(WGRIB2+' '+template+' -import_bin record_out.bin -set_metadata_str "'+string+'" -set_grib_type c3 -grib_out premod.grb')
  os.system(WGRIB2+' premod.grb -set_byte 4 12 197 -set_byte 4 17 0 -set_byte 4 24:35 0:0:0:0:0:255:0:0:0:0:0:0 -set_byte 4 36 '+str(nm_use)+' -set_byte 4 38:42 0:0:0:0:0 -set_byte 4 43 3 -set_byte 4 44 0 -set_byte 4 45 '+str(byte45)+' -set_byte 4 46 '+str(byte46)+' -set_byte 4 47 '+str(byte47)+' -append  -set_grib_type c3 -grib_out '+outfile)

  print 'byte, byte45, byte46, byte47: ', byte, byte45, byte46, byte47

  os.system('rm record_out.bin')
  os.system('rm premod.grb')

# low

# write binary file out of probfinal array, then import it into grib file using WGRIB2
#  myfort_low = F.FortranFile('record_out.bin',mode='w')
#  myfort_low.writeReals(probfinal_low)
#  myfort_low.close()
#
#  os.system(WGRIB2+' '+template+' -import_bin record_out.bin -set_metadata_str "'+string+'" -set_grib_type c3 -grib_out premod.grb')
#  os.system(WGRIB2+' premod.grb -set_byte 4 12 197 -set_byte 4 17 0  -set_byte 4 24:35 0:0:0:0:0:255:0:0:0:0:0:0 -set_byte 4 36 '+str(nm_use)+' -set_byte 4 38:42 0:0:0:0:0 -set_byte 4 43 3 -set_byte 4 44 0 -set_byte 4 45 '+str(byte45)+' -set_byte 4 46 '+str(byte46)+' -set_byte 4 47 '+str(byte47)+' -append  -set_grib_type c3 -grib_out '+outfile_low)

#  os.system('rm record_out.bin')
#  os.system('rm premod.grb')

# high

# write binary file out of probfinal array, then import it into grib file using WGRIB2
#  myfort_high = F.FortranFile('record_out.bin',mode='w')
#  myfort_high.writeReals(probfinal_high)
#  myfort_high.close()
#
#  os.system(WGRIB2+' '+template+' -import_bin record_out.bin -set_metadata_str "'+string+'" -set_grib_type c3 -grib_out premod.grb')
#  os.system(WGRIB2+' premod.grb -set_byte 4 12 197 -set_byte 4 17 0  -set_byte 4 24:35 0:0:0:0:0:255:0:0:0:0:0:0 -set_byte 4 36 '+str(nm_use)+' -set_byte 4 38:42 0:0:0:0:0 -set_byte 4 43 3 -set_byte 4 44 0 -set_byte 4 45 '+str(byte45)+' -set_byte 4 46 '+str(byte46)+' -set_byte 4 47 '+str(byte47)+' -append  -set_grib_type c3 -grib_out '+outfile_high)

#  os.system('rm record_out.bin')



# Write variables to grib file
#  grbout = open(outfile,'a')
#  grbtmp['values'] = probfinal.astype(int)
#  grbtmp['scaledValueOfUpperLimit'] = int(1000*round(t*2.54,3))
#  grbout.write(grbtmp.tostring())

  print 'Wrote PSNOW for ', qpf_interval, ' to:',outfile, 'for ',t, 'inch threshold'

#  grbout.close()
# End of loop over thresholds
