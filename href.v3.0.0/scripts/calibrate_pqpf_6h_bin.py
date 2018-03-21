#####################################################
# CALIBRATE_PQPF_6H.PY                              #
#                                                   #
# Calculates HRRR-TLE PQPF calibration coeffs       #
#                                                   #
# Trevor Alcott / Isidora Jankov / Curtis Alexander #
# Dec 2015 - Jun 2016                               #
#####################################################

# 2017-02-27	Ben Blake	modified for HREFv2
# Create 4 coeffs_files: one for HiResW ARW, one for HiResW NMMB, one for HiResW NSSL, one for NAMX
# 2018-02-02    Matthew Pyle    extended for HREFv2.1 (adding HRRR)
# 2018-03-14    Matthew Pyle    pygrib free version

###################
# PACKAGE IMPORTS
###################

import os, sys, time, gzip
from netCDF4 import Dataset
import numpy as np
from datetime import datetime, timedelta
from cal_functions import quantile_map

### parms imported from PQPF config file
from eas_config import *

####################
# GET ENVIRO VARS
####################

try:
  os.environ["HOMEhref"]
except KeyError:
  print "NEED TO DEFINE HOMEhref"
  exit(1)
HOMEhref=os.environ.get('HOMEhref','trash')
print 'found HOMEhref as ', HOMEhref

try:
  os.environ["COMOUTcalib"]
except KeyError:
  print "NEED TO DEFINE COMOUTcalib"
  exit(1)
COMOUTcalib=os.environ.get('COMOUTcalib','trash')
print 'found COMOUTcalib as ', COMOUTcalib

try:
  os.environ["COMOUTcal"]
except KeyError:
  print "NEED TO DEFINE COMOUTcal"
  exit(1)
COMOUTcal=os.environ.get('COMOUTcal','trash')
print 'found COMOUTcal as ', COMOUTcal

try:
  os.environ["COMOUTclimo"]
except KeyError:
  print "NEED TO DEFINE COMOUTclimo"
  exit(1)
COMOUTclimo=os.environ.get('COMOUTclimo','trash')
print 'found COMOUTclimo as ', COMOUTclimo

try:
  os.environ["dom"]
except KeyError:
  print "NEED TO DEFINE dom"
  exit(1)
dom=os.environ.get('dom','trash')
print 'found dom as ', dom

try:
  os.environ["DATA"]
except KeyError:
  print "NEED TO DEFINE DATA"
  exit(1)
DATA=os.environ.get('DATA','trash')
print 'found DATA as ', DATA


####################
# PATH DEFINITIONS
####################


# static/config files
sys.path.append(HOMEhref)

os.system("mkdir -p " + DATA)
os.system("cd " + DATA)

# directory for calibration coefficients
os.system("mkdir -p " + COMOUTcal)

if (dom == 'conus'):
  template = HOMEhref + '/fix/pqpf_conustemplate.grib2'
  landmask = HOMEhref + '/fix/href_conus_slmask.nc'
  coeffs_temp_arw = COMOUTcal + '/pqpf_6h_coeffs_arw.wrk'
  coeffs_file_arw = COMOUTcal + '/pqpf_6h_coeffs_arw.csv'
  coeffs_temp_nmmb = COMOUTcal + '/pqpf_6h_coeffs_nmmb.wrk'
  coeffs_file_nmmb = COMOUTcal + '/pqpf_6h_coeffs_nmmb.csv'
  coeffs_temp_nssl = COMOUTcal + '/pqpf_6h_coeffs_nssl.wrk'
  coeffs_file_nssl = COMOUTcal + '/pqpf_6h_coeffs_nssl.csv'
  coeffs_temp_nam = COMOUTcal + '/pqpf_6h_coeffs_nam.wrk'
  coeffs_file_nam = COMOUTcal + '/pqpf_6h_coeffs_nam.csv'
  coeffs_temp_hrrr = COMOUTcal + '/pqpf_6h_coeffs_hrrr.wrk'
  coeffs_file_hrrr = COMOUTcal + '/pqpf_6h_coeffs_hrrr.csv'

# directory for matched pairs files needed for calibration
os.system("mkdir -p " + COMOUTcalib+"/sseox")
os.system("mkdir -p " + COMOUTcalib+"/realtimecoeffs")

# where to store climo netCDF files
os.system("mkdir -p " + COMOUTclimo)

forecast_length = 6

###############
# FUNCTIONS
##############

# calculate datetime object as a function of year and Julian day 
def jul2dt(year,jul):
  d = datetime(year,1,1)+timedelta(jul-1)
  return d

def dt2jul(y,m,d):
  jul = (datetime(y,m,d)-datetime(y,1,1)).days + 1
  return jul

def mindate(l):
  if len(l)==0:
    return 0
  else:
    return min(l)

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
    array2d.shape = (nx,ny)
    return array2d,nx,ny
  F1.close()


#################
# SCRIPT
#################

# Determine which HREF forecast hours to match with QPE
fstarts = [0,6,12,18,24,30]
fstarts = np.unique(fstarts)
print 'Here is fstarts'
print(fstarts)

# get dimensions and lat/lons from a sample file
# the grib-2 template file is perfect for this
# grbs = pygrib.open(template)
# grb = grbs[1]
# lat, lon = grb.latlons()
# grbs.close()

os.system(WGRIB2+' '+template+'  -rpn rcl_lat -text lat.txt  -rpn rcl_lon -text lon.txt')

lon,nx,ny=simplewgrib2('lon.txt')
lon = np.where(lon > 180.,lon-360.,lon)
lon = np.where(lon < -180.,lon+360.,lon)

lat,nx,ny=simplewgrib2('lat.txt')

print 'Searching for new QPE-QPF matches...'

# Create and dimension climo netcdf files if they do not already exist
members = ['arw','nmmb','nssl','nam','hrrr']

for fstart in fstarts:
  fend = fstart + forecast_length

  for mem in members:
    print 'Working with',mem,'members...'
    climonc = COMOUTcalib+'/sseox/'+mem+'_qpf6_%02d'%fend+'.nc'

    print '6-h QPF ending FHR',fend
    if not os.path.exists(climonc):
      print 'Creating',climonc
      nc = Dataset(climonc,'w')
      t = nc.createDimension('t', None)
      x = nc.createDimension('x', np.shape(lat)[1])
      y = nc.createDimension('y', np.shape(lat)[0])
      times = nc.createVariable('validtime','i8',('t',))
      lats = nc.createVariable('lat','f4',('y','x',))
      lons = nc.createVariable('lon','f4',('y','x',))
      qpf = nc.createVariable('qpf','f4',('t','y','x',))
      qpe = nc.createVariable('qpe','f4',('t','y','x',))
      lats[:] = lat
      lons[:] = lon
      lats.units = 'degrees_north'
      lons.units = 'degrees_east'
      times.units = 'seconds since 0000 UTC 1970-01-01'
      qpf.units = 'mm'
      qpe.units = 'mm'
      nc.description = 'Stage-IV / HREF QPF climatology for calibration'
      nc.close()

  # get a list of existing matches
    nc = Dataset(climonc,'r')
    tlist = nc.variables['validtime'][:].tolist()
    nc.close()
    if len(tlist)>0:
      print 'File',climonc,'contains',len(tlist),'matches valid:'
      print datetime.fromtimestamp(min(np.array(tlist))),'to',datetime.fromtimestamp(max(np.array(tlist)))
    else:
      print 'File',climonc,'contains no matches yet'

  # get a list of available QPE data in order of newest first
    qpedates = []
    if (dom == 'conus'):
      qpedir2 = COMOUTclimo +'/qpe/conus'
    for dirpath, dirnames, files in os.walk(qpedir2):
      for qpefile in files:
        qpedates.append(int(qpefile[8:18]))
    qpedates = sorted(qpedates,reverse=True)

  # loop through each of those stage-iv files and see if they are in the climatology already
    for qpedate in qpedates:
      qpefile = 'stageIV_'+str(qpedate)+'_06h.grb'
      year, month, day, hour = int(qpefile[8:12]), int(qpefile[12:14]), int(qpefile[14:16]), int(qpefile[16:18])
      vtime = datetime(year,month,day,hour)
      es = int(time.mktime(vtime.timetuple()))

    # if not in the climatology, and not older than all of the matches in there, then...
    # attempt to add to the climatology
      if es not in tlist and es > mindate(tlist):

      # determine HREF files needed for a match
        print 'Search for QPF to match QPE valid:',qpedate
        fend = int(fstart + forecast_length)	# precip for 6th hour
        itime = vtime - timedelta(fend/24.0)
        fhr3 = int(fstart + 3)			# precip for 3rd hour
        print itime, fhr3, fend
        ijul = dt2jul(itime.year,itime.month,itime.day)
        if (dom == 'conus'):
          hrefdir = COMOUTclimo + '/sseox/qpf/conus/%i'%itime.year+'%02d'%itime.month+'%02d'%itime.day+'%02d'%itime.hour
        if (mem == 'arw'):
          hreffile3 = hrefdir + '/arw%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fhr3+'00'
          hreffile6 = hrefdir + '/arw%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fend+'00'
        elif (mem == 'nmmb'):
          hreffile3 = hrefdir + '/nmmb%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fhr3+'00'
          hreffile6 = hrefdir + '/nmmb%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fend+'00'
        elif (mem == 'nssl'):
          hreffile3 = hrefdir + '/nssl%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fhr3+'00'
          hreffile6 = hrefdir + '/nssl%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fend+'00'
        elif (mem == 'nam'):
          hreffile3 = hrefdir + '/nam%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fhr3+'00'
          hreffile6 = hrefdir + '/nam%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fend+'00'
        elif (mem == 'hrrr'):
          hreffile3 = hrefdir + '/hrrr%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fhr3+'00'
          hreffile6 = hrefdir + '/hrrr%02d'%(itime.year-2000)+'%03d'%ijul+'%02d'%itime.hour+'00%02d'%fend+'00'
        missing = 0

     # if a file does not exist or has a file size of 0, do not add the match
        if ((not os.path.exists(hreffile3)) or (not os.path.exists(hreffile6))):
          missing = 1
        if (os.path.exists(hreffile3) and os.stat(hreffile3).st_size == 0) or (os.path.exists(hreffile6) and os.stat(hreffile6).st_size == 0):
          missing = 1

     # if all are available, add the QPE-QPF match to the climo file
        if missing == 0:
          print 'Adding',vtime,'to matched QPE/QPF climatology'
          nc_out = Dataset(climonc,'a')
          times = nc_out.variables['validtime']
          qpematched = nc_out.variables['qpe']
          qpfmatched = nc_out.variables['qpf']
 
        # figure out where to put the match
        # if the climatology is not complete, just add another time
          if len(times)<pqpf_climo_size:
            ind = len(times[:])
        # otherwise replace the oldest data
          else:
            tlist = times[:].tolist()
            ind = tlist.index(min(tlist))
 
        # first add QPE from the stage-IV grib file (archive is kg/m2, the same as mm)
#          idx = pygrib.index(qpedir2+'/'+qpefile,'name')
#          grb = idx(name='Total Precipitation')[0]

          os.system(WGRIB2+' '+file+' -match "APCP:" -end -text tmptxt')
          qpein,nx,ny=simplewgrib2('tmptxt')


# assumption that qpein is masked??

#          qpein = grb.values
#          idx.close()
          times[ind]=es
          qpematched[ind,:,:]=qpein
 
        # now add the QPF - sum up 0-3 hr accums for 00-06z ST4
#          idx = pygrib.index(hreffile3,'endStep')
#          grb = idx(endStep=fhr3)[0]
#          qpf3 = grb.values
#          idx.close()
#          idx = pygrib.index(hreffile6,'endStep')
#          grb = idx(endStep=fend)[0]
#          qpf6 = grb.values
#          idx.close()         

## rework to key on proper time periods

          qpfs= []
## rework to key on proper time periods
          os.system(WGRIB2+' '+hreffile3+' -match "APCP:surface:%i'%fstart+'-%i'%fhr3+'" -end -text tmptxt')
          qpf3,nx,ny=simplewgrib2('tmptxt')
          

          qpfs= []
## rework to key on proper time periods
#          os.system(WGRIB2+' '+hreffile6+' -match "APCP:" -end -text tmptxt')
          os.system(WGRIB2+' '+hreffile6+' -match "APCP:surface:%i'%fhr3+'-%i'%fhr6+'" -end -text tmptxt')
          qpf6,nx,ny=simplewgrib2('tmptxt')

          qpfin = qpf6 + qpf3

          qpfintmp = np.ma.masked_greater(qpfin,9.0e+20)
          qpfin = np.ma.filled(qpfintmp.astype(float), np.nan)	# Convert all masked values to NaN         
          qpfmatched[ind,:,:]=qpfin
 
        # close the matched climo file
          nc_out.close()

# call the quantile mapping function for each lead time and for each member - ARW, NMMB, NSSL, NAM
# output to text file: regression coefficients for each lead time, and valid times used

count = 0

for mem in members:
  if mem == 'arw':
    coeffs_temp = coeffs_temp_arw
    coeffs_file = coeffs_file_arw
  elif mem == 'nmmb':
    coeffs_temp = coeffs_temp_nmmb
    coeffs_file = coeffs_file_nmmb
  elif mem == 'nssl':
    coeffs_temp = coeffs_temp_nssl
    coeffs_file = coeffs_file_nssl
  elif mem == 'nam':
    coeffs_temp = coeffs_temp_nam
    coeffs_file = coeffs_file_nam
  elif mem == 'hrrr':
    coeffs_temp = coeffs_temp_hrrr
    coeffs_file = coeffs_file_hrrr

  os.system('rm -f '+coeffs_temp)
  f = open(coeffs_temp,'w')
# retrieve HREF landmask and mask out QPE over water
  nc = Dataset(landmask,'r')
  if (dom == 'conus'):
    l = nc.variables['LAND_GDS3_SFC'][:]	# The variable that was in the .nc file
  elif (dom == 'alaska'):
    l = nc.variables['LAND_GDS5_SFC'][:]	# The variable that was in the .nc file
  nc.close()
  for fstart in fstarts:
    fend = fstart+forecast_length
    climonc = COMOUTcalib+'/sseox/'+mem+'_qpf6_%02d'%fend+'.nc'

    print 'Reading time-matched QPF/QPE: FHRs',fstart,'-',fend
    nc = Dataset(climonc,'r')
    times = nc.variables['validtime'][:]
    qpf = nc.variables['qpf'][:]
    qpe = nc.variables['qpe'][:]
    nc.close()
    qpe = np.where(np.less(l,1),-1.0,qpe)
    if len(qpf)>0:
      print 'Calculating quantile-mapping coefficients...'
      coeffs = quantile_map(qpf,qpe,pqpf_6h_minpct,pqpf_6h_fitorder)
      f.write('qmap,%i'%fstart+',%i'%fend+',')
      for c in coeffs:
        f.write('%.4f'%c+',')
      f.write('cal_dates,%i'%len(times)+',[')
      for t in times:
        f.write('%i'%t+',')
      f.write(']\n')
      count += 1
    else:
      print 'No QPF/QPE pairs found'

  f.close()
  if (count != 0):
    os.system('mv '+coeffs_temp+' '+coeffs_file)
    os.system('cp '+coeffs_file+' '+COMOUTcalib+'/realtimecoeffs')

