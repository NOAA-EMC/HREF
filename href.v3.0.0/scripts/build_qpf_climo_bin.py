##############################
#
# BUILD_CLIMO.PY
#
# COPIES NEW STAGE-IV AND HRRR FILES FROM REAL-TIME DIRECTORIES TO LOCAL DIRECTORY
# FINDS MATCHING QPE/QPF DATA IN LOCAL DIRECTORY AND APPENDS THIS TO NETCDF CLIMO FILES
# CLEANS UP OLD DATA IN LOCAL DIRECTORY
#
# T ALCOTT - ESRL/GSD - 10/29/15
#
##############################

# 2017-02-27	Ben Blake	modified for HREFv2
# 2018-01-02    Matthew Pyle    Cray port for HREFv2.1

###################
# PACKAGE IMPORTS
###################

import os, sys, time
# from netCDF4 import Dataset
import numpy as np
from datetime import datetime, timedelta
from cal_functions import quantile_map

####################
# PATH DEFINITIONS
####################

# # static/config files
# staticdir = sys.argv[1]
# sys.path.append(staticdir)

from eas_config import *
# WGRIB2 = '/nwprod/util/exec/wgrib2'
# COPYGB = '/nwprod/util/exec/copygb'


try:
  os.environ["WGRIB2"]
except KeyError:
  print "NEED TO DEFINE WGRIB2"
  exit(1)
WGRIB2=os.environ.get('WGRIB2','trash')
print 'found WGRIB2 as ', WGRIB2 

try:
  os.environ["COPYGB"]
except KeyError:
  print "NEED TO DEFINE COPYGB"
  exit(1)
COPYGB=os.environ.get('COPYGB','trash')
print 'found COPYGB as ', COPYGB 

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
  os.environ["COMINpcpanl"]
except KeyError:
  print "NEED TO DEFINE COMINpcpanl"
  exit(1)
COMINpcpanl=os.environ.get('COMINpcpanl','trash')
print 'found COMINpcpanl as ', COMINpcpanl

try:
  os.environ["COMINclimo"]
except KeyError:
  print "NEED TO DEFINE COMINclimo"
  exit(1)
COMINclimo=os.environ.get('COMINclimo','trash')
print 'found COMINclimo as ', COMINclimo

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
print 'found dom as ', dom

try:
  os.environ["DATA"]
except KeyError:
  print "NEED TO DEFINE DATA"
  exit(1)
DATA=os.environ.get('DATA','trash')
print 'found DATA as ', DATA

# input directory and config file
sys.path.append(HOMEhref)


#################
# SCRIPT
#################

starttime = time.time()
climohref = COMINclimo + '/qpe/conus/'
os.system('mkdir -p '+climohref)

# copy any stage-iv data from the realtime directory to climo
print 'Syncing any real-time Stage-IV files to local directory...'
# dirpath: a string, the path to the directory
# dirnames: a list of the names of the subdirectories in dirpath
# filenames: a list of the names of the non-directory files in dirpath
for dirpath, dirnames, files in os.walk(COMINpcpanl):
  for dir in dirnames:
    if dir[0:6] == 'pcpanl':
      ymd = dir[7:15]	# pcpanl.YYYYMMDD
      for dirpath, dirnames, files in os.walk(COMINpcpanl + '/' + dir):
        for file in files:	# ST4.YYYYMMDDHH.06h.gz
          if (file[0:3] == 'ST4') and (file[15:18] == '06h'):
            h = file[12:14]          
            if (dom == 'conus'):
              climofile = COMINclimo + '/qpe/conus/stageIV_'+ymd+h+'_06h.gz'	# was .nc
              climofile2 = COMINclimo + '/qpe/conus/stageIV_'+ymd+h+'_06h'	# unzipped file
              climofile2_bin = COMINclimo + '/qpe/conus/stageIV_'+ymd+h+'_06h.bin'	# unzipped file
            if os.path.exists(climofile2) and os.stat(climofile2).st_size == 0:
              os.system('rm '+climofile2)
            if not os.path.exists(climofile2+'.grb'):
              rtfile = COMINpcpanl + '/' + dir + '/ST4.'+ymd+h+'.06h.gz' 
              if os.path.exists(rtfile):
                print 'Copying from real-time directory:',rtfile
                os.system('cp '+rtfile+' '+climofile)               
# unzip the stage IV files
                os.system('gunzip '+climofile)
                os.system('pwd')

#budget interpolate to the HREF CONUS grid - eventually use Trevor's code for neighbor-budget
                if (dom == 'conus'):
                  print 'Budget interpolating to the HREF CONUS grid:',rtfile
                  os.system(COPYGB+' -xg "255 3 1473 1025 12190 -133459 \
                            8 -95000 5079 5079 0 64 25000 25000" -i "3" '+climofile2+' '+climofile2+'.grb')
                  climofull = climofile2 + '.bin'
                  os.system(WGRIB2+' '+climofull+' -bin '+climofile2_bin)   
                  

# copy HiResW (ARW + NMMB) from the realtime directory to climo
#  strip out just the QPF using a wgrib2 command
print 'Syncing any real-time HiResW files to local directory...'
dir = COMINhiresw + '/' + 'hiresw.'+PDY[:8]
runs = []
if os.path.exists(dir):
  files = next(os.walk(dir))[2]
  for file in files:
    if (len(file) == 35 and (file[8:10] == cyc[0:2]) and file[12:19] == 'arw_5km' and (1 <= int(file[21:23])) and file[24:35] == 'conus.grib2'):
      arwfile = dir + '/' + file
      print 'Found',arwfile
      iyear = int(PDY[:4])
      imonth = int(PDY[4:6])
      iday = int(PDY[6:8])
      ihour = int(cyc[0:2]) 
      ijul = (datetime(iyear,imonth,iday)-datetime(iyear,1,1)).days+1
      climohref = COMINclimo + '/sseox/qpf/conus/%i'%iyear+'%02d'%imonth+'%02d'%iday+'%02d'%ihour
      os.system('mkdir -p '+climohref)
      fhour = int(file[21:23])
      shour = fhour - 3
      newfile = 'arw%02d'%(iyear-2000)+'%03d'%ijul+'%02d'%ihour+'00%02d'%fhour+'00'
      climofile = climohref + '/' + newfile
      climofile_bin = climohref + '/' + newfile + '.bin'
      print climofile
      if os.path.exists(climofile2) and os.stat(climofile2).st_size == 0:
        os.system('rm '+climofile2)
      if not os.path.exists(climofile2) and (fhour%3 == 0):
        print 'Creating '+climofile
        os.system(WGRIB2+' '+arwfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -grib '+climofile)   
        os.system(WGRIB2+' '+arwfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -bin '+climofile_bin)   
    elif (len(file) == 36 and (file[8:10] == cyc[0:2]) and file[12:20] == 'nmmb_5km' and (1 <= int(file[22:24])) and file[25:30] == 'conus'):
      nmmbfile = dir + '/' + file
      print 'Found',nmmbfile
      iyear = int(PDY[:4])
      imonth = int(PDY[4:6])
      iday = int(PDY[6:8])
      ihour = int(cyc[0:2]) 
      ijul = (datetime(iyear,imonth,iday)-datetime(iyear,1,1)).days+1
      climohref = COMINclimo + '/sseox/qpf/conus/%i'%iyear+'%02d'%imonth+'%02d'%iday+'%02d'%ihour
      os.system('mkdir -p '+climohref)
      fhour = int(file[22:24])
      shour = fhour - 3
      newfile = 'nmmb%02d'%(iyear-2000)+'%03d'%ijul+'%02d'%ihour+'00%02d'%fhour+'00'
      climofile = climohref + '/' + newfile
      climofile_bin = climohref + '/' + newfile + '.bin'
      print climofile
      if os.path.exists(climofile2) and os.stat(climofile2).st_size == 0:
        os.system('rm '+climofile2)
      if not os.path.exists(climofile2) and (fhour%3 == 0):
        print 'Creating '+climofile
        os.system(WGRIB2+' '+nmmbfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -grib '+climofile)
        os.system(WGRIB2+' '+nmmbfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -bin '+climofile_bin)
    elif (len(file) == 39 and (file[8:10] == cyc[0:2]) and file[12:19] == 'arw_5km' and (1 <= int(file[21:23])) and file[24:33] == 'conusmem2'):

      nsslfile = dir + '/' + file
      print 'Found',nsslfile
      iyear = int(PDY[:4])
      imonth = int(PDY[4:6])
      iday = int(PDY[6:8])
      ihour = int(cyc[0:2]) 
      ijul = (datetime(iyear,imonth,iday)-datetime(iyear,1,1)).days+1
      climohref = COMINclimo + '/sseox/qpf/conus/%i'%iyear+'%02d'%imonth+'%02d'%iday+'%02d'%ihour
      os.system('mkdir -p '+climohref)
      fhour = int(file[21:23])
      shour = fhour - 3
      newfile = 'nssl%02d'%(iyear-2000)+'%03d'%ijul+'%02d'%ihour+'00%02d'%fhour+'00'
      climofile = climohref + '/' + newfile
      climofile_bin = climohref + '/' + newfile + '.bin'
      print climofile
      if os.path.exists(climofile2) and os.stat(climofile2).st_size == 0:
        os.system('rm '+climofile2)
      if not os.path.exists(climofile2) and (fhour%3 == 0):
        print 'Creating '+climofile
        os.system(WGRIB2+' '+nsslfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -grib '+climofile)
        os.system(WGRIB2+' '+nsslfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -bin '+climofile_bin)

# copy HRRR from the realtime directory to climo
#  strip out just the QPF using a wgrib2 command
dir = COMINhrrr + '/' + 'hrrr.'+PDY[:8]
print 'dir is : ', dir
if os.path.exists(dir):
  files = next(os.walk(dir))[2]
  for file in files:
    if (len(file) == 19) and (file[6:8] == cyc[0:2]): 
      hrrrfile = dir + '/' + file
      print 'Found',hrrrfile
      iyear = int(PDY[:4])
      imonth = int(PDY[4:6])
      iday = int(PDY[6:8])
      ihour = int(cyc[0:2]) 
      ijul = (datetime(iyear,imonth,iday)-datetime(iyear,1,1)).days+1
      climohref = COMINclimo + '/sseox/qpf/conus/%i'%iyear+'%02d'%imonth+'%02d'%iday+'%02d'%ihour
      os.system('mkdir -p '+climohref)
      fhour = int(file[11:13])
      shour = fhour - 3
      newfile = 'hrrr%02d'%(iyear-2000)+'%03d'%ijul+'%02d'%ihour+'00%02d'%fhour+'00'
      climofile = climohref + '/' + newfile + '_copy'
      climofile2 = climohref + '/' + newfile
      climofile2_bin = climohref + '/' + newfile + '.bin'
      print climofile2
      if os.path.exists(climofile2) and os.stat(climofile2).st_size == 0:
        os.system('rm '+climofile2)
      if not os.path.exists(climofile2) and (fhour%3 == 0):
        print 'Creating '+climofile2
        os.system(WGRIB2+' '+hrrrfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -grib '+climofile2)   
        os.system(WGRIB2+' '+hrrrfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -bin '+climofile2_bin)   
#        os.system(WGRIB2+' '+hrrrfile+' -match "APCP:surface:" -end -grib '+climofile)   

# copy NAM from the realtime directory to climo
#  strip out just the QPF using a wgrib2 command
# print dirs
dir = COMINnam + '/' + 'nam.'+PDY[:8]
print 'dir is : ', dir
if os.path.exists(dir):
  files = next(os.walk(dir))[2]
  for file in files:
    if (len(file) == 18) and (file[5:7] == cyc[0:2]) and (1 <= int(file[10:12])) and (file[13:18] == 'grib2'): 
      namxfile = dir + '/' + file
      print 'Found',namxfile
      iyear = int(PDY[:4])
      imonth = int(PDY[4:6])
      iday = int(PDY[6:8])
      ihour = int(cyc[0:2]) 
      ijul = (datetime(iyear,imonth,iday)-datetime(iyear,1,1)).days+1
      climohref = COMINclimo + '/sseox/qpf/conus/%i'%iyear+'%02d'%imonth+'%02d'%iday+'%02d'%ihour
      os.system('mkdir -p '+climohref)
      fhour = int(file[10:12])
      shour = fhour - 3
      newfile = 'nam%02d'%(iyear-2000)+'%03d'%ijul+'%02d'%ihour+'00%02d'%fhour+'00'
      climofile = climohref + '/' + newfile + '_copy'
      climofile2 = climohref + '/' + newfile
      climofile2_bin = climohref + '/' + newfile + '.bin'
      print climofile2
      if os.path.exists(climofile2) and os.stat(climofile2).st_size == 0:
        os.system('rm '+climofile2)
      if not os.path.exists(climofile2) and (fhour%3 == 0):
        print 'Creating '+climofile2
        os.system(WGRIB2+' '+namxfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -grib '+climofile2)   
        os.system(WGRIB2+' '+namxfile+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -bin '+ climofile2_bin)   


# Remove old Stage-IV and HREFv2 files
#sys.exit() # skip for now

nowd = datetime.now()
climostart = nowd-timedelta(pqpf_climo_days)
climostart_es = int(time.mktime(climostart.timetuple()))
print 'Maintaining climatology back to:',climostart
print 'Removing any old HREF/Stage-IV files no longer needed...'
for dirpath, dirnames, files in os.walk(COMINclimo+'/qpe/conus'):
  for file in files:
    year, month, day, hour = int(file[8:12]), int(file[12:14]), int(file[14:16]), int(file[16:18])
    vtime = datetime(year,month,day,hour)
    es = int(time.mktime(vtime.timetuple()))
    if es<climostart_es:
      filename = COMINclimo+'/qpe/conus/'+file
      os.system('rm -f '+filename)
      print 'Deleted',filename
   # Remove the non-interpolated Stage IV files
    elif len(file) == 22:
      filename = COMINclimo+'/qpe/conus/'+file
      os.system('rm -f '+filename)
      print 'Deleted',filename
for dirpath, dirnames, files in os.walk(COMINclimo+'/sseox/qpf/conus'):
  for dir in dirnames:
    year, month, day, hour = int(dir[0:4]), int(dir[4:6]), int(dir[6:8]), int(dir[8:10])
    vtime = datetime(year,month,day,hour)
    es = int(time.mktime(vtime.timetuple()))
    if es<climostart_es:
      fulldir = COMINclimo+'/sseox/qpf/conus/'+dir
      os.system('rm -rf '+fulldir)
      print 'Deleted',dir

endtime = time.time()
print 'Total time elapsed:',(endtime-starttime)

