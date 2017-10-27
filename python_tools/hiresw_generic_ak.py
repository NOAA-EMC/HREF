import pygrib
import numpy as np
import numpy.ma as ma
import matplotlib.pyplot as plt
import ncepy
from mpl_toolkits.basemap import Basemap, cm, maskoceans
import time,sys,scipy,os
from matplotlib import colors as c

# 
import sys
from scipy.ndimage.filters import minimum_filter, maximum_filter
import dateutil.relativedelta, dateutil.parser
#from subprocess import call
import subprocess
from matplotlib import colors

maxhourly=1

# 

#----------Define some useful functions needed to start---------------------------------------------#

def extrema(mat,mode='wrap',window=10):
  # From: http://matplotlib.org/basemap/users/examples.html

  """find the indices of local extrema (min and max)
  in the input array."""
  mn = minimum_filter(mat, size=window, mode=mode)
  mx = maximum_filter(mat, size=window, mode=mode)
  # (mat == mx) true if pixel is equal to the local max
  # (mat == mn) true if pixel is equal to the local in
  # Return the indices of the maxima, minima
  return np.nonzero(mat == mn), np.nonzero(mat == mx)


#--------------------------------------------------------------------------#

#Necessary to generate figs when not running an Xserver (e.g. via PBS)
plt.switch_backend('agg')

t1a = time.clock()
print("Starting...")
  
# Read grib file and dom id from command line
gribfile=sys.argv[1]
domid=sys.argv[2]
core=sys.argv[3]
#make domid upper case
domid=domid.upper()
core=core.upper()

grbs= pygrib.open(gribfile)

for g in grbs:
 print g.name, g.typeOfLevel, g.level # g.probabilityTypeName # g.scaledValueofUpperLimit, g.lengthOfTimeRange
# if  g.name=='Total Precipitation' and g.probabilityTypeName=='Probability of event above upper limit':
 if  g.name=='Total Precipitation' and g.productDefinitionTemplateNumber=='9':

#  print g.name
  print g.lengthOfTimeRange
#  print g.scaledValueOfLowerLimit
#   print g.scaleFactorOfLowerLimit
#  print g.scaledValueOfUpperLimit
#  print g.scaleFactorOfUpperLimit
#  print g.derivedForecast
#  upperlim = self['scaledValueOfUpperLimit']/\
#             np.power(10.0,self['scaleFactorOfUpperLimit'])
  upperlim=g.scaledValueOfUpperLimit/np.power(10.0,g.scaleFactorOfUpperLimit)
  print upperlim

# Get the lats and lons
lats, lons = grbs[1].latlons()
    
# Get grid projection info.  Use same projection for plotting - obviates need to do
#  any wind vector rotation

gribproj=grbs[1]['gridType']
if gribproj.lower()=='lambert':
  # Everything below was lifted directly from pygrib.pyx
  Lon0=grbs[1]['LoVInDegrees']
  Lat0=grbs[1]['LaDInDegrees']
  Lat1=grbs[1]['Latin1InDegrees']
  Lat2=grbs[1]['Latin2InDegrees']
elif gribproj.lower()=='polar_stereographic':
  # Everything below was lifted directly from pygrib.pyx
  Lon0=grbs[1]['orientationOfTheGridInDegrees']
  Lat_ts=grbs[1]['latitudeWhereDxAndDyAreSpecifiedInDegrees']
  if grbs[1].has_key('projectionCentreFlag'):
    projcenterflag = grbs[1]['projectionCentreFlag']
  elif grbs[1].has_key('projectionCenterFlag'):
    projcenterflag = grbs[1]['projectionCenterFlag']
  if projcenterflag == 0:
    Lat0=90.
  else:
    Lat0=-90.
elif gribproj.lower()=='mercator':
  scale = float(grbs[1]['grib2divider'])
  lon1 = grbs[1]['longitudeOfFirstGridPoint']/scale
  if grbs[1]['truncateDegrees']:
    lon1 = int(lon1)
  lon2 = grbs[1]['longitudeOfLastGridPoint']/scale
  if grbs[1]['truncateDegrees']:
    lon2 = int(lon2)
  Lat_ts=grbs[1]['LaD']/scale
  Lon0=0.5*(lon1+lon2)


print 'MY PROJECTION IS: ',gribproj

rearth=grbs[1]['radius']

try:
  dx=grbs[1]['DxInMetres']/1000.
except:
  dx=grbs[1]['DiInMetres']/1000.

try:
  nx=grbs[1]['Nx']
except:
  nx=grbs[1]['Ni']

try:
  ny=grbs[1]['Ny']
except:
  ny=grbs[1]['Nj']


#Make sure lons are west oriented
if Lon0 > 0:
  Lon0=Lon0-360
lons=np.where(lons>0.,lons-360.,lons)
    
#Get the date/time and forecast hour
fullfhr=grbs[1]['stepRange'] # Forecast hour
# Pad fhr with a 0
fullfhr=str(fullfhr).zfill(2)
print 'fullfhr is: ', fullfhr
uselen=len(fullfhr)
print 'uselen is: ', uselen

if uselen is 2:
 fhr=fullfhr[uselen-2:uselen]
if uselen is 3:
 fhr=fullfhr[uselen-1:uselen]
if uselen is 4:
 fhr=fullfhr[uselen-2:uselen]
if uselen is 5:
 fhr=fullfhr[uselen-2:uselen]


fhr=str(fhr).zfill(2)
print 'fhr now is: ', fhr

cyctime=grbs[1].dataTime #Cycle (e.g. 1200 UTC)
print 'cyctime is: ', cyctime
  
#Pad with a zero and convert to a string
grbtime=str(cyctime).zfill(4)

print 'grbtime is: ', grbtime
  
date=grbs[1].dataDate    #PDY
# Now we need to figure out the forecast valid date/time

print 'repr(date) ', repr(date)

idate=repr(date)+grbtime    #CDATE 
vdate=ncepy.ndate(idate,int(fhr))
valpdy=vdate[0:8]
valcyc=vdate[8:11]

# Specify some plotting domains which have the regions pre-set in ncepy
if '_GES' in domid or '_AN' in domid or '_FILT' in domid:
  domains=['CONUS']
  anl_diag_skip=1
elif 'ALASKANEST' in domid or 'ALASKA' in domid:
  domains=['AK','NAK','SAK','SWAK','SEAK']
  anl_diag_skip=0
else:
#  domains=['CONUS','NW','NC','NE','SW','SC','SE','Great_Lakes','MIDATL']
  domains=['CONUS']
  anl_diag_skip=0

# Start reading fields to be plotted

try:
  # read refc dBZ
  dbz = grbs.select(name='Maximum/Composite radar reflectivity')[0].values # This is actually REFC dBZ
except:
  dbz=None

try:
  # 1km AGL ref
  onekmrefd=grbs.select(stepType='instant',name='Derived radar reflectivity',typeOfLevel='heightAboveGround',level=1000)[0].values
except:
  onekmrefd=None

try:
  # 1km AGL ref
  onekmrefdprob40=grbs.select(stepType='instant',name='Derived radar reflectivity',typeOfLevel='heightAboveGround',\
  productDefinitionTemplateNumber=5,scaledValueOfUpperLimit=40000,scaleFactorOfUpperLimit=3,level=1000)[0].values
  print 'max onekmrefdprob40: ', np.max(onekmrefdprob40)
except:
  onekmrefdprob40=None

try:
  # read mean cape
  cape = grbs.select(name='Convective available potential energy',topLevel=9000,bottomLevel=0)[0].values
  print 'max cape: ', np.max(cape)
except:
  print 'no cape'
  cape=None


try:
  vis800 = grbs.select(name='Visibility',productDefinitionTemplateNumber=5)[0].values
  print 'max vis800:', np.max(vis800)
except:
  vis800=None

try:
  vis = grbs.select(name='Visibility')[0].values
  print 'max vis:', np.max(vis)
except:
  vis=None

try:
  pwat = grbs.select(name='Precipitable water')[0].values
  print 'max pwat:', np.max(pwat)
except:
  pwat=None


try:
  ceil610 = grbs.select(name='Geopotential Height',productDefinitionTemplateNumber=5,scaledValueOfLowerLimit=610000,scaleFactorOfLowerLimit=3)[0].values
  print 'max ceil610:', np.max(ceil610)
except:
  print 'no ceil610'
  ceil610=None

try:
  ceil305 = grbs.select(name='Geopotential Height',productDefinitionTemplateNumber=5,scaledValueOfLowerLimit=305000,scaleFactorOfLowerLimit=3)[0].values
  print 'max ceil305:', np.max(ceil305)
except:
  print 'no ceil305'
  ceil305=None


try:
  # 2-5 km MXUPHL
  mxuphlprob25=grbs.select(parameterName="199",typeOfLevel="heightAboveGroundLayer",\
  productDefinitionTemplateNumber=5,topLevel=5000,bottomLevel=2000)[0].values
  print 'mxuphlprob25: ', np.max(mxuphlprob25)
except:
  mxuphlprob25=None
  print 'no mxuphlprob25'

try:
  # 1km AGL HM ref
  mx1kmrefdprob40=grbs.select(parameterName='198',typeOfLevel="heightAboveGround",\
  productDefinitionTemplateNumber=5,scaledValueOfUpperLimit=40000,scaleFactorOfUpperLimit=3,level=1000)[0].values
  print 'max mx1kmrefdprob40: ', np.max(mx1kmrefdprob40)
except:
  print 'no mx1kmrefdprob40'
  mx1kmrefdprob40=None

try:
 # snow3h
  snow3h=grbs.select(stepType='accum',name='Water equivalent of accumulated snow depth',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=8)[0].values
  snow3h=10.*snow3h/25.4
  print 'max snow3h ', np.max(snow3h)
except:
  print 'no snow3h'
  snow3h=None

try:
 # snow1h
  snow1h=grbs.select(stepType='accum',name='Water equivalent of accumulated snow depth',lengthOfTimeRange=1,\
  productDefinitionTemplateNumber=8)[0].values
  snow1h=10.*snow1h/25.4
  print 'max snow1h ', np.max(snow1h)
except:
  print 'no snow1h'
  snow1h=None

try:
# apcp6hffg6
  apcp6hffg6=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=6,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=6000,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp6hffg6: ', np.max(apcp6hffg6)
except:
  apcp6hffg6=None

try:
# apcp3hffg3
  apcp3hffg3=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=3000,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3hffg3: ', np.max(apcp3hffg3)
except:
  apcp3hffg3=None

try:
# apcp3hffg4
  apcp3hffg4=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=4000,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3hffg4: ', np.max(apcp3hffg4)
except:
  apcp3hffg4=None

try:
# apcp3hri10
  apcp3hri10=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=10000,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3hri10: ', np.max(apcp3hri10)
except:
  apcp3hri10=None

try:
 # apcp3h12p7
  apcp3h12p7=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=12700,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3h12p7 ', np.max(apcp3h12p7)
except:
  apcp3h12p7=None  

try:
 # apcp1h12p7
  apcp1h12p7=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=1,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=12700,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp1h12p7 ', np.max(apcp1h12p7)
except:
  apcp1h12p7=None  

try:
 # apcp3h12p4
  apcp3h12p4=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=12400,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3h12p4 ', np.max(apcp3h12p4)
except:
  apcp3h12p4=None  

try:
 # apcp3h25p1
  apcp3h25p1=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=25100,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3h25p1 ', np.max(apcp3h25p1)
except:
  apcp3h25p1=None  


try:
 # apcp3hp254
  apcp3hp254=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=250,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3hp254 ', np.max(apcp3hp254)
except:
  print 'no 3hp254'
  exit
  apcp3hp254=None  

try:
 # apcp3h25p4
  apcp3h25p4=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=25400,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3h25p4 ', np.max(apcp3h25p4)
except:
  apcp3h25p4=None  

try:
 # apcp3h50p8
  apcp3h50p8=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=50800,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3h50p8 ', np.max(apcp3h50p8)
except:
  apcp3h50p8=None  

try:
 # apcp3h76p2
  apcp3h76p2=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=76200,scaleFactorOfUpperLimit=3)[0].values
  print 'max apcp3h76p2 ', np.max(apcp3h76p2)
except:
  apcp3h76p2=None  


try:
 # apcp3h
  apcp3h=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=8)[0].values
  apcp3h=apcp3h/25.4
  print 'max apcp3h ', np.max(apcp3h)
except:
  print 'no apcp3h'
  apcp3h=None  

try:
 # apcp1h
  apcp1h=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=1,\
  productDefinitionTemplateNumber=8)[0].values
  apcp1h=apcp1h/25.4
  print 'max apcp1h ', np.max(apcp1h)
except:
  print 'no apcp1h'
  apcp1h=None  


try:
 # apcp3hb
  apcp3hb=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=3,productDefinitionTemplateNumber=8)[0].values
  apcp3hb=apcp3hb/25.4
  print 'max apcp3hb ', np.max(apcp3hb)
except:
  print 'no apcp3hb'
  apcp3hb=None  


try:
 # apcp6h
  apcp6h=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=6,productDefinitionTemplateNumber=8)[0].values
# convert to inches
  apcp6h=apcp6h/25.4
  print 'max apcp6h ', np.max(apcp6h)
except:
  apcp6h=None  

try:
 # apcp6hb
  apcp6hb=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=6,productDefinitionTemplateNumber=8)[0].values
  apcp6hb=apcp6hb/25.4
  print 'max apcp6hb ', np.max(apcp6hb)
except:
  apcp6hb=None  
  print 'no apcp6hb'

try:
 # sn1h1inch
  sn1h1inch=grbs.select(stepType='accum',name='Water equivalent of accumulated snow depth',lengthOfTimeRange=1,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=2540,scaleFactorOfUpperLimit=3)[0].values
  print 'max sn1h1inch ', np.max(sn1h1inch)
except:
  print 'no sn1h1inch'
  sn1h1inch=None

try:
 # sn3h1inch
  sn3h1inch=grbs.select(stepType='accum',name='Water equivalent of accumulated snow depth',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=2540,scaleFactorOfUpperLimit=3)[0].values
  print 'max sn3h1inch ', np.max(sn3h1inch)
except:
  sn3h1inch=None

try:
 # sn3h3inch
  sn3h3inch=grbs.select(stepType='accum',name='Water equivalent of accumulated snow depth',lengthOfTimeRange=3,\
  productDefinitionTemplateNumber=9,scaledValueOfUpperLimit=7620,scaleFactorOfUpperLimit=3)[0].values
  print 'max sn3h3inch ', np.max(sn3h3inch)
except:
  sn3h3inch=None


try:
  t2m = grbs.select(stepType='instant',name='2 metre temperature',typeOfLevel='heightAboveGround',level=2)[0].values
  t2m = t2m - 273.15
except:
  t2m=None

try:
  t2mprob273 = grbs.select(name='2 metre temperature',typeOfLevel='heightAboveGround',level=2,productDefinitionTemplateNumber=5)[0].values
  print 'max t2mprob273:', np.max(t2mprob273)
except:
  t2mprob273=None

try:
  td2mgt285 = grbs.select(stepType='instant',name='2 metre dewpoint temperature',typeOfLevel='heightAboveGround',level=2,productDefinitionTemplateNumber=5,scaledValueOfUpperLimit=285930,scaleFactorOfUpperLimit=3)[0].values
  print 'min/max td2mgt285: ', np.min(td2mgt285), np.max(td2mgt285)
except:
  td2mgt285=None


try:
  windprobgt20 = grbs.select(name='10 metre wind speed',typeOfLevel='heightAboveGround',level=10,productDefinitionTemplateNumber=5)[0].values
  print 'max windprobgt20:', np.max(windprobgt20)
except:
  print 'no windprobgt20'
  windprobgt20=None
  
print 'min/max t2m: ', np.min(t2m), np.max(t2m)

try:
  td2m = grbs.select(stepType='instant',name='2 metre dewpoint temperature',typeOfLevel='heightAboveGround',level=2)[0].values
  td2m = td2m - 273.15
  print 'min/max td2m: ', np.min(td2m), np.max(td2m)
except:
  td2m=None

try:
 # apcp12h
  apcp12h=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=12,productDefinitionTemplateNumber=8)[0].values
# convert to inches
  apcp12h=apcp12h/25.4
  print 'max apcp12h ', np.max(apcp12h)
except:
  apcp12h=None  

try:
  u10=grbs.select(name='10 metre U wind component',typeOfLevel="heightAboveGround",level=10)[0].values
except:
  u10=None

try:
  v10=grbs.select(name='10 metre V wind component',typeOfLevel="heightAboveGround",level=10)[0].values
except:
  v10=None

print 'u10,v10 maxes: ', np.max(u10),np.max(v10)


try:
 # apcp48h
  apcp48h=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=48,productDefinitionTemplateNumber=8)[0].values
  apcp48h=apcp48h/25.4
  print 'max apcp48h ', np.max(apcp48h)
except:
  apcp48h=None  



# this item above snhfx needs to NOT be found, I believe

try:
 # apcp24h
  apcp24h=grbs.select(stepType='accum',name='Total Precipitation',lengthOfTimeRange=24,productDefinitionTemplateNumber=8)[0].values
# convert to inches
  apcp24h=apcp24h/25.4
  print 'max apcp24h ', np.max(apcp24h)
except:
  apcp24h=None  

try:
  td2mgt285 = grbs.select(stepType='instant',name='2 metre dewpoint temperature',typeOfLevel='heightAboveGround',level=2,productDefinitionTemplateNumber=5,scaledValueOfUpperLimit=285930,scaleFactorOfUpperLimit=3)[0].values
  print 'min/max td2mgt285: ', np.min(td2mgt285), np.max(td2mgt285)
except:
  td2mgt285=None



  snhfx=None
  lhfx=None
  clearskydownswfx=None

  try:
    #read max hrly UH
    mxuphl=grbs.select(stepType='max',parameterName="199",typeOfLevel="heightAboveGroundLayer",topLevel=5000,bottomLevel=2000)[0].values
    # read max hrly 10m winds
    u10mx=grbs.select(stepType='max',parameterName="222",typeOfLevel="heightAboveGround",level=10)[0].values
    v10mx=grbs.select(stepType='max',parameterName="223",typeOfLevel="heightAboveGround",level=10)[0].values
    spd10mx=np.sqrt(u10mx*u10mx+v10mx*v10mx)
    # read max hrly uvvel
    if 'ops' in core or 'OPS' in core:
      print 'ops 400-1000 branch'
      mxuvvel=grbs.select(stepType='max',parameterName="220",typeOfLevel="isobaricLayer",topLevel=400,bottomLevel=1000)[0].values
      print 'past mxuvvel'
      maxdvvel=grbs.select(stepType='max',parameterName="221",typeOfLevel="isobaricLayer",topLevel=400,bottomLevel=1000)[0].values
      print 'past mxdvvel'
    else:
      print 'para 100-1000 branch'
      mxuvvel=grbs.select(stepType='max',parameterName="220",typeOfLevel="isobaricLayer",topLevel=100,bottomLevel=1000)[0].values
      maxdvvel=grbs.select(stepType='max',parameterName="221",typeOfLevel="isobaricLayer",topLevel=100,bottomLevel=1000)[0].values

    # max 1km agl ref
    mx1kmrefd=grbs.select(stepType='max',parameterName="198",typeOfLevel="heightAboveGround",level=1000)[0].values
    maxhourly=1
  except:
    maxhourly=0
    print 'Max hourlies unavailable - not plotting!'


# if dx < 6.0:
  try:
    # Echo top

# cat 16
    print('loop for etop')
    etop=grbs.select(stepType='instant',parameterCategory=16,parameterName="197",level=0)[0].values*3.28084 #convert meters to feet
    print 'etop(a) max and min in m:',np.max(etop),np.min(etop)
    etop=etop/1000.  #Convert to kft
    print 'etop max and min in kft:',np.max(etop),np.min(etop)
  except:
    etop=None
# else:
#   print 'Do not plot echo tops for coarse grids'
#   etop=None  
  
#  snhfx=grbs.select(name='Sensible heat net flux',stepType='instant',typeOfLevel='surface')[0].values
#  lhfx=grbs.select(name='Latent heat net flux',stepType='instant',typeOfLevel='surface')[0].values

  snhfx=None
  lhfx=None
  swup=None
  swdown=None
  lwup=None
  lwdown=None
  try:
    clearskydownswfx=grbs.select(name='Clear Sky Downward Solar Flux',stepType='instant',typeOfLevel='surface')[0].values
  except:
    clearskydownswfx=None

  netswf=0.
  netlwf=0.
  netrad=0.

 # End if test for analysis time


# Read in some cloud variables if they are available

# Set up the contour intervals to use for all mixing ratio plots here
q_cint=np.array([0.001, 0.005, 0.01, 0.05, 0.1, 0.25,0.5, 1., 2., 4., 6., 10., 15., 20., 25.]) # kg/m^2 --> mm
q_color_list=plt.cm.gist_stern_r(np.linspace(0, 1, len(q_cint)+1))
q_color_list_cmap=c.ListedColormap(q_color_list)
#cm.set_under(alpha = 0.0)
q_color_list_norm=c.BoundaryNorm(q_cint, q_color_list_cmap.N)


#  Total column-integrated lower-density snow/graupel (RF<10) and higher-density graupel/sleet/"hail" (RF>10)
try:
  rfthresh=10.
  tqsld,tqshd=qs_rimefactor_partition(grbs,nx,ny,rfthresh)
except:
  tqsld=None
  tqshd=None

try:
  lowcloudcover=grbs.select(parameterName='Low cloud cover',stepType='instant')[0].values
except:
  lowcloudcover=None

try:
  midcloudcover=grbs.select(parameterName='Medium cloud cover',stepType='instant')[0].values
except:
  midcloudcover=None

try:
  highcloudcover=grbs.select(parameterName='High cloud cover',stepType='instant')[0].values
except:
  highcloudcover=None

try:
  totalcloudcover=grbs.select(name='Total Cloud Cover',stepType='instant')[0].values
except:
  totalcloudcover=None

# Winter weather params
try:
  #Despite what pygrib tells us, this is in meters
  snowdepth=grbs.select(parameterName='Snow depth',typeOfLevel='surface')[0].values 
except:
  snowdepth=None

try:
  pofp=grbs.select(name='Percent frozen precipitation')[0].values
except:
  pofp=None

try:
  csnow=grbs.select(name='Categorical snow')[0].values
  cicep=grbs.select(name='Categorical ice pellets')[0].values
  cfzr=grbs.select(name='Categorical freezing rain')[0].values
  crain=grbs.select(name='Categorical rain')[0].values
#  ptype=crain/100+cfzr*2/100+cicep*3/100+csnow*4/100
# no divide if is the mean value
  ptype=crain+cfzr*2+cicep*3+csnow*4
  print 'have ptype'
except:
  print 'no have ptype'
  csnow=None
  cicep=None
  cfzr=None
  crain=None
  ptype=None

try:
  #Read lowest model level RF and mask with pofp
  RF_lev1=grbs.select(name='Rime factor',stepType='instant',typeOfLevel='hybrid',level=1)[0].values + 1e-6 #(add a tiny number to assist with plotting, otherwise it looks choppy)
  #qsnow_lev1=grbs.select(name='Snow mixing ratio',stepType='instant',typeOfLevel='hybrid',level=1)[0].values
  RF_lev1=np.where(pofp>0,RF_lev1,0.)
except:
  RF_lev1=None

# 500 hghts + wind barbs + vort
  

t2a=time.clock()
t3a=round(t2a-t1a, 3)
print("%.3f seconds to read all gribs msgs!") % t3a

exit

  ###################################################
  #       START PLOTTING FOR EACH DOMAIN            #
  ###################################################


def main(dom):

 plot_all(dom)
 print ('fake main')

# Number of processes must coincide with the number of domains to plot
#  pool = multiprocessing.Pool(len(domains))
#  pool.map(plot_all,domains)
  
def plot_all(dom):

    if '_GES' in domid or '_AN' in domid or '_FILT' in domid:
      anl_diag_skip=1
    else:
      anl_diag_skip=0
   
    t1dom=time.clock()
    print('Working on '+dom)

    # create figure and axes instances
    fig = plt.figure(figsize=(11,11))
    ax = fig.add_axes([0.1,0.1,0.8,0.8])
    

    if gribproj.lower()=='lambert':
      llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=ncepy.corners_res(dom,proj='lcc')
#      print("%.2f llcrnrlat now is ") % llcrnrlat
      if dom=='CONUS':
        print('specify new corner vals for CONUS')
        llcrnrlon=-121.0
        llcrnrlat=21.0
        urcrnrlon=-62.6
        urcrnrlat=49.0
      if dom=='NW':
        llcrnrlat=36.
        llcrnrlon=-126.
        urcrnrlat=53.
        urcrnrlon=-108.
      if dom=='NC':
        llcrnrlat=36.
        llcrnrlon=-112.
        urcrnrlat=52.5
        urcrnrlon=-82.
      if dom=='NE':
        llcrnrlat=39.
        llcrnrlon=-89.
        urcrnrlat=49.
        urcrnrlon=-63.
      if dom=='SW':
        llcrnrlat=22.
        llcrnrlon=-122.
        urcrnrlat=41.
        urcrnrlon=-106.
      if dom=='SC':
        llcrnrlat=24.
        llcrnrlon=-109.
        urcrnrlat=41.
        urcrnrlon=-85.
      if dom=='SE':
        llcrnrlat=24.
        llcrnrlon=-91.
        urcrnrlat=38.
        urcrnrlon=-68.
      if dom=='AK':
        llcrnrlat=53
        llcrnrlon=-165.
        urcrnrlat=72.
        urcrnrlon=-120.

#      print("%.2f llcrnrlat now(2) is ") % llcrnrlat
      m = Basemap(llcrnrlon=llcrnrlon,llcrnrlat=llcrnrlat,urcrnrlon=urcrnrlon,urcrnrlat=urcrnrlat,\
     	        rsphere=rearth,resolution=res,projection='lcc',\
                lat_1=Lat1,lat_2=Lat2,lat_0=Lat0,lon_0=Lon0,ax=ax)
    elif gribproj.lower()=='polar_stereographic':
      # create basemap instance and set the dimensions
      llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=ncepy.corners_res(dom,proj='stere') 
      m = Basemap(llcrnrlon=llcrnrlon,llcrnrlat=llcrnrlat,urcrnrlon=urcrnrlon,urcrnrlat=urcrnrlat,\
     	        rsphere=rearth,resolution=res,projection='stere',\
                lat_ts=Lat_ts,lat_0=Lat0,lon_0=Lon0,ax=ax)
    elif gribproj.lower()=='mercator':
      # Grib grid projection info
      llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=ncepy.corners_res(dom)
      m = Basemap(llcrnrlon=llcrnrlon,llcrnrlat=llcrnrlat,urcrnrlon=urcrnrlon,urcrnrlat=urcrnrlat,\
     	        rsphere=rearth,resolution=res,projection='merc',\
                lat_ts=Lat_ts,lon_0=Lon0,ax=ax)
    
    parallels = np.arange(-80.,90,10.)
    meridians = np.arange(0.,360.,10.)
#    m.drawmapboundary(fill_color='#7777ff')
#mptest    m.fillcontinents(color='#ddaa66', lake_color='#7777ff', zorder = 0)
    m.fillcontinents(color='#f9f8f4',zorder = 0)
    m.drawmapboundary(fill_color='#fffefc')
    m.drawcoastlines(linewidth=1.50,color='#ddaa66')
    m.drawstates(linewidth=1.00,color='#ddaa66')
    m.drawcountries(linewidth=0.75,color='#ddaa66')
    m.drawparallels(parallels,labels=[1,0,0,1])
    m.drawmeridians(meridians,labels=[1,0,0,1])


    t1=time.clock()
    t2 = time.clock()


    if dom != 'CONUS' and dom != 'AK':
      # Draw the the counties if not CONUS
      # Note that drawing the counties can slow things down!
      if dom in ['NAK','SAK','SWAK','SEAK']:
        if 'USHnam' in os.environ:
          print('Adding zones to AK SUBDOMAIN %s' % dom)
          USHnam=os.environ['USHnam']
          m.readshapefile(shapefile=USHnam+'/shapefiles/AK_zones/z_03de14', name='z_03de14',
                          drawbounds=True, linewidth=0.5, color='k')
      else:
        m.drawcounties(linewidth=0.2, color='k')
      skip=18
      if dom=='MIDATL': skip=15
      if dom in ['NAK','SAK','SWAK','SEAK'] and dx <= 6.0: skip=10
      if dx > 6.0:
        # Less 'skip' for coarser grids (tuned for NAM12)
        skip=8
        if dom=='MIDATL': skip=6
    elif dom=='CONUS': 
      skip=int(150/dx)
    elif dom=='AK':
      skip=int(150/dx)+2
    
    #  Map/figure has been set up here (bulk of the work), save axes instances for
    #     use again later   
    keep_ax_lst = ax.get_children()[:]


    if anl_diag_skip == 0:
      plt.title(domid+core+' MSLP (hPa) and Wind (kts)'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
    else:
      plt.title(domid+core+' MSLP (hPa)'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
    outfile='./hiresw_slp_'+dom+'_f'+fhr+'_'+domid+core+'.png'



    if dbz is not None:
      # Get the next plot ready:  REFC
      t1=time.clock()
      print('Working on refc for '+dom)   

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      #Now plot REFC dBZ
      clevs = [5,10,15,20,25,30,35,40,45,50,55,60,65,70,75]
      cs = m.contourf(lons,lats,dbz,clevs,cmap=ncepy.mrms_radarmap(),latlon=True,extend='max')
      cs.set_clim(5,75) 
      cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs)
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('dBZ')

      plt.title(domid+core+' Column Max Reflectivity \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')   
      plt.savefig('./hiresw_refc_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      subprocess.call(['convert','-density', '100x100', 'hiresw_refc_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_refc_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot refc for: "+dom) % t3

    if etop is not None:    # Get the next plot ready: ETOP 
      t1=time.clock()
      print('Working on etop for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      #Now plot ETOP
      #Try plotting airways and ARTCCs
      if 'USHnam' in os.environ:
        USHnam=os.environ['USHnam']
        m.readshapefile(shapefile=USHnam+'/shapefiles/airways/artcc_h', name='artcc', drawbounds=True, linewidth=.75, color='b', antialiased=1)
        m.readshapefile(shapefile=USHnam+'/shapefiles/airways/airway', name='airways',drawbounds=False) #MUST be FALSE - otherwise defaults to TRUE
        top25=ncepy.top25airways()
        for shape, artcc in zip(m.artcc, m.artcc_info):     
           shp=np.asarray(shape)
           name=ncepy.get_artcc_name(artcc['Name'])
           ha='center'
           x=np.mean(shp[:,0])
           y=np.mean(shp[:,1])
           if name=='ZNY': 
             ha='left'
             x=x+100000.
             y=y-100000.
           plt.text(x,y,name,ha=ha,size='x-small',color='r',weight='bold',clip_on=True)
        for shape, airway in zip(m.airways, m.airways_info):   
          if airway['AWY_ID'] in top25:
            #print 'Working on airway ',airway['AWY_ID']
            xx,yy = zip(*shape)
            m.plot(xx,yy,color='m',lw=0.5)
            top25.remove(airway['AWY_ID'])  #remove value from list - do not want to plot duplicates

      colors=['#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#cc66ff','#6600cc','#ff7f00','#b35900','#ff0000']
      clevs = np.arange(5.,55.,5.)
      cm = c.ListedColormap(colors)
      norm = c.BoundaryNorm(clevs, cm.N)
#      cs = m.contourf(lons,lats,etop,clevs,colors=colors,latlon=True,extend='max')
      cs = m.contourf(lons,lats,etop,clevs,cmap=cm,norm=norm,latlon=True,extend='max')
      cs.set_clim(5.,50.)
      cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('kft')
      plt.title(domid+core+' Echo Top \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_etop_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      subprocess.call(['convert','-density', '100x100', 'hiresw_etop_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_etop_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot etop for: "+dom) % t3

#      colors = ['#99ff66','#00ff00','#009933','#0033cc','#0066ff','#3399ff','#00ccff','#cc99ff','#9933ff','#990033','#ff3300','#ff9900','#996633','#ffff00']
#      cm = c.ListedColormap(colors)
#     cm.set_over('white')
#     norm = c.BoundaryNorm(clevs, cm.N)
#      cs = m.contourf(lons,lats,apcp1h,clevs,cmap=cm,norm=norm,latlon=True,extend='max')
#      cs.set_clim(0.01,4.00)


#--------------------------------------------------------------------------------------------
    if csnow is not None:
      # Get the next plot ready instantaneous onekmrefdprob40
       t1=time.clock()
       print('Working on CSNOW for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,csnow,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' prob ptype snow  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_csnow_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_csnow_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_csnow_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot csnow for: "+dom) % t3

    if crain is not None:
       t1=time.clock()
       print('Working on CRAIN for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,crain,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' prob ptype rain  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_crain_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_crain_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_crain_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot crain for: "+dom) % t3

    if cicep is not None:
       t1=time.clock()
       print('Working on CICEP for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,cicep,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' prob ptype ice pellets  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_cicep_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_cicep_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_cicep_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot cicep for: "+dom) % t3

    if cfzr is not None:
       t1=time.clock()
       print('Working on CFRZR for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,cfzr,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' prob ptype freezing rain  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_cfzr_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_cfzr_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_cfzr_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot cfzr for: "+dom) % t3

## combo ptype

#    if ptype is not None:

#       print 'here '
#       t1=time.clock()
#       print('Working on ptype for '+dom)
#       # Clear off old plottables but keep all the map info
#       ncepy.clear_plotables(ax,keep_ax_lst,fig)
#
#       cm = c.ListedColormap(['blue'])
#       cm2 = c.ListedColormap(['green'])
#       cm3 = c.ListedColormap(['red'])
#       cm4 = c.ListedColormap(['magenta'])
#       cm.set_under(alpha = 0.0)
#       cm2.set_under(alpha = 0.0)
#       cm3.set_under(alpha = 0.0)
#       cm4.set_under(alpha = 0.0)
#
#       cs = m.pcolormesh(lons,lats,crain/100,cmap=cm2,vmin=0.51,vmax=1.,latlon=True)
#       cs = m.pcolormesh(lons,lats,cicep/100,cmap=cm4,vmin=0.51,vmax=1.,latlon=True)
#       cs = m.pcolormesh(lons,lats,cfzr/100,cmap=cm3,vmin=0.51,vmax=1.,latlon=True)
#       cs = m.pcolormesh(lons,lats,csnow/100,cmap=cm,vmin=0.51,vmax=1.,latlon=True)
#
#       fc='#f0f0f0'
##       ax.annotate('Rain',xy=(0.15,0.025),xycoords='axes fraction',color='green', fontsize=11,weight='bold',
#               ha='center', va='center',bbox=dict(fc=fc),zorder=10)
#       ax.annotate('ZR',xy=(0.40,0.025),xycoords='axes fraction', color='red', fontsize=11,weight='bold',
#               ha='center', va='center',bbox=dict(fc=fc),zorder=10)
#       ax.annotate('IP',xy=(0.65,0.025),xycoords='axes fraction', color='magenta', fontsize=11,weight='bold',
###               ha='center', va='center',bbox=dict(fc=fc),zorder=10)
#       ax.annotate('Snow',xy=(0.90,0.025),xycoords='axes fraction', color='blue', fontsize=11,weight='bold',
#               ha='center', va='center',bbox=dict(fc=fc),zorder=10)
#       plt.title(domid+core+' Precipitation Type \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#       plt.savefig('./hiresw_ptype_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
#       subprocess.call(['convert','-density', '100x100', 'hiresw_ptype_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_ptype_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
#       t2 = time.clock()
#       t3=round(t2-t1, 3)
#       print("%.3f seconds to plot ptype "+dom) % t3

    if ptype is not None:
       print 'here '
       t1=time.clock()
       print('Working on ptype for '+dom)
       # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       clevs=[1,2,3,4]
       cm = c.ListedColormap(['green','red','magenta','blue'])
       cm.set_under(alpha = 0.0)
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.pcolormesh(lons,lats,ptype,cmap=cm,vmin=1,vmax=4,norm=norm,latlon=True)
       fc='#f0f0f0'
       ax.annotate('Rain',xy=(0.15,0.025),xycoords='axes fraction',color='green', fontsize=11,weight='bold',
               ha='center', va='center',bbox=dict(fc=fc),zorder=10)
       ax.annotate('ZR',xy=(0.40,0.025),xycoords='axes fraction', color='red', fontsize=11,weight='bold',
               ha='center', va='center',bbox=dict(fc=fc),zorder=10)
       ax.annotate('IP',xy=(0.65,0.025),xycoords='axes fraction', color='magenta', fontsize=11,weight='bold',
               ha='center', va='center',bbox=dict(fc=fc),zorder=10)
       ax.annotate('Snow',xy=(0.90,0.025),xycoords='axes fraction', color='blue', fontsize=11,weight='bold',
               ha='center', va='center',bbox=dict(fc=fc),zorder=10)
       plt.title(domid+core+' Precipitation Type \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_ptypemean_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_ptypemean_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_ptypemean_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot ptype "+dom) % t3


## end combo ptype


    if apcp6h is not None:
      # Get the next plot ready instantaneous APCP6H
       t1=time.clock()
       print('here Working on APCP6H for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp6h

#       colors = ['#99ff66','#00ff00','#009933','#0033cc','#0066ff','#3399ff','#00ccff','#cc99ff','#9933ff','#990033','#ff3300','#ff9900','#996633','#ffff00']
       clevs = [0.01,0.10,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00,5.00,6.00,7.00,10.0]
       cm = c.ListedColormap(pcolors)
#       cm=plt.get_cmap(name='gist_ncar')
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp6h,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(0.01,10.0)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 6h APCP'+' '+grbtime+'Z cycle f'+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp6h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp6h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp6h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP6h for: "+dom) % t3

#--------------------------------------------------------------------------------------------

    if apcp1h is not None:
      # Get the next plot ready instantaneous APCP1H
       t1=time.clock()
       print('here Working on APCP1H for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp1h

#       colors = ['#99ff66','#00ff00','#009933','#0033cc','#0066ff','#3399ff','#00ccff','#cc99ff','#9933ff','#990033','#ff3300','#ff9900','#996633','#ffff00']
#       clevs = [0.01,0.10,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00,5.00,6.00,7.00,10.0]
       clevs = [0.01,0.05,0.10,0.15,0.20,0.25,0.35,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00]
       cm = c.ListedColormap(pcolors)
#       cm=plt.get_cmap(name='gist_ncar')
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp1h,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(0.01,4.0)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 1h APCP \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp1h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp1h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp1h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP1h for: "+dom) % t3

# ----------

    if sn3h1inch is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on SN3H1inch for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot sn3h1inch
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,95.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,sn3h1inch,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h SNOW > 1" (10:1 SLR) \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_sn3h1inch_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_sn3h1inch_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_sn3h1inch_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot SN3H1inch for: "+dom) % t3

# ----------

    if sn1h1inch is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on SN1H1inch for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot sn1h1inch
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,95.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,sn1h1inch,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 1h SNOW > 1" (10:1 SLR) \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_sn1h1inch_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_sn1h1inch_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_sn1h1inch_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot SN1H1inch for: "+dom) % t3

# ----------
    if sn3h3inch is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on SN3H3inch for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot sn3h3inch
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,95.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,sn3h3inch,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h SNOW > 3" (10:1 SLR) \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_sn3h3inch_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_sn3h3inch_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_sn3h3inch_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot SN3H3inch for: "+dom) % t3


#--------------------------------------------------------------------------------------------
    if apcp12h is not None:
      # Get the next plot ready instantaneous APCP12H
       t1=time.clock()
       print('here Working on APCP12H for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp12h

#       colors = ['#99ff66','#00ff00','#009933','#0033cc','#0066ff','#3399ff','#00ccff','#cc99ff','#9933ff','#990033','#ff3300','#ff9900','#996633','#ffff00']
       clevs = [0.01,0.05,0.10,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00,5.00,6.00,7.00,10.0]
       cm = c.ListedColormap(pcolors)
#       cm=plt.get_cmap(name='gist_ncar')
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp12h,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(0.01,10.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 12h APCP'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp12h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp12h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp12h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP12h for: "+dom) % t3

#------------------------------------

    if apcp24h is not None:
      # Get the next plot ready instantaneous APCP24H
       t1=time.clock()
       print('Working on APCP24H for '+dom)   
      
      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)
      
       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp24h

       clevs = [0.01,0.10,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00,5.00,6.00,8.00,10.00,15.0]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp24h,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(0.01,15.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 24h APCP \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp24h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp24h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp24h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP24h for: "+dom) % t3	   

#------------------------------------
    if apcp48h is not None:
      # Get the next plot ready instantaneous APCP48H
       t1=time.clock()
       print('Working on APCP48H for '+dom)   
      
      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)
      
       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp48h

       clevs = [0.01,0.10,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00,5.00,6.00,8.00,10.00,15.0]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp48h,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(0.01,15.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 48h APCP \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp48h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp48h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp48h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP48h for: "+dom) % t3	   

#--------------------------------------------------------------------------------------------
    if apcp3h is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3H for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3h
       clevs = [0.01,0.05,0.10,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00,5.00,6.00,7.00,10.0]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3h,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(0.01,10.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 3h APCP \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3h for: "+dom) % t3

# ----------------------


    if snow1h is not None:
      # Get the next plot ready instantaneous SNOW1hj
       t1=time.clock()
       print('here Working on SNOW1H for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot snow1h

       clevs = [0.10,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,3.50,4.00]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,snow1h,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(0.1,4.0)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 1h SNOW (10:1 SLR) \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_snow1h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_snow1h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_snow1h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
       subprocess.call(['rm','hiresw_snow1h_'+dom+'_f'+fhr+'_'+domid+core+'.png'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot SNOW1h for: "+dom) % t3


#--------------------------------------------------------------------------------------------

    if snow3h is not None:
      # Get the next plot ready instantaneous SNOW3h
       t1=time.clock()
       print('here Working on SNOW3H for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot snow3h

       clevs = [0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00,5.00,6.00,7.00,10.0]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,snow3h,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(0.1,10.0)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 3h SNOW (10:1 SLR) \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_snow3h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_snow3h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_snow3h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
       subprocess.call(['rm','hiresw_snow3h_'+dom+'_f'+fhr+'_'+domid+core+'.png'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot SNOW3h for: "+dom) % t3

#--------------------------------------------------------------------------------------------
    if onekmrefdprob40 is not None:
      # Get the next plot ready instantaneous onekmrefdprob40
       t1=time.clock()
       print('Working on ONEKMREFDPROB40 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot onekmrefdprob40
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,onekmrefdprob40,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 1 km prob refd > 40 dBZ  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_onekmrefdprob40_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_onekmrefdprob40_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_onekmrefdprob40_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot onekmrefdprob40 for: "+dom) % t3

#--------------------------------------------------------------------------------------------
    if mx1kmrefdprob40 is not None:
      # Get the next plot ready instantaneous onekmrefdprob40
       t1=time.clock()
       print('Working on MX1KMREFDPROB40 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot mx1kmrefdprob40
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,mx1kmrefdprob40,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' mx 1 km prob refd > 40 dBZ  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_mx1kmrefdprob40_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_mx1kmrefdprob40_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_mx1kmrefdprob40_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)

#--------------------------------------------------------------------------------------------
    if mxuphlprob25 is not None:
      # Get the next plot ready
       t1=time.clock()
       print('Working on MXUPHLPROB25 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot 
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,mxuphlprob25,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' mx 2-5 km prob UPHL > 25 m^2 s^-2  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_mxuphlprob25_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_mxuphlprob25_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_mxuphlprob25_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)

#--------------------------------------------------------------------------------------------
    if vis800 is not None:
      # Get the next plot ready instantaneous vis800
       t1=time.clock()
       print('Working on VIS800 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot vis800
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,vis800,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' prob visibility < 800 m  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_vis800_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_vis800_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_vis800_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)

#--------------------------------------------------------------------------------------------
## HERE
    if vis is not None:
      # Get the next plot ready instantaneous vis
       t1=time.clock()
       print('Working on VIS for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[14,15,16,17,18,19,20,30,29,28,27,21,22,23,24,25,26]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot vis
       clevs = [1.,300.,600.,900.,1200.,1500.,1800.,2100.,2500.,3000.,4000.,5000.,6000.,8000.,10000.,15000.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,vis,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(0.0,20000.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('Visibility (m)')
       plt.title(domid+core+' visibility (m) \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_vis_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_vis_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_vis_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
       subprocess.call(['rm','hiresw_vis_'+dom+'_f'+fhr+'_'+domid+core+'.png'])

       t2 = time.clock()
       t3=round(t2-t1, 3)

#--------------------------------------------------------------------------------------------

    if pwat is not None:
      # Get the next plot ready instantaneous pwat
       t1=time.clock()
       print('Working on PWAT for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)
      #Now plot pwat
       clevs = [5,10,15,20,25,30,35,40,45,50,55,60,65,70,75]
       cs = m.contourf(lons,lats,pwat,clevs,cmap=ncepy.mrms_radarmap(),latlon=True,extend='max')
       cs.set_clim(5,75)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs)
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('mm')
       plt.title(domid+core+' Precipitable water (mm) \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_pwat_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_pwat_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_pwat_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
       subprocess.call(['rm','hiresw_pwat_'+dom+'_f'+fhr+'_'+domid+core+'.png'])

       t2 = time.clock()
       t3=round(t2-t1, 3)


    if cape is not None:
      # Get the next plot ready instantaneous cape
       t1=time.clock()
       print('Working on CAPE for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3h
       clevs = [100.,250.,500.,750.,1000.,1250.,1500.,1750.,2000.,2250.,2500.,2750.,3000.,3500.,4000.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,cape,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(100.,6000.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('J/kg')
       plt.title(domid+core+' Mean ML CAPE \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_cape_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_cape_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_cape_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
       subprocess.call(['rm','hiresw_cape_'+dom+'_f'+fhr+'_'+domid+core+'.png'])


#--------------------------------------------------------------------------------------------
    if ceil610 is not None:
      # Get the next plot ready instantaneous ceil610
       t1=time.clock()
       print('Working on CEIL610 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot ceil610
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,ceil610,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' prob ceiling heigh < 610 m  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_ceil610_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_ceil610_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_ceil610_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)

#--------------------------------------------------------------------------------------------
    if ceil305 is not None:
      # Get the next plot ready instantaneous ceil305
       t1=time.clock()
       print('Working on CEIL305 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot ceil305
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,ceil305,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' prob ceiling heigh < 305 m  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_ceil305_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_ceil305_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_ceil305_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)

#--------------------------------------------------------------------------------------------
    if t2mprob273 is not None:
      # Get the next plot ready
       t1=time.clock()
       print('Working on T2MPROB273for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot 
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,t2mprob273,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 2 m TMP < 273 K \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_t2mprob273_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_t2mprob273_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_t2mprob273_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)


#--------------------------------------------------------------------------------------------
    if td2mgt285 is not None:
      # Get the next plot ready instantaneous td2mgt285
       t1=time.clock()
       print('Working on TD2MGT285 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot td2mgt285
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,td2mgt285,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' prob 2 m Td > 55 F \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_td2mgt285_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_td2mgt285_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_td2mgt285_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)


#--------------------------------------------------------------------------------------------


    if windprobgt20 is not None:
      # Get the next plot ready
       t1=time.clock()
       print('Working on WINDPROBGT20'+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot 
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,windprobgt20,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 10 m WIND SPEED > 20 kt \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_windprobgt20_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_windprobgt20_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_windprobgt20_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)


# ---------------------------------

    if apcp3hp254 is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3HP254 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3hp254
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,95.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3hp254,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h APCP > 0.01" \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3hp254_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3hp254_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3hp254_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3hp254 for: "+dom) % t3

# ----------

    if apcp3h12p4 is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3H12P4 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3h12p4
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,95.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3h12p4,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h APCP > 0.5" \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3h12p4_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3h12p4_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3h12p4_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3h12p4 for: "+dom) % t3

# ----------

    if apcp3h12p7 is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3H12P7 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3h12p7
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,95.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3h12p7,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h APCP > 0.5" \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3h12p7_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3h12p7_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3h12p7_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3h12p7 for: "+dom) % t3

# ----------

    if apcp1h12p7 is not None:
      # Get the next plot ready instantaneous APCP1H
       t1=time.clock()
       print('Working on APCP1H12P7 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp1h12p7
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,95.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp1h12p7,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 1h APCP > 0.5" \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp1h12p7_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp1h12p7_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp1h12p7_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP1h12p7 for: "+dom) % t3

# ----------

    if apcp3h25p1 is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3H25P1 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3h25p1
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3h25p1,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h APCP > 1.0" \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3h25p1_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3h25p1_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3h25p1_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3h25p1 for: "+dom) % t3


    if apcp3h25p4 is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3H25P4 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3h25p4
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3h25p4,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h APCP > 1.0" \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3h25p4_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3h25p4_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3h25p4_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3h25p4 for: "+dom) % t3

# ----------

    if apcp3hffg3 is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3HFFG3 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3hffg3
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,99.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3hffg3,clevs,cmap=cm,norm=norm,latlon=True,extend='max')
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h APCP \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3hffg3_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3hffg3_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3hffg3_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3hffg3  for: "+dom) % t3

# ----------

    if apcp3hffg4 is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3HFFG4 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3hffg4
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3hffg4,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h APCP > 3 h FFG (smoothed)  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3hffg4_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3hffg4_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3hffg4_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3hffg4  for: "+dom) % t3

# ----------

    if apcp3hri10 is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3HRI10 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3hffg4
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3hri10,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 3h APCP > 10 yr RI \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3hri10_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3hri10_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3hri10_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3hri10  for: "+dom) % t3

# ----------

    if apcp6hffg6 is not None:
      # Get the next plot ready instantaneous APCP6H
       t1=time.clock()
       print('Working on APCP6HFFG6 for '+dom)

      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)

       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp6hffg6
       clevs = [5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100.]
       cm = c.ListedColormap(pcolors)
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp6hffg6,clevs,cmap=cm,norm=norm,latlon=True)
       cs.set_clim(1.0,100.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('probability')
       plt.title(domid+core+' 6h APCP > 6h FFG  \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp6hffg6_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp6hffg6_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp6hffg6_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP6hffg6  for: "+dom) % t3


#--------------------------------------------------------------------------------------------

   #This block is for max hourly fields

    if maxhourly==1:
#---------------------------------------------------------------------------------------------    
      # Get the next plot ready MAX HOURLY 1 KM AGL REF
      t1=time.clock()
      print('Working on mx1kmrefd for '+dom)   
      
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

    #Now plot REFC dBZ
      clevs = [5,10,15,20,25,30,35,40,45,50,55,60,65,70,75]
      cs = m.contourf(lons,lats,mx1kmrefd,clevs,cmap=ncepy.mrms_radarmap(),latlon=True)
      cs.set_clim(5,75) 
      cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs)
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('dBZ')
      plt.title(domid+core+' Max Hourly 1 km AGL Reflectivity \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
     
      plt.savefig('./hiresw_mx1kmrefd_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      subprocess.call(['convert','-density', '100x100', 'hiresw_mx1kmrefd_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_mx1kmrefd_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
  
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot mx1kmrefd for: "+dom) % t3    
    
    
#---------------------------------------------------------------------------------------------    
      # Get the next plot ready MAX HOURLY UPHL
      t1=time.clock()
      print('Working on mxuphl for '+dom)   
    
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      #Now plot max hourly uphl
      colors = ['lightgray','skyblue','mediumblue','green','darkgreen','orchid','firebrick','orangered','red','DarkViolet','black']
      clevs = [4.,8.,12.,20.,30.,40.,50.,75.,100.,125.,150.]
      cm = c.ListedColormap(colors)
      cm.set_over('white')
      norm = c.BoundaryNorm(clevs, cm.N)
      cs = m.contourf(lons,lats,mxuphl,clevs,cmap=cm,norm=norm,latlon=True,extend='max')
      cs.set_clim(4.,150.) 
      cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('m2/s2')
      #plot contours too
#      cs = m.contour(lons,lats,mxuphl,clevs,colors='k',linewidths=1.5,latlon=True)
#      clab = plt.clabel(cs,clevs[::3],inline=1, fontsize=10,fmt='%.0f')
      
      plt.title(domid+core+' Max Hourly Updraft Helicity \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
  
      plt.savefig('./hiresw_mxuphl_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      subprocess.call(['convert','-density', '100x100', 'hiresw_mxuphl_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_mxuphl_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
  
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot mxuphl for: "+dom) % t3	 

#--------------------------------------------------------------------------------------------    
      # Get the next plot ready MAX HOURLY 10mw
      t1=time.clock()
      print('Working on spd10mx for '+dom)   
      
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      
      #Now plot max hourly 10mw
      clevs = np.arange(10.,75.,5.)
      cs = m.contourf(lons,lats,ncepy.ms2kts(spd10mx),clevs,cmap=plt.get_cmap(name='Paired'),latlon=True,extend='max')
      cs.set_clim(10,70) 
      cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs)
      cbar.set_label('kts')
      cbar.ax.tick_params(labelsize=8.5) 
      #plot contours too
#      cs = m.contour(lons,lats,spd10mx,clevs,colors='k',linewidths=1.5,latlon=True)
#      clab = plt.clabel(cs,clevs[::3],inline=1, fontsize=10,fmt='%.0f')
      
      plt.title(domid+core+' Max Hourly 10 m Wind \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
  
      plt.savefig('./hiresw_max10mw_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      subprocess.call(['convert','-density', '100x100', 'hiresw_max10mw_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_max10mw_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
  
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot spd10mx for: "+dom) % t3	   

#--------------------------------------------------------------------------------------------    

      if apcp3h is not None:
      # Get the next plot ready instantaneous APCP3H
       t1=time.clock()
       print('Working on APCP3H for '+dom)   
      
      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)
      
       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp3h

#       colors = ['#99ff66','#00ff00','#009933','#0033cc','#0066ff','#3399ff','#00ccff','#cc99ff','#9933ff','#990033','#ff3300','#ff9900','#996633','#ffff00']
       clevs = [0.01,0.05,0.10,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00,5.00]
       cm = c.ListedColormap(pcolors)
#       cm=plt.get_cmap(name='gist_ncar')
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp3h,clevs,cmap=cm,norm=norm,latlon=True,extend='max')
       cs.set_clim(0.01,5.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 3h APCP \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp3h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp3h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp3h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP3h for: "+dom) % t3	   

#--------------------------------------------------------------------------------------------    
      if apcp6h is not None:
      # Get the next plot ready instantaneous APCP6H
       t1=time.clock()
       print('Working on APCP6H for '+dom)   
      
      # Clear off old plottables but keep all the map info
       ncepy.clear_plotables(ax,keep_ax_lst,fig)
      
       gemlist=ncepy.gem_color_list()
       pcplist=[21,22,23,24,25,26,27,28,29,14,15,16,17,18,19,20]
       #Extract these colors to a new list for plotting
       pcolors=[gemlist[i] for i in pcplist]

      #Now plot apcp6h

#       colors = ['#99ff66','#00ff00','#009933','#0033cc','#0066ff','#3399ff','#00ccff','#cc99ff','#9933ff','#990033','#ff3300','#ff9900','#996633','#ffff00']
       clevs = [0.01,0.05,0.10,0.25,0.50,0.75,1.00,1.50,2.00,2.50,3.00,4.00,5.00,6.00,7.00]
       cm = c.ListedColormap(pcolors)
#       cm=plt.get_cmap(name='gist_ncar')
       cm.set_over('white')
       norm = c.BoundaryNorm(clevs, cm.N)
       cs = m.contourf(lons,lats,apcp6h,clevs,cmap=cm,norm=norm,latlon=True,extend='max')
       cs.set_clim(0.01,7.)
       cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.2f')
       cbar.ax.tick_params(labelsize=8.5)
       cbar.set_label('inches')
       plt.title(domid+core+' 6h APCP \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
       plt.savefig('./hiresw_apcp6h_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
       subprocess.call(['convert','-density', '100x100', 'hiresw_apcp6h_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_apcp6h_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])

       t2 = time.clock()
       t3=round(t2-t1, 3)
       print("%.3f seconds to plot APCP6h for: "+dom) % t3	   


# ---------------------------------

#--------------------------------------------------------------------------------------------    
    if onekmrefd is not None:
      # Get the next plot ready 1 KM AGL REF
      t1=time.clock()
      print('Working on 1kmrefd for '+dom)         
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      #Now plot 1km agl refd
      clevs = [5,10,15,20,25,30,35,40,45,50,55,60,65,70,75]
      cs = m.contourf(lons,lats,onekmrefd,clevs,cmap=ncepy.mrms_radarmap(),latlon=True,extend='max')
      cs.set_clim(5,75) 
      cbar = m.colorbar(cs,location='bottom',size="2%",pad="5%",ticks=clevs)
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('dBZ')
      plt.title(domid+core+' 1 km AGL refl '+repr(date)+' '+grbtime+'Z cycle f'+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_1kmrefd_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight') 
      subprocess.call(['convert','-density', '100x100', 'hiresw_1kmrefd_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_1kmrefd_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot 1kmrefd for: "+dom) % t3     

#---- End of max hourly if-test


   #This block is for avoiding sfc/land fields when doing 
   #  analysis diag plotting

    if anl_diag_skip == 0:
   
#-   -------------------------------------------------------------------------------------------    
     # Get the next plot ready 2mt
     t1=time.clock()
     if t2m is not None:
      print('Working on 2mt for '+dom)   
      
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      clevs = np.arange(-6.,36.,2)

      #Now plot t2m
      cs1 = m.contourf(lons,lats,t2m,clevs,cmap=ncepy.ncl_t2m(),latlon=True)
      cs1.set_clim(-7, 37.)
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')    
      cbar.ax.tick_params(labelsize=8.5)    
      cbar.set_label('C')

      plt.title(domid+core+' 2 m T \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_2mtw_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      subprocess.call(['convert','-density', '100x100', 'hiresw_2mtw_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_2mtw_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot 2mtw "+dom) % t3

#-   -------------------------------------------------------------------------------------------
      # Get the next plot ready for skin temp
      t1=time.clock()

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      clevs = np.arange(-36.,104.,4)
      #Now plot
#      mysoilt=maskoceans(lons, lats, tsoil_0_10, inlands=True, resolution=res)
#      #Reverse StepSeq
#      cs1 = m.contourf(lons,lats,mysoilt,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
#      cs1.set_clim(-36, 100.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('F')
#      plt.title(domid+core+' 0-10 cm Soil Temperature \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_tsoil0-10_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(-36.,104.,4)
#      #Now plot
#      mysoilt=maskoceans(lons, lats, tsoil_10_40, inlands=True, resolution=res)
#      cs1 = m.contourf(lons,lats,mysoilt,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
#      cs1.set_clim(-36, 100.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('F')
#      plt.title(domid+core+' 10-40 cm Soil Temperature \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_tsoil10-40_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
###
#      # Clear off old plottables but keep all the map info
#      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(-36.,104.,4)
#      #Now plot
#      mysoilt=maskoceans(lons, lats, tsoil_40_100, inlands=True, resolution=res)
#      cs1 = m.contourf(lons,lats,mysoilt,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
#      cs1.set_clim(-36, 100.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('F')
#      plt.title(domid+core+' 40-100 cm Soil Temperature \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_tsoil40-100_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
#      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(-36.,104.,4)
#      #Now plot
#      mysoilt=maskoceans(lons, lats, tsoil_100_200, inlands=True, resolution=res)
#      cs1 = m.contourf(lons,lats,mysoilt,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
#      cs1.set_clim(-36, 100.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('F')
#      plt.title(domid+core+' 100-200 cm Soil Temperature \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_tsoil100-200_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      t2 = time.clock()
      t3=round(t2-t1, 3)


#-   -------------------------------------------------------------------------------------------    
      # Get the next plot ready 2mtd
     if td2m is not None:
      t1=time.clock()
      print('Working on 2mtd for '+dom)   
      
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      clevs = np.arange(-27.,21.,3.)
      cs1.set_clim(-27., 21.)

      #Now plot td2m
      cs1 = m.contourf(lons,lats,td2m,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')    
      cbar.ax.tick_params(labelsize=8.5)    
      cbar.set_label('C')

      #plot wind barbs
      
      m.barbs(lons[::skip,::skip],lats[::skip,::skip],u10[::skip,::skip],v10[::skip,::skip],latlon=True,length=5.5,sizes={'spacing':0.2},pivot='middle')
      
      plt.title(domid+core+' 2 m Td and 10 m Wind (kts)\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_2mtdw_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      subprocess.call(['convert','-density', '100x100', 'hiresw_2mtdw_'+dom+'_f'+fhr+'_'+domid+core+'.png', 'hiresw_2mtdw_'+dom+'_f'+fhr+'_'+domid+core+'.gif'])
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot 2mtdw "+dom) % t3


#-   --------------------------------------------------------------------------------------------
#    Now plot the radiation fields

      t1=time.clock()
      print('Working on radiation fields for '+dom)
      gdhfx=None

      if gdhfx is not None:
        # Clear off old plottables but keep all the map info
        ncepy.clear_plotables(ax,keep_ax_lst,fig)

        # Set contour levels    
        clevs = [-300.,-200.,-100.,-75.,-50.,-25.0,-10.0,0.,10.0,25.,50.,75.,100.,200.,300.]
        #Now plot 
        mygdhfx=maskoceans(lons, lats, gdhfx, inlands=True, resolution=res)
        cs1 = m.contourf(lons,lats,mygdhfx,clevs,cmap=ncepy.ncl_grnd_hflux(),latlon=True,extend='both')
        cs1.set_clim(-300.,300.)
        # add colorbar.
        cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
        cbar.ax.tick_params(labelsize=8.5)
        cbar.set_label('W/m^2')

        plt.title(domid+core+' Ground Heat Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
        plt.savefig('./hiresw_ghflux_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
 
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      # Set contour levels    
#      clevs = [-2500.,-2000.,-1500.,-1000.,-750.,-500.,-300.,-200.,-100.,-75.,-50.,-25.0,0.,25.,50.,75.,100.,200.,300.,500.,750.,1000.,1500.,2000.,2500]
#      cs1 = m.contourf(lons,lats,netrad,clevs,cmap=ncepy.ncl_grnd_hflux(),latlon=True,extend='both')
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('W/m^2')
#      plt.title(domid+core+' Surface Net Radiative Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_netrad_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      # Set contour levels    
#      clevs = [-2500.,-2000.,-1500.,-1000.,-750.,-500.,-300.,-200.,-100.,-75.,-50.,-25.0,0.,25.,50.,75.,100.,200.,300.,500.,750.,1000.,1500.,2000.,2500]
#      cs1 = m.contourf(lons,lats,lhfx,clevs,cmap=ncepy.ncl_grnd_hflux(),latlon=True,extend='both')
      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('W/m^2')
#      plt.title(domid+core+' Latent Heat Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_lhfx_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      # Set contour levels    
#      clevs = [-2500.,-2000.,-1500.,-1000.,-750.,-500.,-300.,-200.,-100.,-75.,-50.,-25.0,0.,25.,50.,75.,100.,200.,300.,500.,750.,1000.,1500.,2000.,2500]
#      cs1 = m.contourf(lons,lats,snhfx,clevs,cmap=ncepy.ncl_grnd_hflux(),latlon=True,extend='both')
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('W/m^2')
#      plt.title(domid+core+' Sensible Heat Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_snhfx_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      # Set contour levels    

      # Set contour levels
#      clevs = np.arange(0,1025,25)
#      cs1 = m.contourf(lons,lats,swdown,clevs,cmap=plt.get_cmap(name='Spectral_r'),latlon=True,extend='max')
#      #cs1.cmap.set_under('white')
#      cs1.set_clim(0,1000.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('W/m^2')
#      for label in cbar.ax.xaxis.get_ticklabels()[::2]:  #Only show every other ticklabel on the colorbar
#     	label.set_visible(False)
#      plt.title(domid+core+' Surface Downward Shortwave Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_swdown_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

#      if clearskydownswfx is not None:
#        # Clear off old plottables but keep all the map info
#        ncepy.clear_plotables(ax,keep_ax_lst,fig)
#  
#        # Set contour levels    
#        clevs = np.arange(0,1025,25)
#        #Now plot
#        cs1 = m.contourf(lons,lats,clearskydownswfx,clevs,cmap=plt.get_cmap(name='Spectral_r'),latlon=True,extend='max')
#        #cs1.cmap.set_under('white')
#        cs1.set_clim(0,1000.)
#        # add colorbar.
#        cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
#        cbar.ax.tick_params(labelsize=8.5)
#        cbar.set_label('W/m^2')
#        for label in cbar.ax.xaxis.get_ticklabels()[::2]:  #Only show every other ticklabel on the colorbar
#          label.set_visible(False)
#        plt.title(domid+core+' Clear Sky Surface Downward Shortwave Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#        plt.savefig('./hiresw_clearskydownswfx_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

  
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      # Set contour levels    
#      clevs = np.arange(0,525,25) 
#      #Now plot
#      cs1 = m.contourf(lons,lats,swup,clevs,cmap=plt.get_cmap(name='Spectral_r'),latlon=True,extend='max')
#      #cs1.cmap.set_under('white')
#      cs1.set_clim(0,500.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',size="2%",pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('W/m^2')
#      plt.title(domid+core+' Surface Upward Shortwave Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_swup_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      # Set contour levels    
#      clevs = np.arange(0,525,25)
#      cs1 = m.contourf(lons,lats,lwup,clevs,cmap=plt.get_cmap(name='Spectral_r'),latlon=True,extend='max')
#      #cs1.cmap.set_under('white')
#      cs1.set_clim(0,500.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      #plot contour lines

#      cbar.set_label('W/m^2')
#      plt.title(domid+core+' Surface Upward Longwave Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_lwup_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      # Set contour levels    
#      clevs = np.arange(0,525,25)
#      #Now plot 
#      cs1 = m.contourf(lons,lats,lwdown,clevs,cmap=plt.get_cmap(name='Spectral_r'),latlon=True,extend='max')
#      #cs1.cmap.set_under('white')
#      cs1.set_clim(0,500.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('W/m^2')
#      plt.title(domid+core+' Surface Downward Longwave Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_lwdown_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot radiation fields "+dom) % t3


    t3dom=round(t2-t1dom, 3)
    print("%.3f seconds to plot ALL for: "+dom) % t3dom
    plt.clf()


#############################################
  
main('AK')
main('SAK')
main('SEAK')
