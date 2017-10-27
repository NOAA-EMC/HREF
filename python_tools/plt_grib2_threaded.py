import pygrib
import numpy as np
import numpy.ma as ma
import matplotlib.pyplot as plt
import ncepy
from mpl_toolkits.basemap import Basemap, cm, maskoceans
import time,sys,scipy,multiprocessing,os
from matplotlib import colors as c

#----------Define some useful functions needed to start---------------------------------------------#

def qs_rimefactor_partition(grbs,nx,ny,rfthresh):
  #  Calculate total column-integrated 
  #    lower-density snow/graupel (RF<rfthresh) and 
  #     higher-density graupel/sleet/"hail" (RF>=rfthresh)
  # ---------------------------------------------------
  # Retrieve list of msgs/dictionaries needed to do the calculation first (much faster)

  # Could make more general by creating a list of available pressures
  #  and then sorting and looping through.  But this works fine for now.

  rft1=time.clock()
  rfmsgs=grbs.select(name='Rime factor',typeOfLevel='isobaricInhPa')
  zmsgs=grbs.select(name='Geopotential Height',typeOfLevel='isobaricInhPa')
  snowmsgs=grbs.select(name='Snow mixing ratio',typeOfLevel='isobaricInhPa')
  tmsgs=grbs.select(name='Temperature',typeOfLevel='isobaricInhPa')
  qmsgs=grbs.select(name='Specific humidity',typeOfLevel='isobaricInhPa')  
  tqsld=np.zeros([ny,nx])
  tqshd=np.zeros([ny,nx])
  for k in np.arange(0,np.size(rfmsgs)-1):    
    p=rfmsgs[k]['level']      # Grab level metadata (pressure in hPa)
    pbot=rfmsgs[k+1]['level'] # Grab level metadata (one layer below, in hPa)
    ztop=next(l for l in zmsgs if l['level'] == p).values  # m
    zbot=next(l for l in zmsgs if l['level'] == pbot).values  # m
    dz=ztop-zbot                                              # m
    snow=next(l for l in snowmsgs if l['level'] == p).values  # kg/kg
    t=next(l for l in tmsgs if l['level'] == p).values   # K
    q=next(l for l in qmsgs if l['level'] == p).values   #kg/kg
    denom=(287.04*t*(q*0.608+1.))
    # Code may whine about div by zero on the following line
    # After investigating I think this is because
    # denom is a masked array and p*100.0 is a scalar.  However, the fill_value is 1e20.
    # So this still shouldn't throw an error if the divison is done.
    rhoa=(p*100.0)/denom             #kg/m**3
    rf=rfmsgs[k].values
    tqshd=tqshd+np.where(rf>=rfthresh,snow*dz*rhoa,0.)   # mm
    tqsld=tqsld+np.where(rf<rfthresh,snow*dz*rhoa,0.)    # mm
  rft2=time.clock()
  print ("%.3f seconds to read and calculate high and low density column integrated Qs!") % round(rft2-rft1, 3)
  return tqsld,tqshd

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
fhr=grbs[1]['stepRange'] # Forecast hour
# Pad fhr with a 0
fhr=str(fhr).zfill(2)
cyctime=grbs[1].dataTime #Cycle (e.g. 1200 UTC)
  
#Pad with a zero and convert to a string
grbtime=str(cyctime).zfill(4)
  
date=grbs[1].dataDate    #PDY
# Now we need to figure out the forecast valid date/time
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
  domains=['CONUS','NW','NC','NE','SW','SC','SE','Great_Lakes','MIDATL']
  anl_diag_skip=0

# Start reading fields to be plotted
# read prmsl, convert to hPa (mb).
mslp_msgs=grbs.select(name='Pressure reduced to MSL')[0]
mslp=mslp_msgs.values*0.01

mslp=scipy.ndimage.gaussian_filter(mslp, 2)
passes="one pass"
if dx < 6.0:
  # Smooth mslp using 2 passes of a Gaussian filter if this is a hi-res grid
  mslp=scipy.ndimage.gaussian_filter(mslp, 2)
  passes="two passes"

try:
  # read refc dBZ
  dbz = grbs.select(name='Maximum/Composite radar reflectivity')[0].values # This is actually REFC dBZ
except:
  dbz=None

try:
  # 1km AGL ref
  onekmrefd=grbs.select(name='Derived radar reflectivity',typeOfLevel='heightAboveGround',level=1000)[0].values
except:
  onekmrefd=None

if dx < 6.0:
  try:
    # Echo top
    etop=grbs.select(stepType='instant',parameterName="197",level=0)[0].values*3.28084 #convert meters to feet
    etop=etop/1000.  #Convert to kft
    print 'etop max and min in kft:',np.max(etop),np.min(etop)
  except:
    etop=None
else:
  print 'Do not plot echo tops for coarse grids'
  etop=None  
  
# Try reading max hourly fields if using a grid with sufficient resolution
if dx < 6.0:
  try:
    #read max hrly UH
    mxuphl=grbs.select(stepType='max',parameterName="199",typeOfLevel="heightAboveGroundLayer",topLevel=5000,bottomLevel=2000)[0].values
    # read max hrly 10m winds
    u10mx=grbs.select(stepType='max',parameterName="222",typeOfLevel="heightAboveGround",level=10)[0].values
    v10mx=grbs.select(stepType='max',parameterName="223",typeOfLevel="heightAboveGround",level=10)[0].values
    spd10mx=np.sqrt(u10mx*u10mx+v10mx*v10mx)
    # read max hrly uvvel
    mxuvvel=grbs.select(stepType='max',parameterName="220",typeOfLevel="isobaricLayer",topLevel=400,bottomLevel=1000)[0].values
    # max 1km agl ref
    mx1kmrefd=grbs.select(stepType='max',parameterName="198",typeOfLevel="heightAboveGround",level=1000)[0].values
    maxhourly=1
  except:
    maxhourly=0
    print 'Max hourlies unavailable - not plotting!' 
else:
  print 'Do not plot max hourlies for coarse grids'
  maxhourly=0  

tskin=None
if anl_diag_skip == 0:
  SBCAPE=grbs.select(name="Convective available potential energy",typeOfLevel='surface',level=0)[0].values  #SBCAPE
  SBCINH=grbs.select(name="Convective inhibition",typeOfLevel='surface',level=0)[0].values  #SBCINH
  # read 10m agl winds and convert to kts 
  ug  = grbs.select(stepType='instant',name='10 metre U wind component',typeOfLevel='heightAboveGround',level=10)[0].values
  vg  = grbs.select(stepType='instant',name='10 metre V wind component',typeOfLevel='heightAboveGround',level=10)[0].values
  u10=ncepy.ms2kts(ug)
  v10=ncepy.ms2kts(vg)
  # 2mT
  t2m = ncepy.Kelvin2F(grbs.select(stepType='instant',name='2 metre temperature',typeOfLevel='heightAboveGround',level=2)[0].values)
  # 2mTd
  td2m=ncepy.Kelvin2F(grbs.select(stepType='instant',name='2 metre dewpoint temperature',typeOfLevel='heightAboveGround',level=2)[0].values)

  # Lowest model level temperature
  # Not all grids may have this
  try:
    tlowestmodlev=ncepy.Kelvin2F(grbs.select(stepType='instant',name='Temperature',typeOfLevel='hybrid',level=1)[0].values)
  except:
    tlowestmodlev=None

  # Tskin
  tskin=ncepy.Kelvin2F(grbs.select(stepType='instant',name='Temperature',typeOfLevel='surface',level=0)[0].values) 
  # Smooth tskin for use in plotting low level Rime factor
  tskin_smth=scipy.ndimage.gaussian_filter(tskin, 2)
  tskin_passes="one pass"
  if dx < 6.0:
    # Smooth mslp using 2 passes of a Gaussian filter if this is a hi-res grid
    tskin_smth=scipy.ndimage.gaussian_filter(tskin_smth, 2)
    tskin_passes="two passes"
  #Soil Temps

  print 'trying soil'
  tsoil_0_10=ncepy.Kelvin2F(grbs.select(parameterName='Soil temperature',typeOfLevel='depthBelowLandLayer',
                            topLevel=0,bottomLevel=0,scaledValueOfFirstFixedSurface=0,
                            scaledValueOfSecondFixedSurface=10)[0].values)
#  tsoil_10_40=ncepy.Kelvin2F(grbs.select(parameterName='Soil temperature',typeOfLevel='depthBelowLandLayer',
#                            topLevel=0,bottomLevel=0,scaledValueOfFirstFixedSurface=10,
#                            scaledValueOfSecondFixedSurface=40)[0].values)
#  tsoil_40_100=ncepy.Kelvin2F(grbs.select(parameterName='Soil temperature',typeOfLevel='depthBelowLandLayer',
#                            topLevel=0,bottomLevel=1,scaledValueOfFirstFixedSurface=40,
#                            scaledValueOfSecondFixedSurface=100)[0].values)
#  tsoil_100_200=ncepy.Kelvin2F(grbs.select(parameterName='Soil temperature',typeOfLevel='depthBelowLandLayer',
#                            topLevel=1,bottomLevel=2,scaledValueOfFirstFixedSurface=100,
#                            scaledValueOfSecondFixedSurface=200)[0].values)
  # TOTAL volumetric Soil moisture (fraction)(LIQUID + SOLID)
  soilm_0_10=grbs.select(name='Volumetric soil moisture content',typeOfLevel='depthBelowLandLayer',
                         topLevel=0,bottomLevel=0,scaledValueOfFirstFixedSurface=0,
                         scaledValueOfSecondFixedSurface=10)[0].values
#  soilm_10_40=grbs.select(name='Volumetric soil moisture content',typeOfLevel='depthBelowLandLayer',
#                         topLevel=0,bottomLevel=0,scaledValueOfFirstFixedSurface=10,
#                         scaledValueOfSecondFixedSurface=40)[0].values
#  soilm_40_100=grbs.select(name='Volumetric soil moisture content',typeOfLevel='depthBelowLandLayer',
#                         topLevel=0,bottomLevel=1,scaledValueOfFirstFixedSurface=40,
#                         scaledValueOfSecondFixedSurface=100)[0].values
#  soilm_100_200=grbs.select(name='Volumetric soil moisture content',typeOfLevel='depthBelowLandLayer',
#                          topLevel=1,bottomLevel=2,scaledValueOfFirstFixedSurface=100,
#                         scaledValueOfSecondFixedSurface=200)[0].values

#  soilfz_0_10=soilm_0_10-grbs.select(name='Liquid volumetric soil moisture (non-frozen)',units='Proportion',
#                                     typeOfLevel='depthBelowLandLayer',topLevel=0,bottomLevel=0,
#                                     scaledValueOfFirstFixedSurface=0,scaledValueOfSecondFixedSurface=10)[0].values
#  soilfz_10_40=soilm_10_40-grbs.select(name='Liquid volumetric soil moisture (non-frozen)',units='Proportion',
#                                     typeOfLevel='depthBelowLandLayer',topLevel=0,bottomLevel=0,
#                                     scaledValueOfFirstFixedSurface=10,scaledValueOfSecondFixedSurface=40)[0].values
#  soilfz_40_100=soilm_40_100-grbs.select(name='Liquid volumetric soil moisture (non-frozen)',units='Proportion',
#                                     typeOfLevel='depthBelowLandLayer',topLevel=0,bottomLevel=1,
#                                     scaledValueOfFirstFixedSurface=40,scaledValueOfSecondFixedSurface=100)[0].values
#  soilfz_100_200=soilm_100_200-grbs.select(name='Liquid volumetric soil moisture (non-frozen)',units='Proportion',
#                                     typeOfLevel='depthBelowLandLayer',topLevel=1,bottomLevel=2,
#                                     scaledValueOfFirstFixedSurface=100,scaledValueOfSecondFixedSurface=200)[0].values
    
  # Instantaneous surface fluxes (latent, ground, sensible, sw up, sw down, lw up, lw down, net)
  try:
    gdhfx=grbs.select(name='Ground heat flux',stepType='instant',typeOfLevel='surface')[0].values
  except:
    print 'Unable to read gdhfx!'
    gdhfx=None

  snhfx=grbs.select(name='Sensible heat net flux',stepType='instant',typeOfLevel='surface')[0].values
  lhfx=grbs.select(name='Latent heat net flux',stepType='instant',typeOfLevel='surface')[0].values
#  swup=grbs.select(name='Upward short-wave radiation flux',stepType='instant',typeOfLevel='surface')[0].values
  swup=None
#  swdown=grbs.select(name='Downward short-wave radiation flux',stepType='instant',typeOfLevel='surface')[0].values
  swdown=None
#  lwup=grbs.select(name='Upward long-wave radiation flux',stepType='instant',typeOfLevel='surface')[0].values
  lwup=None
#  lwdown=grbs.select(name='Downward long-wave radiation flux',stepType='instant',typeOfLevel='surface')[0].values
  lwdown=None
  try:
    clearskydownswfx=grbs.select(name='Clear Sky Downward Solar Flux',stepType='instant',typeOfLevel='surface')[0].values
  except:
    print 'Unable to read clearskydownswfx!'
    clearskydownswfx=None

#   netswf=swdown-swup
#   netlwf=lwdown-lwup
#   netrad=netswf+netlwf

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

#  Total column-integrated qw
try:
  tqw=grbs.select(name='Total column-integrated cloud water',stepType='instant')[0].values
except:
  print 'Unable to read Total column-integrated cloud water'
  tqw=None

#  Total column-integrated qr
try:
  tqr=grbs.select(name='Total column integrated rain',stepType='instant')[0].values
except:
  print 'Unable to read Total column integrated rain'
  tqr=None

#  Total column-integrated qi
try:
  tqi=grbs.select(name='Total column-integrated cloud ice',stepType='instant')[0].values
except:
  print 'Unable to read Total column-integrated cloud ice'
  tqi=None

#  Total column-integrated qs
try:
  tqs=grbs.select(name='Total column integrated snow',stepType='instant')[0].values
except:
  print 'Unable to read Total column integrated snow'
  tqs=None

#  Total column-integrated cwmr
try:
  tcwmr=grbs.select(name='Total column-integrated condensate',stepType='instant')[0].values
except:
  print 'Unable to read Total column-integrated condensate'
  tcwmr=None


#  Total column-integrated lower-density snow/graupel (RF<10) and higher-density graupel/sleet/"hail" (RF>10)
try:
  rfthresh=10.
  tqsld,tqshd=qs_rimefactor_partition(grbs,nx,ny,rfthresh)
except:
  print 'Unable to read and calculate low/high density qs'
  tqsld=None
  tqshd=None

try:
  lowcloudcover=grbs.select(parameterName='Low cloud cover',stepType='instant')[0].values
except:
  print 'Unable to read Low cloud cover'
  lowcloudcover=None

try:
  midcloudcover=grbs.select(parameterName='Medium cloud cover',stepType='instant')[0].values
except:
  print 'Unable to read Medium cloud cover'
  midcloudcover=None

try:
  highcloudcover=grbs.select(parameterName='High cloud cover',stepType='instant')[0].values
except:
  print 'Unable to read High cloud cover'
  highcloudcover=None

try:
  totalcloudcover=grbs.select(name='Total Cloud Cover',stepType='instant')[0].values
except:
  print 'Unable to read Total Cloud Cover (instant)'
  totalcloudcover=None

# Winter weather params
try:
  #Despite what pygrib tells us, this is in meters
  snowdepth=grbs.select(parameterName='Snow depth',typeOfLevel='surface')[0].values 
except:
  print 'Unable to read Snow depth'
  snowdepth=None

try:
  pofp=grbs.select(name='Percent frozen precipitation')[0].values
except:
  print 'Unable to read Percent frozen precipitation'
  pofp=None

try:
  csnow=grbs.select(name='Categorical snow')[0].values
  cicep=grbs.select(name='Categorical ice pellets')[0].values
  cfzr=grbs.select(name='Categorical freezing rain')[0].values
  crain=grbs.select(name='Categorical rain')[0].values
  ptype=crain+cfzr*2+cicep*3+csnow*4
except:
  print 'Unable to read/calculate ptype'
  ptype=None

try:
  #Read lowest model level RF and mask with pofp
  RF_lev1=grbs.select(name='Rime factor',stepType='instant',typeOfLevel='hybrid',level=1)[0].values + 1e-6 #(add a tiny number to assist with plotting, otherwise it looks choppy)
  #qsnow_lev1=grbs.select(name='Snow mixing ratio',stepType='instant',typeOfLevel='hybrid',level=1)[0].values
  RF_lev1=np.where(pofp>0,RF_lev1,0.)
except:
  print 'Unable to read RF_lev1'
  RF_lev1=None

# 850 hghts + wind barbs + RH
h850=grbs.select(name='Geopotential Height',typeOfLevel='isobaricInhPa',level=850)[0].values
rh850=grbs.select(name='Relative humidity',typeOfLevel='isobaricInhPa',level=850)[0].values
ug850=grbs.select(name='U component of wind',typeOfLevel='isobaricInhPa',level=850)[0].values
vg850=grbs.select(name='V component of wind',typeOfLevel='isobaricInhPa',level=850)[0].values
# convert to kts
u850=ncepy.ms2kts(ug850)
v850=ncepy.ms2kts(vg850)  

# 500 hghts + wind barbs + vort
h500=grbs.select(name='Geopotential Height',typeOfLevel='isobaricInhPa',level=500)[0].values
ug500=grbs.select(name='U component of wind',typeOfLevel='isobaricInhPa',level=500)[0].values
vg500=grbs.select(name='V component of wind',typeOfLevel='isobaricInhPa',level=500)[0].values
avort500=grbs.select(name='Absolute vorticity',typeOfLevel='isobaricInhPa',level=500)[0].values 
#convert to kts
u500=ncepy.ms2kts(ug500)
v500=ncepy.ms2kts(vg500)    
 
# Read 1000 hPa heights for thickness lines
h1000=grbs.select(name='Geopotential Height',typeOfLevel='isobaricInhPa',level=1000)[0].values
thickness=h500-h1000
  
# 250 hgts + winds + isotachs
h250=grbs.select(name='Geopotential Height',typeOfLevel='isobaricInhPa',level=250)[0].values
ug250=grbs.select(name='U component of wind',typeOfLevel='isobaricInhPa',level=250)[0].values
vg250=grbs.select(name='V component of wind',typeOfLevel='isobaricInhPa',level=250)[0].values
#convert to kts
u250=ncepy.ms2kts(ug250)
v250=ncepy.ms2kts(vg250)  

t2a=time.clock()
t3a=round(t2a-t1a, 3)
print("%.3f seconds to read all gribs msgs!") % t3a


  ###################################################
  #       START PLOTTING FOR EACH DOMAIN            #
  ###################################################


def main():

  # Number of processes must coincide with the number of domains to plot
  pool = multiprocessing.Pool(len(domains))
  pool.map(plot_all,domains)
  
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
    m.drawcoastlines(linewidth=1.50,color='#ddaa66')
    m.drawstates(linewidth=1.00,color='#ddaa66')
    m.drawcountries(linewidth=0.75,color='#ddaa66')
    m.drawparallels(parallels,labels=[1,0,0,1])
    m.drawmeridians(meridians,labels=[1,0,0,1])


    t1=time.clock()
    print('Working on slp for '+dom) 


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
      plt.title(domid+core+' MSLP (hPa) and Wind (kts)\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
    else:
      plt.title(domid+core+' MSLP (hPa)\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
    outfile='./hiresw_slp_'+dom+'_f'+fhr+'_'+domid+core+'.png'

    #plot mslp
    clevs = np.arange(900,1100.,4.)
    cs = m.contour(lons,lats,mslp,clevs,colors='black',linewidths=1.75,latlon=True)
    clab = plt.clabel(cs, inline=1,colors='k',fontsize=8,fmt='%.0f')
    for t in clab:
      t.set_bbox(dict(boxstyle="square",fc="white"))
    if anl_diag_skip == 0:
      #Plot 10m winds
      m.barbs(lons[::skip,::skip],lats[::skip,::skip],u10[::skip,::skip],v10[::skip,::skip],latlon=True,length=5.5,sizes={'spacing':0.2},pivot='middle') 
    #Plot thickness in decameters
    clevs = np.arange(468,546,6) #cold side
    cs = m.contour(lons,lats,thickness/10.,clevs,colors='hotpink',linestyles='dashed',linewidths=2.,latlon=True)
    clab = plt.clabel(cs, inline=1,colors='purple', fontsize=8,fmt='%.0f')
    for t in clab:
      t.set_bbox(dict(boxstyle="square",fc="white"))
    clevs = np.arange(546,606,6) #warm side
    cs = m.contour(lons,lats,thickness/10.,clevs,colors='#E42217',linestyles='dashed',linewidths=2.,latlon=True)
    clab = plt.clabel(cs, inline=1, fontsize=8,fmt='%.0f')
    for t in clab:
      t.set_bbox(dict(boxstyle="square",fc="white"))
       
    # plot highs and lows - window parameter controls the number of highs and lows detected.
    if dx>6.0:
      highslowswindow=175
    else:
      highslowswindow=300
    ncepy.plt_highs_and_lows(m,mslp,lons,lats,mode='reflect',window=highslowswindow)

    msg="MSLP smoothed with "+passes+" of a Gaussian filter"
    ax.annotate(msg,xy=(0.5,0.025),xycoords='axes fraction',color='k',zorder=10,
             fontsize=10,ha='center', va='center',bbox=dict(fc="white"))

    plt.savefig(outfile,bbox_inches='tight')
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot slp for: "+dom) % t3

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
      cbar = m.colorbar(cs,location='bottom',pad="5%",ticks=clevs)
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('dBZ')
      plt.title(domid+core+' Column Max Reflectivity \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')   
      plt.savefig('./hiresw_refc_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
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

      clevs = [15,20,25,30,35,40,45,50]
      colors=['#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00']
      cs = m.contourf(lons,lats,etop,clevs,colors=colors,latlon=True,extend='max')
      cs.set_clim(15,50)
      cbar = m.colorbar(cs,location='bottom',pad="5%",ticks=clevs)
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('kft')
      plt.title(domid+core+' Echo Top \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_etop_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot etop for: "+dom) % t3


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
      cs = m.contourf(lons,lats,mx1kmrefd,clevs,cmap=ncepy.mrms_radarmap(),latlon=True,extend='max')
      cs.set_clim(5,75) 
      cbar = m.colorbar(cs,location='bottom',pad="5%",ticks=clevs)
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('dBZ')
      plt.title(domid+core+' Max Hourly 1 km AGL Reflectivity \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
     
      plt.savefig('./hiresw_mx1kmrefd_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
  
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
      colors = ['lightgray','skyblue','mediumblue','green','orchid','firebrick','orangered','DarkViolet','black']
      clevs = [10.,25.,50.,75.,100.,150.,200.,250.,300.]
      cm = c.ListedColormap(colors)
      cm.set_over('white')
      norm = c.BoundaryNorm(clevs, cm.N)
      cs = m.contourf(lons,lats,mxuphl,clevs,cmap=cm,norm=norm,latlon=True,extend='max')
      cs.set_clim(10.,300.) 
      cbar = m.colorbar(cs,location='bottom',pad="5%",ticks=clevs,format='%.0f')
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('m2/s2')
      #plot contours too
#      cs = m.contour(lons,lats,mxuphl,clevs,colors='k',linewidths=1.5,latlon=True)
#      clab = plt.clabel(cs,clevs[::3],inline=1, fontsize=10,fmt='%.0f')
      
      plt.title(domid+core+' Max Hourly Updraft Helicity \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
  
      plt.savefig('./hiresw_mxuphl_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
  
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
      clevs = np.arange(5.,75.,5.)
      cs = m.contourf(lons,lats,ncepy.ms2kts(spd10mx),clevs,cmap=plt.get_cmap(name='Paired'),latlon=True,extend='max')
      cs.set_clim(5,70) 
      cbar = m.colorbar(cs,location='bottom',pad="5%",ticks=clevs)
      cbar.set_label('kts')
      cbar.ax.tick_params(labelsize=8.5) 
      #plot contours too
#      cs = m.contour(lons,lats,spd10mx,clevs,colors='k',linewidths=1.5,latlon=True)
#      clab = plt.clabel(cs,clevs[::3],inline=1, fontsize=10,fmt='%.0f')
      
      plt.title(domid+core+' Max Hourly 10 m Wind \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
  
      plt.savefig('./hiresw_max10mw_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
  
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot spd10mx for: "+dom) % t3	   
      
      
#--------------------------------------------------------------------------------------------    
      # Get the next plot ready MAX HOURLY uvvel
      t1=time.clock()
      print('Working on mxuvvel for '+dom)   
      
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      
      clevs = [5.,10.,15.,20.,25.,30.,35.,40.,45.,50.]
      cmap=plt.get_cmap("Paired")
      cs = m.contourf(lons,lats,mxuvvel,clevs,cmap=cmap,latlon=True,extend='max')
      cs.set_clim(5.,50.)
      cbar = m.colorbar(cs,location='bottom',pad="5%",ticks=clevs,format='%.1f')
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('m/s')
      #plot contours too
#      cs = m.contour(lons,lats,mxuvvel,clevs,colors='k',linewidths=1.5,latlon=True)
#      clab = plt.clabel(cs,clevs[::3], inline=1, fontsize=10,fmt='%.0f')
      
      plt.title(domid+core+' Max Hourly Updraft Speed \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
  
      plt.savefig('./hiresw_maxvvel_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
  
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot mxuvvel for: "+dom) % t3	
#---- End of max hourly if-test
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
      cbar = m.colorbar(cs,location='bottom',pad="5%",ticks=clevs)
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('dBZ')
      plt.title(domid+core+' 1 km AGL Reflectivity \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_1kmrefd_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight') 
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot 1kmrefd for: "+dom) % t3     

#---- End of max hourly if-test


   #This block is for avoiding sfc/land fields when doing 
   #  analysis diag plotting

    if anl_diag_skip == 0:
   
#-   -------------------------------------------------------------------------------------------    
      # Get the next plot ready CAPE/CINH
      t1=time.clock()
      print('Working on cape for '+dom)   
      
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      
      clevs = np.arange(250.,5250.,250.)
      cs = m.contourf(lons,lats,SBCAPE,clevs,cmap=plt.get_cmap(name='Spectral_r'),latlon=True,extend='max')
      cs.set_clim(250.,5000.) 
      cbar = m.colorbar(cs,location='bottom',pad="5%",ticks=clevs,format='%.0f')
      cbar.ax.tick_params(labelsize=8.5) 
      cbar.set_label('J/kg')
      
      plt.title(domid+core+' SBCAPE \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
  
      plt.savefig('./hiresw_sbcape_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
  
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot cape for: "+dom) % t3   


#-   -------------------------------------------------------------------------------------------
      # Get the next plot ready CINH
      t1=time.clock()
      print('Working on cinh for '+dom)

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      clevs = np.arange(-200.0,0.,25.)
      cmap=plt.get_cmap(name='Spectral')
      #cmap.set_over('w')

# print min/max sbcinh vals
      print 'min/max of sbcinh ', np.min(SBCINH),np.max(SBCINH)
      
      cs = m.contourf(lons,lats,SBCINH,clevs,cmap=cmap,latlon=True,extend='min')
      cs.set_clim(-200.0,-25.)
      cbar = m.colorbar(cs,location='bottom',pad="5%",ticks=clevs,format='%.0f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('J/kg')
      plt.title(domid+core+' SBCINH \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_sbcinh_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot cinh for: "+dom) % t3

#-   -------------------------------------------------------------------------------------------    
      # Get the next plot ready 2mt
      t1=time.clock()
      print('Working on 2mt for '+dom)   
      
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      clevs = np.arange(-36.,104.,4) 
      #Now plot t2m
      cs1 = m.contourf(lons,lats,t2m,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
      cs1.set_clim(-36, 100.) 
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')    
      cbar.ax.tick_params(labelsize=8.5)    
      cbar.set_label('F')

      #Contour 32f line too
      cs=m.contour(lons,lats,t2m,levels=[32.],colors='black',linewidths=1.5,latlon=True)
      clab = plt.clabel(cs,levels=[32.],inline=1,colors='black', fontsize=10,fmt='%.0f F',fontweight='bold')
      for t in clab:
     	t.set_bbox(dict(boxstyle="square",fc="white"))

      #plot wind barbs
      
      m.barbs(lons[::skip,::skip],lats[::skip,::skip],u10[::skip,::skip],v10[::skip,::skip],latlon=True,length=5.5,sizes={'spacing':0.2},pivot='middle')
      
      plt.title(domid+core+' 2 m T and Wind (kts)\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_2mtw_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot 2mtw "+dom) % t3
#-   -------------------------------------------------------------------------------------------

      if tlowestmodlev is not None:
        # Get the next plot ready lowest model level T 
        t1=time.clock()
        print('Working on lowest model level T for '+dom)

        # Clear off old plottables but keep all the map info
        ncepy.clear_plotables(ax,keep_ax_lst,fig)
        clevs = np.arange(-36.,104.,4)
        #Now plot 
        cs1 = m.contourf(lons,lats,tlowestmodlev,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
        cs1.set_clim(-36, 100.)
        # add colorbar.
        cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
        cbar.ax.tick_params(labelsize=8.5)
        cbar.set_label('F')
        #Contour 32f line too
        cs=m.contour(lons,lats,tlowestmodlev,levels=[32.],colors='black',linewidths=1.5,latlon=True)
        clab = plt.clabel(cs,levels=[32.],inline=1,colors='black', fontsize=10,fmt='%.0f F',fontweight='bold')
        for t in clab:
          t.set_bbox(dict(boxstyle="square",fc="white"))

        plt.title(domid+core+' Lowest Model Level T \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
        plt.savefig('./hiresw_lowestmodlevt_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
        t2 = time.clock()
        t3=round(t2-t1, 3)
        print("%.3f seconds to plot lowest model level T "+dom) % t3

#-   -------------------------------------------------------------------------------------------
      # Get the next plot ready for skin temp 
      t1=time.clock()
      print('Working on skin temp for '+dom)

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      clevs = np.arange(-36.,104.,4)
      #Now plot t2m 
      cs1 = m.contourf(lons,lats,tskin,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
      cs1.set_clim(-36, 100.)
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('F')

      #Contour 32f line too
      cs=m.contour(lons,lats,tskin,levels=[32.],colors='black',linewidths=1.5,latlon=True)
      clab = plt.clabel(cs,levels=[32.],inline=1,colors='black', fontsize=10,fmt='%.0f F',fontweight='bold')
      for t in clab:
     	t.set_bbox(dict(boxstyle="square",fc="white"))
      plt.title(domid+core+' Skin T \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_tskin_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot do skin temp for  "+dom) % t3

#-   -------------------------------------------------------------------------------------------
      # Get the next plot ready for skin temp 
      t1=time.clock()
      print('Working on soil moisture plots '+dom)

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      clevs = np.arange(.05,0.50,0.05)
      #Now plot 
      mysoil=maskoceans(lons, lats, soilm_0_10, inlands=True, resolution=res)
      cs1 = m.contourf(lons,lats,mysoil,clevs,cmap=plt.get_cmap(name='RdYlGn'),latlon=True,extend='max')
      cs1.set_clim(.05,.45)
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('Fraction')
      plt.title(domid+core+' 0-10 cm Volumetric Soil Moisture \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_soilm0-10_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
 
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(.05,0.50,0.05)
#      #Now plot
#      mysoil=maskoceans(lons, lats, soilm_10_40, inlands=True, resolution=res)
#      cs1 = m.contourf(lons,lats,mysoil,clevs,cmap=plt.get_cmap(name='RdYlGn'),latlon=True,extend='max')
#      cs1.set_clim(.05,.45)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('Fraction')
#      plt.title(domid+core+' 10-40 cm Volumetric Soil Moisture \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_soilm10-40_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(.05,0.50,0.05)
#      #Now plot 
#      mysoil=maskoceans(lons, lats, soilm_40_100, inlands=True, resolution=res)
#      cs1 = m.contourf(lons,lats,mysoil,clevs,cmap=plt.get_cmap(name='RdYlGn'),latlon=True,extend='max')
#      cs1.set_clim(.05,.45)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('Fraction')
#      plt.title(domid+core+' 40-100 cm Volumetric Soil Moisture \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_soilm40-100_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
#
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#     clevs = np.arange(.05,0.50,0.05)
#      #Now plot
#       mysoil=maskoceans(lons, lats, soilm_100_200, inlands=True, resolution=res)  
#       cs1 = m.contourf(lons,lats,mysoil,clevs,cmap=plt.get_cmap(name='RdYlGn'),latlon=True,extend='max')
#       cs1.set_clim(.05,.45)
#       # add colorbar.
#       cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')
#       cbar.ax.tick_params(labelsize=8.5)
#       cbar.set_label('Fraction')
#       plt.title(domid+core+' 100-200 cm Volumetric Soil Moisture \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#       plt.savefig('./hiresw_soilm100-200_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')


##   ########################### ------------Frozen Volumetric Soil Moisture--------------------##################################################

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(.05,0.50,0.05)
#      #Now plot t2m 
#      cs1 = m.contourf(lons,lats,soilfz_0_10,clevs,cmap=plt.get_cmap(name='RdYlGn'),latlon=True,extend='max')
#      cs1.set_clim(.05,.45)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('Fraction')
#      plt.title(domid+core+' 0-10 cm Frozen Volumetric Soil Moisture \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_soilfz0-10_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(.05,0.50,0.05)
#      #Now plot t2m 
#      cs1 = m.contourf(lons,lats,soilfz_10_40,clevs,cmap=plt.get_cmap(name='RdYlGn'),latlon=True,extend='max')
#      cs1.set_clim(.05,.45)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('Fraction')
#      plt.title(domid+core+' 10-40 cm Frozen Volumetric Soil Moisture \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_soilfz10-40_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(.05,0.50,0.05)
#      #Now plot t2m 
#      cs1 = m.contourf(lons,lats,soilfz_40_100,clevs,cmap=plt.get_cmap(name='RdYlGn'),latlon=True,extend='max')
#      cs1.set_clim(.05,.45)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('Fraction')
#      plt.title(domid+core+' 40-100 cm Frozen Volumetric Soil Moisture \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_soilfz40-100_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(.05,0.50,0.05)
#      #Now plot t2m 
#      cs1 = m.contourf(lons,lats,soilfz_100_200,clevs,cmap=plt.get_cmap(name='RdYlGn'),latlon=True,extend='max')
#      cs1.set_clim(.05,.45)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('Fraction')
#      plt.title(domid+core+' 100-200 cm Frozen Volumetric Soil Moisture \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_soilfz100-200_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot soil moisture plots  "+dom) % t3

#-   -------------------------------------------------------------------------------------------
      # Get the next plot ready for skin temp
      t1=time.clock()
      print('Working on soil temperature plots '+dom)

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      clevs = np.arange(-36.,104.,4)
      #Now plot
      mysoilt=maskoceans(lons, lats, tsoil_0_10, inlands=True, resolution=res)
      #Reverse StepSeq
      cs1 = m.contourf(lons,lats,mysoilt,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
      cs1.set_clim(-36, 100.)
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('F')
      plt.title(domid+core+' 0-10 cm Soil Temperature \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_tsoil0-10_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      clevs = np.arange(-36.,104.,4)
#      #Now plot
#      mysoilt=maskoceans(lons, lats, tsoil_10_40, inlands=True, resolution=res)
#      cs1 = m.contourf(lons,lats,mysoilt,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
#      cs1.set_clim(-36, 100.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
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
      print("%.3f seconds to plot soil temperature plots  "+dom) % t3


#-   -------------------------------------------------------------------------------------------    
      # Get the next plot ready 2mtd
      t1=time.clock()
      print('Working on 2mtd for '+dom)   
      
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      clevs = np.arange(-36.,104.,4)
      #Now plot td2m
      cs1 = m.contourf(lons,lats,td2m,clevs,cmap=ncepy.ncl_t2m(),latlon=True,extend='both')
      cs1.set_clim(-36, 100.) 
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')    
      cbar.ax.tick_params(labelsize=8.5)    
      cbar.set_label('F')

      #plot wind barbs
      
      m.barbs(lons[::skip,::skip],lats[::skip,::skip],u10[::skip,::skip],v10[::skip,::skip],latlon=True,length=5.5,sizes={'spacing':0.2},pivot='middle')
      
      plt.title(domid+core+' 2 m Td and Wind (kts)\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_2mtdw_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot 2mtdw "+dom) % t3


#-   --------------------------------------------------------------------------------------------
#    Now plot the radiation fields

      t1=time.clock()
      print('Working on radiation fields for '+dom)

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
        cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
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
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('W/m^2')
#      plt.title(domid+core+' Surface Net Radiative Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_netrad_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

      # Set contour levels    
      clevs = [-2500.,-2000.,-1500.,-1000.,-750.,-500.,-300.,-200.,-100.,-75.,-50.,-25.0,0.,25.,50.,75.,100.,200.,300.,500.,750.,1000.,1500.,2000.,2500]
      cs1 = m.contourf(lons,lats,lhfx,clevs,cmap=ncepy.ncl_grnd_hflux(),latlon=True,extend='both')
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('W/m^2')
      plt.title(domid+core+' Latent Heat Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_lhfx_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      # Set contour levels    
#      clevs = [-2500.,-2000.,-1500.,-1000.,-750.,-500.,-300.,-200.,-100.,-75.,-50.,-25.0,0.,25.,50.,75.,100.,200.,300.,500.,750.,1000.,1500.,2000.,2500]
#      cs1 = m.contourf(lons,lats,snhfx,clevs,cmap=ncepy.ncl_grnd_hflux(),latlon=True,extend='both')
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
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
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
#      cbar.ax.tick_params(labelsize=8.5)
#      cbar.set_label('W/m^2')
#      for label in cbar.ax.xaxis.get_ticklabels()[::2]:  #Only show every other ticklabel on the colorbar
#     	label.set_visible(False)
#      plt.title(domid+core+' Surface Downward Shortwave Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
#      plt.savefig('./hiresw_swdown_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

      if clearskydownswfx is not None:
        # Clear off old plottables but keep all the map info
        ncepy.clear_plotables(ax,keep_ax_lst,fig)
  
        # Set contour levels    
        clevs = np.arange(0,1025,25)
        #Now plot
        cs1 = m.contourf(lons,lats,clearskydownswfx,clevs,cmap=plt.get_cmap(name='Spectral_r'),latlon=True,extend='max')
        #cs1.cmap.set_under('white')
        cs1.set_clim(0,1000.)
        # add colorbar.
        cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
        cbar.ax.tick_params(labelsize=8.5)
        cbar.set_label('W/m^2')
        for label in cbar.ax.xaxis.get_ticklabels()[::2]:  #Only show every other ticklabel on the colorbar
          label.set_visible(False)
        plt.title(domid+core+' Clear Sky Surface Downward Shortwave Flux \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
        plt.savefig('./hiresw_clearskydownswfx_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')

  
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)

#      # Set contour levels    
#      clevs = np.arange(0,525,25) 
#      #Now plot
#      cs1 = m.contourf(lons,lats,swup,clevs,cmap=plt.get_cmap(name='Spectral_r'),latlon=True,extend='max')
#      #cs1.cmap.set_under('white')
#      cs1.set_clim(0,500.)
#      # add colorbar.
#      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
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


#    ------------DONE WITH RADIATION FIELDS ----------------------------------------------

  #End of analysis diag skip

    if tqw is not None:
      t1=time.clock()
      print('Working on tqw for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels    
      clevs = q_cint
      cs1 = m.contourf(lons,lats,tqw,clevs,cmap=q_color_list_cmap,norm=q_color_list_norm,latlon=True,extend='max')
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.3f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('mm')
      plt.title(domid+core+' Total Column Integrated Cloud Water \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_tqw_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot tqw "+dom) % t3

    if tqr is not None:
      t1=time.clock()
      print('Working on tqr for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels    
      clevs = q_cint
      cs1 = m.contourf(lons,lats,tqr,clevs,cmap=q_color_list_cmap,norm=q_color_list_norm,latlon=True,extend='max')
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.3f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('mm')
      plt.title(domid+core+' Total Column Integrated Rain Water \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_tqr_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot tqr "+dom) % t3
   
    if tqi is not None:
      t1=time.clock()
      print('Working on tqi for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels    
      clevs = q_cint
      cs1 = m.contourf(lons,lats,tqi,clevs,cmap=q_color_list_cmap,norm=q_color_list_norm,latlon=True,extend='max')
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.3f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('mm')
      plt.title(domid+core+' Total Column Integrated Cloud Ice \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_tqi_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot tqi "+dom) % t3

    if tqs is not None:
      t1=time.clock()
      print('Working on tqs for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels    
      clevs = q_cint
      cs1 = m.contourf(lons,lats,tqs,clevs,cmap=q_color_list_cmap,norm=q_color_list_norm,latlon=True,extend='max')
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.3f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('mm')
      plt.title(domid+core+' Total Column Integrated Snow/Graupel \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_tqs_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot tqs "+dom) % t3

    if tcwmr is not None:
      t1=time.clock()
      print('Working on tcwmr for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels    
      clevs = q_cint
      cs1 = m.contourf(lons,lats,tcwmr,clevs,cmap=q_color_list_cmap,norm=q_color_list_norm,latlon=True,extend='max')
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.3f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('mm')
      plt.title(domid+core+' Total Column Integrated Condensate \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_tcwmr_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot tcwmr "+dom) % t3

    if tqsld is not None:
      t1=time.clock()
      print('Working on tqsld for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels    
      clevs = q_cint
      cs1 = m.contourf(lons,lats,tqsld,clevs,cmap=q_color_list_cmap,norm=q_color_list_norm,latlon=True,extend='max')
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.3f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('mm')
      plt.title(domid+core+' Total Column Integrated Low Density Snow/Graupel \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      msg="Rime Factor < "+str(rfthresh)
      ax.annotate(msg,xy=(0.5,0.025),xycoords='axes fraction',color='k',zorder=10,
             fontsize=10,ha='center', va='center',bbox=dict(fc="white"))
      plt.savefig('./hiresw_tqsld_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot tqsld "+dom) % t3

    if tqshd is not None:
      t1=time.clock()
      print('Working on tqshd for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels    
      clevs = q_cint
      cs1 = m.contourf(lons,lats,tqshd,clevs,cmap=q_color_list_cmap,norm=q_color_list_norm,latlon=True,extend='max')
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.3f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('mm')
      plt.title(domid+core+' Total Column Integrated High Density Graupel/Sleet/"Hail" \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      msg="Rime Factor >= "+str(rfthresh)
      ax.annotate(msg,xy=(0.5,0.025),xycoords='axes fraction',color='k',zorder=10,
             fontsize=10,ha='center', va='center',bbox=dict(fc="white"))
      plt.savefig('./hiresw_tqshd_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot tqshd "+dom) % t3

#---------------------------------------------------------------------------------------------
#                Cloud Cover


    if lowcloudcover is not None and midcloudcover is not None and highcloudcover is not None:
      cmaphigh=plt.get_cmap(name='Blues')
      cmapmid=plt.get_cmap(name='Greens')
      cmaplow=plt.get_cmap(name='Reds')
      t1=time.clock()
      print('Working on low_mid_high_clouds for '+dom)  
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels    
      clevs = np.arange(10.,105.,5.)
      cs1 = m.contourf(lons,lats,highcloudcover,clevs,cmap=cmaphigh,latlon=True)
      cbar1 = m.colorbar(cs1,location='bottom',pad="-6%",size="2.5%",ticks=clevs,format='%.0f') #,shrink=0.5)
      cbar1.set_label('High Cloud (%)',size='small')
      plt.setp(cbar1.ax.get_xticklabels(), visible=False)
      cs2 = m.contourf(lons,lats,midcloudcover,clevs,cmap=cmapmid,latlon=True)
      cbar2 = m.colorbar(cs2,location='bottom',pad="7%",size="2.5%",ticks=clevs,format='%.0f') #,shrink=0.5)
      cbar2.set_label('Mid Cloud (%)',size='small')
      plt.setp(cbar2.ax.get_xticklabels(), visible=False)  
      cs3 = m.contourf(lons,lats,lowcloudcover,clevs,cmap=cmaplow,latlon=True)
      cbar3 = m.colorbar(cs3,location='bottom',pad="20%",size="2.5%",ticks=clevs,format='%.0f') #,shrink=0.5)
      cbar3.set_label('Low Cloud (%)',size='small')
      cbar3.ax.tick_params(labelsize=10)
      plt.title(domid+core+' Low, Middle, and High Clouds\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_low_mid_high_clouds_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      try:
        cbar3.remove()
        cbar2.remove()
        cbar1.remove()
      except:
        print 'Unable to use colorbar class to remove cloud colorbars - hoping clear_plottables will take care of it'
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot low_mid_high_clouds "+dom) % t3     


    if totalcloudcover is not None:
      t1=time.clock()
      print('Working on totalcloudcover for '+dom)  
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels    
      clevs = np.arange(10.,105.,5.)
      try:
        cmap=plt.get_cmap(name='viridis')
      except:
        cmap=plt.get_cmap(name='gray')
      cs1 = m.contourf(lons,lats,totalcloudcover,clevs,cmap=cmap,latlon=True)
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
      cbar.set_label('%') 
      plt.title(domid+core+' Total Cloud Cover\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_totalcloudcover_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot totalcloudcover "+dom) % t3


#------Winter Weather Plots--------------------------------------------------------
    if snowdepth is not None:
      # Get the next plot ready (snowdepth)	
      t1=time.clock()
      print('Working on snowdepth for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      # Set contour levels for snowdepth in inches    
      clevs = [0.1,1,2,4,6,9,12,18,24,36,48]
      cs1 = m.contourf(lons,lats,snowdepth*39.3701,clevs,cmap=ncepy.ncl_perc_11Lev(),latlon=True,extend='max')
      cs1.set_clim(0.1,48.)
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.1f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('Inches')
      plt.title(domid+core+' Snow Depth \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_snowdepth_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot snowdepth "+dom) % t3


    if pofp is not None:
      t1=time.clock()
      print('Working on Percent of Frozen Precip. for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      clevs=[20.,30.,40.,50.,60.,70.,80.,90.,100.]
      colors=['#fff7fb','#ece2f0','#d0d1e6','#a6bddb','#67a9cf','#3690c0','#02818a','#016c59','#014636']
      cm = c.ListedColormap(colors)
      norm = c.BoundaryNorm(clevs, cm.N)
      cs1 = m.contourf(lons,lats,pofp,clevs,cmap=cm,norm=norm,latlon=True)
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
      cbar.set_label('%')    
      plt.title(domid+core+' Percent of Frozen Precip.\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_pofp_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot Percent of Frozen Precip. "+dom) % t3

    if ptype is not None:
      t1=time.clock()
      print('Working on ptype for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      clevs=[1,2,3,4] 
      cm = c.ListedColormap(['green','red','magenta','blue'])
      cm.set_under(alpha = 0.0)
      norm = c.BoundaryNorm(clevs, cm.N)
      cs1 = m.pcolormesh(lons,lats,ptype,cmap=cm,vmin=1,vmax=4,norm=norm,latlon=True)
      fc='#f0f0f0'
      ax.annotate('Rain',xy=(0.15,0.025),xycoords='axes fraction',color='green', fontsize=11,weight='bold',
              ha='center', va='center',bbox=dict(fc=fc),zorder=10)
      ax.annotate('ZR',xy=(0.40,0.025),xycoords='axes fraction', color='red', fontsize=11,weight='bold',
              ha='center', va='center',bbox=dict(fc=fc),zorder=10)
      ax.annotate('IP',xy=(0.65,0.025),xycoords='axes fraction', color='magenta', fontsize=11,weight='bold',
              ha='center', va='center',bbox=dict(fc=fc),zorder=10)
      ax.annotate('Snow',xy=(0.90,0.025),xycoords='axes fraction', color='blue', fontsize=11,weight='bold',
              ha='center', va='center',bbox=dict(fc=fc),zorder=10)
      plt.title(domid+core+' Precipitation Type\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
      plt.savefig('./hiresw_ptype_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot ptype "+dom) % t3

    if RF_lev1 is not None and tskin is not None:
      t1=time.clock()
      print('Working on RF_lev1 for '+dom)
      # Clear off old plottables but keep all the map info
      ncepy.clear_plotables(ax,keep_ax_lst,fig)
      clevs = [1,2,5,20,45,50]
      colors = ['skyblue','mediumblue','green','orchid','firebrick','DarkViolet','black']
      cm = c.ListedColormap(colors)
      norm = c.BoundaryNorm(clevs, cm.N)
      cs1 = m.contourf(lons,lats,RF_lev1,clevs,cmap=cm,norm=norm,latlon=True)
      # add colorbar.
      cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')
      cbar.ax.tick_params(labelsize=8.5)
      cbar.set_label('Rime Factor (dimensionless)')
      #Contour 32f line too if we are not plotting CONUS or some other big domain
      if dom != 'CONUS' and dom != 'AK':
        plt.title(domid+core+' Lowest Model Level Rime Factor and 32F Skin Temp. Line \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
        cs=m.contour(lons,lats,tskin_smth,levels=[32.],colors='black',linewidths=1.5,latlon=True)
        clab = plt.clabel(cs,levels=[32.],inline=1,colors='black', fontsize=8,fmt='%.0f F',fontweight='bold')
        for t in clab:
       	  t.set_bbox(dict(boxstyle="square",fc="white",alpha=0.5))
        msg="Skin T. smoothed with "+tskin_passes+" of a Gaussian filter.\n RF shown where percent of frozen precip. >0."
        ax.annotate(msg,xy=(0.5,0.035),xycoords='axes fraction',color='k',zorder=10,
             fontsize=10,ha='center', va='center',bbox=dict(fc="white"))
      else:
        plt.title(domid+core+' Lowest Model Level Rime Factor \n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
        msg='RF shown where percent of frozen precip. >0.'
        ax.annotate(msg,xy=(0.5,0.025),xycoords='axes fraction',color='k',zorder=10,
               fontsize=10,ha='center', va='center',bbox=dict(fc="white"))
      plt.savefig('./hiresw_lev1RF_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
      t2 = time.clock()
      t3=round(t2-t1, 3)
      print("%.3f seconds to plot RF_lev1 "+dom) % t3


#------------Upper Air--------------------------------------------------------    
    # Get the next plot ready 850
    t1=time.clock()
    print('Working on 850 for '+dom)   
    
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)

    clevs = np.arange(50.,110.,10) 
    #Now plot 850
    cmap = plt.get_cmap(name='BuGn')
    new_cmap = ncepy.truncate_colormap(cmap, 0, 0.85)
    cs1 = m.contourf(lons,lats,rh850,clevs,cmap=new_cmap,latlon=True)
    cs1.set_clim(50.,100.) 
    # add colorbar.
    cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')    
    cbar.ax.tick_params(labelsize=8.5)    
    cbar.set_label('%')

    #plot hght contours
    clevs=np.arange(1300.,1830.,30)
    cs = m.contour(lons,lats,h850,clevs,colors='#DA180D',linewidths=1.5,latlon=True)
    clab = plt.clabel(cs,clevs,inline=1, fontsize=10,fmt='%.0f')    
    for t in clab:
      t.set_bbox(dict(boxstyle="square",fc="white"))

    #plot wind barbs    
    m.barbs(lons[::skip,::skip],lats[::skip,::skip],u850[::skip,::skip],v850[::skip,::skip],latlon=True,length=5.5,sizes={'spacing':0.2},pivot='middle')
    
    plt.title(domid+core+' 850  Hgts (m), RH, and Wind (kts)\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
    plt.savefig('./hiresw_850_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot 850 "+dom) % t3

#---------------------------------------------------------------------------------------------    
    # Get the next plot ready 500
    t1=time.clock()
    print('Working on 500 for '+dom)   
    
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)

    clevs = [16.,20.,24.,28.,32.,36.,40.,42.]
    #Now plot 500
    cmap = plt.get_cmap(name='YlOrBr')
    new_cmap = ncepy.truncate_colormap(cmap, 0, 0.75)
    #new_cmap.set_under('w')
    cs1 = m.contourf(lons,lats,avort500*1.e5,clevs,cmap=new_cmap,latlon=True,extend='max')
    cs1.set_clim(16.,42.) 
    # add colorbar.
    cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')    
    cbar.ax.tick_params(labelsize=8.5)    
    cbar.set_label('s-1 (scaled by 1e5)')

    #plot hght contours
    clevs=np.arange(4980.,6560.,60) #includes the 540 dm line
    cs = m.contour(lons,lats,h500,clevs,colors='r',linewidths=1.5,latlon=True)
    clab = plt.clabel(cs,clevs,colors='k',inline=1, fontsize=10,fmt='%.0f')    
    for t in clab:
      t.set_bbox(dict(boxstyle="square",fc="white"))

    #plot wind barbs    
    m.barbs(lons[::skip,::skip],lats[::skip,::skip],u500[::skip,::skip],v500[::skip,::skip],latlon=True,length=5.5,sizes={'spacing':0.2},pivot='middle')
    
    plt.title(domid+core+' 500  Hgts (m), Abs Vort., and Wind (kts)\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
    plt.savefig('./hiresw_500_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot 500 "+dom) % t3

#---------------------------------------------------------------------------------------------    
    # Get the next plot ready 250
    t1=time.clock()
    print('Working on 250 for '+dom)   
    
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)

    clevs = np.arange(50.,175.,25.)
    spd250=np.sqrt(u250*u250+v250*v250)
    #Now plot 250
    cmap = plt.get_cmap(name='Dark2')
    new_cmap = ncepy.truncate_colormap(cmap, 0, 0.45)
    #new_cmap.set_under('w')
    cs1 = m.contourf(lons,lats,spd250,clevs,cmap=new_cmap,latlon=True,extend='max')
    cs1.set_clim(50.,150.) 
    # add colorbar.
    cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.0f')    
    cbar.ax.tick_params(labelsize=8.5)    
    cbar.set_label('kts')

    #plot hght contours
    clevs=np.arange(9000.,12120.,120)
    cs = m.contour(lons,lats,h250,clevs,colors='black',linewidths=1.5,latlon=True)
    clab = plt.clabel(cs,clevs,colors='k',inline=1, fontsize=10,fmt='%.0f')    
    for t in clab:
      t.set_bbox(dict(boxstyle="square",fc="white"))

    #plot wind barbs    
    m.barbs(lons[::skip,::skip],lats[::skip,::skip],u250[::skip,::skip],v250[::skip,::skip],latlon=True,length=5.5,sizes={'spacing':0.2},pivot='middle')
    
    plt.title(domid+core+' 250  Hgts (m) and Winds (kts)\n'+repr(date)+' '+grbtime+'Z cycle Fhr '+fhr+' Valid '+valpdy+' '+valcyc+'00Z')
    plt.savefig('./hiresw_250_'+dom+'_f'+fhr+'_'+domid+core+'.png',bbox_inches='tight')
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot 250 "+dom) % t3
      
    t3dom=round(t2-t1dom, 3)
    print("%.3f seconds to plot ALL for: "+dom) % t3dom
    plt.clf()


#############################################
  
main()  

