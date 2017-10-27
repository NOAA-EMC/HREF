# Plots hourly QPF

import pygrib
import ncepy
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, cm
import time
import sys,os

if __name__ == '__main__':

  #Necessary to generate figs when not running an Xserver (e.g. via PBS)
  plt.switch_backend('agg')

  t1a = time.clock()
  print("Starting...")
  
  # Read forecast grib file and dom id from command line
  gribfile=sys.argv[1]
  domid=sys.argv[2]
  #make domid upper case
  domid=domid.upper()
  try:
    bucket_length=int(sys.argv[3])
  except:
    bucket_length=1

  grbs=pygrib.open(gribfile)
  precip = grbs.select(name='Total Precipitation',lengthOfTimeRange=bucket_length)[0] # QPF is stored as mm
  weasdmsg = grbs.select(name='Water equivalent of accumulated snow depth',lengthOfTimeRange=bucket_length)[0] # mm
  # Get the lats and lons
  lats, lons = precip.latlons()
    
  # Get grid projection info.  Use same projection for plotting - obviates need to do
  #  any wind vector rotation

  gribproj=precip['gridType']
  if gribproj.lower()=='lambert':
    # Everything below was lifted directly from pygrib.pyx
    Lon0=precip['LoVInDegrees']
    Lat0=precip['LaDInDegrees']
    Lat1=precip['Latin1InDegrees']
    Lat2=precip['Latin2InDegrees']
  elif gribproj.lower()=='polar_stereographic':
    # Everything below was lifted directly from pygrib.pyx
    Lon0=precip['orientationOfTheGridInDegrees']
    Lat_ts=precip['latitudeWhereDxAndDyAreSpecifiedInDegrees']
    if precip.has_key('projectionCentreFlag'):
      projcenterflag = precip['projectionCentreFlag']
    elif precip.has_key('projectionCenterFlag'):
      projcenterflag = precip['projectionCenterFlag']
    if projcenterflag == 0:
      Lat0=90.
    else:
      Lat0=-90.
  elif gribproj.lower()=='mercator':
    scale = float(precip['grib2divider'])
    lon1 = precip['longitudeOfFirstGridPoint']/scale
    if precip['truncateDegrees']:
      lon1 = int(lon1)
    lon2 = precip['longitudeOfLastGridPoint']/scale
    if precip['truncateDegrees']:
      lon2 = int(lon2)
    Lat_ts=precip['LaD']/scale
    Lon0=0.5*(lon1+lon2)


  print 'MY PROJECTION IS: ',gribproj

  rearth=precip['radius']

  try:
    dx=precip['DxInMetres']/1000.
  except:
    dx=precip['DiInMetres']/1000.

  try:
    nx=precip['Nx']
  except:
    nx=precip['Ni']

  try:
    ny=precip['Ny']
  except:
    ny=precip['Nj']


#Make sure lons are west oriented
  if Lon0 > 0:
    Lon0=Lon0-360
  lons=np.where(lons>0.,lons-360.,lons)
    
#Get the date/time and forecast hour
  frange_end=precip['stepRange'] # Forecast hour  #since this is an accumulation we don't have forecast hour but a forecast range
  dummy1,sep,dummy2=frange_end.partition('-')  #for 4-5 hour accumulation we want the 5 since that's the length of the forecast
                                               # we already know this is 1hr qpf anyway and this grib file has just a single field.   
  fhr=dummy2
  fhr=str(fhr).zfill(2)
  cyctime=precip.dataTime #Cycle (e.g. 1200 UTC)
  
  #Pad with a zero and convert to a string
  grbtime=str(cyctime).zfill(4)
  
  date=precip.dataDate    #PDY
  # Now we need to figure out the forecast valid date/time
  idate=str(date)+grbtime    #CDATE 
  vdate=ncepy.ndate(idate,int(fhr))
  valpdy=vdate[0:8]
  valcyc=vdate[8:11]

# Specify some plotting domains which have the regions pre-set in ncepy
  if 'ALASKANEST' in domid or 'ALASKA' in domid:
    domains=['AK','NAK','SAK','SWAK','SEAK']
  else:
    domains=['CONUS','NW','NC','NE','SW','SC','SE','Great_Lakes','MIDATL']
  
  # Now get the values from the precip weads grib messages
  precip_vals = precip.values/25.4
  weasd_vals = ((weasdmsg.values)/25.4)*10.

  t2a=time.clock()
  t3a=round(t2a-t1a, 3)
  print(repr(t3a)+" seconds to read all gribs msgs!")

  ###################################################
  #       START PLOTTING FOR EACH DOMAIN            #
  ###################################################

  #Use gempak color table for precipitation    
  gemlist=ncepy.gem_color_list()
  # Use gempak fline color levels from pcp verif page
  pcplist=[23,22,21,20,19,10,17,16,15,14,29,28,24,25]
  #Extract these colors to a new list for plotting
  pcolors=[gemlist[i] for i in pcplist]



  for dom in domains:
    
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
    m.drawmapboundary(fill_color='#7777ff')
    m.fillcontinents(color='#ddaa66', lake_color='#7777ff', zorder = 0)
    m.drawcoastlines(linewidth=1.25)
    m.drawstates(linewidth=1.25)
    m.drawcountries(linewidth=1.25)
    m.drawparallels(parallels,labels=[1,0,0,1])
    m.drawmeridians(meridians,labels=[1,0,0,1])
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
   
    #  Map/figure has been set up here (bulk of the work), save axes instances for
    #     use again later   
    keep_ax_lst = ax.get_children()[:]

    t1=time.clock()
    print('Working on precip for '+dom) 
    # Set contour levels for precip    
    #  clevs = [0,0.1,2,5,10,15,20,25,35,50,75,100,125,150,175]  #mm
    clevs =[0.01,0.05,0.1,0.25,0.5,0.75,1.,1.5,2.,3.,4.,5.,6.,7.] #inches
    #Now plot the precip
    cs1 = m.contourf(lons,lats,precip_vals,clevs,latlon=True,colors=pcolors,extend='max')
    # add colorbar.
    cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')    
    cbar.ax.tick_params(labelsize=8.5)    
    cbar.set_label('inches')
    plt.title(domid+' '+str(bucket_length).zfill(2)+' Hr QPF at F'+fhr+' \n'+repr(date)+' '+grbtime+'Z cycle Valid '+valpdy+' '+valcyc+'00Z')
    outfile='./namrr_'+str(bucket_length)+'hrpcp_'+dom+'_f'+fhr+'_'+domid+'.png'
    plt.savefig(outfile,bbox_inches='tight')
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print(repr(t3)+" seconds to plot precip: "+dom)

    t1=time.clock()
    print('Working on accum snow for '+dom)
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    # Set contour levels for snow in inches and use 10:1 ratio.
    clevs = [0.1,1,2,4,6,9,12,18,24,36,48]
    cs1 = m.contourf(lons,lats,(weasd_vals/25.4)*10.,clevs,cmap=ncepy.ncl_perc_11Lev(),latlon=True,extend='max')
    cs1.set_clim(0.1,48.)
    # add colorbar.
    cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.1f')
    cbar.ax.tick_params(labelsize=8.5)
    cbar.set_label('Inches')
    plt.title(domid+' '+str(bucket_length).zfill(2)+' Hr Accum. Snowfall (10:1) at F'+fhr+' \n'+repr(date)+' '+grbtime+'Z cycle Valid '+valpdy+' '+valcyc+'00Z')
    outfile='./namrr_'+str(bucket_length)+'hraccumsnow_'+dom+'_f'+fhr+'_'+domid+'.png'
    plt.savefig(outfile,bbox_inches='tight')
    t2 = time.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot accum snow "+dom) % t3

    plt.clf()
  
  
  t3all=round(t2-t1a,3)
  print(repr(t3all)+" seconds to run everything!")

  
  
