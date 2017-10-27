
# Reads a number of hourly qpf fields and plots them.
#  Number of grib fields is provided by the user in the input
#  config file.  Note that all dates/times are figured out
#  internally within the pyhton script

#  This python script assumes that the first file is the first fhr in the period
#    and the last file is the last fhr in the period.  The grib files maintain the 
#    cycle date throughout.
#  The handling of the forecast hours can be made more robust in the future, but
#    this is fine for now.


import pygrib
import ncepy
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, cm
import time as mytime
import sys,os
import ConfigParser
import numpy as np

if __name__ == '__main__':

  #Necessary to generate figs when not running an Xserver (e.g. via PBS)
  plt.switch_backend('agg')
  print('Starting...')
  if len(sys.argv)!=2:
    sys.exit('Not enough arguments! Usage:python %s %s' %(sys.argv[0],'full_path_to_config_file'))

  '''  
  config.in example:
   
                                nhours (int) \n\
  				domid \n \
  				full/path/to/input/file1 \n \
  				full/path/to/input/file2 \n \
				full/path/to/input/filenhrs \n \
				
  print(('Usage:python %s) % sys.argv[0])
  
  '''
  config=ConfigParser.ConfigParser()  
  config.read(sys.argv[1])

  t1a = mytime.clock()
   
  nhours=int(config.get("precip", "nhours"))
  domid=config.get("precip", "domid")
  #make domid upper case
  domid=domid.upper()
  bucket_length=int(config.get("precip", "bucket_length"))
  f=0
  for x in np.arange(nhours/bucket_length):
    f=f+bucket_length  
    gribfile=config.get("precip", "gb"+repr(f))
    print("Reading file "+gribfile)
    grbs=pygrib.open(gribfile)
    pcpmsg = grbs.select(name='Total Precipitation',lengthOfTimeRange=bucket_length)[0]
    # Accumulated snowfall in water equivalent
    weasdmsg = grbs.select(name='Water equivalent of accumulated snow depth',lengthOfTimeRange=bucket_length)[0]
    pcptmp = pcpmsg.values #stored as mm
    weasdtmp = weasdmsg.values
    if x==0: #create empty array to start summing and grab first fhr value
      totpcp=np.zeros(np.shape(pcptmp))
      totweasd=np.zeros(np.shape(weasdtmp))
      # Since we assume the bucket lengths for precip and weasd are the same
      #  then the frange stuff must be common as well.
      frange_start=pcpmsg['stepRange'] # Forecast hour range for qpf
      dummy1,sep,dummy2=frange_start.partition('-')
      fhr_start=int(dummy1)
    #Add the precip      
    totpcp=totpcp+pcptmp
    #Add weasd (water equiv accum. snow)
    totweasd=totweasd+weasdtmp


    # The grib file metadata for project/coords ought to be the same for
    #  weasd and precip
    if f==nhours:
      #If we are at the end of the loop extract meta data
      # Get the lats and lons
      lats, lons = pcpmsg.latlons()
      # Get grid projection info.
      gribproj=pcpmsg['gridType']
      if gribproj.lower()=='lambert':
        # Everything below was lifted directly from pygrib.pyx
        Lon0=pcpmsg['LoVInDegrees']
        Lat0=pcpmsg['LaDInDegrees']
        Lat1=pcpmsg['Latin1InDegrees']
        Lat2=pcpmsg['Latin2InDegrees']
      elif gribproj.lower()=='polar_stereographic':
        # Everything below was lifted directly from pygrib.pyx
        Lon0=pcpmsg['orientationOfTheGridInDegrees']
        Lat_ts=pcpmsg['latitudeWhereDxAndDyAreSpecifiedInDegrees']
        if pcpmsg.has_key('projectionCentreFlag'):
          projcenterflag = pcpmsg['projectionCentreFlag']
        elif pcpmsg.has_key('projectionCenterFlag'):
          projcenterflag = pcpmsg['projectionCenterFlag']
        if projcenterflag == 0:
          Lat0=90.
        else:
          Lat0=-90.
      elif gribproj.lower()=='mercator':
        scale = float(pcpmsg['grib2divider'])
        lon1 = pcpmsg['longitudeOfFirstGridPoint']/scale
        if pcpmsg['truncateDegrees']:
          lon1 = int(lon1)
        lon2 = pcpmsg['longitudeOfLastGridPoint']/scale
        if pcpmsg['truncateDegrees']:
          lon2 = int(lon2)
        Lat_ts=pcpmsg['LaD']/scale
        Lon0=0.5*(lon1+lon2)
       
      print 'MY PROJECTION IS: ',gribproj    
      rearth=pcpmsg['radius']    
      try:
        dx=pcpmsg['DxInMetres']/1000.
      except:
        dx=pcpmsg['DiInMetres']/1000.    
      try:
        nx=pcpmsg['Nx']
      except:
        nx=pcpmsg['Ni']    
      try:
        ny=pcpmsg['Ny']
      except:
        ny=pcpmsg['Nj']
      #Make sure lons are west oriented
      if Lon0 > 0:
        Lon0=Lon0-360
      lons=np.where(lons>0.,lons-360.,lons)
      #Get the date/time and forecast hour
      frange_end=pcpmsg['stepRange'] # Forecast hour
      dummy1,sep,dummy2=frange_end.partition('-')
      fhr_end=int(dummy2)
      cyctime=pcpmsg.dataTime #Cycle (e.g. 1200 UTC)
      #Pad with a zero and convert to a string
      grbtime=str(cyctime).zfill(4)
      sPDY=pcpmsg.dataDate
      idate=str(sPDY)+grbtime    #CDATE    

  # Now we need to figure out the forecast valid date/time

  vdate=ncepy.ndate(idate,fhr_end)
  valpdy=vdate[0:8]  #different from NDATE routine - works
  valcyc=vdate[8:11] #different from NDATE routine - works

# Specify some plotting domains which have the regions pre-set in ncepy
  if 'ALASKANEST' in domid or 'ALASKA' in domid:
    domains=['AK','NAK','SAK','SWAK','SEAK']
  else:
    domains=['CONUS','NW','NC','NE','SW','SC','SE','Great_Lakes','MIDATL']

  t2a=mytime.clock()
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
    
    t1dom=mytime.clock()
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

    start_fhr=str(fhr_start).zfill(2)
    end_fhr=str(fhr_end).zfill(2)

    #Now plot the precip (and convert from mm to inches)
    # Set contour levels for precip
    #  clevs = [0,0.1,2,5,10,15,20,25,35,50,75,100,125,150,175]  #mm
    clevs =[0.01,0.05,0.1,0.25,0.5,0.75,1.,1.5,2.,3.,4.,5.,6.,7.] #inches
    cs1 = m.contourf(lons,lats,totpcp/25.4,clevs,latlon=True,colors=pcolors,extend='max')
    # add colorbar.
    cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.2f')    
    cbar.ax.tick_params(labelsize=8.5)    
    cbar.set_label('inches')    
    plt.title(domid+' '+start_fhr+'-'+end_fhr+' Hr QPF\n'+repr(sPDY)+' '+grbtime+'Z cycle Valid '+valpdy+' '+valcyc+'00Z')
    outfile='./namrr_'+dom+'_'+start_fhr+'-'+end_fhr+'pcp_'+domid+'.png'
    plt.savefig(outfile,bbox_inches='tight')
    t2 = mytime.clock()
    t3=round(t2-t1dom, 3)
    print(repr(t3)+" seconds to plot precip: "+dom)

    t1=mytime.clock()
    print('Working on accum snow for '+dom)
    # Clear off old plottables but keep all the map info
    ncepy.clear_plotables(ax,keep_ax_lst,fig)
    # Set contour levels for snow in inches and use 10:1 ratio.
    clevs = [0.1,1,2,4,6,9,12,18,24,36,48]
    cs1 = m.contourf(lons,lats,(totweasd/25.4)*10.,clevs,cmap=ncepy.ncl_perc_11Lev(),latlon=True,extend='max')
    cs1.set_clim(0.1,48.)
    # add colorbar.
    cbar = m.colorbar(cs1,location='bottom',pad="5%",ticks=clevs,format='%.1f')
    cbar.ax.tick_params(labelsize=8.5)
    cbar.set_label('Inches')
    plt.title(domid+' '+start_fhr+'-'+end_fhr+' Hr Accum. Snowfall (10:1)\n'+repr(sPDY)+' '+grbtime+'Z cycle Valid '+valpdy+' '+valcyc+'00Z')
    outfile='./namrr_'+dom+'_'+start_fhr+'-'+end_fhr+'accumsnow_'+domid+'.png'
    plt.savefig(outfile,bbox_inches='tight')
    t2 = mytime.clock()
    t3=round(t2-t1, 3)
    print("%.3f seconds to plot accum snow "+dom) % t3

    plt.clf()
  
  
  t3all=round(t2-t1a,3)
  print(repr(t3all)+" seconds to run everything!")

  
  
