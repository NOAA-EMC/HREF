import pygrib
import csv
import datetime
import ncepy
import numpy as np
#matplotlib.use('Agg')
import matplotlib
import math
import subprocess
matplotlib.use('Agg')
import matplotlib.patches as patches
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, shiftgrid, addcyclic
from scipy import interpolate
import sys

ymdh = str(sys.argv[1])

def find_nearest(array,value):
  idx=(np.abs(array-value)).argmin()
  return idx
members=['c00','p01','p02','p03','p04','p05','p06','p07','p08','p09','p10','p11','p12','p13','p14','p15','p16','p17','p18','p19','p20']
type=['rain','snow','freezing rain','ice pellets']
membertype=['time','date','rain','snow','freezing rain','ice pellets']
closest=3  #starting range of forecast hour
furthest=123 #12 hours more than the actual ending forecast hour you want
#furthest=12 #12 hours more than the actual ending forecast hour you want
ymd=ymdh[0:8]
year=int(ymdh[0:4])
month=int(ymdh[4:6])
day=int(ymdh[6:8])
hour=int(ymdh[8:10])
print year, month , day, hour
dtime=datetime.datetime(year,month,day,hour,0)
date_list = [dtime + datetime.timedelta(hours=x) for x in xrange(closest,furthest,3)]
lastcycle=dtime - datetime.timedelta(hours=6)
lastymd=lastcycle.strftime("%Y%m%d")
lasthour=lastcycle.strftime("%H")
fhours1=range(closest,furthest,3)
tf=0
#for i in range(len(members)):
for j in range(len(fhours1)):
  for i in range(len(members)):
    if (fhours1[j]%6)!=0:
      grbind = pygrib.index('/com2/gens/prod/gefs.'+str(ymd)+'/'+str(hour).zfill(2)+'/pgrb2ap5/ge'+members[i]+'.t'+str(hour).zfill(2)+'z.pgrb2a.0p50.f'+str(fhours1[j]).zfill(3),'name')
      precip=grbind.select(name='Total Precipitation')[0].values*.03937
    else:
      grbindprev = pygrib.index('/com2/gens/prod/gefs.'+str(ymd)+'/'+str(hour).zfill(2)+'/pgrb2ap5/ge'+members[i]+'.t'+str(hour).zfill(2)+'z.pgrb2a.0p50.f'+str(fhours1[j-1]).zfill(3),'name')
      grbind = pygrib.index('/com2/gens/prod/gefs.'+str(ymd)+'/'+str(hour).zfill(2)+'/pgrb2ap5/ge'+members[i]+'.t'+str(hour).zfill(2)+'z.pgrb2a.0p50.f'+str(fhours1[j]).zfill(3),'name')
      precipnewc=grbind.select(name='Total Precipitation')[0].values*.03937
      precipnewp=grbindprev.select(name='Total Precipitation')[0].values*.03937
      precip=precipnewc-precipnewp
    if members[i]=='c00':
      rainper=np.zeros(precip.shape)
      snowper=np.zeros(precip.shape)
      frzper=np.zeros(precip.shape)
      ipper=np.zeros(precip.shape)
      noper=np.zeros(precip.shape)
    catrain=grbind.select(name='Categorical rain')[0].values
    catsnow=grbind.select(name='Categorical snow')[0].values
    catfreezing=grbind.select(name='Categorical freezing rain')[0].values
    catice=grbind.select(name='Categorical ice pellets')[0].values
    precip=np.asarray(precip[::-1,:])
    catrain=np.asarray(catrain[::-1,:])
    catsnow=np.asarray(catsnow[::-1,:])
    catfreezing=np.asarray(catfreezing[::-1,:])
    catice=np.asarray(catice[::-1,:])

    lats,lons=grbind.select(name='Categorical rain')[0].latlons()
    latlist=lats[::-1,0]
    lonlist=lons[0,:]
    lonlist=np.asarray(lonlist)
    latlist=np.asarray(latlist)

    rainper[precip>=.01]=rainper[precip>=.01]+catrain[precip>=.01]
    snowper[precip>=.01]=snowper[precip>=.01]+catsnow[precip>=.01]
    frzper[precip>=.01]=frzper[precip>=.01]+catfreezing[precip>=.01]
    ipper[precip>=.01]=ipper[precip>=.01]+catice[precip>=.01]
    noper[precip<.01]=noper[precip<.01]+1
  rainper=rainper/21.0
  snowper=snowper/21.0
  frzper=frzper/21.0
  ipper=ipper/21.0
  noper=noper/21.0
  pilat=np.linspace(35.0,50.0,31.0)
  pilon=np.linspace(255.0,275.0,41.0)
          
  fig = plt.figure()
  ax = fig.add_subplot(111)
  map = Basemap(ax=ax,llcrnrlon=-105., llcrnrlat=32.5, urcrnrlon=-80.,urcrnrlat=50.,rsphere=(6378137.00,6356752.3142),resolution='i',area_thresh=1000.,projection='lcc',lat_1=45.,lon_0=-95.)
  map.fillcontinents(color='#D4D4D4',zorder=0)
  map.drawmeridians(np.arange(0,360,10))
  map.drawparallels(np.arange(-90,90,5))
  map.drawstates(linewidth=1.0,color='k')
  map.drawcoastlines(linewidth=1.0, color='k')
  map.drawcountries(linewidth=1.0, color='k')
  x,y=map(lons[::-1,:],lats[::-1,:])


  for i in xrange(len(pilon)):
    for j in xrange(len(pilat)):
      a1,a2=map(pilon[i],pilat[j])
      nearestlat=find_nearest(latlist,pilat[j])
      nearestlon=find_nearest(lonlist,pilon[i])
      gray1=0
      gray2=360*noper[nearestlat,nearestlon]
      green1=gray2
      green2=green1+360*rainper[nearestlat,nearestlon]
      blue1=green2
      blue2=blue1+360*snowper[nearestlat,nearestlon]
      red1=blue2
      red2=red1+360*frzper[nearestlat,nearestlon]
      purple1=red2
      purple2=purple1+360*ipper[nearestlat,nearestlon]
    
    #To make the pie chart you're creating n circle wedges that combine into a circle.
    #a1 and a2 are the x and y positions of the center, 30000 is the radius of the wedge in meters I think,
    #the 10 is the start degree and 120 is the end, then there's the color and opacity.
      pierad=19000
      al=1.0
      lw=.25
      if (gray2-gray1)>0 and (gray2-gray1)<359:
        ax.add_patch(patches.Wedge((a1,a2),pierad,gray1,gray2,facecolor="none",alpha=al,linewidth=lw))
      if (green2-green1)>0:
        ax.add_patch(patches.Wedge((a1,a2),pierad,green1,green2,facecolor="#3e9d0b",alpha=al,linewidth=lw))
      if (blue2-blue1)>0:
        ax.add_patch(patches.Wedge((a1,a2),pierad,blue1,blue2,facecolor="#1574ef",alpha=al,linewidth=lw))
      if (red2-red1)>0:
        ax.add_patch(patches.Wedge((a1,a2),pierad,red1,red2,facecolor="#d32df0",alpha=al,linewidth=lw))
      if (purple2-purple1)>0:
        ax.add_patch(patches.Wedge((a1,a2),pierad,purple1,purple2,facecolor="#7f28f0",alpha=al,linewidth=lw))


#  clevs=[.1,.2,.3,.4,.5,.6,.7,.8,.9]
#  clevs=np.linspace(.01,1,256)
#  cspara=map.contourf(x,y,rainper,clevs,cmap=plt.get_cmap('Greens'),extend='max')
#  cb=plt.colorbar(cspara,orientation='horizontal',ticks=[.01,.1,.2,.3,.4,.5,.6,.7,.8,.9],pad=0.02,shrink=.75,aspect=20)
#  cb.ax.tick_params(labelsize=7)
  ax.text(.5,.03,'GEFS precipitation type (F'+str(fhours1[tf]).zfill(2)+')',horizontalalignment='center',fontsize=6,transform=ax.transAxes,bbox=dict(facecolor='white',alpha=.85))
  plt.tight_layout()
  plt.savefig('compare2dew.png', bbox_inches='tight',dpi=150)
  plt.close()
  subprocess.call(['convert','-density', '100x100', 'compare2dew.png', 'testrain'+str(tf+1)+'.gif'])  
  tf+=1
  print str(tf) +' of ' + str(len(fhours1))

