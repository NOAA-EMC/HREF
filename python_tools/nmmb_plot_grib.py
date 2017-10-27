import numpy as np
import fortranfile as F
import pygrib
import sys,ncepy,os
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap, cm, maskoceans


def savetxt_3d(name,data):
# from stack overflow
# http://stackoverflow.com/questions/3685265/how-to-write-a-multidimensional-array-to-a-text-file
  with file(name, 'w') as outfile:
    outfile.write('# Array shape: {0}\n'.format(data.shape))
    for data_slice in data:
        np.savetxt(outfile, data_slice)

if __name__ == '__main__':
  
 print('Starting...assuming single precision (32 bit) and BIG endian!  This can be changed if needed.')
 if len(sys.argv)!=3:
   sys.exit('Incorrect number of arguments! Usage:python %s %s' %(sys.argv[0],'input_file','iso level'))

 fname=sys.argv[1]

 grbs=pygrib.open(fname)

 for g in grbs:
  print g

 lz=int(sys.argv[2])

 print lz

 print ('past grbs')

#  grb=grbs[37]
 grb=grbs.select(name='Geopotential Height',level=lz)[0]

 lat,lon = grb.latlons()

 temp_vals=grb.values

 print np.amin(temp_vals),np.amax(temp_vals)


 # create figure and axes instances
 fig = plt.figure(figsize=(11,11))
 ax = fig.add_axes([0.1,0.1,0.8,0.8])

 # Get the lower left and upper right lat and lons in addition to
 # the map background resolution to use for plotting (res, 
 # similar to mpdset in grads)
 dom='CONUS'
 proj='lcc' # Set map projection to lcc
 llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat=-125.5,15.0,-52.0,55.0
 res='h'
 # Create the map background (LCC projection, corners, reference longitude, etc.)
 lat_1=25.0  # True latitude for the LCC projection
 lon_0=-95.0 # Reference longitude for the LCC projection
 m=Basemap(llcrnrlon=llcrnrlon,llcrnrlat=llcrnrlat,urcrnrlon=urcrnrlon,urcrnrlat=urcrnrlat,\
   	      rsphere=(6378137.00,6356752.3142),\
   	      resolution=res,projection=proj,\
   	      lat_1=lat_1,lon_0=lon_0,ax=ax)

 # Add some nice geographic stuff, like country boundaries, state lines, etc.
 m.drawcoastlines(linewidth=1.25)
 m.drawstates(linewidth=1.25)
 m.drawcountries(linewidth=1.25)
 #  Map/figure has been set up here (bulk of the work), save axes instances for
 #     use again later   
 keep_ax_lst = ax.get_children()[:]

 clevs=np.arange(np.amin(temp_vals),np.amax(temp_vals),20.)
# cmap=plt.get_cmap(name='gist_ncar')  # Use matplotlibs terrain color map
 cmap=plt.get_cmap(name='bone')  # Use matplotlibs terrain color map

 cs1 = m.contour(lon,lat,temp_vals,clevs,cmap=cmap,latlon=True,extend='max')
# cbar = m.colorbar(cs1,location='bottom',pad="5%")
# cbar.ax.tick_params(labelsize=8.5)
 
 
 plt.title(repr(lz)+'Z')
 plt.savefig('./'+os.path.basename(fname)+'_'+repr(lz)+'.png')
 ncepy.clear_plotables(ax,keep_ax_lst,fig)

     
     
     
     
     
     
     

























