import numpy as np
import fortranfile as F
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
 if len(sys.argv)!=7:
   sys.exit('Incorrect number of arguments! Usage:python %s %s' %(sys.argv[0],'input_file','glat_file','glon_file','im','jm','lm'))

 fname=sys.argv[1]
 glat_fname=sys.argv[2]
 glon_fname=sys.argv[3]
 im=int(sys.argv[4])
 jm=int(sys.argv[5])
 lm=int(sys.argv[6])
#--open first file
 fil=F.FortranFile(fname,endian='>')
#--open glat file
 fglat=F.FortranFile(glat_fname,endian='>') 
#--open glon file
 fglon=F.FortranFile(glon_fname,endian='>')

 #--Read lats and lons
 lons=np.degrees([fglon.readReals('f') for y in np.arange(0,jm)])
 print np.shape(lons)
 print np.max(lons),np.min(lons)
 lats=np.degrees([fglat.readReals('f') for y in np.arange(0,jm)])
 print np.shape(lats)
 print np.max(lats),np.min(lats)
 #--Now read the imput file
 fld=np.zeros([jm,im,lm])
 for k in np.arange(0,lm):
   fld[:,:,k] = [fil.readReals('f') for y in np.arange(0,jm)]
 print np.shape(fld)
 print np.max(fld),np.min(fld)

 # create figure and axes instances
 fig = plt.figure(figsize=(11,11))
 ax = fig.add_axes([0.1,0.1,0.8,0.8])

 # Get the lower left and upper right lat and lons in addition to
 # the map background resolution to use for plotting (res, 
 # similar to mpdset in grads)
 dom='CONUS'
 proj='lcc' # Set map projection to lcc
 #llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat,res=ncepy.corners_res(dom,proj=proj)
 llcrnrlon,llcrnrlat,urcrnrlon,urcrnrlat=-125.5,15.0,-52.0,55.0
 res='h'
 # Create the map background (LCC projection, corners, reference longitude, etc.)

 lat_1=25.0  # True latitude for the LCC projection
 lon_0=-95.0 # Reference longitude for the LCC projection
 m = Basemap(llcrnrlon=llcrnrlon,llcrnrlat=llcrnrlat,urcrnrlon=urcrnrlon,urcrnrlat=urcrnrlat,\
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

#--Plot each vertical level cover the domain outlined above
#  for k in np.arange(0,lm): 
 for k in np.arange(9,59,10): 
#    if k==30: savetxt_3d('vdiff.txt',vdiff)
     print "%s %d" % ('Working on model level',k+1)
     #colorfill actual gridpoint values with pcolormesh (this can be really slow)      
#     cs1 = m.pcolormesh(lons,lats,1.e3*fld[:,:,k],latlon=True,vmin=0.,vmax=30.) #cmap=my_cmap,vmin=0.001,vmax=200.)
#     cs1 = m.pcolormesh(lons,lats,1.e3*fld[:,:,k],latlon=True,vmin=0.,vmax=30.) #cmap=my_cmap,vmin=0.001,vmax=200.)
clevs=np.arange(-30.,30.,1.)
cmap=plt.get_cmap(name='gist_ncar')  # Use matplotlibs terrain color map
cs1 = m.contourf(lons,lats,1.e3*fld[:,:,k],clevs,cmap=cmap,latlon=True,extend='max')
cbar = m.colorbar(cs1,location='bottom',pad="5%")
cbar.ax.tick_params(labelsize=8.5)
#     plt.title(' '+fname)
plt.title('Lev '+repr(k+1))
plt.savefig('./'+os.path.basename(fname)+'_Lev'+repr(k+1)+'.png')
ncepy.clear_plotables(ax,keep_ax_lst,fig)

     
     
     
     
     
     
     

























