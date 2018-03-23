import os, sys, time
import numpy as np
import pygrib
from itertools import chain
from netCDF4 import Dataset
import fortranfile as F

file='para.f27.grb2_mean'

fhour = 27
shour = fhour - 3

WGRIB2='./wgrib2'

climofile= 'python_extract_ak.grib2'

record = 1              # PQPF from SREF pgrb212 file

def simplewgrib2_write1d(nx,ny,array1d,fout):
  tmps= []
  
  ilim=nx*ny
  nxjunk=nx
  ij=0
  newstr=str(nx)+' '+str(ny)+' \n'
  fout.write(newstr)
  while ij < ilim:
    newstr=str(array1d[ij])
#    print 'ij, newstr 1d: ', ij,newstr
    fout.write(newstr+' \n')
    ij=ij+1
  return nxjunk
  fout.close()

def simplewgrib2_write(array2d,fout):
  tmps= []
  
  ny,nx=array2d.shape
  nxjunk=nx
  print 'array2d.shape: ', array2d.shape
  ilim=nx*ny
  j=0
  newstr=str(nx)+' '+str(ny)+'\n'
  fout.write(newstr)
  while j < ny:
    i=0
    while i < nx:
      newstr=str(array2d[j,i])
      fout.write(newstr+'\n')
      i=i+1
    j=j+1
  return nx
  fout.close()

def simplewgrib2_write_bin(array2d,fout):
  tmps= []
  
  ny,nx=array2d.shape
  nxjunk=nx
  print 'array2d.shape: ', array2d.shape
  ilim=nx*ny
  j=0
  val=nx*ny*4
#  fout.write(val)
  fout.write(array2d)
  return nx
  fout.close()

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
    array1d = np.asarray(tmps,dtype=np.float32)
    array2d.shape = (ny,nx)
    array1d.shape = (ilim)
    return array2d,array1d,nx,ny
  F1.close()

os.system(WGRIB2+' '+file+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -grib '+climofile)

# get dimensions and message from template file
grbs = pygrib.open(climofile)
grbtmp = grbs[record]
lats, lons = grbtmp.latlons()
undefmask = grbs[1].values
grbs.close()
nlats, nlons = np.shape(lats)

maskregion = np.ma.filled(undefmask,-9999)
print 'maskregion defined'

# print 'pygrib maskregion: ', maskregion

print ' ========================================= '

climofile_bin = 'python_extract_ak.bin'
climofile_txt = 'python_extract_ak.txt'
os.system(WGRIB2+' '+file+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -bin '+climofile_bin)
os.system(WGRIB2+' '+file+' -match "APCP:surface:%i'%shour+'-%i'%fhour+'" -end -text '+climofile_txt)

os.system(WGRIB2+' '+file+' -match_fs "APCP:surface:%i'%shour+'-%i'%fhour+'" -text mask.txt -rpn rcl_lat -text latak  -rpn rcl_lon -text lonak') 

fil=F.FortranFile(climofile_bin,endian='>')
# from_fort_bin=([fil.readReals('f') for y in np.arange(0,ny)])
# climo_array=fil.readReals('f')
# print 'shape climo_array: ', np.shape(climo_array)


lats= []
lons= []

lon,lon1d,nx,ny=simplewgrib2('lonak')

tstfile='garbage.txt'
tst_use=open(tstfile,"w")

tstfile1d='garbage1d.txt'
tst_use_1d=open(tstfile1d,"w")

tstfilebin='garbage.bin'
tst_use_bin=open(tstfilebin,"w")

lat,lat1d,nx,ny=simplewgrib2('latak')
field,field1d,nx,ny=simplewgrib2('mask.txt')

simplewgrib2_write1d(nx,ny,lat1d,tst_use_1d)
simplewgrib2_write(lat,tst_use)
# simplewgrib2_write_bin(lat,tst_use_bin)

# lons=np.degrees([fglon.readReals('f') for y in np.arange(0,jm)])
myfort = F.FortranFile('garbage_fortran.bin',mode='w')
myfort.writeReals(field)

myfort = F.FortranFile('garbage_fortran.bin',mode='r')
latnew=myfort.readReals('f')
print 'shape latnew: ', np.shape(latnew)


ilim=nx*ny
print 'ilim: ', ilim

### see if can do np.ma.filled operation from wgrib2 derived field

undefmask = field
print 'undefmask mean: ', undefmask.mean()
maskregion = np.ma.masked_greater(undefmask,9.0e+20)
print 'maskregion mean: ', maskregion.mean()
maskregionnew = np.ma.filled(maskregion,-9999)



os.system(WGRIB2+' '+file+' -match_fs ":TMP:500 mb:"  -set_metadata_str "0:0:d=2014101012:HGT:2 m above ground:14 hour fcst:90% level"  -set_grib_type c3 -grib_out noimppyth.grib2')

os.system(WGRIB2+' '+file+' -match_fs ":TMP:500 mb:" -import_bin garbage_fortran.bin -set_metadata_str "0:0:d=2014101012:HGT:2 m above ground:18 hour fcst:2 m above ground" -set_grib_type c3 -grib_out imppythbin.grib2')

