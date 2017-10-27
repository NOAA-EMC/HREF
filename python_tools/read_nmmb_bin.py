#!/usr/bin/python
import numpy as np
import fortranfile as F
import sys
import matplotlib.pyplot as plt


def savetxt_3d(name,data):
# from stack overflow
# http://stackoverflow.com/questions/3685265/how-to-write-a-multidimensional-array-to-a-text-file
  with file(name, 'w') as outfile:
    outfile.write('# Array shape: {0}\n'.format(data.shape))
    for data_slice in data:
        np.savetxt(outfile, data_slice)

if __name__ == '__main__':
  
 print('Starting...assuming single precision (32 bit) and native endianess!  This can be changed if needed.')
 if len(sys.argv)!=6:
   sys.exit('Incorrect number of arguments! Usage:python %s %s' %(sys.argv[0],'input_file1','input_file2','im','jm','lm'))

 fname=sys.argv[1]
 fname2=sys.argv[2]
 im=int(sys.argv[3])
 jm=int(sys.argv[4])
 lm=int(sys.argv[5])
#--open first file
 fil=F.FortranFile(fname)
#--open second file
 fil2=F.FortranFile(fname2) 
#--Create some empty arrays - although not very pythonic
 v=np.zeros([im,jm])
 v2=np.zeros([im,jm])  

 xs=np.arange(0,im)
 ys=np.arange(0,jm)

#--Read stuff in - again not very pythonic with the nested loops
 for z in np.arange(0,lm): 
   for y in np.arange(0,jm):
     v[:,y]=fil.readReals('f')
     v2[:,y]=fil2.readReals('f')
     
   vdiff=v-v2
#   if z==30: savetxt_3d('vdiff.txt',vdiff)
   print "%s %.10f %.10f %s %d" % ('f1    max,min: ',np.max(v),np.min(v),'at level',z+1)
   print "%s %.10f %.10f %s %d" % ('f2    max,min: ',np.max(v2),np.min(v2),'at level',z+1)
   print "%s %.10f %.10f %s %d" % ('f1-f2 max,min: ',np.max(vdiff),np.min(vdiff),'at level',z+1)
   if np.sum(np.abs(vdiff))>0:  
     print "%s %d %s" % ('---Whoa! Found Some differences at level',z+1,'Plotting!---')
     plt.clf()
     plt.figure(figsize=(12,6))
     cs1 = plt.contourf(xs,ys,np.transpose(vdiff))
     # add colorbar.
     cbar = plt.colorbar(cs1)
     plt.title('F1-F2 at level'+repr(z+1))
     #cbar.ax.tick_params(labelsize=8.5)
     plt.savefig('./f1-f2_lev'+repr(z+1)+'.png')
     plt.clf()
     
     
     
     
     
     
     

























