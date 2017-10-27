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
 grb1=grbs.select(name='Convective available potential energy',typeOfLevel="pressureFromGroundLayer")[0]
