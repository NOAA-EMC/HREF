# Ben Blake
# 9 June 2016
# Script to generate circles and then calculate/plot neighborhood probability at all grid points
# 4 members - probability at each point is the average of the probability of all members at that point
# For the similarity agreement scale method - refer to Dey et al 2016 for more info

from scipy import ndimage,signal
import matplotlib.pyplot as plt
import numpy as np
import math as m
import time
import sys

xmin = 0	# X minimum value
xmax = 150	# X maximum value
ymin = 0	# Y minimum value
ymax = 150	# Y maximum value
x = np.arange(xmin,xmax,1)
y = np.arange(ymin,ymax,1)
xx, yy = np.meshgrid(x, y)

fig = plt.figure()
plt.axis([0,xmax,0,ymax])
plt.xticks(np.arange(0,xmax+1,25))
plt.yticks(np.arange(0,ymax+1,25))
ax = fig.add_subplot(1,1,1)
x1 = (xmax)/2.
y1 = (ymax)/2. + 13.909
r1 = 18
circ1 = plt.Circle((x1,y1), radius=r1, alpha = 0.7, edgecolor="black", facecolor="red", linewidth = 2.0)
x2 = (xmax)/2. - 19
y2 = ymax/2. - 19
r2 = 18
circ2 = plt.Circle((x2,y2), radius=r2, alpha = 0.7, edgecolor="black", facecolor="blue", linewidth = 2.0)
x3 = xmax/2. + 19
y3 = (ymax)/2. - 19
r3 = 18
circ3 = plt.Circle((x3,y3), radius=r3, alpha = 0.7, edgecolor="black", facecolor="yellow", linewidth = 2.0)
#x4 = (xmax+30)/2.
#y4 = ymax/2.
#r4 = 15
#circ4 = plt.Circle((x4,y4), radius=r4, color='turquoise')
#x5 = (xmax+40)/2.
#y5 = (ymax+25)/2.
#r5 = 10
#circ5 = plt.Circle((x5,y5), radius=r5, color='y')
ax.add_patch(circ1)
ax.add_patch(circ2)
ax.add_patch(circ3)
#ax.add_patch(circ4)
#ax.add_patch(circ5)
plt.show()

#--------------------------------------------------------------
# Define the final probability array, neighborhood size, etc.
mem = 3		# Number of members
pairs = (mem*(mem-1))*0.5	# Number of pairs
pairs = int(pairs)		# Need as an integer not a float
probfinal = np.zeros((ymax,xmax)).astype(float)
grid = np.zeros((mem,ymax,xmax)).astype(float)

# Loop through all points in domain
# Create a grid array of hits - points inside the circle are set to 1
for member in range(0,mem):
  for row in range(0,ymax):
    for column in range(0,xmax):
# Circle 1
      if (member == 0) and (((column-x1)**2 + (row-y1)**2) <= r1**2):
        grid[member,row,column] = 1
# Circle 2
      elif (member == 1) and (((column-x2)**2 + (row-y2)**2) <= r2**2):
        grid[member,row,column] = 1
# Circle 3
      elif (member == 2) and (((column-x3)**2 + (row-y3)**2) <= r3**2):
        grid[member,row,column] = 1

# Get optimal radius size for each grid point
frac = np.zeros((mem)).astype(float)	# Forecast fraction for each member
if (mem == 3):
  pr1 = [1,2,2]
  pr2 = [0,0,1]
elif (mem == 4):
  pr1 = [1,2,3,2,3,3]
  pr2 = [0,0,0,1,1,2]
elif (mem == 5):
  pr1 = [1,2,3,4,2,3,4,3,4,4]
  pr2 = [0,0,0,0,1,1,1,2,2,3]

optrad = np.zeros((ymax,xmax)).astype(float)
dij = np.zeros((pairs)).astype(float)

slim = 10
t1 = time.time()
for row in range(slim,ymax-slim):
  for column in range(slim,xmax-slim):
    r = 2		# Start with a radius of 0 (1 grid point)
    alph = 0.5
    dij[:] = 0		# Reset dij array to 0's
    dijmean = 1		# Set to an arbitrary initial value satisfying the condition
    dcrit = 0		# Set to an arbitrary initial value satisfying the condition
    zero = 'no'

    while (r <= slim) and (dijmean > dcrit):
      footprint = np.ones((r*2+1,r*2+1)).astype(int)	# Creates array for neighborhood
      footprint[m.ceil(r),m.ceil(r)]=0		# Sets the center point to 0
      dist = ndimage.distance_transform_edt(footprint)
      footprint = np.where(np.greater(dist,r),0,1)    
      total = sum(sum(footprint))
      for member in range(0,mem):
        truth = np.where(footprint == 1, grid[member,row-r:row+r+1,column-r:column+r+1],0)
        frac[member] = (float(sum(sum(truth)))) / total	# Gives average over all points in neighborhood
      if (r == slim):
        fracsum = sum(frac) / mem
        if (fracsum == 0):
          zero = 'yes'
          break
      for pair in range(0,pairs):
        if (frac[pr1[pair]] == 0) and (frac[pr2[pair]] == 0):
          dij[pair] = 1
        else:
          numer = (frac[pr1[pair]] - frac[pr2[pair]])**2 
          denom = ((frac[pr1[pair]])**2 + (frac[pr2[pair]])**2)
          dij[pair] = numer / denom
      dijmean = sum(dij) / pairs
      dcrit = alph + ((1 - alph) * (r / slim))
      r = r + 1
    if (zero == 'yes'):
      optrad[row,column] = 10.2
    else:
      optrad[row,column] = r - 1	# Suitably similar scale
t2 = time.time()
print('Total time for get_optrad: ', t2-t1)

optrad = ndimage.filters.gaussian_filter(optrad,2)


def get_footprint(r):
    footprint = np.ones((r*2+1,r*2+1)).astype(int)
    footprint[m.ceil(r),m.ceil(r)]=0
    dist = ndimage.distance_transform_edt(footprint)
    footprint = np.where(np.greater(dist,r),0,1)
    return footprint

t3 = time.time()

filter_footprint_1 = get_footprint(1)
filter_footprint_2 = get_footprint(2)
print filter_footprint_2
print np.sum(filter_footprint_2)
filter_footprint_3 = get_footprint(3)
filter_footprint_4 = get_footprint(4)
filter_footprint_5 = get_footprint(5)
filter_footprint_6 = get_footprint(6)
filter_footprint_7 = get_footprint(7)
filter_footprint_8 = get_footprint(8)
filter_footprint_9 = get_footprint(9)
filter_footprint_10 = get_footprint(10)

prob = np.zeros((ymax,xmax)).astype(float)

for row in range(slim,ymax-slim):
  for column in range(slim,xmax-slim):
    rad2 = optrad[row,column]
    for member in range(0,mem):
      if (0.5 <= rad2 < 1.5):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_1,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
      elif (1.5 <= rad2 < 2.5):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_2,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
#        if (87 <= row <= 92) and (97 <= column <= 102):
#          print prob[row,column], convolve[rad2+1,rad2+1]
      elif (2.5 <= rad2 < 3.5):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_3,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
      elif (3.5 <= rad2 < 4.5):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_4,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
      elif (4.5 <= rad2 < 5.5):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_5,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
      elif (5.5 <= rad2 < 6.5):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_6,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
      elif (6.5 <= rad2 < 7.5):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_7,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
      elif (7.5 <= rad2 < 8.5):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_8,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
      elif (8.5 <= rad2 < 9.5):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_9,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
      elif (9.5 <= rad2 <= 10):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_10,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]
      elif (rad2 > 10):
        convolve = signal.fftconvolve(grid[member,row-rad2:row+rad2+1,column-rad2:column+rad2+1],filter_footprint_10,mode='same')
        prob[row,column] = prob[row,column] + convolve[rad2+1,rad2+1]

probfinal = np.zeros((ymax,xmax)).astype(float)

for row in range(slim,ymax-slim):
  for column in range(slim,xmax-slim):
    rad2 = optrad[row,column]
    if (0.5 <= rad2 < 1.5):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_1) * 3)
    elif (1.5 <= rad2 < 2.5):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_2) * 3)
    elif (2.5 <= rad2 < 3.5):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_3) * 3)
    elif (3.5 <= rad2 < 4.5):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_4) * 3)
    elif (4.5 <= rad2 < 5.5):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_5) * 3)
    elif (5.5 <= rad2 < 6.5):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_6) * 3)
    elif (6.5 <= rad2 < 7.5):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_7) * 3)
    elif (7.5 <= rad2 < 8.5):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_8) * 3)
    elif (8.5 <= rad2 < 9.5):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_9) * 3)
    elif (9.5 <= rad2 <= 10):
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_10) * 3)
    else:
      probfinal[row,column] = (prob[row,column] * 100) / (np.sum(filter_footprint_10) * 3)
      optrad[row,column] = 0

# fix for big circle overlap - unknown problem in center with convolution
optrad2 = np.zeros_like(optrad)
for row in range (slim, ymax-slim):
  for column in range (slim, xmax-slim):
#    if (70 <= row <= 80) and (70 <= column <= 80):
#      probfinal[row,column] = 100
    if (row < 30) or (column < 30) or (row > 125) or (column > 125):
      optrad2[row,column] = 0.0
    else:
      optrad2[row,column] = optrad[row,column]

t4 = time.time()
print('Time for convolution: ', t4-t3)

#-----------------------------------------------------------------
# Plot radii field
plt.figure()
plt.axis([0,xmax+1,0,ymax+1])
plt.xticks(np.arange(0,xmax+1,25))
plt.yticks(np.arange(0,ymax+1,25))
clevs = [1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5]
#cs = plt.pcolormesh(xx,yy,optrad,colors=['blue','dodgerblue','springgreen','forestgreen','yellow','darkorange','darkorchid','magenta','firebrick'],levels=clevs)
cs = plt.contourf(xx,yy,optrad2,colors=['blue','dodgerblue','cyan','springgreen','forestgreen','yellow','goldenrod','darkorange','firebrick'],levels=clevs)
plt.colorbar(cs,ticks=[1,2,3,4,5,6,7,8,9,10])
plt.grid(color='k')
plt.show()

# Plot neighborhood probability for each grid point in the domain
plt.figure()
plt.axis([0,xmax+1,0,ymax+1])
plt.xticks(np.arange(0,xmax+1,25))
plt.yticks(np.arange(0,ymax+1,25))
colorlist = ['white','blue','deepskyblue','mediumspringgreen','limegreen',\
             'yellow','gold','darkorange','red','firebrick','darkorchid','hotpink']
clevs = [0,5,10,20,30,40,50,60,70,80,90,95,100]
cs = plt.contourf(xx,yy,probfinal,clevs,colors=colorlist,extend='max')
#cs = plt.pcolormesh(xx,yy,probfinal,clevs,colors=colorlist,extend='max')
cs.cmap.set_over('hotpink')
plt.colorbar(cs,ticks=clevs)
plt.grid()
plt.show()
