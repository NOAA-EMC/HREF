##################################
# HRRR-TLE calibration functions
#
# Isidora Jankov
# Trevor Alcott
# Curtis Alexander
#
# Jan-May 2016
##################################

from numpy import *

def quantile_map(qpf,qpe,minpct,fitorder):

# quality-control the qpf and qpe data
    qpe = where(greater(qpe,998),-1.0,qpe)
    qpf = where(less(qpe,0),-1.0,qpf)
    qpe = where(less(qpf,0),-1.0,qpe)

    # masks out the bad values (-1.0 in prev lines)
    qpe = where(less(qpe,0),float('NaN'),qpe)
    qpf = where(less(qpf,0),float('NaN'),qpf)

# set up quantile bounds
    q = []
    for p in arange(minpct,99,1):
        q.append(p)
    for p in arange(99,99.9,0.01):
        q.append(p)
    for p in arange(99.9,100,0.001):
        q.append(p)
    q.append(100)
   
    #qpf = where(less(qpf,2.54),float('NaN'),qpf)
    #qpe = where(less(qpe,2.54),float('NaN'),qpe)

    fcst_quantiles = nanpercentile(qpf,q)
    obs_quantiles = nanpercentile(qpe,q)
    print 'Upper forecast quantiles (mm):\n',fcst_quantiles[-30:-1]
    print 'Upper analysis quantiles (mm):\n',obs_quantiles[-30:-1]

    x_fcst1 = q[:-1]
    y_fcst1 = fcst_quantiles[:-1]
    y_obs1 = obs_quantiles[:-1]

    for count in range(len(x_fcst1)):
        if y_obs1[count]==0:
           y_fcst1[count]=0

    coeffs = polyfit(y_fcst1, y_obs1, fitorder)
    polynomial = poly1d(coeffs)
    y_training = polynomial(y_fcst1)

    if coeffs[1]>0:
      print 'QPF_adj = %.4f'%coeffs[0]+'*QPF + %.4f'%coeffs[1]
    else:
      print 'QPF_adj = %.4f'%coeffs[0]+'*QPF - %.4f'%(-1.0*coeffs[1])

    return flipud(coeffs)
