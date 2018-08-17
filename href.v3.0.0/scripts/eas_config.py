# SSEOx CONFIGURATION FILE

###########################
# SETTINGS FOR ALL FIELDS #
###########################

# number of ens members
nm = 10
nm_ak = 8
nm_nonconus = 6
# model resolution (km)
dx = 5
# min latency (hours)
min_latency = 0
# max latency (hours)
max_latency = 12
max_latency_nam = 6

#################
# PQPF SETTINGS #
#################

# exceedance thresholds (in)

# full set
# pqpf_1h_thresh = [.01,0.25,0.5,1.0,2.0,3.0]
# pqpf_3h_thresh = [.01,0.25,0.5,1.0,2.0,3.0]
# pqpf_6h_thresh = [.01,0.25,0.5,1.0,2.0,3.0]
# pqpf_12h_thresh = [0.1,0.25,0.5,1.0,2.0,3.0,5.0]
# pqpf_24h_thresh = [0.1,0.25,0.5,1.0,2.0,3.0,5.0]

# possible optimal set?
pqpf_1h_thresh = [.01,0.25,0.5]
pqpf_3h_thresh = [.01,0.25,0.5,1.0]
# 2" added to below for FFAIR
pqpf_6h_thresh = [.01,0.25,0.5,1.0,2.0]
pqpf_12h_thresh = [0.1,0.25,0.5,1.0,2.0]
pqpf_24h_thresh = [0.1,0.25,0.5,1.0,2.0]

# neighborhood size (km)
pqpf_neighborhood = 40

# spatial filter radius (km)
pqpf_1h_filter = 60
pqpf_3h_filter = 100
pqpf_6h_filter = 100
rlist = [10,25,40,55,70,85,100]

# EAS settings
alpha = 0.1	# similarity criteria parameter
p_smooth = 8	# width of Gaussian filter for smoothing radius field (grid points) 

# forecast hour weighting
pqpf_1h_weight1 = 1.0  # hour before
pqpf_1h_weight2 = 1.0  # valid hour
pqpf_1h_weight3 = 1.0  # hour after

# forecast periods begin at forecast hour(s):
pqpf_1h_starthours = [0,1,2,3,4,5,6,7,8,9,10,11,14,17,20,23,26,29,32,35]
pqpf_3h_starthours = [0,1,2,3,4,5,6,7,8,9,12,15,18,21,24,27,30,33]
pqpf_6h_starthours = [0,3,6,9,12,15,18,21,24,27,30]
pqpf_24h_starthours = [0,6,12]

# PQPF calibration
pqpf_6h_calibrate_arw = 'yes'
pqpf_6h_calibrate_nmmb = 'yes'
pqpf_6h_calibrate_nssl = 'yes'
pqpf_6h_calibrate_nam = 'yes'
pqpf_6h_calibrate_hrrr = 'yes'
pqpf_3h_calibrate_arw = 'yes'
pqpf_3h_calibrate_nmmb = 'yes'
pqpf_3h_calibrate_nssl = 'yes'
pqpf_3h_calibrate_nam = 'yes'
pqpf_1h_calibrate_arw = 'yes'
pqpf_1h_calibrate_nmmb = 'yes'
pqpf_1h_calibrate_nssl = 'yes'
pqpf_1h_calibrate_nam = 'yes'
# minimum quantile for curve fitting
pqpf_6h_minpct = 75
pqpf_3h_minpct = 75
pqpf_1h_minpct = 75
# order of curve fit for calibration
pqpf_6h_fitorder = 2
pqpf_3h_fitorder = 2
pqpf_1h_fitorder = 2
# Number of forecasts to use for calibration
pqpf_climo_size = 50
# Maximum number of days to keep in climatology directories
pqpf_climo_days = 20	# 1000


########################
# PType SETTINGS       #
########################

ptype_fcsthours = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36]

########################
# FLASH-FLOOD SETTINGS #
########################

# ARI uses spatial filter from 6-h PQPF
# both products use pqpf neighborhood

# exceedance thresholds (in)
runoff_3h_thresh = [1.0,2.0,3.0]

# recurrence interval exceedance threshold
ari_years = 100

# percent of this exceedance threshold for a "hit"
ari_pct = 100

# spatial filter radius (km)
runoff_3h_filter = 100

# forecast periods begin at forecast hour(s):
ari_starthours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]
runoff_3h_starthours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]


#################
# SNOW SETTINGS #
#################

# exceedance thresholds (in)
snow_1h_thresh = [0.5,1.0,3.0]
snow_3h_thresh = [1.0,3.0,6.0]
snow_6h_thresh = [1.0,3.0,6.0,12.0]

# neighborhood size (km)
snow_neighborhood = 40

# spatial filter radius (km)
snow_1h_filter = 60
snow_3h_filter = 100
snow_6h_filter = 100

# forecast hour weighting
snow_1h_weight1 = 1.0  # hour before
snow_1h_weight2 = 1.0  # valid hour
snow_1h_weight3 = 1.0  # hour after

# forecast periods begin at forecast hour(s):
snow_1h_starthours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
snow_6h_starthours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]


################################
# T-STORM PROBABILITY SETTINGS #
################################

# T-Storm discipline/cat/parms and thresholds
tstm_parm_a = [0,17,192] # lightning flash density (flashes per km^2 per 5 min)
tstm_parm_a_thresh = 0.02
tstm_parm_b = [0,1,8] # precipitation last hour
tstm_parm_b_thresh = 0.254

# forecast hour weighting
tstm_weight1 = 1.0  # hour before
tstm_weight2 = 1.0  # hour after

# neighborhood size (km)
tstm_neighborhood = 40

# spatial filter radius (km)
tstm_filter = 90

# t-storm forecasts valid at forecast hour(s):
tstm_validhours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]


##################################
# SEVERE WX PROBABILITY SETTINGS #
##################################

# wind discipline/cat/parm and threshold
wind_parm_a = [0,2,1] # max 10-m wind speed last hour [m/s]
#wind_parm_a = [0,2,22] # 10-m wind gust [m/s]
wind_parm_a_thresh = 17.5
wind_parm_b = [0,7,193] # most unstable lifted index
wind_parm_b_thresh = 0.0
wind_parm_b_smooth = 120  # gaussian filter width (km)

# hail discipline/cat/parm and threshold
hail_parm = [0,1,74] # VIG (mm)
hail_parm_thresh = 35.0

# tornado discipline/cat/parm/surfacetype/level, smoothing and thresholds
tor_parm_a = [0,7,199,'sfc',5000] # max 2-5-km UH last hour [m^2/s^2]
tor_parm_a_thresh = 100
tor_parm_b = [0,3,5,5,0] # smoothed surface LCL height MSL [m]
tor_parm_b_thresh = 1500
tor_parm_b_smooth = 120  # gaussian filter width (km)
tor_parm_c1 = [0,2,15,'sfc',0] # smoothed 0-1-km U-comp shear [m/s]
tor_parm_c2 = [0,2,16,'sfc',0] # smoothed 0-1-km V-comp shear [m/s]
tor_parm_c_thresh = 10
tor_parm_c_smooth = 120  # gaussian filter width (km)
tor_parm_d1 = [0,7,6,'sfc',0] # SBCAPE
tor_parm_d2 = [0,7,6,108,25500] # MUCAPE
tor_parm_d_thresh = 0.75
tor_parm_d_smooth = 120  # gaussian filter width (km)

# neighborhood size (km)
severe_neighborhood = 40

# spatial filter radii (km)
wind_filter = 120
hail_filter = 120
tor_filter = 120

# severe wx forecast periods begin at forecast hour(s):
severe_starthours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

# forecast interval length (hours)
severe_interval = 4


#################################
# AVIATION PROBABILITY SETTINGS #
#################################

# visibility thresholds (miles)
vis_thresh = [5.0,3.0,1.0]
# ceiling height thresholds (feet)
ceil_thresh = [3000,1000,500]
# IFR vis/ceiling thresholds (miles/feet)
ifr_vis_thresh = 3
ifr_ceil_thresh = 1000
# echo top thresholds (feet)
echotop_thresh = [25000,30000,35000]

# IFR indicated by flight rules (parm 205) = X 
ifr_grib_value = 2

# smoothing of vis/ceiling/echotop data (Gaussian filter width in km)
vis_smooth = 40
ceil_smooth = 40
echotop_smooth = 40

# spatial filter radii (km)
vis_filter = 90
ceil_filter = 90
echotop_filter = 90
ifr_filter = 90

# forecast valid at forecast hour(s)
vis_validhours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
ceil_validhours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
ifr_validhours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
echotop_validhours = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]


