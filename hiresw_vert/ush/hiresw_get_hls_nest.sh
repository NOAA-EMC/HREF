#!/bin/sh

#
# This script reads the hysplit input file and determines the 
# appropriate HiResWindow Nest, PDY and cycle to use for the run.
# The script simply returns the HiResWindow Nest name, PDY and cycle.
#
# 12-12-08 B. Stunder ARL
# hiresw_get_hls_nest.sh is run first by JHYSPT_DATACHK.sms.prod. The comments at 
# the top of the script say it reads hysplit.ini and outputs hiresw nest, PDY and cyc. 
# Subsequently 2 additional parameters were added to the output, but later not used.  
# This is the PDY and cyc for the hiresw meteorology model run.  PDY and cyc are set 
# to the start of the release.  Then 2 hours are subtracted from cyc, and PDY set to 
# the previous day as needed (e.g. if release start time is 00z).  Possibly the 2 hours 
# were subtracted because it takes 2 hours to run the hiresw model and/or for it to be 
# available.  For instance if the release started at 13z, subtracting 2 gives 11z, which 
# is rounded down to 06z, meaning the 06z hiresw is run, because the 12z is not yet
# available.  JHYSPT_FCST reset PDY and cyc for GFS and NAM to the 'current' date/time, 
# and in exhysplit_fcst.shs.sms, the appropriate GFS or NAM cycle is chosen starting 
# with the 'current' PDY/cyc.  

# Now (12-8-08), a timestamp file is written in the DATACHK job, based on the PDY/cyc 
# from hiresw_get_hls_nest.sh.  This timestamp is then read by JHYSPT_FCST and 
# JHYSPT_POST for consistency. The problem is that the meteorology forecast used 
# should not be based on the release start time, but should be based on the 'current' 
# time.   With the RSMC test 12-11-08, the 06z GFS was the current forecast, but the 
# release began at 00z.  The forecast used was 18z Dec. 10 (cyc minus 2 hours gave 
# previous day's fcst).  But it should have used the current 06z forecast, and the 
# GDAS data between 00z and 06z.

# Since hiresw is commented out of the setup script for the time being, determining the 
# nest is not needed.  PDY/cyc is used for (apparently only) two purposes: (1) naming 
# the output files, and (2) determining the correct forecast file to use.  Hence the 
# change in definition of PDY/cyc in hiresw_get_hls_nest.sh should be changed from that 
# based on the release start to the 'current' date/time.  

set -x

org=${org:-sdm}
envir=${envir:-prod}
DATA=${DATA:-.}
infile=/com/hysplit/$envir/input/hysplit.ini

#############################################################
# Set Model Simulation Variables - Read In SDM Input
#############################################################
cp $infile hysplit.ini
if [ -s hysplit.ini ]
then
  k=0
  cat hysplit.ini | while read tmp
  do
    let k=$k+1
    case $k in
      1)  export title=$tmp;;
      2)  export meteo=$tmp;;
      3)  export site=$tmp;;
      4)  export olat=$tmp;;
      5)  export olon=$tmp;;
      6)  export olvl1=$tmp;;
      7)  export olvl2=$tmp;;
      8)  export nhrs=$tmp;;
      9)  export mtype=$tmp;;
      10) export dir1=$tmp;;
      11) export meteo1=$tmp;;
      12) export dir2=$tmp;;
      13) export meteo2=$tmp;;
      14) export dir3=$tmp;;
      15) export meteo3=$tmp;;
      16) export ident=$tmp;;
      17) export qrat=$tmp;;
      18) export qhrs=$tmp;;
      19) export dsyr=$tmp;;
      20) export dsmo=$tmp;;
      21) export dsda=$tmp;;
      22) export dshr=$tmp;;
      23) export dsmn=$tmp;;
      24) export cnlat=$tmp;;
      25) export cnlon=$tmp;;
      26) export dlat=$tmp;;
      27) export dlon=$tmp;;
      28) export splat=$tmp;;
      29) export splon=$tmp;;
      30) export height=$tmp;;
      31) export csyr=$tmp;;
      32) export csmo=$tmp;;
      33) export csda=$tmp;;
      34) export cshr=$tmp;;
      35) export dhr=$tmp;;
      36) export delhr=$tmp;;
      37) export cnlat2=$tmp;;
      38) export cnlon2=$tmp;;
      39) export dlat2=$tmp;;
      40) export dlon2=$tmp;;
      41) export splat2=$tmp;;
      42) export splon2=$tmp;;
      43) export height2=$tmp;;
      44) export dhr2=$tmp;;
      45) export dryvl=$tmp;;
      46) export wetin=$tmp;;
      47) export wetlo=$tmp;;
      48) export rhalf=$tmp;;
      49) export icmt=$tmp;;
      50) export initd=$tmp;;
      51) export khmax=$tmp;;
      52) export numpar=$tmp;;
      53) export isot=$tmp;;
      54) export ndump=$tmp;;
      55) export kmsl=$tmp;;
      56) export seq=$tmp;;
      57) export system_prio=$tmp;;             # added 3-27-03
    esac
 done
else
   msg='hysplit.ini file not found -- job terminated'
   postmsg "$jlogfile" "$msg"
   export err=1; err_exit
fi

# 12-12-08 Add this section. 
# PDY and cyc are for now, today, as are usually defined
  year=`date -u +%Y`
  mon=`date -u +%m`
  day=`date -u +%d`
  cyc=`date -u +%H`

  if test $cyc -lt 6
  then
    cyc=00
  else
    if test $cyc -lt 12
    then
      cyc=06
    else
      if test $cyc -lt 18
      then
        cyc=12
      else
        cyc=18
      fi
    fi
  fi
# end added section

# 12-12-08 Comment this section out.
#year=$dsyr
#mon=$dsmo
#day=$dsda
#cyc=$dshr

PDY=$year$mon$day

# 12-13-08 Comment this section out.
#let cyc=cyc-2
#if [ $cyc -lt 00 ]
#then
#   let cyc=cyc+24
#   let PDY=`/nwprod/util/ush/finddate.sh $PDY d-1`
#fi
#let cyc=cyc/6*6
#if [ $cyc -lt 10 ]
#then
#   cyc="0$cyc"
#fi

priority=$system_prio
model=$meteo1
run_id=$site
latitude=$olat
longitude=$olon
let longitude=longitude*-1

if test "$model" = "HIRESW"
then

   hls_nest=`/nwprod/exec/hiresw_grid_select <<EOF
&LATLON_INPUT
 XLATITUDE=$latitude   ,
XLONGITUDE=$longitude
/
EOF`
else
   hls_nest=`echo $model | tr [A-Z] [a-z]`
fi

#export hls_nest=hs02
echo "${hls_nest} $PDY $cyc $run_id $model"
