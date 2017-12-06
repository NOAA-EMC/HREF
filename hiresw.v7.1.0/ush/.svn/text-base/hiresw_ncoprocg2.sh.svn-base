#!/bin/ksh
#=======================================================================
# Script developed by NCO to :
# bsm - add processing for conversion to grib2 and awips files
# called by : smartinit.sh
# INPUT:
#  RGIN :  2 letter Region id (eg: CS, HI, PR,AK..)
#  cyc  :  UTC run cycle
#  fhr  :  Forecasts hour
#  ogrd :  output NDFD grid number (eg: 197,196,195,198...)
#  outreg  :  output file region name (eg: conus,ak,pr,hi,conus2p5,ak3
#
#  Modified 2014-04-01 by M. Pyle - modifications for HiresW system
#
#=======================================================================

REGCP=`echo $outreg |tr '[a-z]'  '[A-Z]' `
echo BEGIN NCO sminit Post-Processing for REG $RGIN $outreg $ogrd CYC $cyc FHR $fhr 

RGUSE=`echo $RGIN | cut -c1-4`

if [ $outreg = "ak" -o $outreg = "akmem2" ]
then
res="3km"
else
res="2p5km"
fi

# COMBINE PRDGEN NDFD FILE AND SMARTINIT FILE into SMART output (if mksmart=1)


if [ $mksmart -eq 1 ]
then
cat $PRDGEN_DATA/hiresw.t${cyc}z.${MODEL}_${res}.f${fhr}.${NEST}.grib2  smartg2.${fhr}  >   hiresw.t${cyc}z.${MODEL}_${res}.f${fhr}.${NEST}.grib2
else
cp  $PRDGEN_DATA/hiresw.t${cyc}z.${MODEL}_${res}.f${fhr}.${NEST}.grib2                      hiresw.t${cyc}z.${MODEL}_${res}.f${fhr}.${NEST}.grib2
fi

# rm $PRDGEN_DATA/${mdl}.t${cyc}z.ndfd${res}f${fhr}

if [ $SENDCOM = YES ]
then
cp hiresw.t${cyc}z.${MODEL}_${res}.f${fhr}.${NEST}.grib2  $COMOUT
$WGRIB2   $COMOUT/hiresw.t${cyc}z.${MODEL}_${res}.f${fhr}.${NEST}.grib2 -s > $COMOUT/hiresw.t${cyc}z.${MODEL}_${res}.f${fhr}.${NEST}.grib2.idx
export err=$?; err_chk
fi

if [ $SENDDBN = YES ]
then
  $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_SMART} $job  $COMOUT/hiresw.t${cyc}z.${MODEL}_${res}.f${fhr}.${NEST}.grib2
  $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_SMART_WIDX}  $job $COMOUT/hiresw.t${cyc}z.${MODEL}_${res}.f${fhr}.${NEST}.grib2.idx
fi
