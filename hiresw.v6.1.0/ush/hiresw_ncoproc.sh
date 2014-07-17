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

if [ $outreg = "ak" ]
then
res="3km"
else
res="2p5"
fi

# COMBINE PRDGEN NDFD FILE AND SMARTINIT FILE into SMART output (if mksmart=1)


if [ $mksmart -eq 1 ]
then
cat $PRDGEN_DATA/${mdl}.t${cyc}z.ndfd${res}f${fhr}  MESO${RGUSE}${fhr}.tm00 >   ${mdl}.t${cyc}z.smart${outreg}f${fhr}
else
cp $PRDGEN_DATA/${mdl}.t${cyc}z.ndfd${res}f${fhr}                               ${mdl}.t${cyc}z.smart${outreg}f${fhr}
fi

rm $PRDGEN_DATA/${mdl}.t${cyc}z.ndfd${res}f${fhr}

# CREATE GRIB2 FILE

$utilexec/cnvgrib -g12 -p40 ${mdl}.t${cyc}z.smart${outreg}f${fhr}  ${mdl}.t${cyc}z.smart${outreg}f${fhr}.grib2

if [ $SENDCOM = YES ]
then
cp ${mdl}.t${cyc}z.smart${outreg}f${fhr} ${mdl}.t${cyc}z.smart${outreg}f${fhr}.grib2 $COMOUT
$utilexec/grbindex $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr} $COMOUT/${mdl}.t${cyc}z.smart${outreg}if${fhr}
$utilexec/wgrib2   $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr}.grib2 -s > $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr}.grib2.idx
export err=$?; err_chk
fi

if [ $SENDDBN = YES ]
then
  $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_SMART} $job  $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr}.grib2
  $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_SMART_WIDX}  $job $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr}.grib2.idx
fi
