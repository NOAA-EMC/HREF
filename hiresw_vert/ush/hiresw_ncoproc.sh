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
#=======================================================================

# case $cyc in 
#   00|12) cyctp=on;;
#   06|18) cyctp=off;;
# esac

REGCP=`echo $outreg |tr '[a-z]'  '[A-Z]' `
echo BEGIN NCO sminit Post-Processing for REG $RGIN $outreg $ogrd CYC $cyc FHR $fhr 

RGUSE=`echo $RGIN | cut -c1-4`

# CREATE GRIB2 FILE
$utilexec/cnvgrib -g12 -p40 MESO${RGUSE}${fhr}.tm00 ${mdl}.t${cyc}z.smart${outreg}f${fhr}.grib2

# Processing grids for AWIPS
#  pgm=tocgrib2
#  export pgm; . prep_step
#  startmsg
# export FORTREPORTS=unit_vars=yes
# export FORT11=nam.t${cyc}z.smart${outreg}${fhr}.tm00.grib2 
# export FORT31="";
# export FORT51=grib2.t${cyc}z.smart${outreg}f${fhr}

# Define grib2 awips parm file 

# if [ $outreg = conus2p5 ];then
#   awpparm=$UTILparm/grib2_awpnamdngconus${cyctp}f${fhr}.${ogrd}
# elif [ $outreg = ak3 ];then
#   awpparm=$UTILparm/grib2_awpnamdngak${cyctp}f${fhr}.${ogrd}
# else
#   awpparm=$UTILparm/grib2_awpnamsmart${outreg}${cyctp}f${fhr}.${ogrd}
# fi

# if [ -s "$awpparm" ];then
#   $utilexec/tocgrib2 < ${awpparm} 1 >> $pgmout 2>> errfile
#   echo " error from tocgrib="  $err
# else 
#   echo AWP PARM FILE not found: $awpparm
# fi


mv MESO${RGUSE}${fhr}.tm00                      $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr}
mv ${mdl}.t${cyc}z.smart${outreg}f${fhr}.grib2 $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr}.grib2

# Move grib2 awips file to pcom
# if [ $outreg = ak3 ];then
#   mv grib2.t${cyc}z.smart${outreg}f${fhr} $pcom/grib2.awpnamsmart3.ak${fhr}_awips_f${fhr}_${cyc}
# else
#   mv grib2.t${cyc}z.smart${outreg}f${fhr} $pcom/grib2.awpnamsmart.${outreg}${fhr}_awips_f${fhr}_${cyc}
# fi

# if [ -s "$awpparm" ];then
#   if [ $SENDDBN = YES ];then #bsm 25 feb 2008 - added code for awips alerts
#     if [ $outreg = ak3 ];then
#       $DBNROOT/bin/dbn_alert NTC_LOW SMART${REGCP} $job $pcom/grib2.awpnamsmart3.ak${fhr}_awips_f${fhr}_${cyc}
#     else
#       $DBNROOT/bin/dbn_alert NTC_LOW SMART${REGCP} $job $pcom/grib2.awpnamsmart.${outreg}${fhr}_awips_f${fhr}_${cyc}
#     fi
#   fi
# fi

# if [ $SENDDBN_GB2 = YES ]
#  then
#   $DBNROOT/bin/dbn_alert MODEL NAM_SMART${REGCP}_GB2 $job $COMOUT/nam.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
# fi
