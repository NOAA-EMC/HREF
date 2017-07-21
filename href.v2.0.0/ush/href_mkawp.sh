################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         href_mkawp.sh
# Script description:  To generate the AWIPS products for the HREF
#
# Author:      G Manikin /  EMC         Date: 2014-06-30
#
# Script history log:
# 2016-12-13  M Pyle - adapted for HREF
# 2014-06-30  G Manikin  - adapted for HRRR 
#################################################################################

set -xa

fhr=$1
NEST=${2}

fhr=$(printf "%02d" $fhr)

echo running awp script
pwd

runhrs="01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36"

# only every 3 h for off-time CONUS runs

if [ $NEST = "conus" ]
then
if [ $cyc -eq 06 -o $cyc -eq 18 ]
then
runhrs="03 06 09 12 15 18 21 24 27 30 33 36"
fi
fi

types="mean pmmn prob"

for type in $types
do

if  echo $runhrs |grep $fhr;
then
  # Processing AWIPS grid 227 

  ln -sf $MYCOMROOT/${NET}/${envir}/${RUN}.${PDY}/href.t${cyc}z.${NEST}.${type}.f${fhr}.grib2 .

  $GRBINDEX href.t${cyc}z.${NEST}.${type}.f${fhr}.grib2 href.t${cyc}z.${NEST}.${type}.f${fhr}.grib2i 
  export pgm=tocgrib2
  . prep_step
  startmsg

  export FORTREPORTS=unit_vars=yes 
  export FORT11=href.t${cyc}z.${NEST}.${type}.f${fhr}.grib2
  export FORT12=href.t${cyc}z.${NEST}.${type}.f${fhr}.grib2i
  export FORT51=xtrn.${cycle}.href.${NEST}_${type}_${fhr}
  $TOCGRIB2 <$PARMutil/grib2_awips_href_${NEST}_${type}f${fhr} parm='KWBH'
  err=$?;export err ;err_chk

  if test "$SENDCOM" = 'YES'
  then
    cp xtrn.${cycle}.href.${NEST}_${type}_${fhr} $PCOM/grib2.t${cyc}z.awphref_${NEST}_${type}_f${fhr}_${cyc}
  fi

#  if test "$SENDDBN" = 'YES'
#  then
##    $DBNROOT/bin/dbn_alert MODEL NTC_LOW${ALERT_EXT} $job $PCOM/grib2.${cycle}.awphref227_f${fhr}_${cyc} 
#    $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $PCOM/grib2.${cycle}.awphref227_f${fhr}_${cyc} 
#  fi


fi

done
