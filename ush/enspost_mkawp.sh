#! /bin/bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         enspost_mkawp.sh
# Script description:  To generate the WMO products for the RRFS ensemble prods
#
# Author:      G Manikin /  EMC         Date: 2014-06-30
#
# Script history log:
# 2014-06-30  G Manikin  - adapted for HRRR 
# 2016-12-13  M Pyle - adapted for HREF
# 2023-04-01  J Du - adopted for RRFS Ensemble
# 2023-05-04   J Du - added an option for time-lag ensemble
# 2024-0301   M Pyle - shifted from rrfs to $RUN (now REFS)
# 2025-12  M Pyle - reduced to every 3 h to 48, then 6 hourly
#################################################################################

set -xa

dom=${1}

DBNDOM="${dom^^}"

type=${2}

runhrs="03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 54 60"

looplim=90
sleeptime=15

types="mean pmmn prob"

for fhr in $runhrs
do

for type in $types
do


if [ $type = "mean" ]
then
  if [ $dom = "conus" ]
  then
   alttype="ffri"
  else
   alttype="mean"
  fi
fi

if [ $type = "pmmn" ]
then
alttype="lpmm"
fi

if [ $type = "prob" ]
then
alttype="eas"
fi


loop=0
while [ ! -e ${COMIN}/${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2 -a $loop -lt $looplim ]
do
         echo waiting on ${COMIN}/${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2
         sleep ${sleeptime}
         let loop=loop+1
done

loop=0
while [ ! -e ${COMIN}/${RUN}.t${cyc}z.${alttype}.f${fhr}.${dom}.grib2 -a $loop -lt $looplim ]
do
         echo waiting on ${COMIN}/${RUN}.t${cyc}z.${alttype}.f${fhr}.${dom}.grib2
         sleep ${sleeptime}
         let loop=loop+1
done

if [ ! -e ${COMIN}/${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2  -o ! -e ${COMIN}/${RUN}.t${cyc}z.${alttype}.f${fhr}.${dom}.grib2 ]
then
         msg="FATAL ERROR: ${COMIN}/${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2 or ${COMIN}/${RUN}.t${cyc}z.${alttype}.f${fhr}.${dom}.grib2 missing but required"
         err_exit $msg
fi

  if [ $type = "prob" ]
  then
  cpfs ${COMIN}/${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2 .
# also want EAS prob
  cpfs ${COMIN}/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2 .
# want FFRI prob for conus
  if [ $dom = "conus" ]
  then
    cpfs ${COMIN}/${RUN}.t${cyc}z.ffri.f${fhr}.${dom}.grib2 .
    cat ${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2 ${RUN}.t${cyc}z.ffri.f${fhr}.${dom}.grib2 >> ${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2
  else
    cat ${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2  >> ${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2
  fi

#avoid  cat ${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2 ${RUN}.t${cyc}z.ffri.f${fhr}.${dom}.grib2 >> ${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2

  elif [ $type = "pmmn" ]
  then
  cpfs ${COMIN}/${RUN}.t${cyc}z.lpmm.f${fhr}.${dom}.grib2 .
  cpfs ${COMIN}/${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2 .
  cat ${RUN}.t${cyc}z.lpmm.f${fhr}.${dom}.grib2 >> ${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2

  else
  ln -sf ${COMIN}/${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2 .
  fi

  $GRBINDEX ${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2 ${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2i 
  export pgm=tocgrib2
  . prep_step
  startmsg

  export FORTREPORTS=unit_vars=yes 
  export FORT11=${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2
  export FORT12=${RUN}.t${cyc}z.${type}.f${fhr}.${dom}.grib2i
  export FORT51=xtrn.${cycle}.${RUN}.${dom}_${type}_${fhr}
  $TOCGRIB2 <$PARMwmo/grib2_${RUN}_${dom}_${type}f${fhr} parm='KWDB'
  err=$?;export err ;err_chk

  if test "$SENDCOM" = 'YES'
  then
    cpreq xtrn.${cycle}.${RUN}.${dom}_${type}_${fhr} $COMOUT/grib2.${RUN}.t${cyc}z.${type}.f${fhr}.${dom}
  fi

  if test "$SENDDBN_NTC" = 'YES'
  then
    $DBNROOT/bin/dbn_alert NTC_LOW REFS_ENSPOST_${DBNDOM} $job $COMOUT/grib2.${RUN}.t${cyc}z.${type}.f${fhr}.${dom}
  fi

done
done
