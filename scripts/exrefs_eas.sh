#! /bin/bash

set -x

# Name of Script: exrefs_eas.sh
# This script runs multiple instances of ush python scripts to
# generate EAS probabilities
#
# 
# Argument: forecast hour (fhr) passed in from ecflow task
#
# Author: Matthew Pyle, EMC/NCEP, 2025
#
###########################################################

cd $DATA

#export fhr=$1

echo "$0 STRDATE "`date`

msg="$job HAS BEGUN"
postmsg "$msg"

location=`hostname`

echo running on $location

echo have fhr in exrefs_eas.sh $fhr

if [ -e poe.qpf_snow_${fhr} ]
then
rm -f ./poe.qpf_snow_${fhr}
fi

if [ $((10#$fhr)) -ge 24 -a $((10#$fhr%3)) -eq 0 ]
then
echo "cd ${DATA}; mkdir qpf_${fhr}_24; cd qpf_${fhr}_24 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py ${fhr} 24 >  qpf_combo_${dom}_${cyc}_24h_${fhr}.log" >> poe.qpf_snow_${fhr}
fi

if [ $((10#$fhr)) -ge 6 -a $((10#$fhr%3)) -eq 0 ]
then
echo "cd ${DATA}; mkdir qpf_${fhr}_6; cd qpf_${fhr}_6 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py ${fhr} 6 > qpf_combo_${dom}_${cyc}_6h_${fhr}.log" >> poe.qpf_snow_${fhr}
echo "cd ${DATA}; mkdir snow_${fhr}_6; cd snow_${fhr}_6; python ${USHrefs}/enspost_make_easfracsnow_combo.py ${fhr} 6 > snow_combo_${dom}_${cyc}_6h_${fhr}.log" >> poe.qpf_snow_${fhr}
fi


# need full list for this 12 h product (all 3 hourly from 12 onward)
if [ $((10#$fhr)) -ge 12 -a $((10#$fhr%3)) -eq 0 ]
then

echo "cd ${DATA}; mkdir qpf_${fhr}_12; cd qpf_${fhr}_12 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py ${fhr} 12 >  qpf_combo_${dom}_${cyc}_12h_${fhr}.log" >> poe.qpf_snow_${fhr}
fi


# need full list for this 3 h product (all 3 hourly from 3 onward)
if [ $((10#$fhr)) -ge 3 -a $((10#$fhr%3)) -eq 0 ]
then
echo "cd ${DATA}; mkdir qpf_${fhr}_3; cd qpf_${fhr}_3 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py ${fhr} 3  > qpf_combo_${dom}_${cyc}_3h_${fhr}.log" >> poe.qpf_snow_${fhr}
echo "cd ${DATA}; mkdir snow_${fhr}_3; cd snow_${fhr}_3; python ${USHrefs}/enspost_make_easfracsnow_combo.py ${fhr} 3 > snow_combo_${dom}_${cyc}_3h_${fhr}.log" >> poe.qpf_snow_${fhr}
fi

echo "cd ${DATA}; mkdir qpf_${fhr}_1; cd qpf_${fhr}_1 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py ${fhr} 1 > qpf_combo_${dom}_${cyc}_1h_${fhr}.log" >> poe.qpf_snow_${fhr}
echo "cd ${DATA}; mkdir snow_${fhr}_1; cd snow_${fhr}_1; python ${USHrefs}/enspost_make_easfracsnow_combo.py ${fhr} 1 >  snow_combo_${dom}_${cyc}_1h_${fhr}.log" >> poe.qpf_snow_${fhr}


chmod 775 poe.qpf_snow_${fhr}

nproc_loc=`cat poe.qpf_snow_${fhr} | wc -l`

echo nproc_loc $nproc_loc

mpiexec -n $nproc_loc -ppn $nproc_loc --cpu-bind verbose,core cfp ./poe.qpf_snow_${fhr}

export err=$?; err_chk

if [ ! -e $DATA/${RUN}.t${cyc}z.pqpf01_easfrac.f${fhr}.${dom}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.pqpf01_easfrac.f${fhr}.${dom}.grib2"
err_exit $msg
fi
if [ ! -e $DATA/${RUN}.t${cyc}z.snow01_easfrac.f${fhr}.${dom}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.snow01_easfrac.f${fhr}.${dom}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.pqpf01_easfrac.f${fhr}.${dom}.grib2 > $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2
cat $DATA/${RUN}.t${cyc}z.snow01_easfrac.f${fhr}.${dom}.grib2 >> $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2

if [ $((10#$fhr)) -ge 3 -a $((10#$fhr%3)) -eq 0 ]
then

if [ ! -e $DATA/${RUN}.t${cyc}z.pqpf03_easfrac.f${fhr}.${dom}.grib2  ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.pqpf03_easfrac.f${fhr}.${dom}.grib2"
err_exit $msg
fi

if [ ! -e $DATA/${RUN}.t${cyc}z.snow03_easfrac.f${fhr}.${dom}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.snow03_easfrac.f${fhr}.${dom}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.pqpf03_easfrac.f${fhr}.${dom}.grib2 >> $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2
cat $DATA/${RUN}.t${cyc}z.snow03_easfrac.f${fhr}.${dom}.grib2 >> $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2

fi

if [ $((10#$fhr)) -ge 6 -a $((10#$fhr%3)) -eq 0 ]
then

if [ ! -e $DATA/${RUN}.t${cyc}z.pqpf06_easfrac.f${fhr}.${dom}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.pqpf06_easfrac.f${fhr}.${dom}.grib2"
err_exit $msg
fi

if [ ! -e $DATA/${RUN}.t${cyc}z.snow06_easfrac.f${fhr}.${dom}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.snow06_easfrac.f${fhr}.${dom}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.pqpf06_easfrac.f${fhr}.${dom}.grib2 >> $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2
cat $DATA/${RUN}.t${cyc}z.snow06_easfrac.f${fhr}.${dom}.grib2 >> $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2

fi

if [ $((10#$fhr)) -ge 12 -a $((10#$fhr%3)) -eq 0 ]
then

if [ ! -e $DATA/${RUN}.t${cyc}z.pqpf12_easfrac.f${fhr}.${dom}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.pqpf12_easfrac.f${fhr}.${dom}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.pqpf12_easfrac.f${fhr}.${dom}.grib2 >> $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2
fi

if [ $((10#$fhr)) -ge 24 -a $((10#$fhr%3)) -eq 0 ]
then

if [ ! -e $DATA/${RUN}.t${cyc}z.pqpf24_easfrac.f${fhr}.${dom}.grib2  ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.pqpf24_easfrac.f${fhr}.${dom}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.pqpf24_easfrac.f${fhr}.${dom}.grib2 >> $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2
fi

if [ $SENDCOM == 'YES' ]
then
cpreq $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2 ${COMOUT}/ensprod/
## $WGRIB2 $DATA/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2 -match APCP -grib ${COMOUT}/ensprod/${RUN}.t${cyc}z.easffair.f${fhr}.${dom}.grib2
$WGRIB2 ${COMOUT}/ensprod/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2 -s > ${COMOUT}/ensprod/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2.idx
fi

if [ $SENDDBN = YES ]; then
  $DBNROOT/bin/dbn_alert MODEL REFS_GB2 $job ${COMOUT}/ensprod/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2
  $DBNROOT/bin/dbn_alert MODEL REFS_GB2_WIDX $job ${COMOUT}/ensprod/${RUN}.t${cyc}z.eas.f${fhr}.${dom}.grib2.idx
fi

if [ ! -d $COMOUT/log/eas ] ; then
  mkdir -p $COMOUT/log/eas
fi
cp -p $DATA/*/qpf_combo*log $COMOUT/log/eas/.
cp -p $DATA/*/snow_combo*log $COMOUT/log/eas/.

#####################################################################
# GOOD RUN
set +x
echo "**************$job COMPLETED NORMALLY on `date`"
set -x
#####################################################################

msg="HAS COMPLETED NORMALLY!"
echo $msg
postmsg "$msg"
echo $msg
