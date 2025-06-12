set -x

cd $DATA

#export fhr=$1
export type=$1

echo "$0 STRDATE "`date`

msg="$job HAS BEGUN"
postmsg "$msg"

location=`hostname`

echo running on $location

if [ -e poe.qpf_6 ] 
then
rm poe.*
fi

# ge 24 and mod(fhr%3) = 0
# hrs="24 27 30 33 36 39 42 45 48"
# need full list for this 24 h product (all 3 hourly from 24 onward?)
# for fhr in $hrs
# do

if [ $((10#$fhr)) -ge 24 -a $((10#$fhr%3)) -eq 0 ]
then
echo "cd ${DATA}; mkdir qpf_${fhr}_24; cd qpf_${fhr}_24 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py_${type} ${fhr} 24 >  qpf_combo_${dom}_${cyc}_24h_${fhr}.log" >> poe.qpf_snow
fi

# done

# hrs="06 09 12 15 18 21 24 27 30 33 36 39 42 45 48"
# ge 6 and mod(fhr%3) = 0
# need full list for this 6 h product (all 3 hourly from 6 onward?)
# for fhr in $hrs
# do

if [ $((10#$fhr)) -ge 6 -a $((10#$fhr%3)) -eq 0 ]
then
echo "cd ${DATA}; mkdir qpf_${fhr}_6; cd qpf_${fhr}_6 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py_${type} ${fhr} 6 > qpf_combo_${dom}_${cyc}_6h_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir snow_${fhr}_6; cd snow_${fhr}_6; python ${USHrefs}/enspost_make_easfracsnow_combo.py_${type} ${fhr} 6 > snow_combo_${dom}_${cyc}_6h_${fhr}.log" >> poe.qpf_snow
fi

# done

# need full list for this 12 h product (all 3 hourly from 12 onward?)
if [ $((10#$fhr)) -ge 12 -a $((10#$fhr%3)) -eq 0 ]
then

echo "cd ${DATA}; mkdir qpf_${fhr}_12; cd qpf_${fhr}_12 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py_${type} ${fhr} 12 >  qpf_combo_${dom}_${cyc}_12h_${fhr}.log" >> poe.qpf_snow
fi


# need full list for this 3 h product (all 3 hourly from 3 onward?)
if [ $((10#$fhr)) -ge 3 -a $((10#$fhr%3)) -eq 0 ]
then
echo "cd ${DATA}; mkdir qpf_${fhr}_3; cd qpf_${fhr}_3 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py_${type} ${fhr} 3  > qpf_combo_${dom}_${cyc}_3h_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir snow_${fhr}_3; cd snow_${fhr}_3; python ${USHrefs}/enspost_make_easfracsnow_combo.py_${type} ${fhr} 3 > snow_combo_${dom}_${cyc}_3h_${fhr}.log" >> poe.qpf_snow
fi

echo "cd ${DATA}; mkdir qpf_${fhr}_1; cd qpf_${fhr}_1 ; python ${USHrefs}/enspost_make_easfracqpf_combo.py_${type} ${fhr} 1 > qpf_combo_${dom}_${cyc}_1h_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir snow_${fhr}_1; cd snow_${fhr}_1; python ${USHrefs}/enspost_make_easfracsnow_combo.py_${type} ${fhr} 1 >  snow_combo_${dom}_${cyc}_1h_${fhr}.log" >> poe.qpf_snow


chmod 775 poe.qpf_snow

nproc_loc=`cat poe.qpf_snow | wc -l`

echo nproc_loc $nproc_loc

mpiexec -n $nproc_loc -ppn $nproc_loc --cpu-bind verbose,core cfp ./poe.qpf_snow

export err=$?; err_chk



if [ ! -e $DATA/${RUN}.t${cyc}z.${dom}.pqpf01_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.${dom}.pqpf01_easfrac.f${fhr}.grib2"
err_exit $msg
fi
if [ ! -e $DATA/${RUN}.t${cyc}z.${dom}.snow01_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.${dom}.snow01_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.${dom}.pqpf01_easfrac.f${fhr}.grib2 >> $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/${RUN}.t${cyc}z.${dom}.snow01_easfrac.f${fhr}.grib2 >> $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2

if [ $((10#$fhr)) -ge 3 -a $((10#$fhr%3)) -eq 0 ]
then

if [ ! -e $DATA/${RUN}.t${cyc}z.${dom}.pqpf03_easfrac.f${fhr}.grib2  ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.${dom}.pqpf03_easfrac.f${fhr}.grib2"
err_exit $msg
fi

if [ ! -e $DATA/${RUN}.t${cyc}z.${dom}.snow03_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.${dom}.snow03_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.${dom}.pqpf03_easfrac.f${fhr}.grib2 >> $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/${RUN}.t${cyc}z.${dom}.snow03_easfrac.f${fhr}.grib2 >> $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2

fi

if [ $((10#$fhr)) -ge 6 -a $((10#$fhr%3)) -eq 0 ]
then

if [ ! -e $DATA/${RUN}.t${cyc}z.${dom}.pqpf06_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.${dom}.pqpf06_easfrac.f${fhr}.grib2"
err_exit $msg
fi

if [ ! -e $DATA/${RUN}.t${cyc}z.${dom}.snow06_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.${dom}.snow06_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.${dom}.pqpf06_easfrac.f${fhr}.grib2 >> $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/${RUN}.t${cyc}z.${dom}.snow06_easfrac.f${fhr}.grib2 >> $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2

fi

if [ $((10#$fhr)) -ge 12 -a $((10#$fhr%3)) -eq 0 ]
then

if [ ! -e $DATA/${RUN}.t${cyc}z.${dom}.pqpf12_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.${dom}.pqpf12_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.${dom}.pqpf12_easfrac.f${fhr}.grib2 >> $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2
fi

if [ $((10#$fhr)) -ge 24 -a $((10#$fhr%3)) -eq 0 ]
then

if [ ! -e $DATA/${RUN}.t${cyc}z.${dom}.pqpf24_easfrac.f${fhr}.grib2 -]
then
msg="FATAL ERROR: missing $DATA/${RUN}.t${cyc}z.${dom}.pqpf24_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/${RUN}.t${cyc}z.${dom}.pqpf24_easfrac.f${fhr}.grib2 >> $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2
fi

if [ $SENDCOM == 'YES' ]
then
cp $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2 ${COMOUT}/ensprod/
## $WGRIB2 $DATA/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2 -match APCP -grib ${COMOUT}/ensprod/${RUN}.t${cyc}z.${dom}.easffair.f${fhr}.grib2
$WGRIB2 ${COMOUT}/ensprod/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2 -s > ${COMOUT}/ensprod/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2.idx
fi

if [ $SENDDBN = YES ]; then
  $DBNROOT/bin/dbn_alert MODEL RRFS_ENSPOST_GB2 $job ${COMOUT}/ensprod/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2
  $DBNROOT/bin/dbn_alert MODEL RRFS_ENSPOST_GB2_WIDX $job ${COMOUT}/ensprod/${RUN}.t${cyc}z.${dom}.eas.f${fhr}.grib2.idx
fi


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
