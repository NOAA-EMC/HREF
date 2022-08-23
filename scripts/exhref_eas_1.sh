set -x


cd $DATA

# export fhr=$1

echo "$0 STRDATE "`date`

msg="$job HAS BEGUN"
postmsg "$jlogfile" "$msg"

location=`hostname`

echo running on $location

if [ -e poe.qpf_6 ] 
then
rm poe.*
fi

hrs="24 27 30 33 36"
for fhr in $hrs
do
echo "cd ${DATA}; mkdir qpf_${fhr}_24_low; cd qpf_${fhr}_24_low ; python ${USHhref}/make_easfracqpf_combo.py ${fhr} 240 >  qpf_combo_${dom}_${cyc}_24h_low_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir qpf_${fhr}_24_med; cd qpf_${fhr}_24_med ; python ${USHhref}/make_easfracqpf_combo.py ${fhr} 241 >  qpf_combo_${dom}_${cyc}_24h_med_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir qpf_${fhr}_24_high; cd qpf_${fhr}_24_high ; python ${USHhref}/make_easfracqpf_combo.py ${fhr} 242 >  qpf_combo_${dom}_${cyc}_24h_high_${fhr}.log" >> poe.qpf_snow

done

hrs="06 09 12 15 18 21 24 27 30 33 36"
for fhr in $hrs
do
echo "cd ${DATA}; mkdir qpf_${fhr}_6; cd qpf_${fhr}_6 ; python ${USHhref}/make_easfracqpf_combo.py ${fhr} 6 > qpf_combo_${dom}_${cyc}_6h_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir snow_${fhr}_6; cd snow_${fhr}_6; python ${USHhref}/make_easfracsnow_combo.py ${fhr} 6 > snow_combo_${dom}_${cyc}_6h_${fhr}.log" >> poe.qpf_snow
done

hrs="12 15 18 21 24 27 30 33 36"
for fhr in $hrs
do
echo "cd ${DATA}; mkdir qpf_${fhr}_12_low; cd qpf_${fhr}_12_low ; python ${USHhref}/make_easfracqpf_combo.py ${fhr} 120 >  qpf_combo_${dom}_${cyc}_12h_low_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir qpf_${fhr}_12_med; cd qpf_${fhr}_12_med ; python ${USHhref}/make_easfracqpf_combo.py ${fhr} 121 >  qpf_combo_${dom}_${cyc}_12h_med_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir qpf_${fhr}_12_high; cd qpf_${fhr}_12_high ; python ${USHhref}/make_easfracqpf_combo.py ${fhr} 122 >  qpf_combo_${dom}_${cyc}_12h_high_${fhr}.log" >> poe.qpf_snow
done


hrs="03 06 09 12 15 18 21 24 27 30 33 36"
for fhr in $hrs
do
echo "cd ${DATA}; mkdir qpf_${fhr}_3; cd qpf_${fhr}_3 ; python ${USHhref}/make_easfracqpf_combo.py ${fhr} 3  > qpf_combo_${dom}_${cyc}_3h_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir snow_${fhr}_3; cd snow_${fhr}_3; python ${USHhref}/make_easfracsnow_combo.py ${fhr} 3 > snow_combo_${dom}_${cyc}_3h_${fhr}.log" >> poe.qpf_snow
done

hrs="01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36"
for fhr in $hrs
do
echo "cd ${DATA}; mkdir qpf_${fhr}_1; cd qpf_${fhr}_1 ; python ${USHhref}/make_easfracqpf_combo.py ${fhr} 1 > qpf_combo_${dom}_${cyc}_1h_${fhr}.log" >> poe.qpf_snow
echo "cd ${DATA}; mkdir snow_${fhr}_1; cd snow_${fhr}_1; python ${USHhref}/make_easfracsnow_combo.py ${fhr} 1 >  snow_combo_${dom}_${cyc}_1h_${fhr}.log" >> poe.qpf_snow
done


chmod 775 poe.qpf_snow

#mpiexec -n $NTASK -ppn $PTILE --cpu-bind verbose,core cfp ./poe.qpf_snow
#mpiexec -np $NTASK --cpu-bind verbose,depth cfp ./poe.qpf_snow
mpiexec -np $(($NTASK*$NODES)) --cpu-bind verbose,depth cfp ./poe.qpf_snow

export err=$?; err_chk


if [ $err -eq 0 ]
then

fhr=01

while [ $fhr -le 36 ]
do

if [ ! -e $DATA/href.t${cyc}z.${dom}.pqpf01_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/href.t${cyc}z.${dom}.pqpf01_easfrac.f${fhr}.grib2"
err_exit $msg
fi
if [ ! -e $DATA/href.t${cyc}z.${dom}.snow01_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/href.t${cyc}z.${dom}.snow01_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/href.t${cyc}z.${dom}.pqpf01_easfrac.f${fhr}.grib2 > $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/href.t${cyc}z.${dom}.snow01_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2

if [ $((fhr%3)) -eq 0 ]
then

if [ ! -e $DATA/href.t${cyc}z.${dom}.pqpf03_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/href.t${cyc}z.${dom}.pqpf03_easfrac.f${fhr}.grib2"
err_exit $msg
fi

if [ ! -e $DATA/href.t${cyc}z.${dom}.snow03_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/href.t${cyc}z.${dom}.snow03_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/href.t${cyc}z.${dom}.pqpf03_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/href.t${cyc}z.${dom}.snow03_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2

fi

if [ $((fhr%3)) -eq 0 -a $fhr -ge 6 ]
then

if [ ! -e $DATA/href.t${cyc}z.${dom}.pqpf06_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/href.t${cyc}z.${dom}.pqpf06_easfrac.f${fhr}.grib2"
err_exit $msg
fi

if [ ! -e $DATA/href.t${cyc}z.${dom}.snow06_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/href.t${cyc}z.${dom}.snow06_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/href.t${cyc}z.${dom}.pqpf06_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/href.t${cyc}z.${dom}.snow06_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2

fi

if [ $((fhr%3)) -eq 0 -a $fhr -ge 12 ]
then

if [ ! -e $DATA/href.t${cyc}z.${dom}.pqpf12low_easfrac.f${fhr}.grib2 -o \
     ! -e $DATA/href.t${cyc}z.${dom}.pqpf12med_easfrac.f${fhr}.grib2 -o \
     ! -e $DATA/href.t${cyc}z.${dom}.pqpf12high_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/href.t${cyc}z.${dom}.pqpf12(low|med|high)_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/href.t${cyc}z.${dom}.pqpf12low_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/href.t${cyc}z.${dom}.pqpf12med_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/href.t${cyc}z.${dom}.pqpf12high_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
fi

if [ $fhr -eq 24 -o $fhr -eq 27 -o $fhr -eq 30 -o $fhr -eq 33 -o $fhr -eq 36 ]
then

if [ ! -e $DATA/href.t${cyc}z.${dom}.pqpf24low_easfrac.f${fhr}.grib2 -o \
     ! -e $DATA/href.t${cyc}z.${dom}.pqpf24med_easfrac.f${fhr}.grib2 -o \
     ! -e $DATA/href.t${cyc}z.${dom}.pqpf24high_easfrac.f${fhr}.grib2 ]
then
msg="FATAL ERROR: missing $DATA/href.t${cyc}z.${dom}.pqpf24(low|med|high)_easfrac.f${fhr}.grib2"
err_exit $msg
fi

cat $DATA/href.t${cyc}z.${dom}.pqpf24low_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/href.t${cyc}z.${dom}.pqpf24med_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
cat $DATA/href.t${cyc}z.${dom}.pqpf24high_easfrac.f${fhr}.grib2 >> $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
fi

if [ $SENDCOM == 'YES' ]
then
cp $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2 ${COMOUT}/ensprod/
## $WGRIB2 $DATA/href.t${cyc}z.${dom}.eas.f${fhr}.grib2 -match APCP -grib ${COMOUT}/ensprod/href.t${cyc}z.${dom}.easffair.f${fhr}.grib2
$WGRIB2 ${COMOUT}/ensprod/href.t${cyc}z.${dom}.eas.f${fhr}.grib2 -s > ${COMOUT}/ensprod/href.t${cyc}z.${dom}.eas.f${fhr}.grib2.idx
fi

if [ $SENDDBN = YES ]; then
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job ${COMOUT}/ensprod/href.t${cyc}z.${dom}.eas.f${fhr}.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_WIDX $job ${COMOUT}/ensprod/href.t${cyc}z.${dom}.eas.f${fhr}.grib2.idx
fi

let fhr=fhr+1

if [ $fhr -lt 10 ]
then
fhr=0${fhr}
fi

done

fi

#####################################################################
# GOOD RUN
set +x
echo "**************$job COMPLETED NORMALLY on `date`"
set -x
#####################################################################

msg="HAS COMPLETED NORMALLY!"
echo $msg
postmsg "$jlogfile" "$msg"
echo $msg
