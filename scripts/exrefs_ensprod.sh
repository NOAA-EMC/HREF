#!/bin/ksh
# Name of Script: exrrfs_runall.sh.sms 
# This script runs 1 USH script:
#  (1) rrfs_ensprod.sh to get final ensemble products 
# Arguments: cycle
# Author: Binbin Zhou, EMC/NCEP, 02/02/2015
#         Matthew Pyle, 9 April 2015 - eliminated most arguments in USH calls
#         Matthew Pyle, 29 Mar 2016 - removed call to linking script from this script
#         Jun Du: 03/21/2023 - All names have been changed from href to 
#                              rrfs_enspost or enspost
#         
###########################################################
set -x


cd $DATA

export fhr=$1

echo "$0 STRDATE "`date`

msg="$job HAS BEGUN"
postmsg "$msg"

location=`hostname`

echo running on $location

#
# Prepare to run ensemble product generator
#

echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 1" > ./poe.${fhr}
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 2" >> ./poe.${fhr}
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 3" >> ./poe.${fhr}
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 4" >> ./poe.${fhr}
chmod 775 ${DATA}/poe.${fhr}
mpiexec -n 4 -ppn 4 --cpu-bind verbose,core cfp ${DATA}/poe.${fhr}
export err=$?; err_chk

if [ $err -eq 0 ] 
then

	# copy to COMOUT
	
if [ $NEST = 'conus' ]
then
types="mean pmmn avrg prob sprd lpmm ffri"
else
types="mean pmmn avrg prob sprd lpmm"
fi

if [ $SENDCOM = YES ]; then
subtypes="1 2 3 4"
for subtype in $subtypes
do
 for typ in $types
 do
  if [ -s $DATA/$fhr/${subtype}/${RUN}.${typ}.t${cyc}z.f$fhr ]
  then
  cpreq $DATA/$fhr/${subtype}/${RUN}.${typ}.t${cyc}z.f$fhr  $DATA/${fhr}/${RUN}.t${cyc}z.${NEST}.${typ}.f$fhr.grib2_${subtype}
  fi
 done
done

for typ in $types
do
files=`ls $DATA/${fhr}/${RUN}.t${cyc}z.${NEST}.${typ}.f$fhr.grib2_?`
cat ${files} > $COMOUT/ensprod/${RUN}.t${cyc}z.${NEST}.${typ}.f$fhr.grib2
err=$?; err_chk

$WGRIB2 $COMOUT/ensprod/${RUN}.t${cyc}z.${NEST}.${typ}.f$fhr.grib2  -s >  $COMOUT/ensprod/${RUN}.t${cyc}z.${NEST}.${typ}.f$fhr.grib2.idx
err=$?; err_chk
done

# add verf_g2g and dbnet stuff here

if [ ${fhr}%3 -eq 0 ]
then

  if [ ! -e $COMOUT/verf_g2g ]
  then
   msg="FATAL ERROR: no $COMOUT/verf_g2g directory to copy member files to" 
   err_exit $msg
  fi


mems="01 02 03 04 05 06 07 08 09 10 11 12 13 14"

for m in $mems
do

if [ -e $DATA/refs.m${m}.t${cyc}z.f${fhr} ]
then
cp -d $DATA/refs.m${m}.t${cyc}z.f${fhr}  $COMOUT/verf_g2g/refs.m${m}.t${cyc}z.${NEST}.f${fhr}
fi

if [ -e $DATA/prcip.m${m}.t${cyc}z.f${fhr} ]
then
cp -d $DATA/prcip.m${m}.t${cyc}z.f${fhr} $COMOUT/verf_g2g/prcip.m${m}.t${cyc}z.${NEST}.f${fhr}
fi

cp -d $DATA/${fhr}/1/filename              $COMOUT/verf_g2g/filename.t${cyc}z.${NEST}.f${fhr}

done

fi # 3 hourly for verf_g2g

if [ $SENDDBN = YES ]; then
 for typ in $types
 do
  $DBNROOT/bin/dbn_alert MODEL REFS_GB2 $job $COMOUT/ensprod/${RUN}.t${cyc}z.${dom}.${typ}.f${fhr}.grib2
  $DBNROOT/bin/dbn_alert MODEL REFS_GB2_WIDX $job $COMOUT/ensprod/${RUN}.t${cyc}z.${dom}.${typ}.f${fhr}.grib2.idx
 done
fi

fi # SENDCOM

fi # err=0


#####################################################################
# GOOD RUN
set +x
echo "**************$job COMPLETED NORMALLY on `date`"
set -x
#####################################################################

msg="HAS COMPLETED NORMALLY!"
echo $msg
postmsg  "$msg"
echo $msg
############## END OF SCRIPT #######################
