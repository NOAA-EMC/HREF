#!/bin/ksh
###########################################################
set -x

cd $DATA

export fhr=$1

echo "$0 STRDATE "`date`

msg="$job HAS BEGUN"
postmsg "$msg"


# hrlist="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
# 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 \
# 41 42 43 44 45 46 47 48"

region=${NEST}

echo defined region $region


if [ -e poe.hrrr ]
then
rm poe.hrrr
fi

echo "$USHrefs/enspost_preprocess_hrrr_1h.sh $PDY ${cyc} $fhr ${region}" >> poe.hrrr

chmod 775 poe.hrrr

mpiexec -n $NTASK -ppn $PTILE --cpu-bind verbose,core cfp ./poe.hrrr
export err=$?; err_chk

if [ $fhr -lt 10 ]
then
FHR1=$(printf %1.1i $((10#$fhr)) )
echo FHR1 is $FHR1
else
FHR1=$fhr
fi

# need to generate 3 h QPF
 if [ $FHR1 -gt 0 ]; then
 if (( 10#$hr%3 == 0 )); then

if [ -e poe.3hqpf ]
then
rm poe.3hqpf
fi

if [ -e poe.3hqpf.tlb ]
then
rm poe.3hqpf.tlb
fi

#

echo "$USHrefs/enspost_preprocess_hrrr_3hapcp.sh ${region} ${PDY} ${cyc} ${fhr}" >> poe.3hqpf

nproc=`cat poe.3hqpf | wc -l`

chmod 775 poe.3hqpf

mpiexec -n $nproc -ppn $nproc --cpu-bind verbose,core cfp ./poe.3hqpf
export err=$?; err_chk

if [ -e poe.3hqpf.tlb ]
then

nproc=`cat poe.3hqpf.tlb | wc -l`
chmod 775 poe.3hqpf.tlb
mpiexec -n $nproc -ppn $nproc --cpu-bind verbose,core cfp ./poe.3hqpf.tlb
export err=$?; err_chk

fi

 fi # three hourly
 fi # gt 0

# end QPF

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
