#!/bin/ksh
###########################################################
set -x

cd $DATA

export fhr=$1

echo "$0 STRDATE "`date`

msg="$job HAS BEGUN"
postmsg "$msg"

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


#

$USHrefs/enspost_preprocess_hrrr_3hapcp.sh ${region} ${PDY} ${cyc} ${fhr}

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
