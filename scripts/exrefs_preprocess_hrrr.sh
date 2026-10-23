#!/bin/ksh
# Name of Script:  exrefs_preprocess_hrrr.sh
#
# This script :
# 1) executes the enspost_preprocess_hrrr_1h.sh ush script for hours 00-48
# 2) executes the enspost_preprocess_hrrr_3hapcp.sh ush script for additional QPF/snow processing
# 3) The end result is preprocessed HRRR data ready to be ingested by the ensprod and eas codes.
#
#
# Author: Matthew Pyle, NCEP/EMC, 05/02/2019
# Updated 2022(?) for REFS purposes, largely by Jun Du.
#         
###########################################################
set -x

cd $DATA

export fhr=$1

echo "$0 STRDATE "`date`

msg="$job HAS BEGUN"
postmsg "$msg"


hrlist="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 \
21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 \
41 42 43 44 45 46 47 48"

region=${NEST}

echo defined region $region


if [ -e poe.hrrr ]
then
rm poe.hrrr
fi

for hr in $hrlist
do
echo "$USHrefs/enspost_preprocess_hrrr_1h.sh $PDY ${cyc} $hr ${region} " >> poe.hrrr
done

chmod 775 poe.hrrr

mpiexec -n $NTASK -ppn $PTILE --cpu-bind verbose,core cfp ./poe.hrrr
export err=$?; err_chk

# need to generate 3 h QPF

if [ -e poe.3hqpf ]
then
rm poe.3hqpf
fi

if [ -e poe.3hqpf.tlb ]
then
rm poe.3hqpf.tlb
fi

#

echo "$USHrefs/enspost_preprocess_hrrr_3hapcp.sh ${region} ${PDY} ${cyc} " >> poe.3hqpf

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
