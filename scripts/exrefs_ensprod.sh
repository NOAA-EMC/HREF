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

if [ ${NEST} = "hi" -o ${NEST} = "pr" ]
then
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 1" > ./poe.${fhr}
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 2" >> ./poe.${fhr}
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 3" >> ./poe.${fhr}
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 4" >> ./poe.${fhr}
chmod 775 ${DATA}/poe.${fhr}
mpiexec -n 4 -ppn 4 --cpu-bind verbose,core cfp ${DATA}/poe.${fhr}

else

echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 1" > ./poe.${fhr}
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 2" >> ./poe.${fhr}
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 3" >> ./poe.${fhr}
echo "$USHrefs/enspost_ensprod_multi.sh ${NEST} 4" >> ./poe.${fhr}
chmod 775 ${DATA}/poe.${fhr}
mpiexec -n 4 -ppn 4 --cpu-bind verbose,core cfp ${DATA}/poe.${fhr}
err=$?

if [ $err -ne 0 ]
then
echo "some kind of failure with err $err"
fi

fi

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
