#!/bin/ksh
# This script runs 1 scripts
#  (1) rrfs_getmbr.sh to get soft link for RRFS, RRFSens, and HRRR data as needed
# Arguments: fhr
# Author: Binbin Zhou, EMC/NCEP, 02/02/2015
#         Matthew Pyle, 9 April 2015 - eliminated most arguments in USH calls
#         Matthew Pyle, 29 Mar 2016 - new script to allow the href_getmbr.sh to be separately
#                                     poescript submitted from main job.
#         Jun Du, 03/21/2023 - All names have been following refs_enspost or enspost convention
#         
###########################################################
set -x


cd $DATA

echo "$0 STRDATE "`date`

msg="$job HAS BEGUN"
postmsg "$msg"

location=`hostname`

echo running on $location

export fhr=${1}


# clean up old if needed

if [ -e ./${fhr}/ ]
then
rm -rf *.f${fhr}*
fi

typeset -Z2 fhrm1
typeset -Z2 fhrm2

if [ $fhr -gt 2 ]
then
let fhrm1=fhr-1
let fhrm2=fhr-2
echo have fhrm1 as $fhrm1

looplim=50
loop=1
sleeptime=30

while [ $loop -lt $looplim -a ! -e $DATA/ensprepstart.${fhrm1} ]
do
echo missing previous hour
sleep $sleeptime
let loop=loop+1
done

loop=1
while [ $loop -lt $looplim -a ! -e $DATA/ensprepdone.${fhrm2} ]
do
echo two hours previous did not complete yet
sleep $sleeptime
let loop=loop+1
done

if [ ! -e $DATA/ensprepstart.${fhrm1} -o ! -e $DATA/ensprepdone.${fhrm2} ]
then
echo "NEVER FOUND PREVIOUS HOURS ENSPROD PREP, so quitting"
exit 99
fi

fi

#
# Get members or their softlinks
#

echo NEST is $NEST

echo "DONE" > $DATA/ensprepstart.${fhr}

$USHrefs/enspost_getmbr.sh $fhr $NEST 
err=$?

if [ $err -eq 0 ]
then
 echo "DONE" > $DATA/ensprepdone.${fhr}
fi

####################################################################r
# GOOD RUN
set +x
echo "**************$job COMPLETED NORMALLY on `date`"
set -x
#####################################################################

msg="HAS COMPLETED NORMALLY!"
echo $msg
postmsg  "$msg"

############## END OF SCRIPT #######################
