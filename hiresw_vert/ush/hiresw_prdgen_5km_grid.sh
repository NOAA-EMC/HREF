#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:          hiresw_prdgen_5km_grid.sh
# Script description:   Interpolates CONUS domain output to grid 227
#
#
# Author:        Matthew Pyle       Org: NP22         Date: 2014-02-11
#
# Abstract:             Only run for the CONUS domain, it provides full CONUS
#                       output of a large set of fields at 5 km.
#
# Script history log:
# 2013-11-01  Matthew Pyle - Original script for parallel
# 2014-02-11  Matthew Pyle - documentation block and cleanup



set -x

utilexec=/nwprod/util/exec

export CNVGRIB=/nwprod/util/exec/cnvgrib
export WGRIB2=/nwprod/util/exec/wgrib2

fhr=$1
DOMIN_SMALL=$2
CYC=${3}
model=$4
MEMBER=$5

reflag=1

mkdir ${DATA}/prdgen_5km_${fhr}
cd ${DATA}/prdgen_5km_${fhr}/
sh $utilscript/setup.sh

DOMIN=${DOMIN_SMALL}${model}

modelout=$model
if [ $model = "arw" ]
then
modelout="arw"
reflag=0
# modelout="em"
fi

DOMOUT=${DOMIN_SMALL}${modelout}

if [ $DOMIN = "conusnmmb" ]
then
  filenamthree="wrf.CONUS05"
  DOMIN_bucket="general"
  IM=1473
  JM=1025
fi

if [ $DOMIN = "conusarw" ]
then
  filenamthree="wrf.EMCONUS05"
  DOMIN_bucket="general"
  IM=1473
  JM=1025
fi

#echo $model > lower
#MODEL=`cat lower | tr '[a-z]' '[A-Z]'`

filedir=$DATA

export fhr
export tmmark=tm00


###############################################################
###############################################################
###############################################################

#
# make GRIB file with pressure data every 25 mb for EMC's FVS
# verification

ls -l $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_5km

if [ $DOMIN_SMALL = "conus" ]
then
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_5km_g227 master${fhr}.ctl
else
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_5km master${fhr}.ctl
fi

cat >input${fhr}.prd <<EOF5
$DATA/post_${fhr}/WRFPRS${fhr}.tm00
EOF5

rm fort.*

export pgm=hiresw_prdgen  ;. ./prep_step


if [ $DOMIN_SMALL = "conus" ]
then
export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g227"
else
export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g255_5km"
fi

export FORT10="master${fhr}.ctl"

echo EXECUTING hiresw_prdgen  for 5 km

export FORT11="input${fhr}.prd"
export FORT621="input${fhr}.prd"
$EXEChiresw/hiresw_prdgen  > prdgen.out${fhr}_5km 2>errfile_5km

export err=$?;./err_chk

### cp $DATA/post/WRFPRS${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.wrfprs${fhr}.tm00

###############################################################
###############################################################
###############################################################


# compute precip buckets

threehrprev=`expr $fhr - 3`
onehrprev=`expr $fhr - 1`

if [ $threehrprev -lt 10 ]
then
threehrprev=0$threehrprev
fi


if [ $onehrprev -lt 10 ]
then
onehrprev=0$onehrprev
fi

echo "to f00 test"

if [ $fhr -eq 00 ]
then
echo "inside f00 test"

  ###############################
  # Convert to grib2 format
  ###############################

  if test $SENDCOM = 'YES'
  then
      cp ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}
      $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awp5kmf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awp5kmif${fhr}
      $CNVGRIB -g12 -p40 ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2
      $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2.idx
      if [ $SENDDBN_GB2 = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2 $job $COMOUT/$DOMIN.t${CYC}z.awp5kmf${fhr}.grib2
         $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2_WIDX $job $COMOUT/$DOMIN.t${CYC}z.awp5kmf${fhr}.grib2.idx
      fi
  fi

else

### do precip buckets if model is ARW

while [ ! -e $DATA/prdgen_5km_${onehrprev}/$filenamthree$onehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_5km_${onehrprev}/$filenamthree$onehrprev.tm00
sleep 10
done


  rm PCP1HR${fhr}.tm00
  rm input.card
  echo "$DATA/prdgen_5km" > input.card
  echo $filenamthree >> input.card
  echo $onehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

 $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
 export err=$?;./err_chk

  if [ $model = "arw" ] ; then

  if [ $fhr%3 -eq 0 ]
  then

echo "3 hourly, do 3H precip bucket"

while [ ! -e $DATA/prdgen_5km_${threehrprev}/$filenamthree$threehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_5km_${threehrprev}/$filenamthree$threehrprev.tm00
sleep 10
done


  rm PCP3HR${fhr}.tm00
  rm input.card
  echo "$DATA/prdgen_5km" > input.card
  echo $filenamthree >> input.card
  echo $threehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

 $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
 export err=$?;./err_chk

  fi

ls -l PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00
ls -l ${filenamthree}${fhr}.tm00

#  cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00 PCP6HR${fhr}.tm00 > $DOMOUT.t${CYC}z.awp5kmf${fhr}
  cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00  > $DOMOUT.t${CYC}z.awp5kmf${fhr}

  else

## model = "nmm"
   cat ${filenamthree}${fhr}.tm00  PCP1HR${fhr}.tm00  > $DOMOUT.t${CYC}z.awp5kmf${fhr}

  fi

###### DONE PRECIP BUCKET

  if test $SENDCOM = 'YES'
  then
    cp $DOMOUT.t${CYC}z.awp5kmf${fhr} $COMOUT/.
    $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awp5kmf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awp5kmif${fhr}
    $CNVGRIB -g12 -p40 $DOMOUT.t${CYC}z.awp5kmf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2
    $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2.idx
    if [ $SENDDBN_GB2 = YES ]; then
       $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2 $job $COMOUT/$DOMIN.t${CYC}z.awp5kmf${fhr}.grib2
       $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2_WIDX $job $COMOUT/$DOMIN.t${CYC}z.awp5kmf${fhr}.grib2.idx
    fi
  fi
fi
