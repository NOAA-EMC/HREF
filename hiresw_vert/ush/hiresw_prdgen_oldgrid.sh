#! /bin/ksh

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
  filenamthree="wrf.EAST05"
  DOMIN_bucket="general"
  IM=884
  JM=614
elif [ $DOMIN = "aknmmb" ]
then
  filenamthree="wrf.AK05"
  DOMIN_bucket="general"
  IM=825
  JM=603
  wgt=ak
elif [ $DOMIN = "hinmmb" ]
then
  filenamthree="wrf.HI05"
  DOMIN_bucket="general"
  IM=223
  JM=170
  wgt=hi
elif [ $DOMIN = "prnmmb" ]
then
  filenamthree="wrf.PR05"
  DOMIN_bucket="general"
  IM=340
  JM=208
  wgt=pr
elif [ $DOMIN = "guamnmmb" ]
then
  filenamthree="wrf.GU05"
  DOMIN_bucket="general"
  IM=223
  JM=170
  wgt=guam
fi

if [ $DOMIN = "conusarw" ]
then
  filenamthree="wrf.EMEAST05"
  DOMIN_bucket="general"
  IM=884
  JM=614
elif [ $DOMIN = "akarw" ]
then
  filenamthree="wrf.EMAK05"
  DOMIN_bucket="general"
  IM=825
  JM=603
  wgt=ak
elif [ $DOMIN = "hiarw" ]
then
  filenamthree="wrf.EMHI05"
  DOMIN_bucket="general"
  IM=223
  JM=170
  wgt=hi
elif [ $DOMIN = "prarw" ]
then
  filenamthree="wrf.EMPR05"
  DOMIN_bucket="general"
  IM=340
  JM=208
  wgt=pr
elif [ $DOMIN = "guamarw" ]
then
  filenamthree="wrf.EMGU05"
  DOMIN_bucket="general"
  IM=223
  JM=170
  wgt=guam
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

cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_5km master${fhr}.ctl

cat >input${fhr}.prd <<EOF5
$DATA/post_${fhr}/WRFPRS${fhr}.tm00
EOF5

rm fort.*

export pgm=hiresw_prdgen  ;. ./prep_step

## export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g255_5km"
export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.${wgt}5km"

export FORT10="master${fhr}.ctl"

echo EXECUTING hiresw_prdgen  for 5 km

export FORT11="input${fhr}.prd"
export FORT621="input${fhr}.prd"
$EXEChiresw/hiresw_nmmb_prdgen  > prdgen.out${fhr}_5km 2>errfile_5km

export err=$?;./err_chk

### cp $DATA/post/WRFPRS${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.wrfprs${fhr}.tm00

###############################################################
###############################################################
###############################################################


# compute precip buckets

threehrprev=`expr $fhr - 3`
sixhrprev=`expr $fhr - 6`
onehrprev=`expr $fhr - 1`

if [ $threehrprev -lt 10 ]
then
threehrprev=0$threehrprev
fi

if [ $sixhrprev -lt 10 ]
then
sixhrprev=0$sixhrprev
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
      cp ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5km${fhr}.tm00
      $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awp5km${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5kmi${fhr}.tm00
      $CNVGRIB -g12 -p40 ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5km${fhr}.tm00.grib2
      $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.awp5km${fhr}.tm00.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.awp5km${fhr}.tm00.grib2.idx
      if [ $SENDDBN_GB2 = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2 $job $COMOUT/$DOMIN.t${CYC}z.awp5km${fhr}.tm00.grib2
         $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2_WIDX $job $COMOUT/$DOMIN.t${CYC}z.awp5km${fhr}.tm00.grib2.idx
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

  cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00 PCP6HR${fhr}.tm00 > $DOMOUT.t${CYC}z.awp5km${fhr}.tm00

  else

## model = "nmm"
   cat ${filenamthree}${fhr}.tm00  PCP1HR${fhr}.tm00  > $DOMOUT.t${CYC}z.awp5km${fhr}.tm00

  fi

###### DONE PRECIP BUCKET

  if test $SENDCOM = 'YES'
  then
    cp $DOMOUT.t${CYC}z.awp5km${fhr}.tm00 $COMOUT/.
    $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awp5km${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5kmi${fhr}.tm00
    $CNVGRIB -g12 -p40 $DOMOUT.t${CYC}z.awp5km${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5km${fhr}.tm00.grib2
    $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.awp5km${fhr}.tm00.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.awp5km${fhr}.tm00.grib2.idx
    if [ $SENDDBN_GB2 = YES ]; then
       $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2 $job $COMOUT/$DOMIN.t${CYC}z.awp5km${fhr}.tm00.grib2
       $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2_WIDX $job $COMOUT/$DOMIN.t${CYC}z.awp5km${fhr}.tm00.grib2.idx
    fi
  fi
fi
