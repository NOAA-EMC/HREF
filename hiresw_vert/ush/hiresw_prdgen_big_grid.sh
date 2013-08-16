#! /bin/ksh

set -x

utilexec=/nwprod/util/exec

export CNVGRIB=/nwprod/util/exec/cnvgrib
export WGRIB2=/nwprod/util/exec/wgrib2

fhr=$1
DOMIN_SMALL=$2
CYC=$3
model=$4
MEMBER=$5

reflag=1

# mkdir ${DATA}/prdgen_full
cd ${DATA}/prdgen_full_${fhr}/
sh $utilscript/setup.sh

#cd $DATA
DOMIN=${DOMIN_SMALL}${model}

###
### current definition of modelout will create GRIB files
### with filenames such as westarw.t06z.awpreg00.tm00, while
### they are currently westem.t06z.awpreg00.tm00.  Change
### modelout to "em" if maintaining filenames is required.
###

modelout=$model

if [ $model = "arw" ]
then
modelout="arw"
reflag=0
fi

DOMOUT=${DOMIN_SMALL}${modelout}
DOMOUTtwo="cent"${modelout}


if [ $DOMIN = "eastnmm" ]
then
  filenamthree="wrf.EAST04"
  DOMIN_bucket="full5km"
elif [ $DOMIN = "westnmm" ]
then
  filenamthree="wrf.WEST04"
  DOMIN_bucket="full5km"
elif [ $DOMIN = "aknmm" ]
then
  filenamthree="wrf.AK04"
  DOMIN_bucket="ak5km"
elif [ $DOMIN = "prnmm" ]
then
  filenamthree="wrf.PR04"
  #DOMIN_bucket="hipr5km"
  DOMIN_bucket="haitipr5km"
elif [ $DOMIN = "hinmm" ]
then
  filenamthree="wrf.HI04"
  DOMIN_bucket="hipr5km"
elif [ $DOMIN = "guamnmm" ]
then
  filenamthree="wrf.GU04"
  DOMIN_bucket="hipr5km"
elif [ $DOMIN = "conusnmmb" ]
then
  filenamthree="wrf.CONUS04"
  DOMIN_bucket="general"
  IM=1719
  JM=1044
elif [ $DOMIN = "aknewnmmb" ]
then
  filenamthree="wrf.AK04"
  DOMIN_bucket="general"
  IM=1217
  JM=948
elif [ $DOMIN = "guamnewnmmb" ]
then
  filenamthree="wrf.GU04"
  DOMIN_bucket="general"
  IM=287
  JM=219
elif [ $DOMIN = "hinewnmmb" ]
then
  filenamthree="wrf.HI04"
  DOMIN_bucket="general"
  IM=287
  JM=219
elif [ $DOMIN = "prnewnmmb" ]
then
  filenamthree="wrf.PR04"
  DOMIN_bucket="general"
  IM=440
  JM=269
fi

if [ $DOMIN = "eastarw" ]
then
  filenamthree="wrf.EMEST04"
  DOMIN_bucket="full5km"
elif [ $DOMIN = "westarw" ]
then
  filenamthree="wrf.EMWST04"
  DOMIN_bucket="full5km"
elif [ $DOMIN = "akarw" ]
then
  filenamthree="wrf.EMAK04"
  DOMIN_bucket="ak5km"
elif [ $DOMIN = "aknewarw" ]
then
  filenamthree="wrf.EMAK04"
  DOMIN_bucket="general"
  IM=1217
  JM=948
elif [ $DOMIN = "prarw" ]
then
  filenamthree="wrf.EMPR04"
  DOMIN_bucket="haitipr5km"
elif [ $DOMIN = "hiarw" ]
then
  filenamthree="wrf.EMHI04"
  DOMIN_bucket="hipr5km"
elif [ $DOMIN = "guamarw" ]
then
  filenamthree="wrf.EMGU04"
  DOMIN_bucket="hipr5km"
elif [ $DOMIN = "conusarw" ]
then
  filenamthree="wrf.EMCONUS04"
  DOMIN_bucket="general"
  IM=1719
  JM=1044
elif [ $DOMIN = "guamnewarw" ]
then
  filenamthree="wrf.EMGU04"
  DOMIN_bucket="general"
  IM=287
  JM=219
elif [ $DOMIN = "hinewarw" ]
then
  filenamthree="wrf.EMHI04"
  DOMIN_bucket="general"
  IM=287
  JM=219
elif [ $DOMIN = "prnewarw" ]
then
  filenamthree="wrf.EMPR04"
  DOMIN_bucket="general"
  IM=440
  JM=269
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

ls -l $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl

cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl master${fhr}.ctl

cat >input${fhr}.prd <<EOF5
$DATA/post_${fhr}/WRFPRS${fhr}.tm00
EOF5

rm fort.*

export pgm=hiresw_prdgen # ;. ./prep_step

export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g255"
export FORT10="master${fhr}.ctl"
export FORT11="input${fhr}.prd"

# if [ $model = "nmmb" ]
# then

# can both use this now?

export FORT621="input${fhr}.prd"
$EXEChiresw/hiresw_nmmb_prdgen > prdgen.out${fhr} 2>&1

# else
# $EXEChiresw/hiresw_prdgen > prdgen.out${fhr} 2>&1
# fi

export err=$?;./err_chk

cp $DATA/post_${fhr}/WRFPRS${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.wrfprs${fhr}.tm00

###############################################################
###############################################################
###############################################################


# compute precip buckets

onehrprev=`expr $fhr - 1`
threehrprev=`expr $fhr - 3`


if [ $threehrprev -lt 10 ]
then
threehrprev=0$threehrprev
fi

if [ $onehrprev -lt 10 ]
then
onehrprev=0$onehrprev
fi

if [ $fhr -eq 00 ]
then

  ###############################
  # Convert to grib2 format
  ###############################

  if test $SENDCOM = 'YES'
  then
      cp ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awpreg${fhr}.tm00
      $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awpreg${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awpregi${fhr}.tm00
      $CNVGRIB -g12 -p40 -nv ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awpreg${fhr}.tm00.grib2
      $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.awpreg${fhr}.tm00.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.awpreg${fhr}.tm00.grib2.idx

      if [ $SENDDBN_GB2 = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2 $job $COMOUT/$DOMIN.t${CYC}z.awpreg${fhr}.tm00.grib2
        $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2_WIDX $job $COMOUT/$DOMIN.t${CYC}z.awpreg${fhr}.tm00.grib2.idx
      fi
  fi
	
  
else  # (not f00)


### do one hour precip for everyone (arw,nmm)

  rm PCP3HR${fhr}.tm00
  rm PCP1HR${fhr}.tm00
  rm input.card

  echo "$DATA/prdgen_full" > input.card
  echo $filenamthree >> input.card
  echo $onehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card


while [ ! -e $DATA/prdgen_full_${onehrprev}/$filenamthree$onehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_full_${onehrprev}/$filenamthree$onehrprev.tm00
sleep 10
done

   $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
   export err=$?;./err_chk

     if [ $model = "arw" ] ; then

     if [ $fhr%3 -eq 0 ]
     then

#       create a 3 h bucket as well

while [ ! -e $DATA/prdgen_full_${threehrprev}/$filenamthree$threehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_full_${threehrprev}/$filenamthree$threehrprev.tm00
sleep 10
done

  rm input.card
  echo "$DATA/prdgen_full" > input.card
  echo $filenamthree >> input.card
  echo $threehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

  $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
  export err=$?;./err_chk

     fi

  cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00  > $DOMOUT.t${CYC}z.awpreg${fhr}.tm00

   else

## model = "nmm"

  cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > $DOMOUT.t${CYC}z.awpreg${fhr}.tm00

  fi  # arw/nmm break

###### DONE PRECIP BUCKET

  if test $SENDCOM = 'YES'
  then
    cp $DOMOUT.t${CYC}z.awpreg${fhr}.tm00 $COMOUT/.
    $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awpreg${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awpregi${fhr}.tm00
    $CNVGRIB -g12 -p40 -nv $DOMOUT.t${CYC}z.awpreg${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awpreg${fhr}.tm00.grib2
    $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.awpreg${fhr}.tm00.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.awpreg${fhr}.tm00.grib2.idx

    if [ $SENDDBN_GB2 = YES ]; then
      $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2 $job $COMOUT/$DOMIN.t${CYC}z.awpreg${fhr}.tm00.grib2
      $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2_WIDX $job $COMOUT/$DOMIN.t${CYC}z.awpreg${fhr}.tm00.grib2.idx
    fi
  fi

fi
