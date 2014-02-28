#! /bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:           hiresw_prdgen_big_grid.sh
# Script description:    Runs prdgen for the highest resolution NDFD output from HIRESW
#
#
# Author:        Matthew Pyle       Org: NP22         Date: 2014-02-11
#
# Abstract:            Runs prdgen to horizontally interpolate a limited number
#                      of fields onto the 2.5 or 3 km NDFD grid associated with
#                      a specific region.
#
# Script history log:
# 2013-11-01  Matthew Pyle - Original script for parallel
# 2014-02-11  Matthew Pyle - documentation block

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

mkdir ${DATA}/prdgen_full
cd ${DATA}/prdgen_full/
sh $utilscript/setup.sh

#cd $DATA
DOMIN=${DOMIN_SMALL}${model}

modelout=$model

if [ $model = "arw" ]
then
modelout="arw"
reflag=0
fi

DOMOUT=${DOMIN_SMALL}${modelout}
DOMOUTtwo="cent"${modelout}


if [ $DOMIN = "conusnmmb" ]
then
  filenamthree="wrf.CONUS04"
  DOMIN_bucket="general"
  gres="2p5"
  IM=2145
  JM=1377
elif [ $DOMIN = "aknmmb" ]
then
  filenamthree="wrf.AK04"
  DOMIN_bucket="general"
  gres="3km"
  IM=1649
  JM=1105
elif [ $DOMIN = "guamnmmb" ]
then
  filenamthree="wrf.GU04"
  DOMIN_bucket="general"
  gres="2p5"
  IM=193
  JM=193
elif [ $DOMIN = "hinmmb" ]
then
  filenamthree="wrf.HI04"
  DOMIN_bucket="general"
  gres="2p5"
  IM=321
  JM=225
elif [ $DOMIN = "prnmmb" ]
then
  filenamthree="wrf.PR04"
  DOMIN_bucket="general"
  gres="2p5"
  IM=177
  JM=129
fi

if [ $DOMIN = "akarw" ]
then
  filenamthree="wrf.EMAK04"
  DOMIN_bucket="general"
  gres="3km"
  IM=1649
  JM=1105
elif [ $DOMIN = "conusarw" ]
then
  filenamthree="wrf.EMCONUS04"
  DOMIN_bucket="general"
  gres="2p5"
  IM=2145
  JM=1377
elif [ $DOMIN = "guamarw" ]
then
  filenamthree="wrf.EMGU04"
  DOMIN_bucket="general"
  gres="2p5"
  IM=193
  JM=193
elif [ $DOMIN = "hiarw" ]
then
  filenamthree="wrf.EMHI04"
  DOMIN_bucket="general"
  gres="2p5"
  IM=321
  JM=225
elif [ $DOMIN = "prarw" ]
then
  filenamthree="wrf.EMPR04"
  DOMIN_bucket="general"
  gres="2p5"
  IM=177
  JM=129
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

while [ ! -e $DATA/postdone${fhr} ]
do
sleep 6
done


cat >input${fhr}.prd <<EOF5
$DATA/post/WRFPRS${fhr}.tm00
EOF5

rm fort.*

export pgm=hiresw_prdgen # ;. ./prep_step

if [ $DOMIN_SMALL = "conus" ]
then
export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g184"
elif [ $DOMIN_SMALL = "ak" ]
then
export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g91"
elif [ $DOMIN_SMALL = "pr" ]
then
export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g195"
elif [ $DOMIN_SMALL = "hi" ]
then
export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g196"
elif [ $DOMIN_SMALL = "guam" ]
then
export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g199"
else
export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g255"
fi

export FORT10="master${fhr}.ctl"

export FORT11="input${fhr}.prd"

# if [ $model = "nmmb" ]
# then

# can both use this now?

export FORT621="input${fhr}.prd"
$EXEChiresw/hiresw_prdgen > prdgen.out${fhr} 2>&1

# else
# $EXEChiresw/hiresw_prdgen > prdgen.out${fhr} 2>&1
# fi

export err=$?;./err_chk

#### being done multiple times?????  Make so only a single prdgen job does this copy
cp $DATA/post/WRFPRS${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.wrfprs${fhr}

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
      cp ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.ndfd${gres}f${fhr}
      $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.ndfd${gres}f${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.ndfd${gres}if${fhr}
        datestr=`date`
        echo BEFORE CNVGRIB in NDFD at $datestr

      $CNVGRIB -g12 -p40 -nv ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.ndfd${gres}f${fhr}.grib2
      $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.ndfd${gres}f${fhr}.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.ndfd${gres}f${fhr}.grib2.idx
        datestr=`date`
        echo AFTER CNVGRIB in NDFD at $datestr

      if [ $SENDDBN_GB2 = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2 $job $COMOUT/$DOMIN.t${CYC}z.ndfd${gres}f${fhr}.grib2
        $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2_WIDX $job $COMOUT/$DOMIN.t${CYC}z.ndfd${gres}f${fhr}.rib2.idx
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


while [ ! -e $DATA/prdgen_full/$filenamthree$onehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_full/$filenamthree$onehrprev.tm00
sleep 10
done

   $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
   export err=$?;./err_chk

     if [ $model = "arw" ] ; then

     if [ $fhr%3 -eq 0 ]
     then

#       create a 3 h bucket as well

while [ ! -e $DATA/prdgen_full/$filenamthree$threehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_full/$filenamthree$threehrprev.tm00
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

  cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00  > $DOMOUT.t${CYC}z.ndfd${gres}f${fhr}

   else

## model = "nmm"

  cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > $DOMOUT.t${CYC}z.ndfd${gres}f${fhr}

  fi  # arw/nmm break

###### DONE PRECIP BUCKET

  if test $SENDCOM = 'YES'
  then
    cp $DOMOUT.t${CYC}z.ndfd${gres}f${fhr} $COMOUT/.
    $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.ndfd${gres}f${fhr} $COMOUT/$DOMOUT.t${CYC}z.ndfd${gres}if${fhr}
        datestr=`date`
        echo BEFORE CNVGRIB in NDFD at $datestr

    $CNVGRIB -g12 -p40 -nv $DOMOUT.t${CYC}z.ndfd${gres}f${fhr} $COMOUT/$DOMOUT.t${CYC}z.ndfd${gres}f${fhr}.grib2
    $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.ndfd${gres}f${fhr}.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.ndfd${gres}f${fhr}.grib2.idx
        datestr=`date`
        echo AFTER CNVGRIB in NDFD at $datestr

    if [ $SENDDBN_GB2 = YES ]; then
      $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2 $job $COMOUT/$DOMIN.t${CYC}z.ndfd${gres}f${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP_GB2_WIDX $job $COMOUT/$DOMIN.t${CYC}z.ndfd${gres}f${fhr}.grib2.idx
    fi
  fi

fi
