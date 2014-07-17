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

utilexec=${utilexec:-/nwprod/util/exec}

export CNVGRIB=${CNVGRIB:-${utilexec}/cnvgrib}
export WGRIB2=${WGRIB2:-${utilexec}/wgrib2}

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
  rg="conus"
  gres="2p5"
  IM=2145
  JM=1377
elif [ $DOMIN = "aknmmb" ]
then
  filenamthree="wrf.AK04"
  DOMIN_bucket="general"
  rg="ak"
  gres="3km"
  IM=1649
  JM=1105
elif [ $DOMIN = "guamnmmb" ]
then
  filenamthree="wrf.GU04"
  DOMIN_bucket="general"
  rg="guam"
  gres="2p5"
  IM=193
  JM=193
elif [ $DOMIN = "hinmmb" ]
then
  filenamthree="wrf.HI04"
  DOMIN_bucket="general"
  rg="hi"
  gres="2p5"
  IM=321
  JM=225
elif [ $DOMIN = "prnmmb" ]
then
  filenamthree="wrf.PR04"
  DOMIN_bucket="general"
  rg="pr"
  gres="2p5"
  IM=177
  JM=129
fi

if [ $DOMIN = "akarw" ]
then
  filenamthree="wrf.EMAK04"
  DOMIN_bucket="general"
  rg="ak"
  gres="3km"
  IM=1649
  JM=1105
elif [ $DOMIN = "conusarw" ]
then
  filenamthree="wrf.EMCONUS04"
  DOMIN_bucket="general"
  rg="conus"
  gres="2p5"
  IM=2145
  JM=1377
elif [ $DOMIN = "guamarw" ]
then
  filenamthree="wrf.EMGU04"
  DOMIN_bucket="general"
  rg="guam"
  gres="2p5"
  IM=193
  JM=193
elif [ $DOMIN = "hiarw" ]
then
  filenamthree="wrf.EMHI04"
  DOMIN_bucket="general"
  rg="hi"
  gres="2p5"
  IM=321
  JM=225
elif [ $DOMIN = "prarw" ]
then
  filenamthree="wrf.EMPR04"
  DOMIN_bucket="general"
  rg="pr"
  gres="2p5"
  IM=177
  JM=129
fi

filedir=$DATA

export fhr
export tmmark=tm00

###############################################################
###############################################################
###############################################################

#
# make GRIB file with pressure data every 25 mb for EMC's FVS
# verification

if [ $fhr -eq 0 ]
then
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_for_3h master${fhr}.ctl
elif [ $fhr%3 -eq 0 ]
then
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_for_3h master${fhr}.ctl
else
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_for_1h master${fhr}.ctl
fi


while [ ! -e $INPUT_DATA/postdone${fhr} ]
do
sleep 6
done


cat >input${fhr}.prd <<EOF5
$INPUT_DATA/WRFPRS${fhr}.tm00
EOF5

rm fort.*

export pgm=hiresw_prdgen

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
export FORT621="input${fhr}.prd"

$EXEChiresw/hiresw_prdgen > prdgen.out${fhr} 2>&1
export err=$?;./err_chk

cp $INPUT_DATA/WRFPRS${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.wrfprs${fhr}

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


       cp ${filenamthree}${fhr}.tm00 $DOMOUT.t${CYC}z.ndfd${gres}f${fhr}

### will be cat'd to smartinit generated file in ncoproc script
### no need to index, convert, or alert this file (which is not the final one for output)

  
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

   export pgm=hiresw_pcpbucket_${DOMIN_bucket}
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

  export pgm=hiresw_pcpbucket_${DOMIN_bucket}
  $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
  export err=$?;./err_chk


     cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00  > $DOMOUT.t${CYC}z.ndfd${gres}f${fhr}

     else # not $fhr%3=0

     cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > $DOMOUT.t${CYC}z.ndfd${gres}f${fhr}

     fi


   else

## model = "nmm"

     cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > $DOMOUT.t${CYC}z.ndfd${gres}f${fhr}


  fi  # arw/nmm break

###### DONE PRECIP BUCKET

fi # f00 or not
