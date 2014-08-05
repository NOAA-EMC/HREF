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
  DOMIN_bucket="general_g2"
  rg="conus"
  gres="2p5"
  IM=2145
  JM=1377
  reg="30 6 0 0 0 0 0 0 2145 1377 20192000 238446000 136 25000000 265000000 2540000 2540000 0 64 25000000 25000000"
elif [ $DOMIN = "aknmmb" ]
then
  filenamthree="wrf.AK04"
  DOMIN_bucket="general_g2"
  rg="ak"
  gres="3km"
  IM=1649
  JM=1105
#  reg="20 6 0 0 0 0 0 0 1649 1105 40530000 -178571000 136 60000000 -150000000 2976000 2976000 0 64"
  reg="20 6 0 0 0 0 0 0 1649 1105 40530000 181429000 136 60000000 2100000000 2976000 2976000 0 64"
elif [ $DOMIN = "guamnmmb" ]
then
  filenamthree="wrf.GU04"
  DOMIN_bucket="general_g2"
  rg="guam"
  gres="2p5"
  IM=193
  JM=193
  reg="10 6 0 0 0 0 0 0 193 193 12350000 143687000 136 20000000 16794000 148280000 64 0 2500000 2500000"
elif [ $DOMIN = "hinmmb" ]
then
  filenamthree="wrf.HI04"
  DOMIN_bucket="general_g2"
  rg="hi"
  gres="2p5"
  IM=321
  JM=225
  reg="10 6 0 0 0 0 0 0 321 225 18073000 198475000 136 20000000 23088000 206131000 64 0 2500000 2500000"
elif [ $DOMIN = "prnmmb" ]
then
  filenamthree="wrf.PR04"
  DOMIN_bucket="general_g2"
  rg="pr"
  gres="2p5"
  IM=177
  JM=129
  reg="10 6 0 0 0 0 0 0 177 129 16829000 291804000 136 20000000 19747000 296028000 64 0 2500000 2500000"
fi

if [ $DOMIN = "akarw" ]
then
  filenamthree="wrf.EMAK04"
  DOMIN_bucket="general_g2"
  rg="ak"
  gres="3km"
  IM=1649
  JM=1105
  reg="20 6 0 0 0 0 0 0 1649 1105 40530000 -178571000 136 60000000 -150000000 2976000 2976000 0 64"
  reg="20 6 0 0 0 0 0 0 1649 1105 40530000 181429000 136 60000000 2100000000 2976000 2976000 0 64"
elif [ $DOMIN = "conusarw" ]
then
  filenamthree="wrf.EMCONUS04"
  DOMIN_bucket="general_g2"
  rg="conus"
  gres="2p5"
  IM=2145
  JM=1377
#  reg="30 6 0 0 0 0 0 0 2145 1377 20192000 -121554000 136 25000000 -95000000 2540000 2540000 0 64 25000000 25000000"
  reg="30 6 0 0 0 0 0 0 2145 1377 20192000 238446000 136 25000000 265000000 2540000 2540000 0 64 25000000 25000000"
elif [ $DOMIN = "guamarw" ]
then
  filenamthree="wrf.EMGU04"
  DOMIN_bucket="general_g2"
  rg="guam"
  gres="2p5"
  IM=193
  JM=193
  reg="10 6 0 0 0 0 0 0 193 193 12350000 143687000 136 20000000 16794000 148280000 64 0 2500000 2500000"
elif [ $DOMIN = "hiarw" ]
then
  filenamthree="wrf.EMHI04"
  DOMIN_bucket="general_g2"
  rg="hi"
  gres="2p5"
  IM=321
  JM=225
#  reg="10 6 0 0 0 0 0 0 321 225 18073000 -161525000 136 20000000 23088000 -153869000 64 0 2500000 2500000"
  reg="10 6 0 0 0 0 0 0 321 225 18073000 198475000 136 20000000 23088000 206131000 64 0 2500000 2500000"
elif [ $DOMIN = "prarw" ]
then
  filenamthree="wrf.EMPR04"
  DOMIN_bucket="general_g2"
  rg="pr"
  gres="2p5"
  IM=177
  JM=129
#  reg="10 6 0 0 0 0 0 0 177 129 16829000 -68196000 136 20000000 19747000 -63972000 64 0 2500000 2500000"
  reg="10 6 0 0 0 0 0 0 177 129 16829000 291804000 136 20000000 19747000 296028000 64 0 2500000 2500000"
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

use_1h=0
use_3h=0

if [ $fhr -eq 0 ]
then
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_for_3h master${fhr}.ctl
use_3h=1
elif [ $fhr%3 -eq 0 ]
then
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_for_3h master${fhr}.ctl
use_3h=1
else
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_for_1h master${fhr}.ctl
use_1h=1
fi

echo use_1h $use_1h
echo use_3h $use_3h


while [ ! -e $INPUT_DATA/postdone${fhr} ]
do
sleep 6
done


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

### extract just needed items

if [ $use_1h -eq 1 ]
then

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(MSLET|VIS|VIL|MAXUVV|MAXDVV|REFC|MAXREF|MXUPHL|\
CSNOW|CICEP|CFRZR|CRAIN|TCDC|RETOP|4LFTX):" -grib 1.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(TMAX|TMIN|MAXUW|MAXVW|MAXRH|MINRH):" -grib 2.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(HINDEX|PRES|HGT|TMP|CAPE|CIN):surface:" -grib 3.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HGT:cloud base:" -grib cld.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
":(CAPE|CIN):(180-0|90-0) mb above ground:" \
-grib pbl.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
":(TMP|SPFH|RH|UGRD|VGRD|HLCY|REFD|USTM|VSTM|UPHL|PRES):(10|2|\
3000-0|6000-0|1000-0|1000|4000|80|5000-2000) m above (ground|mean sea level):" \
-grib agl.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "(HGT|VRATE):planetary boundary layer:" -grib pbl2.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'APCP' -grib apcp.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'WEASD' -grib weasd.grb

cat apcp.grb weasd.grb 1.grb 2.grb 3.grb cld.grb  pbl.grb pbl2.grb agl.grb  > inputs.grb
rm  apcp.grb weasd.grb 1.grb 2.grb 3.grb cld.grb  pbl.grb pbl2.grb agl.grb 

elif [ $use_3h -eq 1 ]
then

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(MSLET|VIL|MAXUVV|MAXDVV|MAXREF|MXUPHL|\
TCDC|RETOP):" -grib 1.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(TMAX|TMIN|MAXUW|MAXVW|MAXRH|MINRH):" -grib 2.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(HINDEX|HGT|CAPE|CIN):surface:" -grib 3.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HGT:cloud base:" -grib cld.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
":(CAPE|CIN):(180-0|90-0) mb above ground:" \
-grib pbl.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
":(TMP|RH|UGRD|VGRD|HLCY|REFD|USTM|VSTM|UPHL|PRES):( \
3000-0|6000-0|1000-0|1000|4000|80|5000-2000) m above (ground|mean sea level):" \
-grib agl.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(RH|DPT):2 m above ground:" -grib bonus_agl.grb
cat bonus_agl.grb >> agl.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "(HGT|VRATE):planetary boundary layer:" -grib pbl2.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'APCP' -grib apcp.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'WEASD' -grib weasd.grb

cat  weasd.grb apcp.grb 1.grb 2.grb 3.grb cld.grb  pbl.grb pbl2.grb agl.grb  > inputs.grb
rm   weasd.grb apcp.grb 1.grb 2.grb 3.grb cld.grb  pbl.grb pbl2.grb agl.grb 

fi

# copygb2 -g"${reg}" -x $INPUT_DATA/WRFPRS${fhr}.tm00 ${filenamthree}${fhr}.tm00
copygb2 -g"${reg}" -X -x inputs.grb ${filenamthree}${fhr}.tm00




# copygb2 -g"${reg}" -x $INPUT_DATA/WRFPRS${fhr}.tm00 ${filenamthree}${fhr}.tm00


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

## temp copy to COMOUT
       cp  $DOMOUT.t${CYC}z.ndfd${gres}f${fhr} ${COMOUT}
## temp copy to COMOUT
