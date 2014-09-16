#! /bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:            hiresw_prdgen_oldgrid.sh
# Script description:     Runs prdgen for the legacy (old) grid distributed to AWIPS
#
#
# Author:        Matthew Pyle       Org: NP22         Date: 2014-02-11
#
# Abstract:      Runs prdgen for a specific hour, domain, and model, horizontally interpolating
#                native GRIB output onto the legacy 5 km grids
#
# Script history log:
# 2013-08-01  Matthew Pyle - Original script for parallel
# 2014-02-11  Matthew Pyle - Added brief docblock


set -x

utilexec=${utilexec:-/nwprod/util/exec}

export CNVGRIB=${CNVGRIB:-${utilexec}/cnvgrib}
export WGRIB2=${WGRIB2:-${utilexec}/wgrib2}

fhr=$1
DOMIN_SMALL=$2
CYC=${3}
model=$4
MEMBER=$5

reflag=1

mkdir ${DATA}/prdgen_5km
cd ${DATA}/prdgen_5km/
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
  DOMIN_bucket="general_g2"
  IM=884
  JM=614
elif [ $DOMIN = "aknmmb" ]
then
  filenamthree="wrf.AK05"
  DOMIN_bucket="general_g2"
  IM=825
  JM=603
  wgt=ak
  reg="20 6 0 0 0 0 0 0 825 603 44800000 -174500000 136 60000000 -150000000 5000000 5000000 0 64"
elif [ $DOMIN = "hinmmb" ]
then
  filenamthree="wrf.HI05"
  DOMIN_bucket="general_g2"
  IM=223
  JM=170
  wgt=hi
#  reg="0 6 0 0 0 0 0 0 223 170 0 0 16400000 -162350000 136 24005000 -152360000 45000 45000 64"
  reg="0 6 0 0 0 0 0 0 223 170 0 0 16400000 197650000 136 24005000 207640000 45000 45000 64"
elif [ $DOMIN = "prnmmb" ]
then
  filenamthree="wrf.PR05"
  DOMIN_bucket="general_g2"
  IM=340
  JM=208
  wgt=pr
#  reg="0 6 0 0 0 0 0 0 340 208 0 0 13500000 -76590000 136 22815000 -61335000 45000 45000 64"
  reg="0 6 0 0 0 0 0 0 340 208 0 0 13500000 283410000 136 22815000 298665000 45000 45000 64"
elif [ $DOMIN = "guamnmmb" ]
then
  filenamthree="wrf.GU05"
  DOMIN_bucket="general_g2"
  IM=223
  JM=170
  wgt=guam
  reg="0 6 0 0 0 0 0 0 223 170 0 0 11700000 141000000 136 19305000 150990000 45000 45000 64"
fi

if [ $DOMIN = "conusarw" ]
then
  filenamthree="wrf.EMEAST05"
  DOMIN_bucket="general_g2"
  IM=884
  JM=614
elif [ $DOMIN = "akarw" ]
then
  filenamthree="wrf.EMAK05"
  DOMIN_bucket="general_g2"
  IM=825
  JM=603
  wgt=ak
  reg="20 6 0 0 0 0 0 0 825 603 44800000 -174500000 136 60000000 -150000000 5000000 5000000 0 64"
elif [ $DOMIN = "hiarw" ]
then
  filenamthree="wrf.EMHI05"
  DOMIN_bucket="general_g2"
  IM=223
  JM=170
  wgt=hi
#  reg="0 6 0 0 0 0 0 0 223 170 0 0 16400000 -162350000 136 24005000 -152360000 45000 45000 64"
  reg="0 6 0 0 0 0 0 0 223 170 0 0 16400000 197650000 136 24005000 207640000 45000 45000 64"
elif [ $DOMIN = "prarw" ]
then
  filenamthree="wrf.EMPR05"
  DOMIN_bucket="general_g2"
  IM=340
  JM=208
  wgt=pr
#  reg="0 6 0 0 0 0 0 0 340 208 0 0 13500000 -76590000 136 22815000 -61335000 45000 45000 64"
  reg="0 6 0 0 0 0 0 0 340 208 0 0 13500000 283410000 136 22815000 298665000 45000 45000 64"
elif [ $DOMIN = "guamarw" ]
then
  filenamthree="wrf.EMGU05"
  DOMIN_bucket="general_g2"
  IM=223
  JM=170
  wgt=guam
  reg="0 6 0 0 0 0 0 0 223 170 0 0 11700000 141000000 136 19305000 150990000 45000 45000 64"
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

cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_5km master${fhr}.ctl

while [ ! -e $INPUT_DATA/postdone${fhr} ]
do
sleep 6
done

cat >input${fhr}.prd <<EOF5
$INPUT_DATA/WRFPRS${fhr}.tm00
EOF5

rm fort.*

export pgm=hiresw_prdgen  ;. ./prep_step

# export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.${wgt}5km"

# export FORT10="master${fhr}.ctl"

echo EXECUTING hiresw_prdgen  for 5 km

export FORT621="input${fhr}.prd"
# $EXEChiresw/hiresw_prdgen  > prdgen.out${fhr}_5km 2>errfile_5km

### extract just needed items

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":PRES:mean sea level:"  -grib prmsl.grb

echo collecting 1.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(MSLET|VIS|GUST|VIL|MAXUVV|MAXDVV|REFD|REFC|MAXREF|MXUPHL|\
TCOLI|TCOLR|TCOLS|TCOLC|TCOLW|LCDC|MCDC|HCDC|TCDC|RETOP|PWAT|LFTX|4LFTX):" -grib 1.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(HINDEX|TSOIL|SOILW|CSNOW|CICEP|CFRZR|CRAIN):" -grib nn.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(TMAX|TMIN|MAXUW|MAXVW|MAXRH|MINRH):" -grib 2.grb

echo collecting 3.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(PRES|HGT|TMP|LHTFL|SHTFL|CAPE|CIN):surface:" -grib 3.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HGT:cloud base:" -grib cld.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HGT:cloud ceiling:" -grib ceiling.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
":(TMP|RH|UGRD|VGRD|PLI|POT|DPT|SPFH|MCONV|VVEL|CAPE|CIN):(30-0|60-30|90-60|120-90|150-120|180-0|90-0|255-0) mb above ground:" \
-grib pbl.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
":(TMP|SPFH|DPT|RH|UGRD|VGRD|HLCY|USTM|VSTM|UPHL|PRES):(10|2|\
1000-0|3000-0|6000-0|1524|80|5000-2000) m above (ground|mean sea level):" \
-grib agl.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'planetary boundary layer' -grib pbl2.grb


echo collecting all_iso.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
":(UGRD|VGRD|TMP|HGT|VVEL|RH|DPT|ABSV):(200|250|300|400|500|525|550|575|\
600|625|650|675|700|725|750|775|800|825|850|875|900|925|950|975|1000) mb:" \
-grib all_iso.grb

echo just apcp.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'APCP' -grib apcp.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'WEASD' -grib weasd.grb


## split between inputs for bilinear interpolation and for nearest neighbor interpolation

cat prmsl.grb apcp.grb weasd.grb 1.grb 2.grb 3.grb cld.grb pbl.grb pbl2.grb agl.grb all_iso.grb  > inputs.grb

cat nn.grb ceiling.grb > inputs_nn.grb

rm   prmsl.grb apcp.grb weasd.grb 1.grb 2.grb 3.grb cld.grb nn.grb ceiling.grb pbl.grb pbl2.grb  agl.grb all_iso.grb 



# copygb2 -g"${reg}" -x $INPUT_DATA/WRFPRS${fhr}.tm00 ${filenamthree}${fhr}.tm00
copygb2 -g"${reg}" -x inputs.grb ${filenamthree}${fhr}.tm00_bilin
copygb2 -g"${reg}" -i2 -x inputs_nn.grb ${filenamthree}${fhr}.tm00_nn

cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_nn > ${filenamthree}${fhr}.tm00

export err=$?;./err_chk

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
      cp ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awpregf${fhr}
#      $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awpregf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awpregif${fhr}
#      $CNVGRIB -g12 -p40 ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awpregf${fhr}.grib2
#      $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.awpregf${fhr}.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.awpregf${fhr}.grib2.idx
      if [ $SENDDBN_GB2 = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job $COMOUT/$DOMIN.t${CYC}z.awpregf${fhr}.grib2
         $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_WIDX} $job $COMOUT/$DOMIN.t${CYC}z.awpregf${fhr}.grib2.idx
      fi
  fi

else

### do precip buckets if model is ARW

while [ ! -e $DATA/prdgen_5km/$filenamthree$onehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_5km/$filenamthree$onehrprev.tm00
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

 export pgm=hiresw_pcpbucket_${DOMIN_bucket}
 $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
 export err=$?;./err_chk

mv errfile errfile_${fhr}

  if [ $model = "arw" ] ; then

  if [ $fhr%3 -eq 0 ]
  then

while [ ! -e $DATA/prdgen_5km/$filenamthree$threehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_5km/$filenamthree$threehrprev.tm00
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

 export pgm=hiresw_pcpbucket_${DOMIN_bucket}
 $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
 export err=$?;./err_chk
mv errfile errfile_${fhr}_3hrly

  fi

  cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00 PCP6HR${fhr}.tm00 > $DOMOUT.t${CYC}z.awpregf${fhr}

  else

## model = "nmm"
   cat ${filenamthree}${fhr}.tm00  PCP1HR${fhr}.tm00  > $DOMOUT.t${CYC}z.awpregf${fhr}

  fi

###### DONE PRECIP BUCKET

  if test $SENDCOM = 'YES'
  then
    cp $DOMOUT.t${CYC}z.awpregf${fhr} $COMOUT/.
#    $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awpregf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awpregif${fhr}
#    $CNVGRIB -g12 -p40 $DOMOUT.t${CYC}z.awpregf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awpregf${fhr}.grib2
#    $WGRIB2 $COMOUT/$DOMOUT.t${CYC}z.awpregf${fhr}.grib2 -s > $COMOUT/$DOMOUT.t${CYC}z.awpregf${fhr}.grib2.idx
    if [ $SENDDBN_GB2 = YES ]; then
       $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job $COMOUT/$DOMIN.t${CYC}z.awpregf${fhr}.grib2
       $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE_WIDX} $job $COMOUT/$DOMIN.t${CYC}z.awpregf${fhr}.grib2.idx
    fi
  fi
fi
