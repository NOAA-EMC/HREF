!/bin/ksh
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

utilexec=${utilexec:-/nwprod/util/exec}

export CNVGRIB=${CNVGRIB:-${utilexec}/cnvgrib}
export WGRIB2=${WGRIB2:-${utilexec}/wgrib2}

fhr=$1
DOMIN_SMALL=$2
CYC=${3}
model=$4
MEMBER=$5
subpiece=$6

reflag=1

mkdir ${DATA}/prdgen_5km_${subpiece}
cd ${DATA}/prdgen_5km_${subpiece}/
sh $utilscript/setup.sh

DOMIN=${DOMIN_SMALL}${model}

modelout=$model
if [ $model = "arw" ]
then
modelout="arw"
reflag=0
fi

DOMOUT=${DOMIN_SMALL}${modelout}

if [ $DOMIN = "conusnmmb" ]
then
  filenamthree="wrf.CONUS05"
  DOMIN_bucket="general_g2"
  IM=1473
  JM=1025
fi

if [ $DOMIN = "conusarw" ]
then
  filenamthree="wrf.EMCONUS05"
  DOMIN_bucket="general_g2"
  IM=1473
  JM=1025
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

if [ $DOMIN_SMALL = "conus" ]
then
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_5km_g227 master${fhr}.ctl
cp $PARMhiresw/hiresw_conus_awp5km.txt_${subpiece} hiresw_grid_extract.txt
else
cp $PARMhiresw/hiresw_conus_awp5km.txt hiresw_grid_extract.txt
cp $PARMhiresw/hiresw_${model}_master.${DOMIN}.ctl_5km master${fhr}.ctl
fi


while [ ! -e $INPUT_DATA/postdone${fhr} ]
do
sleep 6
done

cat >input${fhr}.prd <<EOF5
$INPUT_DATA/WRFPRS${fhr}.tm00
EOF5

rm fort.*

# export pgm=hiresw_prdgen  ;. ./prep_step


# if [ $DOMIN_SMALL = "conus" ]
# then
# export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g227"
# else
# export FORT21="$FIXhiresw/hiresw_wgt_${DOMIN}.g255_5km"
# fi

export FORT10="master${fhr}.ctl"

echo EXECUTING hiresw_prdgen  for 5 km

# export FORT621="input${fhr}.prd"
# $EXEChiresw/hiresw_prdgen  > prdgen.out${fhr}_5km 2>errfile_5km

### extract just needed items

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":PRES:mean sea level:"  -grib prmsl.grb
# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(MSLET|VIS|GUST|VIL|MAXUVV|MAXDVV|REFD|REFC|MAXREF|MXUPHL|\
# TCOLI|TCOLR|TCOLS|TCOLC|TCOLW|LCDC|MCDC|HCDC|TCDC|RETOP|PWAT|LFTX|4LFTX):" -grib 1.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(TMAX|TMIN|MAXUW|MAXVW|MAXRH|MINRH):" -grib 2.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(HINDEX|TSOIL|SOILW|CSNOW|CICEP|CFRZR|CRAIN):" -grib nn.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(PRES|HGT|TMP|LHTFL|SHTFL|CAPE|CIN):surface:" -grib 3.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HGT:cloud base:" -grib cld.grb
# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HGT:cloud ceiling:" -grib ceiling.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
# ":(TMP|RH|UGRD|VGRD|PLI|POT|DPT|SPFH|MCONV|VVEL|CAPE|CIN):(30-0|60-30|90-60|120-90|150-120|180-0|90-0|255-0) mb above ground:" \
# -grib pbl.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
# ":(TMP|SPFH|DPT|RH|UGRD|VGRD|HLCY|USTM|VSTM|UPHL|PRES):(10|2|\
# 1000-0|3000-0|6000-0|1524|80|5000-2000) m above (ground|mean sea level):" \
# -grib agl.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'planetary boundary layer' -grib pbl2.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match \
# ":(UGRD|VGRD|TMP|HGT|VVEL|RH|DPT|ABSV):(200|250|300|350|400|450|500|525|550|575|\
# 600|625|650|675|700|725|750|775|800|825|850|875|900|925|950|975|1000) mb:" \
# -grib all_iso.grb

# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'APCP' -grib apcp.grb
# $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match 'WEASD' -grib weasd.grb

# cat prmsl.grb  weasd.grb 1.grb 2.grb 3.grb cld.grb  pbl.grb pbl2.grb agl.grb all_iso.grb  > inputs.grb
# cat nn.grb ceiling.grb  > inputs_nn.grb
# cat apcp.grb > inputs_budget.grb

# rm   prmsl.grb apcp.grb weasd.grb 1.grb 2.grb 3.grb cld.grb nn.grb ceiling.grb pbl.grb pbl2.grb  agl.grb all_iso.grb

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 | grep -F -f hiresw_grid_extract.txt | $WGRIB2 -i -grib inputs.grb $INPUT_DATA/WRFPRS${fhr}.tm00

if [ $subpiece = "1" ]
then
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(APCP|WEASD):" -grib inputs_budget.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(HINDEX|TSOIL|SOILW|CSNOW|CICEP|CFRZR|CRAIN):" -grib nn.grb
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HGT:cloud ceiling:" -grib ceiling.grb
cat nn.grb ceiling.grb > inputs_nn.grb
/u/Wesley.Ebisuzaki/bin/wgrib2  inputs_nn.grb -new_grid_interpolation neighbor -set_grib_type complex2 -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${filenamthree}${fhr}.tm00_nn
/u/Wesley.Ebisuzaki/bin/wgrib2  inputs_budget.grb -new_grid_interpolation budget -set_grib_type complex2 -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${filenamthree}${fhr}.tm00_budget
fi


conus227="30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 136 25000000 265000000 5079000 5079000 0 64 25000000 25000000"

# copygb2 -g"${reg}" -x $INPUT_DATA/WRFPRS${fhr}.tm00 ${filenamthree}${fhr}.tm00
# time copygb2 -g"${conus227}" -x inputs.grb ${filenamthree}${fhr}.tm00
# time $WGRIB2  inputs.grb  -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 iplib${fhr}.tm00
# time /u/Wesley.Ebisuzaki/bin/wgrib2  inputs.grb  -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 iplib${fhr}.tm00

/u/Wesley.Ebisuzaki/bin/wgrib2  inputs.grb  -set_grib_type complex2 -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${filenamthree}${fhr}.tm00_bilin

if [ $subpiece = "1" ]
then
cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00_budget > ${filenamthree}${fhr}.tm00
else
mv ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00
fi

export err=$?;./err_chk


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
      cp ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2_${subpiece}
#      $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awp5kmf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awp5kmif${fhr}
  fi
else

if [ $subpiece = "1" ]
then
echo COMPUTING PRECIP BUCKETS

### do precip buckets if model is ARW

# precip bucket subpieces can be changed to "1" if needed, as only will run for the first subpiece
while [ ! -e $DATA/prdgen_5km_${subpiece}/$filenamthree$onehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_5km_${subpiece}/$filenamthree$onehrprev.tm00
sleep 10
done


  rm PCP1HR${fhr}.tm00
  rm input.card
  echo "$DATA/prdgen_5km_${subpiece}" > input.card
  echo $filenamthree >> input.card
  echo $onehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

 export pgm=hiresw_pcpbucket_${DOMIN_bucket}
 $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
 export err=$?;./err_chk

  if [ $model = "arw" ] ; then

  if [ $fhr%3 -eq 0 ]
  then

echo "3 hourly, do 3H precip bucket"

while [ ! -e $DATA/prdgen_5km_${subpiece}/$filenamthree$threehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_5km_${subpiece}/$filenamthree$threehrprev.tm00
sleep 10
done


  rm PCP3HR${fhr}.tm00
  rm input.card
  echo "$DATA/prdgen_5km_${subpiece}" > input.card
  echo $filenamthree >> input.card
  echo $threehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

 export pgm=hiresw_pcpbucket_${DOMIN_bucket}
 $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
 export err=$?;./err_chk

  fi
  cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00  > $DOMOUT.t${CYC}z.awp5kmf${fhr}

  else
   cat ${filenamthree}${fhr}.tm00  PCP1HR${fhr}.tm00  > $DOMOUT.t${CYC}z.awp5kmf${fhr}
  fi

else

mv ${filenamthree}${fhr}.tm00 $DOMOUT.t${CYC}z.awp5kmf${fhr}

fi # subpiece=1

###### DONE PRECIP BUCKET

  if test $SENDCOM = 'YES'
  then
     cp $DOMOUT.t${CYC}z.awp5kmf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2_${subpiece}
#    $utilexec/grbindex $COMIN/$DOMOUT.t${CYC}z.awp5kmf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awp5kmif${fhr}
  fi

fi # if f00 test
