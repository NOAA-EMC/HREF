#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:          hiresw_prdgen_3km_grid.sh
# Script description:   Interpolates CONUS domain output to 3 km grid
#
#
# Author:        Matthew Pyle       Org: NP22         Date: 2014-02-11
#
# Abstract:             Only run for the CONUS domain, it provides subset of CONUS
#                       output on the 3km HRRR grid.
#
# Script history log:
# 2013-11-01  Matthew Pyle - Original script for parallel
# 2014-02-11  Matthew Pyle - documentation block and cleanup
# 2017-02-16  Matthew Pyle - modified hiresw_prdgen_5km_grid.sh for 3 km.



set -x

fhr=$1
DOMIN_SMALL=$2
CYC=${3}
model=$4
subpiece=$5

wgrib2def="lambert:262.5:38.5:38.5 237.280:1799:3000 21.138:1059:3000"

compress="c3 -set_bitmap 1"

reflag=1

mkdir ${DATA}/prdgen_3km_${subpiece}
cd ${DATA}/prdgen_3km_${subpiece}/

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
  filenamthree="wrf.CONUS03"
  DOMIN_bucket="general_g2"
  IM=1799
  JM=1059
fi

if [ $DOMIN = "conusarw" -o $DOMIN = "conusmem2arw" ]
then
  filenamthree="wrf.EMCONUS03"
  DOMIN_bucket="general_g2"
  IM=1799
  JM=1059
fi

filedir=$DATA

export fhr
export tmmark=tm00


###############################################################
###############################################################
###############################################################

#

if [ $DOMIN_SMALL = "conus"  -o $DOMIN_SMALL = "conusmem2" ]
then
cp $PARMhiresw/hiresw_subset.txt hiresw_grid_extract.txt
fi

if [ $DOMIN_SMALL = "conus" ]
then

if [ $fhr -eq 00 ]
then
INPUT_DATA=$INPUT_DATA_EVEN
elif [ $fhr%2 -eq 0 ]
then
INPUT_DATA=$INPUT_DATA_EVEN
else
INPUT_DATA=$INPUT_DATA_ODD
fi

fi



while [ ! -e $INPUT_DATA/postdone${fhr} ]
do
sleep 6
done

### extract just needed items

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 | grep -F -f hiresw_grid_extract.txt | $WGRIB2 -i -grib inputs.grb $INPUT_DATA/WRFPRS${fhr}.tm00
$WGRIB2  inputs.grb  -set_grib_type ${compress} -new_grid_interpolation neighbor -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_nn


if [ $subpiece = "1" ]
then
cat ${filenamthree}${fhr}.tm00_nn > ${filenamthree}${fhr}.tm00
else
mv ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00
fi

export err=$?; err_chk


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

#  if test $SENDCOM = 'YES'
#  then
      cp ${filenamthree}${fhr}.tm00 $DATA/hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2_${subpiece}
#  fi
else

if [ $subpiece = "1" ]
then
echo COMPUTING PRECIP BUCKETS

### do precip buckets if model is ARW

# precip bucket subpieces can be changed to "1" if needed, as only will run for the first subpiece
while [ ! -e $DATA/prdgen_3km_${subpiece}/$filenamthree$onehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_3km_${subpiece}/$filenamthree$onehrprev.tm00
sleep 10
done


  rm PCP1HR${fhr}.tm00
  rm input.card
  echo "$DATA/prdgen_3km_${subpiece}" > input.card
  echo $filenamthree >> input.card
  echo $onehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

 export pgm=hiresw_pcpbucket_${DOMIN_bucket}
 $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
 export err=$?; err_chk

  if [ $model = "arw" ] ; then

# let fhr=fhr + 0
# typeset -Z2 fhr
	
	echo what is fhr here $fhr

  if [ ${fhr}%3 -eq 0 ]
  then

echo "3 hourly, do 3H precip bucket"

while [ ! -e $DATA/prdgen_3km_${subpiece}/$filenamthree$threehrprev.tm00 ]
do
echo waiting for $DATA/prdgen_3km_${subpiece}/$filenamthree$threehrprev.tm00
sleep 10
done


  rm PCP3HR${fhr}.tm00
  rm input.card
  echo "$DATA/prdgen_3km_${subpiece}" > input.card
  echo $filenamthree >> input.card
  echo $threehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

 export pgm=hiresw_pcpbucket_${DOMIN_bucket}
 $EXEChiresw/hiresw_pcpbucket_${DOMIN_bucket} < input.card >> $pgmout 2>errfile
 export err=$?; err_chk

	if [ $fhr -ne 3 ]
        then
         cat  ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00 > \
                                       hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2
        else
         cat  ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > \
                                       hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2
        fi

  fi # if 3hourly ARW

## all ARW will see this 
        if [ ${fhr}%3 -ne 0 ]
        then
        if [ $fhr -ne 1 ]
        then
        cat  ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > \
                                       hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2
        else
        cp ${filenamthree}${fhr}.tm00 hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2
        fi
        fi
     
  else
# not ARW

        if [ ${fhr}%3 -ne 1 ]
        then
   cat ${filenamthree}${fhr}.tm00  PCP1HR${fhr}.tm00  > hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2
        else
   cp ${filenamthree}${fhr}.tm00   hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2
        fi
  fi

else

cp ${filenamthree}${fhr}.tm00 hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2

fi # subpiece=1

###### DONE PRECIP BUCKET

#  if test $SENDCOM = 'YES'
#  then
     cp hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2 $DATA/hiresw.t${CYC}z.${model}_3km.f${fhr}.conus.grib2_${subpiece}
#  fi

fi # if f00 test

echo  "done" > $DATA/done_conus_3km_${subpiece}_f${fhr}
