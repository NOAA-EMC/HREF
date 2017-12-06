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

fhr=$1
DOMIN_SMALL=$2
CYC=${3}
model=$4
subpiece=$5

compress="c3 -set_bitmap 1"

reflag=1

mkdir ${DATA}/prdgen_5km_${subpiece}
cd ${DATA}/prdgen_5km_${subpiece}/

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
  IM=1473
  JM=1025
fi

if [ $DOMIN = "conusarw" -o $DOMIN = "conusmem2arw" ]
then
  filenamthree="wrf.EMCONUS05"
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

if [ $DOMIN_SMALL = "conus"  -o $DOMIN_SMALL = "conusmem2" ]
then
cp $PARMhiresw/hiresw_conus_awp5km.txt_${subpiece} hiresw_grid_extract.txt
else
cp $PARMhiresw/hiresw_conus_awp5km.txt hiresw_grid_extract.txt
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



looplim=90
loop=1

while [ $loop -le $looplim ]
do
 echo in while
 if [ -s $INPUT_DATA/postdone${fhr} ]
 then
   break
 else
   loop=$((loop+1))
   sleep 20
 fi
 if [ $loop -ge $looplim ]
   then
   msg="FATAL ERROR: ABORTING after 30 minutes of waiting for $INPUT_DATA/postdone${fhr}"
   err_exit $msg
 fi
done


### extract just needed items

$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 | grep -F -f hiresw_grid_extract.txt | $WGRIB2 -i -grib inputs.grb $INPUT_DATA/WRFPRS${fhr}.tm00
export err=$?; err_chk

$WGRIB2  inputs.grb  -set_grib_type ${compress} -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${filenamthree}${fhr}.tm00_bilin
export err=$?; err_chk

if [ $subpiece = "1" ]
then
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(APCP|WEASD|SNOD):" -grib inputs_budget.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(HINDEX|TSOIL|SOILW|CSNOW|CICEP|CFRZR|CRAIN|RETOP|REFD|MAXREF):" -grib nn.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HGT:cloud ceiling:" -grib ceiling.grb
export err=$?; err_chk
cat nn.grb ceiling.grb > inputs_nn.grb

$WGRIB2  inputs_nn.grb -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${filenamthree}${fhr}.tm00_nn
export err=$?; err_chk

$WGRIB2  inputs_budget.grb -new_grid_interpolation budget -set_grib_type ${compress} -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${filenamthree}${fhr}.tm00_budget
export err=$?; err_chk
fi


if [ $subpiece = "1" ]
then
cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00_budget > ${filenamthree}${fhr}.tm00
else
mv ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00
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
#      cp ${filenamthree}${fhr}.tm00 $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2_${subpiece}
      cp ${filenamthree}${fhr}.tm00 $DATA/hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2_${subpiece}
#  fi
else

if [ $subpiece = "1" ]
then
echo COMPUTING PRECIP BUCKETS

### do precip buckets if model is ARW

# precip bucket subpieces can be changed to "1" if needed, as only will run for the first subpiece

looplim=90
loop=1
while [ $loop -le $looplim ]
do
 if [ -s $DATA/prdgen_5km_${subpiece}/$filenamthree$onehrprev.tm00 ]
 then
   break
 else
   loop=$((loop+1))
   sleep 10
 fi
 if [ $loop -ge $looplim ]
   then
   msg="FATAL ERROR: ABORTING after 15 minutes of waiting for $DATA/prdgen_5km_${subpiece}/$filenamthree$onehrprev.tm00"
   err_exit $msg
 fi
done

  rm PCP1HR${fhr}.tm00
  rm input.card
  echo "$DATA/prdgen_5km_${subpiece}" > input.card
  echo $filenamthree >> input.card
  echo $onehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

 export pgm=hiresw_bucket
 $EXEChiresw/hiresw_bucket < input.card >> $pgmout 2>errfile
 export err=$?; err_chk

  if [ $model = "arw" ] ; then

# let fhr=fhr + 0
# typeset -Z2 fhr
	
	echo what is fhr here $fhr

  if [ ${fhr}%3 -eq 0 ]
  then

echo "3 hourly, do 3H precip bucket"

looplim=90
loop=1
while [ $loop -le $looplim ]
do
 if [ -s $DATA/prdgen_5km_${subpiece}/$filenamthree$threehrprev.tm00 ]
 then
   break
 else
   loop=$((loop+1))
   sleep 10
 fi
 if [ $loop -ge $looplim ]
   then
   msg="FATAL ERROR: ABORTING after 15 minutes of waiting for $DATA/prdgen_5km_${subpiece}/$filenamthree$threehrprev.tm00"
   err_exit $msg
 fi
done

  rm PCP3HR${fhr}.tm00
  rm input.card
  echo "$DATA/prdgen_5km_${subpiece}" > input.card
  echo $filenamthree >> input.card
  echo $threehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

 export pgm=hiresw_bucket
 $EXEChiresw/hiresw_bucket < input.card >> $pgmout 2>errfile
 export err=$?; err_chk

	if [ $fhr -ne 3 ]
        then
         cat  ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 PCP3HR${fhr}.tm00 > \
                                       hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2
        else
         cat  ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > \
                                       hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2
        fi

  fi # if 3hourly ARW

## all ARW will see this 
        if [ ${fhr}%3 -ne 0 ]
        then
        if [ $fhr -ne 1 ]
        then
        cat  ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > \
                                       hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2
        else
        cp ${filenamthree}${fhr}.tm00 hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2
        fi
        fi
     
  else
# not ARW


# change logic for SNOWD change?  Could get messy

        if [ ${fhr}%3 -ne 1 ]
        then
   cat ${filenamthree}${fhr}.tm00  PCP1HR${fhr}.tm00  > hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2
        else
   cp ${filenamthree}${fhr}.tm00   hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2
        fi
  fi

else

cp ${filenamthree}${fhr}.tm00 hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2

fi # subpiece=1

###### DONE PRECIP BUCKET

#  if test $SENDCOM = 'YES'
#  then
#     cp $DOMOUT.t${CYC}z.awp5kmf${fhr} $COMOUT/$DOMOUT.t${CYC}z.awp5kmf${fhr}.grib2_${subpiece}
     cp hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2 $DATA/hiresw.t${CYC}z.${model}_5km.f${fhr}.conus.grib2_${subpiece}
#  fi

fi # if f00 test

echo  "done" > $DATA/done_conus_5km_${subpiece}_f${fhr}
