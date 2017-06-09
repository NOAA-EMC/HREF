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

# export WGRIB2=${WGRIB2:-${utilexec}/wgrib2}

fhr=$1
DOMIN_SMALL=$2
CYC=$3
model=$4
subpiece=${5}

reflag=1
compress="c3 -set_bitmap 1"

mkdir ${DATA}/prdgen_full

if [ $DOMIN_SMALL = "conus" -o $DOMIN_SMALL = "conusmem2" ]
then
 mkdir ${DATA}/prdgen_full_${subpiece}
 cd ${DATA}/prdgen_full_${subpiece}/
else
 cd ${DATA}/prdgen_full/
fi

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
  rg="conus"
  gres="2p5km"
  IM=2145
  JM=1377
  reg="30 6 0 0 0 0 0 0 2145 1377 20192000 238446000 136 25000000 265000000 2540000 2540000 0 64 25000000 25000000"
  wgrib2def="lambert:265:25:25 238.446:2145:2540 20.192:1377:2540"
elif [ $DOMIN = "aknmmb" ]
then
  filenamthree="wrf.AK04"
  rg="ak"
  gres="3km"
  IM=1649
  JM=1105
#  reg="20 6 0 0 0 0 0 0 1649 1105 40530000 -178571000 136 60000000 -150000000 2976000 2976000 0 64"
  reg="20 6 0 0 0 0 0 0 1649 1105 40530000 181429000 136 60000000 2100000000 2976000 2976000 0 64"
  wgrib2def="nps:210:60 181.429:1649:2976 40.53:1105:2976"
elif [ $DOMIN = "guamnmmb" ]
then
  filenamthree="wrf.GU04"
  rg="guam"
  gres="2p5km"
  IM=193
  JM=193
  reg="10 6 0 0 0 0 0 0 193 193 12350000 143687000 136 20000000 16794000 148280000 64 0 2500000 2500000"
  wgrib2def="mercator:20 143.687:193:2500:148.280 12.35:193:2500:16.794"
elif [ $DOMIN = "hinmmb" ]
then
  filenamthree="wrf.HI04"
  rg="hi"
  gres="2p5km"
  IM=321
  JM=225
  reg="10 6 0 0 0 0 0 0 321 225 18073000 198475000 136 20000000 23088000 206131000 64 0 2500000 2500000"
  wgrib2def="mercator:20 198.475:321:2500:206.131 18.073:225:2500:23.088"
elif [ $DOMIN = "prnmmb" ]
then
  filenamthree="wrf.PR04"
  rg="pr"
  gres="2p5km"
  IM=177
  JM=129
  reg="10 6 0 0 0 0 0 0 177 129 16829000 291804000 136 20000000 19747000 296028000 64 0 2500000 2500000"
  wgrib2def="mercator:20 291.804:177:2500:296.028 16.829:129:2500:19.747"
fi

if [ $DOMIN = "akarw" -o $DOMIN = "akmem2arw" ]
then
  filenamthree="wrf.EMAK04"
  rg="ak"
  gres="3km"
  IM=1649
  JM=1105
  reg="20 6 0 0 0 0 0 0 1649 1105 40530000 -178571000 136 60000000 -150000000 2976000 2976000 0 64"
  reg="20 6 0 0 0 0 0 0 1649 1105 40530000 181429000 136 60000000 2100000000 2976000 2976000 0 64"
  wgrib2def="nps:210:60 181.429:1649:2976 40.53:1105:2976"
elif [ $DOMIN = "conusarw" -o $DOMIN = "conusmem2arw" ]
then
  filenamthree="wrf.EMCONUS04"
  rg="conus"
  gres="2p5km"
  IM=2145
  JM=1377
#  reg="30 6 0 0 0 0 0 0 2145 1377 20192000 -121554000 136 25000000 -95000000 2540000 2540000 0 64 25000000 25000000"
  reg="30 6 0 0 0 0 0 0 2145 1377 20192000 238446000 136 25000000 265000000 2540000 2540000 0 64 25000000 25000000"
  wgrib2def="lambert:265:25:25 238.446:2145:2540 20.192:1377:2540"
elif [ $DOMIN = "guamarw" ]
then
  filenamthree="wrf.EMGU04"
  rg="guam"
  gres="2p5km"
  IM=193
  JM=193
  reg="10 6 0 0 0 0 0 0 193 193 12350000 143687000 136 20000000 16794000 148280000 64 0 2500000 2500000"
  wgrib2def="mercator:20 143.687:193:2500:148.280 12.35:193:2500:16.794"
elif [ $DOMIN = "hiarw" -o $DOMIN = "himem2arw" ]
then
  filenamthree="wrf.EMHI04"
  rg="hi"
  gres="2p5km"
  IM=321
  JM=225
#  reg="10 6 0 0 0 0 0 0 321 225 18073000 -161525000 136 20000000 23088000 -153869000 64 0 2500000 2500000"
  reg="10 6 0 0 0 0 0 0 321 225 18073000 198475000 136 20000000 23088000 206131000 64 0 2500000 2500000"
  wgrib2def="mercator:20 198.475:321:2500:206.131 18.073:225:2500:23.088"
elif [ $DOMIN = "prarw" -o $DOMIN = "prmem2arw"  ]
then
  filenamthree="wrf.EMPR04"
  rg="pr"
  gres="2p5km"
  IM=177
  JM=129
#  reg="10 6 0 0 0 0 0 0 177 129 16829000 -68196000 136 20000000 19747000 -63972000 64 0 2500000 2500000"
  reg="10 6 0 0 0 0 0 0 177 129 16829000 291804000 136 20000000 19747000 296028000 64 0 2500000 2500000"
  wgrib2def="mercator:20 291.804:177:2500:296.028 16.829:129:2500:19.747"
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


if [ $DOMIN_SMALL = "conus" -o  $DOMIN_SMALL = "conusmem2" ]
then
cp $PARMhiresw/hiresw_ndfd.txt_3h_conus_${subpiece} hiresw_grid_extract.txt
else
cp $PARMhiresw/hiresw_ndfd.txt_3h hiresw_grid_extract.txt
fi

use_3h=1

elif [ $fhr%3 -eq 0 ]
then

if [ $DOMIN_SMALL = "conus" -o  $DOMIN_SMALL = "conusmem2" ]
then
cp $PARMhiresw/hiresw_ndfd.txt_3h_conus_${subpiece} hiresw_grid_extract.txt
else
cp $PARMhiresw/hiresw_ndfd.txt_3h hiresw_grid_extract.txt

fi
use_3h=1

else


if [ $DOMIN_SMALL = "conus" -o  $DOMIN_SMALL = "conusmem2" ]
then
cp $PARMhiresw/hiresw_ndfd.txt_1h_conus_${subpiece} hiresw_grid_extract.txt
else
cp $PARMhiresw/hiresw_ndfd.txt_1h hiresw_grid_extract.txt
fi
use_1h=1

fi


# use *ndfd.txt_1h and *ndfd.txt_3h files?

echo use_1h $use_1h
echo use_3h $use_3h

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

$WGRIB2  inputs.grb  -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_bilin
export err=$?; err_chk

if [ $DOMIN_SMALL = "conus"  -a $subpiece = "1" ] 
then
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(APCP|WEASD):" -grib inputs_budget.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":HGT:cloud ceiling" -grib inputs_budget_b.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":HGT:cloud base" -grib inputs_budget_c.grb
export err=$?; err_chk
cat inputs_budget_b.grb >> inputs_budget.grb
cat inputs_budget_c.grb >> inputs_budget.grb
$WGRIB2 inputs_budget.grb -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_budget
export err=$?; err_chk
fi

if [ $DOMIN_SMALL = "conusmem2"  -a $subpiece = "1" ] 
then
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(APCP|WEASD):" -grib inputs_budget.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":HGT:cloud ceiling" -grib inputs_budget_b.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":HGT:cloud base" -grib inputs_budget_c.grb
export err=$?; err_chk
cat inputs_budget_b.grb >> inputs_budget.grb
cat inputs_budget_c.grb >> inputs_budget.grb
$WGRIB2 inputs_budget.grb -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_budget
export err=$?; err_chk
fi

if [ $DOMIN_SMALL != "conus" -a $DOMIN_SMALL != "conusmem2" ] 
then
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":(APCP|WEASD):" -grib inputs_budget.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":HGT:cloud ceiling" -grib inputs_budget_b.grb
export err=$?; err_chk
$WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match ":HGT:cloud base" -grib inputs_budget_c.grb
export err=$?; err_chk
cat inputs_budget_b.grb >> inputs_budget.grb
cat inputs_budget_c.grb >> inputs_budget.grb
$WGRIB2 inputs_budget.grb -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_budget
export err=$?; err_chk
fi

     if [ $fhr -ne 0 ]
     then
     check=$fhr%3
     else
     check=9
     fi

     if [ $check -ne 0 ]
     then


	echo inside the check block with DOMIN_SMALL $DOMIN_SMALL


#####
if [ $DOMIN_SMALL = "conus" -o $DOMIN_SMALL = "conusmem2" ]
then

if [ $subpiece = "1" ]
then
  $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HINDEX" -grib nn.grb
  export err=$?; err_chk
  $WGRIB2 nn.grb  -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_nn
  export err=$?; err_chk
  cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00_budget  > ${filenamthree}${fhr}.tm00
else
  mv ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00
fi

fi
#####

if [ $DOMIN_SMALL != "conus" -a $DOMIN_SMALL != "conusmem2" ]
then
  $WGRIB2 $INPUT_DATA/WRFPRS${fhr}.tm00 -match "HINDEX" -grib nn.grb
  export err=$?; err_chk
  $WGRIB2 nn.grb  -new_grid_interpolation neighbor -set_grib_type ${compress} -new_grid_winds grid -new_grid ${wgrib2def} ${filenamthree}${fhr}.tm00_nn
  export err=$?; err_chk
  cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_nn ${filenamthree}${fhr}.tm00_budget  > ${filenamthree}${fhr}.tm00
fi

     else # 3 hour time

	echo DOMIN_SMALL $DOMIN_SMALL 
	echo subpiece $subpiece

if [ $DOMIN_SMALL = "conus" -o $DOMIN_SMALL = "conusmem2" ]
then

if [ $subpiece = "1" ]
then
  cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_budget > ${filenamthree}${fhr}.tm00
else
  mv ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00
fi

fi

if [ $DOMIN_SMALL != "conus" -a $DOMIN_SMALL != "conusmem2" ]
then
  cat ${filenamthree}${fhr}.tm00_bilin ${filenamthree}${fhr}.tm00_budget > ${filenamthree}${fhr}.tm00
fi

fi

export err=$?; err_chk

cp $INPUT_DATA/WRFPRS${fhr}.tm00 $COMOUT/hiresw.t${CYC}z.$DOMOUT.wrfprs${fhr}

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


if [ $DOMIN_SMALL = "conus" -o $DOMIN_SMALL = "conusmem2" ] 
then
	echo COPYING f00 file to hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2_${subpiece}
       cp ${filenamthree}${fhr}.tm00 ${DATA}/hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2_${subpiece}
else
       cp ${filenamthree}${fhr}.tm00 ${DATA}/hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2
fi


### will be cat'd to smartinit generated file in ncoproc script
### no need to index, convert, or alert this file (which is not the final one for output)

  
else  # (not f00)



valcheck=`echo $DOMIN_SMALL | cut -c1-5`

if [ $subpiece = "1" -o $valcheck != "conus" ]
then


### do one hour precip for everyone (arw,nmm)

  rm PCP3HR${fhr}.tm00
  rm PCP1HR${fhr}.tm00
  rm input.card

if [ $subpiece = "1" ]
then
  echo "$DATA/prdgen_full_1" > input.card
else
  echo "$DATA/prdgen_full" > input.card
fi

  echo $filenamthree >> input.card
  echo $onehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card


if [ $subpiece = "1" ]
then

looplim=90
loop=1
while [ $loop -le $looplim ]
do
 echo in while
 if [ -s $DATA/prdgen_full_1/$filenamthree$onehrprev.tm00 ]
 then
   break
 else
   loop=$((loop+1))
   sleep 20
 fi
 if [ $loop -ge $looplim ]
   then
   msg="FATAL ERROR: ABORTING after 30 minutes of waiting for $DATA/prdgen_full_1/$filenamthree$onehrprev.tm00"
   err_exit $msg
 fi
done

else

looplim=90
loop=1
while [ $loop -le $looplim ]
do
 echo in while
 if [ -s $DATA/prdgen_full/$filenamthree$onehrprev.tm00 ]
 then
   break
 else
   loop=$((loop+1))
   sleep 20
 fi
 if [ $loop -ge $looplim ]
   then
   msg="FATAL ERROR: ABORTING after 30 minutes of waiting for $DATA/prdgen_full/$filenamthree$onehrprev.tm00"
   err_exit $msg
 fi
done

fi


   export pgm=hiresw_bucket
   $EXEChiresw/hiresw_bucket < input.card >> $pgmout 2>errfile
#   export err=$?; err_chk

     if [ $model = "arw" ] ; then

     if [ $fhr%3 -eq 0 ]
     then

#       create a 3 h bucket as well

if [ $subpiece = "1" ]
then

looplim=90
loop=1
while [ $loop -le $looplim ]
do
 echo in while
 if [ -s $DATA/prdgen_full_1/$filenamthree$threehrprev.tm00 ]
 then
   break
 else
   loop=$((loop+1))
   sleep 20
 fi
 if [ $loop -ge $looplim ]
   then
   msg="FATAL ERROR: ABORTING after 30 minutes of waiting for $DATA/prdgen_full_1/$filenamthree$threehrprev.tm00"
   err_exit $msg
 fi
done

else

looplim=90
loop=1
while [ $loop -le $looplim ]
do
 echo in while
 if [ -s $DATA/prdgen_full/$filenamthree$threehrprev.tm00 ]
 then
   break
 else
   loop=$((loop+1))
   sleep 10
 fi
 if [ $loop -ge $looplim ]
   then
   msg="FATAL ERROR: ABORTING after 15 minutes of waiting for $DATA/prdgen_full/$filenamthree$threehrprev.tm00"
   err_exit $msg
 fi
done

fi

  rm input.card
if [ $subpiece = "1" ]
then
  echo "$DATA/prdgen_full_1" > input.card
else
  echo "$DATA/prdgen_full" > input.card
fi
  echo $filenamthree >> input.card
  echo $threehrprev >> input.card
  echo $fhr >> input.card
  echo $reflag >> input.card
  echo $IM $JM >> input.card

  export pgm=hiresw_bucket
  $EXEChiresw/hiresw_bucket < input.card >> $pgmout 2>errfile
#   export err=$?; err_chk

     cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00 > hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2

     else # not $fhr%3=0

     cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2

     fi

   else

## model = "nmm"

     cat ${filenamthree}${fhr}.tm00 PCP1HR${fhr}.tm00  > hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2


  fi  # arw/nmm break

else

mv ${filenamthree}${fhr}.tm00 hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2

fi # subpiece=1 or non-conus

###### DONE PRECIP BUCKET

fi # f00 or not

echo DOWN HERE

## temp copy to $DATA
	if [ $subpiece -gt 0 ]
        then
         echo copying a grib2 to DATA $DATA
         cp hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2 \
	 ${DATA}/hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2_${subpiece}
        else
	if [ -e hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2 ] 
        then
         cp  hiresw.t${CYC}z.${model}_${gres}.f${fhr}.${DOMIN_SMALL}.grib2 ${DATA}/
	fi
        fi
## temp copy to $DATA

echo  "done" > $DATA/done_ndfd_${subpiece}_f${fhr}
