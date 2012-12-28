###########################################################
# Change log:
# 02/20/10: Jun Du, initial scripts
# 02/14/12: Jun Du, modified to the new SREFv6.0.0 by adding
#                   NMMB model, getting rid of Eta and RSM
#                   models and adjusting membership
#
###########################################################
set -x

wb=/nwprod/util/exec

num=$1
oldHR=$2
newHR=$3
hh2=$4
hh1=$5
p2=$6
model=$7
mem=$8
region=$9

if [ $model = nmb -o $model = nmm -o $model = eastnmm -o $model = westnmm -o $model = aknmm -o $model = prnmm -o $model = hinmm ]; then

 if [ $newHR -eq 03 -o $newHR -eq 06 -o $newHR -eq 09 -o $newHR -eq 12 -o $newHR -eq 15  -o $newHR -eq 18  -o $newHR -eq 21  -o $newHR -eq 24  -o $newHR -eq 27  -o $newHR -eq 30  -o $newHR -eq 33  -o $newHR -eq 36 -o $newHR -eq 39 -o $newHR -eq 42 -o $newHR -eq 45 -o $newHR -eq 48 ]; then
 let "p1=p2-3"
 fi

 if [ $newHR -eq 02 -o $newHR -eq 05 -o $newHR -eq 08 -o $newHR -eq 11 -o $newHR -eq 14 -o $newHR -eq 17  -o $newHR -eq 20  -o $newHR -eq 23  -o $newHR -eq 26  -o $newHR -eq 29  -o $newHR -eq 32  -o $newHR -eq 35 ]; then
 let "p1=p2-2"
 fi

 if [ $newHR -eq 01 -o $newHR -eq 04 -o $newHR -eq 07 -o $newHR -eq 10 -o $newHR -eq 13 -o $newHR -eq 16  -o $newHR -eq 19  -o $newHR -eq 22  -o $newHR -eq 25  -o $newHR -eq 28  -o $newHR -eq 31  -o $newHR -eq 34 ]; then
 let "p1=p2-1"
 fi

fi


if [ $model = eastarw -o $model = westarw -o $model = akarw -o $model = hiarw -o $model = prarw ]; then

 if [ $newHR -eq 03 -o $newHR -eq 06 -o $newHR -eq 09 -o $newHR -eq 12 -o $newHR -eq 15  -o $newHR -eq 18  -o $newHR -eq 21  -o $newHR -eq 24  -o $newHR -eq 27  -o $newHR -eq 30  -o $newHR -eq 33  -o $newHR -eq 36 -o $newHR -eq 39 -o $newHR -eq 42 -o $newHR -eq 45 -o $newHR -eq 48 ]; then
  let "p1=p2-3"
 else 
  let "p1=p2-1"
 fi

fi

if [ $model = em ]; then
 let "p1=p2-3"
fi

if [ $region = east -o $region = west ]; then
 inputgrid=212
fi
if [ $region = ak -o $region = pr ]; then
 inputgrid=221
echo set inputgrid to 221
fi
if [ $region = hi ]; then
 inputgrid=243
fi

#################################
mkdir -p $DATA/${num}_$newHR
cd $DATA/${num}_$newHR  || exit
rm -f $DATA/${num}_$newHR/*
sh $utilscript/setup.sh
##############################

# obtain SREF data:
###########################################################################
# hi-res window grids GDS definitions-
#East US (Lambert): "255,3,884,614,22100,-109800,8,-089000,5000,5000,0,64,38000,38000"
#West US (Lambert): "255,3,884,614,24500,-129200,8,-108000,5000,5000,0,64,40500,40500"
#Alaska (Polar Stereo):"255,5,825,603,44800,-174500,8,-150000,5000,5000,0,64" 
#Hawaii (latlon):      "255,0,223,170,16400,-162350,128,24005,-152360,45,45,64"
#Puerto Rico (latlon): "255,0,223,170,14400,-071500,128,22005,-061510,45,45,64"
#Haiti/Puerto Rico (latlon): "255,0,340,208,13500,-076590,128,22815,-061335,45,45,64"
###########################################################################

# select variables:
if [ $model = nmm -o $model = em -o $model = nmb ];then
 infile=${COMIN_SREF}/sref_${model}.t${hh2}z.pgrb${inputgrid}.${mem}.f$oldHR
 ls -l $infile
 needcheck=no
else
 infile=${COMIN_HIRESW}/${model}.t${hh1}z.awpreg${oldHR}.tm00
 needcheck=yes
fi



if [ $needcheck = yes ]
then
# likely unnecessary, but a potential safety item to prevent split-second
# issues as files are placed into /com/

teststr=`echo $model | cut -c1-2`

if [ $teststr = ea -o $teststr = we ]
then
  core=`echo $model | cut -c5-7`
fi

if [ $teststr = ak ]
then
  core=`echo $model | cut -c3-5`
fi

if [ $teststr = hi ]
then
  core=`echo $model | cut -c3-5`
fi

if [ $teststr = pr ]
then
  core=`echo $model | cut -c3-5`
fi

if [ $teststr = ha ]
then
  core=`echo $model | cut -c8-10`
fi


echo have $core

#### this needs fixing for operations

HIRESW_DATA=$DATA/../hiresw_${region}_${core}_ctl_${hh1}_${envir}

while [ ! -e $HIRESW_DATA/prdgendone${oldHR} ] 
do
echo waiting for $HIRESW_DATA/prdgendone${oldHR}
sleep 10
done

fi

# 221 grid is used for AK,PR but only in 3hrly
# 212 grid is used for East and West in 1hrly
# 243 grid is used for HI and is 3hrly


if [ $model = nmm -o $model = em -o $model = nmb \
  -o $model = eastnmm -o $model = westnmm -o $model = aknmm \
  -o $model = hinmm -o $model = prnmm  \
  -o $model = hiarw -o $model = prarw  \
  -o $model = eastarw -o $model = westarw -o $model = akarw ];then

echo past first if test

if [  \( $reg = ak  -o $reg = pr -o $reg = hi \) -a `expr $newHR % 3` -ne 0 ]; then

echo AK,HI,PR and not a 3-hrly file so quit
echo newHR is $newHR

 exit 0

else

echo not three hourly so looking for infile $infile

# check if input data is available
# icnt=1
# while [ $icnt -lt 1000 ]
# do
#  str=`ls -s $infile`  #get file size in block (1024 bytes)
#  echo "checking on icnt, found str " $icnt $str
#  set -A fsize $str
#  if [ -s $infile ] && [ ${fsize[0]} -gt 50 ] ; then
#    dump=0
#    break
#  else
#    sync
#    icnt=$((icnt + 1))
#    sleep 10
#    if [ $icnt -ge 360 ]
#     then
#     echo $infile "not exist"
#     msg="ABORTING after 60 minutes of waiting for $infile"
#     err_exit $msg
#    fi
#  fi
# done
fi # 3 hourly alaska
fi # model



echo $infile "is OK"
ls -l $infile
${wb}/wgrib $infile | grep ":TMP:kpds5=11:kpds6=105:kpds7=2:"    | ${wb}/wgrib -i -grib $infile -o t2
${wb}/wgrib $infile | grep ":UGRD:kpds5=33:kpds6=105:kpds7=10:"  | ${wb}/wgrib -i -grib $infile -o u10
${wb}/wgrib $infile | grep ":VGRD:kpds5=34:kpds6=105:kpds7=10:"  | ${wb}/wgrib -i -grib $infile -o v10
${wb}/wgrib $infile | grep ":RH:kpds5=52:kpds6=105:kpds7=2:"     | ${wb}/wgrib -i -grib $infile -o rh2
${wb}/wgrib $infile | grep ":PRMSL:kpds5=2:kpds6=102:kpds7=0:"   | ${wb}/wgrib -i -grib $infile -o slp
${wb}/wgrib $infile | grep ":MSLET:kpds5=130:kpds6=102:kpds7=0:"   | ${wb}/wgrib -i -grib $infile -o slp2
${wb}/wgrib $infile | grep ":APCP:kpds5=61:kpds6=1:kpds7=0:TR=4:P1=$p1:P2=$p2:" | ${wb}/wgrib -i -grib $infile -o apcp
${wb}/wgrib $infile | grep ":CAPE:kpds5=157:kpds6=1:kpds7=0:"    | ${wb}/wgrib -i -grib $infile -o cape
${wb}/wgrib $infile | grep ":CIN:kpds5=156:kpds6=1:kpds7=0:"     | ${wb}/wgrib -i -grib $infile -o cin
${wb}/wgrib $infile | grep ":HGT:kpds5=7:kpds6=100:kpds7=500:"   | ${wb}/wgrib -i -grib $infile -o z500
${wb}/wgrib $infile | grep ":RH:kpds5=52:kpds6=100:kpds7=850:"   | ${wb}/wgrib -i -grib $infile -o rh850
${wb}/wgrib $infile | grep ":UGRD:kpds5=33:kpds6=100:kpds7=850:" | ${wb}/wgrib -i -grib $infile -o u850
${wb}/wgrib $infile | grep ":VGRD:kpds5=34:kpds6=100:kpds7=850:" | ${wb}/wgrib -i -grib $infile -o v850
${wb}/wgrib $infile | grep ":UGRD:kpds5=33:kpds6=100:kpds7=250:" | ${wb}/wgrib -i -grib $infile -o u250
${wb}/wgrib $infile | grep ":VGRD:kpds5=34:kpds6=100:kpds7=250:" | ${wb}/wgrib -i -grib $infile -o v250
${wb}/wgrib $infile | grep ":TMP:kpds5=11:kpds6=100:kpds7=850:"  | ${wb}/wgrib -i -grib $infile -o t850
${wb}/wgrib $infile | grep ":TMP:kpds5=11:kpds6=100:kpds7=500:"  | ${wb}/wgrib -i -grib $infile -o t500
${wb}/wgrib $infile | grep ":TMP:kpds5=11:kpds6=100:kpds7=250:"  | ${wb}/wgrib -i -grib $infile -o t250
${wb}/wgrib $infile | grep ":SPFH:kpds5=51:kpds6=105:kpds7=2:"   | ${wb}/wgrib -i -grib $infile -o q2
${wb}/wgrib $infile | grep ":PRES:kpds5=1:kpds6=1:kpds7=0:"      | ${wb}/wgrib -i -grib $infile -o sfcp
${wb}/wgrib $infile | grep ":HGT:kpds5=7:kpds6=1:kpds7=0:"       | ${wb}/wgrib -i -grib $infile -o sfch

if [ -s slp2 ]
then
 cat t2 u10 v10 rh2 slp2 apcp cape cin z500 rh850 u850 v850 u250 v250 t850 t500 t250 q2 sfcp sfch > selected.data
 rm -f t2 u10 v10 rh2 slp slp2 apcp cape cin z500 rh850 u850 v850 u250 v250 t850 t500 t250 q2 sfcp sfch
else
 cat t2 u10 v10 rh2 slp apcp cape cin z500 rh850 u850 v850 u250 v250 t850 t500 t250 q2 sfcp sfch > selected.data
 rm -f t2 u10 v10 rh2 slp slp2 apcp cape cin z500 rh850 u850 v850 u250 v250 t850 t500 t250 q2 sfcp sfch
fi

ls -l selected.data

# convert to a common grid:
if [ $model = nmm -o $model = em -o $model = nmb ];then

 if [ $region = east ];then
$utilexec/copygb -x -g "255,3,884,614,22100,-109800,8,-89000,5000,5000,0,64,38000,38000" selected.data $DATA/r_gribawips${num}.f$newHR
 fi

 if [ $region = west ];then
$utilexec/copygb -x -g "255,3,884,614,24500,-129200,8,-108000,5000,5000,0,64,40500,40500" selected.data $DATA/r_gribawips${num}.f$newHR
 fi

 if [ $region = ak ];then
$utilexec/copygb -x -g "255,5,825,603,44800,-174500,8,-150000,5000,5000,0,64" selected.data $DATA/r_gribawips${num}.f$newHR
 fi

 if [ $region = hi ];then
$utilexec/copygb -x -g "255,0,223,170,16400,-162350,128,24005,-152360,45,45,64" selected.data $DATA/r_gribawips${num}.f$newHR
 fi

 if [ $region = pr ];then
$utilexec/copygb -x -g "255,0,340,208,13500,-76590,128,22815,-61335,45,45,64" selected.data $DATA/r_gribawips${num}.f$newHR
 fi

else
cp selected.data $DATA/r_gribawips${num}.f$newHR
fi
rm -f selected.data
