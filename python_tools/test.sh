#! /bin/sh

module load prod_util

export cyc=${1}

DATEM0=`cat $COMROOT/date/t${cyc}z | cut -c7-14`
MMDD0=`echo $DATEM0 | cut -c5-8`

echo DATEM0 $DATEM0
echo MMDD0 $MMDD0

DATEM1=`$NDATE -24 ${DATEM0}00  | cut -c1-8`
MMDD1=`echo $DATEM1 | cut -c5-8`
DATEM2=`$NDATE -48 ${DATEM0}00  | cut -c1-8`
MMDD2=`echo $DATEM2 | cut -c5-8`
DATEM3=`$NDATE -72 ${DATEM0}00  | cut -c1-8`
MMDD3=`echo $DATEM3 | cut -c5-8`
DATEM4=`$NDATE -96 ${DATEM0}00  | cut -c1-8`
MMDD4=`echo $DATEM4 | cut -c5-8`
DATEM5=`$NDATE -120 ${DATEM0}00 | cut -c1-8`
MMDD5=`echo $DATEM5 | cut -c5-8`
DATEM6=`$NDATE -144 ${DATEM0}00 | cut -c1-8`
MMDD6=`echo $DATEM6 | cut -c5-8`

echo DATEM1 $DATEM1
echo DATEM6 $DATEM6
echo MMDD1 $MMDD1

cd /gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/python_tools


doms="conus ak pr hi conuscompare"

rm oldcycs2.html

if [ $cyc = "00" -o $cyc = "06" ]
then

for  dom in $doms
do
cat oldcycs2.html_innew_00_06_${dom} | sed s:_DATEM0_:${DATEM0}:g | sed s:_MMDD0_:${MMDD0}:g  \
                     | sed s:_DATEM1_:${DATEM1}:g | sed s:_MMDD1_:${MMDD1}:g  \
                     | sed s:_DATEM2_:${DATEM2}:g | sed s:_MMDD2_:${MMDD2}:g  \
                     | sed s:_DATEM3_:${DATEM3}:g | sed s:_MMDD3_:${MMDD3}:g  \
                     | sed s:_DATEM4_:${DATEM4}:g | sed s:_MMDD4_:${MMDD4}:g  \
                     | sed s:_DATEM5_:${DATEM5}:g | sed s:_MMDD5_:${MMDD5}:g  \
                     | sed s:_DATEM6_:${DATEM6}:g | sed s:_MMDD6_:${MMDD6}:g  > oldcycs_${dom}.html
done

elif [ $cyc = "12" -o $cyc = "18" ]
then

for  dom in $doms
do

cat oldcycs2.html_innew_12_18_${dom} | sed s:_DATEM0_:${DATEM0}:g | sed s:_MMDD0_:${MMDD0}:g  \
                     | sed s:_DATEM1_:${DATEM1}:g | sed s:_MMDD1_:${MMDD1}:g  \
                     | sed s:_DATEM2_:${DATEM2}:g | sed s:_MMDD2_:${MMDD2}:g  \
                     | sed s:_DATEM3_:${DATEM3}:g | sed s:_MMDD3_:${MMDD3}:g  \
                     | sed s:_DATEM4_:${DATEM4}:g | sed s:_MMDD4_:${MMDD4}:g  \
                     | sed s:_DATEM5_:${DATEM5}:g | sed s:_MMDD5_:${MMDD5}:g  \
                     | sed s:_DATEM6_:${DATEM6}:g | sed s:_MMDD6_:${MMDD6}:g  > oldcycs_${dom}.html
done

fi

scp oldcycs_*.html mpyle@emcrzdm.ncep.noaa.gov:/home/www/emc/htdocs/mmb/mpyle/href_v2awips/
