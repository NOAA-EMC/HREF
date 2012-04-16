#! /bin/ksh

DOM=$1
DATE=$2

cd /emc2/wx20py/wrf_nmm/buckets/

if [ $DOM = "westnmm" ]
then
filenam="wrf.WEST08"
elif [ $DOM = "westnmm_altphy" ]
then
filenam="wrf.WEST08"
elif [ $DOM = "westnmm_50lev" ]
then
filenam="wrf.WEST08"
elif [ $DOM = "eastnmm" ]
then
filenam="wrf.EAST08"
elif [ $DOM = "eastnmm_another" ]
then
filenam="wrf.EAST08"
elif [ $DOM = "eastnmm_gfs50" ]
then
filenam="wrf.EAST08"
elif [ $DOM = "centnmm" ]
then
filenam="wrf.CENT08"
elif [ $DOM = "centnmm_test" ]
then
filenam="wrf.CENT08"
elif [ $DOM = "centnmm_altbmj" ]
then
filenam="wrf.CENT08"
elif [ $DOM = "centnmmkf" ]
then
filenam="wrf.CENT08"
elif [ $DOM = "cent4km" ]
then
filenam="wrf.CENT04"
elif [ $DOM = "centnmm_netcdf" ]
then
filenam="wrf.CENT08"
elif [ $DOM = "test_bc" ]
then
filenam="wrf.CENT08"
elif [ $DOM = "aknmm" ]
then
filenam="wrf.AK08"
elif [ $DOM = "hinmm" ]
then
filenam="wrf.HI08"
fi

echo "/emc2/wx20py/wrf_nmm/$DOM/output/" > input.card
echo $filenam >> input.card
echo $DATE >> input.card

rm /emc2/wx20py/wrf_nmm/$DOM/output/$DATE/WRFPCP*

if [ $DOM = "westnmm" ]
then
WRFBUCKET.x_westnmm < input.card
elif [ $DOM = "westnmm_altphy" ]
then
WRFBUCKET.x_westnmm < input.card
elif [ $DOM = "westnmm_50lev" ]
then
WRFBUCKET.x_westnmm < input.card
elif [ $DOM = "eastnmm" ]
then
WRFBUCKET.x_eastnmm < input.card
elif [ $DOM = "eastnmm_another" ]
then
WRFBUCKET.x_eastnmm < input.card
elif [ $DOM = "eastnmm_gfs50" ]
then
WRFBUCKET.x_eastnmm < input.card
elif [ $DOM = "centnmm" ]
then
WRFBUCKET.x_eastnmm < input.card
elif [ $DOM = "centnmm_test" ]
then
WRFBUCKET.x_eastnmm < input.card
elif [ $DOM = "centnmm_altbmj" ]
then
WRFBUCKET.x_eastnmm < input.card
elif [ $DOM = "centnmm_rerun" ]
then
WRFBUCKET.x_eastnmm < input.card
elif [ $DOM = "centnmmkf" ]
then
WRFBUCKET.x_eastnmm < input.card
elif [ $DOM = "cent4km" ]
then
WRFBUCKET.x_4km < input.card
elif [ $DOM = "test_bc" ]
then
WRFBUCKET.x_eastnmm < input.card
elif [ $DOM = "aknmm" ]
then
WRFBUCKET.x_aknmm < input.card
elif [ $DOM = "hinmm" ]
then
WRFBUCKET.x_hinmm < input.card
fi
