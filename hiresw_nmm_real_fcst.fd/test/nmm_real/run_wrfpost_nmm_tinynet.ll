#!/bin/ksh
#
#@ output = out.post
#@ error = err.post
#@ job_type = parallel
#@ class = dev
#@ total_tasks = 4
#@ blocking=unlimited
#@ wall_clock_limit = 00:29:00
#@ network.MPI = csss,shared,us
#@ queue
#
#

DOM=${1}
CYC=${2}
######
INCR=03
######

# EXE=wrfpost.x_nmm_mycode
EXE=wrfpost.x_nmm_bin_tiny


OUTTYP=netcdf
MODEL=NMM

filedir=/gpfstmp/wx20py/tmp_wrfreal_${DOM}

bindir=/emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real

mkdir -p /gpfstmp/wx20py/wrfpost_nmmtest_$DOM
cd /gpfstmp/wx20py/wrfpost_nmmtest_$DOM
rm -rf /gpfstmp/wx20py/wrfpost_nmmtest_${DOM}/*

cp $filedir/t${CYC}z .
cp $filedir/namelist.input .

YYYY=`cat t${CYC}z | cut -c7-10`
MM=`cat t${CYC}z | cut -c11-12`
DD=`cat t${CYC}z | cut -c13-14`

startd=$YYYY$MM$DD
dirout=/emc2/wx20py/wrf_nmm/${DOM}/${startd}${CYC}

STARTDATE=${YYYY}-${MM}-${DD}T${CYC}:00:00

datetmp=`$bindir/tomorrow $YYYY$MM$DD`
endtime=`$bindir/tomorrow $datetmp`


YYYY=`echo $endtime | cut -c1-4`
MM=`echo $endtime | cut -c5-6`
DD=`echo $endtime | cut -c7-8`
FINALDATE=${YYYY}-${MM}-${DD}T${CYC}:00:00

export tmmark=tm00

wyr=`echo $STARTDATE | cut -c1-4`
wmn=`echo $STARTDATE | cut -c6-7`
wdy=`echo $STARTDATE | cut -c9-10`
whr=`echo $STARTDATE | cut -c12-13`

eyr=`echo $FINALDATE | cut -c1-4`
emn=`echo $FINALDATE | cut -c6-7`
edy=`echo $FINALDATE | cut -c9-10`
ehr=`echo $FINALDATE | cut -c12-13`

echo $STARTDATE
echo $FINALDATE

edate=$eyr$emn$edy$ehr

wdate=$wyr$wmn$wdy$whr

timeform=$STARTDATE

cp /emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real/wrf_cntrl_central08.all ./wrf_cntrl.parm
# cp /emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real/wrf_cntrl_central08_noabsv.all ./wrf_cntrl.parm

echo $wdate $edate

while [ $wdate -le $edate ]
do
whr=`expr $whr + $INCR`

if [ $whr -ge 24 ]
then
whr=`expr $whr - 24`
# wdy=`expr $wdy + 1`

date=$wyr$wmn$wdy

echo "old date" $date
date=`$bindir/tomorrow $wyr$wmn$wdy`

wyr=`echo $date | cut -c1-4`
wmn=`echo $date | cut -c5-6`
wdy=`echo $date | cut -c7-8`
fi

if [ $whr -lt 10 ]
then
whr=0$whr
fi

echo "working on " $timeform

echo "making itag, here timeform is: " $timeform

cat > itag <<EOF
$filedir/wrfout_d01_${timeform}
$OUTTYP
$timeform
$MODEL
EOF

# cat > itag <<EOF
# $filedir/wrfout_0824_tb_f00
# $OUTTYP
# $timeform
# $MODEL
# EOF

#-----------------------------------------------------------------------
#   Run etapost.

ln -sf wrf_cntrl.parm fort.14

echo "executing the post"
# /emc2/wx20py/wrf_nmm/nmm_wrfpost/$EXE < itag > outpost.$wdate
/emc2/wx20py/wrf_nmm/unified_wrfpost/$EXE < itag > outpost.$wdate
# /emc2/wx20py/wrf_nmm/nmm_wrfpost/$EXE < itag > outpost.$wdate
# echo "done executing the post"

err=$?

echo "exit code= " $err
echo "just ran wrfpost with this in itag:"

cat itag
echo "done"

wdate=$wyr$wmn$wdy$whr

timeform=${wyr}"-"${wmn}"-"${wdy}"T"${whr}":00:00"
echo "wdate at end " $wdate
echo "edate at end " $edate
done

# rcp WRFPRS*.tm00  hanfs2:${dirout}/

exit
