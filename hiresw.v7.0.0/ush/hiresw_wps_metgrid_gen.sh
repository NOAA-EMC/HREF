#! /bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         hiresw_wps_metgrid_gen.sh
# Script description:  Runs the WPS metgrid executable to interpolate data
#
# Author:        Eric Rogers       Org: NP22         Date: 2004-07-07
#
# Abstract: Runs the WPS program metgrid, which horizontally interpolates 
#           fields onto the target WRF domain.  For better parallel
#           performance, this script  is run in simultaneous threads
#           to process all 48-h of NAM input.  Input variables "ICOUNT", 
#           "STARTHR", and "ENDHR" set in the Loadleveler argument
#           card,  tells this script which NAM forecast GRIB files to process
#
# Script history log:
# 2003-11-01  Matt Pyle    - Original script for parallel
# 2004-07-07  Eric Rogers  - Preliminary modifications for production.
# 2007-04-19  Matthew Pyle - Completely renamed and revamped script 
#                            to run WPS metgrid.  Sufficiently flexible to run
#                            with a user-defined number of streams if the proper
#                            items are passed into the script.
                                                                                                                                   
set -x

INT=3
DOMNAM=$1
CYC=$2
STARTHR=$3
ENDHR=$4
ICOUNT=$5
MODEL=$6
SENDCOM=$7

MULTINUM_IN=$ICOUNT

### figure out start and end times for this segment

## NDATE=${NDATE:-${utilexec}/ndate}

ystart=`echo $PDY | cut -c1-4`
mstart=`echo $PDY | cut -c5-6`
dstart=`echo $PDY | cut -c7-8`
hstart=$CYC

orig_start=$ystart$mstart$dstart${CYC}
start=`$NDATE +${STARTHR} ${orig_start}`

ystart=`echo $start | cut -c1-4`
mstart=`echo $start | cut -c5-6`
dstart=`echo $start | cut -c7-8`
hstart=`echo $start | cut -c9-10`

end=`$NDATE +${ENDHR} ${orig_start}`

yend=`echo $end | cut -c1-4`
mend=`echo $end | cut -c5-6`
dend=`echo $end | cut -c7-8`
hend=`echo $end | cut -c9-10`

cd $DATA

myhost=`hostname`
echo "running " $ICOUNT "on" $myhost
date

mkdir -p ./run_metgrid_${ICOUNT}

if [ $MODEL = arw ] ; then
  cp $FIXhiresw/hiresw_wps_static_${DOMNAM}_${MODEL} $DATA/run_metgrid_${ICOUNT}/geo_em.d01.int
elif [ $MODEL = nmmb ] ; then
  cp $FIXhiresw/hiresw_nps_static_${DOMNAM}_${MODEL} $DATA/run_metgrid_${ICOUNT}/geo_nmb.d01.dio
fi

if [ $MODEL != nmmb ] ; then
cat $PARMhiresw/hiresw_${DOMNAM}_${MODEL}.namelist.wps_in  \
 | sed s:YSTART:$ystart: | sed s:MSTART:$mstart: \
 | sed s:DSTART:$dstart: | sed s:HSTART:${hstart}: | sed s:YEND:$yend: \
 | sed s:MEND:$mend:     | sed s:DEND:$dend:   \
 | sed s:HEND:$hend: > namelist.wps.${ICOUNT}
else
cat $PARMhiresw/hiresw_${DOMNAM}_${MODEL}.namelist.nps_in \
 | sed s:YSTART:$ystart: | sed s:MSTART:$mstart: \
 | sed s:DSTART:$dstart: | sed s:HSTART:${hstart}: | sed s:YEND:$yend: \
 | sed s:MEND:$mend:     | sed s:DEND:$dend:   \
 | sed s:HEND:$hend: > namelist.nps.${ICOUNT}
fi

mkdir -p ./run_metgrid_${ICOUNT}
cd  ./run_metgrid_${ICOUNT}

if [ $MODEL != nmmb ]
then
cp $EXEChiresw/hiresw_wps_metgrid  ./metgrid.exe
else
cp $EXEChiresw/hiresw_nps_metgrid  ./metgrid.exe
fi

cp $PARMhiresw/hiresw_METGRID.TBL.${MODEL} METGRID.TBL

if [ $MODEL != nmmb ] ; then
cp ../namelist.wps.${ICOUNT} namelist.wps
else
cp ../namelist.nps.${ICOUNT} namelist.nps
fi

### pull in needed files from ungrib job

FHR=$STARTHR
while [ $FHR -le $ENDHR ]
do
time=`$NDATE +${FHR} ${orig_start}`
yy=`echo $time | cut -c1-4`
mm=`echo $time | cut -c5-6`
dd=`echo $time | cut -c7-8`
hh=`echo $time | cut -c9-10`

# needs to be made more specific in conjuction with ungrib job


### provision for use from preprap job
if [ -e ../FILE:${yy}-${mm}-${dd}_${hh} ]
then
ln -sf ../FILE:${yy}-${mm}-${dd}_${hh} .
else
ln -sf $INPUT_DATA/hiresw.t${CYC}z.${DOMNAM}${MODEL}.FILE:${yy}-${mm}-${dd}_${hh} FILE:${yy}-${mm}-${dd}_${hh}
fi
export err=$?

if [ $err -ne 0 ]
then
echo "MISSING UNGRIB FILE - ABORT"
fi

err_chk

FHR=`expr $FHR + $INT`
done

./metgrid.exe

export err=$?
err_chk

echo "DONE" > wpsdone.${ICOUNT}

if [ $MODEL = arw ] ; then
files=`ls met*.bin`
elif [ $MODEL = nmmb ] ; then
files=`ls met*.dio`
fi


if [ $SENDCOM = 1 ] ; then
for fl in $files
do
mv ${fl} $DATA/hiresw.t${CYC}z.${DOMNAM}${MODEL}.${fl}
done
fi

if [ $ICOUNT == "1" ]
then
for fl in $files
do
cp ${fl} ../
done
fi

mv wpsdone.${ICOUNT} ../

cp metgrid.log $DATA/metgrid.log.0000_${ICOUNT}

echo EXITING $0
exit
