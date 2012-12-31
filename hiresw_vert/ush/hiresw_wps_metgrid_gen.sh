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

MULTINUM_IN=$ICOUNT

### figure out start and end times for this segment

NDATE=/nwprod/util/exec/ndate

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

mkdir -p ./run_metgrid_${ICOUNT}

if [ $MODEL = nmm ] ; then
  cp $FIXhiresw/hiresw_wps_static_${DOMNAM}_${MODEL} $DATA/run_metgrid_${ICOUNT}/geo_nmm.d01.int
else
  cp $FIXhiresw/hiresw_wps_static_${DOMNAM}_${MODEL} $DATA/run_metgrid_${ICOUNT}/geo_em.d01.int
fi

cat $PARMhiresw/hiresw_${DOMNAM}_${MODEL}.namelist.wps_in  \
 | sed s:YSTART:$ystart: | sed s:MSTART:$mstart: \
 | sed s:DSTART:$dstart: | sed s:HSTART:${hstart}: | sed s:YEND:$yend: \
 | sed s:MEND:$mend:     | sed s:DEND:$dend:   \
 | sed s:HEND:$hend: > namelist.wps.${ICOUNT}

mkdir -p ./run_metgrid_${ICOUNT}
cd  ./run_metgrid_${ICOUNT}

cp $EXEChiresw/hiresw_metgrid  .
cp $PARMhiresw/hiresw_METGRID.TBL.${MODEL} METGRID.TBL
cp ../namelist.wps.${ICOUNT} namelist.wps

### pull in needed files from ungrib job

FHR=$STARTHR
while [ $FHR -le $ENDHR ]
do
time=`$NDATE +${FHR} ${orig_start}`
yy=`echo $time | cut -c1-4`
mm=`echo $time | cut -c5-6`
dd=`echo $time | cut -c7-8`
hh=`echo $time | cut -c9-10`
cp $DATA/run_ungrib/FILE:${yy}-${mm}-${dd}_${hh} .
FHR=`expr $FHR + $INT`
done

./hiresw_metgrid
export err=$?; $DATA/err_chk

echo "DONE" > wpsdone.${ICOUNT}

mv met*.int met*.bin  $DATA/.
mv wpsdone.${ICOUNT} ../

cp metgrid.log $DATA/metgrid.log.0000_${ICOUNT}
#cp metgrid.log.0000 $DATA/metgrid.log.0000_${ICOUNT}

echo EXITING $0
exit
