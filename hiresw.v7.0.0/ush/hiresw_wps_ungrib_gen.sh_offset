#! /bin/sh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         hiresw_wps_ungrib_gen.sh
# Script description:  Runs the WPS/NPS ungrib executable to process
#                      GRIB data
#
#
# Author:        Matthew Pyle       Org: NP22         Date: 2014-02-10
#
# Abstract: Runs the WPS/NPS program ungrib, which brings information
#           out of GRIB and writes to an intermediate binary file
#           that gets read by the next executable (metgrid).
#
#           For better parallel
#           performance, this script  is run in simultaneous threads
#           to process all 48-h of GFS input.  Input variable "stream", 
#           tells this script which set of forecast GRIB files to process
#
# Script history log:
# 2013-11-01  Matthew Pyle - Original script for parallel
# 2014-02-10  Matthew Pyle - Added this documentation block 

MODEL=${1}
stream=${2}

cd $DATA/run_ungrib_${stream}/

cp $PARMhiresw/hiresw_Vtable.${GRIBSRC} Vtable

if [ $MODEL != "nmmb" ]
then
cp ${DATA}/namelist.wps.${stream} namelist.wps
cp $EXEChiresw/hiresw_wps_ungrib ./ungrib.exe
export pgm=ungrib
startmsg
./ungrib.exe >> $pgmout 2>errfile
export err=$?
err_chk
cp ${DATA}/namelist.wps.${stream} ../run_ungrib/


else

cp ${DATA}/namelist.nps.${stream} namelist.nps
cp $EXEChiresw/hiresw_nps_ungrib ./ungrib.exe
export pgm=ungrib
startmsg
./ungrib.exe >> $pgmout 2>errfile
export err=$?
err_chk
cp ${DATA}/namelist.nps.${stream} ../run_ungrib/

fi

cp FILE* ../run_ungrib/
echo "DONE" > ../ungribdone.${stream}
