#! /bin/sh


NEST=${1}
MODEL=${2}
cyc=${3}
envir=${4}
stream=${5}

echo DATA $DATA

cd $DATA/run_ungrib_${stream}/
cp ../ungrib.exe .
cp $PARMhiresw/hiresw_Vtable.GFS Vtable

if [ $MODEL != "nmmb" ]
then
cp ../namelist.wps.${stream} namelist.wps
cp $EXEChiresw/hiresw_ungrib ungrib.exe
./ungrib.exe >> $pgmout 2>errfile
cp namelist.wps.${stream} ../run_ungrib/


else

cp ../namelist.nps.${stream} namelist.nps
cp $EXEChiresw/hiresw_nps_ungrib ungrib.exe
./ungrib.exe
cp namelist.nps ../run_ungrib/namelist.nps.${stream}

fi

cp FILE* ../run_ungrib/
echo "DONE" > ../ungribdone.${stream}
