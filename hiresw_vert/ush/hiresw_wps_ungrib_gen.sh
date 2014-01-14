#! /bin/sh


NEST=${1}
MODEL=${2}
cyc=${3}
envir=${4}
stream=${5}

### DATA should not be defined here
export DATA=/ptmpd1/Matthew.Pyle/tmpnwprd/hiresw_${NEST}_${MODEL}_ctl_${cyc}_${envir}
echo DATA $DATA

cd $DATA/run_ungrib_${stream}/
cp ../ungrib.exe .
cp $PARMhiresw/hiresw_Vtable.GFS Vtable

if [ $MODEL != "nmmb" ]
then
cp ../namelist.wps.${stream} namelist.wps
# cp $EXEChiresw/hiresw_ungrib_new ungrib.exe
cp $EXEChiresw/hiresw_ungrib_3.5_src ungrib.exe
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
