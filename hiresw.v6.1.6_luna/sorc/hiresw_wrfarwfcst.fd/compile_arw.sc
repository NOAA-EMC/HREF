#! /bin/ksh --login

# module unload NetCDF/4.2/serial
# module load NetCDF/3.6.3
### BUILD ARW

export WRF_NMM_CORE=0
export WRF_EM_CORE=1

TARGDIR=../../exec

############################

./clean -a

cp configure.wrf_newer_cray configure.wrf

./compile em_real > compile_arw_fast.log 2>&1

if [ -e ./main/real.exe ]
then
cp ./main/real.exe $TARGDIR/hiresw_arw_real
cp ./main/wrf.exe  $TARGDIR/hiresw_arw_fcst
fi

############################

exit
