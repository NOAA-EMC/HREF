#! /bin/ksh --login

export WRF_NMM_CORE=0
export WRF_EM_CORE=1
# export NETCDF=/usrx/local/netcdf.3.5.0

module load netcdf

TARGDIR=../../exec

# ./clean -a
# cp configure.wrf_zeus configure.wrf

./compile em_real > compile_arw.sc.log 2>&1

cp ./main/real.exe $TARGDIR/hiresw_arw_real
cp ./main/wrf.exe  $TARGDIR/hiresw_arw_fcst
