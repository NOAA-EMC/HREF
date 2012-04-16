#! /bin/ksh

### BUILD ARW

export WRF_NMM_CORE=0
export WRF_EM_CORE=1
export NETCDF=/usrx/local/netcdf.3.5.0

TARGDIR=/nwtest/exec

./clean -a

cp configure.wrf_O3 configure.wrf

./compile em_real > compile_arw.sc.log 2>&1

cp ./main/real.exe $TARGDIR/hiresw_arw_real
cp ./main/wrf.exe  $TARGDIR/hiresw_arw_fcst

### BUILD NMM

export WRF_NMM_CORE=1
export WRF_EM_CORE=0

./clean -a

cp configure.wrf_O3 configure.wrf

./compile nmm_real > compile_nmm.sc.log 2>&1

cp ./main/real_nmm.exe $TARGDIR/hiresw_nmm_real
cp ./main/wrf.exe  $TARGDIR/hiresw_nmm_fcst
