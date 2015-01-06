#! /bin/ksh --login

# module load NetCDF/3.6.3

module load NetCDF/4.2/serial

### BUILD ARW

export WRF_NMM_CORE=0
export WRF_EM_CORE=1

TARGDIR=../../exec

############################

./clean -a
cp configure.wrf_wcoss configure.wrf

./compile em_real > compile_arw_fast.log 2>&1

cp ./main/real.exe $TARGDIR/hiresw_arw_real_v3.6.1
cp ./main/wrf.exe  $TARGDIR/hiresw_arw_fcst_v3.6.1

############################

exit

clean -a
cp configure.wrf_wcoss_precise configure.wrf

./compile em_real > compile_arw_precise.log 2>&1

cp ./main/real.exe $TARGDIR/hiresw_arw_real_precise
cp ./main/wrf.exe  $TARGDIR/hiresw_arw_fcst_precise

############################

exit