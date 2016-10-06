#! /bin/ksh --login

# module unload NetCDF/4.2/serial

# module load NetCDF/3.6.3

### BUILD ARW

export WRF_NMM_CORE=0
export WRF_EM_CORE=1

TARGDIR=../../exec

############################

./clean -a
cp configure.wrf_wcoss configure.wrf

./compile -j4 em_real > compile_arw_fast.log 2>&1

cp ./main/real.exe $TARGDIR/hiresw_arw_real
cp ./main/wrf.exe  $TARGDIR/hiresw_arw_fcst

############################

exit

clean -a
cp configure.wrf_wcoss_precise configure.wrf

./compile em_real > compile_arw_precise.log 2>&1

cp ./main/real.exe $TARGDIR/hiresw_arw_real_precise
cp ./main/wrf.exe  $TARGDIR/hiresw_arw_fcst_precise

############################

exit
