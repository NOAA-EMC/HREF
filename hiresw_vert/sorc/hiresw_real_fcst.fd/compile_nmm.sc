#! /bin/ksh --login

export WRF_NMM_CORE=1
export WRF_EM_CORE=0

module load netcdf

TARGDIR=../../exec

# ./clean -a
# cp configure.wrf_O3 configure.wrf

./compile nmm_real > compile_nmm.sc.log 2>&1

cp ./main/real_nmm.exe $TARGDIR/hiresw_nmm_real
cp ./main/wrf.exe  $TARGDIR/hiresw_nmm_fcst

