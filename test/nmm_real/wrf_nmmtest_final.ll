#@job_name=wrfnmm
#@notification=never
#@output=wrf_nmm.o
#@error=wrf_nmm.e
#@job_type = parallel
#@total_tasks = 33
#@blocking=unlimited
#@network.MPI = csss,shared,us
#@class=dev
#@wall_clock_limit=02:29:00
#@queue


cd /gpfstmp/wx20py/tmp_wrfreal_${1}

export OMP_MAX_THREADS=1

rm wrf.exe
rm rsl.*
rm show_dom*
rm wrfout*
rm -rf core*

cp /emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real/wrf.exe .
cp /emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real/LANDUSE.TBL .
cp /emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real/RRTM_DATA .
cp /emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real/tr* .
cp /emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real/eta* .

chmod u+x ./wrf.exe

export INP=/emc2/wx22tb/input08
ln -s -f $INP/co2.60_hyb_bot40m fort.14

./wrf.exe

date
