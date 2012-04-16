# @ step_name = run_wps_met
# @ output = /meso/save/wx20py/WPS+WRFV2/WPS/met.log
# @ error = /meso/save/wx20py/WPS+WRFV2/WPS/met.log
# @ notification = never
# @ wall_clock_limit = 00:08:00
# @ job_type = parallel
# @ total_tasks = 12
# @ blocking=UNLIMITED
# @ arguments = NMM
# @ class=dev
# @ group=devonprod
# @ resources=ConsumableCPUS(1)ConsumableMemory(1 GB)
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ queue

cd /meso/save/wx20py/WPS+WRFV2/WPS/metgrid

ln -sf METGRID.TBL.${1} METGRID.TBL
cd ../

mkdir -p /meso/scrub/wx20py/run_metgrid

rm /meso/scrub/wx20py/run_metgrid/*

cp ./metgrid/METGRID.TBL metgrid.exe namelist.wps geo_nmm.d01* geo_em.d01*  /meso/scrub/wx20py/run_metgrid/

cd /meso/scrub/wx20py/run_metgrid

cp /meso/scrub/wx20py/run_ungrib/FILE* .

date
./metgrid.exe
date
