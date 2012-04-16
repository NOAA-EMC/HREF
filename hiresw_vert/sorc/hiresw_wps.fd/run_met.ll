# @ step_name = run_wps_met
# @ output = /meso/save/wx20py/WPS+WRFV2/WPS/met.log
# @ error = /meso/save/wx20py/WPS+WRFV2/WPS/met.log
# @ notification = never
# @ wall_clock_limit = 00:08:00
# @ job_type = parallel
# @ total_tasks = 15
# @ blocking=UNLIMITED
# @ arguments = NMM
# @ class=dev
# @ group=devonprod
# @ resources=ConsumableCPUS(1)ConsumableMemory(1 GB)
# @ node_usage=not_shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ queue


CORE=${1}
FIXDIR=/meso/save/wx20py/JIF_CODES/fix
PARMDIR=/meso/save/wx20py/JIF_CODES/parm
DOM=east
CYC=18

# cd /meso/save/wx20py/WPS+WRFV2/WPS/metgrid
cd /meso/save/wx20py/JIF_CODES/WPS_NMM/metgrid

ln -sf METGRID.TBL.${1} METGRID.TBL
cd ../

mkdir -p /meso/scrub/wx20py/run_metgrid

rm /meso/scrub/wx20py/run_metgrid/*

cp ./metgrid/METGRID.TBL metgrid.exe /meso/scrub/wx20py/run_metgrid/

cp ${PARMDIR}/hiresw_${DOM}_nmm.namelist.wps_in namelist.wps_in

start=`cat /com/date/t${CYC}z | cut -c7-16`
start=2007041518

ystart=`echo $start | cut -c1-4`
mstart=`echo $start | cut -c5-6`
dstart=`echo $start | cut -c7-8`
hstart=`echo $start | cut -c9-10`

end=`/nwprod/util/exec/ndate +48 $start`

yend=`echo $end | cut -c1-4`
mend=`echo $end | cut -c5-6`
dend=`echo $end | cut -c7-8`
hend=`echo $end | cut -c9-10`

cat namelist.wps_in | sed s:YSTART:${ystart}: | sed s:MSTART:${mstart}: | sed s:DSTART:${dstart}: \
                    | sed s:HSTART:${hstart}: | sed s:YEND:${yend}: | sed s:MEND:${mend}: \
                    | sed s:DEND:${dend}: | sed s:HEND:${hend}: > namelist.wps
                      
cp namelist.wps /meso/scrub/wx20py/run_metgrid/

cp ${FIXDIR}/hiresw_wps_static_${DOM}_nmm /meso/scrub/wx20py/run_metgrid/geo_nmm.d01.int

cd /meso/scrub/wx20py/run_metgrid

cp /meso/scrub/wx20py/run_ungrib/FILE* .

date
./metgrid.exe
date
