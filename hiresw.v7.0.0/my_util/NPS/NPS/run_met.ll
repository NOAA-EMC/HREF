# @ step_name = run_wps_met
# @ output = met.log
# @ error = met.log
# @ notification = never
# @ wall_clock_limit = 00:05:00
# @ job_type = parallel
# @ total_tasks = 4
# @ node=1
# @ arguments = SLA
# @ class=dev
# @ group=devonprod
# @ resources=ConsumableCPUS(1)ConsumableMemory(2 GB)
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ queue

PACKDIR=/ptmp/wx20py/NMMB_init
WORKDIR=/stmp/wx20py/nmmb_init

cd $WORKDIR

mkdir metgrid

ls

cp $PACKDIR/namelist.nps .
cp $PACKDIR/NPS/metgrid/METGRID.TBL.${1} ./metgrid/METGRID.TBL
cp $PACKDIR/NPS/metgrid.exe .

./metgrid.exe
