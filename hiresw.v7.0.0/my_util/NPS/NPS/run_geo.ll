# @ step_name = run_wps_geo
# @ output =geo.log
# @ error = geo.log
# @ notification = never
# @ wall_clock_limit = 00:03:00
# @ job_type = parallel
# @ total_tasks = 1
# @ arguments = SLA
#### @ blocking=UNLIMITED
# @ class=dev
# @ group=devonprod
# @ resources=consumablecpus(1)consumablememory(2000 MB)
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ queue


PACKDIR=/ptmp/wx20py/NMMB_init
WORKDIR=/stmp/wx20py/nmmb_init

cd $WORKDIR
rm $WORKDIR/geo*.dio


mkdir -p geogrid

cp $PACKDIR/NPS/geogrid/GEOGRID.TBL.${1} ./geogrid/GEOGRID.TBL
cp $PACKDIR/NPS/geogrid.exe .

cp $PACKDIR/namelist.nps .

cp $PACKDIR/NPS/geogrid/testb.nml .

ln -sf testb.nml fort.81

## ln -sf ../zj_topo_program/mnts mnts

rm -rf cored*

./geogrid.exe
