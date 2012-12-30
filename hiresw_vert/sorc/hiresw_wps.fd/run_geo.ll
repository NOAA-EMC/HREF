# @ step_name = run_wps_geo
# @ output =geo.log
# @ error = geo.log
# @ notification = never
# @ arguments=NMM
# @ wall_clock_limit = 00:12:00
# @ job_type = parallel
# @ total_tasks = 4
# @ blocking=UNLIMITED
# @ class=dev
# @ group=devonprod
# @ resources=consumablecpus(1)consumablememory(900 MB)
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ queue

ENDHR=48
CYC=00


cd /u/Matthew.Pyle/hiresw_vert/sorc/hiresw_wps.fd/geogrid

ln -sf GEOGRID.TBL.NMM GEOGRID.TBL

cd ..

./geogrid.exe
