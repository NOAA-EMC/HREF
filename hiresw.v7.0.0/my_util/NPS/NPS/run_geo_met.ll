# @ step_name = run_wps_geo
# @ output =geo.log
# @ error = geo.log
# @ notification = never
# @ wall_clock_limit = 00:12:00
# @ job_type = parallel
# @ total_tasks = 1
# @ blocking=UNLIMITED
# @ class=dev
# @ arguments = NMM
# @ group=devonprod
# @ resources=consumablecpus(1)consumablememory(1500 MB)
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ executable = /gpfs/m/meso/noscrub/wx20py/WPS+WRFV2/WPS_bgrid/run_geo.ll
# @ queue

# @ dependency = (run_wps_geo == 0) 
# @ step_name = run_wps_met
# @ output = /gpfs/m/meso/noscrub/wx20py/WPS+WRFV2/WPS_new/met.log
# @ error = /gpfs/m/meso/noscrub/wx20py/WPS+WRFV2/WPS_new/met.log
# @ notification = never
# @ wall_clock_limit = 00:02:00
# @ job_type = parallel
# @ total_tasks = 1
# @ blocking=UNLIMITED
# @ arguments = NMM
# @ class=dev
# @ group=devonprod
# @ resources=ConsumableCPUS(1)ConsumableMemory(1600 MB)
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ executable = /gpfs/m/meso/noscrub/wx20py/WPS+WRFV2/WPS_bgrid/run_met.ll
# @ queue
