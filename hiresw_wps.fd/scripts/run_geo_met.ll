# @ step_name = run_wps_geo
# @ output =geo.log
# @ error = geo.log
# @ notification = never
# @ wall_clock_limit = 00:14:00
# @ job_type = parallel
# @ total_tasks = 8
# @ node=1
# @ class=debug
# @ arguments = NMM
# @ group=devonprod
# @ resources=consumablecpus(1)consumablememory(900 MB)
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ executable = /meso/save/wx20py/WPS+WRFV2/WPS/run_geo.ll
# @ queue

# @ dependency = (run_wps_geo == 0) 
# @ step_name = run_wps_met
# @ output = /meso/save/wx20py/WPS+WRFV2/WPS/met.log
# @ error = /meso/save/wx20py/WPS+WRFV2/WPS/met.log
# @ notification = never
# @ wall_clock_limit = 00:29:00
# @ job_type = parallel
# @ total_tasks = 12
# @ arguments = NMM
# @ class=dev
# @ group=devonprod
# @ resources=ConsumableCPUS(1)ConsumableMemory(1200 MB)
# @ node=1
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ executable = /meso/save/wx20py/WPS+WRFV2/WPS/run_met.ll
# @ queue
