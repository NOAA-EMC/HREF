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

cd /meso/save/wx20py/JIF_CODES/WPS_NMM/geogrid

ln -sf GEOGRID.TBL.${1} GEOGRID.TBL

cd ..

# cp hiresw_${dir}.namelist.wps_in namelist.wps_in

./geogrid.exe

# mv geo_nmm.d01.int hiresw_wps_static_${dir}

done
