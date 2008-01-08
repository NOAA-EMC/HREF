#! /bin/ksh

# @ step_name = compile
# @ output = /dev/null
# @ error = /dev/null
# @ notification = never
# @ wall_clock_limit = 00:39:00
# @ job_type = serial
# @ node_usage=shared
# @ class = prod
# @ queue

export envir=prod
export OBJECT_MODE=64

cd /nw${envir}/sorc/hiresw_nmm_real_fcst.fd
compile nmm_real > compile_ll.log 2>&1

mv /nw${envir}/sorc/hiresw_nmm_real_fcst.fd/main/wrf.exe hiresw_nmm_fcst
mv /nw${envir}/sorc/hiresw_nmm_real_fcst.fd/main/real.exe hiresw_nmm_real

