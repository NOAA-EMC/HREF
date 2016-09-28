#! /bin/ksh

export TASK=1b

# @ step_name = run_wps_nemsinterp
# @ output = nemsinterp.log
# @ error = nemsinterp.log
# @ notification = never
# @ wall_clock_limit = 00:04:29
# @ job_type = parallel
# @ total_tasks = 1
# @ node=1
# @ arguments = NMM
# @ class=dev
# @ group=devonprod
# @ resources=ConsumableCPUS(1)ConsumableMemory(5200 MB)
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ queue

cd /meso/noscrub/wx20py/NMMB_init/NPS

cd /stmp/wx20py/nmmb_init

cp /meso/noscrub/wx20py/NMMB_init/NPS/nemsinterp.exe .

rm -rf temp.*
rm -rf cored*
rm test_input_umo*
rm boco.*

./nemsinterp.exe > nemsinterp.log_1 2>&1


mv boco.000 ${TASK}_boco.000
mv test_input_umo_regional ${TASK}_test_input_umo_regional

grep CHK nemsinterp.log_1 > chk_vals_${TASK}PE.log
