#!/bin/ksh
#set -eu

mkdir -p ${RUNDIR}

## specify the regression test setting

export NEMSDIR=${PATHTR}
export WORKDIR=${RUNDIR}

export REGSDIR=${RTPWD}
#export PARA_CONFIG=${REGSDIR}/data_GOCART/ngac_para_config
export PARA_CONFIG=${NEMSDIR}/tests/ngac_para_config
#export CONFIG_FILE=${NGAC_CONFIG_FILE:-${REGSDIR}/data_GOCART/ngac_config}
export CONFIG_FILE=${NGAC_CONFIG_FILE:-$PATHRT/ngac_config}

####################################################################################################
# Submit test
####################################################################################################

if [ $SCHEDULER = 'moab' ]; then

cat ngac_msub.IN    | sed s:_JBNME_:${JBNME}:g   \
                    | sed s:_WLCLK_:${WLCLK}:g   \
                    | sed s:_TPN_:${TPN}:g       \
                    | sed s:_TASKS_:${TASKS}:g   \
                    | sed s:_CONFIG_:${PARA_CONFIG}:g   \
                    | sed s:_CONFIGFILE_:${CONFIG_FILE}:g   \
                    | sed s:_THRD_:${THRD}:g     >  ngac_msub

elif [ $SCHEDULER = 'pbs' ]; then

export TPN=$((12/THRD))
cat ngac_qsub.IN    | sed s:_JBNME_:${JBNME}:g   \
                    | sed s:_NEMSDIR_:${NEMSDIR}:g   \
                    | sed s:_WORKDIR_:${WORKDIR}:g   \
                    | sed s:_REGSDIR_:${REGSDIR}:g   \
                    | sed s:_DATAICDIR_:${DATAICDIR}:g   \
                    | sed s:_CONFIG_:${PARA_CONFIG}:g   \
                    | sed s:_CONFIGFILE_:${CONFIG_FILE}:g   \
                    | sed s:_NEMSIOIN_:${NEMSIOIN}:g   \
                    | sed s:_NEMSIOOUT_:${NEMSIOOUT}:g   \
                    | sed s:_ACCNR_:${ACCNR}:g   \
                    | sed s:_QUEUE_:${QUEUE}:g   \
                    | sed s:_WLCLK_:${WLCLK}:g   \
                    | sed s:_TASKS_:${TASKS}:g   \
                    | sed s:_THRDS_:${THRD}:g    \
                    | sed s:_RUND_:${RUNDIR}:g   \
                    | sed s:_MACHINE_ID_:${MACHINE_ID}:g          \
                    | sed s:_SCHED_:${SCHEDULER}:g   > ngac_qsub

elif [ $SCHEDULER = 'lsf' ]; then

  export pex=${pex:-1}
  if [ $pex -eq 2 ] ; then
   export TPN=${TPN:-$((24/THRD))}
   export CLASS=${CLASS:-dev$pex}
  else
   export TPN=${TPN:-$((16/THRD))}
  fi
  export CLASS=${CLASS:-dev}
cat ngac_bsub.IN    | sed s:_JBNME_:${JBNME}:g   \
                    | sed s:_NEMSDIR_:${NEMSDIR}:g   \
                    | sed s:_WORKDIR_:${WORKDIR}:g   \
                    | sed s:_REGSDIR_:${REGSDIR}:g   \
                    | sed s:_DATAICDIR_:${DATAICDIR}:g   \
                    | sed s:_CONFIG_:${PARA_CONFIG}:g   \
                    | sed s:_CONFIGFILE_:${CONFIG_FILE}:g   \
                    | sed s:_NEMSIOIN_:${NEMSIOIN}:g   \
                    | sed s:_NEMSIOOUT_:${NEMSIOOUT}:g   \
                    | sed s:_CLASS_:${CLASS}:g   \
                    | sed s:_WLCLK_:${WLCLK}:g   \
                    | sed s:_TASKS_:${TASKS}:g   \
                    | sed s:_THRDS_:${THRD}:g    \
                    | sed s:_RUND_:${RUNDIR}:g   \
                    | sed s:_TPN_:${TPN}:g       \
                    | sed s:_MACHINE_ID_:${MACHINE_ID}:g          \
                    | sed s:_SCHED_:${SCHEDULER}:g   > ngac_bsub
fi

## copy J-job script
cp ${NEMSDIR}/tests/JNGAC_FORECAST.sms.para ${RUNDIR}

## submit the job
if [ $SCHEDULER = 'moab' ]; then
  msub ngac_msub > /dev/null
elif [ $SCHEDULER = 'pbs' ]; then
  qsub ngac_qsub > /dev/null
elif [ $SCHEDULER = 'lsf' ]; then
  bsub < ngac_bsub > /dev/null 2>&1
fi


echo "Test ${TEST_NR}" >> ${REGRESSIONTEST_LOG}
echo "Test ${TEST_NR}"
echo ${TEST_DESCR} >> ${REGRESSIONTEST_LOG}
echo ${TEST_DESCR}


(echo "GFS, ${TASKS} proc, ${THRD} thread";echo;echo)>> ${REGRESSIONTEST_LOG}
 echo "GFS, ${TASKS} proc, ${THRD} thread";echo;echo

# wait for the job to enter the queue
job_running=0
until [ $job_running -eq 1 ]
do
echo "TEST is waiting to enter the queue"
if [ $SCHEDULER = 'moab' ]; then
  job_running=`showq -u ${USER} -n | grep ${JBNME} | wc -l`;sleep 5
elif [ $SCHEDULER = 'pbs' ]; then
  job_running=`qstat -u ${USER} -n | grep ${JBNME} | wc -l`;sleep 5
elif [ $SCHEDULER = 'lsf' ]; then
  job_running=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep $CLASS | wc -l`;sleep 5
fi
done

# wait for the job to finish and compare results
job_running=1
n=1
until [ $job_running -eq 0 ]
do

sleep 60
if [ $SCHEDULER = 'moab' ]; then
  job_running=`showq -u ${USER} -n | grep ${JBNME} | wc -l`
elif [ $SCHEDULER = 'pbs' ]; then
  job_running=`qstat -u ${USER} -n | grep ${JBNME} | wc -l`
elif [ $SCHEDULER = 'lsf' ]; then
  job_running=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | wc -l`
fi

if [ $SCHEDULER = 'moab' ]; then

  status=`showq -u ${USER} -n | grep ${JBNME} | awk '{print $3}'` ; status=${status:--}
  if [ -f ${RUNDIR}/err ] ; then FnshHrs=`grep Finished ${RUNDIR}/err | tail -1 | awk '{ print $10 }'` ; fi
  FnshHrs=${FnshHrs:-0}
  if   [ $status = 'Idle' ];       then echo $n "min. TEST ${TEST_NR} is waiting in a queue, Status: " $status
  elif [ $status = 'Running' ];    then echo $n "min. TEST ${TEST_NR} is running,            Status: " $status  ", Finished " $FnshHrs "hours"
  elif [ $status = 'Starting' ];   then echo $n "min. TEST ${TEST_NR} is ready to run,       Status: " $status  ", Finished " $FnshHrs "hours"
  elif [ $status = 'Completed' ];  then echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status ; job_running=0
  else                                  echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status  ", Finished " $FnshHrs "hours"
  fi

elif [ $SCHEDULER = 'pbs' ]; then

  status=`qstat -u ${USER} -n | grep ${JBNME} | awk '{print $"10"}'` ; status=${status:--}
  if [ -f ${RUNDIR}/err ] ; then FnshHrs=`grep Finished ${RUNDIR}/err | tail -1 | awk '{ print $10 }'` ; fi
  FnshHrs=${FnshHrs:-0}
  if   [ $status = 'Q' ];  then echo $n "min. TEST ${TEST_NR} is waiting in a queue, Status: " $status
  elif [ $status = 'H' ];  then echo $n "min. TEST ${TEST_NR} is held in a queue,    Status: " $status
  elif [ $status = 'R' ];  then echo $n "min. TEST ${TEST_NR} is running,            Status: " $status  ", Finished " $FnshHrs "hours"
  elif [ $status = 'E' ];  then echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status ; job_running=0
  elif [ $status = 'C' ];  then echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status ; job_running=0
  else                          echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status  ", Finished " $FnshHrs "hours"
  fi

elif [ $SCHEDULER = 'lsf' ]; then

  status=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep $CLASS | awk '{print $3}'` ; status=${status:--}
#  if [ $status != '-' ] ; then FnshHrs=`bpeek -J ${JBNME} | grep Finished | tail -1 | awk '{ print $9 }'` ; fi
  if [ -f ${RUNDIR}/err ] ; then FnshHrs=`grep Finished ${RUNDIR}/err | tail -1 | awk '{ print $9 }'` ; fi
  FnshHrs=${FnshHrs:-0}
  if   [ $status = 'PEND' ];  then echo $n "min. TEST ${TEST_NR} is waiting in a queue, Status: " $status
  elif [ $status = 'RUN'  ];  then echo $n "min. TEST ${TEST_NR} is running,            Status: " $status  ", Finished " $FnshHrs "hours"
  else                             echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status  ", Finished " $FnshHrs "hours"
  fi


fi
(( n=n+1 ))
done

####################################################################################################

####################################################################################################
# Check results
####################################################################################################

test_status='PASS'

(echo;echo;echo "Checking test ${TEST_NR} results ....")>> ${REGRESSIONTEST_LOG}
 echo;echo;echo "Checking test ${TEST_NR} results ...."

#
     if [ ${CREATE_BASELINE} = false ]; then
#
# --- regression test comparison ----
#

  for i in ${LIST_FILES} ; do
    printf %s " Comparing " $i "....." >> ${REGRESSIONTEST_LOG}
    printf %s " Comparing " $i "....."

    if [ ! -f ${RUNDIR}/$i ] ; then

     echo ".......MISSING file" >> ${REGRESSIONTEST_LOG}
     echo ".......MISSING file"
     test_status='FAIL'

    elif [ ! -f ${RTPWD}/${CNTL_DIR}/$i ] ; then

     echo ".......MISSING baseline" >> ${REGRESSIONTEST_LOG}
     echo ".......MISSING baseline"
     test_status='FAIL'

    else

     d=`cmp ${RTPWD}/${CNTL_DIR}/$i ${RUNDIR}/$i | wc -l`

     if [[ $d -ne 0 ]] ; then
       echo ".......NOT OK" >> ${REGRESSIONTEST_LOG}
       echo ".......NOT OK"
       test_status='FAIL'

     else

       echo "....OK" >> ${REGRESSIONTEST_LOG}
       echo "....OK"
     fi

    fi

  done

if [ $test_status = 'FAIL' ]; then echo $TEST_NAME >> fail_test ; fi

     else
#
# --- create baselines
#

 sleep 60
 echo;echo;echo "Moving set ${TEST_NR} files ...."

for i in ${LIST_FILES}
do
  printf %s " Moving " $i "....."
  if [ -f ${RUNDIR}/$i ] ; then
    cp ${RUNDIR}/${i} ${RTPWD_U}/${CNTL_DIR}/${i}
  else
    echo "Missing " ${RUNDIR}/$i " output file"
    echo;echo " Set ${TEST_NR} failed "
    exit 2
  fi
done

# ---
     fi
# ---

echo "Test ${TEST_NR} ${test_status} " >> ${REGRESSIONTEST_LOG}
(echo;echo;echo)                       >> ${REGRESSIONTEST_LOG}
echo "Test ${TEST_NR} ${test_status} "
(echo;echo;echo)

sleep 4
echo;echo

####################################################################################################
# End test
####################################################################################################

exit 0
