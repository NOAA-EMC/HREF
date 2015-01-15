#!/bin/ksh
set -aeu

mkdir -p ${RUNDIR}

####################################################################################################
# For the stand alone FIM regression tests.
####################################################################################################

####################################################################################################
# Make configure and run files
####################################################################################################


echo 'RUNDIR=' $RUNDIR

cat fim_fcst_run_G${GLVL}L38_24hr.IN \
                    | sed s:_RTPWD_:${RTPWD}:g \
                    | sed s:_SRCDIR_:${PATHTR}:g \
                    | sed s:_RUNDIR_:${RUNDIR}:g \
                    | sed s:_FIM_USE_NEMS_:${FIM_USE_NEMS}:g > fim_fcst_run

cp atmos.configure_fim ${RUNDIR}/atmos.configure

####################################################################################################
# Submit test
####################################################################################################

cat fim_ll.IN       | sed s:_JBNME_:${JBNME}:g   \
                    | sed s:_CLASS_:${CLASS}:g   \
                    | sed s:_GROUP_:${GROUP}:g   \
                    | sed s:_ACCNR_:${ACCNR}:g   \
                    | sed s:_WLCLK_:${WLCLK}:g   \
                    | sed s:_TASKS_:${TASKS}:g   \
                    | sed s:_THRDS_:${THRD}:g    >  fim_ll

cat fim_fcst_run >> fim_ll

llsubmit fim_ll 2>&1 | grep submitted > /dev/null

echo "Test ${TEST_NR}" >> ${REGRESSIONTEST_LOG}
echo "Test ${TEST_NR}"
echo ${TEST_DESCR} >> ${REGRESSIONTEST_LOG}
echo ${TEST_DESCR}
(echo "FIM, ${TASKS} proc, ${THRD} thread";echo;echo)>> ${REGRESSIONTEST_LOG}
 echo "FIM, ${TASKS} proc, ${THRD} thread";echo;echo

# wait for the job to enter the queue
job_running=0
until [ $job_running -eq 1 ]
do
echo "TEST is waiting to enter the queue"
if [ $SCHEDULER = 'loadleveler' ]; then
  job_running=`llq -u ${USER} -f %st %jn | grep ${JBNME} | wc -l`;sleep 5
elif [ $SCHEDULER = 'moab' ]; then
  job_running=`showq -u ${USER} -n | grep ${JBNME} | wc -l`;sleep 5
elif [ $SCHEDULER = 'pbs' ]; then
  job_running=`qstat -u ${USER} -n | grep ${JBNME} | wc -l`;sleep 5
elif [ $SCHEDULER = 'lsf' ]; then
  job_running=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep " dev " | wc -l`;sleep 5
fi
done

job_running=1

# wait for the job to finish and compare results
n=1
until [ $job_running -eq 0 ]
do

export status=`llq -u ${USER} -f %st %jn | grep ${JBNME} | awk '{ print $1}'` ; export status=${status:--}

if   [ $status = 'I' ];  then echo $n "min. TEST ${TEST_NR} is waiting in a queue, Status: " $status
elif [ $status = 'R' ];  then echo $n "min. TEST ${TEST_NR} is running,            Status: " $status
elif [ $status = 'ST' ]; then echo $n "min. TEST ${TEST_NR} is ready to run,       Status: " $status
elif [ $status = 'C' ];  then echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status
else                          echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status
fi

sleep 60

job_running=`llq -u ${USER} -f %st %jn | grep ${JBNME} | wc -l`
  (( n=n+1 ))
done

####################################################################################################
# Check results
####################################################################################################

(echo;echo;echo "Checking test ${TEST_NR} results ....")>> ${REGRESSIONTEST_LOG}
 echo;echo;echo "Checking test ${TEST_NR} results ...."

# TODO:  generalize this by adding ${NVL}
outdir="${RUNDIR}/fim${GLVL}_38_${TASKS}/fim"

#
     if [ ${CREATE_BASELINE} = false ]; then
#
# --- regression test comparison ----
#

for i in ${LIST_FILES}
do

outfile="${outdir}/${i}"

printf %s " Comparing " $i "....." >> ${REGRESSIONTEST_LOG}
printf %s " Comparing " $i "....."

if [ -f ${outfile} ] ; then

  d=`cmp ${RTPWD}/${CNTL_DIR}/$i ${outfile} | wc -l`

  if [[ $? -ne 0 || $d -ne 0 ]] ; then
   (echo " ......NOT OK" ; echo ; echo "   $i differ!   ")>> ${REGRESSIONTEST_LOG}
    echo " ......NOT OK" ; echo ; echo "   $i differ!   " ; exit 2
  fi

  echo "....OK" >> ${REGRESSIONTEST_LOG}
  echo "....OK"

else

  echo "Missing " ${outfile} " output file" >> ${REGRESSIONTEST_LOG}
  echo "Missing " ${outfile} " output file"
 (echo;echo " Test ${TEST_NR} failed ")>> ${REGRESSIONTEST_LOG}
  echo;echo " Test ${TEST_NR} failed "
  exit 2

fi

done

#
     else
#
# --- create baselines
#

 echo;echo;echo "Moving set ${TEST_NR} files ...."

for i in ${LIST_FILES}
do
  outfile="${outdir}/${i}"
  printf %s " Moving " $i "....."
  if [ -f ${outfile} ] ; then
    cp ${outfile} ${RTPWD_U}/${CNTL_DIR}/${i}
  else
    echo "Missing " ${outfile} " output file"
    echo;echo " Set ${TEST_NR} failed "
    exit 2
  fi
done

# ---
     fi
# ---

echo " Test ${TEST_NR} passed " >> ${REGRESSIONTEST_LOG}
echo " Test ${TEST_NR} passed "

sleep 4
echo;echo

####################################################################################################
# End test
####################################################################################################

exit 0
