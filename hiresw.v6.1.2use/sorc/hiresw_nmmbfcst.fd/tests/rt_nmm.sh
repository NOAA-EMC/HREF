#!/bin/sh
set -eu

####################################################################################################
# Make configure and run files
####################################################################################################

cat nmm_conf/nmm_${GBRG}_run.IN  | sed s:_JBNME_:${JBNME}:g                  \
                                 | sed s:_TS_:${TS}:g                        \
                                 | sed s:_wrtdopost_:${WRITE_DOPOST}:g       \
                                 | sed s:_postgrbvs_:${POST_GRIBVERSION}:g   \
                                 | sed s:_RTPWD_:${RTPWD}:g                  \
                                 | sed s:_SRCD_:${PATHTR}:g                  \
                                 | sed s:_RUND_:${RUNDIR}:g   >  nmm_run

cat nmm_conf/nmm_${GBRG}_conf.IN | sed s:_INPES_:${INPES}:g                  \
                                 | sed s:_JNPES_:${JNPES}:g                  \
                                 | sed s:_WTPG_:${WTPG}:g                    \
                                 | sed s:_wrtdopost_:${WRITE_DOPOST}:g       \
                                 | sed s:_postgrbvs_:${POST_GRIBVERSION}:g   \
                                 | sed s:_FCSTL_:${FCSTL}:g                  \
                                 | sed s:_NEMSI_:${NEMSI}:g                  \
                                 | sed s:_RSTRT_:${RSTRT}:g                  \
                                 | sed s:_gfsP_:${gfsP}:g                    \
                                 | sed s:_RADTN_:${RADTN}:g                  \
                                 | sed s:_CONVC_:${CONVC}:g                  \
                                 | sed s:_MICRO_:${MICRO}:g                  \
                                 | sed s:_SPEC_ADV_:${SPEC_ADV}:g            \
                                 | sed s:_TURBL_:${TURBL}:g                  \
                                 | sed s:_PCPFLG_:${PCPFLG}:g                \
                                 | sed s:_WPREC_:${WPREC}:g                  \
                                 | sed s:_NCHILD_:${NCHILD}:g                \
                                 | sed s:_MODE_:${MODE}:g                    \
                                 | sed s:_WGT_:${WGT}:g  >  configure_file_01

cat nems.configure.IN   | sed s:_atm_model_:${atm_model}:g                    \
                        | sed s:_atm_petlist_bounds_:"${atm_petlist_bounds}":g\
                        | sed s:_ocn_model_:${ocn_model}:g                    \
                        | sed s:_ocn_petlist_bounds_:"${ocn_petlist_bounds}":g\
                        | sed s:_ice_model_:${ice_model}:g                    \
                        | sed s:_ice_petlist_bounds_:"${ice_petlist_bounds}":g\
                        | sed s:_med_model_:${med_model}:g                    \
                        | sed s:_med_petlist_bounds_:"${med_petlist_bounds}":g\
                        | sed s:_med_atm_coupling_interval_sec_:"${med_atm_coupling_interval_sec}":g\
                        | sed s:_med_ocn_coupling_interval_sec_:"${med_ocn_coupling_interval_sec}":g\
                        >  nems.configure

if [ $SCHEDULER = 'moab' ]; then

cat nmm_conf/nmm_msub.IN         | sed s:_JBNME_:${JBNME}:g   \
                                 | sed s:_WLCLK_:${WLCLK}:g   \
                                 | sed s:_TASKS_:${TASKS}:g   \
                                 | sed s:_THRD_:${THRD}:g     >  nmm_msub

elif [ $SCHEDULER = 'pbs' ]; then

cat nmm_conf/nmm_qsub.IN         | sed s:_JBNME_:${JBNME}:g   \
                                 | sed s:_ACCNR_:${ACCNR}:g   \
                                 | sed s:_SRCD_:${PATHTR}:g   \
                                 | sed s:_WLCLK_:${WLCLK}:g   \
                                 | sed s:_TASKS_:${TASKS}:g   \
                                 | sed s:_THRD_:${THRD}:g     >  nmm_qsub

elif [ $SCHEDULER = 'lsf' ]; then

cat nmm_conf/nmm_bsub.IN         | sed s:_JBNME_:${JBNME}:g   \
                                 | sed s:_ACCNR_:${ACCNR}:g   \
                                 | sed s:_SRCD_:${PATHTR}:g   \
                                 | sed s:_WLCLK_:${WLCLK}:g   \
                                 | sed s:_TPN_:${TPN}:g       \
                                 | sed s:_TASKS_:${TASKS}:g   \
                                 | sed s:_THRD_:${THRD}:g     >  nmm_bsub

fi

if [ ${GBRG} = nests ]; then
  cat nmm_conf/nmm_nests_conf_02.IN | sed s:_RSTRT_:${RSTRT}:g                  \
                                    | sed s:_gfsP_:${gfsP}:g                    \
                                    | sed s:_RADTN_:${RADTN}:g                  \
                                    | sed s:_CONVC_:${CONVC}:g                  \
                                    | sed s:_TURBL_:${TURBL}:g > configure_file_02

  cat nmm_conf/nmm_nests_conf_03.IN | sed s:_RSTRT_:${RSTRT}:g                  \
                                    | sed s:_gfsP_:${gfsP}:g                    \
                                    | sed s:_RADTN_:${RADTN}:g                  \
                                    | sed s:_CONVC_:${CONVC}:g                  \
                                    | sed s:_TURBL_:${TURBL}:g > configure_file_03

  cat nmm_conf/nmm_nests_conf_04.IN | sed s:_RSTRT_:${RSTRT}:g                  \
                                    | sed s:_gfsP_:${gfsP}:g                    \
                                    | sed s:_RADTN_:${RADTN}:g                  \
                                    | sed s:_CONVC_:${CONVC}:g                  \
                                    | sed s:_TURBL_:${TURBL}:g > configure_file_04

fi

if [ ${GBRG} = mnests ]; then
  rm -f configure_file_02 configure_file_03 configure_file_04
  cat nmm_conf/nmm_mnests_conf_02.IN | sed s:_RSTRT_:${RSTRT}:g > configure_file_02
  cat nmm_conf/nmm_mnests_conf_03.IN | sed s:_RSTRT_:${RSTRT}:g > configure_file_03
  cat nmm_conf/nmm_mnests_conf_04.IN | sed s:_RSTRT_:${RSTRT}:g > configure_file_04
fi

if [ ${MODE} = 2-way  ]; then
  rm -f configure_file_02 configure_file_03 configure_file_04
  cat nmm_conf/nmm_mnests_2way_conf_02.IN | sed s:_WGT_:${WGT}:g                  \
                                          | sed s:_RSTRT_:${RSTRT}:g > configure_file_02
  cat nmm_conf/nmm_mnests_2way_conf_03.IN | sed s:_WGT_:${WGT}:g                  \
                                          | sed s:_RSTRT_:${RSTRT}:g > configure_file_03
  cat nmm_conf/nmm_mnests_2way_conf_04.IN | sed s:_WGT_:${WGT}:g                  \
                                          | sed s:_RSTRT_:${RSTRT}:g > configure_file_04
fi

if [ ${GBRG} = fltr ]; then
  rm -f configure_file_02 configure_file_03 configure_file_04
  cp nmm_conf/nmm_fltr_conf_02 configure_file_02
  cp nmm_conf/nmm_fltr_conf_03 configure_file_03
fi

####################################################################################################
# Submit test
####################################################################################################

sh ./nmm_run

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
  job_running=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep " dev " | wc -l`;sleep 5
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
  if [ -f ${RUNDIR}/err ] ; then FnshHrs=`tail -100 ${RUNDIR}/err | grep Finished | tail -1 | awk '{ print $10 }'` ; fi
  FnshHrs=${FnshHrs:-0}
  if   [ $status = 'Q' ];  then echo $n "min. TEST ${TEST_NR} is waiting in a queue, Status: " $status
  elif [ $status = 'H' ];  then echo $n "min. TEST ${TEST_NR} is held in a queue,    Status: " $status
  elif [ $status = 'R' ];  then echo $n "min. TEST ${TEST_NR} is running,            Status: " $status  ", Finished " $FnshHrs "hours"
  elif [ $status = 'E' ];  then echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status ; job_running=0
  elif [ $status = 'C' ];  then echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status ; job_running=0
  else                          echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status  ", Finished " $FnshHrs "hours"
  fi

elif [ $SCHEDULER = 'lsf' ]; then

  status=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep " dev " | awk '{print $3}'` ; status=${status:--}
#  if [ $status != '-' -a $status != 'PEND' ] ; then FnshHrs=`bpeek -J ${JBNME} | grep Finished | tail -1 | awk '{ print $10 }'` ; fi
  if [ -f ${RUNDIR}/err ] ; then FnshHrs=`grep Finished ${RUNDIR}/err | tail -1 | awk '{ print $10 }'` ; fi
  FnshHrs=${FnshHrs:-0}
  if   [ $status = 'PEND' ];  then echo $n "min. TEST ${TEST_NR} is waiting in a queue, Status: " $status
  elif [ $status = 'RUN'  ];  then echo $n "min. TEST ${TEST_NR} is running,            Status: " $status  ", Finished " $FnshHrs "hours"
  else                             echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status  ", Finished " $FnshHrs "hours"
  fi


fi
(( n=n+1 ))
done

####################################################################################################
# Check results
####################################################################################################

(echo;echo;echo "Checking test ${TEST_NR} results ....")>> ${REGRESSIONTEST_LOG}
 echo;echo;echo "Checking test ${TEST_NR} results ...."

#
     if [ ${CREATE_BASELINE} = false ]; then
#
# --- regression test comparison ----
#

for i in ${LIST_FILES}
do
printf %s " Comparing " $i "....." >> ${REGRESSIONTEST_LOG}
printf %s " Comparing " $i "....."

if [ -f ${RUNDIR}/$i ] ; then

  d=`cmp ${RTPWD}/${CNTL_DIR}/$i ${RUNDIR}/$i | wc -l`

  if [[ $? -ne 0 || $d -ne 0 ]] ; then
   (echo " ......NOT OK" ; echo ; echo "   $i differ!   ")>> ${REGRESSIONTEST_LOG}
    echo " ......NOT OK" ; echo ; echo "   $i differ!   " ; exit 2
  fi

  echo "....OK" >> ${REGRESSIONTEST_LOG}
  echo "....OK"

else

  echo "Missing " ${RUNDIR}/$i " output file" >> ${REGRESSIONTEST_LOG}
  echo "Missing " ${RUNDIR}/$i " output file"
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

echo " Test ${TEST_NR} passed " >> ${REGRESSIONTEST_LOG}
(echo;echo;echo)                >> ${REGRESSIONTEST_LOG}
echo " Test ${TEST_NR} passed "

sleep 4
echo;echo

####################################################################################################
# End test
####################################################################################################

exit 0
