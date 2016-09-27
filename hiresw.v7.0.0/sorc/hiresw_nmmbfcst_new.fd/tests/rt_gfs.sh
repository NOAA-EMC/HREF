#!/bin/ksh
#set -eu
set -x

export GEFS_ENSEMBLE=${GEFS_ENSEMBLE:-0}
echo "GEFS_ENSEMBLE=" $GEFS_ENSEMBLE

mkdir -p ${RUNDIR}

export CDATE=${CDATE:-2012010100}
export NEMSIOIN=${NEMSIOIN:-.false.}
export SIGIOIN=${SIGIOIN:-.true.}
export SFCIOOUT=${SFCIOOUT:-.true.}
export NEMSIOOUT=${NEMSIOOUT:-.false.}
export SIGIOOUT=${SIGIOOUT:-.true.}
export MACHINE_ID=${MACHINE_ID:-wcoss}
if [ "$NEMSIOIN" = ".true." ]; then
  if [ $MACHINE_ID = wcoss ]; then
    export SIGHDR=/nwprod/exec/global_sighdr
  elif [ $MACHINE_ID = theia ]; then
    export SIGHDR=/scratch4/NCEPDEV/nems/save/Jun.Wang/nems/util/nemsio_get
  fi
else
  export SIGHDR=${SIGHDR:-/nwprod/exec/global_sighdr}
fi
export fcst_begin=${fcst_begin:-YES}

export SCHEDULER=${SCHEDULER:-lsf}
export SHOWQ=${SHOWQ:-/opt/moab/default/bin/showq}
export MSUB=${MSUB:-/opt/moab/default/bin/msub}

export IALB=0
export IEMS=0
export ISOL=1
export ICO2=2
export IAER=111
export fcyc=0
export FHOUT_HF=${FHOUT_HF:-1}
export FHMAX_HF=${FHMAX_HF:-0}

if [ $GEFS_ENSEMBLE = 0 ] ; then

################################################################################
# For the stand alone GFS regression tests.
################################################################################

################################################################################
# Make configure and run files
################################################################################

## determine GOCART and TRACER from gocart_aerosol and passive_tracer 
 export gocart_aerosol=${gocart_aerosol:-NO}
 export passive_tracer=${passive_tracer:-YES}
 if [ $gocart_aerosol = 'YES' ] ; then
  export GOCART=1 
 else
  export GOCART=0 
 fi
 if  [ $passive_tracer = 'YES' ] ; then
  export TRACER=.true.
 else
  export TRACER=.false.
 fi
##

 export WAM_IPE_COUPLING=${WAM_IPE_COUPLING:-.false.}
 export HEIGHT_DEPENDENT_G=${HEIGHT_DEPENDENT_G:-.false.}
 export F107_KP_SKIP_SIZE=${F107_KP_SKIP_SIZE:-0}

 cd $PATHRT

 cat gfs_fcst_run.IN | sed s:_TASKS_:${TASKS}:g                   \
                     | sed s:_PE1_:${PE1}:g                       \
                     | sed s:_NEMSIOIN_:${NEMSIOIN}:g             \
                     | sed s:_NEMSIOOUT_:${NEMSIOOUT}:g           \
                     | sed s:_SIGIOIN_:${SIGIOIN}:g               \
                     | sed s:_SIGIOOUT_:${SIGIOOUT}:g             \
                     | sed s:_SFCIOOUT_:${SFCIOOUT}:g             \
                     | sed s:_WTPG_:${WTPG}:g                     \
                     | sed s:_WRTGP_:${WRTGP}:g                   \
                     | sed s:_wrtdopost_:${WRITE_DOPOST}:g        \
                     | sed s:_postgrbvs_:${POST_GRIBVERSION}:g    \
                     | sed s:_aer2post_:${GOCART_AER2POST}:g      \
                     | sed s:_THRDS_:${THRD}:g                    \
                     | sed s:_NSOUT_:${NSOUT}:g                   \
                     | sed s:_QUILT_:${QUILT}:g                   \
                     | sed s:_IAER_:${IAER}:g                     \
                     | sed s:_IALB_:${IALB}:g                     \
                     | sed s:_wave_:${wave}:g                     \
                     | sed s:_lm_:${lm}:g                         \
                     | sed s:_lsoil_:${lsoil}:g                   \
                     | sed s:_MEMBER_NAMES_:${MEMBER_NAMES}:g     \
                     | sed s:_CP2_:${CP2}:g                       \
                     | sed s:_RUNDIR_:${RUNDIR}:g                 \
                     | sed s:_RESTART_:${RESTART}:g               \
                     | sed s:_FCST_BEGIN_:${fcst_begin}:g               \
                     | sed s:_PATHTR_:${PATHTR}:g                 \
                     | sed s:_FDFI_:${FDFI}:g                     \
                     | sed s:_FHOUT_:${FHOUT}:g                   \
                     | sed s:_FHZER_:${FHZER}:g                   \
                     | sed s:_FHRES_:${FHRES}:g                   \
                     | sed s:_FHROT_:${FHROT}:g                   \
                     | sed s:_FHOUTHF_:${FHOUT_HF}:g              \
                     | sed s:_FHMAXHF_:${FHMAX_HF}:g              \
                     | sed s:_fcyc_:${fcyc}:g                     \
                     | sed s:_REDUCEDGRID_:${REDUCEDGRID}:g       \
                     | sed s:_ADIABATIC_:${ADIABATIC}:g                   \
                     | sed s:_NSTFCST_:${NST_FCST}:g              \
                     | sed s:_NSTSPINUP_:${NST_SPINUP}:g          \
                     | sed s:_NSTREV_:${NST_RESERVED}:g           \
                     | sed s:_ZSEA1_:${ZSEA1}:g                   \
                     | sed s:_ZSEA2_:${ZSEA2}:g                   \
                     | sed s:_GOCART_:${GOCART}:g                 \
                     | sed s:_TRACER_:${TRACER}:g                 \
                     | sed s:_SFCPRESSID_:${SFCPRESS_ID}:g        \
                     | sed s:_THERMODYNID_:${THERMODYN_ID}:g      \
                     | sed s:_IDVC_:${IDVC}:g                     \
                     | sed s:_NDSLFV_:${NDSLFV}:g                 \
                     | sed s:_SPECTRALLOOP_:${SPECTRALLOOP}:g     \
                     | sed s:_IDEA_:${IDEA}:g                     \
                     | sed s:_WAM_IPE_COUPLING_:${WAM_IPE_COUPLING}:g       \
                     | sed s:_HEIGHT_DEPENDENT_G_:${HEIGHT_DEPENDENT_G}:g   \
                     | sed s:_F107_KP_SKIP_SIZE_:${F107_KP_SKIP_SIZE}:g   \
                     | sed s:_CDATE_:${CDATE}:g                   \
                     | sed s:_IEMS_:${IEMS}:g                     \
                     | sed s:_ISOL_:${ISOL}:g                     \
                     | sed s:_ICO2_:${ICO2}:g                     \
                     | sed s:_IAER_:${IAER}:g                     \
                     | sed s:_NGRID_A2OI_:${NGRID_A2OI}:g         \
                     | sed s:_A2OI_OUT_:${A2OI_OUT}:g             \
                     | sed s:_SIGHDR_:${SIGHDR}:g                 \
                     | sed s:_MACHINE_ID_:${MACHINE_ID}:g         \
                     | sed s:_RTPWD_:${RTPWD}:g                   \
                     | sed s:_DATAICDIR_:${DATAICDIR}:g           \
                     | sed s:_SCHEDULER_:${SCHEDULER}:g           \
                     | sed s:_SLG_:${SLG}:g                       \
                     | sed s:_NGRID_A2OI_:${NGRID_A2OI}:g         \
                     | sed s:_A2OI_OUT_:${A2OI_OUT}:g             \
                     | sed s:_F107_KP_SIZE_:${F107_KP_SIZE}:g         \
                     | sed s:_F107_KP_INTERVAL_:${F107_KP_INTERVAL}:g \
                     | sed s:_NDAYS_:${NDAYS}:g   >  gfs_fcst_run


 chmod 755 gfs_fcst_run

 cp gfs_fcst_run ${RUNDIR}

if [ ${nems_configure}"x" != "x" ]; then
 cat nems.configure.${nems_configure}.IN   \
                         | sed s:_atm_model_:${atm_model}:g                    \
                         | sed s:_atm_petlist_bounds_:"${atm_petlist_bounds}":g\
                         | sed s:_lnd_model_:${lnd_model}:g                    \
                         | sed s:_lnd_petlist_bounds_:"${lnd_petlist_bounds}":g\
                         | sed s:_ice_model_:${ice_model}:g                    \
                         | sed s:_ice_petlist_bounds_:"${ice_petlist_bounds}":g\
                         | sed s:_ocn_model_:${ocn_model}:g                    \
                         | sed s:_ocn_petlist_bounds_:"${ocn_petlist_bounds}":g\
                         | sed s:_wav_model_:${wav_model}:g                    \
                         | sed s:_wav_petlist_bounds_:"${wav_petlist_bounds}":g\
                         | sed s:_ipm_model_:${ipm_model}:g                    \
                         | sed s:_ipm_petlist_bounds_:"${ipm_petlist_bounds}":g\
                         | sed s:_hyd_model_:${hyd_model}:g                    \
                         | sed s:_hyd_petlist_bounds_:"${hyd_petlist_bounds}":g\
                         | sed s:_med_model_:${med_model}:g                    \
                         | sed s:_med_petlist_bounds_:"${med_petlist_bounds}":g\
                         | sed s:_atm_coupling_interval_sec_:"${atm_coupling_interval_sec}":g\
                         | sed s:_ocn_coupling_interval_sec_:"${ocn_coupling_interval_sec}":g\
                         | sed s:_coupling_interval_sec_:"${coupling_interval_sec}":g\
                         | sed s:_coupling_interval_slow_sec_:"${coupling_interval_slow_sec}":g\
                         | sed s:_coupling_interval_fast_sec_:"${coupling_interval_fast_sec}":g\
                         >  nems.configure
                         
 cp nems.configure ${RUNDIR}
fi

################################################################################
# Copy init files
################################################################################

 cat atmos.configure_gfs | sed s:_atm_model_:${atm_model}:g  \
                         | sed s:_coupling_interval_fast_sec_:"${coupling_interval_fast_sec}":g\
                         >  atmos.configure
 cp atmos.configure ${RUNDIR}/atmos.configure
 cp MAPL.rc ${RUNDIR}/MAPL.rc
 cp Chem_Registry.rc ${RUNDIR}/Chem_Registry.rc

 if [ $GOCART = 1 ] ; then
  if [ $SCHEDULER = 'loadleveler' ]; then
    export EXTDIR=/global/save/wx23lu/NEMS/fix
    export RCSDIR=/global/save/wx23lu/NEMS/Chem_Registry
    cp -r ${EXTDIR}/ExtData ${RUNDIR}/.
  elif [ $SCHEDULER = 'pbs' ]; then
    export EXTDIR=_RTPWD_/data_GOCART
    export RCSDIR=_RTPWD_/data_GOCART
    cp -r ${EXTDIR}/ngac_fix ${RUNDIR}/.
  elif [ $SCHEDULER = 'lsf' ]; then
    export EXTDIR=/nwprod/ngac.v1.0.0/fix
    export RCSDIR=_RTPWD_/data_GOCART
    cp -r ${EXTDIR}/ngac_fix ${RUNDIR}/.
  fi
 fi

 if [ "$NEMSIOIN" = ".true." ]; then
  export IC_DIR=${IC_DIR:-${RTPWD}/$DATAICDIR}
  if [ $MACHINE_ID = wcoss ] ; then
     export nemsioget=${nemsioget:-/nwprod/ngac.v1.0.0/exec/nemsio_get}
  elif [ $MACHINE_ID = theia ] ; then
     export nemsioget=${nemsioget:-/scratch3/NCEPDEV/nems/save/Jun.Wang/nems/util/nemsio_get}
  fi
  export SIGHDR=$nemsioget
  if [ $fcst_begin = YES ]; then
    cp $IC_DIR/gfsanl.$CDATE $RUNDIR
    cp $IC_DIR/sfnanl.$CDATE $RUNDIR
    cp $IC_DIR/nsnanl.$CDATE $RUNDIR
  else
    cp $IC_DIR/sigf${nhourb} $RUNDIR
    cp $IC_DIR/sfcf${nhourb} $RUNDIR
    cp $IC_DIR/nstf${nhourb} $RUNDIR
  fi

# These gfsanl and sfnanl data were copy from Moorthi's directory at
# /global/noscrub/Shrinivas.Moorthi/data on Surge machine. Weiyu.
#-------------------------------------------------------------------
# cp ${RTPWD}/GFS_SLG_NEMSIO_READ/gfsanl.$CDATE $RUNDIR
# cp ${RTPWD}/GFS_SLG_NEMSIO_READ/sfnanl.$CDATE $RUNDIR
#  cp $IC_DIR/gfsanl.$CDATE $RUNDIR
#  cp $IC_DIR/sfnanl.$CDATE $RUNDIR

#                     NO NEMSIO INPUT
#                     ---------------
 else 
   if [ "$IDEA" = ".true." ]; then
     cp ${RTPWD}/WAM_gh_l150/*anl*${CDATE} ${RUNDIR}/.
   else
     export dprefix=${dprefix:-""}
     if [ "$rungfstest" = ".true." ] ; then
       if [ $MACHINE_ID = wcoss ] ; then
         IC_DIR=${IC_DIR:-$dprefix/global/noscrub/Shrinivas.Moorthi/data}
       elif [ $MACHINE_ID = theia ] ; then
         IC_DIR=${IC_DIR:-$dprefix/global/noscrub/Shrinivas.Moorthi/data}
       fi
       cp $IC_DIR/siganl.$CDATE ${RUNDIR}/.
#??    cp $IC_DIR/siganl.$CDATE ${RUNDIR}/sig_ini2
       cp $IC_DIR/sfcanl.$CDATE ${RUNDIR}/.
       cp $IC_DIR/nstanl.$CDATE ${RUNDIR}/.
     fi
   fi
 fi

else

################################################################################
# For the concurrency ensemble GEFS regression test.
################################################################################

 cd $PATHRT

 cp ${RTPWD}/GEFS_data_2008082500/* $RUNDIR
# cp /climate/noscrub/wx20wa/esmf/nems/IC/nemsio_new/GEFS_data_2008082500/gfsanl* $RUNDIR
# cp /climate/noscrub/wx20wa/esmf/nems/IC/nemsio_new/GEFS_data_2008082500/sfcanl* $RUNDIR

 cat gfs_fcst_run_GEFS.IN \
                     | sed s:_SRCDIR_:${PATHTR}:g \
                     | sed s:_NDSLFV_:${NDSLFV}:g \
                     | sed s:_NEMSIOIN_:${NEMSIOIN}:g \
                     | sed s:_IDEA_:${IDEA}:g \
                     | sed s:_RUNDIR_:${RUNDIR}:g > gfs_fcst_run
 
 
 cp gfs_fcst_run ${RUNDIR}
 chmod +x ${RUNDIR}/gfs_fcst_run
 cp Chem_Registry.rc ${RUNDIR}/Chem_Registry.rc
 cp atmos.configure_gfs ${RUNDIR}/atmos.configure

fi

################################################################################
# Submit test
################################################################################

if [ $SCHEDULER = 'moab' ]; then

 export TPN=$((32/THRD))
 cat gfs_msub.IN     | sed s:_JBNME_:${JBNME}:g   \
                     | sed s:_ACCNR_:${ACCNR}:g   \
                     | sed s:_CLASS_:${CLASS}:g   \
                     | sed s:_WLCLK_:${WLCLK}:g   \
                     | sed s:_TPN_:${TPN}:g       \
                     | sed s:_TASKS_:${TASKS}:g   \
                     | sed s:_RUND_:${RUNDIR}:g   \
                     | sed s:_FIXGLOBAL_:${FIXGLOBAL}:g   \
                     | sed s:_THRD_:${THRD}:g     >  gfs_msub


elif [ $SCHEDULER = 'pbs' ]; then

 export TPN=$((24/THRD))
 cat gfs_qsub.IN     | sed s:_JBNME_:${JBNME}:g   \
                     | sed s:_ACCNR_:${ACCNR}:g   \
                     | sed s:_QUEUE_:${QUEUE}:g   \
                     | sed s:_WLCLK_:${WLCLK}:g   \
                     | sed s:_TASKS_:${TASKS}:g   \
                     | sed s:_THRD_:${THRD}:g     \
                     | sed s:_RUND_:${RUNDIR}:g   \
                     | sed s:_SCHED_:${SCHEDULER}:g   >  gfs_qsub

elif [ $SCHEDULER = 'lsf' ]; then

 export pex=${pex:-1}
 if [ $pex -eq 2 ] ; then
   export TPN=${TPN:-$((24/THRD))}
   export CLASS=${CLASS:-dev$pex}
 else
   export TPN=${TPN:-$((16/THRD))}
 fi
 export CLASS=${CLASS:-dev}
 cat gfs_bsub.IN     | sed s:_JBNME_:${JBNME}:g   \
                     | sed s:_CLASS_:${CLASS}:g   \
                     | sed s:_WLCLK_:${WLCLK}:g   \
                     | sed s:_TPN_:${TPN}:g       \
                     | sed s:_TASKS_:${TASKS}:g   \
                     | sed s:_RUND_:${RUNDIR}:g   \
                     | sed s:_THRDS_:${THRD}:g    \
                     | sed s:_CDATE_:${CDATE}:g   \
                     | sed s:_SCHED_:${SCHEDULER}:g   >  gfs_bsub
fi

cp exglobal_fcst_nems.sh $RUNDIR

export RUNDIR=$RUNDIR

cd $PATHRT

if [ $SCHEDULER = 'moab' ]; then
  $MSUB gfs_msub > /dev/null
elif [ $SCHEDULER = 'pbs' ]; then
  rm -f $PATHRT/err $PATHRT/out
  qsub $PATHRT/gfs_qsub > /dev/null
elif [ $SCHEDULER = 'lsf' ]; then
  bsub <$PATHRT/gfs_bsub > /dev/null 2>&1
fi

echo "Test ${TEST_NR}" >> ${REGRESSIONTEST_LOG}
echo "Test ${TEST_NR}"
echo ${TEST_DESCR} >> ${REGRESSIONTEST_LOG}
echo ${TEST_DESCR}
(echo "GFS, ${TASKS} proc, ${THRD} thread")>> ${REGRESSIONTEST_LOG}
 echo "GFS, ${TASKS} proc, ${THRD} thread"

# wait for the job to enter the queue
job_running=0
echo ' CLASS= ' $CLASS
until [ $job_running -eq 1 ] ; do
 echo "TEST is waiting to enter the queue"
 if [ $SCHEDULER = 'moab' ]; then
  job_running=`$SHOWQ -u ${USER} -n | grep ${JBNME} | wc -l`;sleep 5
 elif [ $SCHEDULER = 'pbs' ]; then
  job_running=`qstat -u ${USER} -n | grep ${JBNME} | wc -l`;sleep 5
 elif [ $SCHEDULER = 'lsf' ]; then
  job_running=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep ${CLASS} | wc -l`;sleep 5
 fi
done

job_running=1

# wait for the job to finish and compare results
n=1
until [ $job_running -eq 0 ] ; do

 sleep 60

 if [ $SCHEDULER = 'moab' ]; then

  status=`$SHOWQ -u ${USER} -n | grep ${JBNME} | awk '{print $3}'` ; status=${status:--}
  if [ -f ${RUNDIR}/err ] ; then FnshHrs=`grep Finished ${RUNDIR}/err | tail -1 | awk '{ print $6 }'` ; fi
  FnshHrs=${FnshHrs:-0}
  if   [ $status = 'Idle' ];       then echo $n "min. TEST ${TEST_NR} is waiting in a queue, Status: " $status
  elif [ $status = 'Running' ];    then echo $n "min. TEST ${TEST_NR} is running,            Status: " $status  ", Finished " $FnshHrs "hours"
  elif [ $status = 'Starting' ];   then echo $n "min. TEST ${TEST_NR} is ready to run,       Status: " $status  ", Finished " $FnshHrs "hours"
  elif [ $status = 'Completed' ];  then echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status
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

 elif [ $SCHEDULER = 'lsf' ] ; then

  status=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep ${CLASS} | awk '{print $3}'` ; status=${status:--}
#  if [ $status != '-' ] ; then FnshHrs=`bpeek -J ${JBNME} | grep Finished | tail -1 | awk '{ print $9 }'` ; fi
  if [ -f ${RUNDIR}/err ] ; then FnshHrs=`grep Finished ${RUNDIR}/err | tail -1 | awk '{ print $9 }'` ; fi
  FnshHrs=${FnshHrs:-0}
  if   [ $status = 'PEND' ];  then echo $n "min. TEST ${TEST_NR} is waiting in a queue, Status: " $status
  elif [ $status = 'RUN'  ];  then echo $n "min. TEST ${TEST_NR} is running,            Status: " $status  ", Finished " $FnshHrs "hours"
  else                             echo $n "min. TEST ${TEST_NR} is finished,           Status: " $status  ", Finished " $FnshHrs "hours"
  fi

 fi

 if [ $SCHEDULER = 'moab' ]; then
  job_running=`$SHOWQ -u ${USER} -n | grep ${JBNME} | wc -l`
 elif [ $SCHEDULER = 'lsf' ] ; then
  job_running=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep ${CLASS} | wc -l`
 fi
  (( n=n+1 ))
done

################################################################################
# Check results
################################################################################

test_status='PASS'

# Give one minute for data to show up on file system
sleep 60

(echo;echo;echo "baseline dir = ${RTPWD}/${CNTL_DIR}")  >> ${REGRESSIONTEST_LOG}
           echo "working dir  = ${RUNDIR}"              >> ${REGRESSIONTEST_LOG}
           echo "Checking test ${TEST_NR} results ...." >> ${REGRESSIONTEST_LOG}
(echo;echo;echo "baseline dir = ${RTPWD}/${CNTL_DIR}")
           echo "working dir  = ${RUNDIR}"
           echo "Checking test ${TEST_NR} results ...."

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

#
else
#
# --- create baselines
#

 sleep 60
 echo;echo;echo "Moving set ${TEST_NR} files ...."

 for i in ${LIST_FILES} ; do
  printf %s " Moving " $i "....."
  ls -ltr ${RUNDIR}/${i}
  if [ -f ${RUNDIR}/$i ] ; then
    cp ${RUNDIR}/${i} /${STMP}/${USER}/REGRESSION_TEST/${CNTL_DIR}/${i}
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
