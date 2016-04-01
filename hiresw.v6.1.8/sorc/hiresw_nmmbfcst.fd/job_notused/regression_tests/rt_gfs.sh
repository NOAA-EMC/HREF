#!/bin/ksh
#set -xua

export GEFS_ENSEMBLE=${GEFS_ENSEMBLE:-0}
echo "GEFS_ENSEMBLE=" $GEFS_ENSEMBLE

mkdir -p ${RUNDIR}

export CDATE=${CDATE:-2012010100}
export NEMSIOIN=${NEMSIOIN:-.false.}
export SIGIOIN=${SIGIOIN:-.true.}
export SFCIOOUT=${SFCIOOUT:-.true.}
export NEMSIOOUT=${NEMSIOOUT:-.false.}
export SIGIOOUT=${SIGIOOUT:-.true.}
export SIGHDR=${SIGHDR:-/nwprod/exec/global_sighdr}
export MACHINE_ID=${MACHINE_ID:-WCOSS}
export SCHEDULER=${SCHEDULER:-lsf}
export SHOWQ=${SHOWQ:-/opt/moab/default/bin/showq}
export MSUB=${MSUB:-/opt/moab/default/bin/msub}

export IEMS=0
export ISOL=1
export ICO2=2
export IAER=111
if [ $GEFS_ENSEMBLE = 0 ] ; then

################################################################################
# For the stand alone GFS regression tests.
################################################################################

################################################################################
# Make configure and run files
################################################################################

## determine GOCART and TRACER from gocart_aerosol and passive_tracer 
 export gocart_aerosol=${gocart_aerosol:-NO}
 export passive_tracer=${passive_tracer:-NO}
 if [ $gocart_aerosol = 'YES' ]; then
  export GOCART=1 
 else
  export GOCART=0 
 fi
 if  [ $passive_tracer = 'YES' ]; then
  export TRACER=.true.
 else
  export TRACER=.false.
 fi
##

 JBNME=NEMS_RT_${TEST_NR}_$$

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
                     | sed s:_WRTGP_:${WRTGP}:g                   \
                     | sed s:_THRDS_:${THRD}:g                    \
                     | sed s:_NSOUT_:${NSOUT}:g                   \
                     | sed s:_QUILT_:${QUILT}:g                   \
                     | sed s:_IAER_:${IAER}:g                     \
                     | sed s:_wave_:${wave}:g                     \
                     | sed s:_lm_:${lm}:g                         \
                     | sed s:_lsoil_:${lsoil}:g                   \
                     | sed s:_MEMBER_NAMES_:${MEMBER_NAMES}:g     \
                     | sed s:_CP2_:${CP2}:g                       \
                     | sed s:_RUNDIR_:${RUNDIR}:g                 \
                     | sed s:_PATHTR_:${PATHTR}:g                 \
                     | sed s:_FDFI_:${FDFI}:g                     \
                     | sed s:_FHOUT_:${FHOUT}:g                   \
                     | sed s:_FHZER_:${FHZER}:g                   \
                     | sed s:_FHRES_:${FHRES}:g                   \
                     | sed s:_REDUCEDGRID_:${REDUCEDGRID}:g       \
                     | sed s:_ADIAB_:${ADIAB}:g                   \
                     | sed s:_NSTFCST_:${NST_FCST}:g              \
                     | sed s:_GOCART_:${GOCART}:g                 \
                     | sed s:_TRACER_:${TRACER}:g                 \
                     | sed s:_SFCPRESSID_:${SFCPRESS_ID}:g        \
                     | sed s:_THERMODYNID_:${THERMODYN_ID}:g      \
                     | sed s:_IDVC_:${IDVC}:g                     \
                     | sed s:_NDSLFV_:${NDSLFV}:g                 \
                     | sed s:_SPECTRALLOOP_:${SPECTRALLOOP}:g     \
                     | sed s:_IDEA_:${IDEA}:g                     \
                     | sed s:_CDATE_:${CDATE}:g                   \
                     | sed s:_IEMS_:${IEMS}:g                     \
                     | sed s:_ISOL_:${ISOL}:g                     \
                     | sed s:_NGRID_A2OI_:${NGRID_A2OI}:g         \
                     | sed s:_A2OI_OUT_:${A2OI_OUT}:g             \
                     | sed s:_ICO2_:${ICO2}:g                     \
                     | sed s:_IAER_:${IAER}:g                     \
                     | sed s:_SIGHDR_:${SIGHDR}:g                 \
                     | sed s:_MACHINE_ID_:${MACHINE_ID}:g         \
                     | sed s:_RTPWD_:${RTPWD}:g                   \
                     | sed s:_SCHEDULER_:${SCHEDULER}:g           \
                     | sed s:_SLG_:${SLG}:g                        \
                     | sed s:_SLG_:${SLG}:g                        \
                     | sed s:_NDAYS_:${NDAYS}:g   >  gfs_fcst_run

 chmod 755 gfs_fcst_run

 cp gfs_fcst_run ${RUNDIR}

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
                         
 cp nems.configure ${RUNDIR}
                         
################################################################################
# Copy init files
################################################################################

 ### for now set symbolic links to HYCOM input files
 ln -s ${RTPWD}/HYCOM_glob/* ${RUNDIR}/.
 ###################################################

 ### for now copy MOM5 input files
 if [ $SCHEDULER = 'pbs' -o $SCHEDULER = 'moab' ]; then
   if [ $MACHINE_ID = zeus ] ; then
     /home/Fei.Liu/bin/setup_mom_input.sh ${RUNDIR}
   elif [ $MACHINE_ID = gaea ] ; then
#    /autofs/na1_home1/Fei.Liu/bin/setup_mom_input.sh ${RUNDIR}
     export ocn_input_dir=${ocn_input_dir:-/lustre/f1/unswept/ncep/Shrinivas.Moorthi/nems/NUOPC/OM_stuff/global_box1}
     export ice_input_dir=${ocn_input_dir:-/lustre/f1/unswept/ncep/Shrinivas.Moorthi/nems/NUOPC/OM_stuff/lanl_cice}
    ./setup_mom_input.sh $RUNDIR $ocn_input_dir $ice_input_dir
   fi
 fi
 ###################################################

 cp atmos.configure_gfs ${RUNDIR}/atmos.configure
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
  if [ $IDVC = 2 ] ; then
    export IC_DIR=${IC_DIR:-${RTPWD}/GFS_DFI_POST}
    cp $IC_DIR/gfsanl.$CDATE ${RUNDIR}/.
    cp $IC_DIR/sfcanl.$CDATE ${RUNDIR}/.

#   cp ${RTPWD}/GFS_DFI_REDUCEDGRID_HYB/gfsanl.$CDATE ${RUNDIR}/.
#   cp ${RTPWD}/GFS_DFI_REDUCEDGRID_HYB/sfcanl.$CDATE ${RUNDIR}/.
#to run gfs test
#   if [ "$rungfstest" = ".true." ]; then
#     cp /climate/save/wx20wa/esmf/nems/20120913/data/nemsio/gfsanl.$CDATE ${RUNDIR}/.
#     cp /climate/save/wx20wa/esmf/nems/20120913/data/nemsio/sfcanl.$CDATE ${RUNDIR}/.
#   fi

#  cp /climate/noscrub/wx20wa/esmf/nems/IC/nemsio_new/t62hyb/gfsanl.$CDATE ${RUNDIR}/.
#  cp /climate/noscrub/wx20wa/esmf/nems/IC/nemsio_new/t62hyb/sfnanl.$CDATE ${RUNDIR}/sfcanl.$CDATE


  elif [ $IDVC = 3 ] ; then
    export IC_DIR=${IC_DIR:-${RTPWD}/GFS_NODFI}
    cp $IC_DIR/gfsanl.$CDATE ${RUNDIR}/.
    cp $IC_DIR/sfnanl.$CDATE ${RUNDIR}/.

#   cp ${RTPWD}/GFS_NODFI/gfsanl.$CDATE ${RUNDIR}/.
#   cp ${RTPWD}/GFS_NODFI/sfcanl.$CDATE ${RUNDIR}/.

#  cp /climate/noscrub/wx20wa/esmf/nems/IC/nemsio_new/t62/gfsanl.$CDATE ${RUNDIR}/.
#  cp /climate/noscrub/wx20wa/esmf/nems/IC/nemsio_new/t62/sfnanl.$CDATE ${RUNDIR}/sfcanl.$CDATE
  fi

#                     NO NEMSIO INPUT
#                     ---------------
 else 
   if [ "$IDEA" = ".true." ]; then
     cp ${RTPWD}/WAM_gh_l150/*anl*${CDATE} ${RUNDIR}/.
   else
     export dprefix2=${dprefix2:-""}
     if [ "$rungfstest" = ".true." ] ; then
       if [ $MACHINE_ID = wcoss ] ; then
         IC_DIR=${IC_DIR:-$dprefix2/global/noscrub/Shrinivas.Moorthi/data}
       elif [ $MACHINE_ID = ccs ] ; then
         IC_DIR=${IC_DIR:-$dprefix2/global/noscrub/wx23sm/data}
       elif [ $MACHINE_ID = zeus ] ; then
         IC_DIR=${IC_DIR:-$dprefix2/global/noscrub/Shrinivas.Moorthi/data}
       fi
       cp $IC_DIR/siganl.$CDATE ${RUNDIR}/.
       cp $IC_DIR/sfcanl.$CDATE ${RUNDIR}/.
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
 cp $PATHRT/gfs_configfile_190 $RUNDIR/configure_file

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

JBNME=RT_${TEST_NR}_$$

if [ $SCHEDULER = 'moab' ]; then

 export TPN=$((32/THRD))
 cat gfs_msub.IN     | sed s:_JBNME_:${JBNME}:g   \
                     | sed s:_ACCNR_:${ACCNR}:g   \
                     | sed s:_CLASS_:${CLASS}:g   \
                     | sed s:_WLCLK_:${WLCLK}:g   \
                     | sed s:_TPN_:${TPN}:g       \
                     | sed s:_TASKS_:${TASKS}:g   \
                     | sed s:_RUND_:${RUNDIR}:g   \
                     | sed s:_THRD_:${THRD}:g     >  gfs_msub


elif [ $SCHEDULER = 'pbs' ]; then

 export TPN=$((12/THRD))
 cat gfs_qsub.IN     | sed s:_JBNME_:${JBNME}:g   \
                     | sed s:_ACCNR_:${ACCNR}:g   \
                     | sed s:_WLCLK_:${WLCLK}:g   \
                     | sed s:_TASKS_:${TASKS}:g   \
                     | sed s:_THRD_:${THRD}:g     \
                     | sed s:_RUND_:${RUNDIR}:g   \
                     | sed s:_SCHED_:${SCHEDULER}:g   >  gfs_qsub

elif [ $SCHEDULER = 'lsf' ]; then

 export TPN=${TPN:-$((16/THRD))}
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

cp ../exglobal_fcst.sh.sms_nems ${RUNDIR}

export RUNDIR=${RUNDIR}

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
(echo "GFS, ${TASKS} proc, ${THRD} thread";echo;echo)>> ${REGRESSIONTEST_LOG}
 echo "GFS, ${TASKS} proc, ${THRD} thread";echo;echo

# wait for the job to enter the queue
job_running=0
until [ $job_running -eq 1 ] ; do
 echo "TEST is waiting to enter the queue"
 if [ $SCHEDULER = 'moab' ]; then
  job_running=`$SHOWQ -u ${USER} -n | grep ${JBNME} | wc -l`;sleep 5
 elif [ $SCHEDULER = 'pbs' ]; then
  job_running=`qstat -u ${USER} -n | grep ${JBNME} | wc -l`;sleep 5
 elif [ $SCHEDULER = 'lsf' ]; then
  job_running=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep " dev " | wc -l`;sleep 5
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

  status=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep " dev " | awk '{print $3}'` ; status=${status:--}
  if [ $status != '-' ] ; then FnshHrs=`bpeek -J ${JBNME} | grep Finished | tail -1 | awk '{ print $9 }'` ; fi
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
  job_running=`bjobs -u ${USER} -J ${JBNME} 2>/dev/null | grep " dev " | wc -l`
 fi
  (( n=n+1 ))
done

################################################################################
# Check results
################################################################################

# Give one minute for data to show up on file system
sleep 60

(echo;echo;echo "Checking test ${TEST_NR} results ....")>> ${REGRESSIONTEST_LOG}
 echo;echo;echo "Checking test ${TEST_NR} results ...."

test_status='PASS'

#
if [ ${CREATE_BASELINE} = false ]; then
#
# --- regression test comparison ----
#

  for i in ${LIST_FILES} ; do
    printf %s " Comparing " $i "....." >> ${REGRESSIONTEST_LOG}
    printf %s " Comparing " $i "....."

    if [ ! -f ${RUNDIR}/$i ] ; then

#     echo "Missing " ${RUNDIR}/$i " output file" >> ${REGRESSIONTEST_LOG}
#     echo "Missing " ${RUNDIR}/$i " output file"
#    (echo;echo " Test ${TEST_NR} failed ")>> ${REGRESSIONTEST_LOG}
#     echo;echo " Test ${TEST_NR} failed "
#     exit 2
     echo ".......MISSING file" >> ${REGRESSIONTEST_LOG}
     echo ".......MISSING file"

    elif [ ! -f ${RTPWD}/${CNTL_DIR}/$i ] ; then

     echo ".......MISSING baseline" >> ${REGRESSIONTEST_LOG}
     echo ".......MISSING baseline"

    else

#tcx
#     echo "compare ${RTPWD}/${CNTL_DIR}/$i ${RUNDIR}/$i"

     d=`cmp ${RTPWD}/${CNTL_DIR}/$i ${RUNDIR}/$i | wc -l`

     if [[ $d -ne 0 ]] ; then
#     (echo " ......NOT OK" ; echo ; echo "   $i differ!   ")>> ${REGRESSIONTEST_LOG}
#      echo " ......NOT OK" ; echo ; echo "   $i differ!   " ; exit 2
       echo ".......NOT OK" >> ${REGRESSIONTEST_LOG}
       echo ".......NOT OK"
       test_status='FAIL'
       if [ ${BAIL_CONDITION} = FILE ]; then
          echo "BAIL_CONDITION=FILE, Abort testing on failure"
          exit 2
       fi

     else

       echo "....OK" >> ${REGRESSIONTEST_LOG}
       echo "....OK"
     fi

    fi

  done

#
else
#
# --- create baselines
#

 echo;echo;echo "Moving set ${TEST_NR} files ...."

 for i in ${LIST_FILES} ; do
  printf %s " Moving " $i "....."
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

echo " Test ${TEST_NR} ${test_status} " >> ${REGRESSIONTEST_LOG}
echo " Test ${TEST_NR} ${test_status} "

if [ ${BAIL_CONDITION} = TEST ]; then
  if [ ${test_status} = FAIL ]; then
     echo "BAIL_CONDITION=TEST, Abort testing on failure"
     exit 2
  fi
fi

sleep 4
clear;echo;echo

####################################################################################################
# End test
####################################################################################################

exit 0
