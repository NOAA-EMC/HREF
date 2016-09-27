#!/bin/bash
set -xeu

hostname

die() { echo "$@" >&2; exit 1; }
usage() {
  set +x
  echo
  echo "Usage: $0 -c <model> | -f | -s | -l <file> | -m | -h"
  echo
  echo "  -c  create new baseline results for <model>"
  echo "  -f  run full suite of regression tests"
  echo "  -s  run standard suite of regression tests"
  echo "  -l  runs test specified in <file>"
  echo "  -m  compare against new baseline results"
  echo "  -h  display this help"
  echo
  exit 1
}

[[ $# -eq 0 ]] && usage

source detect_machine.sh

export dprefix=""
export MACHINE_ID=${MACHINE_ID:-wcoss}
if [ $MACHINE_ID = wcoss ]; then
  source /usrx/local/Modules/default/init/sh
  export DISKNM=/meso
  export pex=1           # for wcoss phase1
  export pex=${pex:-2}   # default - phase2
  export CLASS=debug # dev
  export ACCNR=dev
  if [ $pex -eq 2 ] ; then
#   export CLASS=dev$pex
   export CLASS=debug$pex
   export ACCNR=dev$pex
  fi
# export STMP=/stmp$pex
  export STMP=/ptmpp$pex
  export PTMP=/ptmpp$pex
  export SCHEDULER=lsf
  export SIGHDR=/global/save/Shrinivas.Moorthi/para/sorc/global_sighdr.fd/global_sighdr
elif [ $MACHINE_ID = gaea ]; then
  export DISKNM=/lustre/f1/unswept/ncep/Ratko.Vasic
  export STMP=/lustre/f1/ncep
  export PTMP=/lustre/f1/ncep
  export SCHEDULER=moab
elif [ $MACHINE_ID = theia ]; then
  source /apps/lmod/lmod/init/sh
  export ACCNR
  export dprefix=/scratch4/NCEPDEV
  export DISKNM=$dprefix/meso
  export STMP=$dprefix/stmp4
  export PTMP=$dprefix/stmp3
  export SCHEDULER=pbs
  export SIGHDR=$dprefix/global/save/Shrinivas.Moorthi/para/sorc/global_sighdr.fd/global_sighdr
  export SLG=.false.
else
  die "Unknown machine ID, please edit detect_machine.sh file"
fi
export pex=${pex:-""}

if [ $MACHINE_ID = wcoss ]; then
 cp gfs_fcst_run.IN_IBM gfs_fcst_run.IN
else
 cp gfs_fcst_run.IN_Linux gfs_fcst_run.IN
fi

############################################################
# RTPWD - Path to previously stored regression test answers
############################################################
#export RTPWD=${DISKNM}/noscrub/wx20rv/REGRESSION_TEST
#export RTPWD=${DISKNM}/noscrub/wx20rv/REGRESSION_TEST_new

export CREATE_BASELINE=false
CB_arg=''
TESTS_FILE='rt.conf'
SET_ID='standard'
while getopts ":c:fsl:mh" opt; do
  case $opt in
    c)
      export CREATE_BASELINE=true
      CB_arg=$OPTARG
      SET_ID=' '
      ;;
    f)
      SET_ID=' '
      ;;
    s)
      SET_ID='standard'
      ;;
    l)
      TESTS_FILE=$OPTARG
      SET_ID=' '
      ;;
    m)
      export RTPWD=${STMP}/${USER}/REGRESSION_TEST
      ;;
    h)
      usage
      ;;
    \?)
      usage
      die "Invalid option: -$OPTARG"
      ;;
    :)
      usage
      die "Option -$OPTARG requires an argument."
      ;;
  esac
done

export RTPWD=${RTPWD:-${DISKNM}/noscrub/wx20rv/REGRESSION_TEST}

shift $((OPTIND-1))
[[ $# -gt 0 ]] && usage

mkdir -p $STMP/$USER
mkdir -p $PTMP/$USER

if [[ $CREATE_BASELINE == true ]]; then
  #
  # prepare new regression test directory
  #
  export RTPWD_U=${STMP}/${USER}/REGRESSION_TEST
  rm -rf ${RTPWD_U}
  echo "copy REGRESSION_TEST_baselines"
  mkdir -p ${STMP}/${USER}
  cp -r ${DISKNM}/noscrub/wx20rv/REGRESSION_TEST_baselines ${RTPWD_U}

  if [[ $CB_arg != gfs ]]; then
    echo "copy gfs"
    cp ${RTPWD}/GFS_EULERIAN/*             ${RTPWD_U}/GFS_EULERIAN/.
    cp ${RTPWD}/WAM_gh_l150/*              ${RTPWD_U}/WAM_gh_l150/.
    cp ${RTPWD}/GFS_GOCART_NEMSIO/*        ${RTPWD_U}/GFS_GOCART_NEMSIO/.
    cp ${RTPWD}/GFS_SLG_ADIABATIC/*        ${RTPWD_U}/GFS_SLG_ADIABATIC/.
    cp ${RTPWD}/GFS_SLG/*                  ${RTPWD_U}/GFS_SLG/.
    cp ${RTPWD}/GFS_SLG_RSTHST/*           ${RTPWD_U}/GFS_SLG_RSTHST/.
    cp ${RTPWD}/GFS_SLG_48PE/*             ${RTPWD_U}/GFS_SLG_48PE/.
    cp ${RTPWD}/GFS_SLG_T574/*             ${RTPWD_U}/GFS_SLG_T574/.
    cp ${RTPWD}/GFS_SLG_NSST/*             ${RTPWD_U}/GFS_SLG_NSST/.
  fi
  if [[ $CB_arg != nmm ]]; then
    echo "copy nmm"
    cp ${RTPWD}/NMMB_2way_nests/*          ${RTPWD_U}/NMMB_2way_nests/.
#    cp ${RTPWD}/NMMB_gfsP_glob/*           ${RTPWD_U}/NMMB_gfsP_glob/.
#    cp ${RTPWD}/NMMB_gfsP_reg/*            ${RTPWD_U}/NMMB_gfsP_reg/.
    cp ${RTPWD}/NMMB_glob/*                ${RTPWD_U}/NMMB_glob/.
    cp ${RTPWD}/NMMB_mvg_nests/*           ${RTPWD_U}/NMMB_mvg_nests/.
    cp ${RTPWD}/NMMB_nests/*               ${RTPWD_U}/NMMB_nests/.
    cp ${RTPWD}/NMMB_reg/*                 ${RTPWD_U}/NMMB_reg/.
    cp ${RTPWD}/NMMB_reg_filt/*            ${RTPWD_U}/NMMB_reg_filt/.
    cp ${RTPWD}/NMMB_reg_filt_zombie/*     ${RTPWD_U}/NMMB_reg_filt_zombie/.
    cp ${RTPWD}/NMMB_reg_hur/*             ${RTPWD_U}/NMMB_reg_hur/.
    cp ${RTPWD}/NMMB_reg_pcpadj/*          ${RTPWD_U}/NMMB_reg_pcpadj/.
#    cp ${RTPWD}/NMMB_reg_post/*            ${RTPWD_U}/NMMB_reg_post/.
    cp ${RTPWD}/NMMB_reg_sas_zhao/*        ${RTPWD_U}/NMMB_reg_sas_zhao/.
    cp ${RTPWD}/NMMB_reg_sel_phy/*         ${RTPWD_U}/NMMB_reg_sel_phy/.
    cp ${RTPWD}/NMMB_reg_spec_adv/*        ${RTPWD_U}/NMMB_reg_spec_adv/.
    cp ${RTPWD}/NMMB_reg_thomp/*           ${RTPWD_U}/NMMB_reg_thomp/.
    cp ${RTPWD}/NMMB_reg_timesr/*          ${RTPWD_U}/NMMB_reg_timesr/.
    cp ${RTPWD}/NMMB_reg_wsm6_gfdl/*       ${RTPWD_U}/NMMB_reg_wsm6_gfdl/.
    cp ${RTPWD}/NMMB_reg_wsm6_rrtm/*       ${RTPWD_U}/NMMB_reg_wsm6_rrtm/.
  fi
  if [[ $CB_arg != fim ]]; then
    echo "copy fim"
    #cp ${RTPWD}/FIMdata/*                  ${RTPWD_U}/FIMdata/.
    #TODO:  generalize with $GLVL (see below)
    #cp ${RTPWD}/FIM_G4L38_24hr/*           ${RTPWD_U}/FIM_G4L38_24hr/.
  fi
  if [[ $CB_arg != post ]]; then
    echo "copy post"
#    cp    ${RTPWD}/NMMB_reg_post/*         ${RTPWD_U}/NMMB_reg_post/.
#    cp -r ${RTPWD}/GFS_GOCART_POST/*       ${RTPWD_U}/GFS_GOCART_POST/.
  fi
fi

###################################
# PATHRT - Path to regression test
###################################

export PATHRT=`pwd`            # Path to regression test scripts
export PATHTR=${PATHRT}/..     # Path to NEMS trunk
export REGRESSIONTEST_LOG=${PATHRT}/RegressionTests_$MACHINE_ID.log
COMPILE_LOG=${PATHRT}/Compile_$MACHINE_ID.log

date > ${REGRESSIONTEST_LOG}
echo "Start Regression test" >> ${REGRESSIONTEST_LOG}
(echo;echo;echo)             >> ${REGRESSIONTEST_LOG}

export RUNDIR_ROOT=${PTMP}/${USER}/rt_$$
mkdir -p ${RUNDIR_ROOT}

source default_vars.sh

export TEST_NR=0
rm -f fail_test
export TEST_NAME
cat $TESTS_FILE | while read line; do

  line="${line#"${line%%[![:space:]]*}"}"
  [[ ${#line} == 0 ]] && continue
  [[ $line == \#* ]] && continue

  if [[ $line == COMPILE* ]] ; then
    (
      NEMS_VER=`echo $line | cut -d'|' -f2`
      SET=`     echo $line | cut -d'|' -f3`
      MACHINES=`echo $line | cut -d'|' -f4`
      ESMF_VER=`echo $line | cut -d'|' -f5 | sed -e 's/^ *//' -e 's/ *$//'`
      [[ $SET_ID != ' ' && $SET != *${SET_ID}* ]] && continue
      [[ $MACHINES != ' ' && $MACHINES != *${MACHINE_ID}* ]] && continue

      echo "Compiling $NEMS_VER $ESMF_VER"
      cd $PATHTR/src
      ./configure ${ESMF_VER}_${MACHINE_ID}                        > $COMPILE_LOG 2>&1
      source conf/modules.nems                                    >> $COMPILE_LOG 2>&1
      module list                                                 >> $COMPILE_LOG 2>&1
      gmake clean                                                 >> $COMPILE_LOG 2>&1
      gmake ${NEMS_VER} J=-j2                                     >> $COMPILE_LOG 2>&1
      cd $PATHRT
    )
    continue
  fi

  if [[ $line == RUN* ]] ; then
    TEST_NAME=`echo $line | cut -d'|' -f2 | sed -e 's/^ *//' -e 's/ *$//'`
    SET=`      echo $line | cut -d'|' -f3`
    MACHINES=` echo $line | cut -d'|' -f4`
    CB=`       echo $line | cut -d'|' -f5`
    [[ -e "tests/$TEST_NAME" ]] || die "run test file tests/$TEST_NAME does not exist"
    [[ $SET_ID != ' ' && $SET != *${SET_ID}* ]] && continue
    [[ $MACHINES != ' ' && $MACHINES != *${MACHINE_ID}* ]] && continue
    [[ $CREATE_BASELINE == true && $CB != *${CB_arg}* && 'all' != *${CB_arg}* ]] && continue

    (( TEST_NR += 1 ))
    (
      export RUNDIR=${RUNDIR_ROOT}/${TEST_NAME}
      source tests/$TEST_NAME
      export JBNME=`basename $RUNDIR_ROOT`_${TEST_NR}
      echo "Test ${TEST_NR} ${TEST_NAME} ${TEST_DESCR}" >> ${REGRESSIONTEST_LOG}
      echo "Test ${TEST_NR} ${TEST_NAME} ${TEST_DESCR}"
      ./${RUN_SCRIPT} || die "Test ${TEST_NR} ${TEST_NAME} ${TEST_DESCR} failed"
    )
    continue
  fi

  die "Unknown command $line"

done

if [ -e fail_test ]; then
  echo "FAILED TESTS: "
  echo "FAILED TESTS: " >> ${REGRESSIONTEST_LOG}
  for failed_test_name in `cat fail_test`
  do
    echo "Test " ${failed_test_name} " failed "
    echo "Test " ${failed_test_name} " failed " >> ${REGRESSIONTEST_LOG}
  done
  echo ; echo REGRESSION TEST FAILED
  (echo ; echo REGRESSION TEST FAILED) >> ${REGRESSIONTEST_LOG}
else
  echo ; echo REGRESSION TEST WAS SUCCESSFUL
  (echo ; echo REGRESSION TEST WAS SUCCESSFUL) >> ${REGRESSIONTEST_LOG}
fi

# Finalize, Clenaup
rm -f err out nmm_msub nmm_bsub nmm_qsub nmm_run gfs_fcst_run \
nems.configure gfs_qsub gfs_fcst_run.IN ngac_qsub ngac_bsub gfs_bsub \
configure_file_01 configure_file_02 configure_file_03 configure_file_04 \
atmos.configure fail_test

date >> ${REGRESSIONTEST_LOG}

exit
