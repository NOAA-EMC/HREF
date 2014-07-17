#!/bin/ksh
set -ua

#WY
# RUNDIR=/ptmp/wx20wy/multi_phys_gefs
RUNDIR=/ptmp/wx20wy/multi_phys_gefs_copy1

DATA_DIR=/gpfs/t2c/global/noscrub/wx20wy/gefs.20100912

#WY
# PATH_DIR=/gpfs/t3/global/save/wx20wy/svn_nems_trunk/trunk/job/multi_phys_gefs
PATH_DIR=/gpfs/t3/global/save/wx20wy/svn_nems_trunk/trunk_copy1/job/multi_phys_gefs

mkdir -p ${RUNDIR}

#########################################
# For the concurrency ensemble GEFS test.
#########################################

cd ${PATH_DIR}

echo 'RUNDIR=' ${RUNDIR}

  cp ${DATA_DIR}/gfsanl* ${RUNDIR}
  cp ${DATA_DIR}/sfcanl* ${RUNDIR}

NDSLFV=.false.
ENS_NUM=21

cat multi_phys_gefs_run.IN \
                    | sed s:_SRCDIR_:${PATH_DIR}:g \
                    | sed s:_NDSLFV_:${NDSLFV}:g \
                    | sed s:_ENS_NUM_:${ENS_NUM}:g \
                    | sed s:_RUNDIR_:${RUNDIR}:g > multi_phys_gefs_run

cp Chem_Registry.rc    ${RUNDIR}/Chem_Registry.rc
cp atmos.configure_gfs ${RUNDIR}/atmos.configure

rm -f ${RUNDIR}/PET* 

####################################################################################################
# Submit test
####################################################################################################

JBNME=multi_phys_gefs_test

CLASS=dev
# CLASS=debug

GROUP=dev
ACCNR=GFS-MTN

TASKS=672
NODES=12

#TASKS=128
#NODES=2

THRD=1
WALL_CLOCK_HOUR=2

WALL_CLOCK_MINS=30
# WALL_CLOCK_MINS=0

cat multi_phys_gefs_ll.IN       | sed s:_JBNME_:${JBNME}:g   \
                                | sed s:_CLASS_:${CLASS}:g   \
                                | sed s:_GROUP_:${GROUP}:g   \
                                | sed s:_ACCNR_:${ACCNR}:g   \
                                | sed s:_TASKS_:${TASKS}:g   \
                                | sed s:_NODES_:${NODES}:g   \
                                | sed s:_WALL_CLOCK_HOUR_:${WALL_CLOCK_HOUR}:g   \
                                | sed s:_WALL_CLOCK_MINS_:${WALL_CLOCK_MINS}:g   \
                                | sed s:_THRDS_:${THRD}:g    >  multi_phys_gefs_ll

llsubmit multi_phys_gefs_ll 2>&1 

exit 0
