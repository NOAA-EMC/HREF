#! /bin/sh

cd /lfs/h2/emc/lam/noscrub/${USER}/rrfs.v3.1.0/launch/


DOM=${1}
CYC=${2}
DATE=${3}

NTASK=61
NODES=1
PTILE=61

cat preproc_fv3.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g | sed s:_NTASK_:${NTASK}:g | sed s:_NODES_:${NODES}:g | \
sed s:_PTILE_:${PTILE}:g > preproc_fv3.sh_${DOM}_${CYC}

chmod u+x preproc_fv3.sh_${DOM}_${CYC}

qsub preproc_fv3.sh_${DOM}_${CYC}
