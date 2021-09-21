#! /bin/sh

cd /lfs/h2/emc/lam/noscrub/Matthew.Pyle/HREF_fork/href.v3.0.0/launch/


DOM=${1}
CYC=${2}
DATE=${3}

NTASK=55
NODES=4
PTILE=14

cat preproc_nam.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g | sed s:_NTASK_:${NTASK}:g | sed s:_NODES_:${NODES}:g | \
sed s:_PTILE_:${PTILE}:g > preproc_nam.sh_${DOM}_${CYC}

chmod u+x preproc_nam.sh_${DOM}_${CYC}

qsub preproc_nam.sh_${DOM}_${CYC}
