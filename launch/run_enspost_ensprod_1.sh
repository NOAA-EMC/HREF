#! /bin/sh

export USER=Jun.Du
cd /lfs/h2/emc/lam/noscrub/${USER}/rrfs.v1.0.0/launch/


DOM=${1}
CYC=${2}
DATE=${3}
TYPE=${4}


NTASK=36

if [ $DOM = "conus" ]
then
#NODES=2
#PTILE=18
NODES=3
PTILE=12
else
NODES=1
PTILE=36
fi

cat enspost_ensprod_1.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g | sed s:_NTASK_:${NTASK}:g | sed s:_NODES_:${NODES}:g | \
sed s:_PTILE_:${PTILE}:g | sed s:_TYPE_:${TYPE}:g > enspost_ensprod_1.sh_${DOM}_${CYC}

chmod u+x enspost_ensprod_1.sh_${DOM}_${CYC}

qsub enspost_ensprod_1.sh_${DOM}_${CYC}
