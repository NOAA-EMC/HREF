#! /bin/sh

cd /lfs/h2/emc/lam/noscrub/${USER}/rrfs.v3.1.0/launch/


DOM=${1}
CYC=${2}
DATE=${3}

NTASK=24
NTASK_LINK=60
PTILE_LINK=60

if [ $DOM = "conus" ]
then
PTILE=12
NODES=2
else
PTILE=24
NODES=1
fi

cat href_ensprod_2.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g | sed s:_NTASK_:${NTASK}:g | sed s:_PTILE_:${PTILE}:g |\
sed s:_NTASK_LINK_:${NTASK_LINK}:g | sed s:_PTILE_LINK_:${PTILE_LINK}:g | \
sed s:_NODES_:${NODES}:g > href_ensprod_2.sh_${DOM}_${CYC}

chmod u+x href_ensprod_2.sh_${DOM}_${CYC}

qsub href_ensprod_2.sh_${DOM}_${CYC}
