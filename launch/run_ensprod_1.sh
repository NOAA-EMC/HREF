#! /bin/sh

cd /lfs/h2/emc/lam/noscrub/Matthew.Pyle/HREF_wcoss2/href.v3.1.0/launch/


DOM=${1}
CYC=${2}
DATE=${3}


NTASK=36

if [ $DOM = "conus" ]
then
NODES=2
PTILE=18
else
NODES=1
PTILE=36
fi

cat href_ensprod_1.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g | sed s:_NTASK_:${NTASK}:g | sed s:_NODES_:${NODES}:g | \
sed s:_PTILE_:${PTILE}:g > href_ensprod_1.sh_${DOM}_${CYC}

chmod u+x href_ensprod_1.sh_${DOM}_${CYC}

qsub href_ensprod_1.sh_${DOM}_${CYC}
