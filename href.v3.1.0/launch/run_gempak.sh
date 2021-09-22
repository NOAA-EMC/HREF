#! /bin/sh

cd /lfs/h2/emc/lam/noscrub/Matthew.Pyle/HREF_fork/href.v3.0.0/launch/

DOM=${1}
CYC=${2}
DATE=${3}

if [ $DOM = "conus" ]
then
	PTILE=8
	NTASK=8
else
	PTILE=7
	NTASK=7
fi

cat gempak.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g  | sed s:_NTASK_:${NTASK}:g | sed s:_PTILE_:${PTILE}:g > gempak.sh_${DOM}_${CYC}

chmod u+x gempak.sh_${DOM}_${CYC}

qsub gempak.sh_${DOM}_${CYC}
