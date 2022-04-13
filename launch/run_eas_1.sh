#! /bin/sh

cd /lfs/h2/emc/lam/noscrub/${USER}/rrfs.v3.1.0/launch/


DOM=${1}
CYC=${2}
DATE=${3}

if [ $DOM = "conus" ]
then
	NODES=2
	PTILE=80
	NTASK=160

elif [ $DOM = "ak" ]
then
	NODES=1
	PTILE=106
	NTASK=106
else
	NODES=1
	PTILE=64
	NTASK=64
fi

cat href_eas_1.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g | sed s:_NODES_:${NODES}:g  | sed s:_PTILE_:${PTILE}:g | \
sed s:_NTASK_:${NTASK}:g > href_eas_1.sh_${DOM}_${CYC}

chmod u+x href_eas_1.sh_${DOM}_${CYC}

qsub href_eas_1.sh_${DOM}_${CYC}
