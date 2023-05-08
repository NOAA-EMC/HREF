#! /bin/sh

export USER=Jun.Du
cd /lfs/h2/emc/lam/noscrub/${USER}/rrfs.v1.0.0/launch/

DOM=${1}
CYC=${2}
DATE=${3}
TYPE=${4}

cat enspost_ffggen.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g |  sed s:_TYPE_:${TYPE}:g > enspost_ffg_gen.sh_${DOM}_${CYC}

chmod u+x enspost_ffg_gen.sh_${DOM}_${CYC}

qsub enspost_ffg_gen.sh_${DOM}_${CYC}
