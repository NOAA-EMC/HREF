#! /bin/sh

cd /lfs/h2/emc/lam/noscrub/Matthew.Pyle/HREF_fork/href.v3.0.0/launch/

DOM=${1}
CYC=${2}
DATE=${3}

cat gempak.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g > gempak.sh_${DOM}_${CYC}

chmod u+x gempak.sh_${DOM}_${CYC}

qsub gempak.sh_${DOM}_${CYC}
