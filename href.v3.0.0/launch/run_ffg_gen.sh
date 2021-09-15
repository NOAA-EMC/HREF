#! /bin/sh

cd /lfs/h2/emc/lam/noscrub/Matthew.Pyle/HREF_fork/href.v3.0.0/launch/

DOM=${1}
CYC=${2}
DATE=${3}

cat ffggen.sh_in | sed s:_DOM_:${DOM}:g |  sed s:_CYC_:${CYC}:g | \
sed s:_DATE_:${DATE}:g > ffg_gen.sh_${DOM}_${CYC}

chmod u+x ffg_gen.sh_${DOM}_${CYC}

qsub ffg_gen.sh_${DOM}_${CYC}
