#!/bin/bash

module load core/rocoto/1.3.5

module list

sleepval=20
doms="conus hi pr" 

last_dom="${doms##* }"

dir="/lfs/h2/emc/lam/noscrub/Matthew.Pyle/enspost/refs.v1.0.0/rocoto"

for dom in $doms
do

echo running ${dom}

rocotorun -v 10 -w ${dir}/drive_refs_enspost_${dom}.xml -d ${dir}/drive_refs_enspost_${dom}.db

if [ $dom != ${last_dom} ]; then
echo sleep ${sleepval}
sleep ${sleepval}
else
echo not sleeping
date
fi

done
