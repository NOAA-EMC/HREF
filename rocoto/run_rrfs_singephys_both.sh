#!/bin/bash

module load core/rocoto/1.3.5

module list

doms="ak"

echo WGRIB2 is $WGRIB2

dir="/lfs/h2/emc/lam/noscrub/Matthew.Pyle/enspost/rrfs.v1.2.0_winter/rocoto"

for dom in $doms
do

rocotorun -v 10 -w ${dir}/drive_rrfs_enspost_${dom}_singlephys_timelag.xml -d ${dir}/drive_rrfs_enspost_${dom}_singlephys_timelag.db

# sleep 60


done
