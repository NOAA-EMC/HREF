#!/bin/bash

module load core/rocoto/1.3.5

module list

doms="conus ak pr hi"

dir="/lfs/h2/emc/lam/noscrub/Matthew.Pyle/enspost/rrfs.v1.1.0/rocoto"

for dom in $doms
do

rocotorun -v 10 -w ${dir}/drive_rrfs_enspost_${dom}_singlephys_timelag.xml -d ${dir}/drive_rrfs_enspost_${dom}_singlephys_timelag.db

sleep 15

done
