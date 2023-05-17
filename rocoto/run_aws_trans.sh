#!/bin/bash

module load core/rocoto/1.3.5

module list

echo WGRIB2 is $WGRIB2

dir="/lfs/h2/emc/lam/noscrub/emc.lam/enspost/rrfs.v1.0.0/rocoto"

thing=drive_rrfsa_enspost
rocotorun -v 10 -w ${dir}/${thing}.xml -d ${dir}/${thing}.db

