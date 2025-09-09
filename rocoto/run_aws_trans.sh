#!/bin/bash

module load core/rocoto/1.3.5

module list

echo WGRIB2 is $WGRIB2

dir="/lfs/h2/emc/lam/noscrub/emc.lam/enspost/refs.v1.0.0/rocoto"

# make sure we are on prod machine

qstat -Q | grep devmax | grep no
err=$?

if [ $err -ne 0 ]
then
                echo "NOT prod machine, so exit"
                        exit
                else
                        echo "looks like prod, so proceed"
fi

doms="conus ak hi pr"
for dom in $doms
do
thing=drive_refs_transfers_${dom}_mixed
rocotorun -v 10 -w ${dir}/${thing}.xml -d ${dir}/${thing}.db
sleep 12
done

# thing=drive_refs_transfers_conus_mixed
# rocotorun -v 10 -w ${dir}/${thing}.xml -d ${dir}/${thing}.db
# sleep 12

# thing=drive_refs_transfers_ak_mixed
# rocotorun -v 10 -w ${dir}/${thing}.xml -d ${dir}/${thing}.db

