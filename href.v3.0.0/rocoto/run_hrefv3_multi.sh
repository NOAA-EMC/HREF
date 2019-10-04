#!/bin/bash -l
set +x
# . /usrx/local/prod/lmod/lmod/init/sh
set -x

# dell stuff?

# module load impi/18.0.1
# module load lsf/10.1

# module use /gpfs/dell3/usrx/local/dev/emc_rocoto/modulefiles/
# module load ruby/2.5.1 rocoto/1.2.4

module load rocoto/1.2.4

###
### for using latest WGRIB2
###
module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
module switch grib_util/1.1.1

module use -a /opt/modulefiles
module load gcc/4.9.2

export WGRIB2=$WGRIB2

doms="hi pr conus ak conus_alt"


echo WGRIB2 is $WGRIB2

dir="/gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/HREF_fork/href.v3.0.0/rocoto"

for dom in $doms
do
rocotorun -v 10 -w ${dir}/drive_hrefv3_${dom}.xml -d ${dir}/drive_hrefv3_${dom}.db
sleep 3
done
