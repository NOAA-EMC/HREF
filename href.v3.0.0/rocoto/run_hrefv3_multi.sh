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

doms="hi pr"


dir="/gpfs/dell2/emc/modeling/noscrub/${USER}/regional_workflow/rocoto"
dir="/gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/git_repo/EMC_hrw/href.v3.0.0/rocoto"

for dom in $doms
do
rocotorun -v 10 -w ${dir}/drive_hrefv3_${dom}.xml -d ${dir}/drive_hrefv3_${dom}.db
sleep 30
done
