#! /bin/sh

module purge
module use -a ../modulefiles/RRFS_ENSPOST
module load v1.0.0
module list

sleep 1

BASE=`pwd`


#########################

cd ${BASE}/enspost_fv3_3hqpf.fd
make clean
make enspost_fv3_3hqpf
