#! /bin/sh

module purge
module use -a ../modulefiles/HREF
module load v3.1.0
module list

sleep 1

BASE=`pwd`


#########################

cd ${BASE}/href_fv3_3hqpf.fd
make clean
make all
