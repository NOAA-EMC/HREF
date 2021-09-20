#! /bin/sh

module purge
module use -a ../modulefiles/HREF/
module load v3.0.0
module list

sleep 1

BASE=`pwd`

cd ${BASE}/href_ffg_gen.fd
make clean
# make all
make href_ffg_gen
# make debug
