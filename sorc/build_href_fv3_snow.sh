#! /bin/sh

module purge
module use -a ../modulefiles/HREF
module load v3.1.0
module list

sleep 1

BASE=`pwd`


#########################

cd ${BASE}/href_fv3snowbucket.fd
make clean
make href_fv3bucket
