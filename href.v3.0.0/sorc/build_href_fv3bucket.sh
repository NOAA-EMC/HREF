#! /bin/sh

module purge
module load ../modulefiles/HREF/v3.0.0
module list

BASE=`pwd`

cd ${BASE}/href_fv3bucket.fd
make clean
make href_fv3bucket 
