#! /bin/sh

module purge
module load ../modulefiles/HREF/v2.0.0
module list

sleep 1

BASE=`pwd`

mkdir -p ../exec

GET_PRCIP=1
FV3BUCKET=1
FFG_GEN=1
ENSPROD=1

#########################

if [ $GET_PRCIP = "1" ]
then
cd ${BASE}/href_get_prcip.fd
make copy
make clean
fi

############################

if [ $FV3BUCKET = "1" ]
then
cd ${BASE}/href_fv3bucket.fd
make copy
make clean
fi

############################

if [ $FFG_GEN = "1" ]
then
cd ${BASE}/href_ffg_gen.fd
make copy
make clean
fi

############################


if [ $ENSPROD = "1" ]
then
cd ${BASE}/href_ensprod.fd
make copy
make clean
fi
