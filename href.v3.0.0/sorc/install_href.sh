#! /bin/sh


BASE=`pwd`

mkdir -p ../exec

GET_PRCIP=1
FFG_GEN=1
ENSPROD=1

#########################

if [ $GET_PRCIP = "1" ]
then
cd ${BASE}/href_get_prcip.fd
make install
# make clean
fi

############################

if [ $FFG_GEN = "1" ]
then
cd ${BASE}/href_ffg_gen.fd
make install
# make clean
fi

############################


if [ $ENSPROD = "1" ]
then
cd ${BASE}/href_ensprod.fd
make install
# make clean
fi
