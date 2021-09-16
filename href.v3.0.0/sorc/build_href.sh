#! /bin/sh

module purge
# source /apps/prod/lmodules/startLmod

source ../versions/build.ver

env | grep ver

module use -a  ../modulefiles/HREF
module load v3.0.0
module list

sleep 1

BASE=`pwd`

mkdir -p ../exec
mkdir -p ./log/

GET_PRCIP=1
FFG_GEN=1
ENSPROD=1

#########################

if [ $GET_PRCIP = "1" ]
then
./build_href_get_prcip.sh > ./log/build_href_get_prcip.log 2>&1
fi

############################

if [ $FFG_GEN = "1" ]
then
./build_href_ffg_gen.sh > ./log/build_href_ffg_gen.log 2>&1
fi

############################


if [ $ENSPROD = "1" ]
then
./build_href_ensprod.sh > ./log/build_href_ensprod.log 2>&1
fi
