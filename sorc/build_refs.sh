#! /bin/sh
#set -x

module purge

source ../versions/build.ver

env | grep ver

module use -a  ../modulefiles/RRFS_ENSPOST
module load v1.0.0
module list

sleep 1

BASE=`pwd`

mkdir -p ../exec
mkdir -p ./log/

GET_PRCIP=1
FFG_GEN=1
ENSPROD=1
QPF3H=1
FV3SNOW=1

#########################

if [ $GET_PRCIP = "1" ]
then
./build_refs_get_prcip.sh > ./log/build_refs_get_prcip.log 2>&1
fi

#########################

if [ $FFG_GEN = "1" ]
then
./build_refs_ffg_gen.sh > ./log/build_refs_ffg_gen.log 2>&1
fi

############################


if [ $ENSPROD = "1" ]
then
./build_refs_ensprod.sh > ./log/build_refs_ensprod.log 2>&1
fi

############################

if [ $QPF3H = "1" ]
then
./build_refs_preproc_3hqpf.sh >& ./log/build_refs_preproc_3hqpf.log
fi

############################

if [ $FV3SNOW = "1" ]
then
./build_refs_preproc_snow.sh >& ./log/build_refs_preproc_snow.log
fi

