#!/bin/sh

COMP=intel

module load PrgEnv-intel
module load cray-mpich

module load jasper-gnu-sandybridge/1.900.1
module load png-intel-sandybridge/1.2.44
module load zlib-intel-sandybridge/1.2.7

echo JASPER_INC $JASPER_INC

case ${COMP:?} in
  intel)
    export FC=${1:-ftn}
    export CC=${2:-cc}
    export CPP=${3:-cpp -p} 
    export flagOpt="-O3 -axCore-AVX2 -g"
    export flag64flt="-r8"
    export flagFort="-assume noold_ldout_format -module"
  ;;
  cray)
    export FC=${1:-ftn}
    export CC=${2:-cc}
    export CPP=${3:-CC}
    export flagOpt="-O2 -G2"
    export flag64flt="-s real64"
    export flagFort="-J"
  ;;
  *)
    >&2 echo "Don't know how to build lib under $COMP compiler"
    exit 1
  ;;
esac

echo here 1

export LIB=${G2_LIB4:-../${COMP}/libg2_4.a}
export MODDIR=${G2_INC4:-../${COMP}/include/g2_4}
mkdir -p $(dirname $LIB) $MODDIR
echo makefile_4
make -f makefile_4

exit

export LIB=${G2_LIBd:-../${COMP}/libg2_d.a}
export MODDIR=${G2_INCd:-../${COMP}/include/g2_d}
mkdir -p $(dirname $LIB) $MODDIR
make -f makefile_d

