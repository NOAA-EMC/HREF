

module load  w3emc/v2.2.0
module load  w3nco/v2.0.6
module load bacio/v2.0.2
module load g2/v2.5.0
module load g2tmpl/v1.3.0  
module load jasper/v1.900.1
module load png/v1.2.44
module load z/v1.2.6

module list

export FFLAGS=-O3

ifort -o tst_stitch.x tst_stitch.f90 $FFLAGS $G2_LIB4 $W3NCO_LIB4 $BACIO_LIB4 $G2_LIB4 -I$G2_INC4 $JASPER_LIB $PNG_LIB $Z_LIB
