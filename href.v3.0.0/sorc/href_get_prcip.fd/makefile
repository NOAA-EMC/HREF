#!/bin/sh
#
#  This makefile is to compile ensemble production generator get precip code
#  Author: Binbin Zhou
#  OS: Intel WCOSS
#

INC= ${G2_INC4}
LIBS=   ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} ${SP_LIB4} ${G2_LIB4}  \
        ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}
FFLAGS_DEBUG =        -g -traceback -O3 -ftrapuv  -check all  -fp-stack-check  -fstack-protector  -assume noold_ldout_format -g -convert big_endian -I $(INC)
FFLAGS =        -fp-model strict -g -traceback -O3 -assume noold_ldout_format -g -convert big_endian -I $(INC)

all:	clean href_get_prcip install

href_get_prcip:
	$(FC) $(FFLAGS) -o href_get_prcip href_get_prcip.f $(LIBS)
debug:
	$(FC) $(FFLAGS_DEBUG) -o href_get_prcip href_get_prcip.f $(LIBS)
clean:
	/bin/rm   href_get_prcip
install:
	/bin/cp href_get_prcip ../../exec/href_get_prcip
#
