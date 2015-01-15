#!/bin/sh
#
#  This makefile is to compile ensemble production generator
#  Author: Binbin Zhou
#          Aug 1, 2005
#  OS: Linux (on WCOSS)
#


FC=     ifort -convert big_endian
INC= /nwprod/lib/incmod/g2_4
LIBS=   -L/nwprod/lib -lip_4 -lw3nco_4 -lw3emc_4  -lbacio_4 -lsp_4 -lg2_4 -ljasper -lpng -lz
#FFLAGS = -O2 -g -convert big_endian -openmp -auto  -I $(INC)
FFLAGS = -O3 -openmp -auto -I $(INC)

SRCS=	char_process.f util.f readGB2.f print_gfld.f Zputgb2.f \
	packGB2_mean.f packGB2_prob.f packGB2_mean_derv.f packGB2_prob_derv.f \
	preciptype.f wind.f get_icing.f get_cat.f getflight.f Hains_index.f getceiling.f \
	new_fog.f get_new_fog.f thickness.f llws.f getconv.f \
	get_mxp.f mxp.f packGB2_mxp.f \
	get_cptp_severe.f calibrate2_hrly_rgn3.f calibrate2_dryt.f  calibrate2_svr.f calibrate2.f \
	meanwind.f

OBJ=${SRCS:.f=.o}

# nsse: $(OBJ)
#	$(FC) $(FFLAGS) -o nsse_ensprod  nsse_ens_gen.f $(OBJ)  $(LIBS)
sref: $(OBJ)
	$(FC) $(FFLAGS) -o ../../exec/sref_ensprod  sref_ens_gen.f $(OBJ)  $(LIBS)
clear:
	/bin/rm  *.o nsse_ensprod sref_ensprod
#

