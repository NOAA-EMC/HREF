#!/bin/sh
#
#  This makefile is to compile ensemble production generator
#  Author: Binbin Zhou
#          Aug 1, 2005
#  OS: AIX (IBM-blue)
#


FC=     ifort -O2 -convert big_endian  
ARCH     =
OPTS     = 
LIST     =
TRAPS    =
PROFILE  =
DEBUG    =
MEM      =
MAP      =
W3LIBDIR = /nwprod/lib
ESSL     = 
MASS     =


SRC=	util.f \
	char_process.f \
	print.f pack.f \
	thickness.f preciptype.f apcp.f snow.f wind.f getceiling.f \
	dtra.f getfog.f frzn.f vorticity.f getflight.f getcldtop.f sref_ens_gen.f

OBJ=${SRC:.f=.o}

LIBS= $(ESSL) $(MASS) $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG) -L$(W3LIBDIR) -lw3nco_4 -lw3emc_4 -lbacio_4 -lip_4

../../exec/hiresw_ens_gen: $(OBJ)
	$(FC)  -o $(@) $(OBJ) $(LIBS)
clean:
	/bin/rm  *.o
clear:
	/bin/rm  *.o
#
