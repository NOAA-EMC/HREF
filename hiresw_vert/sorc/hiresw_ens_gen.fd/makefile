#!/bin/sh
#
#  This makefile is to compile ensemble production generator
#  Author: Binbin Zhou
#          Aug 1, 2005
#  OS: AIX (IBM-blue)
#


FC=     ncepxlf -O2  # -qflttrap=zerodivide:invalid:enable -qsigtrap -qcheck
#FC=     ncepxlf -O3 -qhot -bmaxdata:0x1200000000
ARCH     = auto
OPTS     =  -g -qnosave -qarch=$(ARCH) -qmaxmem=-1 -NS2000 # -qflttrap=zerodivide:invalid:enable -qsigtrap -qcheck
LIST     =
#FREE     = -qfree
FREE     =
TRAPS    =
PROFILE  =
DEBUG    = -g
MEM      =
MAP      =
W3LIBDIR = /nwprod/lib
ESSL     = -lessl
MASS     = -lmass


SRC=	util.f \
	char_process.f \
	print.f pack.f \
	thickness.f preciptype.f apcp.f snow.f wind.f getceiling.f \
	dtra.f getfog.f frzn.f vorticity.f getflight.f getcldtop.f sref_ens_gen.f

OBJ=${SRC:.f=.o}

LIBS= $(ESSL) $(MASS) $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG) -L/nwprod/lib -lip_4 -lw3_4 -lbacio_4 -lsp_4 -l  essl -l  mass

../../exec/hiresw_ens_gen: $(OBJ)
	$(FC)  -o $(@) sref_ens_gen.f $(OBJ) $(LIBS)
clean:
	/bin/rm sref_ens_gen *.o
clear:
	/bin/rm  *.o
#
