SHELL=/bin/sh
#
OBJS1= aset3d.o aset2d.o grddef.o constants.o asetdown.o rdgrib.o GETGRIB.o
OBJS2= MAIN.o BOUND.o GRIBIT.o GET_BITS.o MAKESTRING.o NDFDgrid.o VADJUST.o

FCA =   ${FC} -convert big_endian -assume noold_ldout_format -g -traceback # -ftrapuv  -check all  -fp-stack-check  -fstack-protector	
FCB =   ${FC} -O3 -convert big_endian -assume noold_ldout_format -g -traceback # -ftrapuv  -check all  -fp-stack-check  -fstack-protector	

LIBS = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}
INC= -I${G2_INC4} -I${G2TMPL_INC}


# FFLAGS = -O -fp-model strict -list 
# LDFLAGS =-FR -assume byterecl -check all -debug all -warn nounused  

FFLAGS = -O3 
LDFLAGS =   -FR  -assume byterecl # -check all -debug all -warn nounused  

CMD =	../../exec/hiresw_smartinit_g2

all:		$(CMD)

$(OBJS1): %.o: %.f90
	$(FCA) $(INC) -c $*.f90

$(OBJS2): %.o: %.f90
	$(FCB) $(INC) -c $*.f90

$(CMD):		$(OBJS1) $(OBJS2) 
	$(FCB) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS1) $(OBJS2) $(INC) $(LIBS)

clean:
	-rm -f $(OBJS1) $(OBJS2) *.mod
