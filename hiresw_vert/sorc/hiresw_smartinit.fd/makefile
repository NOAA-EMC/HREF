SHELL=/bin/sh
#
#SRCS=    aset3d.f aset2d.f grddef.f constants.f asetdown.f rdgrib.f     \
GETGRIB.f90 MAIN.f90 BOUND.f90 GRIBIT.f90 GET_BITS.f90  MAKESTRING.f90 NDFDgrid.f90 VADJUST.f90

OBJS1= aset3d.o aset2d.o grddef.o constants.o asetdown.o rdgrib.o 
OBJS2= GETGRIB.o MAIN.o BOUND.o GRIBIT.o GET_BITS.o MAKESTRING.o NDFDgrid.o VADJUST.o

FC =   ifort -convert big_endian -g -traceback -assume noold_ldout_format	

LIBS = -L/usr/local/lib   \
	-L/nwprod/lib  -lip_4 -lw3nco_4 -lw3emc_4 -lbacio_4
NCEP_LIB_DIR = 

# FFLAGS = -O -fp-model strict -list 
# LDFLAGS =-FR -assume byterecl -check all -debug all -warn nounused  

FFLAGS = -O -list # -fp-model strict -list 
LDFLAGS =-FR -assume byterecl # -check all -debug all -warn nounused  

CMD =	../../exec/hiresw_smartinit

PROFLIB =	-lprof

all:		$(CMD)

$(OBJS1): %.o: %.f90
	$(FC) -c $*.f90

$(OBJS2): %.o: %.f90
	$(FC) -c $*.f90

$(CMD):		$(OBJS1) $(OBJS2)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS1) $(OBJS2) $(LIBS)

clean:
	-rm -f $(OBJS1) $(OBJS2) *.mod
