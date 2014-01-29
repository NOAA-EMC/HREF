SHELL=/bin/sh
#
###SRCS=SMARTPRECIP.f90
OBJS=SMARTPRECIP.o

FC =   ifort -convert big_endian -g -traceback	 -assume noold_ldout_format

LIBS = -L/usr/local/lib   \
	-L/nwprod/lib  -lip_4 -lw3nco_4 -lw3emc_4 -lbacio_4
NCEP_LIB_DIR = 

FFLAGS = -O -fp-model strict -list
LDFLAGS =-FR -assume byterecl -check all -debug all -warn nounused 

CMD =	../../exec/hiresw_smartprecip    
PROFLIB =	-lprof

all:		$(CMD)

$(OBJS): %.o: %.f90
	$(FC) -c $*.f90


$(CMD):		$(OBJS) 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)
