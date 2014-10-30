SHELL=/bin/sh
#
###SRCS=SMARTPRECIP.f90
OBJS=SMARTPRECIP.o

FC =   ifort -convert big_endian -g -traceback	 -assume noold_ldout_format

LIBS = -L/usr/local/lib   \
	-L/nwprod/lib  -lip_4 -lw3nco_4 -lw3emc_4 -lbacio_4
NCEP_LIB_DIR = 
LIBDEFG2=-L/nwprod/lib -lg2tmpl -lg2_4 -ljasper -lpng -lz
INCG2=-I/nwprod/lib/incmod/g2_4 -I/nwprod/lib/incmod/g2tmpl


FFLAGS = -O -fp-model strict -list
LDFLAGS =-FR -assume byterecl

CMD =	../../exec/hiresw_smartprecipg2    
PROFLIB =	-lprof

all:		$(CMD)

$(OBJS): %.o: %.f90
	$(FC) $(INCG2) -c $*.f90


$(CMD):		$(OBJS) 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBDEFG2) $(LIBS)

clean:
	-rm -f $(OBJS)
