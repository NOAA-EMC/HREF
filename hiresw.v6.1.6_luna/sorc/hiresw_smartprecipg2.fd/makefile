SHELL=/bin/sh
#
OBJS=SMARTPRECIP.o

# FC =   ${FC} -convert big_endian -g -traceback -assume noold_ldout_format

LIBS = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} \
       ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}

INCG2=-I${G2_INC4} -I${G2TMPL_INC}

FCOPTS = -convert big_endian -xAVX -g -traceback -assume noold_ldout_format
FFLAGS = -O -fp-model strict -list -xAVX
LDFLAGS =-FR -assume byterecl

CMD =	../../exec/hiresw_smartprecipg2    
PROFLIB =	-lprof

all:		$(CMD)

$(OBJS): %.o: %.f90
	$(FC) $(FCOPTS) $(INCG2) -c $*.f90

$(CMD):		$(OBJS) 
	$(FC) $(FCOPTS) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)
