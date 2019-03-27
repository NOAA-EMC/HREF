SHELL=/bin/sh
#
SRCS=	fv3bucket.f

OBJS=	fv3bucket.o 

LIBS =   ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} 
INC =  -I${G2_INC4} -I${G2TMPL_INC}
DEBUG = -fpe0 -ftrapuv -g -traceback

FFLAGS = -O   

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	 clean href_fv3bucket copy

$(OBJS):       
	$(FC) ${INC} -c $(FFLAGS) $(@:.o=.f)

href_fv3bucket:
	$(FC) ${INC} $(FFLAGS) ${DEBUG} -o $(@) fv3bucket.f $(LIBS)
clean:
	/bin/rm   href_fv3bucket
copy:
	/bin/cp href_fv3bucket ../../exec/href_fv3bucket

