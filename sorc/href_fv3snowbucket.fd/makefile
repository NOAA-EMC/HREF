SHELL=/bin/sh
#
SRCS=	fv3bucket.f GET_BITS.f

OBJS=	fv3bucket.o  GET_BITS.o

INC = ${G2_INC4}
LIBS =  ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} ${SP_LIB4} ${G2_LIB4}  \
                       ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}
DEBUG = -fpe0 -ftrapuv -g -traceback

FFLAGS =        -fp-model strict -g -traceback -O3 -assume noold_ldout_format -g -convert big_endian -I $(INC)

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	 clean href_fv3bucket

$(OBJS):       
	$(FC) -I ${INC} -c $(FFLAGS) $(@:.o=.f)

href_fv3bucket:
	$(FC) -I ${INC} $(FFLAGS) -o $(@) fv3bucket.f GET_BITS.f $(LIBS)
clean:
	/bin/rm   href_fv3bucket
copy:
	/bin/cp href_fv3bucket ../../exec/href_fv3snowbucket
