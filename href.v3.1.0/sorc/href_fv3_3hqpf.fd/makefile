SHELL=/bin/sh
#
SRCS=	WRFBUCKETG2.f

OBJS=	WRFBUCKETG2.o 

# Tunable parameters
#
# LIBS		List of libraries
# CMD		Name of the executable
#
CMD = ./href_fv3_3hqpf


FFLAGS_DEBUG =        -g -traceback -O3 -ftrapuv  -check all  -fp-stack-check  -fstack-protector  -assume noold_ldout_format -g -convert big_endian -I $(INC)
FFLAGS =        -fp-model strict -g -traceback -O3 -assume noold_ldout_format -g -convert big_endian -I $(INC)


INC = ${G2_INC4}
LIBS =  ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} ${SP_LIB4} ${G2_LIB4}  \
	                ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}


# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	 $(CMD) delete

$(CMD):		$(OBJS)
	$(FC) -I${INC} -o $(@) $(OBJS) $(LIBS)

$(OBJS):       
	$(FC) -I${INC} -c $(FFLAGS) $(@:.o=.f)
clean:
	@rm  *.o
delete:
	@rm  *.o
copy:
	/bin/cp ${CMD}  ../../exec/
