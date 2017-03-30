SHELL=/bin/sh
#
SRCS=	WRFBUCKETG2.f

OBJS=	WRFBUCKETG2.o 

# Tunable parameters
#
# LIBS		List of libraries
# CMD		Name of the executable
#
CMD = ../../exec/hiresw_bucket

LIBS =   ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} 
INC =  -I${G2_INC4} -I${G2TMPL_INC}

FFLAGS = -O   

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	 $(CMD) delete

$(CMD):		$(OBJS)
	$(COMPILER) ${INC} -o $(@) $(OBJS) $(LIBS)

$(OBJS):       
	$(COMPILER) ${INC} -c $(FFLAGS) $(@:.o=.f)
delete:
	@rm  *.o
