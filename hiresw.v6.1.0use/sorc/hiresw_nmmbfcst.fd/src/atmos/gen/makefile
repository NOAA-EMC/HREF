include ../../conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I../share

LIBRARY = libgen.a

OPTS     = $(FFLAGS_GEN) $(R8) -O3  $(UTILINCS)
FFLAG90  = $(OPTS) $(FREE)

OBJS   = module_GEN_INTERNAL_STATE.o module_GEN_GRID_COMP.o

DEPEND_FILES = ${OBJS:.o=.F90}

MODULES_STUB  = module_GEN_INTERNAL_STATE.o module_GEN_GRID_COMP_stub.o 

all default: depend
	@gmake -f makefile $(LIBRARY)

$(LIBRARY): $(OBJS) 
	$(AR) $(ARFLAGS) $@ $?

stub: $(MODULES_STUB)
	$(AR) $(ARFLAGS) $(LIBRARY) $(MODULES_STUB)

$(OBJS): %.o: %.f90
	$(FC) $(FFLAG90) -c $*.f90

$(MODULES_STUB): %.o: %.f90
	$(FC) $(FFLAGS_GEN) $(UTILINCS) -c $*.f90

clean:
	$(RM) -f $(LIBRARY) *.f90 *.o *.mod *.lst lm map depend

MKDEPENDS = ../../../exe/mkDepends.pl

include ../../conf/make.rules
