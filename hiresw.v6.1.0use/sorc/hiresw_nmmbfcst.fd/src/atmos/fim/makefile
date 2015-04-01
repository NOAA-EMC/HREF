SHELL = /bin/sh

include ../../conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I../share

#TODO:  DRY $(FIMSRC) and $(SMS), they are redundant with FIMsrc/* and ../../makefile
FIMSRC   = FIMsrc_devccs
SMS      = /gpfs/t3/global/save/wx20tbh/SMS/SMS_r237
FIMINCS  = -I$(SMS)/include -I$(FIMSRC)/prep/sigio -I$(FIMSRC)/prep/sfcio -I$(FIMSRC)/post/pop -I$(FIMSRC)/utils -I$(FIMSRC)/icosio -I$(FIMSRC)/fim/Horizontal -I$(FIMSRC)/fim/column

LIBRARY  = libfimtop.a

MODULES  = fim_internal_state.o fim_grid_comp.o module_FIM_INTEGRATE.o module_DYNAMICS_GRID_COMP.o module_DYN_PHY_CPL_COMP.o module_PHYSICS_GRID_COMP.o
MODULES_STUB  = fim_grid_comp_stub.o

DEPEND_FILES = ${MODULES:.o=.F90}

# TODO:  consider passing configuration settings from ../../conf/configure.nems 
# TODO:  down into makefim via "env x=y a=b makefim"
# TODO:  at present we use settings specified in FIMsrc/Makefile and
# TODO:  FIMsrc/macros.make.devccs
# TODO:  replace with explicit depenence on built libraries in $(FIMSRC)/lib*.a
all default: depend buildfimsrclibs
	@gmake -f $(MAKEFILE) $(LIBRARY)

# always execute makefim, it is cheap if a re-build is not needed
.PHONY: buildfimsrclibs
buildfimsrclibs:
	(cd FIMsrc && ./makefim devccs) || (echo "makefim failure" && exit 1)

$(LIBRARY): $(MODULES)
	$(AR) $(ARFLAGS) $@ $(MODULES)

stub: $(MODULES_STUB)
	$(AR) $(ARFLAGS) $(LIBRARY) $(MODULES_STUB)
	
$(MODULES): %.o: %.f90
	$(FC) $(FFLAGS_FIM) $(UTILINCS) $(FIMINCS) -c $*.f90

$(MODULES_STUB): %.o: %.f90
	$(FC) $(FFLAGS_FIM) $(UTILINCS) -c $*.f90

clean:
	$(RM) -f $(LIBRARY) *.o *.mod *.f90 *.lst lm map depend
	(cd FIMsrc && ./makefim clean)

MKDEPENDS = ../../../exe/mkDepends.pl

include ../../conf/make.rules

include depend
