include ../../conf/configure.nems

MAKEFILE = makefile

LIBRARY  = libshare.a

MODULES_F90  = module_ERR_MSG.o \
               module_INCLUDE.o \
               module_CONSTANTS.o \
               module_CPLFIELDS.o \
	       check_esmf_pet.o \
	       ffsync.o 

MODULES_IDEA = idea_composition.o

MODULES_ALL = $(MODULES_F90) $(MODULES_IDEA)

DEPEND_FILES = ${MODULES_F90:.o=.F90} ${MODULES_IDEA:.o=.f}


#all default: depend
#	@gmake -f $(MAKEFILE) $(LIBRARY)

all default: depend $(LIBRARY)

$(LIBRARY): $(MODULES_ALL)
	$(AR) $(ARFLAGS) $@ $(MODULES_ALL)

$(MODULES_F90): %.o: %.f90
	$(FC) $(FFLAGS) $(UTILINCS) -c $*.f90

$(MODULES_IDEA): %.o: %.f
	$(FC) $(FFLAGS_GFSF) $(UTILINCS) $(R8) -c $*.f


clean:
	$(RM) -f $(LIBRARY) *.f90 *.o *.mod *.lst *.a lm map depend

MKDEPENDS = ../../../exe/mkDepends.pl

include ../../conf/make.rules

include depend
