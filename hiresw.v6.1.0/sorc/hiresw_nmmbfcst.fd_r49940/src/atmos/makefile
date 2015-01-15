include ../conf/configure.nems

MAKEFILE = makefile

UTILINCS = -Ishare -Inmm -I$(gfs) -Ifim -Iphys -Igen

LIBRARY  = libatmos.a

MODULES  = module_ATM_INTERNAL_STATE.o module_ATM_GRID_COMP.o

DEPEND_FILES = ${MODULES:.o=.F90}


#all default: depend
#	@gmake $(J) -f $(MAKEFILE) $(LIBRARY)
all default: depend $(LIBRARY)

$(LIBRARY): $(MODULES)
	$(AR) $(ARFLAGS) $@ $?
	
$(MODULES): %.o: %.f90
	$(FC) $(FFLAGS) $(UTILINCS) -c $*.f90

clean:
	cd share        && gmake clean
	cd phys         && gmake clean
	cd nmm          && gmake clean
	cd io           && gmake clean
	cd $(gfs)       && gmake clean
	cd fim          && gmake clean
	cd gen          && gmake clean
	cd post         && gmake clean
	$(RM) -f $(LIBRARY) *.f90 *.o *.mod *.lst depend

MKDEPENDS = ../../exe/mkDepends.pl

include ../conf/make.rules

include depend
