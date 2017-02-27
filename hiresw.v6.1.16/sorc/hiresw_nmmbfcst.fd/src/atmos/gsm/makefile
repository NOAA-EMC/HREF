include ../../conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I../share -I../io -Ilibutil

ARCH=$(shell uname -s)
GOCARTINCS = -I../../chem/gocart/${ARCH}/include/Chem_Base \
             -I../../chem/gocart/${ARCH}/include/GMAO_mpeu \
             -I../../chem/gocart/${ARCH}/include/GEOSchem_GridComp

LIBRARY = libgfs.a

DIR_LIBUTIL = libutil
DIR_DYN     = dyn
DIR_PHY     = phys
GCINCS      = -I$(DIR_DYN) -I$(DIR_PHY)


#OPTS     = $(FFLAGS_GFS) $(R8) -O3  $(UTILINCS) $(GCINCS) -g -traceback
#OPTSGOC  = $(FFLAGS_GFS) $(R8) -O3  $(UTILINCS) $(GCINCS) $(GOCARTINCS) -g -traceback
OPTS     = $(FFLAGS_GFS) $(R8)  $(UTILINCS) $(GCINCS)
OPTSGOC  = $(FFLAGS_GFS) $(R8)  $(UTILINCS) $(GCINCS) $(GOCARTINCS)
FFLAG90  = $(OPTS) $(FREE)
FFLAG90GOC  = $(OPTSGOC) $(FREE)

OBJS   = module_GFS_CORE_SETUP.o module_GFS_INTERNAL_STATE.o \
	 module_GFS_INTEGRATE.o module_GFS_GRID_COMP.o

OBJSGOC   = module_GOCART_ROUTINES.o
OBJSGOC_STUB   = module_GOCART_ROUTINES_stub.o

MODULES_STUB  = module_GFS_GRID_COMP_stub.o

ifeq ($(GOCART_MODE),full)
	DEPEND_FILES = ${OBJS:.o=.F90} ${OBTSGOC:.o=.F90}
else
	DEPEND_FILES = ${OBJS:.o=.F90} ${OBTSGOC_STUB:.o=.F90}
endif

all default: depend
	cd $(DIR_LIBUTIL) && gmake gocart_stub && cd ..
	cd $(DIR_DYN) && gmake && cd ..
	cd $(DIR_PHY) && gmake && cd ..
	@gmake -f makefile $(LIBRARY)

$(LIBRARY): $(OBJSGOC_stub) $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJSGOC_stub) $(OBJS)

stub: $(MODULES_STUB)
	$(AR) $(ARFLAGS) $(LIBRARY) $(MODULES_STUB)

gocart_full: gfs_gocart_full_libs  $(OBJSGOC) $(OBJS)
	$(AR) $(ARFLAGS) $(LIBRARY)  $(OBJSGOC) $(OBJS)

gfs_gocart_full_libs:
	cd $(DIR_LIBUTIL) && gmake gocart_full && cd ..
	cd $(DIR_DYN) && gmake gocart_full && cd ..
	cd $(DIR_PHY) && gmake && cd ..

gocart_stub: gfs_gocart_stub_libs  $(OBJSGOC_STUB) $(OBJS)
	$(AR) $(ARFLAGS) $(LIBRARY)  $(OBJSGOC_STUB) $(OBJS)

gfs_gocart_stub_libs:
	cd $(DIR_LIBUTIL) && gmake gocart_stub && cd ..
	cd $(DIR_DYN) && gmake && cd ..
	cd $(DIR_PHY) && gmake && cd ..

$(OBJS): %.o: %.f90
	$(FC) $(FFLAG90) -c $*.f90

$(OBJSGOC_STUB): %.o: %.f90
	$(FC) $(FFLAG90) -c $*.f90

$(OBJSGOC): %.o: %.f90
	$(FC) $(FFLAG90GOC) -c $*.f90

$(MODULES_STUB): %.o: %.f90
	$(FC) $(FFLAGS_GFS) $(UTILINCS) -c $*.f90

clean:
	cd $(DIR_LIBUTIL) && gmake clean && cd ..
	cd $(DIR_DYN) && gmake clean && cd ..
	cd $(DIR_PHY) && gmake clean && cd ..
	$(RM) -f $(LIBRARY) *.f90 *.o *.mod *.lst lm map depend

MKDEPENDS = ../../../exe/mkDepends.pl

include ../../conf/make.rules

include depend
