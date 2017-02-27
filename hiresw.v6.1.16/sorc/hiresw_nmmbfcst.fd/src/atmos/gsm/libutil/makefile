include ../../../conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I../../share
ARCH=$(shell uname -s)
GOCARTINCS = -I../../../chem/gocart/${ARCH}/include/Chem_Base \
             -I../../../chem/gocart/${ARCH}/include/GMAO_mpeu \
             -I../../../chem/gocart/${ARCH}/include/GEOSchem_GridComp

LIBRARY  = gfs_libutil.a

OPTS     = -O3

FFLAG_GFS_UTIL90 = $(FFLAGS_GFS) $(OPTS) $(R8) $(FREE)  $(UTILINCS)
FFLAG_GFS_UTIL90_GOC = $(FFLAGS_GFS) $(OPTS) $(R8) $(FREE)  $(UTILINCS) $(GOCARTINCS)

OBJS01  = atmos_phy_chem_cpl_comp_mod.o atmos_chem_phy_cpl_comp_mod.o
OBJS01A  = atmos_phy_chem_cpl_comp_mod_stub.o atmos_chem_phy_cpl_comp_mod_stub.o

OBJS02  = \
	module_gfs_machine.o          \
	module_include_gfs.o          \
	module_gfs_mpi_def.o          \
	module_gfs_physcons.o         \
	module_gfs_funcphys.o         \
	module_gfs_tropp.o            \
	module_DM_PARALLEL_GFS.o      \
	module_digital_filter_gfs.o   \
	module_export_import_data.o   \
	gfs_dyn_phy_cpl_comp_mod.o    


ifeq ($(GOCART_MODE),full)
	DEPEND_FILES = ${OBJS01:.o=.f90} ${OBJS02:.o=.f90}
else
	DEPEND_FILES = ${OBJS01A:.o=.f90} ${OBJS02:.o=.f90}
endif

.SUFFIXES: .F90 .f90 .o
.F90.f90:
	$(CPP) $(CPPFLAGS) $< > $*.f90

all default: depend
	@gmake -f $(MAKEFILE) $(LIBRARY)

$(LIBRARY): $(OBJS01) $(OBJS02)
	$(AR) $(ARFLAGS) $@ $(OBJS01) $(OBJS02)

gocart_stub: $(OBJS01A) $(OBJS02)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS01A) $(OBJS02)

gocart_full: $(OBJS01) $(OBJS02)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS01) $(OBJS02)

$(OBJS01): %.o: %.f90
	$(FC) $(FFLAG_GFS_UTIL90_GOC) -c $*.f90

$(OBJS01A): %.o: %.f90
	$(FC) $(FFLAG_GFS_UTIL90) -c $*.f90

$(OBJS02): %.o: %.f90
	$(FC) $(FFLAG_GFS_UTIL90) -c $*.f90

clean:
	$(RM) -f $(LIBRARY) *.f90 *.o *.mod *.lst lm map depend

MKDEPENDS = ../../../../exe/mkDepends.pl

include ../../../conf/make.rules

include depend
