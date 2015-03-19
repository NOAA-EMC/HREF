include ../../conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I../share -I../phys

LIBRARY  = libnmm.a

MODULES  = \
           module_BGRID_INTERP.o \
           module_CLOCKTIMES.o \
           module_CONTROL.o \
           module_CONVECTION.o \
           module_DERIVED_TYPES.o \
           module_DIAGNOSE.o \
           module_DIGITAL_FILTER_NMM.o \
           module_DM_PARALLEL.o \
           module_DOMAIN_GRID_COMP.o \
           module_DOMAIN_INTERNAL_STATE.o \
           module_DYNAMICS_ROUTINES.o \
           module_EXCHANGE.o \
           module_FLTBNDS.o \
           module_GET_CONFIG.o \
           module_GET_CONFIG_WRITE.o \
           module_GWD.o \
           module_H_TO_V.o \
           module_INIT_READ_BIN.o \
           module_INIT_READ_NEMSIO.o \
           module_MICROPHYSICS.o \
           module_MY_DOMAIN_SPECS.o \
           module_NESTING.o \
           module_NMM_GRID_COMP.o \
           module_NMM_INTEGRATE.o \
           module_NMM_INTERNAL_STATE.o \
           module_OUTPUT.o \
           module_PARENT_CHILD_CPL_COMP.o \
           module_PRECIP_ADJUST.o \
           module_RADIATION.o \
           module_SOLVER_GRID_COMP.o \
           module_SOLVER_INTERNAL_STATE.o \
           module_TIMESERIES.o \
           module_TURBULENCE.o \
           module_VARS.o \
           module_VARS_STATE.o \
           module_WRITE_GRID_COMP.o \
           module_WRITE_INTERNAL_STATE.o \
           module_WRITE_ROUTINES.o

MODULE_GFSP   = \
                n_namelist_physics_def.o \
                n_layout1.o \
                n_resol_def.o \
                n_compns_physics.o \
                n_mpi_def.o \
                n_module_gfs_mpi_def.o \
                n_mpi_quit.o

MODULES_STUB  = module_NMM_GRID_COMP_stub.o

DEPEND_FILES = ${MODULES:.o=.F90} ${MODULE_GFSP:.o=.f}

#all default: depend
#	@gmake -f $(MAKEFILE) $(LIBRARY)

all default: depend $(LIBRARY)

$(LIBRARY): $(MODULES) ${MODULE_GFSP}
	$(AR) $(ARFLAGS) $@ $(MODULES) ${MODULE_GFSP}

stub: $(MODULES_STUB)
	$(AR) $(ARFLAGS) $(LIBRARY) $(MODULES_STUB)
	
$(MODULES): %.o: %.f90
	$(FC) $(FFLAGS_NMM) $(UTILINCS) -c $*.f90

$(MODULE_GFSP): %.o: %.f
	$(F77) $(OPTS_GFS) $(FIXED) $(TRAPS) $(UTILINCS) -c $*.f

$(MODULES_STUB): %.o: %.f90
	$(FC) $(FFLAGS_NMM) $(UTILINCS) -c $*.f90

clean:
	$(RM) -f $(LIBRARY) *.f90 *.o *.mod *.lst lm map depend

MKDEPENDS = ../../../exe/mkDepends.pl

include ../../conf/make.rules

include depend
