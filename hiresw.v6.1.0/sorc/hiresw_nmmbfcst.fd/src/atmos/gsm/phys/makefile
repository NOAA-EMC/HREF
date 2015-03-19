include ../../../conf/configure.nems

MAKEFILE = makefile

UTILINCS = -I../../share -I../libutil -I../../phys $(GOCARTINCS)
ARCH=$(shell uname -s)
GOCARTINCS = -I../../../chem/gocart/${ARCH}/include/Chem_Base \
             -I../../../chem/gocart/${ARCH}/include/GMAO_mpeu \
             -I../../../chem/gocart/${ARCH}/include/GEOSchem_GridComp

LIBRARY  = gfs_physics.a

OPTS     = $(FFLAGS_GFS) $(R8) $(UTILINCS)

FFLAG90  = $(OPTS) $(FREE)
FFLAGS1  = $(OPTS) $(FIXED)

OBJ_MOD	 =                           \
	  resol_def.o               \
	  layout1.o                 \
	  vert_def.o                \
	  gg_def.o                  \
	  namelist_physics_def.o    \
	  coordinate_def.o          \
	  reduce_lons_grid_module.o \
	  mpi_def.o		    \
	  d3d_def.o        	    \
	  sfcio_module.o            \
	  mod_state.o               \
	  nstio_module.o

OBJS0	 =                                      \
	  gfs_physics_err_msg_mod.o            \
	  gfs_physics_sfc_flx_mod.o	       \
	  gfs_physics_nst_var_mod.o            \
	  gfs_physics_sfc_flx_set_mod.o	       \
	  gfs_physics_gridgr_mod.o             \
	  gfs_physics_g2d_mod.o                \
	  gfs_physics_g3d_mod.o      	       \
	  gfs_physics_aoi_var_mod.o            \
	  gfs_physics_namelist_mod.o           \
	  gfs_physics_start_time_get_mod.o     \
	  gfs_physics_internal_state_mod.o     \
	  gfs_physics_grid_create_mod.o        \
	  gfs_physics_output.o                 \
	  gfs_physics_getcf_mod.o              \
 	  gfs_physics_initialize_mod.o         \
	  gfs_physics_run_mod.o                \
	  gfs_physics_finalize_mod.o           \
	  gfs_physics_add_get_state_ESMFField_mod.o      \
	  gfs_phy_states_mod.o                 \
	  gfs_physics_grid_comp_mod.o

OBJS     =                              \
          glats_physics.o               \
          gcycle.o                      \
          do_physics_one_step.o         \
          get_lats_node_shuff_r_fix.o   \
          setlats.o                     \
	  setlats_r_slg.o               \
          lats_shuff.o                  \
          getcon_physics.o              \
          lon_lat_para.o                \
          compns_physics.o              \
          info.o                        \
          coundummy.o                   \
          sortrx.o                      \
          mpi_quit.o                    \
          dfi_fixwr.o                   \
          hpmdummy.o                    \
	  gloopr.o                      \
	  gloopb.o                      \
	  gbphys_adv_hyb_gc.o           \
	  gbphys_adv_hyb_gc_h.o         \
	  fix_fields.o                  \
	  read_fix.o                    \
	  GFS_simple_scatter.o          \
	  wrtout_physics.o              \
	  fld_collect.o                 \
	  fld_collect_nst.o             \
	  gribit_gsm.o         		\
	  wrt3d_hyb.o          		\
	  wrt3d.o              		\
	  aoicpl_prep.o                   \
	  para_fixio_w.o                \
	  para_nst_w.o

#
# *****************************************************************
#

DEPEND_FILES = ${OBJS:.o=.f} ${OBJS0:.o=.f} ${OBJ_MOD:.o=.f}

all default: depend
	@gmake -f $(MAKEFILE) $(LIBRARY)

$(LIBRARY):  $(OBJ_MOD) $(OBJS0) $(OBJS)
	$(AR) $(ARFLAGS) $@ $?

clean:
	$(RM) -f $(LIBRARY) *.o *.mod *.lst *.f90 lm map depend

.SUFFIXES: .f .f90 .o

.f.f90:
	$(CPP) $< > $*.f90

${OBJ_MOD}: %.o: %.f
	$(CPP) $< > $*.f90
	$(FC) $(FFLAGS1) -c $*.f90

${OBJS0}: %.o: %.f
	${CPP} $< >$*.f90
	$(FC) $(FFLAG90) ${UTILINCS} -c $*.f90

${OBJS}: %.o: %.f
	$(CPP) $< > $*.f90
	$(FC) $(FFLAGS1) -c $*.f90

#
# *****************************************************************
#

gbphys_adv_hyb_gc.o:	gbphys_adv_hyb_gc.f90
		$(FC) $(FFLAGS1) -c gbphys_adv_hyb_gc.f90

gbphys_adv_hyb_gc_h.o:	gbphys_adv_hyb_gc_h.f90
		$(FC) $(FFLAGS1) -c gbphys_adv_hyb_gc_h.f90

mpi_def.o:	mpi_def.f
	        ${CPP} mpi_def.f > mpi_def.f90
		$(FC) $(FFLAGS1) -c mpi_def.f90

sfcio_module.o:	sfcio_module.f90
	        ${CPP} sfcio_module.f > sfcio_module.f90
		$(FC) $(FFLAGS) -c sfcio_module.f90

gloopr.o:	gloopr.f90
		$(FC) $(FFLAGS1) -c gloopr.f90

gloopb.o:	gloopb.f90
		$(FC) $(FFLAGS1) -c gloopb.f90

gcycle.o:	gcycle.f90
		$(FC) $(FFLAGS1) -c gcycle.f90

getaer.o:	getaer.f90
		$(FC) $(FFLAGS1) -c getaer.f90

gribit_gsm.o:	gribit_gsm.f90
		$(FC) $(FFLAGS1) -c gribit_gsm.f90

wrt3d_hyb.o:    wrt3d_hyb.f
		$(FC) $(FFLAGS1) -c wrt3d_hyb.f

wrt3d.o:        wrt3d.f
		$(FC) $(FFLAGS1) -c wrt3d.f

fld_collect.o:  fld_collect.f90
	        $(FC) $(FFLAGS1) -c fld_collect.f90

fld_collect_nst.o:  fld_collect_nst.f90
	        $(FC) $(FFLAGS1) -c fld_collect_nst.f90

read_fix.o: read_fix.f90
	        $(FC) $(FFLAGS1) -c read_fix.f90

para_fixio_w.o:  para_fixio_w.f90
		$(FC) $(FFLAGS1) -c para_fixio_w.f90

para_nst_w.o:  para_nst_w.f90
		$(FC) $(FFLAGS1) -c para_nst_w.f90

para_nstio_w.o:  para_nstio_w.f90
		$(FC) $(FFLAGS1) -c para_nstio_w.f90

wrtout_physics.o: wrtout_physics.f90
		$(FC) $(FFLAGS1) -c wrtout_physics.f90

aoicpl_prep.o:	  aoicpl_prep.f90
		$(FC) $(FFLAGS1) -c aoicpl_prep.f90

nstio_module.o:	  nstio_module.f
	        ${CPP} nstio_module.f > nstio_module.f90
		$(FC) $(FFLAG90) -c nstio_module.f90

gfs_physics_err_msg_mod.o:      gfs_physics_err_msg_mod.f
	        ${CPP} gfs_physics_err_msg_mod.f > gfs_physics_err_msg_mod.f90
		$(FC) $(FFLAG90) -c gfs_physics_err_msg_mod.f90

MKDEPENDS = ../../../../exe/mkDepends.pl

include ../../../conf/make.rules

include depend
