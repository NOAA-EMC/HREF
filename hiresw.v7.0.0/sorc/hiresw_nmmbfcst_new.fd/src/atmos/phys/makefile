include ../../conf/configure.nems

# consider setting this in configure.nems* (JM 20140616)
CHNK_RRTM=8

MAKEFILE = makefile

UTILINCS = -I../share
ARCH=$(shell uname -s)
GOCARTINCS = -I../../chem/gocart/${ARCH}/include/Chem_Base

LIBRARY  = libphys.a

MODULES_NMM      = module_BL_GFSPBL.o \
                   module_BL_GFSPBLHUR.o \
                   module_BL_MYJPBL.o \
                   module_CU_BMJ.o \
                   module_CU_SAS.o \
                   module_CU_SASHUR.o \
                   module_LS_LISS.o \
                   module_LS_NOAHLSM.o \
                   module_MP_ETANEW.o \
                   module_MP_FER_HIRES.o \
                   module_mp_thompson.o \
                   module_mp_radar.o \
                   module_MP_GFS.o \
                   module_MP_WSM6.o \
                   module_RA_GFDL.o \
                   module_RA_RRTM.o \
                   module_SF_JSFC.o \
                   module_SF_GFDL.o \
                   module_SF_URBAN.o

MODULES_NMM_RRTM = grrad_nmmb.o                   \
                   radiation_astronomy_nmmb.o     \
                   radiation_aerosols_nmmb.o      \
                   radiation_gases_nmmb.o         \
                   radiation_clouds_nmmb.o        \
                   radiation_surface_nmmb.o       \
                   rad_initialize_nmmb.o          \
	           radlw_param_nmmb.o             \
 	           radlw_datatb_nmmb.o            \
	           radlw_main_nmmb.o              \
	           radsw_param_nmmb.o             \
	           radsw_datatb_nmmb.o            \
	           radsw_main_nmmb.o              \
                   radiation_astronomy_gfs.o \
                   radiation_aerosols_gfs.o  \
                   radiation_gases_gfs.o     \
                   radiation_clouds_gfs.o    \
                   radiation_surface_gfs.o   \
                   precpd_nmmb.o

#                   grrad_gfs.o               \

MODULE_FGFS =                                \
	           num_parthds.o

MODULES_GFS =                                \
	           machine.o                 \
	           wam_f107_kp_mod.o         \
		   physparam.o               \
	           physcons.o                \
	           funcphys.o                \
		   cs_conv.o                 \
	           module_nst_parameters.o   \
	           module_nst_water_prop.o   \
	           module_nst_model.o        \
		   gcm_shoc.o                \
	           calpreciptype.o

MODULES_GFSF=                                \
	           iounitdef.o               \
	           ozne_def.o                \
	           progtm_module.o           \
	           date_def.o                \
	           rascnvv2.o                \
	           namelist_soilveg.o        \
	           module_bfmicrophysics.o   \
	           mersenne_twister.o        \
	           tracer_const_h.o          \
	           gfs_phy_tracer_config.o   \
                   rad_initialize.o          \
	           radlw_param.o             \
 	           radlw_datatb.o            \
	           radlw_main.o              \
	           radsw_param.o             \
	           radsw_datatb.o            \
	           radsw_main.o              \
	           radiation_astronomy.o     \
	           radiation_aerosols.o      \
	           radiation_gases.o         \
	           radiation_clouds.o        \
	           radiation_surface.o       \
	           grrad.o                   \
	           ozphys.o                  \
	           ozinterp.o                \
	           sfcsub.o                  \
	           gbphys.o                  \
	           dcyc2.o                   \
	           dcyc2.pre.rad.o           \
	           set_soilveg.o             \
	           sfc_drv.o                 \
	           sfc_land.o                \
	           progt2.o                  \
	           sfc_sice.o                \
	           sfc_cice.o                \
	           sfc_ocean.o               \
	           sfc_nst.o                 \
	           sfc_diff.o                \
	           sfc_diag.o                \
	           sflx.o                    \
		   moninshoc.o               \
	           moninp.o                  \
	           moninp1.o                 \
	           moninq.o                  \
	           moninedmf.o               \
	           mfpbl.o                   \
	           gwdps.o                   \
	           gwdc.o                    \
	           rayleigh_damp.o           \
	           rayleigh_damp_mesopause.o \
	           sascnv.o                  \
	           sascnvn.o                 \
	           mfdeepcnv.o               \
	           cnvc90.o                  \
		   tridi2t3.o                \
	           shalcv.o                  \
	           shalcv_opr.o              \
	           shalcnv.o                 \
	           mfshalcnv.o               \
	           lrgsclr.o                 \
	           gscond.o                  \
	           gscondp.o                 \
	           precpd.o                  \
	           precpdp.o                 \
		   precpd_shoc.o             \
	           mstadb.o                  \
	           mstadbtn.o                \
	           mstcnv.o                  \
	           get_prs.o                 \
	           gsmddrive.o

MODULES_IDEA = co2hc.o              \
               idea_solar_heating.o \
               idea_tracer.o        \
               h2oc.o               \
               h2ohdc.o             \
               efield.o             \
               ideaca.o             \
               idea_co2.o           \
               idea_h2o.o           \
               idea_ion.o           \
               idea_o2_o3.o         \
               idea_dissipation.o   \
               idea_phys.o

MODULES_TRAC=      gocart_tracer_config.o

MODULES_TRAC_stub= gocart_tracer_config_stub.o

MODULES_ALL =	$(MODULES_NMM) $(MODULES_GFS) $(MODULES_IDEA) \
		$(MODULES_TRAC_stub) $(MODULES_GFSF) $(MODULES_NMM_RRTM) $(MODULE_FGFS)
MODULES_GOCART =	$(MODULES_NMM) $(MODULES_GFS) $(MODULES_IDEA) \
			$(MODULES_GFSF) $(MODULES_TRAC) $(MODULES_NMM_RRTM) $(MODULE_FGFS)

#			$(MODULES_TRAC) $(MODULES_GFSF) $(MODULES_NMM_RRTM) $(MODULE_FGFS)

ifeq ($(GOCART_MODE),full)
	DEPEND_FILES =	${MODULES_NMM:.o=.F90} ${MODULES_GFS:.o=.f} ${MODULES_TRAC:.o=.f} \
			${MODULES_NMM_RRTM:.o=.f} ${MODULES_GFSF:.o=.f} ${MODULES_IDEA:.o=.f} ${MODULE_FGFS:.o=.F}
else
	DEPEND_FILES =	${MODULES_NMM:.o=.F90} ${MODULES_GFS:.o=.f} ${MODULES_TRAC_stub:.o=.f} \
			${MODULES_NMM_RRTM:.o=.f} ${MODULES_GFSF:.o=.f} ${MODULES_IDEA:.o=.f} ${MODULE_FGFS:.o=.F}
endif

all default: depend
	gmake -f $(MAKEFILE) $(LIBRARY)

$(LIBRARY): $(MODULES_ALL)
	$(AR) $(ARFLAGS) $@ $(MODULES_ALL)

gocart_stub: depend $(MODULES_ALL)
	$(AR) $(ARFLAGS) $(LIBRARY) $(MODULES_ALL)

gocart_full: $(MODULES_GOCART)
	$(AR) $(ARFLAGS) $(LIBRARY) $(MODULES_GOCART)

$(MODULES_NMM): %.o: %.f90
	$(FC) $(FFLAGS_NMM) $(UTILINCS) -c $*.f90

$(MODULES_NMM_RRTM): %.o: %.f
	$(FC) $(FREE) $(FFLAGS_NMM) $(UTILINCS) $(FPP) $(CPPFLAGS) $(R8) -c $*.f

$(MODULE_FGFS): %.o: %.F
	$(FC) $(FFLAGS_GFS) $(UTILINCS) $(R8) $(SUFFIX_FLAG) -c $*.F

$(MODULES_GFS): %.o: %.f
	$(FC) $(FFLAGS_GFS) $(UTILINCS) $(FPP) $(CPPFLAGS) $(R8) -c $*.f

$(MODULES_IDEA): %.o: %.f
	$(FC) $(FFLAGS_GFSF) $(UTILINCS) $(R8) -c $*.f

$(MODULES_TRAC): %.o: %.f
	$(FC) $(FFLAGS_GFSF) $(UTILINCS) $(GOCARTINCS) $(R8) -c $*.f

$(MODULES_TRAC_stub): %.o: %.f
	$(FC) $(FFLAGS_GFSF) $(UTILINCS) $(R8) -c $*.f

$(MODULES_GFSF): %.o: %.f
	$(FC) $(FFLAGS_GFSF) $(UTILINCS) $(FPP) $(CPPFLAGS) $(R8) -c $*.f

clean:
	$(RM) -f $(LIBRARY) *.f90 *.o *.mod *.lst lm map depend

MKDEPENDS = ../../../exe/mkDepends.pl

include ../../conf/make.rules

include depend
