#include "./ESMFVersionDefine.h"

!-----------------------------------------------------------------------
!
      MODULE module_EARTH_GRID_COMP
!
!-----------------------------------------------------------------------
!***  This module contains codes directly related to the EARTH component.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!  2010-03-24  Black - Created Earth component module.
!  2010-04     Yang  - Added Ensemble capability.
!  2011-05-11  Theurich & Yang - Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!  2011-10-04  Yang - Modified for using the ESMF 5.2.0r library.
!  2012-02     Tripp - Added ESMF superstructure to support an OCN model
!  2013-06     Theurich - Reworked OCN dependency to be NUOPC based
!  2013-07     Theurich - Macro based ESMF error handling
!-----------------------------------------------------------------------
!
!***  The EARTH component lies in the hierarchy seen here:
!
!          Main program
!               |
!               |
!          NEMS component
!               |     |________________________.
!               |                              |
!          EARTH component        Ensemble Coupler component
!              /|\
!             / | \
!          ATM/OCN/ICE components
!          |    |
!          |    |
!          |    |
!          |    (MOM5, HYCOM, etc.)
!          |
!          CORE component (GSM, NMM, FIM, GEN, etc.)
!
!-----------------------------------------------------------------------
!
      USE esmf_mod

#ifdef WITH_NUOPC
      use NUOPC
      use module_EARTH_GENERIC_COMP, &
        driver_routine_SS             => routine_SetServices, &
        driver_type_IS                => type_InternalState, &
        driver_label_IS               => label_InternalState, &
        driver_label_SetModelPetLists => label_SetModelPetLists, &
        driver_label_SetModelServices => label_SetModelServices
      use NUOPC_Connector, only: conSS => routine_SetServices
  ! - Handle build time OCN options:
#ifdef FRONT_OCN_DUMMY
      use FRONT_OCN_DUMMY,  only: OCN_DUMMY_SS  => SetServices
#endif
#ifdef FRONT_HYCOM
      use FRONT_HYCOM,      only: OCN_HYCOM_SS  => SetServices
#endif
#ifdef FRONT_MOM5
      use FRONT_MOM5,       only: OCN_MOM5_SS   => SetServices
#endif
  ! - Handle build time ICE options:
#ifdef FRONT_ICE_DUMMY
      use FRONT_ICE_DUMMY,  only: ICE_DUMMY_SS  => SetServices
#endif
#ifdef FRONT_CICE
      use FRONT_CICE,  only: ICE_CICE_SS  => SetServices
#endif
  ! - Mediator
      use module_MEDIATOR,  only: MED_SS        => SetServices
#endif

      USE module_EARTH_INTERNAL_STATE,ONLY: EARTH_INTERNAL_STATE        &
                                           ,WRAP_EARTH_INTERNAL_STATE
!
      USE module_ATM_GRID_COMP
!
      USE module_ERR_MSG,ONLY: ERR_MSG,MESSAGE_CHECK
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: EARTH_REGISTER
!
!-----------------------------------------------------------------------
!
      TYPE(EARTH_INTERNAL_STATE),POINTER,SAVE :: EARTH_INT_STATE           !<-- Internal state of the EARTH component
      TYPE(WRAP_EARTH_INTERNAL_STATE)   ,SAVE :: WRAP                      !<-- F90 pointer to the EARTH internal state
!
!-----------------------------------------------------------------------
!
      CONTAINS

!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE EARTH_REGISTER(EARTH_GRID_COMP,RC_REG)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: EARTH_GRID_COMP                               !<-- The EARTH component
!
      INTEGER,INTENT(OUT) :: RC_REG                                        !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC

      
#ifdef WITH_NUOPC
      type(ESMF_Config)             :: config
#endif
      
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_REG = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
#ifdef WITH_NUOPC

      ! EARTH_GENERIC registers the generic methods

      call NUOPC_CompDerive(EARTH_GRID_COMP, driver_routine_SS, rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)

      ! attach specializing method(s)

      call NUOPC_CompSpecialize(EARTH_GRID_COMP, &
        specLabel=driver_label_SetModelPetLists, specRoutine=SetModelPetLists, &
        rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      
      call NUOPC_CompSpecialize(EARTH_GRID_COMP, &
        specLabel=driver_label_SetModelServices, specRoutine=SetModelServices, &
        rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      
      ! create, open, and set the config
      
      config = ESMF_ConfigCreate(rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      call ESMF_ConfigLoadFile(config, "nems.configure", rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      call ESMF_GridCompSet(EARTH_GRID_COMP, config=config, rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      
      ! Added the following Field Dictionary block to the EARTH component level
      ! in order to prevent different dictionary definitions in the lower
      ! components. Doing this here isn't without problems because it
      ! potentially makes the components (ATM & OCN) depend on this environment,
      ! which lowers their transferability to other coupled systems. However,
      ! extending the Field Dictionary is a temporary solution anyway (see the
      ! TODO: below), so this isn't going to stay for ever this way.
      
      ! Extend the NUOPC Field Dictionary to hold required entries.
      !TODO: In the long run this section will not be needed when we have
      !TODO: absorbed the needed standard names into the default dictionary.
      ! -> 20 fields identified as exports by the GSM component
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "mean_zonal_moment_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_zonal_moment_flx", &
          canonicalUnits="N m-2", &
          defaultLongName="Mean Zonal Component of Momentum Flux", &
          defaultShortName="mzmfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "mean_merid_moment_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_merid_moment_flx", &
          canonicalUnits="N m-2", &
          defaultLongName="Mean Merid Component of Momentum Flux", &
          defaultShortName="mmmfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "mean_sensi_heat_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_sensi_heat_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Sensible Heat Flux", &
          defaultShortName="mshfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "mean_laten_heat_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_laten_heat_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Latent Heat Flux", &
          defaultShortName="mlhfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "mean_down_lw_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_down_lw_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Downward Long Wave Radiation Flux", &
          defaultShortName="mdlwfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "mean_down_sw_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_down_sw_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Downward Short Wave Radiation Flux", &
          defaultShortName="mdswfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "mean_fprec_rate")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_fprec_rate", &
          canonicalUnits="kg s m-2", &
          defaultLongName="Mean Frozen Precipitation Rate", &
          defaultShortName="fprec", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "mean_prec_rate")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_prec_rate", &
          canonicalUnits="kg s m-2", &
          defaultLongName="Mean Liquid Precipitation Rate", &
          defaultShortName="lprec", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "mean_evap_rate")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_evap_rate", &
          canonicalUnits="kg s m-2", &
          defaultLongName="Mean Evaporation Rate", &
          defaultShortName="mevap", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_zonal_moment_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_zonal_moment_flx", &
          canonicalUnits="N m-2", &
          defaultLongName="Instantaneous Zonal Component of Momentum Flux", &
          defaultShortName="izmfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_merid_moment_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_merid_moment_flx", &
          canonicalUnits="N m-2", &
          defaultLongName="Instantaneous Merid Component of Momentum Flux", &
          defaultShortName="immfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_sensi_heat_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_sensi_heat_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Sensible Heat Flux", &
          defaultShortName="ishfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_laten_heat_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_laten_heat_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Latent Heat Flux", &
          defaultShortName="ilhfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_down_lw_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_down_lw_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Downward Long Wave Radiation Flux", &
          defaultShortName="idlwfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_down_sw_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_down_sw_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Downward Short Wave Radiation Flux", &
          defaultShortName="idswfx", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_temp_height2m")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_temp_height2m", &
          canonicalUnits="K", &
          defaultLongName="Instantaneous Temperature 2m Above Ground", &
          defaultShortName="ith2m", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_spec_humid_height2m")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_spec_humid_height2m", &
          canonicalUnits="kg kg-1", &
          defaultLongName="Instantaneous Specific Humidity 2m Above Ground", &
          defaultShortName="ishh2m", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_u_wind_height10m")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_u_wind_height10m", &
          canonicalUnits="m s-1", &
          defaultLongName="Instantaneous u Wind 10m Above Ground", &
          defaultShortName="iuwh10m", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_v_wind_height10m")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_v_wind_height10m", &
          canonicalUnits="m s-1", &
          defaultLongName="Instantaneous v Wind 10m Above Ground", &
          defaultShortName="ivwh10m", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_temp_height_surface")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_temp_height_surface", &
          canonicalUnits="K", &
          defaultLongName="Instantaneous Temperature Surface", &
          defaultShortName="its", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_pres_height_surface")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_pres_height_surface", &
          canonicalUnits="Pa", &
          defaultLongName="Instantaneous Pressure Surface", &
          defaultShortName="ips", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not.NUOPC_FieldDictionaryHasEntry( &
        "inst_surface_height")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_surface_height", &
          canonicalUnits="m", &
          defaultLongName="Instantaneous Surface Height", &
          defaultShortName="ish", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      ! -> Additional fields identified as needed by MOM5 and others...
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_down_sw_vis_dir_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_down_sw_vis_dir_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Downward Direct Visible Short Wave Radiation Flux", &
          defaultShortName="sw_flux_vis_dir", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_down_sw_vis_dif_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_down_sw_vis_dif_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Downward Diffuse Visible Short Wave Radiation Flux", &
          defaultShortName="sw_flux_vis_dif", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_down_sw_ir_dir_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_down_sw_ir_dir_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Downward Direct Short Wave IR Radiation Flux", &
          defaultShortName="sw_flux_nir_dir", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_down_sw_ir_dif_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_down_sw_ir_dif_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Downward Short Wave IR Radiation Flux", &
          defaultShortName="sw_flux_nir_dif", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_down_sw_vis_dir_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_down_sw_vis_dir_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Downward Direct Visible Short Wave Radiation Flux", &
          defaultShortName="inst_sw_flux_vis_dir", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_down_sw_vis_dif_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_down_sw_vis_dif_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Downward Diffuse Visible Short Wave Radiation Flux", &
          defaultShortName="inst_sw_flux_vis_dif", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_down_sw_ir_dir_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_down_sw_ir_dir_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Downward Direct Short Wave IR Radiation Flux", &
          defaultShortName="inst_sw_flux_nir_dir", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_down_sw_ir_dif_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_down_sw_ir_dif_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Downward Short Wave IR Radiation Flux", &
          defaultShortName="inst_sw_flux_nir_dif", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_net_sw_vis_dir_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_net_sw_vis_dir_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Net Direct Visible Short Wave Radiation Flux", &
          defaultShortName="sw_net_flux_vis_dir", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_net_sw_vis_dif_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_net_sw_vis_dif_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Net Diffuse Visible Short Wave Radiation Flux", &
          defaultShortName="sw_net_flux_vis_dif", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_net_sw_ir_dir_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_net_sw_ir_dir_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Net Direct Short Wave IR Radiation Flux", &
          defaultShortName="sw_net_flux_nir_dir", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_net_sw_ir_dif_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_net_sw_ir_dif_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Net Short Wave IR Radiation Flux", &
          defaultShortName="sw_net_flux_nir_dif", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_net_sw_vis_dir_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_net_sw_vis_dir_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Net Direct Visible Short Wave Radiation Flux", &
          defaultShortName="inst_net_sw_flux_vis_dir", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_net_sw_vis_dif_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_net_sw_vis_dif_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Net Diffuse Visible Short Wave Radiation Flux", &
          defaultShortName="inst_net_sw_flux_vis_dif", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_net_sw_ir_dir_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_net_sw_ir_dir_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Net Direct Short Wave IR Radiation Flux", &
          defaultShortName="inst_net_sw_flux_nir_dir", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_net_sw_ir_dif_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_net_sw_ir_dif_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Net Short Wave IR Radiation Flux", &
          defaultShortName="inst_net_sw_flux_nir_dif", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_salt_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_salt_flx", &
          canonicalUnits="kg m-2 s", &
          defaultLongName="Mean Salt Into Ocean Flux", &
          defaultShortName="salt_flux", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_runoff_rate")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_runoff_rate", &
          canonicalUnits="kg m-2 s", &
          defaultLongName="Mean Liquid Runoff Mass Flux", &
          defaultShortName="runoff", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_calving_rate")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_calving_rate", &
          canonicalUnits="kg m-2 s", &
          defaultLongName="Mean Frozen Runoff Mass Flux", &
          defaultShortName="calving", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_runoff_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_runoff_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Liquid Land Water Heat Flux Into Ocean, "// &
            "Relative To 0C", &
          defaultShortName="runoff_hflx", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry(  &
        "mean_calving_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_calving_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Frozen Land Water Heat Flux Into Ocean, "// &
            "Relative to 0C", &
          defaultShortName="calving_hflx", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mass_of_overlying_sea_ice")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mass_of_overlying_sea_ice", &
          canonicalUnits="kg", &
          defaultLongName="Mass Of Overlying Sea Ice", &
          defaultShortName="mi", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "s_surf")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="s_surf", &
          canonicalUnits="psu", &
          defaultLongName="sea surface salinity on t-cell", &
          defaultShortName="s_surf", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "u_surf")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="u_surf", &
          canonicalUnits="m s-1", &
          defaultLongName="i-directed surface ocean velocity on u-cell", &
          defaultShortName="u_surf", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "v_surf")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="v_surf", &
          canonicalUnits="m s-1", &
          defaultLongName="j-directed surface ocean velocity on u-cell", &
          defaultShortName="v_surf", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "sea_lev")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="sea_lev", &
          canonicalUnits="m", &
          defaultLongName="sea level", &
          defaultShortName="sea_lev", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "wind_stress_zonal")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="wind_stress_zonal", &
          canonicalUnits="N m-2", &
          defaultLongName="wind stress x component", &
          defaultShortName="strax", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "wind_stress_merid")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="wind_stress_merid", &
          canonicalUnits="N m-2", &
          defaultLongName="wind stress y component", &
          defaultShortName="stray", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "ocn_current_zonal")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="ocn_current_zonal", &
          canonicalUnits="m s-1", &
          defaultLongName="ocean current x component", &
          defaultShortName="uocn", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "ocn_current_merid")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="ocn_current_merid", &
          canonicalUnits="m s-1", &
          defaultLongName="ocean current y component", &
          defaultShortName="vocn", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "sss_zonal")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="sss_zonal", &
          canonicalUnits="m m-1", &
          defaultLongName="sea surface slope x component", &
          defaultShortName="ss_tltx", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "sss_merid")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="sss_merid", &
          canonicalUnits="m m-1", &
          defaultLongName="sea surface slope y component", &
          defaultShortName="ss_tlty", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "stress_on_air_ice_zonal")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="stress_on_air_ice_zonal", &
          canonicalUnits="N m-2", &
          defaultLongName="stress on air by ice x component", &
          defaultShortName="strairxT", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "stress_on_air_ice_merid")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="stress_on_air_ice_merid", &
          canonicalUnits="N m-2", &
          defaultLongName="stress on air by ice y component", &
          defaultShortName="strairyT", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "stress_on_ocn_ice_zonal")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="stress_on_ocn_ice_zonal", &
          canonicalUnits="N m-2", &
          defaultLongName="stress on ocn by ice x component", &
          defaultShortName="strocnxT", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "stress_on_ocn_ice_merid")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="stress_on_ocn_ice_merid", &
          canonicalUnits="N m-2", &
          defaultLongName="stress on ocn by ice y component", &
          defaultShortName="strocnyT", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "wind_stress_x")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="wind_stress_x", &
          canonicalUnits="N m-2", &
          defaultLongName="wind stress x component", &
          defaultShortName="strax", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "wind_stress_y")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="wind_stress_y", &
          canonicalUnits="N m-2", &
          defaultLongName="wind stress y component", &
          defaultShortName="stray", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "ocn_current_x")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="ocn_current_x", &
          canonicalUnits="m s-1", &
          defaultLongName="ocean current x component", &
          defaultShortName="uocn", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "ocn_current_y")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="ocn_current_y", &
          canonicalUnits="m s-1", &
          defaultLongName="ocean current y component", &
          defaultShortName="vocn", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "sss_x")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="sss_x", &
          canonicalUnits="m m-1", &
          defaultLongName="sea surface slope x component", &
          defaultShortName="ss_tltx", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "sss_y")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="sss_y", &
          canonicalUnits="m m-1", &
          defaultLongName="sea surface slope y component", &
          defaultShortName="ss_tlty", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "stress_on_air_ice_x")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="stress_on_air_ice_x", &
          canonicalUnits="N m-2", &
          defaultLongName="stress on air by ice x component", &
          defaultShortName="strairxT", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "stress_on_air_ice_y")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="stress_on_air_ice_y", &
          canonicalUnits="N m-2", &
          defaultLongName="stress on air by ice y component", &
          defaultShortName="strairyT", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "stress_on_ocn_ice_x")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="stress_on_ocn_ice_x", &
          canonicalUnits="N m-2", &
          defaultLongName="stress on ocn by ice x component", &
          defaultShortName="strocnxT", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "stress_on_ocn_ice_y")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="stress_on_ocn_ice_y", &
          canonicalUnits="N m-2", &
          defaultLongName="stress on ocn by ice y component", &
          defaultShortName="strocnyT", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_net_lw_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_net_lw_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Net Long Wave Radiation Flux", &
          defaultShortName="mnlwfx", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "mean_net_sw_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="mean_net_sw_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Mean Net Short Wave Radiation Flux", &
          defaultShortName="mnswfx", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_net_lw_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_net_lw_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Net Long Wave Radiation Flux", &
          defaultShortName="inlwfx", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_net_sw_flx")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_net_sw_flx", &
          canonicalUnits="W m-2", &
          defaultLongName="Instantaneous Net Short Wave Radiation Flux", &
          defaultShortName="inswfx", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_ir_dir_albedo")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_ir_dir_albedo", &
          canonicalUnits="1", &
          defaultLongName="Instantaneous Infrared Direct Albedo", &
          defaultShortName="iirdira", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_ir_dif_albedo")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_ir_dif_albedo", &
          canonicalUnits="1", &
          defaultLongName="Instantaneous Infrared Diffused Albedo", &
          defaultShortName="iirdifa", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_vis_dir_albedo")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_vis_dir_albedo", &
          canonicalUnits="1", &
          defaultLongName="Instantaneous Visible Direct Albedo", &
          defaultShortName="ivisdira", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_vis_dif_albedo")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_vis_dif_albedo", &
          canonicalUnits="1", &
          defaultLongName="Instantaneous Visible Diffused Albedo", &
          defaultShortName="ivisdifa", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_ocn_ir_dir_albedo")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_ocn_ir_dir_albedo", &
          canonicalUnits="1", &
          defaultLongName="Instantaneous Ocean Infrared Direct Albedo", &
          defaultShortName="iirdira", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_ocn_ir_dif_albedo")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_ocn_ir_dif_albedo", &
          canonicalUnits="1", &
          defaultLongName="Instantaneous Ocean Infrared Diffused Albedo", &
          defaultShortName="iirdifa", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_ocn_vis_dir_albedo")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_ocn_vis_dir_albedo", &
          canonicalUnits="1", &
          defaultLongName="Instantaneous Ocean Visible Direct Albedo", &
          defaultShortName="ivisdira", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
      if (.not. NUOPC_FieldDictionaryHasEntry( &
        "inst_ocn_vis_dif_albedo")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="inst_ocn_vis_dif_albedo", &
          canonicalUnits="1", &
          defaultLongName="Instantaneous Ocean Visible Diffused Albedo", &
          defaultShortName="ivisdifa", &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif 
#else

!-----------------------------------------------------------------------
!***  Register the EARTH Initialize, Run, and Finalize routines.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for EARTH Initialize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(EARTH_GRID_COMP                   &  !<-- The EARTH component
                                     ,ESMF_METHOD_INITIALIZE            &  !<-- Subroutine type (Initialize)
                                     ,EARTH_INITIALIZE                  &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for EARTH Run"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(EARTH_GRID_COMP                   &  !<-- The EARTH component
                                     ,ESMF_METHOD_RUN                   &  !<-- Subroutine type (Run)
                                     ,EARTH_RUN                         &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for EARTH Finalize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(EARTH_GRID_COMP                   &  !<-- The EARTH component
                                     ,ESMF_METHOD_FINALIZE              &  !<-- Subroutine type (Finalize)
                                     ,EARTH_FINALIZE                    &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
!
!-----------------------------------------------------------------------

#endif

!-----------------------------------------------------------------------
!
      END SUBROUTINE EARTH_REGISTER
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!

#ifdef WITH_NUOPC

      subroutine SetModelPetLists(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc
        
        ! local variables
        type(driver_type_IS)          :: is
        integer                       :: petCount, i
        integer                       :: petListBounds(2)
        type(ESMF_Config)             :: config
        character(len=20)             :: model

        rc = ESMF_SUCCESS

        ! query Component for its internal State
        nullify(is%wrap)
        call ESMF_UserCompGetInternalState(driver, driver_label_IS, is, rc)
        ESMF_ERR_RETURN(rc,rc)
          
        ! get petCount and config
        call ESMF_GridCompGet(driver, petCount=petCount, config=config, rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        
        ! determine the ATM petList bounds
        call ESMF_ConfigGetAttribute(config, petListBounds, &
          label="atm_petlist_bounds:", default=-1, rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
          petListBounds(1) = 0
          petListBounds(2) = petCount - 1
        endif
        
        call ESMF_ConfigGetAttribute(config, model, label="atm_model:", rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (trim(model) /= "none") then
          ! set petList for ATM
          allocate(is%wrap%atmPetList(petListBounds(2)-petListBounds(1)+1))
          do i=petListBounds(1), petListBounds(2)
            is%wrap%atmPetList(i-petListBounds(1)+1) = i ! PETs are 0 based
          enddo
        endif
          
        ! determine the OCN petList bounds
        call ESMF_ConfigGetAttribute(config, petListBounds, &
          label="ocn_petlist_bounds:", default=-1, rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
          petListBounds(1) = 0
          petListBounds(2) = petCount - 1
        endif
        
        call ESMF_ConfigGetAttribute(config, model, label="ocn_model:", rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (trim(model) /= "none") then
          ! set petList for OCN
          allocate(is%wrap%ocnPetList(petListBounds(2)-petListBounds(1)+1))
          do i=petListBounds(1), petListBounds(2)
            is%wrap%ocnPetList(i-petListBounds(1)+1) = i ! PETs are 0 based
          enddo
        endif
        
        ! determine the ICE petList bounds
        call ESMF_ConfigGetAttribute(config, petListBounds, &
          label="ice_petlist_bounds:", default=-1, rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
          petListBounds(1) = 0
          petListBounds(2) = petCount - 1
        endif
        
        call ESMF_ConfigGetAttribute(config, model, label="ice_model:", rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (trim(model) /= "none") then
          ! set petList for ICE
          allocate(is%wrap%icePetList(petListBounds(2)-petListBounds(1)+1))
          do i=petListBounds(1), petListBounds(2)
            is%wrap%icePetList(i-petListBounds(1)+1) = i ! PETs are 0 based
          enddo
        endif
        
        ! determine the MED petList bounds
        call ESMF_ConfigGetAttribute(config, petListBounds, &
          label="med_petlist_bounds:", default=-1, rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
          petListBounds(1) = 0
          petListBounds(2) = petCount - 1
        endif
        
        call ESMF_ConfigGetAttribute(config, model, label="med_model:", rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (trim(model) /= "none") then
          ! set petList for MED
          allocate(is%wrap%medPetList(petListBounds(2)-petListBounds(1)+1))
          do i=petListBounds(1), petListBounds(2)
            is%wrap%medPetList(i-petListBounds(1)+1) = i ! PETs are 0 based
          enddo
        endif

      end subroutine
      
      ! ------------------------------------------------------------------------

      subroutine SetModelServices(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc

        ! local variables
        type(driver_type_IS)          :: is
        type(ESMF_GridComp)           :: comp
        type(ESMF_CplComp)            :: conn
        type(ESMF_Config)             :: config
        character(len=20)             :: model
        character(len=160)            :: msg
        logical                       :: atmFlag, ocnFlag, iceFlag, medFlag

        rc = ESMF_SUCCESS

        ! query Component for its internal State
        nullify(is%wrap)
        call ESMF_UserCompGetInternalState(driver, driver_label_IS, is, rc)
        ESMF_ERR_RETURN(rc,rc)
        
        ! get config
        call ESMF_GridCompGet(driver, config=config, rc=rc)
        ESMF_ERR_RETURN(rc,rc)

        ! SetServices for ATM
        call ESMF_ConfigGetAttribute(config, model, label="atm_model:", rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        atmFlag = .false.
        !print *, "atm_model: ", trim(model)
        if (trim(model) /= "none") then
          atmFlag = .true.
#define WITH_ATM
#ifdef WITH_ATM
          call NUOPC_DriverAddComp(driver, "ATM", ATM_REGISTER, comp, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(comp, name="Verbosity", value="high", &
            convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
#else
          write (msg, *) "ATM model '", trim(model), "' was requested, "// &
            "but is not available in the executable!"
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
            file=__FILE__, rcToReturn=rc)
          return  ! bail out
#endif
        endif
        
        ! SetServices for OCN
        call ESMF_ConfigGetAttribute(config, model, label="ocn_model:", rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        ocnFlag = .false.
        !print *, "ocn_model: ", trim(model)
        if (trim(model) == "dummy") then
          ocnFlag = .true.
#ifdef FRONT_OCN_DUMMY
          call NUOPC_DriverAddComp(driver, "OCN", OCN_DUMMY_SS, comp, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(comp, name="Verbosity", value="high", &
            convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
#else
          write (msg, *) "OCN model '", trim(model), "' was requested, "// &
            "but is not available in the executable!"
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
            file=__FILE__, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "hycom") then
          ocnFlag = .true.
#ifdef FRONT_HYCOM
          call NUOPC_DriverAddComp(driver, "OCN", OCN_HYCOM_SS, comp, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(comp, name="Verbosity", value="high", &
            convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
#else
          write (msg, *) "OCN model '", trim(model), "' was requested, "// &
            "but is not available in the executable!"
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
            file=__FILE__, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "mom5") then
          ocnFlag = .true.
#ifdef FRONT_MOM5
          call NUOPC_DriverAddComp(driver, "OCN", OCN_MOM5_SS, comp, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(comp, name="Verbosity", value="high", &
            convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
#else
          write (msg, *) "OCN model '", trim(model), "' was requested, "// &
            "but is not available in the executable!"
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
            file=__FILE__, rcToReturn=rc)
          return  ! bail out
#endif
        endif
        
        ! SetServices for ICE
        call ESMF_ConfigGetAttribute(config, model, label="ice_model:", rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        iceFlag = .false.
        !print *, "ice_model: ", trim(model)
        if (trim(model) == "dummy") then
          iceFlag = .true.
#ifdef FRONT_ICE_DUMMY
          call NUOPC_DriverAddComp(driver, "ICE", ICE_DUMMY_SS, comp, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(comp, name="Verbosity", value="high", &
            convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
#else
          write (msg, *) "ICE model '", trim(model), "' was requested, "// &
            "but is not available in the executable!"
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
            file=__FILE__, rcToReturn=rc)
          return  ! bail out
#endif
        elseif (trim(model) == "cice") then
          iceFlag = .true.
#ifdef FRONT_CICE
          call NUOPC_DriverAddComp(driver, "ICE", ICE_CICE_SS, comp, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(comp, name="Verbosity", value="high", &
            convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
#else
          write (msg, *) "ICE model '", trim(model), "' was requested, "// &
            "but is not available in the executable!"
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
            file=__FILE__, rcToReturn=rc)
          return  ! bail out
#endif
        endif
        
        ! SetServices for Mediator
        call ESMF_ConfigGetAttribute(config, model, label="med_model:", rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        medFlag = .false.
        !print *, "med_model: ", trim(model)
        if (trim(model) /= "none") then
          medFlag = .true.
          call NUOPC_DriverAddComp(driver, "MED", MED_SS, comp, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(comp, name="Verbosity", value="high", &
            convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
        endif

        ! SetServices for Connectors
        if (atmFlag .and. medFlag) then
          ! SetServices for atm2med
          call NUOPC_DriverAddComp(driver, &
            srcCompLabel="ATM", dstCompLabel="MED", &
            compSetServicesRoutine=conSS, comp=conn, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(conn, name="Verbosity", &
            value="high", convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ! SetServices for med2atm
          call NUOPC_DriverAddComp(driver, &
            srcCompLabel="MED", dstCompLabel="ATM", &
            compSetServicesRoutine=conSS, comp=conn, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(conn, name="Verbosity", &
            value="high", convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
        endif
        if (ocnFlag .and. medFlag) then
          ! SetServices for ocn2med
          call NUOPC_DriverAddComp(driver, &
            srcCompLabel="OCN", dstCompLabel="MED", &
            compSetServicesRoutine=conSS, comp=conn, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(conn, name="Verbosity", &
            value="high", convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ! SetServices for med2ocn
          call NUOPC_DriverAddComp(driver, &
            srcCompLabel="MED", dstCompLabel="OCN", &
            compSetServicesRoutine=conSS, comp=conn, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(conn, name="Verbosity", &
            value="high", convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
        endif
        if (iceFlag .and. medFlag) then
          ! SetServices for ice2med
          call NUOPC_DriverAddComp(driver, &
            srcCompLabel="ICE", dstCompLabel="MED", &
            compSetServicesRoutine=conSS, comp=conn, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(conn, name="Verbosity", &
            value="high", convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ! SetServices for med2ice
          call NUOPC_DriverAddComp(driver, &
            srcCompLabel="MED", dstCompLabel="ICE", &
            compSetServicesRoutine=conSS, comp=conn, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_AttributeSet(conn, name="Verbosity", &
            value="high", convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
        endif

        ! Read in the coupling intervals and set in the internal state
        call ESMF_ConfigGetAttribute(config, is%wrap%medAtmCouplingIntervalSec,&
          label="med_atm_coupling_interval_sec:", default=-1.0_ESMF_KIND_R8, &
          rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        call ESMF_ConfigGetAttribute(config, is%wrap%medOcnCouplingIntervalSec,&
          label="med_ocn_coupling_interval_sec:", default=-1.0_ESMF_KIND_R8, &
          rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        
        ! Internal Clock and RunSequence will be set by EARTH_GENERIC_COMP

      end subroutine

#else

!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!

      SUBROUTINE EARTH_INITIALIZE(EARTH_GRID_COMP                       &
                                 ,IMP_STATE                             &
                                 ,EXP_STATE                             &
                                 ,CLOCK_NEMS                            &
                                 ,RC_INIT)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: EARTH_GRID_COMP                               !<-- The EARTH component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The EARTH import state
                         ,EXP_STATE                                        !<-- The EARTH export state
!
      TYPE(ESMF_Clock) :: CLOCK_NEMS                                       !<-- The NEMS component ESMF Clock
!
      INTEGER,INTENT(OUT) :: RC_INIT                                       !<-- Error return code
!
!-----------------------------------------------------------------------
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_INIT = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Allocate the EARTH component's internal state, point at it,
!***  and attach it to the EARTH component.
!-----------------------------------------------------------------------
!
      ALLOCATE(EARTH_INT_STATE,stat=RC)
      wrap%EARTH_INT_STATE=>EARTH_INT_STATE
!
      CALL ESMF_GridCompSetInternalState(EARTH_GRID_COMP                &  !<--The EARTH component
                                        ,WRAP                           &  !<-- Pointer to the EARTH internal state
                                        ,RC)     
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the NEMS Clock within
!***  the EARTH component.
!-----------------------------------------------------------------------
!
      earth_int_state%CLOCK_EARTH=CLOCK_NEMS
!
!-----------------------------------------------------------------------
!***  The ATM (atmosphere) gridded component resides inside of
!***  the EARTH internal state.
!-----------------------------------------------------------------------
!
      earth_int_state%ATM_GRID_COMP=ESMF_GridCompCreate(name        ="ATM component" &
                                                       ,rc          =RC)
!-----------------------------------------------------------------------
!***  Register the Initialize, Run, and Finalize routines of
!***  the ATM component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Register ATM Init, Run, Finalize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetServices(earth_int_state%ATM_GRID_COMP       &
                                   ,ATM_REGISTER                        &  !<-- The user's subroutine name
                                   ,rc=RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!***  Create the ATM import and export states.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the ATM import state"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      earth_int_state%ATM_IMP_STATE=ESMF_StateCreate(STATENAME="ATM Import"      &
                                                    ,stateintent = ESMF_STATEINTENT_IMPORT &
                                                    ,rc       =RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the ATM export state"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      earth_int_state%ATM_EXP_STATE=ESMF_StateCreate(STATENAME   ="ATM Export"             &
                                                    ,stateintent = ESMF_STATEINTENT_EXPORT &
                                                    ,rc       =RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!***  Insert the import/export states of the ATMOS component into the
!***  import/export states of the EARTH component.  This simplifies
!***  the passing of information between lower and higher component 
!***  levels seen in the diagram above.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK= "Add the ATMOS states into the EARTH states"
!     CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_StateAdd(IMP_STATE, LISTWRAPPER(earth_int_state%ATM_IMP_STATE), rc = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
      CALL ESMF_StateAdd(EXP_STATE, LISTWRAPPER(earth_int_state%ATM_EXP_STATE), rc = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!***  Execute the Initialize step of the ATM component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Initialize step of the ATM component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompInitialize(gridcomp   =earth_int_state%ATM_GRID_COMP &
                                  ,importState=earth_int_state%ATM_IMP_STATE &
                                  ,exportState=earth_int_state%ATM_EXP_STATE &
                                  ,clock      =earth_int_state%CLOCK_EARTH   &
                                  ,phase      =1                             &
                                  ,rc         =RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!-----------------------------------------------------------------------
!
      END SUBROUTINE EARTH_INITIALIZE
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE EARTH_RUN(EARTH_GRID_COMP                              &
                          ,IMP_STATE                                    &
                          ,EXP_STATE                                    &
                          ,CLOCK_NEMS                                   &
                          ,RC_RUN)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: EARTH_GRID_COMP                               !<-- The EARTH component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The EARTH import state
                         ,EXP_STATE                                        !<-- The EARTH export state
!
      TYPE(ESMF_Clock) :: CLOCK_NEMS                                       !<-- The NEMS component ESMF Clock
!
      INTEGER,INTENT(OUT) :: RC_RUN                                        !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      TYPE(ESMF_Time) :: CURRTIME                                       &
                        ,STARTTIME
!
      TYPE(ESMF_TimeInterval) :: RUNDURATION
!
      INTEGER :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_RUN = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Execute the Run step of the ATM component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Run step of the  ATM component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompRun(gridcomp   =earth_int_state%ATM_GRID_COMP   &
                           ,importState=earth_int_state%ATM_IMP_STATE   &
                           ,exportState=earth_int_state%ATM_EXP_STATE   &
                           ,clock      =earth_int_state%CLOCK_EARTH     &
                           ,phase      =1                               &
                           ,rc         =RC)
      ESMF_ERR_RETURN(RC,RC_RUN)
!
!-----------------------------------------------------------------------
!***  Update the EARTH clock.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK = "Update the current time of the EARTH clock"
!     CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ClockGet(clock       = earth_int_state%CLOCK_EARTH      &
                        ,startTime   = startTime                        &
                        ,runDuration = runDuration                      &
                        ,rc          = RC)
      ESMF_ERR_RETURN(RC,RC_RUN)
!
      CURRTIME = STARTTIME + RUNDURATION
!
      CALL ESMF_ClockSet(clock    = earth_int_state%CLOCK_EARTH         &
                        ,currTime = CURRTIME                            &
                        ,rc       = RC)
      ESMF_ERR_RETURN(RC,RC_RUN)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE EARTH_RUN
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE EARTH_FINALIZE(EARTH_GRID_COMP                         &
                               ,IMP_STATE                               &
                               ,EXP_STATE                               &
                               ,CLOCK_NEMS                              &
                               ,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: EARTH_GRID_COMP                               !<-- The EARTH component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The EARTH import state
                         ,EXP_STATE                                        !<-- The EARTH export state
!
      TYPE(ESMF_Clock) :: CLOCK_NEMS                                       !<-- The NEMS component ESMF Clock
!
      INTEGER,INTENT(OUT) :: RC_FINALIZE                                   !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
!-----------------------------------------------------------------------
!
      INTEGER :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_FINALIZE = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Execute the Finalize step of the ATM ccomponent.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Finalize step of the  ATM component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompFinalize(gridcomp   =earth_int_state%ATM_GRID_COMP &
                                ,importState=earth_int_state%ATM_IMP_STATE &
                                ,exportState=earth_int_state%ATM_EXP_STATE &
                                ,clock      =earth_int_state%CLOCK_EARTH   &
                                ,phase      =1                             &
                                ,rc         =RC)
      ESMF_ERR_RETURN(RC,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE EARTH_FINALIZE

#endif

!
!-----------------------------------------------------------------------
!
      END MODULE module_EARTH_GRID_COMP
!
!-----------------------------------------------------------------------
