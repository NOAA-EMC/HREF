#include "./ESMFVersionDefine.h"
#ifdef WITH_NUOPC

module module_MEDIATOR

  !-----------------------------------------------------------------------------
  ! NEMS Mediator Component.
  !
  ! The Mediator has two Run() phases:
  !
  !   * Run(phase=1) covers the more frequent interaction with the ATM
  !     component. The ATM exports some fields as time averages over the 
  !     integration period. Other fields are exported as instantaneous. For 
  !     the time averaged fields the Mediator is responsible to continue
  !     the averaging.
  !
  !   * Run(phase=2) is invoked for the less frequent interaction with the OCN
  !     component. Here the averaged and instantanous ATM fields are passed on
  !     to the OCN component, and OCN export fields are received by the
  !     Mediator and forwarded to the ATM component.
  !
  ! The two phases are operating on different time scales, and hence require
  ! two separate internal Component Clocks. The NUOPC layer accesses a
  ! Component's Clock through the ESMF CompGet() interface, regardless of the
  ! phase. Phase specific Clocks are implemented by swapping Clocks during the
  ! phase specific "label_SetRunClock" specialization method. These Clock
  ! objects are stored in the Component instance's own internal state.
  !
  ! Current implementation (March 2014) assumes the atm and ice are running
  ! at the same coupling period which is shorter than the ocean coupling period.
  ! Accumulation of atm and ice fields are done for the ocean model.  No 
  ! accumulation is done for the atm or ice models from any model.  The
  ! atm, ice, and ocean pass their latest data directly to the atm and ice models.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Mediator, &
    mediator_routine_SS             => routine_SetServices, &
    mediator_routine_Run            => routine_Run, &
    mediator_type_IS                => type_InternalState, &
    mediator_label_IS               => label_InternalState, &
    mediator_label_DataInitialize   => label_DataInitialize, &
    mediator_label_Advance          => label_Advance, &
    mediator_label_CheckImport      => label_CheckImport, &
    mediator_label_TimestampExport  => label_TimestampExport, &
    mediator_label_SetRunClock      => label_SetRunClock
  
  implicit none
  
  private
  
  ! private internal state to keep instance data
  type InternalStateStruct
    type(ESMF_Clock)      :: clockAtm    ! clock for atm
    type(ESMF_Clock)      :: clockOcn    ! clock for ocn
    type(ESMF_Clock)      :: clockIce    ! clock for ice
    integer               :: fastcntr    ! slice counter for writing to NetCDF file
    integer               :: slowcntr    ! slice counter for writing to NetCDF file
    integer               :: accumcntAtm ! accumulator counter
    integer               :: accumcntOcn ! accumulator counter
    integer               :: accumcntIce ! accumulator counter
    type(ESMF_FieldBundle):: FBaccumAtm  ! accumulator of atm export data
    type(ESMF_FieldBundle):: FBaccumOcn  ! accumulator of ocn export data
    type(ESMF_FieldBundle):: FBaccumIce  ! accumulator of ice export data
    type(ESMF_FieldBundle):: FBforOcn    ! data storage for ocn import
    type(ESMF_FieldBundle):: FBforAtm    ! data storage for atm import
    type(ESMF_FieldBundle):: FBforIce    ! data storage for ice import
    type(ESMF_RouteHandle):: rh !gjt: temporary solution
! tcx Xgrid
!    type(ESMF_RouteHandle):: RHa2x       ! atm to xgrid RH
!    type(ESMF_RouteHandle):: RHo2x       ! ocn to xgrid RH
!    type(ESMF_RouteHandle):: RHx2a       ! xgrid to atm RH
!    type(ESMF_RouteHandle):: RHx2o       ! xgrid to ocn RH
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  interface fieldBundle_accum ; module procedure &
    fieldBundle_accumFB2FB, &
    fieldBundle_accumFB2ST, &
    fieldBundle_accumST2FB
  end interface

  interface fieldBundle_copy ; module procedure &
    fieldBundle_copyFB2FB, &
    fieldBundle_copyFB2ST, &
    fieldBundle_copyST2FB
  end interface

  ! tcraig some temporary debug variables
  integer, parameter :: nx_atm=400, ny_atm=200
  integer, parameter :: nx_ocn=400, ny_ocn=200
  integer, parameter :: nx_ice=400, ny_ice=200
  integer, parameter :: dbug_flag = 5
  integer            :: dbrc
  character(len=256) :: tmpstr
  real(ESMF_KIND_R8), parameter :: const_lhvap = 2.501e6_ESMF_KIND_R8  ! latent heat of evaporation ~ J/kg

  integer, parameter :: med_phase_max = 3

  !--- These also appear in module_EARTH_GENERIC_COMP.F90
  integer, parameter :: medPhase_slow = 1
  integer, parameter :: medPhase_fast_before = 2
  integer, parameter :: medPhase_fast_after  = 3

  type fld_list_type
    integer :: num = -1
    character(len=64), pointer :: stdname(:)
    character(len=64), pointer :: shortname(:)
    character(len=64), pointer :: longname(:)
    character(len=64), pointer :: units(:)
    character(len=64), pointer :: transferOffer(:)
  end type fld_list_type
  type (fld_list_type) :: fldsToAtm
  type (fld_list_type) :: fldsFrAtm
  type (fld_list_type) :: fldsToOcn
  type (fld_list_type) :: fldsFrOcn
  type (fld_list_type) :: fldsToIce
  type (fld_list_type) :: fldsFrIce


  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer :: nphase
    character(len=*),parameter :: subname='(module_MEDIATOR:SetServices)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! the NUOPC mediator component will register the generic methods
    call NUOPC_CompDerive(gcomp, mediator_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Provide InitP0 to overwrite the default IPD00
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! InitP1, where Fields should be advertised
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP1, phase=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! InitP2, where Fields should be realized,
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP2, phase=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! InitP6, where ...
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP6, phase=6, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! InitP7, where ...
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP7, phase=7, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do nphase = 2,med_phase_max
      call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
        userRoutine=mediator_routine_Run, phase=nphase, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    ! overwrite Finalize
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing methods for Run ( phase = slow) "ocn"
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specIndex=MedPhase_slow, specRoutine=SetRunClock_ocn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specIndex=MedPhase_slow, specRoutine=Advance_slow, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing methods for Run( phase = fast_before )
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specIndex=MedPhase_fast_before, specRoutine=SetRunClock_atm_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_CheckImport, &
      specIndex=MedPhase_fast_before, specRoutine=CheckImport_atm_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_TimestampExport, &
      specIndex=MedPhase_fast_before, specRoutine=TimestampExport_atm_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specIndex=MedPhase_fast_before, specRoutine=Advance_fast_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing methods for Run( phase = fast_after )
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specIndex=MedPhase_fast_after, specRoutine=SetRunClock_atm_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_CheckImport, &
      specIndex=MedPhase_fast_after, specRoutine=CheckImport_atm_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_TimestampExport, &
      specIndex=MedPhase_fast_after, specRoutine=TimestampExport_atm_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specIndex=MedPhase_fast_after, specRoutine=Advance_fast_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if (1 == 0)
    !----- Ocn-Atm

    call fld_list_add(fldsFrOcn,"sea_surface_temperature", "sst", "sea surface temperature on t-cell","K", "will provide")
    call fld_list_add(fldsToAtm,"sea_surface_temperature", "sst", "sea surface temperature on t-cell","K", "will provide")

    !----- Atm-Ocn, Atm-Ice

    ! shortwave
    call fld_list_add(fldsFrAtm,"mean_down_sw_ir_dir_flx" , "sw_flux_nir_dir" , "Mean infrared direct downward shortwave flux" , "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_down_sw_ir_dif_flx" , "sw_flux_nir_dif" , "Mean infrared diffuse downward shortwave flux", "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_down_sw_vis_dir_flx", "sw_flux_vis_dir" , "Mean visible direct downward shortwave flux"  , "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_down_sw_vis_dif_flx", "sw_flux_vis_dif" , "Mean visible diffuse downward shortwave flux" , "W m-2", "will provide")

    ! other heat fluxes
    call fld_list_add(fldsFrAtm,"mean_down_lw_flx"        , "mdlwfx" , "Mean Downward Long Wave Radiation Flux", "W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_down_lw_flx"        , "mdlwfx" , "Mean Downward Long Wave Radiation Flux", "W m-2", "will provide")

    ! water
    call fld_list_add(fldsFrAtm,"mean_prec_rate"          , "lprec"  , "Mean Liquid Precipitation Rate", "kg/m^2/s", "will provide")
    call fld_list_add(fldsFrAtm,"mean_fprec_rate"         , "fprec"  , "Mean Frozen Precipitation Rate", "kg/m^2/s", "will provide")
    call fld_list_add(fldsToOcn,"mean_prec_rate"          , "lprec"  , "Mean Liquid Precipitation Rate", "kg/m^2/s", "will provide")
!   call fld_list_add(fldsToOcn,"mean_fprec_rate"         , "fprec"  , "Mean Frozen Precipitation Rate", "kg/m^2/s", "will provide")

    ! states
    call fld_list_add(fldsFrAtm,"inst_pres_height_surface", "ips"    , "Pressure at surface defined by inst_surface_height","Pa", "will provide")
    call fld_list_add(fldsToOcn,"inst_pres_height_surface", "ips"    , "Pressure at surface defined by inst_surface_height","Pa", "will provide")
    call fld_list_add(fldsFrAtm,"inst_spec_humid_height2m", "ishh2m" , "Instantaneous Specific Humidity 2m Above Ground","kg kg-1", "will provide")
    call fld_list_add(fldsToOcn,"mean_laten_heat_flx"     , "mlhfx"  , "Mean Latent Heat Flux"                          ,"W m-2"  , "will provide")
    call fld_list_add(fldsToOcn,"mean_evap_rate"          , "mevap"  , "Mean Evaporation Rate"                          ,"kg m-2 s-1"  , "will provide")

    !----- Ice-Atm

    ! momentum stress
    call fld_list_add(fldsFrIce,"stress_on_air_ice_zonal", "strairxT", "stress on air by ice x component", "N m-2", "will provide")
    call fld_list_add(fldsFrIce,"stress_on_air_ice_merid", "strairyT", "stress on air by ice y component", "N m-2", "will provide")
    call fld_list_add(fldsToAtm,"stress_on_air_ice_zonal", "strairxT", "stress on air by ice x component", "N m-2", "will provide")
    call fld_list_add(fldsToAtm,"stress_on_air_ice_merid", "strairyT", "stress on air by ice y component", "N m-2", "will provide")

    !----- Ice-Ocn

    ! momentum stress
    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_zonal", "strocnxT", "stress on ocn by ice x component", "N m-2", "will provide")
    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_merid", "strocnyT", "stress on ocn by ice y component", "N m-2", "will provide")
    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_zonal", "strocnxT", "stress on ocn by ice x component", "N m-2", "will provide")
    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_merid", "strocnyT", "stress on ocn by ice y component", "N m-2", "will provide")

    ! ----- Ocn-Ice

    ! states
    call fld_list_add(fldsFrOcn,"ocn_current_zonal", "ocncx", "ocean current x component", "m s-1", "will provide")
    call fld_list_add(fldsFrOcn,"ocn_current_merid", "ocncy", "ocean current y component", "m s-1", "will provide")
    call fld_list_add(fldsToIce,"ocn_current_zonal", "ocncx", "ocean current x component", "m s-1", "will provide")
    call fld_list_add(fldsToIce,"ocn_current_merid", "ocncy", "ocean current y component", "m s-1", "will provide")

    call fld_list_add(fldsFrOcn,"s_surf", "sss", "sea surface salinity on t-cell", "psu", "will provide")
    call fld_list_add(fldsToIce,"s_surf", "sss", "sea surface salinity on t-cell", "psu", "will provide")
#endif

    ! Fields to ATM
    call fld_list_add(fldsToAtm,"sea_surface_temperature", "sst"     , "sea surface temperature on t-cell","K"    , "will provide")
    call fld_list_add(fldsToAtm,"stress_on_air_ice_zonal"    , "strairxT", "stress on air by ice x component" ,"N m-2", "will provide")
    call fld_list_add(fldsToAtm,"stress_on_air_ice_merid"    , "strairyT", "stress on air by ice y component" ,"N m-2", "will provide")
 
    ! Fields from ATM
    call fld_list_add(fldsFrAtm,"mean_zonal_moment_flx"   , "mzmfx"  , "Mean Zonal Component of Momentum Flux",               "Pa", "will provide")
    call fld_list_add(fldsFrAtm,"mean_merid_moment_flx"   , "mmmfx"  , "Mean Merid Component of Momentum Flux",               "Pa", "will provide")
    call fld_list_add(fldsFrAtm,"mean_sensi_heat_flx"     , "mshfx"  , "Mean Sensible Heat Flux",                          "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_laten_heat_flx"     , "mlhfx"  , "Mean Latent Heat Flux",                            "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_down_lw_flx"        , "mdlwfx" , "Mean Downward Long Wave Radiation Flux",           "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_down_sw_flx"        , "mdswfx" , "Mean Downward Shortwave Radiation Flux",           "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_prec_rate"          , "lprec"  , "Mean Liquid Precipitation Rate",                "kg/m^2/s", "will provide")
    call fld_list_add(fldsFrAtm,"mean_fprec_rate"         , "fprec"  , "Mean Frozen Precipitation Rate",                "kg/m^2/s", "will provide")
    call fld_list_add(fldsFrAtm,"inst_zonal_moment_flx"   , "izmfx"  , "Instantaneous Zonal Component of Momentum Flux",      "Pa", "will provide")
    call fld_list_add(fldsFrAtm,"inst_merid_moment_flx"   , "immfx"  , "Instantaneous Merid Component of Momentum Flux",      "Pa", "will provide")
    call fld_list_add(fldsFrAtm,"inst_sensi_heat_flx"     , "ishfx"  , "Instantaneous Sensible Heat Flux",                 "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_laten_heat_flx"     , "ilhfx"  , "Instantaneous Latent Heat Flux",                   "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_down_lw_flx"        , "idlwfx" , "Instantaneous Downward Long Wave Radiation Flux",  "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_down_sw_flx"        , "idswfx" , "Instantaneous Downward Shortwave Radiation Flux",  "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_temp_height2m"      , "ith2m"  , "Instantaneous Temperature 2m Above Ground",            "K", "will provide")
    call fld_list_add(fldsFrAtm,"inst_spec_humid_height2m", "ishh2m" , "Instantaneous Specific Humidity 2m Above Ground","kg kg-1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_u_wind_height10m"   , "iuwh10m", "Instantaneous u Wind 10m Above Ground",            "m s-1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_v_wind_height10m"   , "ivwh10m", "Instantaneous v Wind 10m Above Ground",            "m s-1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_temp_height_surface", "its"    , "Instantaneous Temperature Surface",                    "K", "will provide")
    call fld_list_add(fldsFrAtm,"inst_pres_height_surface", "ips"    , "Instantaneous Pressure Surface",                      "Pa", "will provide")
    call fld_list_add(fldsFrAtm,"inst_surface_height"     , "ish"    , "Instantaneous Surface Height",                         "m", "will provide")
    ! new imports from GSM added 04/23/14:
    call fld_list_add(fldsFrAtm,"mean_net_lw_flx"         , "mnlwfx" , "Mean Net Long Wave Radiation Flux",                "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_net_sw_flx"         , "mnswfx" , "Mean Net Short Wave Radiation Flux",               "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_net_lw_flx"         , "inlwfx" , "Instantaneous Net Long Wave Radiation Flux",       "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_net_sw_flx"         , "inswfx" , "Instantaneous Net Short Wave Radiation Flux",      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_down_sw_ir_dir_flx" , "sw_flux_nir_dir" , ""                                  ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_down_sw_ir_dif_flx" , "sw_flux_nir_dif" , ""                                  ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_down_sw_vis_dir_flx", "sw_flux_vis_dir" , ""                                  ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_down_sw_vis_dif_flx", "sw_flux_vis_dif" , ""                                  ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_down_sw_ir_dir_flx" , "inst_sw_flux_nir_dir" , ""                             ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_down_sw_ir_dif_flx" , "inst_sw_flux_nir_dif" , ""                             ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_down_sw_vis_dir_flx", "inst_sw_flux_vis_dir" , ""                             ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_down_sw_vis_dif_flx", "inst_sw_flux_vis_dif" , ""                             ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_net_sw_ir_dir_flx"  , "sw_net_flux_nir_dir"  , ""                             ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_net_sw_ir_dif_flx"  , "sw_net_flux_nir_dif"  , ""                             ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_net_sw_vis_dir_flx" , "sw_net_flux_vis_dir"  , ""                             ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"mean_net_sw_vis_dif_flx" , "sw_net_flux_vis_dif"  , ""                             ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_net_sw_ir_dir_flx"  , "inst_net_sw_flux_nir_dir" , ""                         ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_net_sw_ir_dif_flx"  , "inst_net_sw_flux_nir_dif" , ""                         ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_net_sw_vis_dir_flx" , "inst_net_sw_flux_vis_dir" , ""                         ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_net_sw_vis_dif_flx" , "inst_net_sw_flux_vis_dif" , ""                         ,      "W m-2", "will provide")
    call fld_list_add(fldsFrAtm,"inst_ir_dir_albedo"      , "iirdira" , ""                                          ,          "1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_ir_dif_albedo"      , "iirdifa" , ""                                          ,          "1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_vis_dir_albedo"     , "ivisdira", ""                                          ,          "1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_vis_dif_albedo"     , "ivisdifa", ""                                          ,          "1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_ocn_ir_dir_albedo"  , "ioirdira" , "Ocean infrared band direct albedo"        ,          "1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_ocn_ir_dif_albedo"  , "ioirdifa" , "Ocean infrared band diffuse albedo"       ,          "1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_ocn_vis_dir_albedo" , "iovisdira", "Ocean visible band direct albedo"         ,          "1", "will provide")
    call fld_list_add(fldsFrAtm,"inst_ocn_vis_dif_albedo" , "iovisdifa", "Ocean visible band diffuse albedo"        ,          "1", "will provide")

    ! Fields to OCN
    call fld_list_add(fldsToOcn,"mean_zonal_moment_flx"   , "mzmfx"   , "Mean Zonal Component of Momentum Flux"                         ,"Pa", "will provide")
    call fld_list_add(fldsToOcn,"mean_merid_moment_flx"   , "mmmfx"   , "Mean Merid Component of Momentum Flux"                         ,"Pa", "will provide")
    call fld_list_add(fldsToOcn,"mean_sensi_heat_flx"     , "mshfx"   , "Mean Sensible Heat Flux"                                    ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_laten_heat_flx"     , "mlhfx"   , "Mean Latent Heat Flux"                                      ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_down_lw_flx"        , "mdlwfx"  , "Mean Downward Long Wave Radiation Flux"                     ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_vis_dir_flx", "mdvrsfx" , "Mean Downward visible direct Shortwave Radiation Flux"      ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_vis_dif_flx", "mdvfsfx" , "Mean Downward visible diffuse Shortwave Radiation Flux"     ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_ir_dir_flx" , "mdirsfx" , "Mean Downward nearinfrared direct Shortwave Radiation Flux" ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_ir_dif_flx" , "mdifsfx" , "Mean Downward nearinfrared diffuse Shortwave Radiation Flux","W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_vis_dir_flx" , "mndvrsfx"         , "Mean Downward visible direct Shortwave Radiation Flux"      ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_vis_dif_flx" , "mndvfsfx"         , "Mean Downward visible diffuse Shortwave Radiation Flux"     ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_ir_dir_flx"  , "mndirsfx"         , "Mean Downward nearinfrared direct Shortwave Radiation Flux" ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_ir_dif_flx"  , "mndifsfx"         , "Mean Downward nearinfrared diffuse Shortwave Radiation Flux","W m-2", "will provide")
!   call fld_list_add(fldsToOcn,"mean_salt_flx"           , "saltflx" , "salt flux into ocean"                                    ,"kg/m^2/s", "will provide")
    call fld_list_add(fldsToOcn,"mean_prec_rate"          , "lprec"   , "Mean Liquid Precipitation Rate"                          ,"kg/m^2/s", "will provide")
!   call fld_list_add(fldsToOcn,"mean_fprec_rate"         , "fprec"   , "Mean Frozen Precipitation Rate"                          ,"kg/m^2/s", "will provide")
    call fld_list_add(fldsToOcn,"mean_evap_rate"          , "mevap"   , "Mean Evaporation Rate"                                   ,"kg/m^2/s", "will provide")
!   call fld_list_add(fldsToOcn,"mean_runoff_rate"        , "runoff"  , "mass flux of liquid runoff"                              ,"kg/m^2/s", "will provide")
!   call fld_list_add(fldsToOcn,"mean_calving_rate"       , "calving" , "mass flux of frozen runoff"                              ,"kg/m^2/s", "will provide")
!   call fld_list_add(fldsToOcn,"mean_runoff_flx"         , "rofhfx"  , "heat flux, relative to 0C, of liquid land water into ocean" ,"W m-2", "will provide")
!   call fld_list_add(fldsToOcn,"mean_calving_flx"        , "cahflx"  , "heat flux, relative to 0C, of frozen land water into ocean" ,"W m-2", "will provide")
    call fld_list_add(fldsToOcn,"inst_pres_height_surface", "ips"     , "pressure of overlying sea ice and atmosphere"                  ,"Pa", "will provide")
!   call fld_list_add(fldsToOcn,"mass_of_overlying_sea_ice, "massice" , "mass of overlying sea ice"                                     ,"kg", "will provide")
    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_zonal"     , "strocnxT", "stress on ocn by ice x component"                           ,"N m-2", "will provide")
    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_merid"     , "strocnyT", "stress on ocn by ice y component"                           ,"N m-2", "will provide")

    ! Fields from OCN
    call fld_list_add(fldsFrOcn,"sea_surface_temperature" , "sst" , "sea surface temperature on t-cell","K", "will provide")
    call fld_list_add(fldsFrOcn,"s_surf"                  , "sss" , "sea surface salinity on t-cell" ,"psu", "will provide")
    call fld_list_add(fldsFrOcn,"u_surf"                  , "uocn", "ocean current x component"    ,"m s-1", "will provide")
    call fld_list_add(fldsFrOcn,"v_surf"                  , "vocn", "ocean current y component"    ,"m s-1", "will provide")
    call fld_list_add(fldsFrOcn,"sea_lev"                 , "ssh" , "Sea Level Height"                 ,"m", "will provide")

    ! Fields to ICE
    call fld_list_add(fldsToIce,"sss_zonal"        , "ss_tltx", "sea surface slope x component" ,"m m-1", "will provide")
    call fld_list_add(fldsToIce,"sss_merid"        , "ss_tlty", "sea surface slope y component" ,"m m-1", "will provide")
    call fld_list_add(fldsToIce,"wind_stress_zonal", "strax"  , "wind stress x component"       ,"N m-2", "will provide")
    call fld_list_add(fldsToIce,"wind_stress_merid", "stray"  , "wind stress y component"       ,"N m-2", "will provide")
    call fld_list_add(fldsToIce,"ocn_current_zonal", "uocn"   , "ocean current x component"     ,"m s-1", "will provide")
    call fld_list_add(fldsToIce,"ocn_current_merid", "vocn"   , "ocean current y component"     ,"m s-1", "will provide")

    ! Fields from ICE
    call fld_list_add(fldsFrIce,"stress_on_air_ice_zonal", "strairxT", "stress on air by ice x component","N m-2", "will provide")
    call fld_list_add(fldsFrIce,"stress_on_air_ice_merid", "strairyT", "stress on air by ice y component","N m-2", "will provide")
    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_zonal", "strocnxT", "stress on ocn by ice x component","N m-2", "will provide")
    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_merid", "strocnyT", "stress on ocn by ice y component","N m-2", "will provide")
 
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetServices
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables    
    character(len=NUOPC_PhaseMapStringLength) :: initPhases(6)
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeP0)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

!gjt    initPhases(1) = "IPDv02p1=1"
!gjt    initPhases(2) = "IPDv02p3=2"
!gjt    initPhases(3) = "IPDv02p4=3"
!gjt    initPhases(4) = "IPDv02p5=5"

    initPhases(1) = "IPDv03p1=1"
    initPhases(2) = "IPDv03p3=2"
    initPhases(3) = "IPDv03p4=6"
    initPhases(4) = "IPDv03p5=7"
    initPhases(5) = "IPDv03p6=3"
    initPhases(6) = "IPDv03p7=5"
    
    call ESMF_AttributeSet(gcomp, &
      name="InitializePhaseMap", valueList=initPhases, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeP0

  !-----------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    integer :: n
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeP1)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! importable fields:

    do n = 1,fldsFrAtm%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Advertise "//trim(fldsFrAtm%stdname(n))//":"// &
          trim(fldsFrAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(importState, &
        StandardName = trim(fldsFrAtm%stdname(n)), &
        name=fldsFrAtm%shortname(n), &
        TransferOfferGeomObject=fldsFrAtm%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    do n = 1,fldsFrOcn%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Advertise "//trim(fldsFrOcn%stdname(n))//":"// &
          trim(fldsFrOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(importState, &
        StandardName = fldsFrOcn%stdname(n), &
        name = fldsFrOcn%shortname(n), &
        TransferOfferGeomObject=fldsFrOcn%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    do n = 1,fldsFrIce%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Advertise "//trim(fldsFrIce%stdname(n))//":"// &
          trim(fldsFrIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(importState, &
        StandardName = fldsFrIce%stdname(n), &
        name = fldsFrIce%shortname(n), &
        TransferOfferGeomObject=fldsFrIce%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo
      
    ! exportable fields:

    do n = 1,fldsToAtm%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Advertise "//trim(fldsToAtm%stdname(n))//":"// &
          trim(fldsToAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(exportState, &
        StandardName = fldsToAtm%stdname(n), &
        name = fldsToAtm%shortname(n), &
        TransferOfferGeomObject=fldsToAtm%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    do n = 1,fldsToOcn%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Advertise "//trim(fldsToOcn%stdname(n))//":"// &
          trim(fldsToOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(exportState, &
        StandardName = fldsToOcn%stdname(n), &
        name = fldsToOcn%shortname(n), &
        TransferOfferGeomObject=fldsToOcn%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    do n = 1,fldsToIce%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Advertise "//trim(fldsToIce%stdname(n))//":"// &
          trim(fldsToIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(exportState, &
        StandardName = fldsToIce%stdname(n), &
        name = fldsToIce%shortname(n), &
        TransferOfferGeomObject=fldsToIce%transferOffer(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeP1
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    type(ESMF_Grid)             :: gridAtm, gridOcn, gridIce
    type(ESMF_Array)            :: array
    integer                     :: i, j
    real(kind=ESMF_KIND_R8),pointer :: lonPtr(:,:), latPtr(:,:)
    type(InternalState)         :: is
    integer                     :: stat
    type(ESMF_Config)           :: config
    real(ESMF_KIND_R8)          :: intervalSec
    type(ESMF_TimeInterval)     :: timeStep
! tcx XGrid
!    type(ESMF_Field)            :: fieldX, fieldA, fieldO
!    type(ESMF_XGrid)            :: xgrid
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeP2)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! Allocate memory for the internal state and set it in the Component.
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of the internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Initialize the internal state members
    is%wrap%fastcntr = 1
    is%wrap%slowcntr = 1

    gridAtm = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_atm, ny_atm, &
      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    gridOcn = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_ocn, ny_ocn, &
      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    gridIce = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_ice, ny_ice, &
      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if 1
    ! dump the ATM Grid coordinate arrays for reference      
    call ESMF_GridGetCoord(gridAtm, coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_med_atm_grid_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(gridAtm, coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_med_atm_grid_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! dump the OCN Grid coordinate arrays for reference      
    call ESMF_GridGetCoord(gridOcn, coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_med_ocn_grid_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(gridOcn, coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_med_ocn_grid_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! dump the ICE Grid coordinate arrays for reference      
    call ESMF_GridGetCoord(gridIce, coordDim=1, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_med_ice_grid_coord1.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(gridIce, coordDim=2, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayWrite(array, file="array_med_ice_grid_coord2.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif
        
    !--- Generate RouteHandles
! tcx Xgrid
! what needs to be in the grids to create an XGrid (corners?)
! add error checking code

!    xgrid = ESMF_XGridCreate(sideAGrid=(/gridatm/), sideBGrid=(/gridocn/), rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    fieldX = ESMF_FieldCreate(xgrid  , typekind=ESMF_TYPEKIND_R8, rc=rc)
!    fieldA = ESMF_FieldCreate(gridAtm, typekind=ESMF_TYPEKIND_R8, rc=rc)
!    fieldO = ESMF_FieldCreate(gridAtm, typekind=ESMF_TYPEKIND_R8, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldA, fieldX, routehandle=is%wrap%RHa2x, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldO, fieldX, routehandle=is%wrap%RHo2x, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldX, fieldA, routehandle=is%wrap%RHx2a, rc=rc)
!    call ESMF_FieldRegridStore(xgrid, fieldX, fieldO, routehandle=is%wrap%RHx2o, rc=rc)
!    call ESMF_FieldDestroy(fieldX, rc=rc)
!    call ESMF_FieldDestroy(fieldA, rc=rc)
!    call ESMF_FieldDestroy(fieldO, rc=rc)
!    call ESMF_XGridDestroy(xgrid, rc=rc)

    !--- Importable fields from atm:

!gjt: import fields from ATM are now marked as "cannot provide" thus accept Grid
!gjt: -> eventually comment out the following lines...
    call realizeConnectedFields(importState, fieldNameList=fldsFrAtm%shortname(1:fldsFrAtm%num), &
      grid=gridAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Exportable fields to atm:

    call realizeConnectedFields(exportState, fieldNameList=fldsToAtm%shortname(1:fldsToAtm%num), &
      grid=gridAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Importable fields from ocn:

    call realizeConnectedFields(importState, fieldNameList=fldsFrOcn%shortname(1:fldsFrOcn%num), &
      grid=gridOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Exportable fields to ocn:

    call realizeConnectedFields(exportState, fieldNameList=fldsToOcn%shortname(1:fldsToOcn%num), &
      grid=gridOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Importable fields from ice:

    call realizeConnectedFields(importState, fieldNameList=fldsFrIce%shortname(1:fldsFrIce%num), &
      grid=gridIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Exportable fields to ice:

    call realizeConnectedFields(exportState, fieldNameList=fldsToIce%shortname(1:fldsToIce%num), &
      grid=gridIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Accumulators

    call fieldBundle_init(is%wrap%FBaccumAtm, fieldNameList=fldsFrAtm%shortname(1:fldsFrAtm%num), &
      grid=gridAtm, state=importState, name='FBaccumAtm', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBaccumOcn, fieldNameList=fldsFrOcn%shortname(1:fldsFrOcn%num), &
      grid=gridOcn, state=importState, name='FBaccumOcn', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBaccumIce, fieldNameList=fldsFrIce%shortname(1:fldsFrIce%num), &
      grid=gridIce, state=importState, name='FBaccumIce', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Data ready for export to models

    call fieldBundle_init(is%wrap%FBforAtm, fieldNameList=fldsToAtm%shortname(1:fldsToAtm%num), &
      grid=gridAtm, state=exportState, name='FBforAtm', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBforOcn, fieldNameList=fldsToOcn%shortname(1:fldsToOcn%num), &
      grid=gridOcn, state=exportState, name='FBforOcn', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBforIce, fieldNameList=fldsToIce%shortname(1:fldsToIce%num), &
      grid=gridIce, state=exportState, name='FBforIce', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Initialize the internal clocks
    
    ! both atm and ocn clocks start out as copies of the incoming clock
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": clockcreate", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    is%wrap%clockAtm = ESMF_ClockCreate(clock, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    is%wrap%clockOcn = ESMF_ClockCreate(clock, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    is%wrap%clockIce = ESMF_ClockCreate(clock, rc=rc)
    ESMF_ERR_RETURN(rc,rc)

    config = ESMF_ConfigCreate(rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    call ESMF_ConfigLoadFile(config, "nems.configure", rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    ! ATM coupling interval -> atm time step
    call ESMF_ConfigGetAttribute(config, intervalSec, &
      label="med_atm_coupling_interval_sec:", default=-1.0_ESMF_KIND_R8, &
      rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    if (dbug_flag > 1) then
      write(tmpstr,'(A,f12.3)') trim(subname)//': atm intervalSec = ',intervalSec
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    if (intervalSec>0._ESMF_KIND_R8) then
      ! The coupling time step was provided
      call ESMF_TimeIntervalSet(timeStep, s_r8=intervalSec, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
      call ESMF_ClockSet(is%wrap%clockAtm, timestep=timeStep, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
    endif
    
    ! OCN coupling interval -> ocn time step
    call ESMF_ConfigGetAttribute(config, intervalSec, &
      label="med_ocn_coupling_interval_sec:", default=-1.0_ESMF_KIND_R8, &
      rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    if (dbug_flag > 1) then
      write(tmpstr,'(A,f12.3)') trim(subname)//': ocn intervalSec = ',intervalSec
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    if (intervalSec>0._ESMF_KIND_R8) then
      ! The coupling time step was provided
      call ESMF_TimeIntervalSet(timeStep, s_r8=intervalSec, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
      call ESMF_ClockSet(is%wrap%clockOcn, timestep=timeStep, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
    endif

    ! ICE coupling interval -> ice time step
    call ESMF_ConfigGetAttribute(config, intervalSec, &
      label="med_ice_coupling_interval_sec:", default=-1.0_ESMF_KIND_R8, &
      rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    if (dbug_flag > 1) then
      write(tmpstr,'(A,f12.3)') trim(subname)//': ice intervalSec = ',intervalSec
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    if (intervalSec>0._ESMF_KIND_R8) then
      ! The coupling time step was provided
      call ESMF_TimeIntervalSet(timeStep, s_r8=intervalSec, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
      call ESMF_ClockSet(is%wrap%clockIce, timestep=timeStep, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
    endif
    
    ! Clean Up

!    call ESMF_GridDestroy(gridAtm, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
!    call ESMF_GridDestroy(gridOcn, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    
    call ESMF_ConfigDestroy(config, rc=rc)
    ESMF_ERR_RETURN(rc,rc)
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
    subroutine realizeConnectedFields(state, fieldNameList, grid, rc)
      type(ESMF_State)                :: state
      character(len=*)                :: fieldNameList(:)
      type(ESMF_Grid)                 :: grid
      integer, intent(out), optional  :: rc

      integer                         :: n
      type(ESMF_Field)                :: field
      character(len=*),parameter :: subname='(module_MEDIATOR:realizeConnectedFields)'

      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      if (present(rc)) rc = ESMF_SUCCESS
      
      do n=1, size(fieldNameList)
        if (NUOPC_StateIsFieldConnected(state, fieldName=fieldNameList(n))) then
          ! realize the connected Field using the internal coupling Field
          field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=fieldNameList(n),rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_StateRealizeField(state, field=field, rc=rc)
          if (dbug_flag > 1) then
            call ESMF_LogWrite(trim(subname)//": create  "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          endif
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        else
          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldNameList(n)/), rc=rc)
          if (dbug_flag > 1) then
            call ESMF_LogWrite(trim(subname)//": exclude "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          endif
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
      enddo
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    end subroutine realizeConnectedFields

  end subroutine InitializeP2
  
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------

  subroutine InitializeP6(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Field)              :: field
    type(ESMF_Grid)               :: grid
    integer                       :: localDeCount
    character(160)                :: msgString

    type(ESMF_DistGrid)           :: distgrid
    integer                       :: dimCount, tileCount, petCount
    integer                       :: deCountPTile, extraDEs
    integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
    integer, allocatable          :: regDecompPTile(:,:)
    integer                       :: i, j, n
    
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeP6)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    rc = ESMF_SUCCESS
    
#if 0

    !NOTE: All fo the Fields that set their TransferOfferGeomObject Attribute
    !NOTE: to "cannot provide" should now have the accepted Grid available.
    !NOTE: Go and pull out this Grid for one of a representative Field and 
    !NOTE: modify the decomposition and distribution of the Grid to match the
    !NOTE: Mediator PETs.

    !gjt: for now I am only doing this on the ATM side, but it should eventually
    !gjt: be done on OCN side, too.
    
    !TODO: quick implementation, assume that there is at least one field
    !TODO: and assume that all fields in fldsFrAtm are going to accept Grids
    
    ! access the first Field that is in fldsFrAtm and access in importState
    call ESMF_StateGet(exportState, field=field, &
      itemName=fldsFrAtm%shortname(1), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! while this is still an empty field, it does now hold a Grid with DistGrid
    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    write (msgString,*) "MED - InitializeP6: localDeCount = ", localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Create a custom DistGrid, based on the minIndex, maxIndex of the 
    ! accepted DistGrid, but with a default regDecomp for the current VM
    ! that leads to 1DE/PET.
    
    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
      maxIndexPTile(dimCount, tileCount))
    
    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
      maxIndexPTile=maxIndexPTile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! construct a default regDecompPTile -> TODO: move this into ESMF as default
    call ESMF_GridCompGet(gcomp, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    allocate(regDecompPTile(dimCount, tileCount))
    deCountPTile = petCount/tileCount
    extraDEs = max(0, petCount-deCountPTile)
    do i=1, tileCount
      if (i<=extraDEs) then
        regDecompPTile(1, i) = deCountPTile + 1
      else
        regDecompPTile(1, i) = deCountPTile
      endif
      do j=2, dimCount
        regDecompPTile(j, i) = 1
      enddo
    enddo
    
    ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
    ! but with a default regDecompPTile
    distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
      maxIndexPTile=maxIndexPTile, regDecompPTile=regDecompPTile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Create a new Grid on the new DistGrid and swap it in the Field
    grid = ESMF_GridCreate(distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! local clean-up
    deallocate(minIndexPTile, maxIndexPTile, regDecompPTile)

    ! access localDeCount of the final Grid
    call ESMF_GridGet(grid, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    write (msgString,*) "MED - InitializeP6: final Grid localDeCount = ", &
      localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
        
    ! Also must swap the Grid for all the other fldsFrAtm Fields in the 
    ! importState
    
    do n=2, fldsFrAtm%num
      ! access a field in the importState and set the Grid
      call ESMF_StateGet(importState, field=field, &
        itemName=fldsFrAtm%shortname(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
#endif

  end subroutine InitializeP6
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP7(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Field)              :: field
    integer                       :: n
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeP6)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    rc = ESMF_SUCCESS

#if 0
    
    do n=1, fldsFrAtm%num
      ! access a field in the importState and complete it with data allocation
      call ESMF_StateGet(importState, field=field, &
        itemName=fldsFrAtm%shortname(n), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! the transferred Grid is already set, allocate field data memory
      call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo
#endif

  end subroutine InitializeP7
  
  !-----------------------------------------------------------------------------

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: time
    type(ESMF_Field)            :: field
    type(ESMF_StateItem_Flag)   :: itemType
    logical                     :: neededCurrent
    logical                     :: allDone
    type(InternalState)         :: is
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:DataInitialize)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! the MED needs valid ATM export Fields to initialize its internal state

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! Get the internal state from Component.
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get the current time out of the clock
    call ESMF_ClockGet(clock, currTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    allDone = .true.  ! flag that can be reset if anything is not found done
    
    ! check that required Fields in the importState show correct timestamp
    ! -> really should check _all_ of the fields from the ATM
    
    ! -> check for "mzmfx"
    call ESMF_StateGet(importState, itemName="mzmfx", itemType=itemType, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (itemType /= ESMF_STATEITEM_NOTFOUND) then
      call ESMF_StateGet(importState, field=field, itemName="mzmfx", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      neededCurrent = NUOPC_FieldIsAtTime(field, time, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
      if (.not.neededCurrent) then
        call ESMF_LogWrite("MED - Initialize-Data-Dependency NOT YET SATISFIED!!!", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        allDone = .false.
      else
        call ESMF_LogWrite("MED - Initialize-Data-Dependency SATISFIED!!!", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

      endif
    endif

    ! TODO - tcraig ?? what's above here?
    ! For the real case this should probably use the "mzmfx" field from the
    ! importState and do something with it as a sensible starting point
    ! for the accumulation field so that the OCN receives a meaningful
    ! "mzmfx" field during its first time step. However, here for testing
    ! I simply initialize to zero.
          
    call fieldBundle_reset(is%wrap%FBaccumAtm, value=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    is%wrap%accumcntAtm = 0

    call fieldBundle_reset(is%wrap%FBaccumOcn, value=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    is%wrap%accumcntOcn = 0

    call fieldBundle_reset(is%wrap%FBaccumIce, value=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    is%wrap%accumcntIce = 0
    
#if 0
!gjt: with different Grids on each side, need to precompute a Regrid
    call ESMF_FieldBundleRegridStore(srcFieldBundle=is%wrap%FBaccumAtm, &
      dstFieldBundle=is%wrap%FBforOcn, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
      routehandle=is%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    if (allDone) then
      ! -> set InitializeDataComplete Component Attribute to "true", indicating
      ! to the driver that this Component has fully initialized its data
      call ESMF_AttributeSet(gcomp, &
        name="InitializeDataComplete", value="true", &
        convention="NUOPC", purpose="General", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine SetRunClock_atm_before(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! local variables
    type(InternalState)     :: is_local
    type(mediator_type_IS)  :: is
    character(len=*),parameter :: subname='(module_MEDIATOR:SetRunClock_atm_before)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query component for its internal state
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set clockAtm to be the component clock
    call ESMF_GridCompSet(gcomp, clock=is_local%wrap%clockAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query component for its inherited internal state
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, mediator_label_IS, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! check and set the component clock against the driver clock
    call NUOPC_GridCompCheckSetClock(gcomp, is%wrap%driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="NUOPC INCOMPATIBILITY DETECTED: between model and driver clocks", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetRunClock_atm_before

  !-----------------------------------------------------------------------------

  subroutine SetRunClock_atm_after(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! local variables
    type(InternalState)     :: is_local
    type(mediator_type_IS)  :: is
    character(len=*),parameter :: subname='(module_MEDIATOR:SetRunClock_atm_after)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query component for its internal state
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set clockAtm to be the component clock
    call ESMF_GridCompSet(gcomp, clock=is_local%wrap%clockAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query component for its inherited internal state
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, mediator_label_IS, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! check and set the component clock against the driver clock
    call NUOPC_GridCompCheckSetClock(gcomp, is%wrap%driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="NUOPC INCOMPATIBILITY DETECTED: between model and driver clocks", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetRunClock_atm_after

  !-----------------------------------------------------------------------------

  subroutine SetRunClock_ocn(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! local variables
    type(InternalState)     :: is_local
    type(mediator_type_IS)  :: is
    character(len=*),parameter :: subname='(module_MEDIATOR:SetRunClock_ocn)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query component for its internal state
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set clockOcn to be the component clock
    call ESMF_GridCompSet(gcomp, clock=is_local%wrap%clockOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query component for its inherited internal state
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, mediator_label_IS, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! check and set the component clock against the driver clock
    call NUOPC_GridCompCheckSetClock(gcomp, is%wrap%driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="NUOPC INCOMPATIBILITY DETECTED: between model and driver clocks", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine SetRunClock_ocn

  !-----------------------------------------------------------------------------

  subroutine CheckImport_atm_before(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This is the routine that ensures that the import Fields come in with
    ! the correct time stamps during the "fast" cycle: 
    ! -> Fields from the ATM are not used by the "before" phase and need not 
    !    be checked.
    ! -> Fields from the OCN must be at the startTime of the parent driver 
    !    Clock
    
    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: startTime
    type(ESMF_State)        :: importState
    type(ESMF_Field)        :: field
    logical                 :: atCorrectTime
    character(len=*),parameter :: subname='(module_MEDIATOR:CheckImport_atm_before)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its importState
    call ESMF_GridCompGet(gcomp, importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query the Component for its driverClock
    call NUOPC_MediatorGet(gcomp, driverClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! get the start time out of the driver Clock
    call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! check fields from OCN to be at startTime of the driver Clock
    if (NUOPC_StateIsFieldConnected(importState, fieldName="sst")) then
      call ESMF_StateGet(importState, itemName="sst", field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      atCorrectTime = NUOPC_FieldIsAtTime(field, startTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (.not.atCorrectTime) then
        !TODO: introduce and use INCOMPATIBILITY return codes!!!!
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at correct time", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif
    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine CheckImport_atm_before

  !-----------------------------------------------------------------------------
  
  subroutine CheckImport_atm_after(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This is the routine that ensures that the import Fields come in with
    ! the correct time stamps during the "fast" cycle: 
    ! -> Fields from the ATM must be at stopTime because this mediator phase
    !    runs _after_ the ATM runs.
    ! -> Fields from the OCN are not used by the "after" phase and need not 
    !    be checked.
    
    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: stopTime
    type(ESMF_State)        :: importState
    type(ESMF_Field)        :: field
    logical                 :: atCorrectTime
    character(len=*),parameter :: subname='(module_MEDIATOR:CheckImport_atm_after)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its Clock and importState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! get the stop time out of the Clock
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! check fields from ATM to be at stopTime
    if (NUOPC_StateIsFieldConnected(importState, fieldName="pmsl")) then
      call ESMF_StateGet(importState, itemName="pmsl", field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      atCorrectTime = NUOPC_FieldIsAtTime(field, stopTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (.not.atCorrectTime) then
        !TODO: introduce and use INCOMPATIBILITY return codes!!!!
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at correct time", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif
    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine CheckImport_atm_after

  !-----------------------------------------------------------------------------
  
  subroutine TimestampExport_atm_before(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This is the routine that executes _after_ the "fast_before" mediator 
    ! phase has been run. Timestamping does not need to be adjusted here,
    ! but the Clock needs to be stepped back because the "fast_after" phase
    ! will be updating the same Clock during the same driver cylce.

    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_TimeInterval) :: timeStep
    character(len=*),parameter :: subname='(module_MEDIATOR:TimestampExport_atm_before)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! get the timeStep out of Clock
    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! step the Clock back one timestep
    call ESMF_ClockAdvance(clock, timeStep= -timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine TimestampExport_atm_before

  !-----------------------------------------------------------------------------

  subroutine TimestampExport_atm_after(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc
    
    ! This is the routine that applies the time stamp on the export Fields
    ! during the "atm" cycle: 
    ! -> By default the MED Run method time stamps the export Fields with the
    !    current time at the beginning of the advance step, however here,
    !    because the "atm" cycle runs after the ATM model, the correct time
    !    stamp is the currTime _after_ the MED advance step.

    ! local variables
    type(ESMF_Clock)      :: clock
    type(ESMF_State)      :: exportState
    character(len=*),parameter :: subname='(module_MEDIATOR:TimestampExport_atm_after)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(gcomp, clock=clock, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! update timestamp on export Fields
    call NUOPC_StateSetTimestamp(exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine TimestampExport_atm_after

  !-----------------------------------------------------------------------------

  subroutine Advance_fast_before(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! This Mediator phase runs before ATM and ICE are being called and
    ! prepares the ATM and ICE import Fields.
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is
    integer                     :: i,j
    character(len=*),parameter :: subname='(module_MEDIATOR:Advance_fast_before)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Get the internal state from Component.
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields to the currTime.
    
    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    call ESMF_TimeGet(time,timestring=timestr)
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call NUOPC_ClockPrintCurrTime(clock, &
      "-------->MED Advance() mediating for: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !---------------------------------------
    !--- this is fast, so just copy latest values from import state to atm/ice FB
    !--- no accumulator needed
    !---------------------------------------

    call fieldBundle_copy(is%wrap%FBforAtm, importState, rc=rc)
    call fieldBundle_copy(is%wrap%FBforIce, importState, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBforAtm, trim(subname)//' FBforA_AFcopy ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBforIce, trim(subname)//' FBforI_AFcopy ', rc=rc)
    endif

    !---------------------------------------
    !--- custom calculations to atm and ice
    !---------------------------------------


        ! None yet


    !---------------------------------------
    !--- set export State to special value for testing
    !---------------------------------------

    call state_reset(exportState, value=-99._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(exportState, trim(subname)//' es_AF99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call fieldBundle_copy(exportState, is%wrap%FBforAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call fieldBundle_copy(exportState, is%wrap%FBforIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
      call state_diagnose(exportState, trim(subname)//' es_endfast ', rc=rc)
    endif

    ! write the fields exported to atm to file
    call NUOPC_StateWrite(exportState, fieldNameList=fldsToAtm%shortname, &
      filePrefix="field_med_to_atm_", timeslice=is%wrap%fastcntr, &
      relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write the fields exported to ice to file
    call NUOPC_StateWrite(exportState, fieldNameList=fldsToIce%shortname, &
      filePrefix="field_med_to_ice_", timeslice=is%wrap%fastcntr, &
      relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Advance_fast_before

  !-----------------------------------------------------------------------------

  subroutine Advance_fast_after(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(InternalState)         :: is
    integer                     :: i,j
    character(len=*),parameter :: subname='(module_MEDIATOR:Advance_fast_after)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Get the internal state from Component.
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the stopTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields to the stopTime, and then update 
    ! the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.
    
    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    call ESMF_TimeGet(time,timestring=timestr)
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call NUOPC_ClockPrintCurrTime(clock, &
      "-------->MED Advance() mediating for: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write the fields imported from atm to file
    call NUOPC_StateWrite(importState, fieldNameList=fldsFrAtm%shortname, &
      filePrefix="field_med_from_atm_", timeslice=is%wrap%fastcntr, &
      relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write the fields imported from ice to file
    call NUOPC_StateWrite(importState, fieldNameList=fldsFrIce%shortname, &
      filePrefix="field_med_from_ice_", timeslice=is%wrap%fastcntr, &
      relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !---------------------------------------
    !--- atm, ice accumulator for ocean
    !---------------------------------------

    if (dbug_flag > 1) then
      call State_diagnose(importState, trim(subname)//' is_beginfast ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBaccumAtm, trim(subname)//' FBaccA_B4accum ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBaccumIce, trim(subname)//' FBaccI_B4accum ', rc=rc)
    endif

    call fieldBundle_accum(is%wrap%FBaccumAtm, importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    is%wrap%accumcntAtm = is%wrap%accumcntAtm + 1

    call fieldBundle_accum(is%wrap%FBaccumIce, importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    is%wrap%accumcntIce = is%wrap%accumcntIce + 1
         
    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBaccumAtm, trim(subname)//' FBaccA_AFaccum ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBaccumIce, trim(subname)//' FBaccI_AFaccum ', rc=rc)
    endif

    if (dbug_flag > 1) then
      call State_diagnose(exportState, trim(subname)//' es_AF99 ', rc=rc)
    endif

    if (dbug_flag > 1) then
      call state_diagnose(exportState, trim(subname)//' es_endfast ', rc=rc)
    endif

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    !---------------------------------------

    is%wrap%fastcntr = is%wrap%fastcntr + 1

    !---------------------------------------

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Advance_fast_after

  !-----------------------------------------------------------------------------

  subroutine Advance_slow(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: time
    character(len=64)           :: timestr
    type(ESMF_State)            :: importState, exportState
    type(ESMF_StateItem_Flag)   :: itemType
    type(InternalState)         :: is
    integer                     :: i,j,n
    character(len=64)           :: fieldname1(10),fieldname2(10),fieldname3(10)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:)
    logical                     :: isPresent, checkOK, checkOK1, checkOK2
    character(len=*),parameter  :: subname='(module_MEDIATOR:Advance_slow)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Get the internal state from Component.
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
     return  ! bail out

    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the 
    ! mediators Clock.
    
    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.
    
    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields to the currentTime, and then update 
    ! the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.
    
    call ESMF_ClockGet(clock,currtime=time,rc=rc)
    call ESMF_TimeGet(time,timestring=timestr)
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": time = "//trim(timestr), ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    call NUOPC_ClockPrintCurrTime(clock, &
      "-------->MED Advance() mediating for: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write the fields imported from ocn to file
    call NUOPC_StateWrite(importState, fieldNameList=fldsFrOcn%shortname, &
      filePrefix="field_med_from_ocn_", timeslice=is%wrap%slowcntr, &
      overwrite=.true., relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !---------------------------------------
    !--- average atm accumulator
    !---------------------------------------

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBaccumAtm, trim(subname)//' FBaccA_B4avg ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBaccumIce, trim(subname)//' FBaccI_B4avg ', rc=rc)
    endif

    call FieldBundle_average(is%wrap%FBaccumAtm, is%wrap%accumcntAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call FieldBundle_average(is%wrap%FBaccumIce, is%wrap%accumcntIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    !---------------------------------------
    !--- regrid average atm fields to ocean grid
    !---------------------------------------

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBaccumAtm, trim(subname)//' FBaccA_B4regrid ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBaccumIce, trim(subname)//' FBaccI_B4regrid ', rc=rc)
    endif

! tcx Xgrid
    ! XGrid intermediary required? instantiate FBXgrid FieldBundle?
    ! call ESMF_FieldBundleRegrid(is%wrap%FBaccumAtm, FBXgrid, is%wrap%RHa2x, rc=rc)
    ! call ESMF_FieldBundleRegrid(FBXgrid, is%wrap%FBforOcn  , is%wrap%RHx2o, rc=rc)
    ! tcraig temporarily copy
    
#if 0    
!gjt: with different grids on each side we need at least a regrid here    
!gjt    call fieldBundle_copy(is%wrap%FBforOcn, is%wrap%FBaccumAtm, rc=rc)
    call ESMF_FieldBundleRegrid(srcFieldBundle=is%wrap%FBaccumAtm, &
      dstFieldBundle=is%wrap%FBforOcn, routehandle=is%wrap%rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#else
    call fieldBundle_copy(is%wrap%FBforOcn, is%wrap%FBaccumAtm, rc=rc)
    call fieldBundle_copy(is%wrap%FBforOcn, is%wrap%FBaccumIce, rc=rc)
#endif

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBforOcn, trim(subname)//' FB4ocn_AFregrid ', rc=rc)
    endif

    !---------------------------------------
    !--- custom calculations to ocn
    !---------------------------------------

    !--- split total solar into 4 terms and compute net shortwave

    !--- atm downward sw

    fieldname1(1) = 'sw_flux_vis_dir'
    fieldname1(2) = 'sw_flux_vis_dif'
    fieldname1(3) = 'sw_flux_nir_dir'
    fieldname1(4) = 'sw_flux_nir_dif'

    ! check fields exist
    checkOK = .true.
    do n = 1,4
      checkOK = checkOK .and. FieldBundle_FldChk(is%wrap%FBaccumAtm, trim(fieldname1(n)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    enddo

    if (dbug_flag > 1) then
      if (checkOK) then
        call ESMF_LogWrite(trim(subname)//' swd:found 4 sw down terms from atm', ESMF_LOGMSG_INFO, rc=dbrc)
      else
        call ESMF_LogWrite(trim(subname)//' swd:did not find 4 sw down terms from atm', ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

! tcraig old single downward sw field
!    if (.not. checkOK) then
!      fieldname1(1) = 'mdswfx'
!      ! check field exists
!      checkOK = FieldBundle_FldChk(is%wrap%FBaccumAtm, trim(fieldname1(1)), rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
!      if (dbug_flag > 1) then
!        if (checkOK) then
!          call ESMF_LogWrite(trim(subname)//' swd:found mdswfx from atm', ESMF_LOGMSG_INFO, rc=dbrc)
!        else
!          call ESMF_LogWrite(trim(subname)//' swd:did not find mdswfx from atm', ESMF_LOGMSG_INFO, rc=dbrc)
!        endif
!      endif
!    endif

    if (checkOK) then
      !--- ocn net sw
      fieldname2(1) = 'mndvrsfx'
      fieldname2(2) = 'mndvfsfx'
      fieldname2(3) = 'mndirsfx'
      fieldname2(4) = 'mndifsfx'

      ! check fields exist
      checkOK1 = .true.
      do n = 1,4
        checkOK1 = checkOK1 .and. FieldBundle_FldChk(is%wrap%FBforOcn, trim(fieldname2(n)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      enddo

      if (dbug_flag > 1) then
        if (checkOK1) then
          call ESMF_LogWrite(trim(subname)//' swd:found 4 net sw for ocn', ESMF_LOGMSG_INFO, rc=dbrc)
        else
          call ESMF_LogWrite(trim(subname)//' swd:did not find 4 net sw for ocn', ESMF_LOGMSG_INFO, rc=dbrc)
        endif
      endif

      !--- ocn down sw
      if (.not. checkOK1) then
        fieldname2(1) = 'mdvrsfx'
        fieldname2(2) = 'mdvfsfx'
        fieldname2(3) = 'mdirsfx'
        fieldname2(4) = 'mdifsfx'

        checkOK1 = .true.
        do n = 1,4
          checkOK1 = checkOK1 .and. FieldBundle_FldChk(is%wrap%FBforOcn, trim(fieldname2(n)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        enddo

        if (dbug_flag > 1) then
          if (checkOK1) then
            call ESMF_LogWrite(trim(subname)//' swd:found 4 down sw for ocn', ESMF_LOGMSG_INFO, rc=dbrc)
          else
            call ESMF_LogWrite(trim(subname)//' swd:did not find 4 down sw for ocn', ESMF_LOGMSG_INFO, rc=dbrc)
          endif
        endif
      endif
    endif

    if (checkOK .and. checkOK1) then
      !--- ocean albedo from atm
      fieldname3(1) = 'iovisdira'
      fieldname3(2) = 'iovisdifa'
      fieldname3(3) = 'ioirdira'
      fieldname3(4) = 'ioirdifa'
      checkOK2 = .true.
      do n = 1,4
        checkOK2 = checkOK2 .and. FieldBundle_FldChk(is%wrap%FBaccumAtm, trim(fieldname3(n)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      enddo

      if (dbug_flag > 1) then
        if (checkOK2) then
          call ESMF_LogWrite(trim(subname)//' swd:found 4 ocn albedos from atm', ESMF_LOGMSG_INFO, rc=dbrc)
        else
          call ESMF_LogWrite(trim(subname)//' swd:did not find 4 ocn albedos from atm', ESMF_LOGMSG_INFO, rc=dbrc)
        endif
      endif

      if (.not.checkOK2) then
        !--- use merged albedo from atm
        fieldname3(1) = 'ivisdira'
        fieldname3(2) = 'ivisdifa'
        fieldname3(3) = 'iirdira'
        fieldname3(4) = 'iirdifa'
        checkOK2 = .true.
        do n = 1,4
          checkOK2 = checkOK2 .and. FieldBundle_FldChk(is%wrap%FBaccumAtm, trim(fieldname3(n)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        enddo

        if (dbug_flag > 1) then
          if (checkOK2) then
            call ESMF_LogWrite(trim(subname)//' swd:found 4 avg albedos from atm', ESMF_LOGMSG_INFO, rc=dbrc)
          else
            call ESMF_LogWrite(trim(subname)//' swd:did not find 4 avg albedos from atm', ESMF_LOGMSG_INFO, rc=dbrc)
            call ESMF_LogWrite(trim(subname)//' swd:use ocean albedo 0.06', ESMF_LOGMSG_INFO, rc=dbrc)
          endif
        endif

      endif
    endif

    if (checkOK .and. checkOK1) then
! tcraig old single downward sw field
!      call FieldBundle_GetFldPtr(is%wrap%FBaccumAtm, trim(fieldname1(1)), dataPtr1, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do n = 1,4
        call FieldBundle_GetFldPtr(is%wrap%FBaccumAtm, trim(fieldname1(n)), dataPtr1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        call FieldBundle_GetFldPtr(is%wrap%FBforOcn, trim(fieldname2(n)), dataPtr2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        if (.not.FldPtr_SameCheck(dataPtr1, dataPtr2, 'swnet', rc)) then
          call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
          return
        endif

        if (checkOK2) then
          call FieldBundle_GetFldPtr(is%wrap%FBaccumAtm, trim(fieldname3(n)), dataPtr3, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          if (.not.FldPtr_SameCheck(dataPtr1, dataPtr3, 'swnet-albedo', rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
            return
          endif
        endif

        do j=lbound(dataPtr2,2),ubound(dataPtr2,2)
        do i=lbound(dataPtr2,1),ubound(dataPtr2,1)
          if (checkOK2) then
! tcraig old single downward sw field
!            dataPtr2(i,j) = dataPtr1(i,j) * (1.0 - dataPtr3(i,j)) * 0.25_ESMF_KIND_R8
            dataPtr2(i,j) = dataPtr1(i,j) * (1.0 - dataPtr3(i,j))
          else
            !--- hardwire 0.06 ocean albedo
! tcraig old single downward sw field
!            dataPtr2(i,j) = dataPtr1(i,j) * (1.0 - 0.06)          * 0.25_ESMF_KIND_R8
            dataPtr2(i,j) = dataPtr1(i,j) * (1.0 - 0.06)
          endif
        enddo
        enddo
      enddo
    else
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//' swd:failed', ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    !--- compute specific humidity flux from latent heat flux

    fieldname1(1) = 'mlhfx'
    fieldname2(1) = 'mevap'

    ! check fields exist
    checkOK = FieldBundle_FldChk(is%wrap%FBaccumAtm, trim(fieldname1(1)), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    checkOK = checkOK .and. FieldBundle_FldChk(is%wrap%FBforOcn, trim(fieldname2(1)), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (checkOK) then
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//' evap:compute mevap from mlhfx', ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call FieldBundle_GetFldPtr(is%wrap%FBaccumAtm, trim(fieldname1(1)), dataPtr1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      call FieldBundle_GetFldPtr(is%wrap%FBforOcn, trim(fieldname2(1)), dataPtr2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      if (.not.FldPtr_SameCheck(dataPtr1, dataPtr2, 'evap_from_mlhfx', rc)) then
         call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
         return
      endif

      do j=lbound(dataPtr2,2),ubound(dataPtr2,2)
      do i=lbound(dataPtr2,1),ubound(dataPtr2,1)
        dataPtr2(i,j) = dataPtr1(i,j) / const_lhvap !Lw is temperature dependent so more accurate calc can be done here.
      enddo
      enddo
    else
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//' evap:failed', ESMF_LOGMSG_INFO, rc=dbrc)
      endif
    endif

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBforOcn, trim(subname)//' FB4ocn_AFcc ', rc=rc)
    endif

    !---------------------------------------
    !--- zero accumulator
    !---------------------------------------

    is%wrap%accumcntAtm = 0
    call fieldBundle_reset(is%wrap%FBaccumAtm, value=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    is%wrap%accumcntIce = 0
    call fieldBundle_reset(is%wrap%FBaccumIce, value=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
!tcx      call FieldBundle_diagnose(is%wrap%FBaccumAtm, trim(subname)//' FBacc_AFzero ', rc=rc)
!tcx      call FieldBundle_diagnose(is%wrap%FBaccumIce, trim(subname)//' FBacc_AFzero ', rc=rc)
    endif

    !--- set export State to special value for testing

    call state_reset(exportState, value=-99._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(exportState, trim(subname)//' es_AF99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call fieldBundle_copy(exportState, is%wrap%FBforOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(exportState, trim(subname)//' es_AFcp ', rc=rc)
    endif

    ! write the fields exported to ocn to file
    call NUOPC_StateWrite(exportState, fieldNameList=fldsToOcn%shortname, &
      filePrefix="field_med_to_ocn_", timeslice=is%wrap%slowcntr, &
      relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !---------------------------------------

    is%wrap%slowcntr = is%wrap%slowcntr + 1

    !---------------------------------------
    !--- clean up
    !---------------------------------------

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Advance_slow

  !-----------------------------------------------------------------------------

  subroutine Finalize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(InternalState)  :: is
    integer              :: stat
    character(len=*),parameter :: subname='(module_MEDIATOR:Finalize)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
  
    ! Get the internal state from Component.
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! Destroy objects inside of internal state.
    ! TODO: destroy objects inside objects

    call fieldBundle_clean(is%wrap%FBaccumAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

! tcraig - generates errors
!    call fieldBundle_clean(is%wrap%FBaccumOcn, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

    call fieldBundle_clean(is%wrap%FBaccumIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_clean(is%wrap%FBforAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_clean(is%wrap%FBforOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_clean(is%wrap%FBforIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Deallocate the internal state memory.
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Finalize

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine fieldBundle_initFromFB(FBout, FBin, grid, name, rc)
    ! ----------------------------------------------
    ! Create FieldBundle from another FieldBundle.
    ! Zero out new FieldBundle
    ! If grid is not passed, use grid from FBin
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    type(ESMF_Grid)       , intent(in), optional :: grid
    character(len=*)      , intent(in), optional :: name
    integer               , intent(out),optional :: rc

    ! local variables
    integer                    :: i,j,n
    integer                    :: fieldCount
    character(len=64) ,pointer :: fieldNameList(:)
    type(ESMF_Field)           :: field
    type(ESMF_Grid)            :: lgrid
    character(len=64)          :: lname
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_initFromFB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS
      
    lname = 'undefined'
    if (present(name)) then
       lname = trim(name)
    endif

    call ESMF_FieldBundleGet(FBin, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FBin, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (present(grid)) then
      call fieldBundle_init(FBout, fieldNameList=fieldNameList, grid=grid, name=trim(lname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    else
      call ESMF_FieldBundleGet(FBin, grid=lgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call fieldBundle_init(FBout, fieldNameList=fieldNameList, grid=lgrid, name=trim(lname), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    endif
    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldbundle_initFromFB

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_init(FieldBundle, fieldNameList, grid, State, name, rc)
    ! ----------------------------------------------
    ! Create FieldBundle from fieldNameList and grid
    ! If State if present, only include fields that are 
    !   in both fieldNameList and State
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    character(len=*)      , intent(in)    :: fieldNameList(:)
    type(ESMF_Grid)       , intent(in)    :: grid
    type(ESMF_State)      , intent(in), optional  :: State  ! check if fieldnames are there
    character(len=*)      , intent(in), optional  :: name
    integer               , intent(out),optional  :: rc

    ! local variables
    integer                    :: i,j,n
    logical                    :: doadd
    character(len=64)          :: lname
    type(ESMF_Field)           :: field
    type(ESMF_StateItem_Flag)  :: itemType
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_init)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    lname = 'undefined'
    if (present(name)) then
       lname = trim(name)
    endif

    FieldBundle = ESMF_FieldBundleCreate(name=trim(lname), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    do n=1, size(fieldNameList)
      doadd = .true.
      if (present(State)) then
        call ESMF_StateGet(State, itemName=fieldNameList(n), itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        doadd = (itemType /= ESMF_STATEITEM_NOTFOUND)
      endif
      if (doadd) then
        field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=fieldNameList(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_FieldBundleAdd(FieldBundle, (/field/), rc=rc)
        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//":"//trim(lname)//":add  "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif
      else
        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//":"//trim(lname)//":skip "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif
      endif  ! doadd
    enddo  ! fieldNameList

    call fieldBundle_reset(FieldBundle, value=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
  end subroutine fieldBundle_init

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_clean(FieldBundle, rc)
    ! ----------------------------------------------
    ! Destroy fields in FieldBundle and FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    integer, intent(out), optional  :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(len=64) ,pointer  :: fieldNameList(:)
    type(ESMF_Field)            :: field
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_clean)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    do n = 1, fieldCount
      call ESMF_FieldBundleGet(FieldBundle, fieldName=fieldNameList(n), field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call ESMF_FieldDestroy(field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    enddo
    call ESMF_FieldBundleDestroy(FieldBundle, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_clean

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_reset(FieldBundle, value, rc)
    ! ----------------------------------------------
    ! Set all fields to value in FieldBundle
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    real(ESMF_KIND_R8), intent(in), optional :: value
    integer, intent(out), optional :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(len=64) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8)          :: lvalue
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_reset)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    lvalue = 0._ESMF_KIND_R8
    if (present(value)) then
      lvalue = value
    endif

    call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    do n = 1, fieldCount
      call FieldBundle_GetFldPtr(FieldBundle, fieldNameList(n), dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do j=lbound(dataPtr,2),ubound(dataPtr,2)
      do i=lbound(dataPtr,1),ubound(dataPtr,1)
        dataPtr(i,j) = lvalue
      enddo
      enddo

    enddo
    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_reset

  !-----------------------------------------------------------------------------

  subroutine state_reset(State, value, rc)
    ! ----------------------------------------------
    ! Set all fields to value in State
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(ESMF_State), intent(inout) :: State
    real(ESMF_KIND_R8), intent(in), optional :: value
    integer, intent(out), optional  :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(len=64) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8)          :: lvalue
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:state_reset)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    lvalue = 0._ESMF_KIND_R8
    if (present(value)) then
      lvalue = value
    endif

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    do n = 1, fieldCount
      call State_GetFldPtr(State, fieldNameList(n), dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do j=lbound(dataPtr,2),ubound(dataPtr,2)
      do i=lbound(dataPtr,1),ubound(dataPtr,1)
        dataPtr(i,j) = lvalue
      enddo
      enddo

    enddo
    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine state_reset

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_average(FieldBundle, count, rc)
    ! ----------------------------------------------
    ! Set all fields to zero in FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    integer               , intent(in)    :: count
    integer, intent(out), optional  :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(len=64) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_average)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    if (count == 0) then

      call ESMF_LogWrite(trim(subname)//": WARNING count is 0", ESMF_LOGMSG_INFO, rc=dbrc)

    else

      call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      allocate(fieldNameList(fieldCount))
      call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      do n = 1, fieldCount
        call FieldBundle_GetFldPtr(FieldBundle, fieldNameList(n), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        do j=lbound(dataPtr,2),ubound(dataPtr,2)
        do i=lbound(dataPtr,1),ubound(dataPtr,1)
          dataPtr(i,j) = dataPtr(i,j) / real(count, ESMF_KIND_R8)
        enddo
        enddo
      enddo
      deallocate(fieldNameList)

    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_average

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_diagnose(FieldBundle, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of fieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    character(len=*), intent(in), optional :: string
    integer, intent(out), optional  :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(len=64) ,pointer  :: fieldNameList(:)
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_diagnose)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    do n = 1, fieldCount
      call FieldBundle_GetFldPtr(FieldBundle, fieldNameList(n), dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      write(tmpstr,'(A,3g14.7)') trim(subname)//' '//trim(lstring)//':'//trim(fieldNameList(n)), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_diagnose

  !-----------------------------------------------------------------------------

  subroutine state_diagnose(State, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of fieldBundle
    ! ----------------------------------------------
    type(ESMF_State), intent(inout) :: State
    character(len=*), intent(in), optional :: string
    integer, intent(out), optional  :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(len=64) ,pointer  :: fieldNameList(:)
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:state_diagnose)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    do n = 1, fieldCount
      call State_GetFldPtr(State, fieldNameList(n), dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      write(tmpstr,'(A,3g14.7)') trim(subname)//' '//trim(lstring)//':'//trim(fieldNameList(n)), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine state_diagnose

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_copyFB2FB(FBout, FBin, rc)
    ! ----------------------------------------------
    ! Copy common field names from FBin to FBout
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    integer, intent(out), optional  :: rc

    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_copyFB2FB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    call fieldBundle_accum(FBout, FBin, copy=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_copyFB2FB

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_copyFB2ST(STout, FBin, rc)
    ! ----------------------------------------------
    ! Copy common field names from FBin to STout
    ! ----------------------------------------------
    type(ESMF_State)      , intent(inout) :: STout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    integer, intent(out), optional  :: rc

    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_copyFB2ST)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    call fieldBundle_accum(STout, FBin, copy=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_copyFB2ST

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_copyST2FB(FBout, STin, rc)
    ! ----------------------------------------------
    ! Copy common field names from STin to FBout
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_State)      , intent(in)    :: STin
    integer, intent(out), optional  :: rc

    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_copyST2FB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    call fieldBundle_accum(FBout, STin, copy=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_copyST2FB

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_accumFB2FB(FBout, FBin, copy, rc)
    ! ----------------------------------------------
    ! Accumulate common field names from FBin to FBout
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    logical, intent(in) , optional  :: copy
    integer, intent(out), optional  :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(len=64) ,pointer  :: fieldNameList(:)
    logical                     :: exists
    logical                     :: lcopy
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtri(:,:), dataPtro(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_accumFB2FB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    lcopy = .false.  ! accumulate by default
    if (present(copy)) then
      lcopy = copy
    endif

    call ESMF_FieldBundleGet(FBout, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FBout, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    do n = 1, fieldCount
      call ESMF_FieldBundleGet(FBin, fieldName=fieldNameList(n), isPresent=exists, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      if (exists) then
        call FieldBundle_GetFldPtr(FBin,  fieldNameList(n), dataPtri, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call FieldBundle_GetFldPtr(FBout, fieldNameList(n), dataPtro, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        if (.not.FldPtr_SameCheck(dataPtro, dataPtri, subname, rc)) then
           call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
           return
        endif

        if (lcopy) then
          do j=lbound(dataPtri,2),ubound(dataPtri,2)
          do i=lbound(dataPtri,1),ubound(dataPtri,1)
            dataPtro(i,j) = dataPtri(i,j)
          enddo
          enddo
        else
          do j=lbound(dataPtri,2),ubound(dataPtri,2)
          do i=lbound(dataPtri,1),ubound(dataPtri,1)
            dataPtro(i,j) = dataPtro(i,j) + dataPtri(i,j)
          enddo
          enddo
        endif

      endif
    enddo

    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_accumFB2FB
  !-----------------------------------------------------------------------------

  subroutine fieldBundle_accumST2FB(FBout, STin, copy, rc)
    ! ----------------------------------------------
    ! Accumulate common field names from State to FieldBundle
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    type(ESMF_State)      , intent(in)    :: STin
    logical, intent(in) , optional :: copy
    integer, intent(out), optional :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    logical                     :: lcopy
    character(len=64) ,pointer  :: fieldNameList(:)
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtrS(:,:), dataPtrB(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_accumST2FB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    lcopy = .false.
    if (present(copy)) then
      lcopy = copy
    endif

    call ESMF_FieldBundleGet(FBout, fieldCount=fieldCount, rc=rc)
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FBout, fieldNameList=fieldNameList, rc=rc)
    do n = 1, fieldCount
      call ESMF_StateGet(STin, itemName=fieldNameList(n), itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then

        call State_GetFldPtr(STin, fieldNameList(n), dataPtrS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call FieldBundle_GetFldPtr(FBout, fieldNameList(n), dataPtrB, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        if (.not.FldPtr_SameCheck(dataPtrS, dataPtrB, subname, rc)) then
           call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
           return
        endif

        if (lcopy) then
          do j=lbound(dataPtrB,2),ubound(dataPtrB,2)
          do i=lbound(dataPtrB,1),ubound(dataPtrB,1)
            dataPtrB(i,j) = dataPtrS(i,j)
          enddo
          enddo
        else
          do j=lbound(dataPtrB,2),ubound(dataPtrB,2)
          do i=lbound(dataPtrB,1),ubound(dataPtrB,1)
            dataPtrB(i,j) = dataPtrB(i,j) + dataPtrS(i,j)
          enddo
          enddo
        endif

      endif  ! statefound
    enddo  ! fieldCount

    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_accumST2FB

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_accumFB2ST(STout, FBin, copy, rc)
    ! ----------------------------------------------
    ! Accumulate common field names from FieldBundle to State
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(ESMF_State)      , intent(inout) :: STout
    type(ESMF_FieldBundle), intent(in)    :: FBin
    logical, intent(in) , optional :: copy
    integer, intent(out), optional :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    logical                     :: lcopy
    character(len=64) ,pointer  :: fieldNameList(:)
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtrS(:,:), dataPtrB(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_accumFB2ST)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    lcopy = .false.
    if (present(copy)) then
      lcopy = copy
    endif

    call ESMF_FieldBundleGet(FBin, fieldCount=fieldCount, rc=rc)
    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FBin, fieldNameList=fieldNameList, rc=rc)
    do n = 1, fieldCount
      call ESMF_StateGet(STout, itemName=fieldNameList(n), itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then

        call FieldBundle_GetFldPtr(FBin, fieldNameList(n), dataPtrB, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call State_GetFldPtr(STout, fieldNameList(n), dataPtrS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        if (.not.FldPtr_SameCheck(dataPtrS, dataPtrB, subname, rc)) then
           call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
           return
        endif

        if (lcopy) then
          do j=lbound(dataPtrB,2),ubound(dataPtrB,2)
          do i=lbound(dataPtrB,1),ubound(dataPtrB,1)
            dataPtrS(i,j) = dataPtrB(i,j)
          enddo
          enddo
        else
          do j=lbound(dataPtrB,2),ubound(dataPtrB,2)
          do i=lbound(dataPtrB,1),ubound(dataPtrB,1)
            dataPtrS(i,j) = dataPtrS(i,j) + dataPtrB(i,j)
          enddo
          enddo
        endif

      endif  ! statefound
    enddo  ! fieldCount

    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_accumFB2ST

  !-----------------------------------------------------------------------------

  logical function FieldBundle_FldChk(FB, fldname, rc)
    type(ESMF_FieldBundle), intent(in) :: FB
    character(len=*)      ,intent(in) :: fldname
    integer, intent(out), optional :: rc

    ! local variables
    logical :: isPresent
    integer :: lrc
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FldChk)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    FieldBundle_FldChk = .false.

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), isPresent=isPresent, rc=lrc)
    if (present(rc)) rc = lrc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (isPresent) then
       FieldBundle_FldChk = .true.
    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function FieldBundle_FldChk

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_GetFldPtr(FB, fldname, fldptr, rc)
    type(ESMF_FieldBundle), intent(in) :: FB
    character(len=*)      , intent(in) :: fldname
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr(:,:)
    integer, intent(out), optional :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer :: lrc
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_GetFldPtr)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), field=lfield, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (present(rc)) rc = lrc

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FieldBundle_GetFldPtr

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine State_GetFldPtr(ST, fldname, fldptr, rc)
    type(ESMF_State), intent(in) :: ST
    character(len=*), intent(in) :: fldname
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr(:,:)
    integer, intent(out), optional :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer :: lrc
    character(len=*),parameter :: subname='(module_MEDIATOR:State_GetFldPtr)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_StateGet(ST, itemName=trim(fldname), field=lfield, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (present(rc)) rc = lrc

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine State_GetFldPtr

  !-----------------------------------------------------------------------------

  logical function FldPtr_SameCheck(fldptr1, fldptr2, cstring, rc)
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr1(:,:)
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr2(:,:)
    character(len=*), intent(in) :: cstring
    integer, intent(out), optional :: rc

    ! local variables
    integer :: lrc
    character(len=*),parameter :: subname='(module_MEDIATOR:FldPtr_SameCheck)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    if (present(rc)) rc = ESMF_SUCCESS

    FldPtr_SameCheck = .false.
    if (lbound(fldptr2,2) /= lbound(fldptr1,2) .or. &
        lbound(fldptr2,1) /= lbound(fldptr1,1) .or. &
        ubound(fldptr2,2) /= ubound(fldptr1,2) .or. &
        ubound(fldptr2,1) /= ubound(fldptr1,1)) then
      call ESMF_LogWrite(trim(subname)//": ERROR in data size "//trim(cstring), ESMF_LOGMSG_ERROR, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      write(tmpstr,*) trim(subname)//': fldptr2 ',lbound(fldptr2),ubound(fldptr2)
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
      write(tmpstr,*) trim(subname)//': fldptr1 ',lbound(fldptr1),ubound(fldptr1)
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    else
      FldPtr_SameCheck = .true.
    endif

    if (present(rc)) rc = lrc

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function FldPtr_SameCheck

  !-----------------------------------------------------------------------------

  subroutine fld_list_add(fldlist, stdname, shortname, longname, units, &
    transferOffer)
    ! ----------------------------------------------
    ! Accumulate common field names from FieldBundle to State
    ! If copy is passed in and true, the this is a copy
    ! ----------------------------------------------
    type(fld_list_type),intent(inout) :: fldlist
    character(len=*), intent(in) :: stdname
    character(len=*), intent(in) :: shortname
    character(len=*), intent(in) :: longname
    character(len=*), intent(in) :: units
    character(len=*), intent(in) :: transferOffer

    ! local variables
    integer :: cnum    ! current size of array
    integer :: nnum    ! new size of array
    integer :: rc
    character(len=256), pointer :: tmpstr(:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fld_list_add)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    if (fldlist%num < 0) then
       nnum = 10
       fldlist%num = 0
       allocate(fldlist%stdname(nnum))
       allocate(fldlist%shortname(nnum))
       allocate(fldlist%longname(nnum))
       allocate(fldlist%units(nnum))
       allocate(fldlist%transferOffer(nnum))
    endif

    cnum = size(fldlist%stdname)
    if (fldlist%num > cnum) then
      call ESMF_LogWrite(trim(subname)//": ERROR in num for fld "//trim(stdname), ESMF_LOGMSG_ERROR, rc=rc)
      return
    endif
    if (fldlist%num == cnum) then
      nnum = cnum + 10
      allocate(tmpstr(cnum))
      tmpstr(1:cnum) = fldlist%stdname(1:cnum)
      deallocate(fldlist%stdname)
      allocate(fldlist%stdname(nnum))
      fldlist%stdname(1:cnum) = tmpstr(1:cnum)
      tmpstr(1:cnum) = fldlist%shortname(1:cnum)
      deallocate(fldlist%shortname)
      allocate(fldlist%shortname(nnum))
      fldlist%shortname(1:cnum) = tmpstr(1:cnum)
      tmpstr(1:cnum) = fldlist%longname(1:cnum)
      deallocate(fldlist%longname)
      allocate(fldlist%longname(nnum))
      fldlist%longname(1:cnum) = tmpstr(1:cnum)
      tmpstr(1:cnum) = fldlist%units(1:cnum)
      deallocate(fldlist%units)
      allocate(fldlist%units(nnum))
      fldlist%units(1:cnum) = tmpstr(1:cnum)
      tmpstr(1:cnum) = fldlist%transferOffer(1:cnum)
      deallocate(fldlist%transferOffer)
      allocate(fldlist%transferOffer(nnum))
      fldlist%transferOffer(1:cnum) = tmpstr(1:cnum)
      deallocate(tmpstr)
    endif

    fldlist%num = fldlist%num + 1
    fldlist%stdname       (fldlist%num) = trim(stdname)
    fldlist%shortname     (fldlist%num) = trim(shortname)
    fldlist%longname      (fldlist%num) = trim(longname)
    fldlist%units         (fldlist%num) = trim(units)
    fldlist%transferOffer (fldlist%num) = trim(transferOffer)

!--------------------------------------------------
! This automatically adds dictionary entries
! Want fields to be defined in the global dictionary
!--------------------------------------------------
!    if (.not.NUOPC_FieldDictionaryHasEntry(trim(stdname))) then
!       call NUOPC_FieldDictionaryAddEntry( &
!          standardName     = trim(stdname)   , &
!          canonicalUnits   = trim(units)     , &
!          defaultLongName  = trim(longname)  , &
!          defaultShortName = trim(shortname) , rc=rc);
!       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!          line=__LINE__, &
!          file=__FILE__)) &
!          return  ! bail out
!    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fld_list_add

  !-----------------------------------------------------------------------------

end module
#endif
