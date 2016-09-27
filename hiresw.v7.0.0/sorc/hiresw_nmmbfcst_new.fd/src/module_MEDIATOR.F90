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
    mediator_routine_SS             => SetServices, &
    mediator_routine_Run            => routine_Run, &
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
    type(ESMF_FieldBundle):: FBAtm_a     ! Atm export data on atm grid
    type(ESMF_FieldBundle):: FBAtm_o     ! Atm export data mapped to ocn grid
    type(ESMF_FieldBundle):: FBAtm_i     ! Atm export data mapped to ice grid
    type(ESMF_FieldBundle):: FBOcn_a     ! Ocn export data mapped to atm grid
    type(ESMF_FieldBundle):: FBOcn_o     ! Ocn export data on ocn grid
    type(ESMF_FieldBundle):: FBOcn_i     ! Ocn export data mapped to ice grid
    type(ESMF_FieldBundle):: FBIce_a     ! Ice export data mapped to atm grid
    type(ESMF_FieldBundle):: FBIce_o     ! Ice export data mapped to ocn grid
    type(ESMF_FieldBundle):: FBIce_i     ! Ice export data on ice grid
    type(ESMF_FieldBundle):: FBforAtm    ! data storage for atm import
    type(ESMF_FieldBundle):: FBforOcn    ! data storage for ocn import
    type(ESMF_FieldBundle):: FBforIce    ! data storage for ice import
    type(ESMF_RouteHandle):: RH_a2o_bilnr  ! atm to ocn bilinear
    type(ESMF_RouteHandle):: RH_o2a_bilnr  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_bilnr  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_bilnr  ! ice to atm
    type(ESMF_RouteHandle):: RH_o2i_bilnr  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_bilnr  ! ice to ocn
    type(ESMF_RouteHandle):: RH_a2o_consv  ! atm to ocn conservative
    type(ESMF_RouteHandle):: RH_o2a_consv  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_consv  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_consv  ! ice to atm
    type(ESMF_RouteHandle):: RH_o2i_consv  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_consv  ! ice to ocn
    type(ESMF_RouteHandle):: RH_a2o_patch  ! atm to ocn patch
    type(ESMF_RouteHandle):: RH_o2a_patch  ! ocn to atm
    type(ESMF_RouteHandle):: RH_a2i_patch  ! atm to ice
    type(ESMF_RouteHandle):: RH_i2a_patch  ! ice to atm
    type(ESMF_RouteHandle):: RH_o2i_patch  ! ocn to ice
    type(ESMF_RouteHandle):: RH_i2o_patch  ! ice to ocn
    logical               :: a2o_active
    logical               :: o2a_active
    logical               :: a2i_active
    logical               :: i2a_active
    logical               :: o2i_active
    logical               :: i2o_active
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
  type(ESMF_Grid)    :: gridAtm, gridOcn, gridIce, gridMed
  integer, parameter :: nx_med=400, ny_med=200
  integer, parameter :: dbug_flag = 5
  integer            :: dbrc
  character(len=256) :: msgString
  logical            :: isPresent
  real(ESMF_KIND_R8), parameter :: const_lhvap = 2.501e6_ESMF_KIND_R8  ! latent heat of evaporation ~ J/kg
  integer            :: SrcTermProcessing_Value = 0

  type fld_list_type
    integer :: num = -1
    character(len=64), pointer :: stdname(:)
    character(len=64), pointer :: shortname(:)
    character(len=64), pointer :: transferOffer(:)
    character(len=64), pointer :: mapping(:)
  end type fld_list_type
  type(ESMF_State)     :: NState_AtmImp   ! Atm Import nested state
  type(ESMF_State)     :: NState_OcnImp   ! Ocn Import nested state
  type(ESMF_State)     :: NState_IceImp   ! Ice Import nested state
  type(ESMF_State)     :: NState_AtmExp   ! Atm Export nested state
  type(ESMF_State)     :: NState_OcnExp   ! Ocn Export nested state
  type(ESMF_State)     :: NState_IceExp   ! Ice Export nested state
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
    
    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! IPDv03p1: advertise Fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeIPDv03p1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! IPDv03p3: realize connected Fields with transfer action "provide"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeIPDv03p3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeIPDv03p4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! IPDv03p5: realize all Fields with transfer action "accept"
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeIPDv03p5, rc=rc)
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

    ! overwrite Finalize
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE, &
      userRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for Run( phase = slow ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_slow"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specPhaseLabel="MedPhase_slow", specRoutine=SetRunClock_ocn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_slow", specRoutine=Advance_slow, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for Run( phase = fast_before ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_fast_before"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specPhaseLabel="MedPhase_fast_before", &
      specRoutine=SetRunClock_atm_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_CheckImport, &
      specPhaseLabel="MedPhase_fast_before", &
      specRoutine=CheckImport_atm_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="MedPhase_fast_before", &
      specRoutine=TimestampExport_atm_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_fast_before", &
      specRoutine=Advance_fast_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for Run( phase = fast_after ) and specialize
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_RUN, &
      phaseLabelList=(/"MedPhase_fast_after"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_SetRunClock, &
      specPhaseLabel="MedPhase_fast_after", &
      specRoutine=SetRunClock_atm_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_CheckImport, &
      specPhaseLabel="MedPhase_fast_after", &
      specRoutine=CheckImport_atm_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="MedPhase_fast_after", &
      specRoutine=TimestampExport_atm_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(gcomp, specLabel=mediator_label_Advance, &
      specPhaseLabel="MedPhase_fast_after", &
      specRoutine=Advance_fast_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Fields to ATM
    call fld_list_add(fldsToAtm,"land_mask"               , "cannot provide")
    call fld_list_add(fldsToAtm,"sea_surface_temperature" , "cannot provide")
    call fld_list_add(fldsToAtm,"inst_ice_ir_dir_albedo"  , "cannot provide")
    call fld_list_add(fldsToAtm,"inst_ice_ir_dif_albedo"  , "cannot provide")
    call fld_list_add(fldsToAtm,"inst_ice_vis_dir_albedo" , "cannot provide")
    call fld_list_add(fldsToAtm,"inst_ice_vis_dif_albedo" , "cannot provide")
    call fld_list_add(fldsToAtm,"ice_fraction"            , "cannot provide")
    call fld_list_add(fldsToAtm,"stress_on_air_ice_zonal" , "cannot provide")
    call fld_list_add(fldsToAtm,"stress_on_air_ice_merid" , "cannot provide")
    call fld_list_add(fldsToAtm,"mean_up_lw_flx_ice"      , "cannot provide")
    call fld_list_add(fldsToAtm,"mean_sensi_heat_flx_atm_into_ice", "cannot provide")
    call fld_list_add(fldsToAtm,"mean_laten_heat_flx_atm_into_ice", "cannot provide")
    call fld_list_add(fldsToAtm,"mean_evap_rate_atm_into_ice"     , "cannot provide")

    ! Fields from ATM
    call fld_list_add(fldsFrAtm,"mean_zonal_moment_flx"   , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_merid_moment_flx"   , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_sensi_heat_flx"     , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_laten_heat_flx"     , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_down_lw_flx"        , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_down_sw_flx"        , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_prec_rate"          , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_fprec_rate"         , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_zonal_moment_flx"   , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_merid_moment_flx"   , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_sensi_heat_flx"     , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_laten_heat_flx"     , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_down_lw_flx"        , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_down_sw_flx"        , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_temp_height2m"      , "cannot provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_spec_humid_height2m", "cannot provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_u_wind_height10m"   , "cannot provide","patch")
    call fld_list_add(fldsFrAtm,"inst_v_wind_height10m"   , "cannot provide","patch")
    call fld_list_add(fldsFrAtm,"inst_zonal_wind_height10m", "cannot provide","patch")
    call fld_list_add(fldsFrAtm,"inst_merid_wind_height10m", "cannot provide","patch")
    call fld_list_add(fldsFrAtm,"inst_temp_height_surface", "cannot provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_pres_height_surface", "cannot provide","bilinear")
    call fld_list_add(fldsFrAtm,"inst_surface_height"     , "cannot provide","bilinear")
    ! new imports from GSM added 04/23/14:
    call fld_list_add(fldsFrAtm,"mean_net_lw_flx"         , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_net_sw_flx"         , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_net_lw_flx"         , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_net_sw_flx"         , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_down_sw_ir_dir_flx" , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_down_sw_ir_dif_flx" , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_down_sw_vis_dir_flx", "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_down_sw_vis_dif_flx", "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_down_sw_ir_dir_flx" , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_down_sw_ir_dif_flx" , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_down_sw_vis_dir_flx", "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_down_sw_vis_dif_flx", "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_net_sw_ir_dir_flx"  , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_net_sw_ir_dif_flx"  , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_net_sw_vis_dir_flx" , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"mean_net_sw_vis_dif_flx" , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_net_sw_ir_dir_flx"  , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_net_sw_ir_dif_flx"  , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_net_sw_vis_dir_flx" , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_net_sw_vis_dif_flx" , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_ir_dir_albedo"      , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_ir_dif_albedo"      , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_vis_dir_albedo"     , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_vis_dif_albedo"     , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_ocn_ir_dir_albedo"  , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_ocn_ir_dif_albedo"  , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_ocn_vis_dir_albedo" , "cannot provide","conserve")
    call fld_list_add(fldsFrAtm,"inst_ocn_vis_dif_albedo" , "cannot provide","conserve")

    ! Fields to OCN
    call fld_list_add(fldsToOcn,"mean_zonal_moment_flx"   , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_merid_moment_flx"   , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_sensi_heat_flx"     , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_laten_heat_flx"     , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_down_lw_flx"        , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_vis_dir_flx", "cannot provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_vis_dif_flx", "cannot provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_ir_dir_flx" , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_ir_dif_flx" , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_vis_dir_flx" , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_vis_dif_flx" , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_ir_dir_flx"  , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_ir_dif_flx"  , "cannot provide")
!   call fld_list_add(fldsToOcn,"mean_salt_flx"           , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_prec_rate"          , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_fprec_rate"         , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_evap_rate"          , "cannot provide")
!   call fld_list_add(fldsToOcn,"mean_runoff_rate"        , "cannot provide")
!   call fld_list_add(fldsToOcn,"mean_calving_rate"       , "cannot provide")
!   call fld_list_add(fldsToOcn,"mean_runoff_flx"         , "cannot provide")
!   call fld_list_add(fldsToOcn,"mean_calving_flx"        , "cannot provide")
    call fld_list_add(fldsToOcn,"inst_pres_height_surface", "cannot provide")
!   call fld_list_add(fldsToOcn,"mass_of_overlying_sea_ice, "cannot provide")
    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_zonal" , "cannot provide")
    call fld_list_add(fldsToOcn,"stress_on_ocn_ice_merid" , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_sw_pen_to_ocn"      , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_down_sw_flx"        , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_net_sw_flx"         , "cannot provide")
    call fld_list_add(fldsToOcn,"mean_net_lw_flx"         , "cannot provide")
    call fld_list_add(fldsToOcn,"inst_temp_height2m"      , "cannot provide")
    call fld_list_add(fldsToOcn,"inst_spec_humid_height2m", "cannot provide")
 
    ! Fields from OCN
    call fld_list_add(fldsFrOcn,"ocean_mask"              , "cannot provide","conserve")
    call fld_list_add(fldsFrOcn,"sea_surface_temperature" , "cannot provide","bilinear")
    call fld_list_add(fldsFrOcn,"s_surf"                  , "cannot provide","bilinear")
    call fld_list_add(fldsFrOcn,"u_surf"                  , "cannot provide","patch")
    call fld_list_add(fldsFrOcn,"v_surf"                  , "cannot provide","patch")
    call fld_list_add(fldsFrOcn,"ocn_current_zonal"       , "cannot provide","bilinear")
    call fld_list_add(fldsFrOcn,"ocn_current_merid"       , "cannot provide","bilinear")
    call fld_list_add(fldsFrOcn,"sea_lev"                 , "cannot provide","bilinear")
    call fld_list_add(fldsFrOcn,"upward_sea_ice_basal_available_heat_flux" &
                                                          , "cannot provide","conserve")
    call fld_list_add(fldsFrOcn,"mixed_layer_depth"       , "cannot provide","bilinear")
    call fld_list_add(fldsFrOcn,"sea_surface_slope_zonal" , "cannot provide","bilinear")
    call fld_list_add(fldsFrOcn,"sea_surface_slope_merid" , "cannot provide","bilinear")

    ! Fields to ICE
    call fld_list_add(fldsToIce,"dummyfield"               , "cannot provide")
    call fld_list_add(fldsToIce,"inst_temp_height2m"       , "cannot provide")
    call fld_list_add(fldsToIce,"inst_spec_humid_height2m" , "cannot provide")
    call fld_list_add(fldsToIce,"inst_zonal_wind_height10m", "cannot provide")
    call fld_list_add(fldsToIce,"inst_merid_wind_height10m", "cannot provide")
    call fld_list_add(fldsToIce,"inst_temp_height_surface" , "cannot provide")
    call fld_list_add(fldsToIce,"inst_surface_height"      , "cannot provide")
    call fld_list_add(fldsToIce,"inst_pres_height_surface" , "cannot provide")
    call fld_list_add(fldsToIce,"mean_down_lw_flx"         , "cannot provide")
    call fld_list_add(fldsToIce,"mean_down_sw_vis_dir_flx" , "cannot provide")
    call fld_list_add(fldsToIce,"mean_down_sw_vis_dif_flx" , "cannot provide")
    call fld_list_add(fldsToIce,"mean_down_sw_ir_dir_flx"  , "cannot provide")
    call fld_list_add(fldsToIce,"mean_down_sw_ir_dif_flx"  , "cannot provide")
    call fld_list_add(fldsToIce,"mean_prec_rate"           , "cannot provide")
    call fld_list_add(fldsToIce,"mean_fprec_rate"          , "cannot provide")
    call fld_list_add(fldsToIce,"sea_surface_temperature"  , "cannot provide")
    call fld_list_add(fldsToIce,"s_surf"                   , "cannot provide")
    call fld_list_add(fldsToIce,"sea_surface_slope_zonal"  , "cannot provide")
    call fld_list_add(fldsToIce,"sea_surface_slope_merid"  , "cannot provide")
    call fld_list_add(fldsToIce,"ocn_current_zonal"        , "cannot provide")
    call fld_list_add(fldsToIce,"ocn_current_merid"        , "cannot provide")
    call fld_list_add(fldsToIce,"freezing_melting_potential", "cannot provide")
    call fld_list_add(fldsToIce,"mixed_layer_depth"        , "cannot provide")

    ! Fields from ICE
    call fld_list_add(fldsFrIce,"dummyfield"              , "cannot provide","bilinear")
    call fld_list_add(fldsFrIce,"ice_mask"                , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"sea_ice_temperature"     , "cannot provide","bilinear")
    call fld_list_add(fldsFrIce,"inst_ice_ir_dir_albedo"  , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"inst_ice_ir_dif_albedo"  , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"inst_ice_vis_dir_albedo" , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"inst_ice_vis_dif_albedo" , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"ice_fraction"            , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"stress_on_air_ice_zonal" , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"stress_on_air_ice_merid" , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_zonal" , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"stress_on_ocn_ice_merid" , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"mean_sw_pen_to_ocn"      , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"mean_up_lw_flx_ice"      , "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"mean_sensi_heat_flx_atm_into_ice", "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"mean_laten_heat_flx_atm_into_ice", "cannot provide","conserve")
    call fld_list_add(fldsFrIce,"mean_evap_rate_atm_into_ice"     , "cannot provide","conserve")
 
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

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine InitializeP0

  !-----------------------------------------------------------------------

  subroutine InitializeIPDv03p1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables    
    integer :: n
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p1)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! importable fields:

    ! add a namespace
    call NUOPC_StateNamespaceAdd(importState, namespace="ATM", nestedStateName="NestedState-AtmImp", nestedState=NState_AtmImp, rc=rc)
    call NUOPC_StateNamespaceAdd(importState, namespace="OCN", nestedStateName="NestedState-OcnImp", nestedState=NState_OcnImp, rc=rc)
    call NUOPC_StateNamespaceAdd(importState, namespace="ICE", nestedStateName="NestedState-IceImp", nestedState=NState_IceImp, rc=rc)
    call NUOPC_StateNamespaceAdd(exportState, namespace="ATM", nestedStateName="NestedState-AtmExp", nestedState=NState_AtmExp, rc=rc)
    call NUOPC_StateNamespaceAdd(exportState, namespace="OCN", nestedStateName="NestedState-OcnExp", nestedState=NState_OcnExp, rc=rc)
    call NUOPC_StateNamespaceAdd(exportState, namespace="ICE", nestedStateName="NestedState-IceExp", nestedState=NState_IceExp, rc=rc)

    do n = 1,fldsFrAtm%num
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": Advertise "// &
          trim(fldsFrAtm%stdname(n))//":"// &
          trim(fldsFrAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(NState_AtmImp, &
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
        call ESMF_LogWrite(trim(subname)//": Advertise "// &
          trim(fldsFrOcn%stdname(n))//":"// &
          trim(fldsFrOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(NState_OcnImp, &
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
        call ESMF_LogWrite(trim(subname)//": Advertise "// &
          trim(fldsFrIce%stdname(n))//":"// &
          trim(fldsFrIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(NState_IceImp, &
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
        call ESMF_LogWrite(trim(subname)//": Advertise "// &
          trim(fldsToAtm%stdname(n))//":"// &
          trim(fldsToAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(NState_AtmExp, &
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
        call ESMF_LogWrite(trim(subname)//": Advertise "// &
          trim(fldsToOcn%stdname(n))//":"// &
          trim(fldsToOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(NState_OcnExp, &
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
        call ESMF_LogWrite(trim(subname)//": Advertise "// &
          trim(fldsToIce%stdname(n))//":"// &
          trim(fldsToIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call NUOPC_StateAdvertiseField(NState_IceExp, &
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

  end subroutine InitializeIPDv03p1
  
  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables    
    integer                     :: i, j
    real(kind=ESMF_KIND_R8),pointer :: lonPtr(:,:), latPtr(:,:)
    type(InternalState)         :: is
    integer                     :: stat
    real(ESMF_KIND_R8)          :: intervalSec
    type(ESMF_TimeInterval)     :: timeStep
    character(ESMF_MAXSTR)      :: transferAction
! tcx XGrid
!    type(ESMF_Field)            :: fieldX, fieldA, fieldO
!    type(ESMF_XGrid)            :: xgrid
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p3)'
    
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

    gridMed = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!    gridAtm = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
!      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
!      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)    
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

!    gridOcn = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
!      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
!      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)    
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

!    gridIce = NUOPC_GridCreateSimpleSph(0._ESMF_KIND_R8, -85._ESMF_KIND_R8, &
!      360._ESMF_KIND_R8, 85._ESMF_KIND_R8, nx_med, ny_med, &
!      scheme=ESMF_REGRID_SCHEME_FULL3D, rc=rc)    
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

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
    call realizeConnectedFields(NState_AtmImp, &
      fieldNameList=fldsFrAtm%shortname(1:fldsFrAtm%num), &
      string='AtmImp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Exportable fields to atm:

    call realizeConnectedFields(NState_AtmExp, &
      fieldNameList=fldsToAtm%shortname(1:fldsToAtm%num), &
      string='AtmExp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Importable fields from ocn:

    call realizeConnectedFields(NState_OcnImp, &
      fieldNameList=fldsFrOcn%shortname(1:fldsFrOcn%num), &
      string='OcnImp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Exportable fields to ocn:

    call realizeConnectedFields(NState_OcnExp, &
      fieldNameList=fldsToOcn%shortname(1:fldsToOcn%num), &
      string='OcnExp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Importable fields from ice:

    call realizeConnectedFields(NState_IceImp, &
      fieldNameList=fldsFrIce%shortname(1:fldsFrIce%num), &
      string='IceImp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- Exportable fields to ice:

    call realizeConnectedFields(NState_IceExp, &
      fieldNameList=fldsToIce%shortname(1:fldsToIce%num), &
      string='IceExp',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
    subroutine realizeConnectedFields(state, fieldNameList, grid, string, rc)
      type(ESMF_State)                :: state
      character(len=*)                :: fieldNameList(:)
      type(ESMF_Grid),optional        :: grid
      character(len=*)                :: string
      integer, intent(out)            :: rc

      integer                         :: n
      type(ESMF_Field)                :: field
      character(len=*),parameter :: subname='(module_MEDIATOR:realizeConnectedFields)'

      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      rc = ESMF_SUCCESS
      
      do n=1, size(fieldNameList)
        if (NUOPC_StateIsFieldConnected(state, fieldName=fieldNameList(n))) then

          call ESMF_StateGet(state, field=field, itemName=fieldNameList(n), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out
          call NUOPC_FieldAttributeGet(field, name="TransferActionGeomObject", &
            value=transferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out

          if (trim(transferAction) == "accept") then
            if (dbug_flag > 1) then
              call ESMF_LogWrite(trim(subname)//trim(string)//" field+grid connected "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
            endif

          else   ! provide

            ! realize the connected Field using the internal coupling Field
            if (.not.present(grid)) then
              call ESMF_LogWrite(trim(subname)//trim(string)//": ERROR grid expected", ESMF_LOGMSG_INFO, rc=rc)
              rc = ESMF_FAILURE
              return
            endif
            field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=fieldNameList(n),rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call NUOPC_StateRealizeField(state, field=field, rc=rc)
            if (dbug_flag > 1) then
              call ESMF_LogWrite(trim(subname)//trim(string)//" field connected      "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
            endif

          endif   ! transferAction

        else   ! StateIsFieldConnected

          ! remove a not connected Field from State
          call ESMF_StateRemove(state, (/fieldNameList(n)/), rc=rc)
          if (dbug_flag > 1) then
            call ESMF_LogWrite(trim(subname)//trim(string)//" field NOT connected  "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
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

  end subroutine InitializeIPDv03p3
  
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p4(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Field)              :: field
    type(ESMF_Grid)               :: grid
    integer                       :: localDeCount

    type(ESMF_DistGrid)           :: distgrid
    integer                       :: dimCount, tileCount, petCount
    integer                       :: deCountPTile, extraDEs
    integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
    integer, allocatable          :: regDecompPTile(:,:)
    integer                       :: i, j, n, n1
    character(ESMF_MAXSTR)        :: transferAction
    
    character(len=*),parameter :: subname='(module_MEDIATOR:InitializeIPDv03p4)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    rc = ESMF_SUCCESS

    call realizeConnectedGrid(NState_atmImp, 'AtmImp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call realizeConnectedGrid(NState_atmExp, 'AtmExp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call realizeConnectedGrid(NState_ocnImp, 'OcnImp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call realizeConnectedGrid(NState_ocnExp, 'OcnExp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call realizeConnectedGrid(NState_iceImp, 'IceImp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call realizeConnectedGrid(NState_iceExp, 'IceExp', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine realizeConnectedGrid(State,string,rc)

      type(ESMF_State)   , intent(inout) :: State
      character(len=*)   , intent(in)    :: string
      integer            , intent(out)   :: rc
    
      ! local variables
      type(ESMF_Field)              :: field
      type(ESMF_Grid)               :: grid
      integer                       :: localDeCount

      type(ESMF_DistGrid)           :: distgrid
      integer                       :: dimCount, tileCount, petCount
      integer                       :: deCountPTile, extraDEs
      integer, allocatable          :: minIndexPTile(:,:), maxIndexPTile(:,:)
      integer, allocatable          :: regDecompPTile(:,:)
      integer                       :: i, j, n, n1, fieldCount
      character(ESMF_MAXSTR),allocatable :: fieldNameList(:)
      character(ESMF_MAXSTR)        :: transferAction
      character(len=*),parameter :: subname='(module_MEDIATOR:realizeConnectedGrid)'
    
      !NOTE: All fo the Fields that set their TransferOfferGeomObject Attribute
      !NOTE: to "cannot provide" should now have the accepted Grid available.
      !NOTE: Go and pull out this Grid for one of a representative Field and 
      !NOTE: modify the decomposition and distribution of the Grid to match the
      !NOTE: Mediator PETs.

      !TODO: quick implementation, do it for each field one by one
      !TODO: commented out below are application to other fields

      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      rc = ESMF_Success

      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do n=1, fieldCount
!tcx    do n=1, 1

        call ESMF_StateGet(State, field=field, itemName=fieldNameList(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return ! bail out
        call NUOPC_FieldAttributeGet(field, name="TransferActionGeomObject", &
          value=transferAction, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return ! bail out

        if (trim(transferAction) == "accept") then

          call ESMF_LogWrite(trim(subname)//trim(string)//": accept grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

          ! while this is still an empty field, it does now hold a Grid with DistGrid
          call ESMF_FieldGet(field, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          call Grid_Print(grid,trim(fieldNameList(n))//'_orig',rc)
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

          ! local clean-up
          deallocate(minIndexPTile, maxIndexPTile, regDecompPTile)

          ! Swap all the Grids in the State
    
!tcx         do n1=1, fieldCount
          do n1=n,n
            ! access a field in the importState and set the Grid
            call ESMF_StateGet(State, field=field, &
              itemName=fieldNameList(n1), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call ESMF_LogWrite(trim(subname)//trim(string)//": attach grid for "//trim(fieldNameList(n1)), ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call Grid_print(grid,trim(fieldNameList(n))//'_new',rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          enddo

        else

          call ESMF_LogWrite(trim(subname)//trim(string)//": provide grid for "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)

        endif   ! accept

      enddo   ! nflds

      deallocate(fieldNameList)

      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    end subroutine realizeConnectedGrid

  end subroutine InitializeIPDv03p4
  
  !-----------------------------------------------------------------------------

  subroutine InitializeIPDv03p5(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Field)            :: field, field1, field2
    type(ESMF_Grid)             :: grid
    type(InternalState)         :: is
    integer                     :: fieldCount
    character(ESMF_MAXSTR)      :: name
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    character(len=*),parameter  :: subname='(module_MEDIATOR:InitializeIPDv03p5)'
    
    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    
    rc = ESMF_SUCCESS

    !----------------------------------------------------------
    !--- Finish initializing the State Fields
    !----------------------------------------------------------

    call completeFieldInitialization(NState_atmImp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call completeFieldInitialization(NState_atmExp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call completeFieldInitialization(NState_ocnImp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call completeFieldInitialization(NState_ocnExp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call completeFieldInitialization(NState_iceImp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call completeFieldInitialization(NState_iceExp, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    !----------------------------------------------------------
    !--- Set the model grids using first field in each model's import state
    !----------------------------------------------------------

!tcraig old version
!    call ESMF_StateGet(NState_atmImp, field=field, itemName=fldsFrAtm%shortname(1), rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return ! bail out

!    call ESMF_FieldGet(field, grid=gridAtm, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

!    call ESMF_StateGet(NState_ocnImp, field=field, itemName=fldsFrOcn%shortname(1), rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return ! bail out

!    call ESMF_FieldGet(field, grid=gridOcn, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

    call ESMF_StateGet(NState_atmImp, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_atmImp, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return
      call ESMF_StateGet(NState_atmImp, field=field, itemName=fieldNameList(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out
      call ESMF_FieldGet(field, grid=gridAtm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      deallocate(fieldNameList)
    else
      gridAtm = gridMed
    endif

    call ESMF_StateGet(NState_ocnImp, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_ocnImp, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return
      call ESMF_StateGet(NState_ocnImp, field=field, itemName=fieldNameList(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out
      call ESMF_FieldGet(field, grid=gridOcn, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      deallocate(fieldNameList)
    else
      gridOcn = gridMed
    endif

    call ESMF_StateGet(NState_iceImp, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return
    if (fieldCount > 0) then
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(NState_iceImp, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return
      call ESMF_StateGet(NState_iceImp, field=field, itemName=fieldNameList(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out
      call ESMF_FieldGet(field, grid=gridIce, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      deallocate(fieldNameList)
    else
      gridIce = gridMed
    endif

    !----------------------------------------------------------
    !--- Diagnose Grid Info
    !----------------------------------------------------------

    call Grid_Print(gridAtm,'gridAtm',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call Grid_Print(gridOcn,'gridOcn',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call Grid_Print(gridIce,'gridIce',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    call Grid_Print(gridMed,'gridMed',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

#if 1
    !----------------------------------------------------------
    ! dump the Grid coordinate arrays for reference      
    !----------------------------------------------------------

    call Grid_Write(gridAtm, 'array_med_atm', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call Grid_Write(gridOcn, 'array_med_ocn', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call Grid_Write(gridIce, 'array_med_ice', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call Grid_Write(gridMed, 'array_med_med', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#endif


    !----------------------------------------------------------
    ! NOW allocate other Mediator datatypes
    !----------------------------------------------------------

    ! Get the internal state from Component.
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !----------------------------------------------------------
    ! Initialize FB for each model import states on each grid
    !----------------------------------------------------------

    !--- atm

    call fieldBundle_init(is%wrap%FBAtm_a, grid=gridAtm, &
      state=NState_AtmImp, name='FBAtm_a', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBAtm_o, grid=gridOcn, &
      state=NState_AtmImp, name='FBAtm_o', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBAtm_i, grid=gridIce, &
      state=NState_AtmImp, name='FBAtm_i', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- ocn

    call fieldBundle_init(is%wrap%FBOcn_a, grid=gridAtm, &
      state=NState_OcnImp, name='FBOcn_a', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBOcn_o, grid=gridOcn, &
      state=NState_OcnImp, name='FBOcn_o', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBOcn_i, grid=gridIce, &
      state=NState_OcnImp, name='FBOcn_i', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !--- ice

    call fieldBundle_init(is%wrap%FBIce_a, grid=gridAtm, &
      state=NState_IceImp, name='FBIce_a', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBIce_o, grid=gridOcn, &
      state=NState_IceImp, name='FBIce_o', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBIce_i, grid=gridIce, &
      state=NState_IceImp, name='FBIce_i', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !----------------------------------------------------------
    ! Initialize Accumulators
    !----------------------------------------------------------

    call fieldBundle_init(is%wrap%FBaccumAtm, grid=gridAtm, &
      state=NState_AtmImp, name='FBaccumAtm', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBaccumOcn, grid=gridOcn, &
      state=NState_OcnImp, name='FBaccumOcn', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBaccumIce, grid=gridIce, &
      state=NState_IceImp, name='FBaccumIce', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !----------------------------------------------------------
    ! Initialize FB for export to models
    !----------------------------------------------------------

    call fieldBundle_init(is%wrap%FBforAtm, &
      state=NState_AtmExp, name='FBforAtm', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBforOcn, &
      state=NState_OcnExp, name='FBforOcn', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_init(is%wrap%FBforIce, &
      state=NState_IceExp, name='FBforIce', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !----------------------------------------------------------
    !--- Check for active regrid directions
    !----------------------------------------------------------
    
    ! initialize
    is%wrap%a2o_active = .false.
    is%wrap%o2a_active = .false.
    is%wrap%a2i_active = .false.
    is%wrap%i2a_active = .false.
    is%wrap%o2i_active = .false.
    is%wrap%i2o_active = .false.

    ! a2o & o2a    
    call ESMF_FieldBundleGet(is%wrap%FBOcn_o, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is%wrap%FBOcn_a, fieldCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (fieldCount > 0) then
        is%wrap%a2o_active = .true.
        is%wrap%o2a_active = .true.
      endif
    endif
    
    ! a2i & i2a    
    call ESMF_FieldBundleGet(is%wrap%FBIce_i, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is%wrap%FBIce_a, fieldCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (fieldCount > 0) then
        is%wrap%a2i_active = .true.
        is%wrap%i2a_active = .true.
      endif
    endif
    
    ! o2i & i2o    
    call ESMF_FieldBundleGet(is%wrap%FBOcn_o, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (fieldCount > 0) then
      call ESMF_FieldBundleGet(is%wrap%FBOcn_i, fieldCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (fieldCount > 0) then
        is%wrap%o2i_active = .true.
        is%wrap%i2o_active = .true.
      endif
    endif
    
    !----------------------------------------------------------
    !--- Initialize route handles 
    !----------------------------------------------------------
    
    call ESMF_LogWrite("Starting to initialize RHs", ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()

    !--- a2o, o2a
    
    if (is%wrap%a2o_active) then

    call fieldBundle_getFieldN(is%wrap%FBOcn_o, 1, field1, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_getFieldN(is%wrap%FBOcn_a, 1, field2, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field2, field1, &
      routehandle=is%wrap%RH_a2o_bilnr, dstMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_FieldRegridStore(field2, field1, &
      routehandle=is%wrap%RH_a2o_consv, dstMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_FieldRegridStore(field2, field1, &
      routehandle=is%wrap%RH_a2o_patch, dstMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_PATCH, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Done RHs: a2o", ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()      
      
    call ESMF_FieldRegridStore(field1, field2, &
      routehandle=is%wrap%RH_o2a_bilnr, srcMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field1, field2, &
      routehandle=is%wrap%RH_o2a_consv, srcMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field1, field2, &
      routehandle=is%wrap%RH_o2a_patch, srcMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_PATCH, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Done RHs: o2a", ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()
    
    endif

    !--- a2i, i2a
    
    if (is%wrap%a2i_active) then

    call fieldBundle_getFieldN(is%wrap%FBIce_i, 1, field1, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_getFieldN(is%wrap%FBIce_a, 1, field2, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field2, field1, &
      routehandle=is%wrap%RH_a2i_bilnr, &
      regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field2, field1, &
      routehandle=is%wrap%RH_a2i_consv, &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field2, field1, &
      routehandle=is%wrap%RH_a2i_patch, &
      regridmethod=ESMF_REGRIDMETHOD_PATCH, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field1, field2, &
      routehandle=is%wrap%RH_i2a_bilnr, &
      regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field1, field2, &
      routehandle=is%wrap%RH_i2a_consv, &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field1, field2, &
      routehandle=is%wrap%RH_i2a_patch, &
      regridmethod=ESMF_REGRIDMETHOD_PATCH, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Done RHs: a2i, i2a", ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()
    
    endif

    ! i2o, o2i
    
    if (is%wrap%i2o_active) then

    call fieldBundle_getFieldN(is%wrap%FBOcn_o, 1, field1, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call fieldBundle_getFieldN(is%wrap%FBOcn_i, 1, field2, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field2, field1, &
      routehandle=is%wrap%RH_i2o_bilnr, dstMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field2, field1, &
      routehandle=is%wrap%RH_i2o_consv, dstMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field2, field1, &
      routehandle=is%wrap%RH_i2o_patch, dstMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_PATCH, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field1, field2, &
      routehandle=is%wrap%RH_o2i_bilnr, srcMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field1, field2, &
      routehandle=is%wrap%RH_o2i_consv, srcMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridStore(field1, field2, &
      routehandle=is%wrap%RH_o2i_patch, srcMaskValues=(/0/), &
      regridmethod=ESMF_REGRIDMETHOD_PATCH, &
      srcTermProcessing=srcTermProcessing_Value, &
      unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Done RHs: i2o, o2i", ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()
    
    endif

    !----------------------------------------------------------
    ! Print routehandles
    !----------------------------------------------------------
    
    if (is%wrap%a2o_active) then

    call ESMF_RouteHandlePrint(is%wrap%RH_a2o_bilnr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_a2o_consv, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_a2o_patch, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_RouteHandlePrint(is%wrap%RH_o2a_bilnr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_o2a_consv, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_o2a_patch, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    endif
    
    if (is%wrap%o2i_active) then

    call ESMF_RouteHandlePrint(is%wrap%RH_o2i_bilnr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_o2i_consv, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_o2i_patch, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_RouteHandlePrint(is%wrap%RH_i2o_bilnr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_i2o_consv, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_i2o_patch, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    endif
    
    if (is%wrap%a2i_active) then

    call ESMF_RouteHandlePrint(is%wrap%RH_i2a_bilnr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_i2a_consv, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_i2a_patch, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_RouteHandlePrint(is%wrap%RH_a2i_bilnr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_a2i_consv, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_RouteHandlePrint(is%wrap%RH_a2i_patch, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine completeFieldInitialization(State,rc)

      type(ESMF_State)   , intent(inout) :: State
      integer            , intent(out)   :: rc
    
      integer                     :: n, fieldCount
      character(ESMF_MAXSTR),allocatable :: fieldNameList(:)
      character(ESMF_MAXSTR)      :: transferAction
      character(len=*),parameter  :: subname='(module_MEDIATOR:completeFieldInitialization)'

      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

      rc = ESMF_Success

      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      allocate(fieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do n=1, fieldCount

        call ESMF_StateGet(State, field=field, itemName=fieldNameList(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return ! bail out
        call NUOPC_FieldAttributeGet(field, name="TransferActionGeomObject", &
          value=transferAction, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return ! bail out

        if (trim(transferAction) == "accept") then
          call ESMF_LogWrite(subname//" is accepting grid for field "//trim(fieldNameList(n)), &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return ! bail out
          ! the transferred Grid is already set, allocate field data memory
          call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif   ! accept

        call FldGrid_Print(field,fieldNameList(n),rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return ! bail out

      enddo

      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
      endif

    end subroutine completeFieldInitialization

  end subroutine InitializeIPDv03p5
  
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
    logical                     :: atCorrectTime, allDone, connected
    type(InternalState)         :: is
    character(len=*), parameter :: subname='(module_MEDIATOR:DataInitialize)'
    integer                     :: n

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
    
    ! initialze cumulative flag
    allDone = .true.  ! reset if an item is found that is not done
    
    ! check that all imported fields from ATM show correct timestamp
    do n = 1,fldsFrAtm%num
      call ESMF_StateGet(NState_AtmImp, itemName=fldsFrAtm%shortname(n), &
        itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        connected = NUOPC_StateIsFieldConnected(NState_AtmImp, &
          fieldName=fldsFrAtm%shortname(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (connected) then
          call ESMF_StateGet(NState_AtmImp, itemName=fldsFrAtm%shortname(n), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          atCorrectTime = NUOPC_FieldIsAtTime(field, time, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (.not.atCorrectTime) then
            call ESMF_LogWrite("MED - Initialize-Data-Dependency NOT YET SATISFIED!!!", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            allDone = .false.
            exit  ! break out of the loop when first not satisfied found
          else
            call ESMF_LogWrite("MED - Initialize-Data-Dependency SATISFIED!!!", &
              ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          endif
        endif
      endif
    enddo

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

      ! TODO - tcraig ?? what's above here?
      ! gjt: The above code ensures that the MED has initial conditions from ATM.
    
      ! TODO - For the real case this should probably use the fields from the
      ! importState and do something with it as a sensible starting point
      ! for the accumulation field so that the OCN receives a meaningful
      ! fields during its first time step. However, here for testing
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
    type(ESMF_Clock)        :: driverClock
    character(len=*),parameter :: subname='(module_MEDIATOR:SetRunClock_atm_before)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query component for the driver clock
    call NUOPC_MediatorGet(gcomp, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query component for its internal state
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! first time here must create clockAtm from driverClock
    if (.not.NUOPC_IsCreated(is_local%wrap%clockAtm)) then
      is_local%wrap%clockAtm = ESMF_ClockCreate(driverClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! set clockAtm to be the component clock
    call ESMF_GridCompSet(gcomp, clock=is_local%wrap%clockAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
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
    type(ESMF_Clock)        :: driverClock
    character(len=*),parameter :: subname='(module_MEDIATOR:SetRunClock_atm_after)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query component for the driver clock
    call NUOPC_MediatorGet(gcomp, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
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
    
    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
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
    type(ESMF_Clock)        :: driverClock
    character(len=*),parameter :: subname='(module_MEDIATOR:SetRunClock_ocn)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
    
    ! query component for the driver clock
    call NUOPC_MediatorGet(gcomp, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! query component for its internal state
    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! first time here must create clockOcn from driverClock
    if (.not.NUOPC_IsCreated(is_local%wrap%clockOcn)) then
      is_local%wrap%clockOcn = ESMF_ClockCreate(driverClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

    ! set clockOcn to be the component clock
    call ESMF_GridCompSet(gcomp, clock=is_local%wrap%clockOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(gcomp, driverClock, rc=rc)
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
    type(ESMF_Clock)            :: driverClock
    type(ESMF_Time)             :: startTime
    type(ESMF_State)            :: importState
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_Field)            :: field
    integer                     :: n
    logical                     :: atCorrectTime, connected
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
    call NUOPC_MediatorGet(gcomp, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! get the start time out of the driver Clock
    call ESMF_ClockGet(driverClock, startTime=startTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! check fields from OCN to be at startTime of the driver Clock
    do n = 1,fldsFrOcn%num
      call ESMF_StateGet(NState_OcnImp, itemName=fldsFrOcn%shortname(n), &
        itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        connected = NUOPC_StateIsFieldConnected(NState_OcnImp, &
          fieldName=fldsFrOcn%shortname(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (connected) then
          call ESMF_StateGet(NState_OcnImp, itemName=fldsFrOcn%shortname(n), &
            field=field, rc=rc)
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
      endif
    enddo

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
    type(ESMF_Clock)            :: clock
    type(ESMF_Time)             :: stopTime
    type(ESMF_State)            :: importState
    type(ESMF_StateItem_Flag)   :: itemType
    type(ESMF_Field)            :: field
    integer                     :: n
    logical                     :: atCorrectTime, connected
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
    do n = 1,fldsFrAtm%num
      call ESMF_StateGet(NState_AtmImp, itemName=fldsFrAtm%shortname(n), &
        itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        connected = NUOPC_StateIsFieldConnected(NState_AtmImp, &
          fieldName=fldsFrAtm%shortname(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (connected) then
          call ESMF_StateGet(NState_AtmImp, itemName=fldsFrAtm%shortname(n), &
            field=field, rc=rc)
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
      endif
    enddo

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
    call NUOPC_StateSetTimestamp(NState_AtmExp, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateSetTimestamp(NState_OcnExp, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateSetTimestamp(NState_IceExp, clock, rc=rc)
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
    type(ESMF_Field)            :: field
    type(InternalState)         :: is
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:)
    real(ESMF_KIND_R8)          :: masko, maski, maskoi, maskl
    integer                     :: i,j,n
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

    call fieldBundle_copy(is%wrap%FBAtm_a, NState_AtmImp, rc=rc)
    call fieldBundle_copy(is%wrap%FBOcn_o, NState_OcnImp, rc=rc)
    call fieldBundle_copy(is%wrap%FBIce_i, NState_IceImp, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBAtm_a, trim(subname)//' FBAtm_a ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBOcn_o, trim(subname)//' FBOcn_o ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBIce_i, trim(subname)//' FBIce_i ', rc=rc)
    endif

    ! Regrid Full Field Bundles conservatively

    call fieldBundle_reset(is%wrap%FBAtm_i, value=0._ESMF_KIND_R8, rc=rc)
    call fieldBundle_reset(is%wrap%FBOcn_i, value=0._ESMF_KIND_R8, rc=rc)
    call fieldBundle_reset(is%wrap%FBOcn_a, value=0._ESMF_KIND_R8, rc=rc)
    call fieldBundle_reset(is%wrap%FBIce_a, value=0._ESMF_KIND_R8, rc=rc)

    if (is%wrap%a2i_active) then

    call ESMF_FieldBundleRegrid(is%wrap%FBAtm_a, is%wrap%FBAtm_i, &
      routehandle=is%wrap%RH_a2i_consv, &
      termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    endif
    
    if (is%wrap%o2i_active) then

    call ESMF_FieldBundleRegrid(is%wrap%FBOcn_o, is%wrap%FBOcn_i, &
      routehandle=is%wrap%RH_o2i_consv, &
      termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    endif
    
    if (is%wrap%o2a_active) then

    call ESMF_FieldBundleRegrid(is%wrap%FBOcn_o, is%wrap%FBOcn_a, &
      routehandle=is%wrap%RH_o2a_consv, &
      termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    endif
    
    if (is%wrap%i2a_active) then

    call ESMF_FieldBundleRegrid(is%wrap%FBIce_i, is%wrap%FBIce_a, &
      routehandle=is%wrap%RH_i2a_consv, &
      termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    endif

    if (is%wrap%a2i_active) then

    ! Then regrid individual fields other than conservative and overwrite

    do n = 1,fldsFrAtm%num
       if (fldsFrAtm%mapping(n) == 'bilinear') then
          call ESMF_LogWrite(trim(subname)//": map a2i_bilnr "//trim(fldsFrAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call FieldBundle_FieldRegrid(is%wrap%FBAtm_a,fldsFrAtm%shortname(n), &
                                       is%wrap%FBAtm_i,fldsFrAtm%shortname(n), &
                                       is%wrap%RH_a2i_bilnr,rc)
       endif
       if (fldsFrAtm%mapping(n) == 'patch') then
          call ESMF_LogWrite(trim(subname)//": map a2i_patch "//trim(fldsFrAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call FieldBundle_FieldRegrid(is%wrap%FBAtm_a,fldsFrAtm%shortname(n), &
                                       is%wrap%FBAtm_i,fldsFrAtm%shortname(n), &
                                       is%wrap%RH_a2i_patch,rc)
       endif
    enddo
    
    endif
    
    do n = 1,fldsFrOcn%num
       if (fldsFrOcn%mapping(n) == 'bilinear') then
          call ESMF_LogWrite(trim(subname)//": map o2i_bilnr "//trim(fldsFrOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_LogWrite(trim(subname)//": map o2a_bilnr "//trim(fldsFrOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          if (is%wrap%o2i_active) then
          call FieldBundle_FieldRegrid(is%wrap%FBOcn_o,fldsFrOcn%shortname(n), &
                                       is%wrap%FBOcn_i,fldsFrOcn%shortname(n), &
                                       is%wrap%RH_o2i_bilnr,rc)
          endif
          if (is%wrap%o2a_active) then
          call FieldBundle_FieldRegrid(is%wrap%FBOcn_o,fldsFrOcn%shortname(n), &
                                       is%wrap%FBOcn_a,fldsFrOcn%shortname(n), &
                                       is%wrap%RH_o2a_bilnr,rc)
          endif
       endif
       if (fldsFrOcn%mapping(n) == 'patch') then
          call ESMF_LogWrite(trim(subname)//": map o2i_patch "//trim(fldsFrOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call ESMF_LogWrite(trim(subname)//": map o2a_patch "//trim(fldsFrOcn%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          if (is%wrap%o2i_active) then
          call FieldBundle_FieldRegrid(is%wrap%FBOcn_o,fldsFrOcn%shortname(n), &
                                       is%wrap%FBOcn_i,fldsFrOcn%shortname(n), &
                                       is%wrap%RH_o2i_patch,rc)
          endif
          if (is%wrap%o2a_active) then
          call FieldBundle_FieldRegrid(is%wrap%FBOcn_o,fldsFrOcn%shortname(n), &
                                       is%wrap%FBOcn_a,fldsFrOcn%shortname(n), &
                                       is%wrap%RH_o2a_patch,rc)
          endif
       endif
    enddo

    if (is%wrap%i2a_active) then
    do n = 1,fldsFrIce%num
       if (fldsFrIce%mapping(n) == 'bilinear') then
          call ESMF_LogWrite(trim(subname)//": map i2a_bilnr "//trim(fldsFrIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call FieldBundle_FieldRegrid(is%wrap%FBIce_i,fldsFrIce%shortname(n), &
                                       is%wrap%FBice_a,fldsFrIce%shortname(n), &
                                       is%wrap%RH_i2a_bilnr,rc)
       endif
       if (fldsFrIce%mapping(n) == 'patch') then
          call ESMF_LogWrite(trim(subname)//": map i2a_patch "//trim(fldsFrIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call FieldBundle_FieldRegrid(is%wrap%FBIce_i,fldsFrIce%shortname(n), &
                                       is%wrap%FBice_a,fldsFrIce%shortname(n), &
                                       is%wrap%RH_i2a_patch,rc)
       endif
    enddo
    endif

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBAtm_i, trim(subname)//' FBAtm_i ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBOcn_i, trim(subname)//' FBOcn_i ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBOcn_a, trim(subname)//' FBOcn_a ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBIce_a, trim(subname)//' FBIce_a ', rc=rc)
    endif

    call fieldBundle_copy(is%wrap%FBforIce, is%wrap%FBAtm_i, rc=rc)
    call fieldBundle_copy(is%wrap%FBforIce, is%wrap%FBOcn_i, rc=rc)
    call fieldBundle_copy(is%wrap%FBforAtm, is%wrap%FBOcn_a, rc=rc)
    call fieldBundle_copy(is%wrap%FBforAtm, is%wrap%FBIce_a, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBforAtm, trim(subname)//' FBforAtm ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBforIce, trim(subname)//' FBforIce ', rc=rc)
    endif

    !---------------------------------------
    !--- custom calculations to atm and ice
    !---------------------------------------

    ! land mask

    if (FieldBundle_FldChk(is%wrap%FBIce_a , 'ice_mask'  , rc=rc) .and. &
        FieldBundle_FldChk(is%wrap%FBOcn_a , 'ocean_mask', rc=rc) .and. &
        FieldBundle_FldChk(is%wrap%FBforAtm, 'land_mask' , rc=rc)) then

       call FieldBundle_GetFldPtr(is%wrap%FBIce_a, 'ice_mask', dataPtr1, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       call FieldBundle_GetFldPtr(is%wrap%FBOcn_a, 'ocean_mask', dataPtr2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       call FieldBundle_GetFldPtr(is%wrap%FBforAtm, 'land_mask', dataPtr3, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       ! Here, the ocean/ice mask is the intersection of ocean and ice masks
       do j = lbound(dataPtr3,2),ubound(dataPtr3,2)
       do i = lbound(dataPtr3,1),ubound(dataPtr3,1)
          maski = dataPtr1(i,j)
          if (maski < 0.01) maski = 0.0_ESMF_KIND_R8
          if (maski > 0.99) maski = 1.0_ESMF_KIND_R8
          masko = dataPtr2(i,j)
          if (masko < 0.01) masko = 0.0_ESMF_KIND_R8
          if (masko > 0.99) masko = 1.0_ESMF_KIND_R8
          maskoi = min(masko,maski)
          maskl = max(min(1.0_ESMF_KIND_R8 - maskoi,1.0_ESMF_KIND_R8),0.0_ESMF_KIND_R8)
          dataPtr3(i,j) = maskl
!          write(msgString,*) trim(subname)//'maskl1',i,j,dataPtr1(i,j),dataPtr2(i,j)
!          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
!          write(msgString,*) trim(subname)//'maskl2',i,j,maski,masko,maskoi,maskl
!          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
       enddo
       enddo

       write (msgString,*) trim(subname)//"ice_mask = ",minval(dataPtr1),maxval(dataPtr1)
       call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
       write (msgString,*) trim(subname)//"ocean_mask = ",minval(dataPtr2),maxval(dataPtr2)
       call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
       write (msgString,*) trim(subname)//"land_mask = ",minval(dataPtr3),maxval(dataPtr3)
       call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

       call ESMF_FieldBundleGet(is%wrap%FBIce_a, fieldName='ice_mask', field=field, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       call ESMF_FieldWrite(field,'field_med_ice_a_ice_mask.nc',overwrite=.true.,rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       call ESMF_FieldBundleGet(is%wrap%FBOcn_a, fieldName='ocean_mask', field=field, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       call ESMF_FieldWrite(field,'field_med_ocn_a_ocean_mask.nc',overwrite=.true.,rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       call ESMF_FieldBundleGet(is%wrap%FBforAtm, fieldName='land_mask', field=field, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       call ESMF_FieldWrite(field,'field_med_atm_a_land_mask.nc',overwrite=.true.,rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    endif

    !---------------------------------------
    !--- merges to atm
    !---------------------------------------

    ! ocn and ice fraction
!    call FieldBundle_GetFldPtr(is%wrap%FBIce_a, 'ice_fraction', icewgt, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
!    allocate(ocnwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
!    do j=lbound(icewgt,2),ubound(icewgt,2)
!    do i=lbound(icewgt,1),ubound(icewgt,1)
!      ocnwgt = 1.0_ESMF_KIND_R8 - icewgt
!    enddo
!    enddo

!    call fieldBundle_FieldMerge(is%wrap%FBforAtm,'field1' , & 
!                                is%wrap%FBOcn_a, 'field1' ,ocnwgt, &
!                                is%wrap%FBIce_a, 'field1' ,icewgt, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

!    deallocate(ocnwgt)


    !---------------------------------------
    !--- merges to ice
    !---------------------------------------


    !---------------------------------------
    !--- set export State to special value for testing
    !---------------------------------------

    call state_reset(NState_AtmExp, value=-99._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call state_reset(NState_IceExp, value=-99._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(NState_AtmExp, trim(subname)//' AtmExp_99 ', rc=rc)
      call State_diagnose(NState_IceExp, trim(subname)//' IceExp_99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call fieldBundle_copy(NState_AtmExp, is%wrap%FBforAtm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call fieldBundle_copy(NState_IceExp, is%wrap%FBforIce, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
      call state_diagnose(NState_AtmExp, trim(subname)//' AtmExp_final ', rc=rc)
      call state_diagnose(NState_IceExp, trim(subname)//' IceExp_final ', rc=rc)
    endif

    ! write the fields exported to atm to file
    call NUOPC_StateWrite(NState_AtmExp, &
      fieldNameList=fldsToAtm%shortname(1:fldsToAtm%num), &
      filePrefix="field_med_to_atm_", timeslice=is%wrap%fastcntr, &
      relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write the fields exported to ice to file
    call NUOPC_StateWrite(NState_IceExp, &
      fieldNameList=fldsToIce%shortname(1:fldsToIce%num), &
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
    call NUOPC_StateWrite(NState_AtmImp, &
      fieldNameList=fldsFrAtm%shortname(1:fldsFrAtm%num), &
      filePrefix="field_med_from_atm_", timeslice=is%wrap%fastcntr, &
      relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write the fields imported from ice to file
    call NUOPC_StateWrite(NState_IceImp, &
      fieldNameList=fldsFrIce%shortname(1:fldsFrIce%num), &
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
      call State_diagnose(NState_IceImp, trim(subname)//' IceImp ', rc=rc)
      call State_diagnose(NState_AtmImp, trim(subname)//' AtmImp ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBaccumAtm, trim(subname)//' FBaccAtm_B4accum ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBaccumIce, trim(subname)//' FBaccIce_B4accum ', rc=rc)
    endif

    call fieldBundle_accum(is%wrap%FBaccumAtm, NState_AtmImp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    is%wrap%accumcntAtm = is%wrap%accumcntAtm + 1

    call fieldBundle_accum(is%wrap%FBaccumIce, NState_IceImp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    is%wrap%accumcntIce = is%wrap%accumcntIce + 1
         
    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBaccumAtm, trim(subname)//' FBaccAtm_AFaccum ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBaccumIce, trim(subname)//' FBaccIce_AFaccum ', rc=rc)
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
    character(ESMF_MAXSTR)      :: fieldname1(10),fieldname2(10),fieldname3(10)
    real(ESMF_KIND_R8), pointer :: dataPtr1(:,:),dataPtr2(:,:),dataPtr3(:,:)
    real(ESMF_KIND_R8), pointer :: atmwgt(:,:),icewgt(:,:),customwgt(:,:)
    logical                     :: checkOK, checkOK1, checkOK2
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
    call NUOPC_StateWrite(NState_OcnImp, &
      fieldNameList=fldsFrOcn%shortname(1:fldsFrOcn%num), &
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
    
    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBaccumAtm, trim(subname)//' FBaccA_avg ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBaccumIce, trim(subname)//' FBaccI_avg ', rc=rc)
    endif

    !---------------------------------------
    !--- regrid average atm+ice fields to ocean grid
    !---------------------------------------
    
    if (is%wrap%a2o_active) then

    call ESMF_LogWrite(trim(subname)//' calling FBRegrid FBaccumAtm to FBAtm_o', ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_FieldBundleRegrid(is%wrap%FBaccumAtm, is%wrap%FBAtm_o, &
      routehandle=is%wrap%RH_a2o_consv, &
      termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    endif
    
    if (is%wrap%i2o_active) then

    call ESMF_LogWrite(trim(subname)//' calling FBRegrid FBaccumIce to FBIce_o', ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_FieldBundleRegrid(is%wrap%FBaccumIce, is%wrap%FBIce_o, &
      routehandle=is%wrap%RH_i2o_consv, &
      termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    endif

    do n = 1,fldsFrAtm%num
       if (fldsFrAtm%mapping(n) == 'bilinear') then
          call ESMF_LogWrite(trim(subname)//": map a2o_bilnr "//trim(fldsFrAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call FieldBundle_FieldRegrid(is%wrap%FBaccumAtm,fldsFrAtm%shortname(n), &
                                       is%wrap%FBAtm_o,fldsFrAtm%shortname(n), &
                                       is%wrap%RH_a2o_bilnr,rc)
       endif
       if (fldsFrAtm%mapping(n) == 'patch') then
          call ESMF_LogWrite(trim(subname)//": map a2o_patch "//trim(fldsFrAtm%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call FieldBundle_FieldRegrid(is%wrap%FBaccumAtm,fldsFrAtm%shortname(n), &
                                       is%wrap%FBAtm_o,fldsFrAtm%shortname(n), &
                                       is%wrap%RH_a2o_patch,rc)
       endif
    enddo

    do n = 1,fldsFrIce%num
       if (fldsFrIce%mapping(n) == 'bilinear') then
          call ESMF_LogWrite(trim(subname)//": map i2o_bilnr "//trim(fldsFrIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call FieldBundle_FieldRegrid(is%wrap%FBaccumIce,fldsFrIce%shortname(n), &
                                       is%wrap%FBIce_o,fldsFrIce%shortname(n), &
                                       is%wrap%RH_i2o_bilnr,rc)
       endif
       if (fldsFrIce%mapping(n) == 'patch') then
          call ESMF_LogWrite(trim(subname)//": map i2o_patch "//trim(fldsFrIce%shortname(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          call FieldBundle_FieldRegrid(is%wrap%FBaccumIce,fldsFrIce%shortname(n), &
                                       is%wrap%FBIce_o,fldsFrIce%shortname(n), &
                                       is%wrap%RH_i2o_patch,rc)
       endif
    enddo

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBAtm_o, trim(subname)//' FBAtm_o ', rc=rc)
      call FieldBundle_diagnose(is%wrap%FBIce_o, trim(subname)//' FBIce_o ', rc=rc)
    endif

! tcx Xgrid
    ! XGrid intermediary required? instantiate FBXgrid FieldBundle?
    ! call ESMF_FieldBundleRegrid(is%wrap%FBaccumAtm, FBXgrid, is%wrap%RHa2x, &
    !    termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    ! call ESMF_FieldBundleRegrid(FBXgrid, is%wrap%FBforOcn  , is%wrap%RHx2o, &
    !    termorderflag=(/ESMF_TERMORDER_SRCSEQ/), rc=rc)
    ! tcraig temporarily copy
    
    call fieldBundle_copy(is%wrap%FBforOcn, is%wrap%FBAtm_o, rc=rc)
    call fieldBundle_copy(is%wrap%FBforOcn, is%wrap%FBIce_o, rc=rc)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBforOcn, trim(subname)//' FB4ocn_AFregrid ', rc=rc)
    endif

    !---------------------------------------
    !--- custom calculations to ocn
    !---------------------------------------

    !--- split total solar into 4 terms and compute net shortwave

    !--- atm downward sw

    fieldname1(1) = 'mean_down_sw_vis_dir_flx'
    fieldname1(2) = 'mean_down_sw_vis_dif_flx'
    fieldname1(3) = 'mean_down_sw_ir_dir_flx'
    fieldname1(4) = 'mean_down_sw_ir_dif_flx'

    ! check fields exist
    checkOK = .true.
    do n = 1,4
      checkOK = checkOK .and. FieldBundle_FldChk(is%wrap%FBAtm_o, trim(fieldname1(n)), rc=rc)
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
!      checkOK = FieldBundle_FldChk(is%wrap%FBAtm_o, trim(fieldname1(1)), rc=rc)
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
      fieldname2(1) = 'mean_net_sw_vis_dir_flx'
      fieldname2(2) = 'mean_net_sw_vis_dif_flx'
      fieldname2(3) = 'mean_net_sw_ir_dir_flx'
      fieldname2(4) = 'mean_net_sw_ir_dif_flx'

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
        fieldname2(1) = 'mean_down_sw_vis_dir_flx'
        fieldname2(2) = 'mean_down_sw_vis_dif_flx'
        fieldname2(3) = 'mean_down_sw_ir_dir_flx'
        fieldname2(4) = 'mean_down_sw_ir_dif_flx'

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
      fieldname3(1) = 'inst_ocn_vis_dir_albedo'
      fieldname3(2) = 'inst_ocn_vis_dif_albedo'
      fieldname3(3) = 'inst_ocn_ir_dir_albedo'
      fieldname3(4) = 'inst_ocn_ir_dif_albedo'
      checkOK2 = .true.
      do n = 1,4
        checkOK2 = checkOK2 .and. FieldBundle_FldChk(is%wrap%FBAtm_o, trim(fieldname3(n)), rc=rc)
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
        fieldname3(1) = 'inst_vis_dir_albedo'
        fieldname3(2) = 'inst_vis_dif_albedo'
        fieldname3(3) = 'inst_ir_dir_albedo'
        fieldname3(4) = 'inst_ir_dif_albedo'
        checkOK2 = .true.
        do n = 1,4
          checkOK2 = checkOK2 .and. FieldBundle_FldChk(is%wrap%FBAtm_o, trim(fieldname3(n)), rc=rc)
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
!      call FieldBundle_GetFldPtr(is%wrap%FBAtm_o, trim(fieldname1(1)), dataPtr1, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do n = 1,4
        call FieldBundle_GetFldPtr(is%wrap%FBAtm_o, trim(fieldname1(n)), dataPtr1, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        call FieldBundle_GetFldPtr(is%wrap%FBforOcn, trim(fieldname2(n)), dataPtr2, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        if (.not.FldPtr_SameCheck(dataPtr1, dataPtr2, 'swnet', rc)) then
          call ESMF_LogWrite(trim(subname)//": ERROR in dataPtr size ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=rc)
          return
        endif

        if (checkOK2) then
          call FieldBundle_GetFldPtr(is%wrap%FBAtm_o, trim(fieldname3(n)), dataPtr3, rc=rc)
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

#if (1 == 0)
    !--- compute specific humidity flux from latent heat flux

    fieldname1(1) = 'mean_laten_heat_flx'
    fieldname2(1) = 'mean_evap_rate'

    ! check fields exist
    checkOK = FieldBundle_FldChk(is%wrap%FBAtm_o, trim(fieldname1(1)), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    checkOK = checkOK .and. FieldBundle_FldChk(is%wrap%FBforOcn, trim(fieldname2(1)), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (checkOK) then
      if (dbug_flag > 1) then
        call ESMF_LogWrite(trim(subname)//' evap:compute mevap from mlhfx', ESMF_LOGMSG_INFO, rc=dbrc)
      endif
      call FieldBundle_GetFldPtr(is%wrap%FBAtm_o, trim(fieldname1(1)), dataPtr1, rc=rc)
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
#endif

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBforOcn, trim(subname)//' FB4ocn_AFcc ', rc=rc)
    endif

    !---------------------------------------
    !--- merges to ocn
    !---------------------------------------

    if (is%wrap%i2a_active) then

    ! atm and ice fraction
    call FieldBundle_GetFldPtr(is%wrap%FBIce_o, 'ice_fraction', icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(atmwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    allocate(customwgt(lbound(icewgt,1):ubound(icewgt,1),lbound(icewgt,2):ubound(icewgt,2)))
    do j=lbound(icewgt,2),ubound(icewgt,2)
    do i=lbound(icewgt,1),ubound(icewgt,1)
      atmwgt = 1.0_ESMF_KIND_R8 - icewgt
    enddo
    enddo

    customwgt = atmwgt / const_lhvap
    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_evap_rate' , & 
                                is%wrap%FBAtm_o, 'mean_latent_heat_flux' ,customwgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_prec_rate' , & 
                                is%wrap%FBAtm_o, 'mean_prec_rate' ,atmwgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_fprec_rate' , & 
                                is%wrap%FBAtm_o, 'mean_fprec_rate' ,atmwgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_evap_rate' , & 
                                is%wrap%FBAtm_o, 'mean_evap_rate' ,atmwgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_sensi_heat_flx' , & 
                                is%wrap%FBAtm_o, 'mean_sensi_heat_flx' ,atmwgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_laten_heat_flx' , & 
                                is%wrap%FBAtm_o, 'mean_laten_heat_flx' ,atmwgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_down_lw_flx' , & 
                                is%wrap%FBAtm_o, 'mean_down_lw_flx' ,atmwgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_zonal_moment_flx' , & 
                                is%wrap%FBAtm_o, 'mean_zonal_moment_flx'  ,atmwgt, &
                                is%wrap%FBIce_o, 'stress_on_air_ice_zonal',icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call fieldBundle_FieldMerge(is%wrap%FBforOcn,'mean_merid_moment_flx' , & 
                                is%wrap%FBAtm_o, 'mean_merid_moment_flx'  ,atmwgt, &
                                is%wrap%FBIce_o, 'stress_on_air_ice_merid',icewgt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    deallocate(atmwgt,customwgt)

    if (dbug_flag > 1) then
      call FieldBundle_diagnose(is%wrap%FBforOcn, trim(subname)//' FB4ocn_AFmrg ', rc=rc)
    endif
    
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

    call state_reset(NState_OcnExp, value=-99._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(NState_OcnExp, trim(subname)//' es_AF99 ', rc=rc)
    endif

    !---------------------------------------
    !--- copy into export state
    !---------------------------------------

    call fieldBundle_copy(NState_OcnExp, is%wrap%FBforOcn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
      call State_diagnose(NState_OcnExp, trim(subname)//' es_AFcp ', rc=rc)
    endif

    ! write the fields exported to ocn to file
    call NUOPC_StateWrite(NState_OcnExp, &
      fieldNameList=fldsToOcn%shortname(1:fldsToOcn%num), &
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
    integer               , intent(out)   :: rc

    ! local variables
    integer                    :: i,j,n
    integer                    :: fieldCount
    character(ESMF_MAXSTR) ,pointer :: fieldNameList(:)
    type(ESMF_Field)           :: field
    type(ESMF_Grid)            :: lgrid
    character(ESMF_MAXSTR)     :: lname
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_initFromFB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS
      
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
    ! Create FieldBundle from fieldNameList and grid OR
    ! from State with State field grids or argument grid
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    character(len=*)      , intent(in), optional  :: fieldNameList(:)
    type(ESMF_Grid)       , intent(in), optional  :: grid
    type(ESMF_State)      , intent(in), optional  :: State  ! check if fieldnames are there
    character(len=*)      , intent(in), optional  :: name
    integer               , intent(out) :: rc

    ! local variables
    integer                    :: i,j,n,fieldCount
    character(ESMF_MAXSTR)     :: lname
    character(ESMF_MAXSTR),allocatable :: lfieldNameList(:)
    type(ESMF_Field)           :: field,lfield
    type(ESMF_Grid)            :: lgrid
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_init)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    lname = 'undefined'
    if (present(name)) then
       lname = trim(name)
    endif

    !--- check argument consistency

    if (present(fieldNameList)) then
      if (.not. present(grid)) then
        call ESMF_LogWrite(trim(subname)//": ERROR fieldNameList requires grid", ESMF_LOGMSG_INFO, rc=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
      if (present(State)) then
        call ESMF_LogWrite(trim(subname)//": ERROR fieldNameList cannot pass State", ESMF_LOGMSG_INFO, rc=rc)
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      endif
    endif

    FieldBundle = ESMF_FieldBundleCreate(name=trim(lname), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (present(fieldNameList)) then
      do n = 1, size(fieldNameList)
        field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=fieldNameList(n), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_FieldBundleAdd(FieldBundle, (/field/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        if (dbug_flag > 1) then
          call ESMF_LogWrite(trim(subname)//":"//trim(lname)//":add  "//trim(fieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
        endif
      enddo  ! fieldNameList
    endif  ! present fldnamelist

    if (present(State)) then
      call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      allocate(lfieldNameList(fieldCount))
      call ESMF_StateGet(State, itemNameList=lfieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      do n = 1, fieldCount
        if (present(grid)) then
          field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, name=lfieldNameList(n), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          call ESMF_FieldBundleAdd(FieldBundle, (/field/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          if (dbug_flag > 1) then
            call ESMF_LogWrite(trim(subname)//":"//trim(lname)//":add  "//trim(lfieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          endif

        else
          call ESMF_StateGet(State, itemName=trim(lfieldNameList(n)), field=lfield, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          call ESMF_FieldGet(lfield, grid=lgrid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

          field = ESMF_FieldCreate(lgrid, ESMF_TYPEKIND_R8, name=lfieldNameList(n), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          call ESMF_FieldBundleAdd(FieldBundle, (/field/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          if (dbug_flag > 1) then
            call ESMF_LogWrite(trim(subname)//":"//trim(lname)//":add  "//trim(lfieldNameList(n)), ESMF_LOGMSG_INFO, rc=dbrc)
          endif
        endif
      enddo  ! fieldCount
      deallocate(lfieldNameList)
    endif  ! present State

    call fieldBundle_reset(FieldBundle, value=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
  end subroutine fieldBundle_init

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_getName(FieldBundle, fieldnum, fieldname, rc)
    ! ----------------------------------------------
    ! Destroy fields in FieldBundle and FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    integer               , intent(in)    :: fieldnum
    character(len=*)      , intent(out)   :: fieldname
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_getName)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    fieldname = ' '

    call ESMF_FieldBundleGet(FieldBundle, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (fieldnum > fieldCount) then
      call ESMF_LogWrite(trim(subname)//": ERROR fieldnum > fieldCount ", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    allocate(fieldNameList(fieldCount))
    call ESMF_FieldBundleGet(FieldBundle, fieldNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    fieldname = fieldNameList(fieldnum)

    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_getName

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_getFieldN(FieldBundle, fieldnum, field, rc)
    ! ----------------------------------------------
    ! Destroy fields in FieldBundle and FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    integer               , intent(in)    :: fieldnum
    type(ESMF_Field)      , intent(inout) :: field
    integer               , intent(out)   :: rc

    ! local variables
    character(len=ESMF_MAXSTR) :: name
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_getFieldN)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call fieldBundle_getName(FieldBundle, fieldnum, name, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_FieldBundleGet(FieldBundle, fieldName=name, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_getFieldN

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_getFieldName(FieldBundle, fieldname, field, rc)
    ! ----------------------------------------------
    ! Destroy fields in FieldBundle and FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    character(len=*)      , intent(in)    :: fieldname
    type(ESMF_Field)      , intent(inout) :: field
    integer               , intent(out)   :: rc

    ! local variables
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_getFieldName)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FieldBundle, fieldName=fieldname, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_getFieldName

  !-----------------------------------------------------------------------------

  subroutine fieldBundle_clean(FieldBundle, rc)
    ! ----------------------------------------------
    ! Destroy fields in FieldBundle and FieldBundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FieldBundle
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    type(ESMF_Field)            :: field
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_clean)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
    real(ESMF_KIND_R8)    , intent(in), optional :: value
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8)          :: lvalue
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_reset)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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

  subroutine FieldBundle_FieldCopy(FBin,fldin,FBout,fldout,rc)
    ! ----------------------------------------------
    ! Regrid a field in a field bundle to another field in a field bundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBin
    character(len=*)      , intent(in)    :: fldin
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in)    :: fldout
    integer               , intent(out)   :: rc

    ! local
    real(ESMF_KIND_R8), pointer :: dataPtrIn(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtrOut(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FieldCopy)'

    rc = ESMF_SUCCESS

    if (FieldBundle_FldChk(FBin , trim(fldin) , rc=rc) .and. &
        FieldBundle_FldChk(FBout, trim(fldout), rc=rc)) then

      call FieldBundle_GetFldPtr(FBin, trim(fldin), dataPtrIn, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call FieldBundle_GetFldPtr(FBout, trim(fldout), dataPtrOut, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (.not.FldPtr_SameCheck(dataPtrIn, dataPtrOut, subname, rc)) then
        call ESMF_LogWrite(trim(subname)//": ERROR fname not present with FBin", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
        rc = ESMF_FAILURE
        return
      endif

      dataPtrOut = dataPtrIn

    else

       if (dbug_flag > 1) then
         call ESMF_LogWrite(trim(subname)//" field not found: "//trim(fldin)//","//trim(fldout), ESMF_LOGMSG_INFO, rc=dbrc)
       endif

    endif

  end subroutine FieldBundle_FieldCopy

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_FieldRegrid(FBin,fldin,FBout,fldout,RH,rc)
    ! ----------------------------------------------
    ! Regrid a field in a field bundle to another field in a field bundle
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBin
    character(len=*)      , intent(in)    :: fldin
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in)    :: fldout
    type(ESMF_RouteHandle), intent(inout) :: RH
    integer               , intent(out)   :: rc

    ! local
    type(ESMF_Field) :: field1, field2
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FieldRegrid)'

    rc = ESMF_SUCCESS

    if (FieldBundle_FldChk(FBin , trim(fldin) , rc=rc) .and. &
        FieldBundle_FldChk(FBout, trim(fldout), rc=rc)) then

       call FieldBundle_GetFieldName(FBin, trim(fldin), field1, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
       call FieldBundle_GetFieldName(FBout, trim(fldout), field2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

       call ESMF_FieldRegrid(field1, field2, routehandle=RH, &
         termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    else

       if (dbug_flag > 1) then
         call ESMF_LogWrite(trim(subname)//" field not found: "//trim(fldin)//","//trim(fldout), ESMF_LOGMSG_INFO, rc=dbrc)
       endif

    endif

  end subroutine FieldBundle_FieldRegrid

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_FieldMerge(FBout, fnameout, &
                                    FBinA, fnameA, wgtA, &
                                    FBinB, fnameB, wgtB, &
                                    FBinC, fnameC, wgtC, &
                                    FBinD, fnameD, wgtD, &
                                    FBinE, fnameE, wgtE, rc)
    ! ----------------------------------------------
    ! Supports up to a five way merge
    ! ----------------------------------------------
    type(ESMF_FieldBundle), intent(inout) :: FBout
    character(len=*)      , intent(in)    :: fnameout
    type(ESMF_FieldBundle), intent(in), optional :: FBinA
    character(len=*)      , intent(in), optional :: fnameA
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtA(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinB
    character(len=*)      , intent(in), optional :: fnameB
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtB(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinC
    character(len=*)      , intent(in), optional :: fnameC
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtC(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinD
    character(len=*)      , intent(in), optional :: fnameD
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtD(:,:)
    type(ESMF_FieldBundle), intent(in), optional :: FBinE
    character(len=*)      , intent(in), optional :: fnameE
    real(ESMF_KIND_R8)    , intent(in), optional, pointer :: wgtE(:,:)
    integer               , intent(out)   :: rc

    ! local variables
    real(ESMF_KIND_R8), pointer :: dataOut(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    real(ESMF_KIND_R8), pointer :: wgt(:,:)
    integer :: lb1,ub1,lb2,ub2,i,j,n
    logical :: wgtfound, FBinfound
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FieldMerge)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc=ESMF_SUCCESS

    if (.not. FieldBundle_FldChk(FBout, trim(fnameout), rc=rc)) then
      call ESMF_LogWrite(trim(subname)//": WARNING field not in FBout, skipping merge "//trim(fnameout), ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=dbrc)
      return
    endif
    call FieldBundle_GetFldPtr(FBout, trim(fnameout), dataOut, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    lb1 = lbound(dataOut,1)
    ub1 = ubound(dataOut,1)
    lb2 = lbound(dataOut,2)
    ub2 = ubound(dataOut,2)
    allocate(wgt(lb1:ub1,lb2:ub2))

    dataOut = 0.0_ESMF_KIND_R8

    ! check each field has a fieldname passed in
    if ((present(FBinA) .and. .not.present(fnameA)) .or. &
        (present(FBinB) .and. .not.present(fnameB)) .or. &
        (present(FBinC) .and. .not.present(fnameC)) .or. &
        (present(FBinD) .and. .not.present(fnameD)) .or. &
        (present(FBinE) .and. .not.present(fnameE))) then
      call ESMF_LogWrite(trim(subname)//": ERROR fname not present with FBin", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    ! check that each field passed in actually exists, if not DO NOT do any merge
    FBinfound = .true.
    if (present(FBinA)) then
      if (.not. FieldBundle_FldChk(FBinA, trim(fnameA), rc=rc)) FBinfound = .false.
    endif
    if (present(FBinB)) then
      if (.not. FieldBundle_FldChk(FBinB, trim(fnameB), rc=rc)) FBinfound = .false.
    endif
    if (present(FBinC)) then
      if (.not. FieldBundle_FldChk(FBinC, trim(fnameC), rc=rc)) FBinfound = .false.
    endif
    if (present(FBinD)) then
      if (.not. FieldBundle_FldChk(FBinD, trim(fnameD), rc=rc)) FBinfound = .false.
    endif
    if (present(FBinE)) then
      if (.not. FieldBundle_FldChk(FBinE, trim(fnameE), rc=rc)) FBinfound = .false.
    endif
    if (.not. FBinfound) then
      call ESMF_LogWrite(trim(subname)//": WARNING fname not found in FBin, skipping merge "//trim(fnameout), ESMF_LOGMSG_WARNING, line=__LINE__, file=__FILE__, rc=dbrc)
      return
    endif

    ! n=1,5 represents adding A to E inputs if they exist
    do n = 1,5
      FBinfound = .false.
      wgtfound = .false.

      if (n == 1 .and. present(FBinA)) then
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinA, trim(fnameA), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        if (present(wgtA)) then
          wgtfound = .true.
          wgt => wgtA
        endif

      elseif (n == 2 .and. present(FBinB)) then
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinB, trim(fnameB), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        if (present(wgtB)) then
          wgtfound = .true.
          wgt => wgtB
        endif

      elseif (n == 3 .and. present(FBinC)) then
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinC, trim(fnameC), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        if (present(wgtC)) then
          wgtfound = .true.
          wgt => wgtC
        endif

      elseif (n == 4 .and. present(FBinD)) then
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinD, trim(fnameD), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        if (present(wgtD)) then
          wgtfound = .true.
          wgt => wgtD
        endif

      elseif (n == 5 .and. present(FBinE)) then
        FBinfound = .true.
        call FieldBundle_GetFldPtr(FBinE, trim(fnameE), dataPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        if (present(wgtE)) then
          wgtfound = .true.
          wgt => wgtE
        endif

      endif

      if (FBinfound) then
        if (.not.FldPtr_SameCheck(dataPtr, dataOut, subname, rc)) then
          call ESMF_LogWrite(trim(subname)//": ERROR FBin wrong size", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
        endif

        if (wgtfound) then
          if (.not.FldPtr_SameCheck(dataPtr, wgt, subname, rc)) then
            call ESMF_LogWrite(trim(subname)//": ERROR wgt wrong size", ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
            rc = ESMF_FAILURE
            return
          endif
          do j = lb2,ub2
          do i = lb1,ub1
            dataOut(i,j) = dataOut(i,j) + dataPtr(i,j) * wgt(i,j)
          enddo
          enddo
        else
          do j = lb2,ub2
          do i = lb1,ub1
            dataOut(i,j) = dataOut(i,j) + dataPtr(i,j)
          enddo
          enddo
        endif  ! wgtfound

      endif  ! FBin found
    enddo  ! n

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FieldBundle_FieldMerge

  !-----------------------------------------------------------------------------

  subroutine state_reset(State, value, rc)
    ! ----------------------------------------------
    ! Set all fields to value in State
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(ESMF_State)  , intent(inout) :: State
    real(ESMF_KIND_R8), intent(in), optional :: value
    integer           , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8)          :: lvalue
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:state_reset)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_average)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
    character(len=*)      , intent(in), optional :: string
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_diagnose)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
      write(msgString,'(A,3g14.7)') trim(subname)//' '//trim(lstring)//':'//trim(fieldNameList(n)), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    deallocate(fieldNameList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fieldBundle_diagnose

  !-----------------------------------------------------------------------------

  subroutine array_diagnose(array, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of fieldBundle
    ! ----------------------------------------------
    type(ESMF_Array), intent(inout) :: array
    character(len=*), intent(in), optional :: string
    integer         , intent(out) :: rc

    ! local variables
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:array_diagnose)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! this is not working yet, not sure about dataPtr dim/type
    return

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_ArrayGet(Array, farrayPtr=dataPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    write(msgString,'(A,3g14.7)') trim(subname)//' '//trim(lstring), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine array_diagnose

  !-----------------------------------------------------------------------------

  subroutine state_diagnose(State, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of fieldBundle
    ! ----------------------------------------------
    type(ESMF_State), intent(inout) :: State
    character(len=*), intent(in), optional :: string
    integer         , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:state_diagnose)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
      write(msgString,'(A,3g14.7)') trim(subname)//' '//trim(lstring)//':'//trim(fieldNameList(n)), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
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
    integer               , intent(out)   :: rc

    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_copyFB2FB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
    integer               , intent(out)   :: rc

    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_copyFB2ST)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
    integer               , intent(out)   :: rc

    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_copyST2FB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
    logical, optional     , intent(in)    :: copy
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    logical                     :: exists
    logical                     :: lcopy
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtri(:,:), dataPtro(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_accumFB2FB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
    logical, optional     , intent(in)    :: copy
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    logical                     :: lcopy
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtrS(:,:), dataPtrB(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_accumST2FB)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
    logical, optional     , intent(in)    :: copy
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    logical                     :: lcopy
    character(ESMF_MAXSTR) ,pointer  :: fieldNameList(:)
    type(ESMF_StateItem_Flag)   :: itemType
    real(ESMF_KIND_R8), pointer :: dataPtrS(:,:), dataPtrB(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:fieldBundle_accumFB2ST)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

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
    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*)      , intent(in)  :: fldname
    integer               , intent(out) :: rc

    ! local variables
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FldChk)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    FieldBundle_FldChk = .false.

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (isPresent) then
       FieldBundle_FldChk = .true.
    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function FieldBundle_FldChk

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_GetFldPtr(FB, fldname, fldptr, rc)
    type(ESMF_FieldBundle), intent(in)  :: FB
    character(len=*)      , intent(in)  :: fldname
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr(:,:)
    integer               , intent(out) :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_GetFldPtr)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    if (.not. FieldBundle_FldChk(FB, trim(fldname), rc=rc)) then
      call ESMF_LogWrite(trim(subname)//": ERROR field not in FB "//trim(fldname), ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      rc = ESMF_FAILURE
      return
    endif

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), field=lfield, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FieldBundle_GetFldPtr

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine State_GetFldPtr(ST, fldname, fldptr, rc)
    type(ESMF_State), intent(in)  :: ST
    character(len=*), intent(in)  :: fldname
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr(:,:)
    integer         , intent(out) :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    character(len=*),parameter :: subname='(module_MEDIATOR:State_GetFldPtr)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_StateGet(ST, itemName=trim(fldname), field=lfield, rc=rc)
!tcx
    call ESMF_LogWrite(trim(subname)//": fldname ="//trim(fldname), ESMF_LOGMSG_INFO,rc=dbrc)
!    call ESMF_StatePrint(ST,rc=dbrc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine State_GetFldPtr

  !-----------------------------------------------------------------------------

  logical function FldPtr_SameCheck(fldptr1, fldptr2, cstring, rc)
    real(ESMF_KIND_R8), pointer, intent(in)  :: fldptr1(:,:)
    real(ESMF_KIND_R8), pointer, intent(in)  :: fldptr2(:,:)
    character(len=*)           , intent(in)  :: cstring
    integer                    , intent(out) :: rc

    ! local variables
    character(len=*),parameter :: subname='(module_MEDIATOR:FldPtr_SameCheck)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    FldPtr_SameCheck = .false.
    if (lbound(fldptr2,2) /= lbound(fldptr1,2) .or. &
        lbound(fldptr2,1) /= lbound(fldptr1,1) .or. &
        ubound(fldptr2,2) /= ubound(fldptr1,2) .or. &
        ubound(fldptr2,1) /= ubound(fldptr1,1)) then
      call ESMF_LogWrite(trim(subname)//": ERROR in data size "//trim(cstring), ESMF_LOGMSG_ERROR, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      write(msgString,*) trim(subname)//': fldptr2 ',lbound(fldptr2),ubound(fldptr2)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
      write(msgString,*) trim(subname)//': fldptr1 ',lbound(fldptr1),ubound(fldptr1)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=dbrc)
    else
      FldPtr_SameCheck = .true.
    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end function FldPtr_SameCheck

!-----------------------------------------------------------------------------
  subroutine FldGrid_Print(field, string, rc)

    type(ESMF_Field), intent(in)  :: field
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Grid)     :: grid
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:)
    character(len=*),parameter  :: subname='(module_MEDIATOR:FldGrid_Print)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    call ESMF_FieldGet(field, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call Grid_Print(grid, string, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(field,farrayPtr=dataptr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write (msgString,*) trim(subname)//":"//trim(string)//": dataptr bounds dim=1 ",lbound(dataptr,1),ubound(dataptr,1)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write (msgString,*) trim(subname)//":"//trim(string)//": dataptr bounds dim=2 ",lbound(dataptr,2),ubound(dataptr,2)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine FldGrid_Print

!-----------------------------------------------------------------------------
  subroutine Grid_Print(grid, string, rc)

    type(ESMF_Grid) , intent(in)  :: grid
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc

    type(ESMF_Distgrid) :: distgrid  
    character(ESMF_MAXSTR)      :: transferAction
    integer                     :: localDeCount
    integer                     :: dimCount, tileCount
    integer, allocatable        :: minIndexPTile(:,:), maxIndexPTile(:,:)
    character(len=*),parameter  :: subname='(module_MEDIATOR:Grid_Print)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif
    rc = ESMF_SUCCESS

    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
   
    write (msgString,*) trim(subname)//":"//trim(string)//": localDeCount=", localDeCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    write (msgString,*) trim(subname)//":"//trim(string)//": dimCount=", dimCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write (msgString,*) trim(subname)//":"//trim(string)//": tileCount=", tileCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
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
      
    write (msgString,*) trim(subname)//":"//trim(string)//": minIndexPTile=", minIndexPTile
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write (msgString,*) trim(subname)//":"//trim(string)//": maxIndexPTile=", maxIndexPTile
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    deallocate(minIndexPTile, maxIndexPTile)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine Grid_Print

!-----------------------------------------------------------------------------

  subroutine Grid_Write(grid, string, rc)
    type(ESMF_Grid) ,intent(in)  :: grid
    character(len=*),intent(in)  :: string
    integer         ,intent(out) :: rc
  
    ! local 
    type(ESMF_Array)            :: array
    character(len=*),parameter  :: subname='(module_MEDIATOR:Grid_Write)'

    ! -- centers --

    call ESMF_GridGetCoord(grid, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      call ESMF_GridGetCoord(grid, coordDim=1, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call Array_diagnose(array,trim(string)//"_grid_coord1", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayWrite(array, file=trim(string)//"_grid_coord1.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_GridGetCoord(grid, coordDim=2, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call Array_diagnose(array,trim(string)//"_grid_coord2", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayWrite(array, file=trim(string)//"_grid_coord2.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif

    ! -- corners --

    call ESMF_GridGetCoord(grid, &
      staggerLoc=ESMF_STAGGERLOC_CORNER, isPresent=isPresent, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      call ESMF_GridGetCoord(grid, coordDim=1, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) then
        call Array_diagnose(array,trim(string)//"_grid_corner1", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayWrite(array, file=trim(string)//"_grid_corner1.nc", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif
      call ESMF_GridGetCoord(grid, coordDim=2, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) then
        call Array_diagnose(array,trim(string)//"_grid_corner2", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
        call ESMF_ArrayWrite(array, file=trim(string)//"_grid_corner2.nc", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return
      endif
    endif

    ! -- mask --

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)    
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call Array_diagnose(array,trim(string)//"_grid_mask", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayWrite(array, file=trim(string)//"_grid_mask.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif

    ! -- area --

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)    
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)    
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call Array_diagnose(array,trim(string)//"_grid_area", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
      call ESMF_ArrayWrite(array, file=trim(string)//"_grid_area.nc", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif

  end subroutine Grid_Write

!-----------------------------------------------------------------------------

  subroutine fld_list_add(fldlist, stdname, transferOffer, mapping)
    ! ----------------------------------------------
    ! Set up a list of field information
    ! ----------------------------------------------
    type(fld_list_type), intent(inout)  :: fldlist
    character(len=*),    intent(in)     :: stdname
    character(len=*),    intent(in)     :: transferOffer
    character(len=*),    intent(in), optional  :: mapping

    ! local variables
    integer :: cnum    ! current size of array
    integer :: nnum    ! new size of array
    integer :: rc
    character(len=256), pointer :: tmpString(:)
    character(len=*), parameter :: subname='(module_MEDIATOR:fld_list_add)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

    ! make sure that stdname is in the NUOPC Field Dictionary 
    call NUOPC_FieldDictionaryGetEntry(stdname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=trim(subname)//&
      ": invalid stdname: "//trim(stdname), &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! potentially extend the existing lists

    if (fldlist%num < 0) then
       nnum = 10
       fldlist%num = 0
       allocate(fldlist%stdname(nnum))
       allocate(fldlist%shortname(nnum))
       allocate(fldlist%transferOffer(nnum))
       allocate(fldlist%mapping(nnum))
    endif

    cnum = size(fldlist%stdname)
    if (fldlist%num > cnum) then
      call ESMF_LogWrite(trim(subname)//&
      ": ERROR in num for fld "//trim(stdname), ESMF_LOGMSG_ERROR)
      return
    endif
    if (fldlist%num == cnum) then
      nnum = cnum + 10
      allocate(tmpString(cnum))
      tmpString(1:cnum) = fldlist%stdname(1:cnum)
      deallocate(fldlist%stdname)
      allocate(fldlist%stdname(nnum))
      fldlist%stdname(1:cnum) = tmpString(1:cnum)
      tmpString(1:cnum) = fldlist%shortname(1:cnum)
      deallocate(fldlist%shortname)
      allocate(fldlist%shortname(nnum))
      fldlist%shortname(1:cnum) = tmpString(1:cnum)
      tmpString(1:cnum) = fldlist%transferOffer(1:cnum)
      deallocate(fldlist%transferOffer)
      allocate(fldlist%transferOffer(nnum))
      fldlist%transferOffer(1:cnum) = tmpString(1:cnum)
      tmpString(1:cnum) = fldlist%mapping(1:cnum)
      deallocate(fldlist%mapping)
      allocate(fldlist%mapping(nnum))
      fldlist%mapping(1:cnum) = tmpString(1:cnum)
      deallocate(tmpString)
    endif
    
    ! fill in the new entry

    fldlist%num = fldlist%num + 1
    fldlist%stdname       (fldlist%num) = trim(stdname)
    fldlist%shortname     (fldlist%num) = trim(stdname)
    fldlist%transferOffer (fldlist%num) = trim(transferOffer)
    if (present(mapping)) then
       if (trim(mapping) /= 'conserve' .and. trim(mapping) /= 'bilinear' .and. trim(mapping) /= 'patch') then
          call ESMF_LogWrite(trim(subname)//": ERROR mapping not allowed "//trim(mapping), ESMF_LOGMSG_ERROR, rc=rc)
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
       endif
       fldlist%mapping    (fldlist%num) = trim(mapping)
    else
       fldlist%mapping    (fldlist%num) = 'undefined'
    endif

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO, rc=dbrc)
    endif

  end subroutine fld_list_add

  !-----------------------------------------------------------------------------

end module
#endif
