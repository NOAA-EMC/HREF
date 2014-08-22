#include "../ESMFVersionDefine.h"

!-----------------------------------------------------------------------
!
      MODULE module_ATM_GRID_COMP
!
!-----------------------------------------------------------------------
!***  This module contains codes directly related to the ATM component.
!-----------------------------------------------------------------------
!
!***  The ATM component lies in the hierarchy seen here:
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
!          CORE component (GFS, NMM, FIM, GEN, etc.)
!
!-----------------------------------------------------------------------
!  2011-05-11  Theurich & Yang  - Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!  2011-10/04  Yang  - Modified for using the ESMF 5.2.0r library.
!  2013-07     Theurich - NUOPC option to be compliant with ESMF 6.2.0 reference.
!-----------------------------------------------------------------------
!
      USE esmf_mod

#ifdef WITH_NUOPC
      use NUOPC
      use NUOPC_Model, only: &
        model_routine_SS    => routine_SetServices, &
        model_label_Advance => label_Advance
#endif

!
      USE module_ATM_INTERNAL_STATE,ONLY: ATM_INTERNAL_STATE            &
                                         ,WRAP_ATM_INTERNAL_STATE
!
      USE module_NMM_GRID_COMP,ONLY: NMM_REGISTER
      USE module_GFS_GRID_COMP,ONLY: GFS_REGISTER
      USE module_FIM_GRID_COMP,ONLY: FIM_REGISTER
      USE module_GEN_GRID_COMP,ONLY: GEN_REGISTER   ! For the "Generic Core" gridded component.
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
      PUBLIC :: ATM_REGISTER
!
!-----------------------------------------------------------------------
!
      TYPE(ATM_INTERNAL_STATE),POINTER,SAVE :: ATM_INT_STATE
      TYPE(WRAP_ATM_INTERNAL_STATE)   ,SAVE :: WRAP
!
      TYPE(ESMF_Clock),SAVE :: CLOCK_ATM                                   !<-- The Clock of the ATM component
!
#ifdef WITH_NUOPC
      character(len=160) :: nuopcMsg
#endif

!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE ATM_REGISTER(ATM_GRID_COMP,RC_REG)
!
!-----------------------------------------------------------------------
!***  Register the Init, Run, and Finalize routines of 
!***  the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: ATM_GRID_COMP                   !<-- The ATM component
      INTEGER            ,INTENT(OUT)   :: RC_REG                          !<-- Error return code
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

#ifdef WITH_NUOPC
      ! the NUOPC model component will register the generic methods
      call model_routine_SS(ATM_GRID_COMP, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      ! Provide InitP0 to overwrite the default IPD00 with IPD01
      call ESMF_GridCompSetEntryPoint(ATM_GRID_COMP, ESMF_METHOD_INITIALIZE, &
        InitializeP0, phase=0, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! NUOPC_Model IPDv00 requires InitP1, where Fields should be advertised
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP, ESMF_METHOD_INITIALIZE, &
        InitializeP1, phase=1, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! NUOPC_Model IPDv00 requires InitP2, where Fields should be realized,
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP, ESMF_METHOD_INITIALIZE, &
        InitializeP2, phase=2, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! attach specializing method(s)
      call ESMF_MethodAdd(ATM_GRID_COMP, label=model_label_Advance, &
        userRoutine=ATM_ADVANCE, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      ! Overwrite generic NUOPC_Model Finalize method
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP, ESMF_METHOD_FINALIZE, &
        ATM_FINALIZE, phase=1, rc=RC_REG)
      if (ESMF_LogFoundError(rcToCheck=RC_REG, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      ! extend the NUOPC Field Dictionary to hold required entries
      if (.not.NUOPC_FieldDictionaryHasEntry("air_temperature_at_sea_level")) then
        call NUOPC_FieldDictionaryAddEntry( &
          standardName="air_temperature_at_sea_level", &
          canonicalUnits="K", defaultLongName="Air Temperature at Sea Level", &
          defaultShortName="tmsl", rc=rc);
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif

#else

!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for ATM Initialize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP                     &  !<-- The ATM component
                                     ,ESMF_METHOD_INITIALIZE            &  !<-- Subroutine type (Initialize)
                                     ,ATM_INITIALIZE                    &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for ATM Run"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!

      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP                     &  !<-- The ATM component
                                     ,ESMF_METHOD_RUN                   &  !<-- Subroutine type (Run)
                                     ,ATM_RUN                           &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for ATM Finalize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(ATM_GRID_COMP                     &  !<-- The ATM component
                                     ,ESMF_METHOD_FINALIZE              &  !<-- Subroutine type (Finalize)
                                     ,ATM_FINALIZE                      &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
#endif
!-----------------------------------------------------------------------
!
      IF(RC_REG==ESMF_SUCCESS)THEN
!       WRITE(0,*)' ATM_REGISTER succeeded'
      ELSE
        WRITE(0,*)' ATM_REGISTER failed  RC_REG=',RC_REG
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE ATM_REGISTER
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
#ifdef WITH_NUOPC
  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! local variables    
    character(len=NUOPC_PhaseMapStringLength) :: initPhases(4)
    
    rc = ESMF_SUCCESS

    initPhases(1) = "IPDv01p1=1"
    ! skip over IPDv01p2, which isn't needed here
    initPhases(2) = "IPDv01p3=2"
    initPhases(3) = "IPDv01p4=3"
    initPhases(4) = "IPDv01p5=4"
    
    call ESMF_AttributeSet(gcomp, &
      name="InitializePhaseMap", valueList=initPhases, &
      convention="NUOPC", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS

    ! advertise Fields

    ! importable field: sea_surface_temperature
    call NUOPC_StateAdvertiseField(importState, &
      StandardName="sea_surface_temperature", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="air_pressure_at_sea_level", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! exportable field: air_temperature_at_sea_level
    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="air_temperature_at_sea_level", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
  end subroutine
  
  !-----------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(ESMF_Grid)         :: gridIn, gridOut
    type(ESMF_Field)        :: field

    rc = ESMF_SUCCESS
    
    ! call into the actual NEMS/ATM initialize rooutine
    
    call ATM_INITIALIZE(gcomp, importState, exportState, clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! create a DUMMY Grid object for import and export Fields
    
    gridIn = NUOPC_GridCreateSimpleXY( &
      x_min=10._ESMF_KIND_R8,   x_max=20._ESMF_KIND_R8, &
      y_min=100._ESMF_KIND_R8,  y_max=200._ESMF_KIND_R8, &
      i_count=1000, j_count=100, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    gridOut = gridIn
      
    ! conditionally realize or remove Fields
    
    ! importable field: sea_surface_temperature
    if (NUOPC_StateIsFieldConnected(importState, fieldName="sst")) then
      ! realize a connected Field
      field = ESMF_FieldCreate(name="sst", grid=gridIn, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_StateRealizeField(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      ! remove a not connected Field from State
      call ESMF_StateRemove(importState, (/"sst"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
 
    ! exportable field: air_pressure_at_sea_level
    if (NUOPC_StateIsFieldConnected(exportState, fieldName="pmsl")) then
      ! realize a connected Field
      field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      ! remove a not connected Field from State
      call ESMF_StateRemove(exportState, (/"pmsl"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    
    ! exportable field: air_temperature_at_sea_level
    if (NUOPC_StateIsFieldConnected(exportState, fieldName="tmsl")) then
      ! realize a connected Field
      field = ESMF_FieldCreate(name="tmsl", grid=gridOut, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      ! remove a not connected Field from State
      call ESMF_StateRemove(exportState, (/"tmsl"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif

  end subroutine

!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
#endif

      SUBROUTINE ATM_INITIALIZE(ATM_GRID_COMP                           &
                               ,IMP_STATE                               &
                               ,EXP_STATE                               &
                               ,CLOCK_EARTH                             &
                               ,RC_INIT)
!
!-----------------------------------------------------------------------
!***  The Initialize step of the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: ATM_GRID_COMP                   !<-- The ATM component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The ATM import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The ATM export state
      TYPE(ESMF_Clock)                  :: CLOCK_EARTH                     !<-- The Clock of the EARTH component
      INTEGER            ,INTENT(OUT)   :: RC_INIT                         !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
      TYPE(ESMF_Config) :: CF
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_INIT = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
#ifdef WITH_NUOPC
      call NUOPC_ClockPrintCurrTime(CLOCK_EARTH, &
        string="entering ATM_INITIALIZE with CLOCK_EARTH current: ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStartTime(CLOCK_EARTH, &
        string="entering ATM_INITIALIZE with CLOCK_EARTH start:   ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStopTime(CLOCK_EARTH, &
        string="entering ATM_INITIALIZE with CLOCK_EARTH stop:    ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
#endif

!
!-----------------------------------------------------------------------
!***  Allocate the ATM component's internal state, point at it,
!***  and attach it to the ATM component.
!-----------------------------------------------------------------------
!
      ALLOCATE(ATM_INT_STATE,stat=RC)
      wrap%ATM_INT_STATE=>ATM_INT_STATE
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set the ATM Internal State"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetInternalState(ATM_GRID_COMP                  &
                                        ,WRAP                           &
                                        ,RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the EARTH Clock within
!***  the ATM component.
!-----------------------------------------------------------------------
!
#ifdef WITH_NUOPC
      call NUOPC_GridCompSetClock(ATM_GRID_COMP, CLOCK_EARTH, rc=RC_INIT)
      if (ESMF_LogFoundError(rcToCheck=RC_INIT, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      atm_int_state%CLOCK_ATM = ESMF_ClockCreate(CLOCK_EARTH, rc=RC_INIT)
      if (ESMF_LogFoundError(rcToCheck=RC_INIT, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

#else

      atm_int_state%CLOCK_ATM=CLOCK_EARTH

#endif

!
!-----------------------------------------------------------------------
!***  Create the configure object for the ATM configure file which
!***  specifies the dynamic core.
!-----------------------------------------------------------------------
!
      CF=ESMF_ConfigCreate(rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Load the ATM configure file"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
#ifdef WITH_NUOPC
      CALL ESMF_ConfigLoadFile(config=CF ,filename='nems.configure' ,rc=RC)
#else
      CALL ESMF_ConfigLoadFile(config=CF ,filename='atmos.configure' ,rc=RC)
#endif
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Attach the configure object to the ATM component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Attach the configure file to the ATM component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSet(gridcomp=ATM_GRID_COMP                      &  !<-- The ATM component
                           ,config  =CF                                 &  !<-- The associated configure object
                           ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Extract the dynamic core name from the configure file.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Extract dynamic core from the ATM configure file"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ConfigGetAttribute(config=CF                            &  !<-- The ATM configure object
                                  ,value =atm_int_state%CORE            &  !<-- The dynamic core name
#ifdef WITH_NUOPC
                                  ,label ='atm_model:'                  &  !<-- The label in the configure file
#else
                                  ,label ='core:'                       &  !<-- The label in the configure file
#endif
                                  ,rc    =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Create the ATM subcomponent and its associated import/export
!***  states for the core name that was extracted.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      atm_int_state%CORE_GRID_COMP=ESMF_GridCompCreate(name=TRIM(atm_int_state%CORE)//' component' &
                                                      ,rc  =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Attach the configure object to the CORE component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     MESSAGE_CHECK="Attach the configure file to the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!     CALL ESMF_GridCompSet(gridcomp=atm_int_state%CORE_GRID_COMP       &  !<-- The ATM component
!                          ,config  =CF                                 &  !<-- The associated configure object
!                          ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Register the subcomponent's Init, Run, and Finalize subroutines.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Register the CORE component's Init, Run, and Finalize steps"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      SELECT CASE(atm_int_state%CORE)
!
        CASE('nmm')
          CALL ESMF_GridCompSetServices (atm_int_state%CORE_GRID_COMP   &
                                        ,NMM_REGISTER                   &
                                        ,rc=RC)
!
        CASE('gfs')
          CALL ESMF_GridCompSetServices (atm_int_state%CORE_GRID_COMP   &
                                        ,GFS_REGISTER                   &
                                        ,rc=RC)
!
        CASE('gsm')
          CALL ESMF_GridCompSetServices (atm_int_state%CORE_GRID_COMP   &
                                        ,GFS_REGISTER                   &
                                        ,rc=RC)
!
        CASE('fim')
          CALL ESMF_GridCompSetServices (atm_int_state%CORE_GRID_COMP   &
                                        ,FIM_REGISTER                   &
                                        ,rc=RC)

        CASE('gen')
          CALL ESMF_GridCompSetServices (atm_int_state%CORE_GRID_COMP   &
                                        ,GEN_REGISTER                   &
                                        ,rc=RC)
        CASE DEFAULT
          write(0,*)' ATM_INITIALIZE requires unknown core: ',TRIM(atm_int_state%CORE)                      
!
      END SELECT
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Create the Core component's import/export states.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the CORE import state"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      atm_int_state%CORE_IMP_STATE=ESMF_StateCreate(STATENAME   = "CORE Import"            &
                                                   ,stateintent = ESMF_STATEINTENT_IMPORT  &
                                                   ,rc          = RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the CORE export state"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      atm_int_state%CORE_EXP_STATE=ESMF_StateCreate(STATENAME   = "CORE Export"            &
                                                   ,stateintent = ESMF_STATEINTENT_EXPORT  &
                                                   ,rc          = RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Nest the import/export states of the CORE component into the
!***  analgous states of the ATM component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK= "Add the CORE states into the ATMOS states"
!     CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
#ifndef WITH_NUOPC
! - Cannot bring these items out through the ATM Import and Export States 
! - under NUOPC, because NUOPC requires a minumum of Field metadata for 
! - anything that is going in/out of a component (e.g. to timestamp).
      CALL ESMF_StateAdd(IMP_STATE,LISTWRAPPER(atm_int_state%CORE_IMP_STATE),rc = RC)
      CALL ESMF_StateAdd(EXP_STATE,LISTWRAPPER(atm_int_state%CORE_EXP_STATE),rc = RC)
#endif
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Initialize the CORE component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Initialize the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompInitialize(gridcomp   =atm_int_state%CORE_GRID_COMP &
                                  ,importState=atm_int_state%CORE_IMP_STATE &
                                  ,exportState=atm_int_state%CORE_EXP_STATE &
                                  ,clock      =atm_int_state%CLOCK_ATM      &
                                  ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
      IF(RC_INIT==ESMF_SUCCESS)THEN
!       WRITE(0,*)' ATM_INITIALIZE succeeded'
      ELSE
        WRITE(0,*)' ATM_INITIALIZE failed  RC_INIT=',RC_INIT
      ENDIF
!
!-----------------------------------------------------------------------
!
#ifdef WITH_NUOPC
      call NUOPC_ClockPrintCurrTime(CLOCK_EARTH, &
        string="leaving  ATM_INITIALIZE with CLOCK_EARTH current: ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStartTime(CLOCK_EARTH, &
        string="leaving  ATM_INITIALIZE with CLOCK_EARTH start:   ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStopTime(CLOCK_EARTH, &
        string="leaving  ATM_INITIALIZE with CLOCK_EARTH stop:    ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)

      call NUOPC_ClockPrintCurrTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_INITIALIZE with CLOCK_ATM current: ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStartTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_INITIALIZE with CLOCK_ATM start:   ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStopTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_INITIALIZE with CLOCK_ATM stop:    ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
#endif
!
!-----------------------------------------------------------------------
!

      END SUBROUTINE ATM_INITIALIZE
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
#ifndef WITH_NUOPC

      SUBROUTINE ATM_RUN(ATM_GRID_COMP                                  &
                        ,IMP_STATE                                      &
                        ,EXP_STATE                                      &
                        ,CLOCK_EARTH                                    &
                        ,RC_RUN)
!
!-----------------------------------------------------------------------
!***  The Run step of the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
! 
      TYPE(ESMF_GridComp)               :: ATM_GRID_COMP                   !<-- The ATM component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The ATM import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The ATM export state
      TYPE(ESMF_Clock)                  :: CLOCK_EARTH                     !<-- The Clock of the EARTH component
      INTEGER            ,INTENT(OUT)   :: RC_RUN                          !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
      TYPE(ESMF_Time) :: CURRTIME                                       &
                        ,STARTTIME
!
      TYPE(ESMF_TimeInterval) :: RUNDURATION
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the EARTH Clock within
!***  the ATM component.
!-----------------------------------------------------------------------
!
      atm_int_state%CLOCK_ATM=CLOCK_EARTH
!
!-----------------------------------------------------------------------
!***  Execute the Run step of the selected dynamic core.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Run step of the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompRun(gridcomp   =atm_int_state%CORE_GRID_COMP    &
                           ,importState=atm_int_state%CORE_IMP_STATE    &
                           ,exportState=atm_int_state%CORE_EXP_STATE    &
                           ,clock      =atm_int_state%CLOCK_ATM         &
                           ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Update the ATMOS clock.
!-----------------------------------------------------------------------

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK = "Update the current time of the ATMOS clock"
!     CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ClockGet(clock      =atm_int_state%CLOCK_ATM            &
                        ,startTime  =STARTTIME                          &
                        ,runDuration=RUNDURATION                        &
                        ,rc         =RC)
!
      CURRTIME=STARTTIME+RUNDURATION
!
      CALL ESMF_ClockSet(clock   =atm_int_state%CLOCK_ATM               &
                        ,currTime=CURRTIME                              &
                        ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
      IF(RC_RUN==ESMF_SUCCESS)THEN
!       WRITE(0,*)' ATM_RUN succeeded'
      ELSE
        WRITE(0,*)' ATM_RUN failed  RC_RUN=',RC_RUN
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE ATM_RUN
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!

#else

      SUBROUTINE ATM_ADVANCE(ATM_GRID_COMP, rc)
!
!-----------------------------------------------------------------------
!***  Advance the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      type(ESMF_GridComp)   :: ATM_GRID_COMP
      integer, intent(out)  :: rc
!
!---------------------
!***  Local Variables
!---------------------
!
      type(ESMF_Clock)      :: clock
      type(ESMF_Time)       :: stopTime
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Use the internal Clock set by NUOPC layer for ATM but update stopTime
!-----------------------------------------------------------------------

      ! Component internal Clock gets updated per NUOPC rules
      call ESMF_GridCompGet(ATM_GRID_COMP, clock=clock, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
      
      ! The stopTime will be updated to be the next ATM-OCN coupling time
      call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
      ESMF_ERR_RETURN(rc,rc)
      
      ! Set the ATM-OCN coupling time to be stopTime in Clock that ATM core uses
      call ESMF_ClockSet(atm_int_state%CLOCK_ATM, stopTime=stopTime, rc=rc)
      ESMF_ERR_RETURN(rc,rc)

      call NUOPC_ClockPrintCurrTime(atm_int_state%CLOCK_ATM, &
        string="entering ATM_ADVANCE with CLOCK_ATM current: ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStartTime(atm_int_state%CLOCK_ATM, &
        string="entering ATM_ADVANCE with CLOCK_ATM start:   ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStopTime(atm_int_state%CLOCK_ATM, &
        string="entering ATM_ADVANCE with CLOCK_ATM stop:    ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)

!-----------------------------------------------------------------------
!***  Execute the Run step of the selected dynamic core.
!-----------------------------------------------------------------------

      CALL ESMF_GridCompRun(gridcomp   =atm_int_state%CORE_GRID_COMP    &
                           ,importState=atm_int_state%CORE_IMP_STATE    &
                           ,exportState=atm_int_state%CORE_EXP_STATE    &
                           ,clock      =atm_int_state%CLOCK_ATM         &
                           ,rc         =rc)
      ESMF_ERR_RETURN(rc,rc)

!-----------------------------------------------------------------------

      call NUOPC_ClockPrintCurrTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_ADVANCE with CLOCK_ATM current: ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStartTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_ADVANCE with CLOCK_ATM start:   ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)
      call NUOPC_ClockPrintStopTime(atm_int_state%CLOCK_ATM, &
        string="leaving  ATM_ADVANCE with CLOCK_ATM stop:    ", &
        unit=nuopcMsg)
      call ESMF_LogWrite(nuopcMsg, ESMF_LOGMSG_INFO)

!-----------------------------------------------------------------------

      END SUBROUTINE ATM_ADVANCE
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
#endif

      SUBROUTINE ATM_FINALIZE(ATM_GRID_COMP                             &
                             ,IMP_STATE                                 &
                             ,EXP_STATE                                 &
                             ,CLOCK_EARTH                               &
                             ,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!***  Finalize the ATM component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: ATM_GRID_COMP                   !<-- The ATM component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The ATM import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The ATM import state
      TYPE(ESMF_Clock)                  :: CLOCK_EARTH                     !<-- The Clock of the EARTH component
      INTEGER            ,INTENT(OUT)   :: RC_FINALIZE                     !<-- Error return code
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
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Finalize step of the CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompFinalize(gridcomp   =atm_int_state%CORE_GRID_COMP &
                                ,importState=atm_int_state%CORE_IMP_STATE &
                                ,exportState=atm_int_state%CORE_EXP_STATE &
                                ,clock      =atm_int_state%CLOCK_ATM      &
                                ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_FINALIZE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
#ifdef WITH_NUOPC
!-----------------------------------------------------------------------
!
      call ESMF_ClockDestroy(atm_int_state%CLOCK_ATM, rc=RC_FINALIZE)
      if (ESMF_LogFoundError(rcToCheck=RC_FINALIZE, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
#endif

      IF(RC_FINALIZE==ESMF_SUCCESS)THEN
!       WRITE(0,*)' ATM_FINALIZE succeeded'
      ELSE
        WRITE(0,*)' ATM_FINALIZE failed  RC_FINALIZE=',RC_FINALIZE
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE ATM_FINALIZE
!
!-----------------------------------------------------------------------
!
      END MODULE module_ATM_GRID_COMP
!
!-----------------------------------------------------------------------
