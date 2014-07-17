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
!          CORE component (GFS, NMM, FIM, GEN, etc.)
!
!-----------------------------------------------------------------------
!
      USE esmf_mod

#ifdef WITH_NUOPC
      use NUOPC
      use NUOPC_DriverAtmOcn, only: &
        driver_routine_SS             => routine_SetServices, &
        driver_type_IS                => type_InternalState, &
        driver_label_IS               => label_InternalState, &
        driver_label_SetModelPetLists => label_SetModelPetLists, &
        driver_label_SetModelServices => label_SetModelServices
      use NUOPC_Connector, only: cplSS => routine_SetServices
#ifdef FRONT_OCN_DUMMY
      use FRONT_OCN_DUMMY,  only: OCN_DUMMY_SS  => SetServices
#endif
#ifdef FRONT_HYCOM
      use FRONT_HYCOM,      only: OCN_HYCOM_SS  => SetServices
#endif
#ifdef FRONT_MOM5
      use FRONT_MOM5,       only: OCN_MOM5_SS   => SetServices
#endif
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

      ! NUOPC_DriverAtmOcn registers the generic methods

      call driver_routine_SS(EARTH_GRID_COMP, rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)

      ! attach specializing method(s)

      call ESMF_MethodAdd(EARTH_GRID_COMP, label=driver_label_SetModelPetLists,&
        userRoutine=SetModelPetLists, rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      
      call ESMF_MethodAdd(EARTH_GRID_COMP, label=driver_label_SetModelServices,&
        userRoutine=SetModelServices, rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      
      ! create, open, and set the config
      
      config = ESMF_ConfigCreate(rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      call ESMF_ConfigLoadFile(config, "nems.configure", rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      call ESMF_GridCompSet(EARTH_GRID_COMP, config=config, rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      
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

      subroutine SetModelPetLists(gcomp, rc)
        type(ESMF_GridComp)  :: gcomp
        integer, intent(out) :: rc
        
        ! local variables
        integer                       :: localrc
        type(driver_type_IS)          :: is
        integer                       :: petCount, i
        integer                       :: petListBounds(2)
        type(ESMF_Config)             :: config

        rc = ESMF_SUCCESS

        ! query Component for its internal State
        nullify(is%wrap)
        call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, is, rc)
        ESMF_ERR_RETURN(rc,rc)
          
        ! get petCount and config
        call ESMF_GridCompGet(gcomp, petCount=petCount, config=config, rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        
        ! determine the ATM petList bounds
        call ESMF_ConfigGetAttribute(config, petListBounds, &
          label="atm_petlist_bounds:", default=-1, rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (petListBounds(1)==-1 .and. petListBounds(2)==-1) then
          petListBounds(1) = 0
          petListBounds(2) = petCount - 1
        endif
!print *, "atm_petlist_bounds:", petListBounds
        
        ! set petList for ATM
        allocate(is%wrap%atmPetList(petListBounds(2)-petListBounds(1)+1))
        do i=petListBounds(1), petListBounds(2)
          is%wrap%atmPetList(i-petListBounds(1)+1) = i ! PET labeling is 0 based
        enddo
          
        ! determine the OCN petList bounds
        call ESMF_ConfigGetAttribute(config, petListBounds, &
          label="ocn_petlist_bounds:", default=-1, rc=rc)
        ESMF_ERR_RETURN(rc,rc)
        if (petListBounds(1)==-1 .and. petListBounds(2)==-1) then
          petListBounds(1) = 0
          petListBounds(2) = petCount - 1
        endif
!print *, "ocn_petlist_bounds:", petListBounds
        
        ! set petList for OCN -> first 146Pets
        allocate(is%wrap%ocnPetList(petListBounds(2)-petListBounds(1)+1))
        do i=petListBounds(1), petListBounds(2)
          is%wrap%ocnPetList(i-petListBounds(1)+1) = i ! PET labeling is 0 based
        enddo
        
      end subroutine

      subroutine SetModelServices(gcomp, rc)
        type(ESMF_GridComp)  :: gcomp
        integer, intent(out) :: rc

        ! local variables
        integer                       :: localrc
        type(driver_type_IS)          :: is
        type(ESMF_Clock)              :: internalClock
        type(ESMF_Config)             :: config
        character(len=20)             :: model
        character(len=160)            :: msg
        real(ESMF_KIND_R8)            :: atmOcnCoulingHours
        type(ESMF_TimeInterval)       :: couplingStep
        logical                       :: atmFlag, ocnFlag

        rc = ESMF_SUCCESS

        ! query Component for its internal State
        nullify(is%wrap)
        call ESMF_UserCompGetInternalState(gcomp, driver_label_IS, is, rc)
        ESMF_ERR_RETURN(rc,rc)
        
        ! get config
        call ESMF_GridCompGet(gcomp, config=config, rc=rc)
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
          call ESMF_GridCompSetServices(is%wrap%atm, ATM_REGISTER, &
            userRc=localrc, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ESMF_ERR_RETURN(localrc,rc)
          call ESMF_AttributeSet(is%wrap%atm, &
            name="Verbosity", value="high", &
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
          call ESMF_GridCompSetServices(is%wrap%ocn, OCN_DUMMY_SS, &
            userRc=localrc, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ESMF_ERR_RETURN(localrc,rc)
          call ESMF_AttributeSet(is%wrap%ocn, &
            name="Verbosity", value="high", &
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
          call ESMF_GridCompSetServices(is%wrap%ocn, OCN_HYCOM_SS, &
            userRc=localrc, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ESMF_ERR_RETURN(localrc,rc)
          call ESMF_AttributeSet(is%wrap%ocn, &
            name="Verbosity", value="high", &
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
          call ESMF_GridCompSetServices(is%wrap%ocn, OCN_MOM5_SS, &
            userRc=localrc, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ESMF_ERR_RETURN(localrc,rc)
          call ESMF_AttributeSet(is%wrap%ocn, &
            name="Verbosity", value="high", &
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
        
        if (atmFlag .and. ocnFlag) then
          ! Both ATM and OCN present -> SetServices for Connectors
          ! SetServices for atm2ocn
          call ESMF_CplCompSetServices(is%wrap%atm2ocn, cplSS, userRc=localrc, &
            rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ESMF_ERR_RETURN(localrc,rc)
          call ESMF_AttributeSet(is%wrap%atm2ocn, name="Verbosity", value="high", &
            convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ! SetServices for ocn2atm
          call ESMF_CplCompSetServices(is%wrap%ocn2atm, cplSS, userRc=localrc, &
            rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          ESMF_ERR_RETURN(localrc,rc)
          call ESMF_AttributeSet(is%wrap%ocn2atm, name="Verbosity", value="high", &
            convention="NUOPC", purpose="General", rc=rc)
          ESMF_ERR_RETURN(rc,rc)
        endif

        ! Get internal clock and set the coupling timeStep according to config
        call ESMF_ConfigGetAttribute(config, atmOcnCoulingHours, &
          label="atm_ocn_coupling_hours:", default=-1.0_ESMF_KIND_R8, rc=rc)
        if (atmOcnCoulingHours>0._ESMF_KIND_R8) then
          ! The coupling time step was provided
          call ESMF_TimeIntervalSet(couplingStep, h_r8=atmOcnCoulingHours, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_GridCompGet(gcomp, clock=internalClock, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
          call ESMF_ClockSet(internalClock, timeStep=couplingStep, rc=rc)
          ESMF_ERR_RETURN(rc,rc)
        else
          ! Keep the default timeStep, i.e. that of parent
        endif

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
      TYPE(ESMF_Clock)   ,INTENT(INOUT) :: CLOCK_NEMS                      !<-- The NEMS component ESMF Clock
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
