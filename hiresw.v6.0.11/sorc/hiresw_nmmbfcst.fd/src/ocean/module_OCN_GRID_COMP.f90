



































! #define WITH_NUOPC

!-----------------------------------------------------------------------
!
      MODULE module_OCN_GRID_COMP
!
!-----------------------------------------------------------------------
!***  This module contains codes directly related to the OCN component.
!-----------------------------------------------------------------------
!
!***  The OCN component lies in the heirarchy seen here:
!
!          Main program
!               |
!               |
!          NEMS component
!               |     |________________________.
!               |                              |
!          EARTH component        Ensemble Coupler component
!               |
!               |
!          OCN/OCN/ICE components
!               |
!               |
!          CORE component (GFS, NMM, FIM, GEN, etc.)
!
!-----------------------------------------------------------------------
!  2011-05-11  Theurich & Yang  - Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!
      USE ESMF_MOD
!
!
      USE module_OCN_INTERNAL_STATE,ONLY: OCN_INTERNAL_STATE            &
                                         ,WRAP_OCN_INTERNAL_STATE

!-----------------------------------------------------------------------
! The different ocean module core registers go here
!-----------------------------------------------------------------------
      USE module_HYCOM_GRID_COMP,ONLY: HYCOM_REGISTER

      USE module_GENOCN_GRID_COMP,ONLY: GENOCN_REGISTER   ! For the "Generic Ocen Core" gridded component.

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
      PUBLIC :: OCN_REGISTER
!
!-----------------------------------------------------------------------
!
      TYPE(OCN_INTERNAL_STATE),POINTER,SAVE :: OCN_INT_STATE
      TYPE(WRAP_OCN_INTERNAL_STATE)   ,SAVE :: WRAP
!
      TYPE(ESMF_Clock),SAVE :: CLOCK_OCN                                   !<-- The Clock of the OCN component
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE OCN_REGISTER(OCN_GRID_COMP,RC_REG)
!
!-----------------------------------------------------------------------
!***  Register the Init, Run, and Finalize routines of 
!***  the OCN component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: OCN_GRID_COMP                   !<-- The OCN component
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



! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for OCN Initialize"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(OCN_GRID_COMP                     &  !<-- The OCN component
                                     ,ESMF_SETINIT                      &  !<-- Subroutine type (Initialize)
                                     ,OCN_INITIALIZE                    &  !<-- User's subroutine name
                                     ,ESMF_SINGLEPHASE                  &
                                     ,RC)



!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!


!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for OCN Run"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!


      CALL ESMF_GridCompSetEntryPoint(OCN_GRID_COMP                     &  !<-- The OCN component
                                     ,ESMF_SETRUN                       &  !<-- Subroutine type (Run)
                                     ,OCN_RUN                           &  !<-- User's subroutine name
                                     ,ESMF_SINGLEPHASE                  &
                                     ,RC)







!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------


!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for OCN Finalize"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!

      CALL ESMF_GridCompSetEntryPoint(OCN_GRID_COMP                     &  !<-- The OCN component
                                     ,ESMF_SETFINAL                     &  !<-- Subroutine type (Finalize)
                                     ,OCN_FINALIZE                      &  !<-- User's subroutine name
                                     ,ESMF_SINGLEPHASE                  &
                                     ,RC)







!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
      IF(RC_REG==ESMF_SUCCESS)THEN
!       WRITE(0,*)' OCN_REGISTER succeeded'
      ELSE
        WRITE(0,*)' OCN_REGISTER failed  RC_REG=',RC_REG
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE OCN_REGISTER
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE OCN_INITIALIZE(OCN_GRID_COMP                           &
                               ,IMP_STATE                               &
                               ,EXP_STATE                               &
                               ,CLOCK_EARTH                             &
                               ,RC_INIT)
!
!-----------------------------------------------------------------------
!***  The Initialize step of the OCN component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: OCN_GRID_COMP                   !<-- The OCN component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The OCN import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The OCN export state
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
      RC_INIT = ESMF_SUCCESS

!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!***  Allocate the OCN component's internal state, point at it,
!***  and attach it to the OCN component.
!-----------------------------------------------------------------------

      ALLOCATE(OCN_INT_STATE,stat=RC)
      wrap%OCN_INT_STATE=>OCN_INT_STATE
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set the OCN Internal State"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetInternalState(OCN_GRID_COMP                  &
                                        ,WRAP                           &
                                        ,RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the EARTH Clock within
!***  the OCN component.
!-----------------------------------------------------------------------
!
      ocn_int_state%CLOCK_OCN=CLOCK_EARTH
!
!-----------------------------------------------------------------------
!***  Create the configure object for the OCN configure file which
!***  specifies the dynamic core.
!-----------------------------------------------------------------------
!
      CF=ESMF_ConfigCreate(rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Load the OCN configure file"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ConfigLoadFile(config=CF ,filename='ocean.configure' ,rc=RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Attach the configure object to the OCN component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Attach the configure file to the OCN component"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSet(gridcomp=OCN_GRID_COMP                      &  !<-- The OCN component
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
      MESSAGE_CHECK="Extract dynamic core from the OCN configure file"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ConfigGetAttribute(config=CF                            &  !<-- The OCN configure object
                                  ,value =ocn_int_state%CORE            &  !<-- The dynamic core name
                                  ,label ='core:'                       &  !<-- The label in the configure file
                                  ,rc    =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Create the OCN subcomponent and its associated import/export
!***  states for the core name that was extracted.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the OCN CORE component"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      ocn_int_state%CORE_GRID_COMP=ESMF_GridCompCreate(name=TRIM(ocn_int_state%CORE)//' component' &
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
!     MESSAGE_CHECK="Attach the configure file to the OCN CORE component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!     CALL ESMF_GridCompSet(gridcomp=ocn_int_state%CORE_GRID_COMP       &  !<-- The OCN component
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
      MESSAGE_CHECK="Register the OCN CORE component's Init, Run, and Finalize steps"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      SELECT CASE(ocn_int_state%CORE)
!
        CASE('hycom')
          CALL ESMF_GridCompSetServices (ocn_int_state%CORE_GRID_COMP   &
                                        ,HYCOM_REGISTER                 &
                                        ,RC)
        CASE('genocn')
          CALL ESMF_GridCompSetServices (ocn_int_state%CORE_GRID_COMP   &
                                        ,HYCOM_REGISTER                 &
                                        ,RC)
        CASE DEFAULT
          write(0,*)' OCN_INITIALIZE requires unknown core: ',TRIM(ocn_int_state%CORE)                      
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
      MESSAGE_CHECK="Create the OCN CORE import state"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      ocn_int_state%CORE_IMP_STATE=ESMF_StateCreate(stateName="CORE Import"     &
                                                   ,statetype=ESMF_STATE_IMPORT &
                                                   ,rc       =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the OCN CORE export state"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      ocn_int_state%CORE_EXP_STATE=ESMF_StateCreate(stateName="CORE Export"     &
                                                   ,statetype=ESMF_STATE_EXPORT &
                                                   ,rc       =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Nest the import/export states of the CORE component into the
!***  analgous states of the OCN component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK= "Add the CORE states into the OCN states"
      CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_StateAdd(state      =IMP_STATE                          &
                        ,nestedState=ocn_int_state%CORE_IMP_STATE       &
                        ,rc         =RC)
!
      CALL ESMF_StateAdd(state      =EXP_STATE                          &
                        ,nestedState=ocn_int_state%CORE_EXP_STATE       &
                        ,rc         =RC)

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
      MESSAGE_CHECK="Initialize the OCN CORE component"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      CALL ESMF_GridCompInitialize(gridcomp   =ocn_int_state%CORE_GRID_COMP &
                                  ,importState=ocn_int_state%CORE_IMP_STATE &
                                  ,exportState=ocn_int_state%CORE_EXP_STATE &
                                  ,clock      =ocn_int_state%CLOCK_OCN      &
                                  ,phase      =ESMF_SINGLEPHASE             &
                                  ,rc         =RC)

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      IF(RC_INIT==ESMF_SUCCESS)THEN
!       WRITE(0,*)' OCN_INITIALIZE succeeded'
      ELSE
        WRITE(0,*)' OCN_INITIALIZE failed  RC_INIT=',RC_INIT
      ENDIF
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
      END SUBROUTINE OCN_INITIALIZE
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE OCN_RUN(OCN_GRID_COMP                                  &
                        ,IMP_STATE                                      &
                        ,EXP_STATE                                      &
                        ,CLOCK_EARTH                                    &
                        ,RC_RUN)
!
!-----------------------------------------------------------------------
!***  The Run step of the OCN component.
!-----------------------------------------------------------------------

!------------------------
!***  Argument Variables
!------------------------
! 
      TYPE(ESMF_GridComp)               :: OCN_GRID_COMP                   !<-- The OCN component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The OCN import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The OCN export state
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


!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the EARTH Clock within
!***  the OCN component.
!-----------------------------------------------------------------------
!
      ocn_int_state%CLOCK_OCN=CLOCK_EARTH
!
!-----------------------------------------------------------------------
!***  Execute the Run step of the selected dynamic core.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      MESSAGE_CHECK="Execute the Run step of the OCN CORE component"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompRun(gridcomp   =ocn_int_state%CORE_GRID_COMP    &
                           ,importState=ocn_int_state%CORE_IMP_STATE    &
                           ,exportState=ocn_int_state%CORE_EXP_STATE    &
                           ,clock      =ocn_int_state%CLOCK_OCN         &
                           ,phase      =ESMF_SINGLEPHASE                & 
                           ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Update the OCN clock.
!-----------------------------------------------------------------------

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK = "Update the current time of the OCN clock"
      CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ClockGet(clock      =ocn_int_state%CLOCK_OCN            &
                        ,startTime  =STARTTIME                          &
                        ,runDuration=RUNDURATION                        &
                        ,rc         =RC)
!
      CURRTIME=STARTTIME+RUNDURATION
!
      CALL ESMF_ClockSet(clock   =ocn_int_state%CLOCK_OCN               &
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
!       WRITE(0,*)' OCN_RUN succeeded'
      ELSE
        WRITE(0,*)' OCN_RUN failed  RC_RUN=',RC_RUN
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE OCN_RUN
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------




!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------

!
      SUBROUTINE OCN_FINALIZE(OCN_GRID_COMP                             &
                             ,IMP_STATE                                 &
                             ,EXP_STATE                                 &
                             ,CLOCK_EARTH                               &
                             ,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!***  Finalize the OCN component.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp)               :: OCN_GRID_COMP                   !<-- The OCN component
      TYPE(ESMF_State)                  :: IMP_STATE                       !<-- The OCN import state
      TYPE(ESMF_State)                  :: EXP_STATE                       !<-- The OCN import state
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
      MESSAGE_CHECK="Execute the Finalize step of the OCN CORE component"
      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompFinalize(gridcomp   =ocn_int_state%CORE_GRID_COMP &
                                ,importState=ocn_int_state%CORE_IMP_STATE &
                                ,exportState=ocn_int_state%CORE_EXP_STATE &
                                ,clock      =ocn_int_state%CLOCK_OCN      &
                                ,phase      =ESMF_SINGLEPHASE             &
                                ,rc         =RC)

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_FINALIZE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      IF(RC_FINALIZE==ESMF_SUCCESS)THEN
!       WRITE(0,*)' OCN_FINALIZE succeeded'
      ELSE
        WRITE(0,*)' OCN_FINALIZE failed  RC_FINALIZE=',RC_FINALIZE
      ENDIF

!-----------------------------------------------------------------------
      END SUBROUTINE OCN_FINALIZE
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
      END MODULE module_OCN_GRID_COMP
!-----------------------------------------------------------------------

