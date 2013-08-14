



































      MODULE module_HYCOM_GRID_COMP

      USE ESMF_MOD

      USE module_ERR_MSG,ONLY: ERR_MSG,MESSAGE_CHECK

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: HYCOM_REGISTER

      INTEGER :: DUMMY

      CONTAINS

!#######################################################################

      SUBROUTINE HYCOM_REGISTER(HYCOM_GRID_COMP,RC_REG)
      TYPE(ESMF_GridComp)               :: HYCOM_GRID_COMP
      INTEGER            ,INTENT(OUT)   :: RC_REG

      INTEGER :: RC

!     write(0,*) "    HYCOM_REGISTER stub"

      CALL ESMF_GridCompSetEntryPoint(HYCOM_GRID_COMP ,ESMF_SETINIT ,HYCOM_INITIALIZE ,ESMF_SINGLEPHASE ,RC)
      CALL ESMF_GridCompSetEntryPoint(HYCOM_GRID_COMP ,ESMF_SETRUN  ,HYCOM_RUN        ,ESMF_SINGLEPHASE ,RC)
      CALL ESMF_GridCompSetEntryPoint(HYCOM_GRID_COMP ,ESMF_SETFINAL,HYCOM_FINALIZE   ,ESMF_SINGLEPHASE ,RC)

      RC_REG = ESMF_SUCCESS
!     write(0,*) "    END OF HYCOM_REGISTER stub"

      END SUBROUTINE HYCOM_REGISTER

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE HYCOM_INITIALIZE(HYCOM_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_HYCOM ,RC_INIT)

      TYPE(ESMF_GridComp)               :: HYCOM_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_HYCOM
      INTEGER            ,INTENT(OUT)   :: RC_INIT

!     write(0,*) "        HYCOM_INITIALIZE stub"
      RC_INIT = ESMF_SUCCESS
!     write(0,*) "        END OF HYCOM_INITIALIZE stub"

      END SUBROUTINE HYCOM_INITIALIZE

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&






!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE HYCOM_RUN(HYCOM_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_HYCOM ,RC_RUN)

      TYPE(ESMF_GridComp)               :: HYCOM_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_HYCOM
      INTEGER            ,INTENT(OUT)   :: RC_RUN

      INTEGER :: RC

!     write(0,*) "        HYCOM_RUN stub"

!      MESSAGE_CHECK="In HYCOM Run"
!      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)

      RC_RUN=ESMF_SUCCESS
!     write(0,*) "        END OF HYCOM_RUN stub"

      END SUBROUTINE HYCOM_RUN

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE HYCOM_FINALIZE(HYCOM_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_HYCOM ,RC_FINALIZE)

      TYPE(ESMF_GridComp)               :: HYCOM_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_HYCOM
      INTEGER            ,INTENT(OUT)   :: RC_FINALIZE

!     write(0,*) "        HYCOM_FINALIZE stub"
      RC_FINALIZE=ESMF_SUCCESS
!     write(0,*) "        END OF HYCOM_FINALIZE stub"

      END SUBROUTINE HYCOM_FINALIZE

!#######################################################################

      END MODULE module_HYCOM_GRID_COMP
