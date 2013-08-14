



































!  2012-03-28  P. Tripp - Generic ocean model stub.

      MODULE module_GENOCN_GRID_COMP

      USE ESMF_MOD

      USE module_ERR_MSG,ONLY: ERR_MSG,MESSAGE_CHECK

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: GENOCN_REGISTER

      INTEGER :: DUMMY

      CONTAINS

!#######################################################################

      SUBROUTINE GENOCN_REGISTER(GENOCN_GRID_COMP,RC_REG)
      TYPE(ESMF_GridComp)               :: GENOCN_GRID_COMP
      INTEGER            ,INTENT(OUT)   :: RC_REG

      INTEGER :: RC

      write(0,*) "    GENOCN_REGISTER stub"

      CALL ESMF_GridCompSetEntryPoint(GENOCN_GRID_COMP ,ESMF_SETINIT ,GENOCN_INITIALIZE ,ESMF_SINGLEPHASE ,RC)
      CALL ESMF_GridCompSetEntryPoint(GENOCN_GRID_COMP ,ESMF_SETRUN  ,GENOCN_RUN        ,ESMF_SINGLEPHASE ,RC)
      CALL ESMF_GridCompSetEntryPoint(GENOCN_GRID_COMP ,ESMF_SETFINAL,GENOCN_FINALIZE   ,ESMF_SINGLEPHASE ,RC)

      RC_REG = ESMF_SUCCESS
      write(0,*) "    END OF GENOCN_REGISTER stub"

      END SUBROUTINE GENOCN_REGISTER

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE GENOCN_INITIALIZE(GENOCN_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_GENOCN ,RC_INIT)

      TYPE(ESMF_GridComp)               :: GENOCN_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_GENOCN
      INTEGER            ,INTENT(OUT)   :: RC_INIT

      write(0,*) "        GENOCN_INITIALIZE stub"
      RC_INIT = ESMF_SUCCESS
      write(0,*) "        END OF GENOCN_INITIALIZE stub"

      END SUBROUTINE GENOCN_INITIALIZE

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&






!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE GENOCN_RUN(GENOCN_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_GENOCN ,RC_RUN)

      TYPE(ESMF_GridComp)               :: GENOCN_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_GENOCN
      INTEGER            ,INTENT(OUT)   :: RC_RUN

      INTEGER :: RC

      write(0,*) "        GENOCN_RUN stub"

!      MESSAGE_CHECK="In GENOCN Run"
!      CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)

      RC_RUN=ESMF_SUCCESS
      write(0,*) "        END OF GENOCN_RUN stub"

      END SUBROUTINE GENOCN_RUN

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE GENOCN_FINALIZE(GENOCN_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_GENOCN ,RC_FINALIZE)

      TYPE(ESMF_GridComp)               :: GENOCN_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_GENOCN
      INTEGER            ,INTENT(OUT)   :: RC_FINALIZE

      write(0,*) "        GENOCN_FINALIZE stub"
      RC_FINALIZE=ESMF_SUCCESS
      write(0,*) "        END OF GENOCN_FINALIZE stub"

      END SUBROUTINE GENOCN_FINALIZE

!#######################################################################

      END MODULE module_GENOCN_GRID_COMP
