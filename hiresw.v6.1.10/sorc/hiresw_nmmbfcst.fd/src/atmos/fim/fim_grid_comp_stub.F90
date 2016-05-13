! 05/11/2011   Weiyu Yang   Modified for using the ESMF 5.2.0r_beta_snapshot_07.
! 02/09/2012   Weiyu Yang   Modified for using the ESMF 5.2.0rp1 library.
!-------------------------------------------------------------------------------
#include "../../ESMFVersionDefine.h"

#if (ESMF_MAJOR_VERSION < 5 || ESMF_MINOR_VERSION < 2)
#undef ESMF_520r
#define ESMF_LogFoundError ESMF_LogMsgFoundError
#else
#define ESMF_520r
#endif

      MODULE module_FIM_GRID_COMP

      USE esmf_mod

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: FIM_REGISTER

      INTEGER :: DUMMY

      CONTAINS

!#######################################################################

      SUBROUTINE FIM_REGISTER(FIM_GRID_COMP,RC_REG)
      TYPE(ESMF_GridComp)               :: FIM_GRID_COMP
      INTEGER            ,INTENT(OUT)   :: RC_REG

      INTEGER :: RC

      write(0,*) "    FIM_REGISTER"


#ifdef ESMF_3
      CALL ESMF_GridCompSetEntryPoint(FIM_GRID_COMP ,ESMF_SETINIT ,FIM_INITIALIZE ,ESMF_SINGLEPHASE ,RC)
      CALL ESMF_GridCompSetEntryPoint(FIM_GRID_COMP ,ESMF_SETRUN  ,FIM_RUN        ,1                ,RC)
      CALL ESMF_GridCompSetEntryPoint(FIM_GRID_COMP ,ESMF_SETFINAL,FIM_FINALIZE   ,ESMF_SINGLEPHASE ,RC)
#else
#ifdef ESMF_520r
      CALL ESMF_GridCompSetEntryPoint(FIM_GRID_COMP ,ESMF_METHOD_INITIALIZE ,FIM_INITIALIZE ,phase=1 ,rc=RC)
      CALL ESMF_GridCompSetEntryPoint(FIM_GRID_COMP ,ESMF_METHOD_RUN,        FIM_RUN,        phase=1 ,rc=RC)
      CALL ESMF_GridCompSetEntryPoint(FIM_GRID_COMP ,ESMF_METHOD_FINALIZE,   FIM_FINALIZE   ,phase=1 ,rc=RC)
#else
      CALL ESMF_GridCompSetEntryPoint(FIM_GRID_COMP ,ESMF_SETINIT ,FIM_INITIALIZE ,phase=ESMF_SINGLEPHASE ,rc=RC)
      CALL ESMF_GridCompSetEntryPoint(FIM_GRID_COMP ,ESMF_SETRUN  ,FIM_RUN        ,phase=1                ,rc=RC)
      CALL ESMF_GridCompSetEntryPoint(FIM_GRID_COMP ,ESMF_SETFINAL,FIM_FINALIZE   ,phase=ESMF_SINGLEPHASE ,rc=RC)
#endif
#endif

      RC_REG = ESMF_SUCCESS
      write(0,*) "    END OF FIM_REGISTER"

      END SUBROUTINE FIM_REGISTER

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE FIM_INITIALIZE(FIM_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_FIM ,RC_INIT)

      TYPE(ESMF_GridComp)               :: FIM_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_FIM
      INTEGER            ,INTENT(OUT)   :: RC_INIT

      write(0,*) "        FIM_INITIALIZE stub"
      RC_INIT = ESMF_SUCCESS
      write(0,*) "        END OF FIM_INITIALIZE stub"

      END SUBROUTINE FIM_INITIALIZE

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE FIM_RUN(FIM_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_FIM ,RC_RUN)

      TYPE(ESMF_GridComp)               :: FIM_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_FIM
      INTEGER            ,INTENT(OUT)   :: RC_RUN

      write(0,*) "        FIM_RUN stub"
      RC_RUN=ESMF_SUCCESS
      write(0,*) "        END OF FIM_RUN stub"

      END SUBROUTINE FIM_RUN

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE FIM_FINALIZE(FIM_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_FIM ,RC_FINALIZE)

      TYPE(ESMF_GridComp)               :: FIM_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_FIM
      INTEGER            ,INTENT(OUT)   :: RC_FINALIZE

      write(0,*) "        FIM_FINALIZE stub"
      RC_FINALIZE=ESMF_SUCCESS
      write(0,*) "        END OF FIM_FINALIZE stub"

      END SUBROUTINE FIM_FINALIZE

!#######################################################################

      END MODULE module_FIM_GRID_COMP
