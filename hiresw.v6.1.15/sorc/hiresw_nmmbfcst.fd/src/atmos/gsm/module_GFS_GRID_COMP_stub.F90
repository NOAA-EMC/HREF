!  2011-05-11  Theurich & Yang  - Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!  2012-03-07  Weiyu Yang       - Modified for using the ESMF 5.2.0rp1 library.
!-------------------------------------------------------------------------------------

#include "../../ESMFVersionDefine.h"

#if (ESMF_MAJOR_VERSION < 5 || ESMF_MINOR_VERSION < 2)
#undef ESMF_520r
#else
#define ESMF_520r
#endif

      MODULE module_GFS_GRID_COMP

      USE esmf_mod

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: GFS_REGISTER

      INTEGER :: DUMMY

      CONTAINS

!#######################################################################

      SUBROUTINE GFS_REGISTER(GFS_GRID_COMP,RC_REG)
      TYPE(ESMF_GridComp)               :: GFS_GRID_COMP
      INTEGER            ,INTENT(OUT)   :: RC_REG

      INTEGER :: RC

      write(0,*) "    GFS_REGISTER stub"
      CALL ESMF_GridCompSetEntryPoint(GFS_GRID_COMP ,ESMF_METHOD_INITIALIZE,GFS_INITIALIZE &
#ifdef ESMF_3
                                     ,ESMF_SINGLEPHASE                                     &
                                     ,RC)
#else
#ifdef ESMF_520r
                                     ,phase=1                                              &
                                     ,rc=RC)
#else
                                     ,phase=ESMF_SINGLEPHASE                               &
                                     ,rc=RC)
#endif
#endif

      CALL ESMF_GridCompSetEntryPoint(GFS_GRID_COMP ,ESMF_METHOD_RUN,GFS_RUN               &
#ifdef ESMF_3
                                     ,ESMF_SINGLEPHASE                                     &
                                     ,RC)
#else
#ifdef ESMF_520r
                                     ,phase=1                                              &
                                     ,rc=RC)
#else
                                     ,phase=ESMF_SINGLEPHASE                               &
                                     ,rc=RC)
#endif
#endif
      CALL ESMF_GridCompSetEntryPoint(GFS_GRID_COMP ,ESMF_METHOD_FINALIZE,GFS_FINALIZE     &
#ifdef ESMF_3
                                     ,ESMF_SINGLEPHASE                                     &
                                     ,RC)
#else
#ifdef ESMF_520r
                                     ,phase=1                                              &
                                     ,rc=RC)
#else
                                     ,phase=ESMF_SINGLEPHASE                               &
                                     ,rc=RC)
#endif
#endif

      RC_REG = ESMF_SUCCESS
      write(0,*) "    END OF GFS_REGISTER stub"

      END SUBROUTINE GFS_REGISTER

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE GFS_INITIALIZE(GFS_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_GFS ,RC_INIT)

      TYPE(ESMF_GridComp)               :: GFS_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_GFS
      INTEGER            ,INTENT(OUT)   :: RC_INIT

      write(0,*) "        GFS_INITIALIZE stub"
      RC_INIT = ESMF_SUCCESS
      write(0,*) "        END OF GFS_INITIALIZE stub"

      END SUBROUTINE GFS_INITIALIZE

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE GFS_RUN(GFS_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_GFS ,RC_RUN)

      TYPE(ESMF_GridComp)               :: GFS_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_GFS
      INTEGER            ,INTENT(OUT)   :: RC_RUN

      write(0,*) "        GFS_RUN stub"
      RC_RUN=ESMF_SUCCESS
      write(0,*) "        END OF GFS_RUN stub"

      END SUBROUTINE GFS_RUN

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      SUBROUTINE GFS_FINALIZE(GFS_GRID_COMP ,IMP_STATE ,EXP_STATE ,CLOCK_GFS ,RC_FINALIZE)

      TYPE(ESMF_GridComp)               :: GFS_GRID_COMP
      TYPE(ESMF_State)                  :: IMP_STATE
      TYPE(ESMF_State)                  :: EXP_STATE
      TYPE(ESMF_Clock)                  :: CLOCK_GFS
      INTEGER            ,INTENT(OUT)   :: RC_FINALIZE

      write(0,*) "        GFS_FINALIZE stub"
      RC_FINALIZE=ESMF_SUCCESS
      write(0,*) "        END OF GFS_FINALIZE stub"

      END SUBROUTINE GFS_FINALIZE

!#######################################################################

      END MODULE module_GFS_GRID_COMP
