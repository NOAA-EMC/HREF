#include "../../ESMFVersionDefine.h"

! February 2011    Weiyu Yang, Updated to use both the ESMF 4.0.0rp2 library,
!                              ESMF 5 library and the the ESMF 3.1.0rp2 library.
!-------------------------------------------------------------------------------

!-----------------------------------------------------------------------
!
      MODULE module_GFS_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      USE esmf_mod
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: GFS_INTERNAL_STATE                                    &
               ,WRAP_GFS_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      TYPE GFS_INTERNAL_STATE
          TYPE(ESMF_GridComp) :: GC_GFS_DYN                           &
                                ,GC_GFS_PHY

          TYPE(ESMF_State   ) :: IMP_GFS_DYN,EXP_GFS_DYN              &  !<-- Import/export states for GFS Dynamics
                                ,IMP_GFS_PHY,EXP_GFS_PHY              &  !<-- Import/export states for GFS Physics
                                ,IMP_GFS_WRT,EXP_GFS_WRT                 !<-- Import/export states for GFS Write

          TYPE(ESMF_Clock   ) :: CLOCK_GFS

#ifdef ESMF_3
          TYPE(ESMF_LOGICAL)  :: Cpl_flag
#else
          LOGICAL             :: Cpl_flag
#endif

!-----------------------------------------------------------------------
!***  FOR GSFC CHEMISTRY PACKAGE
!-----------------------------------------------------------------------
!
          TYPE(ESMF_GridComp) :: GC_GFS_CHEM                              !<-- The GFS chemistry component
          TYPE(ESMF_State)    :: IMP_GFS_CHEM,EXP_GFS_CHEM                !<-- Import/export states for GFS Chemistry
          TYPE(ESMF_CplComp)  :: GC_PHY2CHEM_CPL                          !<-- GFS Phy to Chem coupler gridded component
          TYPE(ESMF_CplComp)  :: GC_CHEM2PHY_CPL                          !<-- GFS Chem to Phy coupler gridded component
 
          TYPE(ESMF_Logical)  :: CHEMISTRY_ON                             !<-- Is chemistry active?

          TYPE(ESMF_CplComp)  :: GC_GFS_CPL
 
          INTEGER             :: MYPE                                  &  !<-- Each MPI task ID
                                ,WRITE_GROUP_READY_TO_GO                  !<-- The write group to use
 
          LOGICAL             :: QUILTING                                 !<-- Is asynchronous quilting specified?
 
          TYPE(ESMF_Logical)  :: PHYSICS_ON                               !<-- Is physics active?
 
          TYPE(ESMF_GridComp), DIMENSION(:), POINTER :: WRT_COMPS
 
          TYPE(ESMF_TimeInterval) :: TIMEINTERVAL_GFS_OUTPUT              !<-- The ESMF time interval between GFS history output
      END TYPE GFS_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      TYPE WRAP_GFS_INTERNAL_STATE
!
        TYPE(GFS_INTERNAL_STATE),POINTER :: GFS_INT_STATE
!
      END TYPE WRAP_GFS_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      END MODULE module_GFS_INTERNAL_STATE
!
!-----------------------------------------------------------------------

