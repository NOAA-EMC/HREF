



































!-----------------------------------------------------------------------
!
      MODULE module_OCN_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      USE ESMF_MOD
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: OCN_INTERNAL_STATE                                      &
               ,WRAP_OCN_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      TYPE OCN_INTERNAL_STATE
!
        CHARACTER(16) :: CORE
!
        TYPE(ESMF_Clock   ) :: CLOCK_OCN
        TYPE(ESMF_GridComp) :: CORE_GRID_COMP
        TYPE(ESMF_State   ) :: CORE_IMP_STATE
        TYPE(ESMF_State   ) :: CORE_EXP_STATE
!
      END TYPE OCN_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      TYPE WRAP_OCN_INTERNAL_STATE
!
        TYPE(OCN_INTERNAL_STATE),POINTER :: OCN_INT_STATE
!
      END TYPE WRAP_OCN_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      END MODULE module_OCN_INTERNAL_STATE
!
!-----------------------------------------------------------------------

