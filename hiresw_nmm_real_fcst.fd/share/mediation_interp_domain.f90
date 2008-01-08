!
!WRF:MEDIATION_LAYER:NESTING
!
SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   USE module_timing
   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
   TYPE (grid_config_rec_type)            :: config_flags
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y

! ----------------------------------------------------------
! ----------------------------------------------------------
! Interface blocks
! ----------------------------------------------------------
   INTERFACE
! ----------------------------------------------------------
!    Interface definitions for EM CORE
! ----------------------------------------------------------
! ----------------------------------------------------------
!    Interface definitions for NMM (placeholder)
! ----------------------------------------------------------
! ----------------------------------------------------------
!    Interface definitions for COAMPS (placeholder)
! ----------------------------------------------------------
   END INTERFACE
! ----------------------------------------------------------
! End of Interface blocks
! ----------------------------------------------------------
! ----------------------------------------------------------
! ----------------------------------------------------------
! Executable code
! ----------------------------------------------------------
! ----------------------------------------------------------
!    Interpolation calls for EM CORE.  The called 
!    routines below are supplied by module_dm.F
!    from the external communications package (e.g. RSL)
! ----------------------------------------------------------
! ------------------------------------------------------
!    End of Interpolation calls for EM CORE.
! ------------------------------------------------------
! ------------------------------------------------------
! ------------------------------------------------------
!    Interpolation calls for NMM. (Placeholder)
! ------------------------------------------------------
! ------------------------------------------------------
!    End of Interpolation calls for NMM.
! ------------------------------------------------------
! ------------------------------------------------------
! ------------------------------------------------------
!    Interpolation calls for COAMPS. (Placeholder)
! ------------------------------------------------------
! ------------------------------------------------------
!    End of Interpolation calls for COAMPS.
! ------------------------------------------------------
   RETURN
END SUBROUTINE med_interp_domain


