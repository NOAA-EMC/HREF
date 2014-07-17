!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  O3_data

  MODULE O3_data

! !USES:

   IMPLICIT NONE

! !PUBLIC TYPES:
!
!
! !PUBLIIC MEMBER FUNCTIONS:
!
!
! !DESCRIPTION:
!
!  This module establishes constants and arrays used by the
!  parameterized chemistry ozone grid component module.
!
! !REVISION HISTORY:
!
!  15Feb2005 Nielsen   First build
!
!EOP
!-------------------------------------------------------------------------
      CHARACTER(LEN=8) :: speciesname(4)
      REAL, ALLOCATABLE :: prod(:,:,:,:),loss(:,:,:,:)
  END MODULE O3_data
