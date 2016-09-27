#include "./ESMFVersionDefine.h"

!-----------------------------------------------------------------------
!
      MODULE module_EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!***  Contents of the ESMF internal state of the EARTH component.
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
      PUBLIC :: EARTH_INTERNAL_STATE                                    &
               ,WRAP_EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      TYPE EARTH_INTERNAL_STATE
!
#ifdef WITH_NUOPC
        integer, pointer    :: atmPetList(:)
        integer, pointer    :: ocnPetList(:)
        integer, pointer    :: icePetList(:)
        integer, pointer    :: wavPetList(:)
        integer, pointer    :: lndPetList(:)
        integer, pointer    :: ipmPetList(:)
        integer, pointer    :: hydPetList(:)
        integer, pointer    :: medPetList(:)
        real(ESMF_KIND_R8)  :: medAtmCouplingIntervalSec
        real(ESMF_KIND_R8)  :: medOcnCouplingIntervalSec
        character(len=20)   :: atmModel
        character(len=20)   :: ocnModel
        character(len=20)   :: iceModel
        character(len=20)   :: wavModel
        character(len=20)   :: lndModel
        character(len=20)   :: ipmModel
        character(len=20)   :: hydModel
        character(len=20)   :: medModel
#else
        TYPE(ESMF_Clock   ) :: CLOCK_EARTH
!
        TYPE(ESMF_GridComp) :: ATM_GRID_COMP
        TYPE(ESMF_State   ) :: ATM_IMP_STATE
        TYPE(ESMF_State   ) :: ATM_EXP_STATE
#endif
!
      END TYPE EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      TYPE WRAP_EARTH_INTERNAL_STATE
!
        TYPE(EARTH_INTERNAL_STATE),POINTER :: EARTH_INT_STATE
!
      END TYPE WRAP_EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      END MODULE module_EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------
