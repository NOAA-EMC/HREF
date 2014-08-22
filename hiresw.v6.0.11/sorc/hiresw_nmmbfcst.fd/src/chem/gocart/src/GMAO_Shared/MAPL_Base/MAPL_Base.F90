! $Id: MAPL_Base.F90,v 1.21 2009/02/20 14:53:02 trayanov Exp $

#include "MAPL_ErrLog.h"

module MAPL_BaseMod

!BOP
!
! !MODULE: MAPL_BaseMod --- A Collection of Assorted MAPL Utilities

! !USES:
!
use ESMF_Mod
use MAPL_ConstantsMod, only: MAPL_PI

implicit NONE
private

public MAPL_ArrayF90Deallocate
public MAPL_FieldF90Deallocate

! !PUBLIC MEMBER FUNCTIONS:
!
public MAPL_AllocateCoupling    ! Atanas: please provide 1-line for each
public MAPL_Asrt
public MAPL_ClimInterpFac
public MAPL_ConnectCoupling
public MAPL_DecomposeDim
public MAPL_FieldCreate
public MAPL_FieldGetTime
public MAPL_FieldSetTime
public MAPL_GridGet
public MAPL_IncYMD
public MAPL_Interp_Fac
public MAPL_LatLonGridCreate   ! Creates regular Lat/Lon ESMF Grids
public MAPL_Nhmsf
public MAPL_Nsecf2
public MAPL_PackTime
public MAPL_RemapBounds
public MAPL_Rtrn
public MAPL_Tick
public MAPL_TimeStringGet
public MAPL_UnpackTime
public MAPL_Vrfy
public MAPL_RmQualifier

! !PUBLIC PARAMETERS
!
integer, public, parameter :: MAPL_CplUNKNOWN        = 0
integer, public, parameter :: MAPL_CplSATISFIED      = 1
integer, public, parameter :: MAPL_CplNEEDED         = 2
integer, public, parameter :: MAPL_CplNOTNEEDED      = 4
integer, public, parameter :: MAPL_FriendlyVariable  = 8
integer, public, parameter :: MAPL_FieldItem         = 8
integer, public, parameter :: MAPL_BundleItem        = 16
integer, public, parameter :: MAPL_NoRestart         = 32

integer, public, parameter :: MAPL_Write2Disk        = 0
integer, public, parameter :: MAPL_Write2RAM         = 1

integer, public, parameter :: MAPL_VLocationNone   = 0
integer, public, parameter :: MAPL_VLocationEdge   = 1
integer, public, parameter :: MAPL_VLocationCenter = 2

integer, public, parameter :: MAPL_DimsUnknown     = 0
integer, public, parameter :: MAPL_DimsVertOnly    = 1
integer, public, parameter :: MAPL_DimsHorzOnly    = 2
integer, public, parameter :: MAPL_DimsHorzVert    = 3
integer, public, parameter :: MAPL_DimsTileOnly    = 4
integer, public, parameter :: MAPL_DimsTileTile    = 5

integer, public, parameter :: MAPL_DuplicateEntry  = -99
integer, public, parameter :: MAPL_Self = 0 
integer, public, parameter :: MAPL_Import = 1
integer, public, parameter :: MAPL_Export = 2
integer, public, parameter :: MAPL_ConnUnknown = -1
integer, public, parameter :: MAPL_RecordPhase  = 99
integer, public, parameter :: MAPL_ColdstartPhase  = 99
integer, public, parameter :: MAPL_FirstPhase   = 81
integer, public, parameter :: MAPL_SecondPhase  = MAPL_FirstPhase+1
integer, public, parameter :: MAPL_ThirdPhase   = MAPL_FirstPhase+2
integer, public, parameter :: MAPL_FourthPhase  = MAPL_FirstPhase+3
integer, public, parameter :: MAPL_FifthPhase   = MAPL_FirstPhase+4

real,    public, parameter :: MAPL_UNDEF              = 1.0e15  

integer, public, parameter :: MAPL_Ocean              = 0
integer, public, parameter :: MAPL_Lake               = 19
integer, public, parameter :: MAPL_LandIce            = 20

integer, public, parameter :: MAPL_BroadleafEvergreen = 1
integer, public, parameter :: MAPL_BroadleafDeciduous = 2
integer, public, parameter :: MAPL_Needleleaf         = 3
integer, public, parameter :: MAPL_GroundCover        = 4
integer, public, parameter :: MAPL_BroadleafShrubs    = 5
integer, public, parameter :: MAPL_Tundra             = 6
integer, public, parameter :: MAPL_BareSoil           = 7
integer, public, parameter :: MAPL_Desert             = 8
integer, public, parameter :: MAPL_NumVegTypes        = 8

integer, public, parameter :: MAPL_Land               = 100
integer, public, parameter :: MAPL_Vegetated          = 101

#ifdef __PROTEX__

 !DESCRIPTION:

   The module {\tt MAPL\_Base} provides a collection assorted
utilities and constants used throughout the MAPL Library.


#endif

!EOP
!----------------------------------------------------------------------

type WRAP1R4
   real (ESMF_KIND_R4), dimension(:)    , pointer :: ptr
end type WRAP1R4

type WRAP2R4
   real (ESMF_KIND_R4), dimension(:,:)    , pointer :: ptr
end type WRAP2R4

type WRAP3R4
   real (ESMF_KIND_R4), dimension(:,:,:)    , pointer :: ptr
end type WRAP3R4

type WRAP4R4
   real (ESMF_KIND_R4), dimension(:,:,:,:)    , pointer :: ptr
end type WRAP4R4

type WRAP1R8
   real (ESMF_KIND_R8), dimension(:)    , pointer :: ptr
end type WRAP1R8

type WRAP2R8
   real (ESMF_KIND_R8), dimension(:,:)    , pointer :: ptr
end type WRAP2R8

type WRAP3R8
   real (ESMF_KIND_R8), dimension(:,:,:)    , pointer :: ptr
end type WRAP3R8

type WRAP4R8
   real (ESMF_KIND_R8), dimension(:,:,:,:)    , pointer :: ptr
end type WRAP4R8


interface MAPL_FieldCreate
   module procedure MAPL_FieldCreateRename
   module procedure MAPL_FieldCreateNewgrid
end interface

interface MAPL_FieldGetTime
   module procedure MAPL_GetFieldTimeFromField
   module procedure MAPL_GetFieldTimeFromState
end interface

interface MAPL_FieldSetTime
   module procedure MAPL_SetFieldTimeFromField
   module procedure MAPL_SetFieldTimeFromState
end interface

interface MAPL_AllocateCoupling
   module procedure MAPL_AllocateCouplingFromArray
   module procedure MAPL_AllocateCouplingFromField
end interface

interface MAPL_ConnectCoupling
   module procedure MAPL_ConnectCouplingFromArray
   module procedure MAPL_ConnectCouplingFromField
   module procedure MAPL_ConnectCouplingFromF901DR4
   module procedure MAPL_ConnectCouplingFromF902DR4
   module procedure MAPL_ConnectCouplingFromF903DR4
   module procedure MAPL_ConnectArrayFromF901DR4
   module procedure MAPL_ConnectArrayFromF902DR4
   module procedure MAPL_ConnectArrayFromF903DR4
end interface

interface MAPL_RemapBounds
   module procedure MAPL_RemapBounds_3dr4
end interface

interface MAPL_VRFY
   module procedure MAPL_VRFY
   module procedure MAPL_VRFYt
end interface

interface MAPL_ASRT
   module procedure MAPL_ASRT
   module procedure MAPL_ASRTt
end interface

interface MAPL_RTRN
   module procedure MAPL_RTRN
   module procedure MAPL_RTRNt
end interface

contains

  subroutine MAPL_AllocateCouplingFromField(field, rc)

    type(ESMF_Field),  intent(INOUT) :: field
    integer, optional, intent(  OUT) :: rc             
    
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_AllocateCouplingFromField'
    type(ESMF_Array)                        :: array
    
    call ESMF_FieldGet (FIELD, Array=ARRAY, RC=STATUS)
    VERIFY_(STATUS)
    
    call MAPL_AllocateCouplingFromArray(array, rc=STATUS)
    VERIFY_(STATUS)
    
    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_AllocateCouplingFromField

  subroutine MAPL_AllocateCouplingFromArray(array, rc)
    type(ESMF_Array),  intent(INOUT) :: array
    integer, optional, intent(  OUT) :: rc             
    
    integer                                 :: rank
    type(ESMF_TypeKind)                     :: tk
    integer, dimension(ESMF_MAXDIM)         :: counts, lbounds, ubounds
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_AllocateCouplingFromArray'

    real (ESMF_KIND_R4), dimension(:)        , pointer :: r4d1
    real (ESMF_KIND_R4), dimension(:,:)      , pointer :: r4d2
    real (ESMF_KIND_R4), dimension(:,:,:)    , pointer :: r4d3
    real (ESMF_KIND_R4), dimension(:,:,:,:)  , pointer :: r4d4

    real (ESMF_KIND_R8), dimension(:)        , pointer :: r8d1
    real (ESMF_KIND_R8), dimension(:,:)      , pointer :: r8d2
    real (ESMF_KIND_R8), dimension(:,:,:)    , pointer :: r8d3
    real (ESMF_KIND_R8), dimension(:,:,:,:)  , pointer :: r8d4

    type (WRAP1R4) :: wrap1dr4
    type (WRAP2R4) :: wrap2dr4
    type (WRAP3R4) :: wrap3dr4
    type (WRAP4R4) :: wrap4dr4

    type (WRAP1R8) :: wrap1dr8
    type (WRAP2R8) :: wrap2dr8
    type (WRAP3R8) :: wrap3dr8
    type (WRAP4R8) :: wrap4dr8

    type (ESMF_LocalArray), target  :: larrayList(1)
    type (ESMF_LocalArray), pointer :: larray
    integer        :: localDeCount

    call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=status)
    VERIFY_(STATUS)
    ASSERT_(localDeCount == 1) !ALT: currently MAPL supports only 1 local array
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=status)
    VERIFY_(STATUS)
    larray => lArrayList(1) ! alias
    call ESMF_LocalArrayGet(larray, rank=rank, typekind=tk, &
                            counts = counts, lbounds=lbounds, ubounds=ubounds, &
                            rc=status)
    VERIFY_(STATUS)
!ALT in case the counts=0 esmf keeps ubounds=lbounds
    where (counts==0) ubounds = lbounds + counts - 1
    if (tk .eq. ESMF_TYPEKIND_R4) then
       if (rank .eq. 1) then
          call ESMF_LocalArrayGet(larray, r4d1, rc=status)
          VERIFY_(STATUS)
          if (.not. associated(r4d1)) then
             allocate(r4d1(lbounds(1):ubounds(1)),  stat=status)
             VERIFY_(STATUS)

             r4d1 = 0.0 ! initialize
             call c_ESMC_LocalArraySetBaseAddr(larray, r4d1, status) 
             VERIFY_(STATUS)

             wrap1dr4%ptr => r4d1
             call c_ESMC_LocalArraySetF90Ptr(larray, wrap1dr4, status) 
             VERIFY_(STATUS)
          endif

       else if (rank .eq. 2) then
          call ESMF_LocalArrayGet(larray, r4d2, rc=status)
          VERIFY_(STATUS)
          if (.not. associated(r4d2)) then
             allocate(r4d2(lbounds(1):ubounds(1), &
                           lbounds(2):ubounds(2)),  stat=status)
             VERIFY_(STATUS)

             r4d2 = 0.0
             call c_ESMC_LocalArraySetBaseAddr(larray, r4d2, status) 
             VERIFY_(STATUS)

             wrap2dr4%ptr => r4d2
             call c_ESMC_LocalArraySetF90Ptr(larray, wrap2dr4, status) 
             VERIFY_(STATUS)
          endif

       else if (rank .eq. 3) then
          call ESMF_LocalArrayGet(larray, r4d3, rc=status)
          VERIFY_(STATUS)
          if (.not. associated(r4d3)) then
             allocate(r4d3(lbounds(1):ubounds(1), &
                           lbounds(2):ubounds(2), &
                           lbounds(3):ubounds(3)), stat=status)
             VERIFY_(STATUS)

             r4d3 = 0.0
             call c_ESMC_LocalArraySetBaseAddr(larray, r4d3, status) 
             VERIFY_(STATUS)

             wrap3dr4%ptr => r4d3
             call c_ESMC_LocalArraySetF90Ptr(larray, wrap3dr4, status) 
             VERIFY_(STATUS)
          endif

       else if (rank .eq. 4) then
          call ESMF_LocalArrayGet(larray, r4d4, rc=status)
          VERIFY_(STATUS)
          if (.not. associated(r4d4)) then
             allocate(r4d4(lbounds(1):ubounds(1), &
                           lbounds(2):ubounds(2), &
                           lbounds(3):ubounds(3), &
                           lbounds(4):ubounds(4)), stat=status)
             VERIFY_(STATUS)

             r4d4 = 0.0
             call c_ESMC_LocalArraySetBaseAddr(larray, r4d4, status) 
             VERIFY_(STATUS)

             wrap4dr4%ptr => r4d4
             call c_ESMC_LocalArraySetF90Ptr(larray, wrap4dr4, status) 
             VERIFY_(STATUS)
          end if

       else
          RETURN_(ESMF_FAILURE)
       endif
    else if (tk .eq. ESMF_TYPEKIND_R8) then
       if (rank .eq. 1) then
          call ESMF_LocalArrayGet(larray, r8d1, rc=status)
          VERIFY_(STATUS)
          if (.not. associated(r8d1)) then
             allocate(r8d1(lbounds(1):ubounds(1)),  stat=status)
             VERIFY_(STATUS)

             r8d1 = 0.0
             call c_ESMC_LocalArraySetBaseAddr(larray, r8d1, status) 
             VERIFY_(STATUS)

             wrap1dr8%ptr => r8d1
             call c_ESMC_LocalArraySetF90Ptr(larray, wrap1dr8, status) 
             VERIFY_(STATUS)
          endif

       else if (rank .eq. 2) then
          call ESMF_LocalArrayGet(larray, r8d2, rc=status)
          VERIFY_(STATUS)
          if (.not. associated(r8d2)) then
             allocate(r8d2(lbounds(1):ubounds(1), &
                           lbounds(2):ubounds(2)),  stat=status)
             VERIFY_(STATUS)

             r8d2 = 0.0
             call c_ESMC_LocalArraySetBaseAddr(larray, r8d2, status) 
             VERIFY_(STATUS)

             wrap2dr8%ptr => r8d2
             call c_ESMC_LocalArraySetF90Ptr(larray, wrap2dr8, status) 
             VERIFY_(STATUS)
          endif

       else if (rank .eq. 3) then
          call ESMF_LocalArrayGet(larray, r8d3, rc=status)
          VERIFY_(STATUS)
          if (.not. associated(r8d3)) then
             allocate(r8d3(lbounds(1):ubounds(1), &
                           lbounds(2):ubounds(2), &
                           lbounds(3):ubounds(3)), stat=status)
             VERIFY_(STATUS)

             r8d3 = 0.0
             call c_ESMC_LocalArraySetBaseAddr(larray, r8d3, status) 
             VERIFY_(STATUS)

             wrap3dr8%ptr => r8d3
             call c_ESMC_LocalArraySetF90Ptr(larray, wrap3dr8, status) 
             VERIFY_(STATUS)
          endif

       else if (rank .eq. 4) then
          call ESMF_LocalArrayGet(larray, r8d4, rc=status)
          VERIFY_(STATUS)
          if (.not. associated(r8d4)) then
             allocate(r8d4(lbounds(1):ubounds(1), &
                           lbounds(2):ubounds(2), &
                           lbounds(3):ubounds(3), &
                           lbounds(4):ubounds(4)), stat=status)
             VERIFY_(STATUS)

             r8d4 = 0.0
             call c_ESMC_LocalArraySetBaseAddr(larray, r8d4, status) 
             VERIFY_(STATUS)

             wrap4dr8%ptr => r8d4
             call c_ESMC_LocalArraySetF90Ptr(larray, wrap4dr8, status) 
             VERIFY_(STATUS)
          end if

       else
          RETURN_(ESMF_FAILURE)
       endif
    else
       RETURN_(ESMF_FAILURE)
    endif

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_AllocateCouplingFromArray





  subroutine MAPL_ConnectCouplingFromField(field, from_field, rc)
    type(ESMF_Field),  intent(INOUT) :: field
    type(ESMF_Field),  intent(INOUT) :: from_field 
!ALT: this should have been intent(IN), but ESMF wants intent(INOUT) on the ESMF_FieldGet call
    integer, optional, intent(  OUT) :: rc             
    
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ConnectCouplingFromField'
    type(ESMF_Array)                        :: array
    type(ESMF_Array)                        :: from_array
    
    call ESMF_FieldGet (FIELD, array=ARRAY, RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_FieldGet (FROM_FIELD, array=FROM_ARRAY, RC=STATUS)
    VERIFY_(STATUS)
    
    call MAPL_ConnectCouplingFromArray(array, from_array, rc=STATUS)
    VERIFY_(STATUS)
    
    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_ConnectCouplingFromField


  subroutine MAPL_ConnectCouplingFromArray(array, from_array, rc)
    type(ESMF_Array),  intent(INOUT) :: array
    type(ESMF_Array),  intent(IN   ) :: from_array
    integer, optional, intent(  OUT) :: rc             
    
    integer                                 :: rank
    type(ESMF_TypeKind)                     :: tk
    integer, dimension(ESMF_MAXDIM)         :: counts, lbounds, ubounds
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ConnectCouplingFromArray'

    real (ESMF_KIND_R4), dimension(:)        , pointer :: p4d1
    real (ESMF_KIND_R4), dimension(:,:)      , pointer :: p4d2
    real (ESMF_KIND_R4), dimension(:,:,:)    , pointer :: p4d3
    real (ESMF_KIND_R4), dimension(:,:,:,:)  , pointer :: p4d4

    real (ESMF_KIND_R4), dimension(:)        , pointer :: r4d1
    real (ESMF_KIND_R4), dimension(:,:)      , pointer :: r4d2
    real (ESMF_KIND_R4), dimension(:,:,:)    , pointer :: r4d3
    real (ESMF_KIND_R4), dimension(:,:,:,:)  , pointer :: r4d4

    real (ESMF_KIND_R8), dimension(:)        , pointer :: r8d1
    real (ESMF_KIND_R8), dimension(:,:)      , pointer :: r8d2
    real (ESMF_KIND_R8), dimension(:,:,:)    , pointer :: r8d3
    real (ESMF_KIND_R8), dimension(:,:,:,:)  , pointer :: r8d4

    type (WRAP1R4) :: wrap1dr4
    type (WRAP2R4) :: wrap2dr4
    type (WRAP3R4) :: wrap3dr4
    type (WRAP4R4) :: wrap4dr4

    type (WRAP1R8) :: wrap1dr8
    type (WRAP2R8) :: wrap2dr8
    type (WRAP3R8) :: wrap3dr8
    type (WRAP4R8) :: wrap4dr8

    type (ESMF_LocalArray), target  :: larrayList(1)
    type (ESMF_LocalArray), pointer :: larray
    type (ESMF_LocalArray), target  :: from_larrayList(1)
    type (ESMF_LocalArray), pointer :: from_larray
    integer        :: localDeCount

    call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=status)
    VERIFY_(STATUS)
    ASSERT_(localDeCount == 1) !ALT: currently MAPL supports only 1 local array
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=status)
    VERIFY_(STATUS)
    larray => lArrayList(1) ! alias

    call ESMF_ArrayGet(from_array, localDeCount=localDeCount, rc=status)
    VERIFY_(STATUS)
    ASSERT_(localDeCount == 1) !ALT: currently MAPL supports only 1 local array
    call ESMF_ArrayGet(from_array, larrayList=from_larrayList, rc=status)
    VERIFY_(STATUS)
    from_larray => from_lArrayList(1) ! alias

    call ESMF_LocalArrayGet(larray, rank, typekind=tk, &
                            counts = counts, lbounds=lbounds, ubounds=ubounds, &
                            rc=status)
    VERIFY_(STATUS)
    where (counts==0) ubounds = lbounds + counts - 1
    if (tk .eq. ESMF_TYPEKIND_R4) then
       if (rank .eq. 1) then
          call ESMF_LocalArrayGet(from_larray, p4d1, rc=status)
          VERIFY_(STATUS)
          call ESMF_LocalArrayGet(larray, r4d1, rc=status)
          VERIFY_(STATUS)
          if (associated(r4d1)) then
             deallocate(r4d1,  stat=status)
             VERIFY_(STATUS)
          endif

          r4d1 => p4d1
          call c_ESMC_LocalArraySetBaseAddr(larray, r4d1, status) 
          VERIFY_(STATUS)

          wrap1dr4%ptr => r4d1
          call c_ESMC_LocalArraySetF90Ptr(larray, wrap1dr4, status) 
          VERIFY_(STATUS)

       else if (rank .eq. 2) then
          call ESMF_LocalArrayGet(from_larray, p4d2, rc=status)
          VERIFY_(STATUS)
          call ESMF_LocalArrayGet(larray, r4d2, rc=status)
          VERIFY_(STATUS)
          if (associated(r4d2)) then
             deallocate(r4d2,  stat=status)
             VERIFY_(STATUS)
          endif

          r4d2 => p4d2
          call c_ESMC_LocalArraySetBaseAddr(larray, r4d2, status) 
          VERIFY_(STATUS)

          wrap2dr4%ptr => r4d2
          call c_ESMC_LocalArraySetF90Ptr(larray, wrap2dr4, status) 
          VERIFY_(STATUS)

       else if (rank .eq. 3) then
          call ESMF_LocalArrayGet(from_larray, p4d3, rc=status)
          VERIFY_(STATUS)
          call ESMF_LocalArrayGet(larray, r4d3, rc=status)
          VERIFY_(STATUS)
          if (associated(r4d3)) then
             deallocate(r4d3, stat=status)
             VERIFY_(STATUS)
          end if
          r4d3 => p4d3
          call c_ESMC_LocalArraySetBaseAddr(larray, r4d3, status) 
          VERIFY_(STATUS)

          wrap3dr4%ptr => r4d3
          call c_ESMC_LocalArraySetF90Ptr(larray, wrap3dr4, status) 
          VERIFY_(STATUS)

       else if (rank .eq. 4) then
          call ESMF_LocalArrayGet(from_larray, p4d4, rc=status)
          VERIFY_(STATUS)
          call ESMF_LocalArrayGet(larray, r4d4, rc=status)
          VERIFY_(STATUS)
          if (associated(r4d4)) then
             deallocate(r4d4, stat=status)
             VERIFY_(STATUS)
          end if
          r4d4 => p4d4
          call c_ESMC_LocalArraySetBaseAddr(larray, r4d4, status) 
          VERIFY_(STATUS)

          wrap4dr4%ptr => r4d4
          call c_ESMC_LocalArraySetF90Ptr(larray, wrap4dr4, status) 
          VERIFY_(STATUS)

       else
          RETURN_(ESMF_FAILURE)
       endif
    else if (tk .eq. ESMF_TYPEKIND_R8) then
!ALT: temporaty set to FAIL; if compiles OK, copy and paste from above; replace r4=>r8
       RETURN_(ESMF_FAILURE)
    else
       RETURN_(ESMF_FAILURE)
    endif

    RETURN_(ESMF_SUCCESS)


  end subroutine MAPL_ConnectCouplingFromArray

  subroutine MAPL_ConnectCouplingFromF901DR4(field, ptr, rc)
    type(ESMF_Field),  intent(INOUT) :: field
    real(kind=ESMF_KIND_R4),  pointer     :: ptr(:)
    integer, optional, intent(  OUT) :: rc             
    
    type (ESMF_Array)                       :: array
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ConnectCouplingFromF901DR4'

    call ESMF_FieldGet(field, Array=array, rc=status)
    VERIFY_(STATUS)

    call MAPL_ConnectCoupling (array, ptr, rc=status)
    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ConnectCouplingFromF901DR4

  subroutine MAPL_ConnectCouplingFromF902DR4(field, ptr, rc)
    type(ESMF_Field),  intent(INOUT) :: field
    real(kind=ESMF_KIND_R4),  pointer     :: ptr(:,:)
    integer, optional, intent(  OUT) :: rc             
    
    type (ESMF_Array)                       :: array
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ConnectCouplingFromF902DR4'

    call ESMF_FieldGet(field, Array=array, rc=status)
    VERIFY_(STATUS)

    call MAPL_ConnectCoupling (array, ptr, rc=status)
    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ConnectCouplingFromF902DR4

  subroutine MAPL_ConnectCouplingFromF903DR4(field, ptr, rc)
    type(ESMF_Field),  intent(INOUT) :: field
    real(kind=ESMF_KIND_R4),  pointer     :: ptr(:,:,:)
    integer, optional, intent(  OUT) :: rc             
    
    type (ESMF_Array)                    :: array
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ConnectCouplingFromF903DR4'

    call ESMF_FieldGet(field, Array=array, rc=status)
    VERIFY_(STATUS)

    call MAPL_ConnectCoupling (array, ptr, rc=status)
    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ConnectCouplingFromF903DR4

  subroutine MAPL_ConnectArrayFromF901DR4(array, ptr, rc)
    type(ESMF_Array),  intent(INOUT) :: array
    real(kind=ESMF_KIND_R4),  pointer     :: ptr(:)
    integer, optional, intent(  OUT) :: rc             
    
    integer                                 :: rank
    type(ESMF_TypeKind)                     :: tk
    integer, dimension(ESMF_MAXDIM)         :: counts, lbounds, ubounds
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ArrayCouplingFromF901DR4'

    real (ESMF_KIND_R4), dimension(:)      , pointer :: r4d1

    type (WRAP1R4) :: wrap1dr4

    type (ESMF_LocalArray), target  :: larrayList(1)
    type (ESMF_LocalArray), pointer :: larray
    type (ESMF_LocalArray), target  :: from_larrayList(1)
    type (ESMF_LocalArray), pointer :: from_larray
    integer        :: localDeCount

    call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=status)
    VERIFY_(STATUS)
    ASSERT_(localDeCount == 1) !ALT: currently MAPL supports only 1 local array
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=status)
    VERIFY_(STATUS)
    larray => lArrayList(1) ! alias

    call ESMF_LocalArrayGet(larray, rank, typekind=tk, &
                            counts = counts, lbounds=lbounds, ubounds=ubounds, &
                            rc=status)
    VERIFY_(STATUS)
    where (counts==0) ubounds = lbounds + counts - 1
    ASSERT_(rank == 1)
    ASSERT_(tk == ESMF_TYPEKIND_R4)

    call ESMF_LocalArrayGet(larray, r4d1, rc=status)
    VERIFY_(STATUS)

    if (associated(ptr)) then
       r4d1 => ptr
       wrap1dr4%ptr => r4d1
    else
       NULLIFY(r4d1)
       NULLIFY(wrap1dr4%ptr)
    end if

    call c_ESMC_LocalArraySetBaseAddr(larray, r4d1, status) 
    VERIFY_(STATUS)

    call c_ESMC_LocalArraySetF90Ptr(larray, wrap1dr4, status) 
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ConnectArrayFromF901DR4

  subroutine MAPL_ConnectArrayFromF902DR4(array, ptr, rc)
    type(ESMF_Array),  intent(INOUT) :: array
    real(kind=ESMF_KIND_R4),  pointer     :: ptr(:,:)
    integer, optional, intent(  OUT) :: rc             
    
    integer                                 :: rank
    type(ESMF_TypeKind)                     :: tk
    integer, dimension(ESMF_MAXDIM)         :: counts, lbounds, ubounds
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ConnectArrayFromF902DR4'

    real (ESMF_KIND_R4), dimension(:,:)      , pointer :: r4d2

    type (WRAP2R4) :: wrap2dr4

    type (ESMF_LocalArray), target  :: larrayList(1)
    type (ESMF_LocalArray), pointer :: larray
    type (ESMF_LocalArray), target  :: from_larrayList(1)
    type (ESMF_LocalArray), pointer :: from_larray
    integer        :: localDeCount

    call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=status)
    VERIFY_(STATUS)
    ASSERT_(localDeCount == 1) !ALT: currently MAPL supports only 1 local array
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=status)
    VERIFY_(STATUS)
    larray => lArrayList(1) ! alias

    call ESMF_LocalArrayGet(larray, rank, typekind=tk, &
                            counts = counts, lbounds=lbounds, ubounds=ubounds, &
                            rc=status)
    VERIFY_(STATUS)
    where (counts==0) ubounds = lbounds + counts - 1
    ASSERT_(rank == 2)
    ASSERT_(tk == ESMF_TYPEKIND_R4)

    call ESMF_LocalArrayGet(larray, r4d2, rc=status)
    VERIFY_(STATUS)

    if (associated(ptr)) then
       r4d2 => ptr
       wrap2dr4%ptr => r4d2
    else
       NULLIFY(r4d2)
       NULLIFY(wrap2dr4%ptr)
    end if

    call c_ESMC_LocalArraySetBaseAddr(larray, r4d2, status) 
    VERIFY_(STATUS)

    call c_ESMC_LocalArraySetF90Ptr(larray, wrap2dr4, status) 
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ConnectArrayFromF902DR4

  subroutine MAPL_ConnectArrayFromF903DR4(array, ptr, rc)
    type(ESMF_Array),  intent(INOUT) :: array
    real(kind=ESMF_KIND_R4),  pointer     :: ptr(:,:,:)
    integer, optional, intent(  OUT) :: rc             
    
    integer                                 :: rank
    type(ESMF_TypeKind)                     :: tk
    integer, dimension(ESMF_MAXDIM)         :: counts, lbounds, ubounds
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ConnectArrayFromF903DR4'

    real (ESMF_KIND_R4), dimension(:,:,:)    , pointer :: r4d3

    type (WRAP3R4) :: wrap3dr4

    type (ESMF_LocalArray), target  :: larrayList(1)
    type (ESMF_LocalArray), pointer :: larray
    type (ESMF_LocalArray), target  :: from_larrayList(1)
    type (ESMF_LocalArray), pointer :: from_larray
    integer        :: localDeCount

    call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=status)
    VERIFY_(STATUS)
    ASSERT_(localDeCount == 1) !ALT: currently MAPL supports only 1 local array
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=status)
    VERIFY_(STATUS)
    larray => lArrayList(1) ! alias

    call ESMF_LocalArrayGet(larray, rank, typekind=tk, &
                            counts = counts, lbounds=lbounds, ubounds=ubounds, &
                            rc=status)
    VERIFY_(STATUS)
    where (counts==0) ubounds = lbounds + counts - 1
    ASSERT_(rank == 3)
    ASSERT_(tk == ESMF_TYPEKIND_R4)

    call ESMF_LocalArrayGet(larray, r4d3, rc=status)
    VERIFY_(STATUS)

    if (associated(ptr)) then
       r4d3 => ptr
       wrap3dr4%ptr => r4d3
    else
       NULLIFY(r4d3)
       NULLIFY(wrap3dr4%ptr)
    end if

    call c_ESMC_LocalArraySetBaseAddr(larray, r4d3, status) 
    VERIFY_(STATUS)

    call c_ESMC_LocalArraySetF90Ptr(larray, wrap3dr4, status) 
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_ConnectArrayFromF903DR4

  subroutine MAPL_ArrayF90Deallocate(array, rc)
    type(ESMF_Array),  intent(INOUT) :: array
    integer, optional, intent(  OUT) :: rc             
    
    integer                                 :: rank
    type(ESMF_TypeKind)                     :: tk
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ArrayF90Deallocate'

    type (ESMF_LocalArray), target  :: larrayList(1)
    type (ESMF_LocalArray), pointer :: larray
    integer                         :: localDeCount
    real*4, pointer                 :: ptr3dr4(:,:,:), ptr2dr4(:,:), ptr1dr4(:) 
    real*8, pointer                 :: ptr3dr8(:,:,:), ptr2dr8(:,:), ptr1dr8(:) 
    type (WRAP3R4)                  :: wrap3dr4, wrap2dr4, wrap1dr4
    type (WRAP3R8)                  :: wrap3dr8, wrap2dr8, wrap1dr8

    call ESMF_ArrayGet(array, localDeCount=localDeCount, rc=status)
    VERIFY_(STATUS)
    ASSERT_(localDeCount == 1) !ALT: currently MAPL supports only 1 local array
    call ESMF_ArrayGet(array, larrayList=larrayList, rc=status)
    VERIFY_(STATUS)
    larray => lArrayList(1) ! alias
    call ESMF_LocalArrayGet(larray, rank, typekind=tk, &
                            rc=status)
    VERIFY_(STATUS)
#if 0
    call ESMF_LocalArrayF90Deallocate(larray, rank, tk, rc=status)
    VERIFY_(STATUS)
#else
    if (tk .eq. ESMF_TYPEKIND_R4) then
       if (rank == 3) then
          call ESMF_LocalArrayGet(larray, ptr3dr4, rc=status)
          VERIFY_(STATUS)
          if (associated(ptr3dr4)) deallocate(ptr3dr4)
          NULLIFY(ptr3dr4)
          NULLIFY(wrap3dr4%ptr)

          call c_ESMC_LocalArraySetBaseAddr(larray, ptr3dr4, status) 
          VERIFY_(STATUS)

          call c_ESMC_LocalArraySetF90Ptr(larray, wrap3dr4, status) 
          VERIFY_(STATUS)
       else if (rank == 2) then
          call ESMF_LocalArrayGet(larray, ptr2dr4, rc=status)
          VERIFY_(STATUS)
          if (associated(ptr2dr4)) deallocate(ptr2dr4)
          NULLIFY(ptr2dr4)
          NULLIFY(wrap2dr4%ptr)

          call c_ESMC_LocalArraySetBaseAddr(larray, ptr2dr4, status) 
          VERIFY_(STATUS)

          call c_ESMC_LocalArraySetF90Ptr(larray, wrap2dr4, status) 
          VERIFY_(STATUS)
       else if (rank == 1) then
          call ESMF_LocalArrayGet(larray, ptr1dr4, rc=status)
          VERIFY_(STATUS)
          if (associated(ptr1dr4)) deallocate(ptr1dr4)
          NULLIFY(ptr1dr4)
          NULLIFY(wrap1dr4%ptr)

          call c_ESMC_LocalArraySetBaseAddr(larray, ptr1dr4, status) 
          VERIFY_(STATUS)

          call c_ESMC_LocalArraySetF90Ptr(larray, wrap1dr4, status) 
          VERIFY_(STATUS)
       end if
    else if (tk .eq. ESMF_TYPEKIND_R8) then
       if (rank == 3) then
          call ESMF_LocalArrayGet(larray, ptr3dr8, rc=status)
          VERIFY_(STATUS)
          if (associated(ptr3dr8)) deallocate(ptr3dr8)
          NULLIFY(ptr3dr8)
          NULLIFY(wrap3dr8%ptr)

          call c_ESMC_LocalArraySetBaseAddr(larray, ptr3dr8, status) 
          VERIFY_(STATUS)

          call c_ESMC_LocalArraySetF90Ptr(larray, wrap3dr8, status) 
          VERIFY_(STATUS)
       else if (rank == 2) then
          call ESMF_LocalArrayGet(larray, ptr2dr8, rc=status)
          VERIFY_(STATUS)
          if (associated(ptr2dr8)) deallocate(ptr2dr8)
          NULLIFY(ptr2dr8)
          NULLIFY(wrap2dr8%ptr)

          call c_ESMC_LocalArraySetBaseAddr(larray, ptr2dr8, status) 
          VERIFY_(STATUS)

          call c_ESMC_LocalArraySetF90Ptr(larray, wrap2dr8, status) 
          VERIFY_(STATUS)
       else if (rank == 1) then
          call ESMF_LocalArrayGet(larray, ptr1dr8, rc=status)
          VERIFY_(STATUS)
          if (associated(ptr1dr8)) deallocate(ptr1dr8)
          NULLIFY(ptr1dr8)
          NULLIFY(wrap1dr8%ptr)

          call c_ESMC_LocalArraySetBaseAddr(larray, ptr1dr8, status) 
          VERIFY_(STATUS)

          call c_ESMC_LocalArraySetF90Ptr(larray, wrap1dr8, status) 
          VERIFY_(STATUS)
       end if
    end if
#endif

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_ArrayF90Deallocate

  subroutine MAPL_FieldF90Deallocate(field, rc)
    type(ESMF_Field),  intent(INOUT) :: field
    integer, optional, intent(  OUT) :: rc             
    integer                                 :: status
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_FieldF90Deallocate'
    type(ESMF_Array)                        :: array

    call ESMF_FieldGet(field, Array=array, rc=status)
    VERIFY_(STATUS)
    call MAPL_ArrayF90Deallocate(array, rc=status)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_FieldF90Deallocate

  subroutine MAPL_DecomposeDim ( dim_world,dim,NDEs )
      implicit   none
      integer    dim_world, NDEs
      integer    dim(0:NDEs-1)
      integer    n,im,rm,nbeg,nend
      im = dim_world/NDEs
      rm = dim_world-NDEs*im
      do n=0,NDEs-1
                      dim(n) = im
      if( n.le.rm-1 ) dim(n) = im+1
      enddo
  end subroutine MAPL_DecomposeDim

  subroutine MAPL_Interp_Fac (TIME0, TIME1, TIME2, FAC1, FAC2, RC)

!------------------------------------------------------------        

!  PURPOSE:
!  ========
!
!    Compute interpolation factors, fac, to be used 
!    in the calculation of the instantaneous boundary 
!    conditions, ie:
!
!     q(i,j) = fac1*q1(i,j) + (1.-fac1)*q2(i,j)
!
!    where:
!     q(i,j)  => Boundary Data valid    at time0
!     q1(i,j) => Boundary Data centered at time1
!     q2(i,j) => Boundary Data centered at time2

!  INPUT:
!  ======
!    time0    : Time of current timestep
!    time1    : Time of boundary data 1 
!    time2    : Time of boundary data 2 

!  OUTPUT:
!  =======
!     fac1    : Interpolation factor for Boundary Data 1
!
! ------------------------------------------------------------        
!               GODDARD LABORATORY FOR ATMOSPHERES            
! ------------------------------------------------------------        

    type(ESMF_Time),   intent(in ) :: TIME0, TIME1, TIME2
    real,              intent(out) :: FAC1
    real,    optional, intent(out) :: FAC2
    integer, optional, intent(out) :: RC
 
    type(ESMF_TimeInterval)        :: TimeDif1
    type(ESMF_TimeInterval)        :: TimeDif
 
    TimeDif1 = TIME2-TIME0
    TimeDif  = TIME2-TIME1
       
    FAC1 = TimeDif1/TimeDif

    if(present(FAC2)) FAC2 = 1.-FAC1
    if(present(RC  )) RC   = ESMF_SUCCESS
 
  end subroutine MAPL_Interp_Fac

  subroutine MAPL_ClimInterpFac (CLOCK,I1,I2,FAC, RC)

!------------------------------------------------------------        

    type(ESMF_CLOCK),  intent(in ) :: CLOCK
    integer,           intent(OUT) :: I1, I2
    real,              intent(out) :: FAC
    integer, optional, intent(out) :: RC
 
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR), parameter   :: IAm='MAPL_ClimInterpFac'

    type (ESMF_Time)                  :: CurrTime
    type (ESMF_Time)                  :: midMonth
    type (ESMF_Time)                  :: BEFORE, AFTER
    type (ESMF_TimeInterval)          :: oneMonth
    type (ESMF_Calendar)              :: cal

    call ESMF_ClockGet       ( CLOCK,    CurrTime=CurrTime, calendar=cal, rc=STATUS )
    VERIFY_(STATUS)
    call ESMF_TimeGet        ( CurrTime, midMonth=midMonth,               rc=STATUS )
    VERIFY_(STATUS)
    call ESMF_TimeIntervalSet( oneMonth, MM = 1, calendar=cal,            rc=status )
    VERIFY_(STATUS)

    if( CURRTIME < midMonth ) then
       AFTER    = midMonth
       midMonth = midMonth - oneMonth
       call ESMF_TimeGet (midMonth, midMonth=BEFORE, rc=STATUS )
       VERIFY_(STATUS)
    else
       BEFORE   = midMonth
       midMonth = midMonth + oneMonth
       call ESMF_TimeGet (midMonth, midMonth=AFTER , rc=STATUS )
       VERIFY_(STATUS)
    endif

    call MAPL_Interp_Fac( CURRTIME, BEFORE, AFTER, FAC, RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_TimeGet (BEFORE, MM=I1, rc=STATUS )
    VERIFY_(STATUS)
    call ESMF_TimeGet (AFTER , MM=I2, rc=STATUS )
    VERIFY_(STATUS)
 

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_ClimInterpFac


subroutine MAPL_TimeStringGet(TIMESTRING,YY,MM,DD,H,M,S)
  character(len=*),  intent (IN ) :: TIMESTRING
  integer, optional, intent (OUT) :: YY
  integer, optional, intent (OUT) :: MM
  integer, optional, intent (OUT) :: DD
  integer, optional, intent (OUT) :: H
  integer, optional, intent (OUT) :: M
  integer, optional, intent (OUT) :: S

  integer :: IYY, IMM, IDD, IHH, IMN, ISS

  read(TIMESTRING,'(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') IYY,IMM,IDD,IHH,IMN,ISS
  
!ALT: SGI compiler does not like this format  read(TIMESTRING,'(I4,"-",I2,"-",I2,"T",I2,":",I2,":",I2)') IYY,IMM,IDD,IHH,IMN,ISS
  if(present(YY)) YY = IYY
  if(present(MM)) MM = IMM
  if(present(DD)) DD = IDD
  if(present(H )) H  = IHH
  if(present(M )) M  = IMN
  if(present(S )) S  = ISS

  return
end subroutine MAPL_TimeStringGet


subroutine MAPL_UnpackTime(TIME,IYY,IMM,IDD)
  integer, intent (IN ) :: TIME
  integer, intent (OUT) :: IYY
  integer, intent (OUT) :: IMM
  integer, intent (OUT) :: IDD
  IYY = TIME/10000
  IMM = mod(TIME/100,100)
  IDD = mod(TIME,100)
end subroutine MAPL_UnpackTime

subroutine MAPL_PackTime(TIME,IYY,IMM,IDD)
  integer, intent (OUT) :: TIME
  integer, intent (IN ) :: IYY
  integer, intent (IN ) :: IMM
  integer, intent (IN ) :: IDD
  TIME=IYY*10000+IMM*100+IDD
end subroutine MAPL_PackTime

subroutine MAPL_tick (nymd,nhms,ndt)
      integer nymd,nhms,ndt,nsec,nsecf
      nsecf(nhms) = nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
      IF(NDT.NE.0) THEN
      NSEC = NSECF(NHMS) + NDT
      IF (NSEC.GT.86400)  THEN
      DO WHILE (NSEC.GT.86400)
      NSEC = NSEC - 86400
      NYMD = MAPL_INCYMD (NYMD,1)
      ENDDO
      ENDIF   
      IF (NSEC.EQ.86400)  THEN
      NSEC = 0
      NYMD = MAPL_INCYMD (NYMD,1)
      ENDIF   
      IF (NSEC.LT.00000)  THEN
      DO WHILE (NSEC.LT.0)
      NSEC = 86400 + NSEC
      NYMD = MAPL_INCYMD (NYMD,-1)
      ENDDO
      ENDIF   
      NHMS = MAPL_NHMSF (NSEC)
      ENDIF   
      RETURN  
end subroutine MAPL_tick    

logical function MAPL_RTRN(A,iam,line,rc)
   integer,           intent(IN ) :: A
   character*(*),     intent(IN ) :: iam
   integer,           intent(IN ) :: line
   integer, optional, intent(OUT) :: RC

     MAPL_RTRN = .true.
     if(A/=ESMF_SUCCESS)print'(A40,I10)',Iam,line
     if(present(RC)) RC=A
end function MAPL_RTRN

logical function MAPL_VRFY(A,iam,line,rc)
   integer,           intent(IN ) :: A
   character*(*),     intent(IN ) :: iam
   integer,           intent(IN ) :: line
   integer, optional, intent(OUT) :: RC
     MAPL_VRFY = A/=ESMF_SUCCESS 
     if(MAPL_VRFY)then
       if(present(RC)) then
         print'(A40,I10)',Iam,line
         RC=A
       endif
     endif
end function MAPL_VRFY

logical function MAPL_ASRT(A,iam,line,rc)
   logical,           intent(IN ) :: A
   character*(*),     intent(IN ) :: iam
   integer,           intent(IN ) :: line
   integer, optional, intent(OUT) :: RC
     MAPL_ASRT = .not.A 
     if(MAPL_ASRT)then
       if(present(RC))then
         print'(A40,I10)',Iam,LINE
         RC=ESMF_FAILURE
       endif
     endif
end function MAPL_ASRT

logical function MAPL_RTRNt(A,text,iam,line,rc)
   integer,           intent(IN ) :: A
   character*(*),     intent(IN ) :: text,iam
   integer,           intent(IN ) :: line
   integer, optional, intent(OUT) :: RC

     MAPL_RTRNt = .true.
     if(A/=ESMF_SUCCESS)then
        print'(A40,I10)',Iam,line
        print *, text
     end if
     if(present(RC)) RC=A

end function MAPL_RTRNT

logical function MAPL_VRFYt(A,text,iam,line,rc)
   integer,           intent(IN ) :: A
   character*(*),     intent(IN ) :: iam,text
   integer,           intent(IN ) :: line
   integer, optional, intent(OUT) :: RC
     MAPL_VRFYt =  MAPL_VRFY(A,iam,line,rc)
     if(MAPL_VRFYt) print *, text
end function MAPL_VRFYT

logical function MAPL_ASRTt(A,text,iam,line,rc)
   logical,           intent(IN ) :: A
   character*(*),     intent(IN ) :: iam,text
   integer,           intent(IN ) :: line
   integer, optional, intent(OUT) :: RC
     MAPL_ASRTt =   MAPL_ASRT(A,iam,line,rc)
     if(MAPL_ASRTt) print *, text
end function MAPL_ASRTT

integer function MAPL_nsecf2 (nhhmmss,nmmdd,nymd)
      integer nhhmmss,nmmdd,nymd,nhms,nday,month
      integer nsday, ncycle,iday,iday2
      integer nsecf,i,nsegm,nsegd
      PARAMETER ( NSDAY  = 86400 )
      PARAMETER ( NCYCLE = 1461*24*3600 )
      INTEGER YEAR, DAY, SEC, YEAR0, DAY0, SEC0
      integer    MNDY(12,4), mnd48(48)
      DATA MND48/0,31,60,91,121,152,182,213,244,274,305,335,366,397,34*0 /
!     DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,397,34*0 /
      equivalence ( mndy(1,1), mnd48(1) )
      nsecf(nhms) = nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
      MAPL_nsecf2 = nsecf( nhhmmss )
      if( nmmdd.eq.0 ) return
      DO 100 I=15,48
!     MNDY(I,1) = MNDY(I-12,1) + 365
      MND48(I) = MND48(I-12) + 365
100   CONTINUE
      nsegm =     nmmdd/100
      nsegd = mod(nmmdd,100)
      YEAR   = NYMD / 10000
      MONTH  = MOD(NYMD,10000) / 100
      DAY    = MOD(NYMD,100)
      SEC    = NSECF(nhhmmss)
      IDAY   = MNDY( MONTH ,MOD(YEAR ,4)+1 )
      month = month + nsegm
      If( month.gt.12 ) then
      month = month - 12
      year = year + 1
      endif
      IDAY2  = MNDY( MONTH ,MOD(YEAR ,4)+1 )
                    nday = iday2-iday
      if(nday.lt.0) nday = nday + 1461
                    nday = nday + nsegd
      MAPL_nsecf2 = MAPL_nsecf2 + nday*nsday
end function MAPL_nsecf2

integer function MAPL_nhmsf (nsec)
        implicit none
        integer  nsec
        MAPL_nhmsf =  nsec/3600*10000 + mod(nsec,3600)/60*100 + mod(nsec,60)
end function MAPL_nhmsf

integer function MAPL_incymd (NYMD,M)                                                  
      integer nymd,ny,nm,nd,m,ny00
      INTEGER NDPM(12)                                                          
      DATA    NDPM /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/             
      LOGICAL LEAP                                                              
      DATA    NY00     / 1900 /                                                 
      LEAP(NY) = MOD(NY,4).EQ.0 .AND. (NY.NE.0 .OR. MOD(NY00,400).EQ.0)         
      NY = NYMD / 10000                                                         
      NM = MOD(NYMD,10000) / 100                                                
      ND = MOD(NYMD,100) + M                                                    
      IF (ND.EQ.0) THEN                                                         
      NM = NM - 1                                                               
      IF (NM.EQ.0) THEN                                                         
          NM = 12                                                               
          NY = NY - 1                                                           
      ENDIF                                                                     
      ND = NDPM(NM)                                                             
      IF (NM.EQ.2 .AND. LEAP(NY))  ND = 29                                      
      ENDIF                                                                     
      IF (ND.EQ.29 .AND. NM.EQ.2 .AND. LEAP(NY))  GO TO 20                      
      IF (ND.GT.NDPM(NM)) THEN                                                  
      ND = 1                                                                    
      NM = NM + 1                                                               
      IF (NM.GT.12) THEN                                                        
          NM = 1                                                                
          NY = NY + 1                                                           
      ENDIF                                                                     
      ENDIF                                                                     
   20 CONTINUE                                                                  
      MAPL_INCYMD = NY*10000 + NM*100 + ND                                           
      RETURN                                                                    
end function MAPL_incymd


subroutine MAPL_PICKEM(II,JJ,IM,JM,COUNT)
integer, intent(IN ) :: IM, JM, COUNT
integer, intent(OUT) :: II(COUNT), JJ(COUNT)

integer, parameter :: NT=3

logical :: MASK(IM,JM)
integer :: L, NN, IX, JX
real    :: IIR(NT*COUNT), JJR(NT*COUNT)

   MASK=.true.

   NN=1

   call RANDOM_NUMBER(IIR)
   call RANDOM_NUMBER(JJR)


   do L=1, COUNT

      do
         IX=IIR(NN)*(IM-1)+2
         JX=JJR(NN)*(JM-2)+2

         NN = NN + 1

         if(MASK(IX,JX)) then
            II(L) = IX
            JJ(L) = JX
            MASK(IX-1:IX+1,JX-1:JX+1) = .false.
            exit
         endif

         if(NN>NT*COUNT) stop 222

      enddo
   enddo

!!$   DO L=1,JM
!!$      PRINT '(144L1)',MASK(:,L) 
!!$   ENDDO
!!$
!!$   PRINT *, COUNT, NN

   return
 end subroutine MAPL_PICKEM




    subroutine MAPL_GetFieldTimeFromField ( FIELD, TIME, RC )
      type(ESMF_Field),        intent(INOUT) :: FIELD ! ALT: IN
      type(ESMF_Time),         intent(  OUT) :: TIME
      integer, optional,       intent(  OUT) :: RC

      character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_GetFieldTimeFromField"
      integer                                :: STATUS

      integer                                :: YEAR, MONTH, DAY
      integer                                :: HOUR, MINUTE, SCND
      character(len=ESMF_MAXSTR)             :: TIMESTAMP

      call ESMF_AttributeGet(FIELD, NAME="TimeStamp", VALUE=TIMESTAMP, RC=STATUS)
      if(STATUS/=0) then
         call ESMF_TimeSet          (TIME,      YY=0,                RC=STATUS)
      else
         call MAPL_TimeStringGet    (TIMESTAMP, YY=YEAR, MM=MONTH,  DD=DAY,   & 
                                                H =HOUR, M =MINUTE, S =SCND   )
         VERIFY_(STATUS)
         call ESMF_TimeSet          (TIME,      YY=YEAR, MM=MONTH,  DD=DAY,   &
                                                H =HOUR, M =MINUTE, S =SCND,  &
                                                                     RC=STATUS)
         VERIFY_(STATUS)
      end if

      RETURN_(ESMF_SUCCESS)
    end subroutine MAPL_GetFieldTimeFromField

! ------------------------------------------------------------------------------

    subroutine  MAPL_SetFieldTimeFromField (FIELD, TIME, RC )
      type(ESMF_FIELD),        intent(INOUT) :: FIELD
      type(ESMF_TIME),         intent(INOUT) :: TIME !ALT: IN
      integer, optional,       intent(  OUT) :: RC

      character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_SetFieldTimeFromField"
      integer                                :: STATUS

      integer                                :: YEAR, MONTH, DAY
      character(len=ESMF_MAXSTR)             :: TIMESTAMP

      call ESMF_TimeGet          (TIME,  timeString=TIMESTAMP,             RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_AttributeSet(FIELD, NAME="TimeStamp", VALUE=TIMESTAMP, RC=STATUS)
      VERIFY_(STATUS)

      RETURN_(ESMF_SUCCESS)
    end subroutine  MAPL_SetFieldTimeFromField


    subroutine  MAPL_GetFieldTimeFromState ( STATE, Fieldname, TIME, RC )
      type(ESMF_STATE),        intent(IN   ) :: STATE
      character(len=*),        intent(IN   ) :: Fieldname
      type(ESMF_Time),         intent(  OUT) :: TIME
      integer, optional,       intent(  OUT) :: RC

      character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_GetFieldTimeFromState"
      integer                                :: STATUS

      type(ESMF_FIELD)                       :: FIELD
      integer                                :: YEAR, MONTH, DAY
      character(len=ESMF_MAXSTR)             :: TIMESTAMP

      call ESMF_StateGet (STATE, FIELDNAME, FIELD, RC=STATUS )
      VERIFY_(STATUS)
      call MAPL_FieldGetTime  (FIELD, TIME,             RC=STATUS)
      VERIFY_(STATUS)

      RETURN_(ESMF_SUCCESS)
    end subroutine  MAPL_GetFieldTimeFromState

! ------------------------------------------------------------------------------

    subroutine  MAPL_SetFieldTimeFromState ( STATE, Fieldname, TIME, RC )
      type(ESMF_STATE),        intent(INOUT) :: STATE
      character(len=*),        intent(IN   ) :: Fieldname
      type(ESMF_Time),         intent(INOUT) :: TIME !ALT: IN
      integer, optional,       intent(  OUT) :: RC

      character(len=ESMF_MAXSTR),parameter   :: IAm=" MAPL_SetFieldTimeFromState"
      integer                                :: STATUS

      type(ESMF_FIELD)                       :: FIELD
      integer                                :: YEAR, MONTH, DAY
      character(len=ESMF_MAXSTR)             :: TIMESTAMP

      call ESMF_StateGet (STATE, FIELDNAME, FIELD, RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_FieldSetTime  (FIELD, TIME,             RC=STATUS)
      VERIFY_(STATUS)

      RETURN_(ESMF_SUCCESS)
    end subroutine  MAPL_SetFieldTimeFromState


    function MAPL_FieldCreateRename(FIELD, NAME, RC) RESULT(F)
      type (ESMF_Field), intent(INOUT) :: FIELD !ALT: IN
      character(len=*),  intent(IN   ) :: NAME
      integer, optional, intent(  OUT) :: RC
      type (ESMF_Field)                :: F

!   we are creating new field so that we can change the name of the field;
!   the important thing is that the data (ESMF_Array) and the grid (ESMF_Grid) 
!   are the SAME as the one in the original Field

      type(ESMF_RelLoc)       :: relloc
      type(ESMF_Grid)         :: grid
      type(ESMF_Array)        :: array
      character(len=ESMF_MAXSTR)       :: attname
      integer, allocatable    :: gridToFieldMap(:)
      integer                 :: gridRank
      integer                 :: status
      character(len=ESMF_MAXSTR), parameter :: Iam='MAPL_FieldCreateRename'

!ALT added kludge (next 6 lines)
      call ESMF_FieldGet(FIELD, name=attname, RC=STATUS)
      VERIFY_(STATUS)
      if (NAME == attname) then
         F = FIELD
         RETURN_(ESMF_SUCCESS)
      endif
      call ESMF_FieldGet(FIELD, grid=GRID, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
      VERIFY_(STATUS)
      allocate(gridToFieldMap(gridRank), stat=status)
      VERIFY_(STATUS)
      call ESMF_FieldGet(FIELD, Array=Array, gridToFieldMap=gridToFieldMap, RC=STATUS)
      VERIFY_(STATUS)
      
      F = ESMF_FieldCreate(GRID, ARRAY, name=NAME, gridToFieldMap=gridToFieldMap, RC=status)
      VERIFY_(STATUS)

      deallocate(gridToFieldMap)

      call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, RC=status)
      VERIFY_(STATUS)

      RETURN_(ESMF_SUCCESS)
    end function MAPL_FieldCreateRename

    function MAPL_FieldCreateNewgrid(FIELD, GRID, RC) RESULT(F)
      type (ESMF_Field), intent(INOUT) :: FIELD !ALT: intent(IN)
      type (ESMF_Grid),  intent(INout) :: GRID
      integer, optional, intent(  OUT) :: RC
      type (ESMF_Field)                :: F

!   we are creating new field so that we can change the grid of the field 
!   (and allocate array accordingly);
!ALT: This function is currently used only in History for regridding on an output grid

!      type(ESMF_FieldDataMap) :: datamap           
      type(ESMF_RelLoc)       :: hrelloc
      type(ESMF_RelLoc)       :: vrelloc
      type(ESMF_Array)        :: array
      integer                 :: rank
      integer                 :: COUNTS(3)
      real, pointer           :: VAR_1D(:), VAR_2D(:,:), VAR_3D(:,:,:)
      character(len=ESMF_MAXSTR) :: NAME
      integer                 :: status
      integer                 :: DIMS
      integer, allocatable    :: gridToFieldMap(:)
      integer                 :: gridRank
      integer                 :: lb, ub
      character(len=ESMF_MAXSTR), parameter :: Iam='MAPL_FieldCreateNewgrid'

      call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
      VERIFY_(STATUS)
      allocate(gridToFieldMap(gridRank), stat=status)
      VERIFY_(STATUS)
      call ESMF_FieldGet(FIELD, Array=Array, name=name, &
           gridToFieldMap=gridToFieldMap, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_ArrayGet(array, rank=rank, rc=status)
      VERIFY_(STATUS)

      call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, RC=STATUS)
      VERIFY_(STATUS)

      if (rank == 1 .or. rank == 2) then
!ALT halowidth assumed 0
         allocate(VAR_2D(COUNTS(1), COUNTS(2)), STAT=STATUS)
         VERIFY_(STATUS)
         VAR_2D = 0.0
         F = ESMF_FieldCreate(GRID, VAR_2D, copyflag=ESMF_DATA_REF, &
              name=NAME, gridToFieldMap=gridToFieldMap, RC=STATUS )
         VERIFY_(STATUS)
         DIMS = MAPL_DimsHorzOnly
      else
!ALT halowidth assumed 0
         call ESMF_ArrayGet(ARRAY, localDE=0, farrayPtr=VAR_3D, RC=STATUS)
         VERIFY_(STATUS)
         lb = lbound(VAR_3D,3)
         ub = ubound(VAR_3D,3)
         ASSERT_(ub-lb+1==counts(3))
         allocate(VAR_3D(COUNTS(1), COUNTS(2), lb:ub), STAT=STATUS)
         VERIFY_(STATUS)
         VAR_3D = 0.0
         F = ESMF_FieldCreate(GRID, VAR_3D, copyflag=ESMF_DATA_REF, &
              name=NAME, gridToFieldMap=gridToFieldMap, RC=STATUS )
         DIMS = MAPL_DimsHorzVert
      end if

      deallocate(gridToFieldMap)

! we are saving DIMS attribute in case the FIELD did not contain one
! otherwise we will overwrite it
      call ESMF_AttributeSet(F, NAME='DIMS', VALUE=DIMS, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT=f, RC=status)
      VERIFY_(STATUS)

      RETURN_(ESMF_SUCCESS)
    end function MAPL_FieldCreateNewgrid

    subroutine MAPL_FieldCopyAttributes(FIELD_IN, FIELD_OUT, RC)
      type (ESMF_Field), intent(INOUT) :: FIELD_IN !ALT: intent(in)
      type (ESMF_Field), intent(INOUT) :: FIELD_OUT
      integer, optional, intent(  OUT) :: RC

      type (ESMF_TypeKind)             :: tk
      integer                          :: status
      character(len=ESMF_MAXSTR), parameter :: Iam='MAPL_FieldCopyAttributes'
      integer                          :: i, n, count
      character(len=ESMF_MAXSTR)       :: attname
      character(len=ESMF_MAXSTR)       :: att
      integer, pointer                 :: iptr(:)
      type(ESMF_Logical), pointer      :: lptr(:)
      real,    pointer                 :: rptr(:)

      call ESMF_AttributeGet(field_in, count=n, rc=status)
      VERIFY_(STATUS)

      do i = 1, n
         call  ESMF_AttributeGet(field_in, attributeIndex=i, name=attname, &
                                          typekind=tk, count=count, rc=status)
         VERIFY_(STATUS)

         if (tk == ESMF_TypeKind_I4) then
            allocate(iptr(count), stat=status)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(field_in,  NAME=attname, count=count, VALUELIST=iptr, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(field_out, NAME=attname, count=count, VALUELIST=iptr, RC=STATUS)
            VERIFY_(STATUS)
            deallocate(iptr)

         else if (tk == ESMF_TypeKind_Logical) then
            allocate(lptr(count), stat=status)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(field_in,  NAME=attname, count=count, VALUELIST=lptr, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(field_out, NAME=attname, count=count, VALUELIST=lptr, RC=STATUS)
            VERIFY_(STATUS)
            deallocate(lptr)

         else if (tk == ESMF_TypeKind_R4) then
            allocate(rptr(count), stat=status)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(field_in,  NAME=attname, count=count, VALUELIST=rptr, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(field_out, NAME=attname, count=count, VALUELIST=rptr, RC=STATUS)
            VERIFY_(STATUS)
            deallocate(rptr)

         else if (tk == ESMF_TypeKind_Character) then
            call ESMF_AttributeGet(field_in,  NAME=attname, VALUE=att, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(field_out, NAME=attname, VALUE=att, RC=STATUS)
            VERIFY_(STATUS)

         else
            RETURN_(ESMF_FAILURE)
         end if
      end do
      RETURN_(ESMF_SUCCESS)
    end subroutine MAPL_FieldCopyAttributes

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      function MAPL_RemapBounds_3dr4(A,I1,IM,J1,JM,L1,LM)
        integer,      intent(IN) :: I1,IM,J1,JM,L1,LM
        real, target, intent(IN) :: A(I1:IM,J1:JM,L1:LM)
        real, pointer            :: MAPL_RemapBounds_3dr4(:,:,:)

        MAPL_RemapBounds_3dr4 => A
      end function MAPL_RemapBounds_3dr4

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!BOP

! !IROUTINE: MAPL_LatLonGridCreate --- Create regular Lat/Lon Grid
!
! !INTERFACE:

  function MAPL_LatLonGridCreate (Name, vm,                 &
                                  Config, ConfigFile,       &
                                  Nx, Ny,                   &
                                  IM_World, BegLon, DelLon, &
                                  JM_World, BegLat, DelLat, &
                                  LM_World,                 &
                                  rc)                       &
  result(Grid)

! !INPUT PARAMETERS:

    character(len=*),            intent(in)  :: Name
    type (ESMF_VM),    OPTIONAL, target,     &
                                 intent(in)  :: VM


!   There are 3 possibilities to provide the coordinate information:

                                             ! 1) Thru Config object:
    type(ESMF_Config), OPTIONAL, target,     & 
                                 intent(in)  :: Config 

                                             ! 2) Thru a resource file:
    character(len=*),  OPTIONAL, intent(in)  :: ConfigFile
        

                                             ! 3) Thru argument list:
    integer,           OPTIONAL, intent(in)  :: Nx, Ny          ! Layout
    integer,           OPTIONAL, intent(in)  :: IM_World        ! Zonal 
    real,              OPTIONAL, intent(in)  :: BegLon, DelLon  ! in degrees

    integer,           OPTIONAL, intent(in)  :: JM_World        ! Meridional
    real,              OPTIONAL, intent(in)  :: BegLat, DelLat  ! in degrees
    
    integer,           OPTIONAL, intent(in)  :: LM_World        ! Vertical
    
! !OUTPUT PARAMETERS:

    type (ESMF_Grid)                         :: Grid  ! Distributed grid
    integer,           OPTIONAL, intent(out) :: rc    ! return code

#ifdef ___PROTEX___

!DESCRIPTION: 

This routine creates a distributed ESMF grid where the horizontal
coordinates are regular longitudes and latitudes. The grid is 
created on the user specified {\bf VM}, or on the current VM if the user 
does not specify one. The layout and the coordinate information can
be provided with a {\tt ESMF\_Config attribute}, a resource file name
or specified through the argument list.

 \subsubsection*{Using resource files}

The {\bf resource file} {\tt ConfigFile} has a syntax similar to a GrADS
control file.  Here is an example defining a typical GEOS-5 1x1.25
grid with 72 layers:
%
\begin{verbatim}
GDEF: LatLon 
IDEF: 32  
JDEF: 16  
LDEF:  1  
XDEF: 288 LINEAR -180. 1.25
YDEF: 181 LINEAR -90. 1.
ZDEF:  72 LINEAR 1 1
\end{verbatim}
%
More generally, 
\begin{verbatim}
GDEF: LatLon 
IDEF: Nx 
JDEF: Ny
LDEF: Nz
XDEF: IM_World XCoordType BegLon, DelLon
YDEF: JM_World YCoordType BegLat, DelLat
ZDEF: LM_World ZCoordType 1        1
\end{verbatim}
The attribute {\bf GDEF} must always be {\tt LatLon} for  Lat/Lon grids. 
The remaining parameters are:
\bd
\item[Nx] is the number of processors used to decompose the X dimension
\item[Ny] is the number of processors used to decompose the Y dimension
\item[Nz] is the number of processors used to decompose the Z dimension;
          must be 1 for now.          
\item[IM\_World] is the number of longitudinal grid points; 
         if {\tt IM\_World=0} then the grid has no zonal dimension.
\item[XCoordType] must be set to LINEAR
\item[BegLon] is the longitude (in degrees) of the {\em center} of the first 
              gridbox
\item[DelLon] is the constant mesh size (in degrees); if {\tt DelLon<1} then a
            global grid is assumed.
%
\item[JM\_World] is the number of meridional grid points
         if {\tt JM\_World=0} then the grid has no meridional dimension.
\item[YCoordType] must be set to LINEAR
\item[BegLat] is the latitude (in degrees) of the {\em center} of the first 
              gridbox
\item[DelLat] is the constant mesh size (in degrees); if {\tt DelLat<1} then a
              global grid is assumed.
%
\item[LM\_World] is the number of vertical grid points;
              if {\tt LM\_World=0} then the grid has no vertical dimension.
\ed
As of this writing, only the size of the vertical grid ({\tt LM\_World})
needs to be specified.

 \subsubsection*{Passing an ESMF Config}

The {\bf ESMF\_Config} object {\tt Config}, when specified, must
contain the same information as the resource file above.

subsubsection*{Providing parameters explicitly through the argument list}

Alternatively, one can specify coordinate information in the argument
list; their units and meaning is as in the resource file above. In
this case you must specify at least {\tt Nx, Ny, IM\_World, JM\_World,} and 
{\tt LM\_World}. The other parameters have default values
\bd
\item[BegLon] defaults to -180. (the date line)
\item[DelLon] defaults to -1. (meaning a global grid)
\item[BegLat] defaults to -90. (the south pole)
\item[DelLat] deaults to -1. (meaning a global grid)
\ed

  \subsubsection*{Restrictions}

The current implementation imposes the following 
restrictions:
\begin{enumerate}
\item Only uniform longitude/latitude grids are supported (no Gaussian grids).
\item Only 2D Lon-Lat or 3D Lon-Lat-Lev grids are currently supported 
      (no Lat-Lev or Lon-Lev grids supprted yet).
\item No vertical decomposition yet ({\tt Nz=1}).
\end{enumerate}

 \subsubsection*{Future enhancements}

The {\tt IDEF/JDEF/LDEF} records in the resource file should be
extended as to allow specification of a more general distribution.
For consistency with the {\tt XDEF/YDEF/ZDEF} records a similar 
syntax could be adopted. For example,
%
\begin{verbatim}
IDEF 4   LEVELS  22 50 50 22 
XDEF 144 LINEAR -180 2.5 
\end{verbatim}
would indicate that longitudes would be decomposed in 4 PETs,
with the first PET having 22 grid points, the second 50 gridpoints,
and so on. 

#endif

!
!EOP
!                                 ------


!   Internal version of the input arguments
!   ---------------------------------------
    type(ESMF_Config), pointer :: Config_
    integer           :: IM_World_      
    real              :: BegLon_
    real              :: DelLon_ 
    integer           :: JM_World_      
    real              :: BegLat_
    real              :: DelLat_
    integer           :: LM_World_ 
    integer           :: Nx_, Ny_, Nz_

    integer, allocatable            :: IMs(:), JMs(:), LMs(:)
    real(ESMF_KIND_R8)              :: minCoord(3)
    real(ESMF_KIND_R8)              :: deltaX, deltaY
    type (ESMF_VM), pointer         :: VM_
    integer                         :: I, J, I1, IN, J1, JN

    real(ESMF_KIND_R8), pointer     :: centerX(:,:)
    real(ESMF_KIND_R8), pointer     :: centerY(:,:)
    real(ESMF_KIND_R8), allocatable :: cornerX(:)
    real(ESMF_KIND_R8), allocatable :: cornerY(:)

    real, parameter                 :: D2R = MAPL_PI / 180.

    integer                         :: STATUS
    character(len=ESMF_MAXSTR)      :: IAm='MAPL_LatLonGridCreate'

!                                ------

!  Defaults
!  --------
   BegLon_ = -180.0  ! centered at date line  
   DelLon_ =   -1.0  ! means global grid
   BegLat_ =  -90.0  ! centered at south pole
   DelLat_ =   -1.0  ! means global grid
   Nz_     =  1      ! place holder for now

!  Either user specified VM or current one
!  ---------------------------------------
   if ( present(vm) ) then
      vm_ => vm
   else
      allocate(vm_, stat=STATUS)
      VERIFY_(STATUS)
      call ESMF_VMGetCurrent(vm_, rc=STATUS)
      VERIFY_(STATUS)
   end if

! Grid info via resources
! -----------------------
  if ( present(Config) .or. present(ConfigFile) ) then

!    Either use supplied Config or load resource file
!    ------------------------------------------------
     if ( present(ConfigFile) ) then
          allocate(Config_,stat=STATUS)
          VERIFY_(STATUS)
          Config_ = ESMF_ConfigCreate (rc=STATUS )
          VERIFY_(STATUS)
          call ESMF_ConfigLoadFile (Config_, ConfigFile, rc=STATUS )
          VERIFY_(STATUS)
     else if ( present(Config) ) then
          Config_ => Config
     else
        STATUS = 100
        VERIFY_(STATUS)
     end if

!    Get relevant parameters from Config
!    -----------------------------------
     call parseConfig_()                            ! internal routine

!  Grid info thru argument list
!  ----------------------------
   else if ( present(IM_World) .AND. &
             present(JM_World) .AND. &
             present(LM_World) .AND. &
             present(Nx)       .AND. &
             present(Ny)             ) then

             IM_World_ = IM_World
             JM_World_ = JM_World
             LM_World_ = LM_World

             Nx_ = Nx
             Ny_ = Ny

             if ( present(BegLon) ) BegLon_ = BegLon
             if ( present(DelLon) ) DelLon_ = DelLon
             if ( present(BegLat) ) BegLat_ = BegLat
             if ( present(DelLat) ) DelLat_ = DelLat

     continue  ! all is well

!  Something is missing
!  --------------------
   else

     STATUS = 300
     VERIFY_(STATUS)

   end if
  
!  Global grids
!  ------------
   if ( IM_World_ < 1 .OR. JM_World_ < 1 ) then
        STATUS = 400
        VERIFY_(STATUS)
   end if
   if ( DelLon_ < 0.0 ) then  ! convention for global grids
      if ( IM_World_ == 1 ) then
           DelLon_ = 0.0
      else                  
           DelLon_ = 360. / IM_World_
      end if
   end if
   if ( DelLat_ < 0.0 ) then  ! convention for global grids
      if ( JM_World_ == 1 ) then
           DelLat_ = 0.0
      else                  
           DelLat_ = 180. / ( JM_World_ - 1)
      end if
   end if

!  Give the IMs, JMs and LMs the MAPL default distribution
!  -------------------------------------------------------
   allocate( IMs(0:Nx_-1), JMs(0:Ny_-1), LMs(0:Nz_-1), stat=STATUS)
   VERIFY_(STATUS) 
   call MAPL_DecomposeDim ( IM_World_, IMs, Nx_ )
   call MAPL_DecomposeDim ( JM_World_, JMs, Ny_ )
   call MAPL_DecomposeDim ( LM_World_, LMs, Nz_ )

!  ------------------------------------------------------------
!  TO DO: implement IMs/JMs/LMs as part of the IDEF/JDEF record
!         our thru command line
!  ------------------------------------------------------------

!  3D Lat-Lon-Lev Grid
!  -------------------
   if ( LM_World_>0 .AND. IM_World_>0 .AND. JM_World_>0 ) then 
        Grid = ESMF_GridCreateShapeTile (     &
               name=Name,                     &
               countsPerDEDim1=IMs,           &
               countsPerDEDim2=JMs,           &
               countsPerDEDim3=LMs,           &
               coordDep1 = (/1,2/),           &
               coordDep2 = (/1,2/),           &
               coordDep3 = (/3/),             &
               gridEdgeLWidth = (/0,0,0/),    &
               gridEdgeUWidth = (/0,0,0/),    &
               rc=STATUS)
          VERIFY_(STATUS)

!  2D Lat-Lon Grid
!  ---------------
   else if ( LM_World_==0 .AND. IM_World_>0 .AND. JM_World>0 ) then 
          Grid = ESMF_GridCreateShapeTile(    &
               name=Name,                 &
               countsPerDEDim1=IMs,           &
               countsPerDEDim2=JMs,           &
               coordDep1 = (/1,2/),           &
               coordDep2 = (/1,2/),           &
               gridEdgeLWidth = (/0,0/),      &
               gridEdgeUWidth = (/0,0/),      &
               rc=STATUS)
          VERIFY_(STATUS)

!  Other possibilities not implemented yet
!  --------------------------------------- 
   else
 
          STATUS = 300
          VERIFY_(STATUS)

   endif

!  -------------------------------------------------------------------
!  NOTE: In the remaining part of this routine it is assumed that the 
!        1st and 2nd axes correspond to lat/lon; revise this for other 
!        arrangements (say, YZ grids)
!  -------------------------------------------------------------------

!  Allocate coords at default stagger location
!  -------------------------------------------
   call ESMF_GridAddCoord(Grid, rc=status)
   VERIFY_(STATUS)

!  Compute the coordinates (the corner/center is for backward compatibility)
!  -------------------------------------------------------------------------
   deltaX      = D2R * DelLon_
   deltaY      = D2R * DelLat_
   minCoord(1) = D2R * BegLon_ - deltaX/2 
   minCoord(2) = D2R * BegLat_ - deltaY/2 

   allocate(cornerX(IM_World_+1),cornerY(JM_World_+1), stat=STATUS)
   VERIFY_(STATUS)
   
   cornerX(1) = minCoord(1)
   do i = 1,IM_World_
      cornerX(i+1) = cornerX(i) + deltaX
   enddo
   
   cornerY(1) = minCoord(2)
   do j = 1,JM_World_
      cornerY(j+1) = cornerY(j) + deltaY
   enddo
   
!  Retrieve the coordinates so we can set them
!  -------------------------------------------
   call ESMF_GridGetCoord (Grid, coordDim=1, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           fptr=centerX, rc=status)
   VERIFY_(STATUS)
   
   call ESMF_GridGetCoord (Grid, coordDim=2, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           fptr=centerY, rc=status)
   VERIFY_(STATUS)
   
   call MAPL_GridGetInterior (Grid,i1,in,j1,jn)
   
   do i = 1,size(centerX,1)
      centerX(i,:) = 0.5d0*(cornerX(i+i1-1)+cornerX(i+i1))
   end do
   
   do j = 1,size(centerY,2)
      centerY(:,j) = 0.5d0*(cornerY(j+j1-1)+cornerY(j+j1))
   enddo
   
   
!  Make sure we've got it right
!  ----------------------------
   call ESMF_GridValidate(Grid,rc=status)
   VERIFY_(STATUS)

!  Clean up
!  --------   
   deallocate(cornerY,cornerX)
   deallocate(IMs,JMs,LMs)
   if ( present(ConfigFile) ) deallocate(Config_)
   if ( .not. present(vm) )   deallocate(vm_)

!  All Done
!  --------
   RETURN_(STATUS)

   Contains

     Subroutine parseConfig_()
!
!    Internal routine to parse the ESMF_Config.
!
       STATUS = 200     ! not implemented yet
       VERIFY_(STATUS)

     end Subroutine parseConfig_

   end function MAPL_LatLonGridCreate

!............................................................................

  subroutine MAPL_GridGet(GRID, globalCellCountPerDim, localCellCountPerDim, RC)
      type (ESMF_Grid), intent(INOUT) :: GRID
      integer, optional, intent(INout) :: globalCellCountPerDim(:)
      integer, optional, intent(INout) :: localCellCountPerDim(:)
      integer, optional, intent(  OUT) :: RC

! local vars
      integer :: status
      character(len=ESMF_MAXSTR) :: Iam="MAPL_GridGet"

      integer :: mincounts(ESMF_MAXDIM)
      integer :: maxcounts(ESMF_MAXDIM)
      integer :: gridRank
      integer :: UNGRID
      integer :: sz
      logical :: plocal, pglobal, lxtradim


      pglobal = present(globalCellCountPerDim)
      plocal  = present(localCellCountPerDim)
      if (.not. (pglobal .or. plocal)) then
         RETURN_(ESMF_FAILURE)
      end if

      call ESMF_GridGet(grid, dimCount=gridRank, rc=status)
      VERIFY_(STATUS)

!ALT kludge
      lxtradim = .false.
      if (gridRank == 1) then
         call ESMF_AttributeGet(grid, name='GRID_EXTRADIM', &
              value=UNGRID, rc=status)
         if (status == ESMF_SUCCESS) then
            lxtradim = .true.
         end if
      end if

      if (pglobal) then

         globalCellCountPerDim = 0

         call ESMF_GridGet(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
              minIndex=mincounts, &
              maxIndex=maxcounts, &
              rc = status)
         VERIFY_(STATUS)

         sz = min(gridRank, ESMF_MAXDIM, size(globalCellCountPerDim)) 
         globalCellCountPerDim(1:sz) = maxcounts(1:sz)-mincounts(1:sz)+1

         if (lxtradim ) then
            globalCellCountPerDim(gridRank+1) = UNGRID
         end if
      end if

      if (plocal) then
         localCellCountPerDim = 0

         call ESMF_GridGet(GRID, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CENTER, &
              exclusiveCount=localCellCountPerDim, RC=STATUS)
         VERIFY_(STATUS)

         if (lxtradim ) then
            localCellCountPerDim(gridRank+1) = UNGRID
         end if
      end if

      RETURN_(ESMF_SUCCESS)

    end subroutine MAPL_GridGet

!
! Note: The routine below came from ESMFL; it has been moved here to
!       avoid circular dependencies (Arlindo).
!
    subroutine MAPL_GridGetInterior(GRID,I1,IN,J1,JN)
    type (ESMF_Grid), intent(IN) :: grid
    integer, intent(OUT)         :: I1, IN, J1, JN

! local vars
    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='MAPL_GridGetInterior'

    type (ESMF_DistGrid)                  :: distGrid
    type(ESMF_DELayout)                   :: LAYOUT
    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer                               :: nDEs
    integer                               :: deId
    integer                               :: gridRank
    integer                               :: deList(1)

    call ESMF_GridGet    (GRID, dimCount=gridRank, distGrid=distGrid, rc=STATUS)
    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS)
    call ESMF_DELayoutGet(layout, deCount =nDEs, localDeList=deList, rc=status)
    deId = deList(1)

    allocate (AL(gridRank,0:nDEs-1),  stat=status)
    allocate (AU(gridRank,0:nDEs-1),  stat=status)

    call ESMF_DistGridGet(distgrid, &
         minIndexPDimPDe=AL, maxIndexPDimPDe=AU, rc=status)

    I1 = AL(1, deId)
    IN = AU(1, deId)
!    ASSERT_(gridRank > 1) !ALT: tilegrid is 1d (without RC this only for info)
    J1 = AL(2, deId)
    JN = AU(2, deId)
    deallocate(AU, AL)

  end subroutine MAPL_GridGetInterior

!.......................................................................

     function MAPL_RmQualifier(str, del) result(new)

     character(len=*),           intent(in)  :: str
     character(len=*), optional, intent(in)  :: del ! optional delimiter

     character(len=ESMF_MAXSTR) :: new
     
!
!     Simple function to remove qualifier from a string. For example,
!     MAPL_RmQualifier('GOCART::du001') yields "du001". By default,
!     '::' is used as the qualifier delimiter.
!
     character(len=ESMF_MAXSTR) :: del_
     integer :: i
     if ( present(del) ) then
        del_ = del
     else
        del_ = '::'
     end if
     new = adjustl(str)
     i = index(str,trim(del_))
     if ( i > 0 ) new = new(i+2:)
   end function MAPL_RmQualifier

end module MAPL_BaseMod
