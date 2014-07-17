!  $Id: MAPL_LocStreamMod.F90,v 1.23 2009/03/26 13:23:52 f4mjs Exp $

#include "MAPL_ErrLog.h"

!=============================================================================
!BOP

! !MODULE: MAPL_LocStreamMod -- A class for manipulation location streams

module MAPL_LocStreamMod

! !USES:

use ESMF_Mod
use ESMFL_Mod
use MAPL_BaseMod
use MAPL_ConstantsMod
use MAPL_IOMod
use MAPL_CommsMod

implicit none
private

! !PUBLIC ROUTINES:

public MAPL_LocStreamCreate
public MAPL_LocStreamAttachGrid
public MAPL_LocStreamTransform
public MAPL_LocStreamIsAssociated
public MAPL_LocStreamXformIsAssociated
public MAPL_LocStreamGet
public MAPL_LocStreamCreateXform
public MAPL_LocStreamFracArea
public MAPL_GridCoordAdjust

! !PUBLIC TYPES:

type, public :: MAPL_LocStream
  private
  type(MAPL_LocStreamType), pointer :: Ptr=>null()
end type MAPL_LocStream

type, public :: MAPL_LocStreamXform
  private
  type(MAPL_LocStreamXformType), pointer :: Ptr=>null()
end type MAPL_LocStreamXform

!EOP

integer, parameter :: NumGlobalVars=4
integer, parameter :: NumLocalVars =4


type MAPL_GeoLocation
   integer                    :: T ! 1st Type designation
   real                       :: X ! Stream coordinate
   real                       :: Y ! Stream coordinate
end type MAPL_GeoLocation

type MAPL_IndexLocation
   integer                    :: I ! Global index into associated grid
   integer                    :: J ! Global index into associated grid
   real                       :: W ! Weight at I J
end type MAPL_IndexLocation

type MAPL_Tiling
   character(len=ESMF_MAXSTR)          :: NAME=""
   integer                             :: IM=0
   integer                             :: JM=0
   type(MAPL_IndexLocation),  pointer  :: Global_IndexLocation(:)=>null() ! Locations in local PE
end type MAPL_Tiling

type MAPL_LocStreamType
   character(len=ESMF_MAXSTR)         :: ROOTNAME=""
   character(len=ESMF_MAXSTR)         :: NAME=""
   integer                            :: NT_GLOBAL=0                     ! Total number locations
   integer                            :: NT_LOCAL=0                      ! Number locations on local PE
   integer                            :: N_GRIDS=0                       ! Number of associated grids
   integer                            :: Current_tiling=-1               ! Grid tiling currently attached 
   type(ESMF_GRID)                    :: GRID                            ! Grid currently attached
   type(ESMF_GRID), pointer           :: GRIDS(:)                        ! List of all grids
   type(ESMF_GRID)                    :: TILEGRID                        ! the next best thing to LocStream grid
   integer,                  pointer  :: GLOBAL_Id(:)           =>null() ! All Location Ids in file order
   integer,                  pointer  :: LOCAL_Id (:)           =>null() ! Location Ids on local PE
   type(MAPL_GeoLocation),   pointer  :: Global_GeoLocation  (:)=>null() ! All GeoLocations
   type(MAPL_IndexLocation), pointer  :: Global_IndexLocation(:)=>null() ! All IndexLocations for attach grid
   type(MAPL_GeoLocation),   pointer  :: Local_GeoLocation   (:)=>null() ! GeoLocations on local PE
   type(MAPL_IndexLocation), pointer  :: Local_IndexLocation (:)=>null() ! Local IndexLocations for attach grid
   type(MAPL_Tiling),        pointer  :: Tiling(:)              =>null() ! Grid associated tilings
   real, pointer              :: D(:,:,:)=>null() ! Bilinear weights
end type MAPL_LocStreamType

type MAPL_LocStreamXformType
   character(len=ESMF_MAXSTR)         :: NAME=""
   type(MAPL_LocStream)               :: InputStream
   type(MAPL_LocStream)               :: OutputStream
   integer                  ,pointer  :: IndexIn (:)=>null()
   integer                  ,pointer  :: IndexOut(:)=>null()
   integer                            :: Count
   integer                            :: LastLocal
   logical                            :: Local
end type MAPL_LocStreamXformType

! Overloads
!----------

interface MAPL_LocStreamCreate
   module procedure MAPL_LocStreamCreateFromFile
   module procedure MAPL_LocStreamCreateFromStream
end interface

interface MAPL_LocStreamTransform
   module procedure MAPL_LocStreamTransformField
   module procedure MAPL_LocStreamTransformT2G
   module procedure MAPL_LocStreamTransformG2T
   module procedure MAPL_LocStreamTransformT2T
end interface

contains

!===================================================================

  logical function MAPL_LocStreamIsAssociated(LocStream, RC)
    type(MAPL_LocStream),                 intent(IN   ) :: LocStream
    integer, optional,                    intent(  OUT) :: RC  
    
    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamIsAssocited'
    integer                    :: STATUS

    MAPL_LocStreamIsAssociated = associated(LocStream%Ptr)

    RETURN_(ESMF_SUCCESS)
  end function MAPL_LocStreamIsAssociated

!===================================================================

  logical function MAPL_LocStreamXformIsAssociated(Xform, RC)
    type(MAPL_LocStreamXform),            intent(IN   ) :: Xform
    integer, optional,                    intent(  OUT) :: RC  
    
    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamXformIsAssocited'
    integer                    :: STATUS

    MAPL_LocStreamXformIsAssociated = associated(Xform%Ptr)

    RETURN_(ESMF_SUCCESS)
  end function MAPL_LocStreamXformIsAssociated

!===================================================================


  subroutine MAPL_LocStreamGet(LocStream, NT_LOCAL, TILETYPE, TILEKIND, &
                               TILELONS, TILELATS, &
!                              TILEI, TILEJ, TILEGRID, &
                               TILEGRID, &
                               GRIDIM, GRIDJM, GRIDNAMES, &
                               GRIDS, RC)
    type(MAPL_LocStream),                 intent(IN   ) :: LocStream
    integer, optional,                    intent(  OUT) :: NT_LOCAL  
    integer, optional,                    pointer       :: TILETYPE(:)
    integer, optional,                    pointer       :: TILEKIND(:)
    real   , optional,                    pointer       :: TILELONS(:)
    real   , optional,                    pointer       :: TILELATS(:)
!    integer, optional,                    pointer       :: TILEI(:)
!    integer, optional,                    pointer       :: TILEJ(:)
    integer, optional,                    pointer       :: GRIDIM(:)
    integer, optional,                    pointer       :: GRIDJM(:)
    character(len=ESMF_MAXSTR), optional, pointer       :: GRIDNAMES(:)
    type(ESMF_Grid), optional,            intent(  OUT) :: TILEGRID
    type(ESMF_Grid), optional,            pointer       :: GRIDS(:)
    integer, optional,                    intent(  OUT) :: RC  
    
! Local variables

    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamGet'
    integer                    :: STATUS

    if (present(NT_LOCAL)) then
       NT_LOCAL = locstream%Ptr%NT_LOCAL
    end if

    if (present(tiletype)) then
       tiletype => locstream%Ptr%Local_GeoLocation(:)%t
    end if

    if (present(tilekind)) then
       PRINT *, 'IN LocStreamGet TILEKIND  NO LONGER VALID ARGUMENT'
       ASSERT_(.false.)
!       tilekind => locstream%Ptr%Local_GeoLocation(:)%u
    end if

    if (present(tilelons)) then
       tilelons => locstream%Ptr%Local_GeoLocation(:)%x
    end if

    if (present(tilelats)) then
       tilelats => locstream%Ptr%Local_GeoLocation(:)%y
    end if

    if (present(gridim)) then
       gridim => locstream%Ptr%tiling(:)%im
    end if

    if (present(gridjm)) then
       gridjm => locstream%Ptr%tiling(:)%jm
    end if

    if (present(gridnames)) then
       gridnames => locstream%Ptr%tiling(:)%name
    end if

    if (present(grids)) then
       grids => locstream%Ptr%grids
    end if

!!$    if (present(tilei)) then
!!$       tilei => locstream%Ptr%TILING(locstream%ptr%CURRENT_TILING)%Global_IndexLocation(:)%i
!!$    end if
!!$
!!$    if (present(tilej)) then
!!$       tilej => locstream%Ptr%TILING(locstream%ptr%CURRENT_TILING)%Global_IndexLocation(:)%j
!!$    end if

    if (present(tilegrid)) then
       tilegrid = locstream%Ptr%TILEGRID
    end if

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_LocStreamGet

!===================================================================


!BOP

! !IROUTINE:  GEOS\_LocStreamCreateFromFile

! !INTERFACE:

  subroutine MAPL_LocStreamCreateFromFile(LocStream, LAYOUT, FILENAME, NAME, MASK, GRID, GRIDS, RC)

! !ARGUMENTS:

    type(MAPL_LocStream),                 intent(  OUT) :: LocStream
    type(ESMF_DELayout),                  intent(IN   ) :: LAYOUT
    character(len=*),                     intent(IN   ) :: FILENAME
    character(len=*),                     intent(IN   ) :: NAME
    integer,                    optional, intent(IN   ) :: MASK(:)
    type(ESMF_Grid), optional,            intent(INout) :: GRID
    type(ESMF_Grid), optional,            pointer       :: GRIDS(:)
    integer,                    optional, intent(  OUT) :: RC  

! !DESCRIPTION: Creates a location stream from a file. This does
! not decompose the location stream; so the global stream is
! described in each processor.  The stream can be decomposed
! later in various ways. Currently we only decompose it by 
! "attaching" it to a decomposed grid.

!EOP

! Local variables

    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamCreateFromFile'
    integer                    :: STATUS

    integer                           :: UNIT
    integer                           :: N, I, K, NT
    type(MAPL_LocStreamType), pointer :: STREAM
    real,    pointer                  :: AVR(:,:)
    logical, pointer                  :: MSK(:)
    real                              :: X, Y

    logical                           :: found
    character(len=ESMF_MAXSTR)        :: gname

    integer                           :: irec
    integer(kind=1)                   :: byte(4)
    logical                           :: isascii
    logical                           :: read_always

! Begin
!------

! Allocate the Location Stream
!-----------------------------

    LocStream%Ptr => null()
    allocate(LocStream%Ptr, STAT=STATUS)
    VERIFY_(STATUS)

    STREAM => LocStream%Ptr

! Use the filename as identifier. NAME is thus the 
! same for all streams made from this file
!-------------------------------------------------

    STREAM%NAME     = NAME
    STREAM%ROOTNAME = FILENAME

! Use some heuristics to determine filetype (choices are BINARY and ASCII)
!------------------------------------------------------------------------
    ! just get the unit
    UNIT = GETFILE(FILENAME, DO_OPEN=0, ALL_PES=.true., RC=STATUS)
    VERIFY_(STATUS)

    INQUIRE(IOLENGTH=IREC) BYTE
    open (UNIT=UNIT, FILE=FILENAME, FORM='unformatted', ACCESS='DIRECT', RECL=IREC, IOSTAT=status)
    VERIFY_(STATUS)
    read (UNIT, REC=1, ERR=100) BYTE
    call FREE_FILE(UNIT)

    isascii = .true.
    do i=1,size(byte)
       if (BYTE(I) < 7) then
          isascii = .false.
          exit
       end if
    end do

    if (isascii) then
! Open file and read header info
!-------------------------------

       UNIT = GETFILE(FILENAME, form='FORMATTED', RC=status)
       VERIFY_(STATUS)

! Total number of tiles in exchange grid
!---------------------------------------

       call READ_PARALLEL(layout, NT, UNIT=UNIT, rc=status)
       VERIFY_(STATUS)

! Number of grids that can be attached
!-------------------------------------

       call READ_PARALLEL(layout, STREAM%N_GRIDS, unit=UNIT, rc=status)
       VERIFY_(STATUS)

! The exchange grid is used to tile each attached grid
!-----------------------------------------------------

       allocate(STREAM%TILING(STREAM%N_GRIDS), STAT=STATUS)
       VERIFY_(STATUS)

! The names and sizes of the grids to be tiled
!---------------------------------------------

       do N=1,STREAM%N_GRIDS
          call READ_PARALLEL(layout, STREAM%TILING(N)%NAME, unit=UNIT, rc=status)
          VERIFY_(STATUS)
          call READ_PARALLEL(layout, STREAM%TILING(N)%IM, unit=UNIT, rc=status)
          VERIFY_(STATUS)
          call READ_PARALLEL(layout, STREAM%TILING(N)%JM, unit=UNIT, rc=status)
          VERIFY_(STATUS)
       enddo


! Read location stream file into AVR
!---------------------------------------

       allocate(AVR(NT,NumGlobalVars+NumLocalVars*STREAM%N_GRIDS), STAT=STATUS)
       VERIFY_(STATUS)

       do I=1, NT
          call READ_PARALLEL(layout, AVR(I,:), unit=UNIT, rc=status)
          VERIFY_(STATUS)
       end do

       call FREE_FILE(UNIT)

! Allocate msk for which tiles to include in the stream being created.
!--------------------------------------------------------------------

       allocate(MSK(NT), STAT=STATUS)
       VERIFY_(STATUS)

! We include any tile whose type matches any element of the mask
!---------------------------------------------------------------

       if(present(MASK)) then
          do N=1,NT
             MSK(N) = any(nint(AVR(N,1))==MASK(:))
          end do
       else
          MSK = .true.
       end if

! The number of tiles in the new stream
!--------------------------------------

       STREAM%NT_GLOBAL = count(MSK)

! Allocate space for global versions of stream parameters
!--------------------------------------------------------

       allocate(STREAM%GLOBAL_ID         (STREAM%NT_GLOBAL), STAT=STATUS)
       VERIFY_(STATUS)
       allocate(STREAM%GLOBAL_GEOLOCATION(STREAM%NT_GLOBAL), STAT=STATUS)
       VERIFY_(STATUS)

       do N=1,STREAM%N_GRIDS
          allocate(STREAM%TILING(N)%GLOBAL_IndexLocation(STREAM%NT_GLOBAL), STAT=STATUS)
          VERIFY_(STATUS)
       end do

! Fill global stream parameters subject to mask
!----------------------------------------------

       K = 0
       do I=1, NT
          if(MSK(I)) then
             K = K + 1
             STREAM%GLOBAL_ID         (K)   = I
             STREAM%GLOBAL_GeoLocation(K)%T = nint(AVR(I,1))
             STREAM%GLOBAL_GeoLocation(K)%X =      AVR(I,3) * (MAPL_PI/180.)
             STREAM%GLOBAL_GeoLocation(K)%Y =      AVR(I,4) * (MAPL_PI/180.)
             X = AVR(I,3)
             Y = AVR(I,4)
             do N=1,STREAM%N_GRIDS
                STREAM%Tiling(N)%GLOBAL_IndexLocation(K)%I = nint(AVR(I,NumGlobalVars+1+NumLocalVars*(N-1)))
                STREAM%Tiling(N)%GLOBAL_IndexLocation(K)%J = nint(AVR(I,NumGlobalVars+2+NumLocalVars*(N-1)))
                STREAM%Tiling(N)%GLOBAL_IndexLocation(K)%W =      AVR(I,NumGlobalVars+3+NumLocalVars*(N-1))
             end do
          end if
       end do

       deallocate(MSK)
       deallocate(AVR)
    else
       ! BINARY
! Open file and read header info
!-------------------------------

       UNIT = GETFILE(FILENAME, ALL_PES=.true., form='UNFORMATTED', RC=status)
       VERIFY_(STATUS)

! Total number of tiles in exchange grid
!---------------------------------------
       read(UNIT) NT

! Number of grids that can be attached
!-------------------------------------

       read(UNIT) STREAM%N_GRIDS

! The exchange grid is used to tile each attached grid
!-----------------------------------------------------

       allocate(STREAM%TILING(STREAM%N_GRIDS), STAT=STATUS)
       VERIFY_(STATUS)

! The names and sizes of the grids to be tiled
!---------------------------------------------

       do N=1,STREAM%N_GRIDS
          read(UNIT) STREAM%TILING(N)%NAME
          read(UNIT) STREAM%TILING(N)%IM
          read(UNIT) STREAM%TILING(N)%JM
       enddo

! Read location stream file into AVR
!---------------------------------------

       allocate(AVR(NT,3), STAT=STATUS)
       VERIFY_(STATUS)

       read(UNIT) AVR

! Allocate msk for which tiles to include in the stream being created.
!--------------------------------------------------------------------

       allocate(MSK(NT), STAT=STATUS)
       VERIFY_(STATUS)

! We include any tile whose type matches any element of the mask
!---------------------------------------------------------------

       if(present(MASK)) then
          do N=1,NT
             MSK(N) = any(nint(AVR(N,1))==MASK(:))
          end do
       else
          MSK = .true.
       end if

! The number of tiles in the new stream
!--------------------------------------

       STREAM%NT_GLOBAL = count(MSK)

! Allocate space for global versions of stream parameters
!--------------------------------------------------------

       allocate(STREAM%GLOBAL_ID         (STREAM%NT_GLOBAL), STAT=STATUS)
       VERIFY_(STATUS)
       allocate(STREAM%GLOBAL_GEOLOCATION(STREAM%NT_GLOBAL), STAT=STATUS)
       VERIFY_(STATUS)

       do N=1,STREAM%N_GRIDS
          allocate(STREAM%TILING(N)%GLOBAL_IndexLocation(STREAM%NT_GLOBAL), STAT=STATUS)
          VERIFY_(STATUS)
       end do

! Fill global stream parameters subject to mask
!----------------------------------------------

       K = 0
       do I=1, NT
          if(MSK(I)) then
             K = K + 1
             STREAM%GLOBAL_ID         (K)   = I
             STREAM%GLOBAL_GeoLocation(K)%T = nint(AVR(I,1))
             STREAM%GLOBAL_GeoLocation(K)%X =      AVR(I,2) * (MAPL_PI/180.)
             STREAM%GLOBAL_GeoLocation(K)%Y =      AVR(I,3) * (MAPL_PI/180.)
          end if
       end do

       if (present(GRID)) then
          call ESMF_GridGet(grid, name=gname, rc=status)
          VERIFY_(STATUS)
          read_always = .false.
       else
          gname = ""
          read_always = .true.
       end if

       do N=1,STREAM%N_GRIDS
          if (read_always .or. gname == STREAM%TILING(N)%NAME) then
             read(UNIT) AVR
          else
             read(UNIT)
             cycle
          endif

          K = 0
          do I=1, NT
             if(MSK(I)) then
                K = K + 1
                STREAM%Tiling(N)%GLOBAL_IndexLocation(K)%I = nint(AVR(I,1))
                STREAM%Tiling(N)%GLOBAL_IndexLocation(K)%J = nint(AVR(I,2))
                STREAM%Tiling(N)%GLOBAL_IndexLocation(K)%W =      AVR(I,3)
             end if
          end do
       end do
       call FREE_FILE(UNIT)

       deallocate(MSK)
       deallocate(AVR)
    end if

    if (present(GRID)) then
       call MAPL_LocStreamAttachGrid(LocStream, GRID, NSUBTILES=1, RC=STATUS)
       VERIFY_(STATUS)
    end if

    if (present(GRIDS)) then
       ASSERT_(STREAM%N_GRIDS == SIZE(GRIDS))
       allocate(STREAM%GRIDS(STREAM%N_GRIDS), stat=status)
       VERIFY_(STATUS)

       DO I=1,STREAM%N_GRIDS
          call ESMF_GridGet(grids(I), name=gname, rc=status)
          VERIFY_(STATUS)
          found = .false.
          do N=1,STREAM%N_GRIDS
             if (gname == STREAM%TILING(N)%NAME) then
                found = .true.
                exit
             end if
          end do
          ASSERT_(found)
          STREAM%GRIDS(N) = grids(I)
       END DO

    end if

    RETURN_(ESMF_SUCCESS)

100 RETURN_(ESMF_FAILURE)
  end subroutine MAPL_LocStreamCreateFromFile



!BOP

! !IROUTINE:  GEOS\_LocStreamCreateFromStream

! !INTERFACE:

  subroutine MAPL_LocStreamCreateFromStream(LocStreamOut, LocStreamIn, NAME, MASK, RC)

! !ARGUMENTS:

    type(MAPL_LocStream),                 intent(  OUT) :: LocStreamOut
    type(MAPL_LocStream),                 intent(IN   ) :: LocStreamIn
    character(len=*),                     intent(IN   ) :: NAME
    integer,                    optional, intent(IN   ) :: MASK(:)
    integer,                    optional, intent(  OUT) :: RC  

! !DESCRIPTION: Creates a location stream as a subset of another
!   according to mask.

!EOP

! Local variables

    character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamCreateFromStream'
    integer                    :: STATUS

    integer                           :: N, I, K, NT
    type(MAPL_LocStreamType), pointer :: STREAMOUT
    type(MAPL_LocStreamType), pointer :: STREAMIN
    integer                           :: NT_LOCAL(1)
    logical, pointer                  :: MSK(:)
    type(ESMF_VM)                     :: VM
    
! Begin
!------

    ASSERT_(     associated(LocStreamIn %Ptr))

! Allocate the Location Stream
!-----------------------------

    LocStreamOut%Ptr => null()
    allocate(LocStreamOut%Ptr, STAT=STATUS)
    VERIFY_(STATUS)

    STREAMOUT => LocStreamOut%Ptr
    STREAMIN  => LocStreamIn %Ptr

! Make sure that the input stream is attached
!--------------------------------------------
    ASSERT_(STREAMIN%CURRENT_TILING > 0)

! New stream has the same identifier as old
!------------------------------------------

    STREAMOUT%NAME       = NAME
    STREAMOUT%ROOTNAME   = STREAMIN%ROOTNAME
    STREAMOUT%N_GRIDS    = STREAMIN%N_GRIDS

    if (associated(STREAMIN%GRIDS)) then
       allocate(STREAMOUT%GRIDS(size(STREAMIN%GRIDS)), stat=status)
       VERIFY_(STATUS)
       STREAMOUT%GRIDS = STREAMIN%GRIDS
    end if

! Allocate the allowed tilings and copy the names and sizes of the grids
!-----------------------------------------------------------------------

    allocate(STREAMOUT%TILING(STREAMOUT%N_GRIDS), STAT=STATUS)
    VERIFY_(STATUS)

    STREAMOUT%Tiling     = STREAMIN%Tiling           

! Local number of tiles in input stream
!--------------------------------------

    NT = STREAMIN%NT_LOCAL

! Allocate msk for which tiles to include in the stream being created.
!--------------------------------------------------------------------

    allocate(MSK(NT), STAT=STATUS)
    VERIFY_(STATUS)

! We include any tile whose type matches any element of the mask
!---------------------------------------------------------------

    if(present(MASK)) then
       do N=1,NT
          MSK(N) = any(STREAMIN%Local_GeoLocation(N)%T==MASK)
       end do
    else
       MSK = .true.
    end if

! The number of tiles in the new stream
!--------------------------------------

    STREAMOUT%NT_LOCAL = count(MSK)

    NT_LOCAL(1) = STREAMOUT%NT_LOCAL
    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(STATUS)
    call ESMF_VMAllFullReduce(vm, sendData=nt_local, recvData=STREAMOUT%NT_GLOBAL, &
         count=1, reduceflag=ESMF_SUM, rc=STATUS)
    VERIFY_(STATUS)

! Allocate space for local versions of stream parameters
!--------------------------------------------------------

    allocate(STREAMOUT%LOCAL_ID         (STREAMOUT%NT_LOCAL), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(STREAMOUT%LOCAL_GEOLOCATION(STREAMOUT%NT_LOCAL), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(STREAMOUT%LOCAL_INDEXLOCATION(STREAMOUT%NT_LOCAL), STAT=STATUS)
    VERIFY_(STATUS)
    allocate(STREAMOUT%D(-1:1,-1:1,STREAMOUT%NT_LOCAL), STAT=STATUS)
    VERIFY_(STATUS)

! Fill local stream parameters subject to mask
!----------------------------------------------

    K = 0
    do I=1, NT
       if(MSK(I)) then
          K = K + 1
          STREAMOUT%LOCAL_ID         (K)   = STREAMIN%LOCAL_ID         (I)  
          STREAMOUT%LOCAL_GeoLocation(K)%T = STREAMIN%LOCAL_GeoLocation(I)%T
          STREAMOUT%LOCAL_GeoLocation(K)%X = STREAMIN%LOCAL_GeoLocation(I)%X
          STREAMOUT%LOCAL_GeoLocation(K)%Y = STREAMIN%LOCAL_GeoLocation(I)%Y

          STREAMOUT%LOCAL_IndexLocation(K)%I = STREAMIN%LOCAL_IndexLocation(I)%I
          STREAMOUT%LOCAL_IndexLocation(K)%J = STREAMIN%LOCAL_IndexLocation(I)%J
          STREAMOUT%LOCAL_IndexLocation(K)%W = STREAMIN%LOCAL_IndexLocation(I)%W

          STREAMOUT%D(:,:,K) = STREAMIN%D(:,:,I)
       end if
    end do

    deallocate(MSK)

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_LocStreamCreateFromStream


!======================================================

subroutine MAPL_LocStreamAttachGrid(LocStream, GRID, NSUBTILES, RC)

  type(MAPL_LocStream),  intent(INOUT) :: LocStream
  type(ESMF_Grid),       intent(INout) :: Grid
  integer, optional,     intent(IN   ) :: NSUBTILES
  integer, optional,     intent(  OUT) :: RC  

! Local variables

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamAttachGrid'
  integer                    :: STATUS

  integer                           :: NG
  type(MAPL_LocStreamType), pointer :: STREAM
  type(MAPL_Tiling       ), pointer :: TILING
  integer                           :: IM_WORLD, JM_WORLD
  type (ESMF_DistGrid)              :: distgrid
  logical, pointer                  :: ISMINE(:)
  integer                           :: I1, IN, J1, JN
  integer                           :: gridRank
  integer                           :: DIMS(ESMF_MAXGRIDDIM)

  integer, allocatable          :: AL(:,:), AU(:,:)
  type (ESMF_Grid)              :: TILEGRID
  type (ESMF_VM)                :: VM
  real(ESMF_KIND_R8), parameter :: dum=1.0
  integer                       :: nts(1)
  integer, allocatable          :: tileIndex(:)
  integer, pointer              :: mymask(:)
  integer, pointer              :: tile_i(:), tile_j(:)
  integer                       :: ndes
  integer                       :: I, J, N
  integer                       :: myid
  real                          :: X, Y
  character(len=ESMF_MAXSTR)    :: GNAME

! Begin
!------

! Make sure Location stream has been created
!-------------------------------------------

  ASSERT_(associated(LocStream%Ptr))

! Alias to the pointer
!---------------------

  STREAM => LocStream%Ptr

! Location stream must have some allowed grids
!---------------------------------------------

  ASSERT_(STREAM%N_GRIDS>0)

! Get the attached grid's info
!-----------------------------
  
  call ESMF_GridGet(GRID, NAME=GNAME, RC=STATUS)
  VERIFY_(STATUS)

! Check if the location stream is already attached
!-------------------------------------------------

  if (stream%current_tiling > 0) then
     if (gname == STREAM%TILING(STREAM%current_tiling)%NAME) then

        if (present(NSUBTILES)) then
           call ESMF_AttributeSet(stream%tilegrid, name='GRID_EXTRADIM', value=NSUBTILES, rc=status)
           VERIFY_(STATUS)
        end if
        RETURN_(ESMF_SUCCESS)
     end if
  end if
          
! Find the given grid among the allowed grids
!--------------------------------------------

  STREAM%CURRENT_TILING = GRIDINDEX(STREAM, GRID, RC=STATUS)
  VERIFY_(STATUS)

  TILING => STREAM%TILING(STREAM%CURRENT_TILING)

! Check if local data has already been filled
! this if-block should be bypassed if the stream was created from another stream

  if (.not. associated(STREAM%LOCAL_ID)) then
     STREAM%GLOBAL_INDEXLOCATION => TILING%GLOBAL_INDEXLOCATION

! Put associated ESMF_LogRectGrid in stream and query grid info
!--------------------------------------------------------------

     STREAM%GRID = GRID

! Verify that the grid is the right size
!---------------------------------------

     call ESMF_GridGet(GRID, dimCount=gridRank, rc=STATUS)
     VERIFY_(STATUS)
     call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
     VERIFY_(STATUS)
     
     IM_WORLD = DIMS(1)
     JM_WORLD = DIMS(2)
     
     ASSERT_(IM_WORLD==TILING%IM)
     ASSERT_(JM_WORLD==TILING%JM)

! Find out which tiles are in local PE
!-------------------------------------

     call ESMF_GRID_INTERIOR  (GRID, I1,IN,J1,JN)
     
     allocate(ISMINE(STREAM%NT_GLOBAL), STAT=STATUS)
     VERIFY_(STATUS)
     
     ISMINE = ( I1<=TILING%GLOBAL_IndexLocation(:)%I .and. &
                IN>=TILING%GLOBAL_IndexLocation(:)%I .and. &
                J1<=TILING%GLOBAL_IndexLocation(:)%J .and. &
                JN>=TILING%GLOBAL_IndexLocation(:)%J       )

! Count locations in the local PE 
!--------------------------------

     STREAM%NT_LOCAL = count(ISMINE)

! Allocate local arrays 
!----------------------

     allocate(STREAM%LOCAL_IndexLocation(STREAM%NT_LOCAL), STAT=STATUS)
     VERIFY_(STATUS)
     allocate(STREAM%LOCAL_GeoLocation  (STREAM%NT_LOCAL), STAT=STATUS)
     VERIFY_(STATUS)
     allocate(STREAM%LOCAL_ID           (STREAM%NT_LOCAL), STAT=STATUS)
     VERIFY_(STATUS)
     allocate(STREAM%D           (-1:1,-1:1,STREAM%NT_LOCAL), STAT=STATUS)
     VERIFY_(STATUS)

! Pick-off local parts of global arrays 
!--------------------------------------

     STREAM%LOCAL_IndexLocation = pack(STREAM%GLOBAL_IndexLocation,ISMINE)
     STREAM%LOCAL_GeoLocation   = pack(STREAM%GLOBAL_GeoLocation,  ISMINE)
     STREAM%LOCAL_ID            = pack(STREAM%GLOBAL_ID,           ISMINE)

     deallocate(ISMINE)

! Local location uses local indexing
!-----------------------------------

     STREAM%LOCAL_IndexLocation(:)%I = STREAM%LOCAL_IndexLocation(:)%I-I1+1
     STREAM%LOCAL_IndexLocation(:)%J = STREAM%LOCAL_IndexLocation(:)%J-J1+1

     do i = 1, size(STREAM%LOCAL_IndexLocation)
        X = STREAM%LOCAL_GeoLocation(I)%X*(180./MAPL_PI)
        Y = STREAM%LOCAL_GeoLocation(I)%Y*(180./MAPL_PI)
        call GetBilinearCoeffs(Tiling, STREAM%LOCAL_IndexLocation(I), &
             Stream%D(:,:,I), X, Y, RC=STATUS)
     end do

! Clean up all global data and unused tiling
!-------------------------------------------
     deallocate(STREAM%GLOBAL_INDEXLOCATION)
     nullify(STREAM%GLOBAL_INDEXLOCATION)
     deallocate(STREAM%GLOBAL_GEOLOCATION)
     deallocate(Stream%Global_id)
     nullify(Stream%Global_id)
     nullify(STREAM%GLOBAL_GEOLOCATION)
     nullify(TILING%GLOBAL_INDEXLOCATION)

     do n=1, size(stream%tiling)
        if (n == stream%current_tiling) cycle
        deallocate(stream%tiling(n)%global_indexlocation)
        nullify(stream%tiling(n)%global_indexlocation)
     end do

  end if

  if (present(NSUBTILES)) then
! Create TILEGRID
!----------------
     distgrid = ESMF_DistGridCreate( &
          arbSeqIndexList=STREAM%LOCAL_ID, rc=status)
     VERIFY_(STATUS)

     TILEGRID = ESMF_GridCreate(    &
       name="tile_grid_"//trim(Stream%NAME)//'@'//trim(GNAME),    &
       distgrid = distgrid, &
       gridEdgeLWidth = (/0/),       &
       gridEdgeUWidth = (/0/),       &
       rc=status)
     VERIFY_(STATUS)

     STREAM%TILEGRID = TILEGRID

     call ESMF_AttributeSet(tilegrid, name='GRID_EXTRADIM', value=NSUBTILES, rc=status)
     VERIFY_(STATUS)

  end if

  RETURN_(ESMF_SUCCESS)

  contains

    subroutine GetBilinearCoeffs(Tiling,Local_IndexLoc,D,X,Y,RC)
      type(MAPL_Tiling), intent(INOUT)  :: Tiling
      type(MAPL_IndexLocation), intent(INOUT) :: Local_IndexLoc
      real,              intent(IN   )  :: X, Y
      real,              intent(  OUT)  :: D(-1:,-1:)
      integer, optional, intent(  OUT)  :: RC

      character(len=ESMF_MAXSTR) :: IAm='GetBilinearCoeffs'
      integer                    :: STATUS
      real                       :: DX, DY, DX0, DY0
      real                       :: X00, Y00, X0, Y0
      integer                    :: I

      DX = 360./float(tiling%IM)

      I  = index(TILING%NAME,'-',.true.)
      ASSERT_(I>0)
      I  = I+1

      if(TILING%NAME(I:I+1)=='DC') then
         X0 = -180.
      else
         X0 = -180. + DX*0.5
      end if

      if(TILING%NAME(1:2)=='PE') then
         DY = 180./float(tiling%JM  )
         Y0 = -90.  + DY*0.5
      else
         DY = 180./float(tiling%JM-1)
         Y0 = -90.
      end if

      X00     = X0 + (Local_IndexLoc%I  -1)*DX
      Y00     = Y0 + (Local_IndexLoc%J  -1)*DY

      DX0     = (X - X00)/DX
      DY0     = (Y - Y00)/DY

      D = 0.0

      if    (DX0 >= 0.0 .and. DY0 >= 0.0) then
        D( 1, 0) = DX0*(1.0-DY0) 
        D( 0, 1) = DY0*(1.0-DX0) 
        D( 0, 0) = (1.0-DX0)*(1.0-DY0) 
        D( 1, 1) = DX0*DY0
      elseif(DX0 >= 0.0 .and. DY0 <= 0.0) then
        DY0 = -DY0
        D( 1, 0) = DX0*(1.0-DY0) 
        D( 0,-1) = DY0*(1.0-DX0) 
        D( 0, 0) = (1.0-DX0)*(1.0-DY0) 
        D( 1,-1) = DX0*DY0 
      elseif(DX0 <= 0.0 .and. DY0 >= 0.0) then
        DX0 = -DX0
        D(-1, 0) = DX0*(1.0-DY0) 
        D( 0, 1) = DY0*(1.0-DX0) 
        D( 0, 0) = (1.0-DX0)*(1.0-DY0) 
        D(-1, 1) = DX0*DY0 
      else
        DX0 = -DX0
        DY0 = -DY0
        D(-1, 0) = DX0*(1.0-DY0) 
        D( 0,-1) = DY0*(1.0-DX0) 
        D( 0, 0) = (1.0-DX0)*(1.0-DY0) 
        D(-1,-1) = DX0*DY0 
      end if

      RETURN_(ESMF_SUCCESS)
   end subroutine GetBilinearCoeffs


end subroutine MAPL_LocStreamAttachGrid

!======================================================


subroutine MAPL_LocStreamTransformField (LocStream, OUTPUT, INPUT, MASK, GRID_ID, GLOBAL, ISMINE, INTERP, RC )

  type(ESMF_Field),          intent(OUT) :: OUTPUT
  type(ESMF_Field),          intent(INout) :: INPUT
  type(MAPL_LocStream),      intent(IN ) :: LocStream
  integer, optional,         intent(IN ) :: MASK(:)
  logical, optional,         intent(IN ) :: ISMINE(:), INTERP
  logical, optional,         intent(IN ) :: GLOBAL
  integer, optional,         intent(IN ) :: GRID_ID
  integer, optional,         intent(OUT) :: RC  

! Local variables

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamTransform'
  integer                    :: STATUS

  integer                    :: N, NT
  integer                    :: OutRank
  integer                    :: InRank
  type(ESMF_GRID)            :: INGRID
  type(ESMF_GRID)            :: OUTGRID
  type(ESMF_ARRAY)           :: INARRAY
  type(ESMF_ARRAY)           :: OUTARRAY
  real, pointer              :: TILEVAR(:)
  real, pointer              :: GRIDVAR(:,:)
  logical, pointer           :: MSK(:)

! Begin

  NT = LocStream%Ptr%NT_LOCAL

  allocate(MSK(NT),STAT=STATUS)
  VERIFY_(STATUS)

  if(present(MASK)) then
     do N = 1, NT
        MSK(N) = any(LocSTREAM%Ptr%LOCAL_GEOLOCATION(N)%T==MASK)
     end do
  else
     MSK = .true.
  end if

  call ESMF_FieldGet     (OUTPUT,   GRID=OUTGRID, RC=STATUS)
  VERIFY_(STATUS)
  call ESMF_FieldGet(OUTPUT, Array=OUTARRAY,     RC=STATUS)
  VERIFY_(STATUS)
  call ESMF_ArrayGet     (OUTARRAY, RANK=OUTRANK, RC=STATUS)
  VERIFY_(STATUS)

  call ESMF_FieldGet     (INPUT,    GRID=INGRID,  RC=STATUS)
  VERIFY_(STATUS)
  call ESMF_FieldGet(INPUT, Array=INARRAY,      RC=STATUS)
  VERIFY_(STATUS)
  call ESMF_ArrayGet     (INARRAY,  RANK=INRANK,  RC=STATUS)
  VERIFY_(STATUS)

  if    ( INRANK==1 .and. OUTRANK==2) then ! T2G
     call ESMF_ArrayGet(OUTARRAY, localDE=0, farrayptr=GRIDVAR, RC=STATUS)
     VERIFY_(STATUS)
     call ESMF_ArrayGet( INARRAY, localDE=0, farrayptr=TILEVAR, RC=STATUS)
     VERIFY_(STATUS)

     ASSERT_(size(TILEVAR)==NT)

     call MAPL_LocStreamTransformT2G (LOCSTREAM, GRIDVAR, TILEVAR, MASK=MSK, RC=STATUS)
     VERIFY_(STATUS)

   elseif( OUTRANK==1 .and. INRANK==2) then ! G2T
     call ESMF_ArrayGet(OUTARRAY, localDE=0, farrayptr=TILEVAR, RC=STATUS)
     VERIFY_(STATUS)
     call ESMF_ArrayGet( INARRAY, localDE=0, farrayptr=GRIDVAR, RC=STATUS)
     VERIFY_(STATUS)

     ASSERT_(size(TILEVAR)==NT)

     call MAPL_LocStreamTransformG2T(LOCSTREAM, TILEVAR, GRIDVAR, MSK, &
          GRID_ID, GLOBAL, ISMINE, INTERP, RC=STATUS)
     VERIFY_(STATUS)

  else
     RETURN_(ESMF_FAILURE)
  end if

  deallocate(MSK)

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformField

subroutine MAPL_LocStreamFracArea (LocStream, TYPE, AREA, RC )
  type(MAPL_LocStream),      intent(IN ) :: LocStream
  integer,                   intent(IN ) :: TYPE
  real,                      intent(OUT) :: AREA(:,:)
  integer, optional,         intent(OUT) :: RC  

! Local variables

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamFracArea'
  integer                    :: STATUS

  integer                    :: II, JJ, N

! Make sure Location stream has been created...
!----------------------------------------------

  ASSERT_(associated(LocStream%Ptr))

! and a grid attached...
!-----------------------

  ASSERT_(LocStream%Ptr%Current_Tiling > 0)

! Compute area over masked locations
!-----------------------------------------------

  AREA   = 0.0
     
  do N = 1, size(LOCSTREAM%Ptr%LOCAL_INDEXLOCATION)
     if(LOCSTREAM%Ptr%LOCAL_GEOLOCATION(N)%T == TYPE) then
        II = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I
        JJ = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J 
        AREA  (II,JJ) = AREA  (II,JJ) + LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W
     end if
  end do

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamFracArea


subroutine MAPL_LocStreamTransformT2G (LocStream, OUTPUT, INPUT,  MASK, SAMPLE, RC )
  type(MAPL_LocStream),      intent(IN ) :: LocStream
  real,                      intent(OUT) :: OUTPUT(:,:)
  real,                      intent(IN ) :: INPUT(:)
  logical, optional,         intent(IN ) :: MASK(:) 
  logical, optional,         intent(IN ) :: SAMPLE
  integer, optional,         intent(OUT) :: RC  

! Local variables

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamTransformT2G'
  integer                    :: STATUS
  real, pointer              :: FF(:,:)
  integer                    :: II, JJ, N, I1, IN, J1, JN
  logical,      allocatable  :: usableMASK(:) 
  logical                    :: uSAMPLE 

! Make sure Location stream has been created...
!----------------------------------------------

  ASSERT_(associated(LocStream%Ptr))

! and a grid attached...
!-----------------------

  ASSERT_(LocStream%Ptr%Current_Tiling > 0)

! that's the size of the output array
!------------------------------------

  call ESMF_GRID_INTERIOR  (LocStream%Ptr%GRID, I1,IN,J1,JN)

  ASSERT_(IN-I1+1==size(OUTPUT,1))
  ASSERT_(JN-J1+1==size(OUTPUT,2))

! Allocate space for mask and cumulative weight at each grid point
!------------------------------------------------------------

  allocate(FF(size(OUTPUT,1),size(OUTPUT,2)), stat=STATUS)
  VERIFY_(STATUS)
  allocate(usableMASK(size(INPUT)), STAT=STATUS)
  VERIFY_(STATUS)

! Make usable mask from optional argument
!----------------------------------------
  
  if (present(MASK)) then
     usableMASK = MASK
  else
     usableMASK = .TRUE.
  end if

  if (present(SAMPLE)) then
     uSAMPLE = SAMPLE
  else
     uSAMPLE = .false.
  end if

! Compute weighted average over masked locations
!-----------------------------------------------

  OUTPUT = 0.0
  FF     = 0.0
     
  do N = 1, size(INPUT)
     if(usableMASK(N) .and. INPUT(N)/=MAPL_UNDEF) then
        II = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I
        JJ = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J 
        if(uSample) then
           if( LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W > FF(II,JJ)) then
              OUTPUT(II,JJ) = INPUT(N)
              FF    (II,JJ) = LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W
           end if
        else
           OUTPUT(II,JJ) = OUTPUT(II,JJ) + LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W * INPUT(N)
           FF    (II,JJ) = FF    (II,JJ) + LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%W
        endif
     end if
  end do

  if(.not.uSample) then
     where(FF>0)
        OUTPUT = OUTPUT / FF
     end where
  endif

  where(FF<=0)
     OUTPUT = MAPL_Undef
  end where

  deallocate(usableMASK)
  deallocate(FF)

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformT2G


subroutine MAPL_LocStreamTransformG2T ( LocStream, OUTPUT, INPUT, MASK, GRID_ID, GLOBAL, ISMINE, INTERP, RC )
  type(MAPL_LocStream),      intent(IN ) :: LocStream
  real,                      intent(OUT) :: OUTPUT(:)
  real,                      intent(IN ) :: INPUT(:,:)
  logical, optional,         intent(IN ) :: MASK(:), ISMINE(:), INTERP
  logical, optional,         intent(IN ) :: GLOBAL
  integer, optional,         intent(IN ) :: GRID_ID
  integer, optional,         intent(OUT) :: RC  

! Local variables

  character(len=ESMF_MAXSTR) :: IAm='MAPL_LocStreamTransformG2T'
  integer                    :: STATUS

  integer                    :: N, I1, IN, J1, JN, I, J, IM, JM
  logical,      allocatable  :: usableMASK(:)
 
  logical                    :: usableATTACHED
  logical                    :: usableGLOBAL
  logical                    :: usableINTERP
  real, pointer              :: ghostedINPUT(:,:)
  real                       :: D(-1:1,-1:1)

  integer, parameter :: HALOWIDTH = 1

  IM = size(INPUT,1)
  JM = size(INPUT,2)

  if(present(INTERP)) then
     usableINTERP = INTERP
  else
     usableINTERP = .false.
  end if

  if(present(GRID_ID)) then
     usableATTACHED = .false.
  else
     usableATTACHED = .true.
  end if

  if(present(GLOBAL)) then
     usableGLOBAL = GLOBAL
  else
     usableGLOBAL = .false.
  end if

! Make sure Location stream has been created...
!----------------------------------------------

  ASSERT_(associated(LocStream%Ptr))

! and a grid attached...
!-----------------------

  if (usableATTACHED) then
     ASSERT_(LocStream%Ptr%Current_Tiling > 0)

! that's the size of the output array
!------------------------------------

     call ESMF_GRID_INTERIOR  (LocStream%Ptr%GRID, I1,IN,J1,JN)

     ASSERT_(IN-I1+1==IM)
     ASSERT_(JN-J1+1==JM)
  else
     ASSERT_(GRID_ID <= LocStream%Ptr%N_GRIDS)
  endif

! Make usable mask from optional argument
!----------------------------------------

  allocate(usableMASK(size(OUTPUT)), STAT=STATUS)
  VERIFY_(STATUS)
  
  if (present(MASK)) then
     usableMASK = MASK
  else
     usableMASK = .TRUE.
  end if

  if(usableINTERP) then
     allocate(ghostedINPUT(1-HALOWIDTH:IM+HALOWIDTH,1-HALOWIDTH:JM+HALOWIDTH),STAT=STATUS)
     VERIFY_(STATUS)
     ghostedINPUT(1:IM,1:JM) = INPUT
     call ESMFL_HALO(LocStream%Ptr%GRID, ghostedINPUT, rc=status)
  end if

! Fill output subject to mask
!----------------------------

  if (usableGLOBAL) then
     PRINT *, 'IN G2T GLOBAL NO LONGER VALID ARGUMENT'
     ASSERT_(.FALSE.)
  else
     do N = 1, size(OUTPUT)
        if(usableMASK(N)) then
           if(usableINTERP) then
              OUTPUT(N) = 0.0
              D = LOCSTREAM%Ptr%D(:,:,N)
              do J=-1,1
                 do I=-1,1
                    OUTPUT(N) = OUTPUT(N) + (  GHOSTEDINPUT(LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I+I,   &
                                                            LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J+J)   &
                                             * D(I,J)   )        
                 end do
              end do
           else
              OUTPUT(N) = INPUT(LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%I, &
                                LOCSTREAM%Ptr%LOCAL_INDEXLOCATION(N)%J  )
           end if
        end if
     end do
  endif

  if(usableINTERP) then
     deallocate(ghostedINPUT,STAT=STATUS)
     VERIFY_(STATUS)
  end if
  deallocate(usableMASK)

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformG2T


subroutine MAPL_LocStreamTransformT2T ( OUTPUT, XFORM, INPUT, RC )
  real,                      intent(OUT) :: OUTPUT(:)
  type(MAPL_LocStreamXform), intent(IN ) :: XFORM
  real,                      intent(IN ) :: INPUT(:)
  integer, optional,         intent(OUT) :: RC  

! Local variables

  character(len=ESMF_MAXSTR)  :: IAm='MAPL_LocStreamTransformT2T'
  integer                     :: STATUS

  integer                     :: N
  real,   allocatable         :: FULLINPUT(:)

  ASSERT_(associated(Xform%PTR))

  do N = 1,Xform%PTR%LastLocal
     OUTPUT(Xform%PTR%IndexOut(N)) = INPUT(Xform%PTR%IndexIn(N))
  end do

  if(.not.Xform%PTR%Local) then

     allocate(FULLINPUT(Xform%Ptr%InputStream%Ptr%NT_GLOBAL),STAT=STATUS)
     VERIFY_(STATUS)

     call ESMFL_FCOLLECT(Xform%Ptr%InputStream%Ptr%TILEGRID, FULLINPUT, INPUT, RC=STATUS)
     VERIFY_(STATUS)

     do N = Xform%PTR%LastLocal+1,Xform%PTR%Count
        OUTPUT(Xform%PTR%IndexOut(N)) = FULLINPUT(Xform%PTR%IndexIn(N))
     end do

     deallocate(FULLINPUT)

  end if

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamTransformT2T



subroutine MAPL_LocStreamCreateXform ( Xform, LocStreamOut, LocStreamIn, NAME, MASK_OUT, RC )
  type(MAPL_LocStreamXform), intent(OUT) :: Xform
  type(MAPL_LocStream),      intent(IN ) :: LocStreamOut
  type(MAPL_LocStream),      intent(IN ) :: LocStreamIn
  character(len=*),          intent(IN ) :: NAME
  logical, optional,         intent(IN ) :: MASK_OUT(:)
  integer, optional,         intent(OUT) :: RC  

! Local variables

  character(len=ESMF_MAXSTR)  :: IAm='MAPL_LocStreamCreateXform'
  integer                     :: STATUS

  integer                     :: N, M, MM
  logical                     :: DONE(LocStreamOut%Ptr%NT_local)
  logical, pointer            :: ISDONE(:)
  logical                     :: dn(1)
  type (ESMF_VM)              :: vm
  integer                     :: NDES
  integer, pointer            :: GLOBAL_IdByPe(:) =>null() ! All Location Ids in PE   order

! Both streams must be subsets of same parent.
! The parent stream is usually an exchange grid.
!-----------------------------------------------

  ASSERT_(trim(LocStreamOut%PTR%ROOTNAME)==trim(LocStreamIn%PTR%ROOTNAME))

  allocate(XFORM%Ptr, STAT=STATUS)
  VERIFY_(STATUS)

  Xform%Ptr%InputStream  = LocStreamIn
  Xform%Ptr%OutputStream = LocStreamOut
  Xform%Ptr%Name         = NAME

! We have to fill all output locations where mask is true
!--------------------------------------------------------

  if(present(MASK_OUT)) then
     DONE = .not. MASK_OUT
  else
     DONE = .false.
  end if

  Xform%Ptr%count = count(.not.DONE)

  ALLOCATE(Xform%Ptr%IndexOut(Xform%Ptr%count), stat=STATUS)
  VERIFY_(STATUS)
  ALLOCATE(Xform%Ptr%IndexIn (Xform%Ptr%count), stat=STATUS)
  VERIFY_(STATUS)

  MM=1
  do N = 1, LocStreamOut%Ptr%NT_local
     if(DONE(N)) cycle
     do M = 1, LocStreamIn%Ptr%NT_local
        if(LocStreamOut%Ptr%Local_Id(N)==LocStreamIn%Ptr%Local_Id(M)) then
           Xform%Ptr%IndexOut(MM) = N
           Xform%Ptr%IndexIn (MM) = M
           DONE  (N) = .TRUE.
           MM=MM+1
           exit
        end if
     end do
  end do

  Xform%Ptr%LastLocal = MM-1

! Otherwise, assume nothing and do a full collect.
!-------------------------------------------------

  call ESMF_VMGetCurrent ( vm, rc=status )
  VERIFY_(STATUS)
  call ESMF_VMGet ( vm, petCount=nDEs, rc=status )
  VERIFY_(STATUS)

  allocate(IsDone(NDES))
  dn(1) = all(done)

  call MAPL_CommsAllGather(vm, dn, 1, &
                           isdone, 1, rc=status)
  VERIFY_(STATUS)

!  call ESMFL_FCOLLECT(LocStreamIn%Ptr%GRID, isdone(:), all(done), RC=STATUS)
!  VERIFY_(STATUS)

  Xform%Ptr%Local = all(isdone)

  if(.not.Xform%Ptr%Local) then

     allocate(GLOBAL_IdByPe(LocStreamIn%Ptr%NT_GLOBAL), STAT=STATUS)
     VERIFY_(STATUS)

     call ESMFL_FCOLLECT(LocStreamIn%Ptr%TILEGRID, GLOBAL_IdByPe, &
          LocStreamIn%Ptr%LOCAL_ID, RC=STATUS)
     VERIFY_(STATUS)
     
     do N = 1, LocStreamOut%Ptr%NT_local
        if(DONE(N)) cycle
        do M = 1, LocStreamIn%Ptr%NT_GLOBAL
           if(LocStreamOut%Ptr%Local_Id(N)==Global_IdByPe(M)) then
              Xform%Ptr%IndexOut(MM) = N
              Xform%Ptr%IndexIn (MM) = M
              DONE  (N) = .TRUE.
              MM=MM+1
              exit
           end if
        end do
     end do

     deallocate(GLOBAL_IdByPe)

  end if

! Make sure that did it
!----------------------

  ASSERT_(all(DONE))

  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_LocStreamCreateXform



integer function GRIDINDEX(STREAM,GRID,RC) 
  type(MAPL_LocStreamType),      intent(IN ) :: Stream
  type(ESMF_Grid),               intent(IN ) :: Grid
  integer, optional,             intent(OUT) :: RC  

  character(len=ESMF_MAXSTR) :: IAm='GridIndex'
  integer                    :: STATUS
  integer                    :: N

  character(len=ESMF_MAXSTR) :: NAME

! Find the given grid among the allowed grids
!--------------------------------------------

  call ESMF_GridGet(GRID, NAME=NAME, RC=STATUS)
  VERIFY_(STATUS)

  GridIndex = 0

  do N=1,STREAM%N_GRIDS
     if(STREAM%TILING(N)%NAME==NAME) then
        GridIndex = N
        exit
     end if
  end do

  ASSERT_(GridIndex/=0)

  RETURN_(ESMF_SUCCESS)

end function GRIDINDEX

subroutine MAPL_GridCoordAdjust(GRID, LOCSTREAM, RC)
  type(ESMF_Grid),               intent(INout ) :: Grid
  type(MAPL_LocStream),          intent(IN ) :: Locstream
  integer, optional,             intent(OUT) :: RC  

! local vars
!------------ 
  character(len=ESMF_MAXSTR) :: IAm='MAPL_GridCoordAdjust'
  integer                    :: STATUS
  
  integer :: NGRIDS
  integer :: I, J, N
  integer :: IM, JM
  integer :: IMSTART, JMSTART
  integer :: IM_WORLD, JM_WORLD

  logical :: found
  integer :: COUNTS(3), DIMS(2)
  integer :: rank, NT, IG
  type(ESMF_TypeKind)       :: tk
  character(len=ESMF_MAXSTR) :: GRIDNAME
  character(len=ESMF_MAXSTR), pointer :: GNAMES(:)
  type (ESMF_Array) :: carray(2)
  real(ESMF_KIND_R8) :: X, Y, W
  real(ESMF_KIND_R8), allocatable :: sumw(:,:), sumxw(:,:), sumyw(:,:)
  real(ESMF_KIND_R8), pointer :: gridx(:,:), gridy(:,:)

! get grid name
  call ESMF_GridGet(grid, name=gridname, rc=status)
  VERIFY_(STATUS)

  call MAPL_LocstreamGet(LOCSTREAM, GRIDNAMES = GNAMES, RC=STATUS)
  VERIFY_(STATUS)
! query loc_in for ngrids
  ngrids = size(gnames)
  ASSERT_(ngrids==2)

! validate that gridname_in is there
  found = .false.
  DO I = 1, NGRIDS
     IF (GNAMES(I) == GRIDNAME) THEN
        FOUND = .TRUE.
        exit
     ENDIF
  ENDDO
  ASSERT_(FOUND)

! get id of the grid we just found
  IG = I 

! get IM, JM and IM_WORLD, JM_WORLD
  call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, &
       globalCellCountPerDim=DIMS, RC=STATUS)
  VERIFY_(STATUS)

  IM = COUNTS(1)
  JM = COUNTS(2)

  IM_WORLD = DIMS(1)
  JM_WORLD = DIMS(2)

! get lower left corner (dims is just dummy arg)
  CALL ESMF_GRID_INTERIOR(GRID, IMSTART, DIMS(1), JMSTART, DIMS(2))
  
! Retrieve the coordinates so we can set them
  call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
       staggerloc=ESMF_STAGGERLOC_CENTER, &
       fptr=gridX, rc=status)
  VERIFY_(STATUS)

  call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
       staggerloc=ESMF_STAGGERLOC_CENTER, &
       fptr=gridY, rc=status)
  VERIFY_(STATUS)

  allocate(sumxw(IM_WORLD, JM_WORLD), stat=status)
  allocate(sumyw(IM_WORLD, JM_WORLD), stat=status)
  allocate(sumw (IM_WORLD, JM_WORLD), stat=status)
  VERIFY_(STATUS)

  SUMW = 0.0
  SUMXW = 0.0
  SUMYW = 0.0

  NT = LOCSTREAM%Ptr%NT_Global

! loop over tiles
  DO N = 1, NT
     I = LOCSTREAM%Ptr%TILING(IG)%Global_IndexLocation(N)%I
     J = LOCSTREAM%Ptr%TILING(IG)%Global_IndexLocation(N)%J
     W = LOCSTREAM%Ptr%TILING(IG)%Global_IndexLocation(N)%W
     X = locstream%Ptr%Global_GeoLocation(N)%X
     Y = locstream%Ptr%Global_GeoLocation(N)%Y
     SUMW(I,J) = SUMW(I,J) + W
     SUMXW(I,J) = SUMXW(I,J) + X * W
     SUMYW(I,J) = SUMYW(I,J) + Y * W
  END DO

  WHERE (SUMW == 0.0)
     SUMXW = MAPL_UNDEF
     SUMYW = MAPL_UNDEF
  ELSEWHERE
     SUMXW = SUMXW / SUMW
     SUMYW = SUMYW / SUMW

! Make sure the longitudes are between -180 and 180 degrees
     SUMXW = mod(SUMXW + 72180._8,360._8) - 180._8 ! -180<= lon0 <180
! Convert to radians
     SUMXW = SUMXW * (MAPL_PI)/180._8
     SUMYW = SUMYW * (MAPL_PI)/180._8
     
  END WHERE

! Modify grid coordinates
!------------------------
  GRIDX = SUMXW(IMSTART:IMSTART+IM-1,JMSTART:JMSTART+JM-1)
  GRIDY = SUMYW(IMSTART:IMSTART+IM-1,JMSTART:JMSTART+JM-1)

! Clean-up
!---------
  deallocate(sumw)
  deallocate(sumyw)
  deallocate(sumxw)

! All done
!---------
  RETURN_(ESMF_SUCCESS)

end subroutine MAPL_GridCoordAdjust

end module MAPL_LocStreamMod
