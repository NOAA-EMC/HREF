#ifdef GEOS5
#include "MAPL_Generic.h"
#endif

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  CARMA_GridCompMod --- CARMA Grid Component Class
!
! !INTERFACE:
!

   module  CARMA_GridCompMod

! !USES:

#ifdef GEOS5
   USE ESMF_Mod
   USE MAPL_Mod
#endif

   use Chem_Mod              ! Chemistry Base Class
   use Chem_StateMod         ! Chemistry State
!  Uses grav from CARMA?  Bad idea?
!   use Chem_ConstMod, only: grav, von_karman, cpd, &
!                            undefval => undef         ! Constants !
   use Chem_UtilMod          ! I/O
   use Chem_MieMod           ! Aerosol LU Tables, calculator
   use m_inpak90             ! Resource file management
   use m_die, only: die

   use carma_main_mod        ! include the CARMA base code
   use carma_constants_mod   ! use CARMA flag constants

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  CARMA_GridComp       ! The CARMA object 

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  CARMA_GridCompInitialize
   PUBLIC  CARMA_GridCompRun
   PUBLIC  CARMA_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the (pre-ESMF) CARMA Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

  type CARMA_GridComp
        character(len=255) :: name
        integer            :: nymd      ! date      
  end type CARMA_GridComp

  real, parameter :: OCEAN=0.0, LAND = 1.0, SEA_ICE = 2.0

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_GridCompInitialize --- Initialize CARMA_GridComp
!
! !INTERFACE:
!

   subroutine CARMA_GridCompInitialize ( gcCARMA, w_c, impChem, expChem, &
                                         nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real, intent(in) :: cdt                     ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(CARMA_GridComp), intent(inout) :: gcCARMA  ! Grid Component
   type(ESMF_State), intent(inout)     :: impChem  ! Import State
   type(ESMF_State), intent(inout)     :: expChem  ! Export State
   integer, intent(out) ::  rc                     ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 

! !DESCRIPTION: Initializes the CARMA Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'CARMA_GridCompInitialize'


   character(len=255) :: rcfilen = 'CARMA_GridComp.rc'
   integer :: ios, n
   integer :: i1, i2, im, j1, j2, jm, km, nbins, n1, n2, nbins_rc
   integer, allocatable :: ier(:)
   real, allocatable :: buffer(:,:)
   real :: qmax, qmin


   gcCARMA%name = 'CARMA Constituent Package'

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   km = w_c%grid%km
   nbins = w_c%reg%n_CARMA
   n1    = w_c%reg%i_CARMA
   n2    = w_c%reg%j_CARMA

!  PRC: hack
   do n = 1, nbins
    w_c%qa(n1+n-1)%data3d(i1:i2,j1:j2,1:km) = 1.
   enddo

   call init_()
   if ( rc /= 0 ) return


!                       -------------------
!                       Parse resource file
!                       -------------------

!  Load resource file
!  ------------------
   call i90_loadf ( rcfilen, ier(1) )
   if ( ier(1) .ne. 0 ) then
      call final_(10)
      return
   end if

!  Potentially here we would parse the resource file


!  Initialize date for BCs
!  -----------------------
   gcCARMA%nymd = -1   ! nothing read yet

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Set which fvGCM fields are needed
!  ---------------------------------
   call Chem_StateSetNeeded ( impChem, iSURFP,    .true., ier(1) )
   call Chem_StateSetNeeded ( impChem, iTSKIN,    .true., ier(2) )
   call Chem_StateSetNeeded ( impChem, iORO,      .true., ier(3) )
   call Chem_StateSetNeeded ( impChem, iT,        .true., ier(4) )
   call Chem_StateSetNeeded ( impChem, iAIRDENS,  .true., ier(5) )

   if ( any(ier(1:5) /= 0) ) then
        call final_(60)
        return
   endif

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif


!  All done
!  --------
   call i90_release()
   deallocate(ier)

   return


CONTAINS

   subroutine init_()
   integer ios, nerr
   nerr = max ( 32, nbins+1 )
   allocate ( ier(nerr), stat=ios )
   if ( ios /= 0 ) rc = 100
   end subroutine init_

   subroutine final_(ierr)
   integer :: ierr
   integer ios
   deallocate ( ier, stat=ios )
   call i90_release()
   rc = ierr
   end subroutine final_

   end subroutine CARMA_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine CARMA_GridCompRun ( gcCARMA, w_c, impChem, expChem, &
                                  nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(CARMA_GridComp), intent(inout) :: gcCARMA   ! Grid Component
   type(Chem_Bundle), intent(inout)    :: w_c       ! Chemical tracer fields   

! !INPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: impChem ! Import State
   integer, intent(in) :: nymd, nhms          ! time
   real, intent(in) :: cdt                    ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: expChem   ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine implements the so-called CARMA Driver. That 
!               is, adds chemical tendencies to each of the constituents,
!  Note: water wapor, the first constituent is not considered a chemical
!  constituents.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'CARMA_GridCompRun'
   character(len=*), parameter :: Iam = myname

   integer :: ier(20), idiag, idiag0, n
   integer :: i1, i2, im, j1, j2, jm, nbins, n1, n2, km, ios
   integer :: nymd1, nhms1, ijl, ijkl
   real :: qmax, qmin
   logical :: do_hostmodel = .true.
   logical :: do_coag, do_vtran


!  Input fields from fvGCM
!  -----------------------
   real, pointer, dimension(:,:)   ::  surfp, tskin, oro
   real, pointer, dimension(:,:,:) ::  tmpu, rhoa

!  Output diagnostic (like DU_set); probably not right for GEOS-5
!  -----------------------
   type(Chem_Array), pointer :: outDiagnostic(:)=>null()

#ifdef GEOS5 

#define EXPORT     expChem

!#define ptrDUSD       DU_set
!#define ptrSSSD       SS_set

   integer :: STATUS

!!!#include "CARMA_GetPointer___.h"

#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Quantities to be exported
!!! See above: I think I want this defined automagically!
!  -------------------------
!!!   type(Chem_Array), pointer :: BC_wet(:)

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm

   km = w_c%grid%km

   ijl  = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   ijkl = ijl * km


#ifdef GEOS5
!  Get 2D Imports
!  --------------
   call MAPL_GetPointer ( impChem, surfp,    'PS',       rc=ier(1) )
   call MAPL_GetPointer ( impChem, tskin,    'TA',       rc=ier(2) )
   call MAPL_GetPointer ( impChem, oro,      'LWI',      rc=ier(3) )

!  Get 3D Imports
!  --------------
   call MAPL_GetPointer ( impChem, tmpu,   'T',       rc=ier(4) )
   call MAPL_GetPointer ( impChem, rhoa,   'AIRDENS', rc=ier(5) )

#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Get input fvGCM 2D diagnostics
!  ------------------------------
   call Chem_StateGetArray2D ( impChem, iTSKIN,    tskin,    ier(1) )
   call Chem_StateGetArray2D ( impChem, iSURFP,    surfp,    ier(2) )
   call Chem_StateGetArray2D ( impChem, iORO,      oro,      ier(3) )

!  Get input fvGCM 3D diagnostics
!  ------------------------------
   call Chem_StateGetArray3D ( impChem, iT,        tmpu,     ier(4) )
   call Chem_StateGetArray3D ( impChem, iAIRDENS,  rhoa,     ier(5) )

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

   if ( any(ier(1:5) /= 0) ) then
        rc = 10 
        return
   end if

#ifdef DEBUG

   call pmaxmin('CARMA: tskin      ', tskin   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('CARMA: surfp      ', surfp   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('CARMA: oro        ', oro     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('CARMA: tmpu       ', tmpu    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('CARMA: rhoa       ', rhoa    , qmin, qmax, ijkl,1, 1. )

#endif

!  Make a CARMA call
!  -----------------
!  DUST
   if( w_c%reg%doing_DU ) then

    nbins  = w_c%reg%n_DU
    n1     = w_c%reg%i_DU
    n2     = w_c%reg%j_DU

!   Do we even request any CARMA services?
    if( index(w_c%qa(n1)%wantServices, "CARMA") .ne. 0) then

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!    Allocate/Get pointers to export state
!    -------------------------------------
     idiag0 = iDUSD001
     allocate( outDiagnostic(nbins), stat=ios)
     do n = 1, nbins
       idiag = idiag0 + n - 1
       call Chem_StateGetArray2D ( expChem, idiag, outDiagnostic(n)%data2d, ier(n) )
     end do
     if ( any(ier(1:nbins) /= 0) ) then
       rc = 15 
       return
     end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!    Set up the call
     do_vtran = .false.
     do_coag  = .false.
     if( index(w_c%qa(n1)%wantServices, ":CARMA_Sedimentation:") .ne. 0) do_vtran = .true.
     if( index(w_c%qa(n1)%wantServices, ":CARMA_Coagulation:"  ) .ne. 0) do_coag = .true.

     call run_carma_ ( )

     if(rc /= 0) return

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

     deallocate(outDiagnostic, stat=ios)

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

    endif
   endif


!  SEASALT
   if( w_c%reg%doing_SS ) then

    nbins  = w_c%reg%n_SS
    n1     = w_c%reg%i_SS
    n2     = w_c%reg%j_SS

!   Do we even request any CARMA services?
    if( index(w_c%qa(n1)%wantServices, "CARMA") .ne. 0) then

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!    Allocate/Get pointers to export state
!    -------------------------------------
     idiag0 = iSSSD001
     allocate( outDiagnostic(nbins), stat=ios)
     do n = 1, nbins
       idiag = idiag0 + n - 1
       call Chem_StateGetArray2D ( expChem, idiag, outDiagnostic(n)%data2d, ier(n) )
     end do
     if ( any(ier(1:nbins) /= 0) ) then
       rc = 15 
       return
     end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!    Set up the call
     do_vtran = .false.
     do_coag  = .false.
     if( index(w_c%qa(n1)%wantServices, ":CARMA_Sedimentation:") .ne. 0) do_vtran = .true.
     if( index(w_c%qa(n1)%wantServices, ":CARMA_Coagulation:"  ) .ne. 0) do_coag = .true.

     call run_carma_ ( )

     if(rc /= 0) return

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

     deallocate(outDiagnostic, stat=ios)

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

    endif
   endif

   return

CONTAINS

   subroutine run_carma_ ( )

   integer :: i, j, k
   integer :: NX, NY, NZ
   integer :: NGROUP = 1, NELEM = 1, NGAS = 0
   real, dimension(:), allocatable :: radius, rup, rlow, rmin, rmrat, rhop
   real :: dlon, dlat, dom_llx, dom_urx, dom_lly, dom_ury, rhFactor
   real, allocatable :: ptop(:,:), relhum(:,:,:), q_array(:,:,:,:,:)

!  Scale to apply to RH to get into range 0 - 1
#ifdef GEOS5
   rhFactor = 1.
#else
   rhFactor = 0.01
#endif

   NX = i2-i1+1
   NY = j2-j1+1
   NZ = km

!  Allocate space for and fill in bin information
   allocate( radius(nbins), rup(nbins), rlow(nbins), rmrat(nbins), &
             rmin(nbins), rhop(nbins), stat = ios)
   if( ios /=0 ) then
     rc = 1
     return
   endif
   do n = 1, nbins
     radius(n) = w_c%qa(n+n1-1)%r
     rlow(n)   = w_c%qa(n+n1-1)%rlow
     rup(n)    = w_c%qa(n+n1-1)%rup
     rhop(n)   = w_c%qa(n+n1-1)%rhop
   enddo

!  Allocate and set other local variables
   allocate( ptop(i1:i2,j1:j2), relhum(i1:i2,j1:j2,1:km), &
             q_array(i1:i2,j1:j2,1:km,1:nbins,NELEM), stat=ios)
   if(ios /= 0) then
     rc = 1
     return
   endif
   ptop(:,:) = w_c%grid%ptop
   relhum(:,:,:) = w_c%rh(i1:i2,j1:j2,1:km) * rhFactor
   do n = 1, nbins
     q_array(:,:,:,n,1) = w_c%qa(n+n1-1)%data3d
   enddo

!  Initialize the counter for the change in column mass loading
   if(associated(outDiagnostic)) then
     do n = 1, nbins
      outDiagnostic(n)%data2d(:,:) = 0.0
      do k = 1, km
       outDiagnostic(n)%data2d = outDiagnostic(n)%data2d + &
          w_c%qa(n+n1-1)%data3d(:,:,k)*w_c%delp(:,:,k)/grav
      enddo
     enddo
   endif    

   dlon = w_c%grid%lon_del
   dlat = w_c%grid%lat_del
   dom_llx = w_c%grid%lon(i1)-dlon/2._f
   dom_urx = w_c%grid%lon(i2)+dlon/2._f
   dom_lly = w_c%grid%lat(j1)-dlat/2._f
   dom_ury = w_c%grid%lat(j2)+dlat/2._f

   call carma_main ( NX, NY, NZ                                              &
                   , NGROUP, NELEM, nbins, NGAS                              &
                   , do_hostmodel = do_hostmodel                             &
                   , igridv = I_SIG, igridh = I_LL                           &
                   , dtime = cdt                                             &
                   , t = tmpu(i1:i2,j1:j2,1:km)                              &
                   , t_surf = tskin(i1:i2,j1:j2)                             &
                   , rhoa = rhoa(i1:i2,j1:j2,1:km)                           &
                   , delp = w_c%delp(i1:i2,j1:j2,1:km)                       &
                   , relhum = relhum                                         &
                   , p_surf = surfp(i1:i2,j1:j2), p_top = ptop(i1:i2,j1:j2)  &
                   , dlon = dlon, dlat = dlat                                &
                   , dom_llx = dom_llx, dom_urx = dom_urx                    &
                   , dom_lly = dom_lly, dom_ury = dom_ury                    &
                   , q = q_array(i1:i2,j1:j2,1:km,1:nbins,:)                 &
                   , do_coag = do_coag, do_vtran = do_vtran                  &
                   , rhFlag = w_c%qa(n1)%irhFlag                             &
                   , r = radius, rlow = rlow, rup = rup                      &
                   , rhop = rhop                                             &
                   , rc = rc )

   if(rc /= 0) then
     print *, 'CARMA rc = ', rc
     return
   endif

!  Deallocate space
   deallocate( radius, rup, rlow, rmrat, rmin, rhop, stat = ios)
   if( ios /=0 ) then
     rc = 1
     return
   endif

   do n = 1, nbins
     w_c%qa(n+n1-1)%data3d = q_array(:,:,:,n,1)
   enddo

!  Compute change in column mass
!  As constructed, a positive number indicates a loss of mass in the bin
   if(associated(outDiagnostic)) then
     do n = 1, nbins
      do k = 1, km
       outDiagnostic(n)%data2d =   outDiagnostic(n)%data2d - &
          w_c%qa(n+n1-1)%data3d(:,:,k)*w_c%delp(:,:,k)/grav
      enddo
     enddo
!    Recast from a change in mass to a flux
     do n = 1, nbins
      outDiagnostic(n)%data2d = outDiagnostic(n)%data2d / cdt
     enddo
   endif    

   deallocate( ptop, q_array, stat = ios)
   if( ios /=0 ) then
     rc = 1
     return
   endif

   return
   end subroutine run_carma_

 end subroutine CARMA_GridCompRun

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine CARMA_GridCompFinalize ( gcCARMA, w_c, impChem, expChem, &
                                       nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(CARMA_GridComp), intent(inout) :: gcCARMA   ! Grid Component

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(in)  :: w_c      ! Chemical tracer fields   
   integer, intent(in) :: nymd, nhms          ! time
   real,    intent(in) :: cdt                 ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: impChem   ! Import State
   type(ESMF_State), intent(inout) :: expChem   ! Import State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine finalizes this Grid Component.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'CARMA_GridCompFinalize'
   rc=0
   return

 end subroutine CARMA_GridCompFinalize

 end module CARMA_GridCompMod

