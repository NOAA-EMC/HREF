#ifdef GEOS5
#include "MAPL_Generic.h"
#endif

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  SS_GridCompMod --- SS Grid Component Class
!
! !INTERFACE:
!

   module  SS_GridCompMod

! !USES:

#ifdef GEOS5
   USE ESMF_Mod
   USE MAPL_Mod
#endif

   use Chem_Mod              ! Chemistry Base Class
   use Chem_StateMod         ! Chemistry State
   use Chem_SettlingMod      ! Settling
   use Chem_DepositionMod    ! Aerosol Deposition
   use Chem_ConstMod, only: grav, von_karman, cpd     ! Constants !
   use Chem_UtilMod          ! I/O
   use Chem_MieMod           ! Aerosol LU Tables, calculator
   use m_inpak90             ! Resource file management
   use m_die, only: die

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  SS_GridComp       ! The SS object 

!
! !PUBLIIC MEMBER FUNCTIONS:
!

   PUBLIC  SS_GridCompInitialize
   PUBLIC  SS_GridCompRun
   PUBLIC  SS_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the (pre-ESMF) SS Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  13Mar2013 Lu        Add NEMS option
!
!EOP
!-------------------------------------------------------------------------

  type SS_GridComp
        character(len=255) :: name
        type(Chem_Mie), pointer :: mie_tables  ! aod LUTs
        integer       :: rhFlag
        real, pointer :: radius(:)      ! particle effective radius [um]
        real, pointer :: rLow(:)        ! lower radius of particle bin [um]
        real, pointer :: rUp(:)         ! upper radius of particle bin [um]
        real, pointer :: rhop(:)        ! dry salt particle density [kg m-3]
        integer       :: doing_scav     ! compute tracer scavenging for NEMS
  end type SS_GridComp

  real, parameter :: OCEAN=0.0, LAND = 1.0, SEA_ICE = 2.0

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SS_GridCompInitialize --- Initialize SS_GridComp
!
! !INTERFACE:
!

   subroutine SS_GridCompInitialize ( gcSS, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real,    intent(in) :: cdt                  ! chemical timestep (secs)

! !OUTPUT PARAMETERS:

   type(SS_GridComp), intent(inout) :: gcSS   ! Grid Component
   type(ESMF_State), intent(inout)  :: impChem  ! Import State
   type(ESMF_State), intent(inout)  :: expChem  ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the SS Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'SS_GridCompInitialize'


   character(len=255) :: rcfilen = 'SS_GridComp.rc'
   integer :: ios, n
   integer, allocatable :: ier(:)
   integer :: i1, i2, im, j1, j2, jm, nbins, n1, n2, nbins_rc
   real :: qmin, qmax
   real :: radius, rlow, rup, rmrat, rmin, rhop, fscav
   integer :: irhFlag
   integer :: idoing_scav  ! NEMS option to re-activate convective removal 
   character(len=255) :: CARMA_Services = ' '



   gcSS%name = 'SS Constituent Package'

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   nbins = w_c%reg%n_SS
   n1  = w_c%reg%i_SS
   n2  = w_c%reg%j_SS

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

   call i90_label ( 'number_SS_bins:', ier(1) )
   nbins_rc = i90_gint ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(20)
      return
   end if
   if ( nbins_rc /= nbins ) then
      call final_(25)
      return
   end if
!   call final_(0)

!  CARMA Services
!  --------------
   call i90_label ( 'CARMA_Services:', ier(1) )
   if ( ier(1) /= 0 ) then
      CARMA_Services = ' '
   else
      call i90_gtoken ( CARMA_Services, ier(1) )
      if ( ier(1) /= 0 ) then
        CARMA_Services = ' '
      end if
   end if
   do n = n1, n2
    w_c%qa(n)%wantServices = trim(CARMA_Services)
   enddo

!  Particle radius
!  ---------------
   call i90_label ( 'particle_radius:', ier(1) )
   do n = 1, nbins
      radius           = i90_gfloat ( ier(n+1) )
      gcSS%radius(n)   = radius
      w_c%qa(n1+n-1)%r = radius * 1.e-6  ! save radius in [m]
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if

!  Particle radius (lower bound)
!  ---------------
   call i90_label ( 'radius_lower:', ier(1) )
   do n = 1, nbins
      rlow                = i90_gfloat ( ier(n+1) )
      gcSS%rlow(n)        = rlow
      w_c%qa(n1+n-1)%rlow = rlow * 1.e-6  ! save radius in [m]
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if

!  Particle radius (upper bound)
!  ---------------
   call i90_label ( 'radius_upper:', ier(1) )
   do n = 1, nbins
      rup                = i90_gfloat ( ier(n+1) )
      gcSS%rup(n)        = rup
      w_c%qa(n1+n-1)%rup = rup * 1.e-6  ! save radius in [m]
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Dry Particle Density
!  ---------------
   call i90_label ( 'SS_density:', ier(1) )
   do n = 1, nbins
      rhop                = i90_gfloat ( ier(n+1) )
      gcSS%rhop(n)        = rhop
      w_c%qa(n1+n-1)%rhop = rhop
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Scavenging Efficiency
!  To be used in convtran.F90, this parameter
!  is the scavenging efficiency of the tracer [km -1]
!  ---------------
   call i90_label ( 'fscav:', ier(1) )
   do n = 1, nbins
      fscav                   = i90_gfloat ( ier(n+1) )
      w_c%reg%fscav(n1+n-1)   = fscav
      w_c%qa(n1+n-1)%fscav    = fscav
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if

!                          -------
!  Option to compute convective rainout/washout in GOCART
!  ---------------

   gcSS%doing_scav = 0     ! Default is to compute convective
!                          ! rainout/washout in GFS physics
#ifdef NEMS
   call i90_label ( 'doing_scav:', ier(1) )
   idoing_scav                 = i90_gint ( ier(2) )
   gcSS%doing_scav             = idoing_scav
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if

!  invoke the option to compute convective removal in GOCART
!  set fscav (scav used in GFS RAS) to 0.
   if ( gcSS%doing_scav == 1 ) then
     do n = 1, nbins
      w_c%reg%fscav(n1+n-1)   = 0.
      w_c%qa(n1+n-1)%fscav    = 0.
     end do
   endif
#endif

!                          -------

!  Particle affected by relative humidity?
!  ---------------
   call i90_label ( 'rhFlag:', ier(1) )
   irhFlag                    = i90_gint ( ier(2) )
   w_c%qa(n1+n-1)%irhFlag     = irhFlag
   gcSS%rhFlag                = irhFlag
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if


#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Set which fvGCM fields are needed
!  ---------------------------------
   call Chem_StateSetNeeded ( impChem, iFRACLAKE, .true., ier(1) )
   call Chem_StateSetNeeded ( impChem, iGWETTOP,  .true., ier(2) )
   call Chem_StateSetNeeded ( impChem, iORO,      .true., ier(3) )
   call Chem_StateSetNeeded ( impChem, iU10M,     .true., ier(4) )
   call Chem_StateSetNeeded ( impChem, iV10M,     .true., ier(5) )
   call Chem_StateSetNeeded ( impChem, iLAI,      .true., ier(6) )
   call Chem_StateSetNeeded ( impChem, iUSTAR,    .true., ier(7) )
   call Chem_StateSetNeeded ( impChem, iPRECC,    .true., ier(8) )
   call Chem_StateSetNeeded ( impChem, iPRECL,    .true., ier(9) )
   call Chem_StateSetNeeded ( impChem, iDQCOND,   .true., ier(10) )
   call Chem_StateSetNeeded ( impChem, iT,        .true., ier(11) )
   call Chem_StateSetNeeded ( impChem, iAIRDENS,  .true., ier(12) )
   call Chem_StateSetNeeded ( impChem, iU,        .true., ier(13) )
   call Chem_StateSetNeeded ( impChem, iV,        .true., ier(14) )
   call Chem_StateSetNeeded ( impChem, iPBLH,     .true., ier(15) )
   call Chem_StateSetNeeded ( impChem, iSHFX,     .true., ier(16) )
   call Chem_StateSetNeeded ( impChem, iZ0H,      .true., ier(17) )
   call Chem_StateSetNeeded ( impChem, iHSURF,    .true., ier(18) )
   call Chem_StateSetNeeded ( impChem, iHGHTE,    .true., ier(19) )

   if ( any(ier(1:19) /= 0) ) then
        call final_(90)
        return
   endif

!  Select fields to be produced in the export state
!  ------------------------------------------------
   n = nbins

!  Emission Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iSSEM001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iSSEM002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iSSEM003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iSSEM004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iSSEM005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iSSEM006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iSSEM007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iSSEM008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(91)
        return
   endif

!  Sedimentation Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iSSSD001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iSSSD002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iSSSD003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iSSSD004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iSSSD005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iSSSD006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iSSSD007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iSSSD008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(92)
        return
   endif

!  Dry Deposition Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iSSDP001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iSSDP002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iSSDP003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iSSDP004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iSSDP005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iSSDP006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iSSDP007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iSSDP008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(93)
        return
   endif

!  Wet Deposition Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iSSWT001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iSSWT002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iSSWT003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iSSWT004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iSSWT005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iSSWT006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iSSWT007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iSSWT008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(94)
        return
   endif


!  Other diagnostics
   call Chem_StateSetNeeded ( expChem, iSSSMASS, .true., ier(1) )
   call Chem_StateSetNeeded ( expChem, iSSCMASS, .true., ier(2) )
   call Chem_StateSetNeeded ( expChem, iSSMASS, .true., ier(3) )
   call Chem_StateSetNeeded ( expChem, iSSEXTTAU, .true., ier(4) )
   call Chem_StateSetNeeded ( expChem, iSSSCATAU, .true., ier(5) )
   call Chem_StateSetNeeded ( expChem, iSSSM25, .true., ier(6) )
   call Chem_StateSetNeeded ( expChem, iSSCM25, .true., ier(7) )
   call Chem_StateSetNeeded ( expChem, iSSMASS25, .true., ier(8) )
   call Chem_StateSetNeeded ( expChem, iSSEXTT25, .true., ier(9) )
   call Chem_StateSetNeeded ( expChem, iSSSCAT25, .true., ier(10) )

   if ( any(ier(1:10) /= 0) ) then
        call final_(70)
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
   allocate ( gcSS%radius(nbins), gcSS%rLow(nbins), gcSS%rUp(nbins), &
              gcSS%rhop(nbins), ier(nerr), stat=ios )
   if ( ios /= 0 ) rc = 100
   end subroutine init_

   subroutine final_(ierr)
   integer :: ierr
   integer ios
   deallocate ( gcSS%radius, gcSS%rhop, gcSS%rLow, gcSS%rUp, ier, stat=ios )
   call i90_release()
   rc = ierr
   end subroutine final_

   end subroutine SS_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SS_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine SS_GridCompRun ( gcSS, w_c, impChem, expChem, &
                               nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(SS_GridComp), intent(inout) :: gcSS   ! Grid Component
   type(Chem_Bundle), intent(inout) :: w_c      ! Chemical tracer fields   

! !INPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: impChem    ! Import State
   integer, intent(in) :: nymd, nhms          ! time
   real,    intent(in) :: cdt                 ! chemical timestep (secs)

! !OUTPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: expChem     ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine implements the so-called SS Driver. That 
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

   character(len=*), parameter :: myname = 'SS_GridCompRun'
   character(len=*), parameter :: Iam = myname
   integer :: ier(32), idiag
   integer :: i1, i2, im, j1, j2, jm, nbins, nbeg, nend, km, n, ios, ijl, ijkl
   real :: qmin, qmax
   real, pointer :: SS_radius(:), SS_rhop(:)


!  Input fields from fvGCM
!  -----------------------
   real, pointer, dimension(:,:)   ::  fraclake, gwettop, oro, u10m, v10m, &
                                       xlai, ustar, precc, precl, pblh, &
                                       shflux, z0h, hsurf
   real, pointer, dimension(:,:,:) ::  dqcond, tmpu, rhoa, u, v, hghte



#ifdef GEOS5 

#define EXPORT        expChem

#define ptrSSWT       SS_wet
#define ptrSSEM       SS_emis
#define ptrSSDP       SS_dep
#define ptrSSSD       SS_set

#define ptrSSMASS     SS_mass
#define ptrSSMASS25   SS_mass25
#define ptrSSSMASS    SS_sfcmass
#define ptrSSCMASS    SS_colmass
#define ptrSSEXTTAU   SS_exttau
#define ptrSSSCATAU   SS_scatau
#define ptrSSSMASS25  SS_sfcmass25
#define ptrSSCMASS25  SS_colmass25
#define ptrSSEXTT25   SS_exttau25
#define ptrSSSCAT25   SS_scatau25
#define ptrSSAERIDX   SS_aeridx

   integer :: STATUS

#include "SS_GetPointer___.h"

#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Quantities to be exported
!  -------------------------
   type(Chem_Array), pointer :: SS_emis(:), SS_set(:), SS_dep(:), SS_wet(:), &
                                SS_sfcmass, SS_colmass, SS_mass, SS_exttau, &
                                SS_scatau, &
                                SS_sfcmass25, SS_colmass25, SS_mass25, SS_exttau25, &
                                SS_scatau25

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif


!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm

   km = w_c%grid%km
   nbins = w_c%reg%n_SS
   nbeg  = w_c%reg%i_SS
   nend  = w_c%reg%j_SS

   ijl  = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   ijkl = ijl * km

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Work space for holding seasalt output
!  ----------------------------------
   allocate ( SS_emis(nbins), SS_set(nbins), SS_dep(nbins), SS_wet(nbins), &
              SS_sfcmass, SS_colmass, SS_mass, SS_exttau, SS_scatau,       &
              SS_sfcmass25, SS_colmass25, SS_mass25, SS_exttau25, SS_scatau25,       & 
              stat = ios)
   if ( ios /= 0 ) then
      rc = 1
      return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif


!  Seasalt particle radius [m] and density [kg m-3]
!  ---------------------------------------------
   allocate( SS_radius(nbins), SS_rhop(nbins) )
   SS_radius = 1.e-6*gcSS%radius
   SS_rhop   = gcSS%rhop

#ifdef GEOS5

!  Get 2D Imports
!  --------------
   call MAPL_GetPointer ( impChem, fraclake, 'FRLAKE',  rc=ier(1) )
   call MAPL_GetPointer ( impChem, gwettop,  'WET1',    rc=ier(2) )
   call MAPL_GetPointer ( impChem, oro,      'LWI',     rc=ier(3) )
   call MAPL_GetPointer ( impChem, u10m,     'U10M',    rc=ier(4) )
   call MAPL_GetPointer ( impChem, v10m,     'V10M',    rc=ier(5) )
   call MAPL_GetPointer ( impChem, xlai,     'LAI',     rc=ier(6) )
   call MAPL_GetPointer ( impChem, ustar,    'USTAR',   rc=ier(7) )
   call MAPL_GetPointer ( impChem, precc,    'CN_PRCP', rc=ier(8) )
   call MAPL_GetPointer ( impChem, precl,    'NCN_PRCP',   rc=ier(9) )
   call MAPL_GetPointer ( impChem, pblh,     'ZPBL',    rc=ier(10) )
   call MAPL_GetPointer ( impChem, shflux,   'SH',      rc=ier(11) )
   call MAPL_GetPointer ( impChem, z0h,      'Z0H',     rc=ier(12) )
   ier(13) = 0 ! see below for hsurf

!  Get 3D Imports
!  --------------
   call MAPL_GetPointer ( impChem, dqcond, 'DQDT',    rc=ier(14) )
   call MAPL_GetPointer ( impChem, tmpu,   'T',       rc=ier(15) )
   call MAPL_GetPointer ( impChem, rhoa,   'AIRDENS', rc=ier(16) )
   call MAPL_GetPointer ( impChem, u,      'U',       rc=ier(17) )
   call MAPL_GetPointer ( impChem, v,      'V',       rc=ier(18) )
   call MAPL_GetPointer ( impChem, hghte,  'ZLE',     rc=ier(19) )

!  Unlike GEOS-4 hghte is defined for km+1
!  ---------------------------------------
   hsurf => hghte(i1:i2,j1:j2,km) ! in GEOS-5 hghte is in [0,km]
    

#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Get input fvGCM 2D diagnostics
!  ------------------------------
   call Chem_StateGetArray2D ( impChem, iFRACLAKE, fraclake, ier(1) )
   call Chem_StateGetArray2D ( impChem, iGWETTOP,  gwettop,  ier(2) )
   call Chem_StateGetArray2D ( impChem, iORO,      oro,      ier(3) )
   call Chem_StateGetArray2D ( impChem, iU10M,     u10m,     ier(4) )
   call Chem_StateGetArray2D ( impChem, iV10M,     v10m,     ier(5) )
   call Chem_StateGetArray2D ( impChem, iLAI,      xlai,     ier(6) )
   call Chem_StateGetArray2D ( impChem, iUSTAR,    ustar,    ier(7) )
   call Chem_StateGetArray2D ( impChem, iPRECC,    precc,    ier(8) )
   call Chem_StateGetArray2D ( impChem, iPRECL,    precl,    ier(9) )
   call Chem_StateGetArray2D ( impChem, iPBLH,     pblh,     ier(10) )
   call Chem_StateGetArray2D ( impChem, iSHFX,     shflux,   ier(11) )
   call Chem_StateGetArray2D ( impChem, iZ0H,      z0h,      ier(12) )
   call Chem_StateGetArray2D ( impChem, iHSURF,    hsurf,      ier(13) )

!  Get input fvGCM 3D diagnostics
!  ------------------------------
   call Chem_StateGetArray3D ( impChem, iDQCOND,   dqcond,   ier(14) )
   call Chem_StateGetArray3D ( impChem, iT,        tmpu,     ier(15) )
   call Chem_StateGetArray3D ( impChem, iAIRDENS,  rhoa,     ier(16) )
   call Chem_StateGetArray3D ( impChem, iU,        u,        ier(17) )
   call Chem_StateGetArray3D ( impChem, iV,        v,        ier(18) )
   call Chem_StateGetArray3D ( impChem, iHGHTE,    hghte,    ier(19) )

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

   if ( any(ier(1:19) /= 0) ) then
        rc = 10 
        return
   end if

!  Make sure LAI has values over ocean
!  -----------------------------------
   where ( oro /= LAND  )  xlai = 0.0

#ifdef DEBUG

   call pmaxmin('SS: fraclake   ', fraclake, qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: gwtop      ', gwettop , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: oro        ', oro     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: u10m       ', u10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: v10m       ', v10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: xlai       ', xlai    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: ustar      ', ustar   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: precc      ', precc   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: precl      ', precl   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: pblh       ', pblh    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: shfflux    ', shflux  , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: z0h        ', z0h     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SS: hsurf      ', hsurf   , qmin, qmax, ijl,1, 1. )

   call pmaxmin('SS: dqcond     ', dqcond  , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SS: tmpu       ', tmpu    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SS: rhoa       ', rhoa    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SS: u          ', u       , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SS: v          ', v       , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SS: hghte      ', hghte   , qmin, qmax, ijkl,1, 1. )

#endif

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Get pointers to export state
!  ----------------------------
   do n = 1, nbins
      idiag = iSSEM001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, SS_emis(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iSSSD001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, SS_set(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iSSDP001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, SS_dep(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iSSWT001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, SS_wet(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   idiag = iSSSMASS
   call Chem_StateGetArray2D ( expChem, idiag, SS_sfcmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSSCMASS
   call Chem_StateGetArray2D ( expChem, idiag, SS_colmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSSMASS
   call Chem_StateGetArray3D ( expChem, idiag, SS_mass%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSSEXTTAU
   call Chem_StateGetArray2D ( expChem, idiag, SS_exttau%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSSSCATAU
   call Chem_StateGetArray2D ( expChem, idiag, SS_scatau%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSSSM25
   call Chem_StateGetArray2D ( expChem, idiag, SS_sfcmass25%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSSCM25
   call Chem_StateGetArray2D ( expChem, idiag, SS_colmass25%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSSMASS25
   call Chem_StateGetArray3D ( expChem, idiag, SS_mass25%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSSEXTT25
   call Chem_StateGetArray2D ( expChem, idiag, SS_exttau25%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSSSCAT25
   call Chem_StateGetArray2D ( expChem, idiag, SS_scatau25%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!  Seasalt Source
!  -----------
!   call SS_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcSS, w_c, &
!                      oro, u10m, v10m, SS_emis, rc )
   call SS_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcSS, w_c, &
                      oro, u10m, v10m, tmpu, SS_emis, rc )

#ifdef DEBUG
   do n = nbeg, nend
      call pmaxmin('SS: q_emi', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), &
                                qmin, qmax, ijl, km, 1. )
   end do
#endif

!  Seasalt Settling
!  ----------------
!  CARMA potentially provides this service.  Check to see.
   if( index(w_c%qa(nbeg)%wantServices,":CARMA_Sedimentation:") .eq. 0) then
     call Chem_Settling ( i1, i2, j1, j2, km, nbeg, nend, nbins, gcSS%rhFlag, &
                          SS_radius, SS_rhop, cdt, w_c, tmpu, rhoa, hsurf,    &
                          hghte, SS_set, rc )
   endif

#ifdef DEBUG
   do n = nbeg, nend
      call pmaxmin('SS: q_set', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Seasalt Deposition
!  -----------
   call Chem_Deposition( i1, i2, j1, j2, km, nbeg, nend, nbins, cdt, w_c, &
                         SS_radius, SS_rhop, tmpu, rhoa, hsurf, hghte, oro, ustar, &
                         u10m, v10m, fraclake, gwettop, pblh, shflux, z0h, SS_dep, rc )

#ifdef DEBUG
   do n = nbeg, nend
      call pmaxmin('SS: q_dry', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Seasalt Wet Removal
!  -----------
   call SS_Wet_Removal ( i1, i2, j1, j2, km, nbins, cdt, rhoa, gcSS, w_c, &
                         precc, precl, dqcond, tmpu, SS_wet, rc )

#ifdef DEBUG
   do n = nbeg, nend
      call pmaxmin('SS: q_wet', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif


!  Compute the desired output diagnostics here
!  Ideally this will go where chemout is called in fvgcm.F since that
!  will reflect the distributions after transport, etc.
!  ------------------------------------------------------------------
   call SS_Compute_Diags(i1, i2, j1, j2, km, nbins, gcSS, w_c, tmpu, rhoa, &
                         SS_sfcmass, SS_colmass, SS_mass, SS_exttau, SS_scatau, &
                         SS_sfcmass25, SS_colmass25, SS_mass25, SS_exttau25, SS_scatau25, &
                         rc)

!  Clean up
!  --------
   deallocate ( SS_radius, SS_rhop, stat=ios )

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

   deallocate ( SS_emis, SS_set, SS_dep, SS_wet, &
                SS_sfcmass, SS_colmass, SS_mass, SS_exttau, SS_scatau, &
                SS_sfcmass25, SS_colmass25, SS_mass25, SS_exttau25,    &
                SS_scatau25, stat=ios )

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

   return



CONTAINS
!##############################################################################
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SS_Emission - Adds seasalt emission for one timestep
!
! !INTERFACE:
!

!   subroutine SS_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcSS, w_c, &
!                            oro, u10m, v10m, SS_emis, rc )
   subroutine SS_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcSS, w_c, &
                            oro, u10m, v10m, tmpu, SS_emis, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   type(SS_GridComp), intent(in)    :: gcSS       ! SS Grid Component
   real, pointer, dimension(:,:) :: oro, u10m, v10m
   real, pointer, dimension(:,:,:) :: tmpu    ! temperature [K]

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: SS_emis(nbins) ! SS emissions, kg/m2/s
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'SS_Emission'

! !DESCRIPTION: Updates the seasalt concentration with emissions every timestep
!  The approach here is to use the modified Monahan et al. (1986) source
!  formulation (see Gong 2003) which does a reasonable job simulating
!  sea salt number concentrations for r80 between 0.07 um and 20 um.
!  Gong suggests that the parameterization is good to dry radius 0.03 um,
!  so we use that as our lower radius bin limit.  The function gives the
!  particle flux at RH = 80% dF/dr [# m-2 s-1 um-1] as a function of particle
!  radius r [um] and the 10-m wind speed [m s-1].  The dry particle number
!  flux is just dF/drDry = fac*dF/dr, where fac is really a function of
!  particle size converting dry radius to wet radius (RH=80%).
!  Two parameterizations for the swelling are permitted.  Setting rhFlag = 1 in
!  the resource file specifies the Fitzgerald parameterization.  For our sizes
!  a value of fac = 2.00 is pretty good, so we use that (see Fitzgerald, JAM,
!  1975 for 100% soluble NaCl).  Setting rhFlag = 2 is the resource file
!  specifies the Gerber parameterization.  See Gong et al. 1997.  For our size
!  range and that choice fac = 1.65 is pretty good.
!
!  Convert to a mass flux by multiplying by the particle mass (note the units).
!  Similar to GOCART, we have five major bins and employ a simple sub-binning 
!  to put the right mass into each bin (i.e., radius_i has rLow_i and rUp_i 
!  which define the edges of the bin and we actually calculate the emissions
!  across the range with a number of sub-bins and dump all the mass into a super-bin.

!  FUTURE IMPROVEMENTS: 1) Evaluate the choice of bin sizes used
!
! !REVISION HISTORY:
!
!  19Apr2013, Sarah Lu, add GEOS-Chem upgrade
!  06Nov2003, Colarco
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer  ::  i, j, k, m, n, ios, ir, nr
   integer  ::  nbeg, nend
   real     ::  emis(i1:i2,j1:j2)       ! Local bin emission
   real, parameter ::  pi = 3.1415
   real, parameter ::  rho = 1000.      ! density of water [kg m-3]
   real            ::  rLow, rUp        ! bounding radii of super-bins [um]
   real            ::  radius           ! sub-bin radius at 80% RH [um]
   real            ::  dr               ! sea-salt sub-bins radius width [um]
   real            ::  fac              ! factor chosen as ratio of particle
                                        ! radius at RH=80% to dry radius
   real            ::  w10m, src, src2
   real            ::  rDry, aFac, bFac
   real :: qmax, qmin
   real :: sst, wgt                     ! temp adj (Jaegle et al., 2011)



!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_SS
   nend  = w_c%reg%j_SS

!  Choice of swelling factor based on value of rhFlag
!  0 = no swelling
!  1 = Fitzgerald Parameterization
!  2 = Gerber Parameterization
   fac = 1.
   if(gcSS%rhFlag .eq. 1) fac = 2.00
   if(gcSS%rhFlag .eq. 2) fac = 1.65

!  Loop over the number of sea-salt super-bins
   do n = 1, nbins

    emis = 0.0
    if( associated(SS_emis(n)%data2d) ) SS_emis(n)%data2d = 0.0

!   Define the upper and lower radii of the super-bin and the number of sub-bins
!   defined at 80% RH
    rLow   = fac * gcSS%rLow(n)
    rUp    = fac * gcSS%rUp(n)
    nr     = 10
    dr     = (rUp-rLow)/nr
    radius = rLow + 0.5*dr

!   Loop over the sub-bins and add mass
!   src is the accumulated dry salt mass in each super-bin divided 
!   by w10m**3.41.  At each sub-bin this is the product of the
!   number flux into the bin (dF/dr * dr) and the mass of the dry particle.
    src = 0.0
    do ir = 1, nr
     rDry = radius/fac
!    Gong 2003
     aFac = 4.7*(1.+30.*radius)**(-0.017*radius**(-1.44))
     bFac = (0.433-log10(radius))/0.433
     src =   src &
          + 4./3.*pi*gcSS%rhop(n)*rDry**3.*1.e-18 &
           *1.373*radius**(-aFac)*(1.+0.057*radius**3.45) &
           *10**(1.607*exp(-bFac**2.))*dr
!    Gong 1997
!     aFac = 3.
!     bFac = (0.380-log10(radius))/0.65
!     src =   src &
!          + 4./3.*pi*gcSS%rhop(n)*rDry**3.*1.e-18 &
!           *1.373*radius**(-aFac)*(1.+0.057*radius**1.05) &
!           *10**(1.19*exp(-bFac**2.))*dr
     radius = radius+dr
    end do

!   Loop over the horizontal grid
    do j = j1, j2
     do i = i1, i2

      if ( oro(i,j) /= OCEAN ) cycle ! only over OCEAN gridpoints

      w10m = sqrt(u10m(i,j)**2.+v10m(i,j)**2.)

!! 
! Based on a comparison of GEOS-Chem sea salt simulation with coarse mode 
! sea salt mass concentration observations obtained on 6 PMEL cruises, a 
! new SST dependent source function was derived (Jaegle et al., 2011):
      sst = tmpu(i,j,km) - 273.15
      sst = min (30., max(sst, 0.))
      wgt = 0.3 + 0.1*sst - 0.0076*sst*sst + 0.00021*sst*sst*sst
      wgt = min (1.0, wgt)
#ifdef NEMS
      emis(i,j) = wgt*src*w10m**3.41               
#else
      emis(i,j) = src*w10m**3.41
#endif
      w_c%qa(nbeg+n-1)%data3d(i,j,km) = &
            w_c%qa(nbeg+n-1)%data3d(i,j,km) + emis(i,j)*cdt*grav/w_c%delp(i,j,km)
     end do   ! i
    end do    ! j
    if( associated(SS_emis(n)%data2d) ) SS_emis(n)%data2d = emis

   end do     ! n

   rc = 0

   end subroutine SS_Emission

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SS_Wet_Removal - Removal of seasalt by precipitation
!  NOTE: For the removal term, fluxout is the sum of the in-cloud
!        convective and large-scale washout and the total flux across
!        the surface due to below-cloud (rainout) convective and
!        large-scale precipitation reaching the surface.  The fluxout
!        is initialized to zero at the beginning and then at each i, j
!        grid point it is added to.
!        
!
! !INTERFACE:
!

   subroutine SS_Wet_Removal ( i1, i2, j1, j2, km, nbins, cdt, rhoa, gcSS, w_c,&
                               precc, precl, dqcond, tmpu, fluxout, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   type(SS_GridComp), intent(in)   :: gcSS  ! SS Grid Component
   real, pointer, dimension(:,:)   :: precc ! total convective precip [mm day-1]
   real, pointer, dimension(:,:)   :: precl ! total large-scale prec. [mm day-1]
   real, pointer, dimension(:,:,:) :: dqcond  ! change in q due to moist
                                              ! processes [kg kg-1 s-1] 
   real, pointer, dimension(:,:,:) :: tmpu    ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa    ! air density [kg m-3]

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: fluxout(nbins) ! Mass lost by wet dep
                                                      ! to surface, kg/m2/s
   integer, intent(out)             :: rc         ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 
   character(len=*), parameter :: myname = 'SS_Wet_Removal'

! !DESCRIPTION: Updates the dust concentration in each vertical layer
!               due to wet removal
!
! !REVISION HISTORY:
!
!  17Nov2003, Colarco
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer  ::  i, j, k, iit, n, LH, kk, ios
   integer  ::  nbeg, nend
   real :: pdog(i1:i2,j1:j2,km)      ! air mass factor dp/g [kg m-2]
   real :: Td_ls, Td_cv              ! ls and cv timescales [s]
   real :: pls, pcv, pac             ! ls, cv, tot precip [mm day-1]
   real :: qls(km), qcv(km)          ! ls, cv portion dqcond [kg m-3 s-1]
   real :: qmx, qd, A                ! temporary variables on moisture
   real :: F, B, BT                  ! temporary variables on cloud, freq.
   real, allocatable :: fd(:,:)      ! flux across layers [kg m-2]
   real, allocatable :: DC(:)        ! scavenge change in mass mixing ratio

!  Rain parameters (from where?)
   real, parameter :: B0_ls = 1.0e-4
   real, parameter :: F0_ls = 1.0
   real, parameter :: XL_ls = 5.0e-4
   real, parameter :: B0_cv = 1.5e-3
   real, parameter :: F0_cv = 0.3
   real, parameter :: XL_cv = 2.0e-3

   rc = 0

!  Initialize local variables
!  --------------------------
   do n = 1, nbins
    if( associated(fluxout(n)%data2d) ) fluxout(n)%data2d(i1:i2,j1:j2) = 0.0
   end do

   nbeg  = w_c%reg%i_SS
   nend  = w_c%reg%j_SS

!  Allocate the dynamic arrays
   allocate(fd(km,nbins),stat=ios)
   if(ios .ne. 0) stop
   allocate(dc(nbins),stat=ios)
   if(ios .ne. 0) stop

!  Duration of rain: ls = model timestep, cv = 1800 s (<= cdt)
   Td_ls = cdt
#ifdef NEMS
   Td_cv = cdt
#else
   Td_cv = 1800.
#endif

!  Accumulate the 3-dimensional arrays of rhoa and pdog
   pdog = w_c%delp/grav

!  Loop over spatial indices
   do j = j1, j2
    do i = i1, i2

!    Check for total precipitation amount
!    Assume no precip in column if precl+precc = 0
     pac = precl(i,j) + precc(i,j)
     if(pac .le. 0.) goto 100
     pls = precl(i,j)
     pcv = precc(i,j)

!    Initialize the precipitation fields
     qls(:)  = 0.
     qcv(:)  = 0.
     fd(:,:) = 0.

!    Find the highest model layer experiencing rainout.  Assumes no
!    scavenging if T < 258 K
     LH = 0
     do k = 1, km
      if(dqcond(i,j,k) .lt. 0. .and. tmpu(i,j,k) .gt. 258.) then
       LH = k
       goto 15
      endif
     end do
 15  continue
     if(LH .lt. 1) goto 100

!    convert dqcond from kg water/kg air/s to kg water/m3/s and reverse
!    sign so that dqcond < 0. (positive precip) means qls and qcv > 0.
     do k = LH, km
      qls(k) = -dqcond(i,j,k)*pls/pac*rhoa(i,j,k)
!      qcv(k) = -dqcond(i,j,k)*pcv/pac*rhoa(i,j,k)
#ifdef NEMS
      if ( gcSS%doing_scav == 1 ) then
      qcv(k) = -dqcond(i,j,k)*pcv/pac*rhoa(i,j,k)
      endif
#endif
     end do

!    Loop over vertical to do the scavenging!
     do k = LH, km

!-----------------------------------------------------------------------------
!   (1) LARGE-SCALE RAINOUT:             
!       Tracer loss by rainout = TC0 * F * exp(-B*dt)
!         where B = precipitation frequency,
!               F = fraction of grid box covered by precipitating clouds.
!       We assume that tracer scavenged by rain is falling down to the
!       next level, where a fraction could be re-evaporated to gas phase
!       if Qls is less then 0 in that level.
!-----------------------------------------------------------------------------
      if (qls(k) .gt. 0.) then
       F  = F0_ls / (1. + F0_ls*B0_ls*XL_ls/(qls(k)*cdt/Td_ls))
       B  = B0_ls/F0_ls +1./(F0_ls*XL_ls/qls(k)) 
       BT = B * Td_ls
       if (BT.gt.10.) BT = 10.               !< Avoid overflow >
!      Adjust du level:
       do n = 1, nbins
        DC(n) = w_c%qa(nbeg+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
        if (w_c%qa(nbeg+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(nbeg+n-1)%data3d(i,j,k) = 1.0E-32
       end do
!      Flux down:  unit is kg m-2
!      Formulated in terms of production in the layer.  In the revaporation step
!      we consider possibly adding flux from above...
       do n = 1, nbins
        Fd(k,n) = DC(n)*pdog(i,j,k)
       end do

      end if                                    ! if Qls > 0  >>>

!-----------------------------------------------------------------------------
! * (2) LARGE-SCALE WASHOUT:
! *     Occurs when rain at this level is less than above.
!-----------------------------------------------------------------------------
      if(k .gt. LH .and. qls(k) .ge. 0.) then
       if(qls(k) .lt. qls(k-1)) then
!       Find a maximum F overhead until the level where Qls<0.
        Qmx   = 0.
        do kk = k-1,LH,-1
         if (Qls(kk).gt.0.) then
          Qmx = max(Qmx,Qls(kk))
         else
          goto 333
         end if
        end do

 333    continue
        F = F0_ls / (1. + F0_ls*B0_ls*XL_ls/(Qmx*cdt/Td_ls))
        if (F.lt.0.01) F = 0.01
!-----------------------------------------------------------------------------
!  The following is to convert Q(k) from kgH2O/m3/sec to mm/sec in order
!  to use the Harvard formula.  Convert back to mixing ratio by multiplying
!  by rhoa.  Multiply by pdog gives kg/m2/s of precip.  Divide by density
!  of water (=1000 kg/m3) gives m/s of precip and multiply by 1000 gives
!  units of mm/s (omit the multiply and divide by 1000).
!-----------------------------------------------------------------------------

        Qd = Qmx /rhoa(i,j,k)*pdog(i,j,k)
        if (Qd.ge.50.) then
         B = 0.
        else
         B = Qd * 0.1
        end if
        BT = B * cdt
        if (BT.gt.10.) BT = 10.

!       Adjust du level:
        do n = 1, nbins
         DC(n) = w_c%qa(nbeg+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
         if (w_c%qa(nbeg+n-1)%data3d(i,j,k) .lt. 1.0E-32) & 
          w_c%qa(nbeg+n-1)%data3d(i,j,k) = 1.0E-32
         if( associated(fluxout(n)%data2d) ) then
          fluxout(n)%data2d(i,j) = fluxout(n)%data2d(i,j)+DC(n)*pdog(i,j,k)/cdt
         endif
        end do

       end if
      end if                                    ! if ls washout  >>>

!-----------------------------------------------------------------------------
!  (3) CONVECTIVE RAINOUT:
!      Tracer loss by rainout = dd0 * F * exp(-B*dt)
!        where B = precipitation frequency,
!              F = fraction of grid box covered by precipitating clouds.
!-----------------------------------------------------------------------------

      if (qcv(k) .gt. 0.) then
       F  = F0_cv / (1. + F0_cv*B0_cv*XL_cv/(Qcv(k)*cdt/Td_cv))
       B  = B0_cv
       BT = B * Td_cv
       if (BT.gt.10.) BT = 10.               !< Avoid overflow >

!      Adjust du level: 
       do n = 1, nbins
        DC(n) = w_c%qa(nbeg+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
        if (w_c%qa(nbeg+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(nbeg+n-1)%data3d(i,j,k) = 1.0E-32
       end do

!------  Flux down:  unit is kg. Including both ls and cv.
       do n = 1, nbins
        Fd(k,n) = Fd(k,n) + DC(n)*pdog(i,j,k)
       end do

      end if                                  ! if Qcv > 0   >>>

!-----------------------------------------------------------------------------
!  (4) CONVECTIVE WASHOUT:
!      Occurs when rain at this level is less than above.
!-----------------------------------------------------------------------------

      if (k.gt.LH .and. Qcv(k).ge.0.) then
       if (Qcv(k).lt.Qcv(k-1)) then
!-----  Find a maximum F overhead until the level where Qls<0.
        Qmx   = 0.
        do kk = k-1, LH, -1
         if (Qcv(kk).gt.0.) then
          Qmx = max(Qmx,Qcv(kk))
         else
          goto 444
         end if
        end do

 444    continue
        F = F0_cv / (1. + F0_cv*B0_cv*XL_cv/(Qmx*cdt/Td_cv))
        if (F.lt.0.01) F = 0.01
!-----------------------------------------------------------------------------
!  The following is to convert Q(k) from kgH2O/m3/sec to mm/sec in order
!  to use the Harvard formula.  Convert back to mixing ratio by multiplying
!  by rhoa.  Multiply by pdog gives kg/m2/s of precip.  Divide by density
!  of water (=1000 kg/m3) gives m/s of precip and multiply by 1000 gives
!  units of mm/s (omit the multiply and divide by 1000).
!-----------------------------------------------------------------------------

        Qd = Qmx / rhoa(i,j,k)*pdog(i,j,k)
        if (Qd.ge.50.) then
         B = 0.
        else
         B = Qd * 0.1
        end if
        BT = B * cdt
        if (BT.gt.10.) BT = 10.

!       Adjust du level:
        do n = 1, nbins
         DC(n) = w_c%qa(nbeg+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
         if (w_c%qa(nbeg+n-1)%data3d(i,j,k) .lt. 1.0E-32) & 
          w_c%qa(nbeg+n-1)%data3d(i,j,k) = 1.0E-32
         if( associated(fluxout(n)%data2d) ) then
          fluxout(n)%data2d(i,j) = fluxout(n)%data2d(i,j)+DC(n)*pdog(i,j,k)/cdt
         endif
        end do

       end if
      end if                                    ! if cv washout  >>>

!-----------------------------------------------------------------------------
!  (5) RE-EVAPORATION.  Assume that SO2 is re-evaporated as SO4 since it
!      has been oxidized by H2O2 at the level above. 
!-----------------------------------------------------------------------------
!     Add in the flux from above, which will be subtracted if reevaporation occurs
      if(k .gt. LH) then
       do n = 1, nbins
        Fd(k,n) = Fd(k,n) + Fd(k-1,n)
       end do

!      Is there evaporation in the currect layer?
       if (-dqcond(i,j,k) .lt. 0.) then
!       Fraction evaporated = H2O(k)evap / H2O(next condensation level).
        if (-dqcond(i,j,k-1) .gt. 0.) then

          A =  abs(  dqcond(i,j,k) * pdog(i,j,k)    &
            /      ( dqcond(i,j,k-1) * pdog(i,j,k-1))  )
          if (A .gt. 1.) A = 1.

!         Adjust tracer in the level
          do n = 1, nbins
           DC(n) =  Fd(k-1,n) / pdog(i,j,k) * A
           w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k) + DC(n)
           w_c%qa(nbeg+n-1)%data3d(i,j,k) = max(w_c%qa(nbeg+n-1)%data3d(i,j,k),1.e-32)
!          Adjust the flux out of the bottom of the layer
           Fd(k,n)  = Fd(k,n) - DC(n)*pdog(i,j,k)
          end do

        endif
       endif                                   ! if -moistq < 0
      endif
     end do  ! k

     do n = 1, nbins
      if( associated(fluxout(n)%data2d) ) then
       fluxout(n)%data2d(i,j) = fluxout(n)%data2d(i,j)+Fd(km,n)/cdt
      endif
     end do

 100 continue
    end do   ! i
   end do    ! j

   deallocate(fd,DC,stat=ios)

   end subroutine SS_Wet_Removal

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SS_Compute_Diags - Calculate dust 2D diagnostics
!
! !INTERFACE:
!

   subroutine SS_Compute_Diags ( i1, i2, j1, j2, km, nbins, gcSS, w_c, tmpu, rhoa, &
                                 sfcmass, colmass, mass, exttau, scatau, &
                                 sfcmass25, colmass25, mass25, exttau25, scatau25, &
                                 rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   type(SS_GridComp), intent(inout):: gcSS     ! SS Grid Component
   type(Chem_Bundle), intent(in)   :: w_c      ! Chem Bundle
   real, pointer, dimension(:,:,:) :: tmpu     ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa     ! air density [kg m-3]

! !OUTPUT PARAMETERS:
   type(Chem_Array), intent(inout)  :: sfcmass ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: colmass ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: mass    ! 3d mass mixing ratio kg/kg
   type(Chem_Array), intent(inout)  :: exttau  ! ext. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: scatau  ! sct. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: sfcmass25 ! sfc mass concentration kg/m3 (pm2.5)
   type(Chem_Array), intent(inout)  :: colmass25 ! col mass density kg/m2 (pm2.5)
   type(Chem_Array), intent(inout)  :: mass25    ! 3d mass mixing ratio kg/kg (pm2.5)
   type(Chem_Array), intent(inout)  :: exttau25  ! ext. AOT at 550 nm (pm2.5)
   type(Chem_Array), intent(inout)  :: scatau25  ! sct. AOT at 550 nm (pm2.5)
   integer, intent(out)             :: rc      ! Error return code:
                                               !  0 - all is well
                                               !  1 - 

! !DESCRIPTION: Calculates some simple 2d diagnostics from the SS fields
!               Surface concentration (dry)
!               Column mass load (dry)
!               Extinction aot 550 (wet)
!               Scattering aot 550 (wet)
!               For the moment, this is hardwired.
!
! !REVISION HISTORY:
!
!  16APR2004, Colarco
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   character(len=*), parameter :: myname = 'SS_Compute_Diags'
   integer :: i, j, k, n, nbeg, nend, ios, nch, idx
   real :: tau, ssa
   real :: fPM25(nbins)  ! fraction of bin with particles diameter < 2.5 um
   character(len=255) :: qname


!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_SS
   nend  = w_c%reg%j_SS
   nch   = gcSS%mie_tables%nch

!  Compute the PM2.5 bin-wise fractions
!  ------------------------------------
   do n = 1, nbins
    if(gcSS%rup(n) < 1.25) then
     fPM25(n) = 1.
    else
     if(gcSS%rlow(n) < 1.25) then
!     Assume dm/dlnr = constant, i.e., dm/dr ~ 1/r
      fPM25(n) = log(1.25/gcSS%rlow(n)) / log(gcSS%rup(n)/gcSS%rlow(n))
     else
      fPM25(n) = 0.
     endif
    endif
   enddo


!  Calculate the diagnostic variables if requested
!  -----------------------------------------------

!  Calculate the surface mass concentration
   if( associated(sfcmass%data2d) ) then
      sfcmass%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
         sfcmass%data2d(i1:i2,j1:j2) &
              =   sfcmass%data2d(i1:i2,j1:j2) &
              + w_c%qa(n+nbeg-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)
      end do
   endif
   if( associated(sfcmass25%data2d) ) then
      sfcmass25%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
         sfcmass25%data2d(i1:i2,j1:j2) &
              =   sfcmass25%data2d(i1:i2,j1:j2) &
              + w_c%qa(n+nbeg-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)*fPM25(n)
      end do
   endif

!  Calculate the seasalt column loading
   if( associated(colmass%data2d) ) then
      colmass%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
       do k = 1, km
        colmass%data2d(i1:i2,j1:j2) &
         =   colmass%data2d(i1:i2,j1:j2) &
           + w_c%qa(n+nbeg-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
       end do
      end do
   endif
   if( associated(colmass25%data2d)) then
      colmass25%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
       do k = 1, km
        colmass25%data2d(i1:i2,j1:j2) &
         =   colmass25%data2d(i1:i2,j1:j2) &
           + w_c%qa(n+nbeg-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav*fPM25(n)
       end do
      end do
   endif

!  Calculate the total mass mixing ratio
   if( associated(mass%data3d) ) then
      mass%data3d(i1:i2,j1:j2,1:km) = 0.
      do n = 1, nbins
       mass%data3d(i1:i2,j1:j2,1:km) &
         =   mass%data3d(i1:i2,j1:j2,1:km) &
           + w_c%qa(n+nbeg-1)%data3d(i1:i2,j1:j2,1:km)
      end do
   endif
   if( associated(mass25%data3d) ) then
      mass25%data3d(i1:i2,j1:j2,1:km) = 0.
      do n = 1, nbins
       mass25%data3d(i1:i2,j1:j2,1:km) &
         =   mass25%data3d(i1:i2,j1:j2,1:km) &
           + w_c%qa(n+nbeg-1)%data3d(i1:i2,j1:j2,1:km)*fPM25(n)
      end do
   endif

!  Calculate the extinction and/or scattering AOD
   if( associated(exttau%data2d) .or. associated(scatau%data2d) ) then

      if( associated(exttau%data2d)) exttau%data2d(i1:i2,j1:j2) = 0.
      if( associated(scatau%data2d)) scatau%data2d(i1:i2,j1:j2) = 0.

      if( associated(exttau25%data2d)) exttau25%data2d(i1:i2,j1:j2) = 0.
      if( associated(scatau25%data2d)) scatau25%data2d(i1:i2,j1:j2) = 0.

      do n = 1, nbins

!      Select the name for species
       qname = trim(w_c%reg%vname(w_c%reg%i_SS+n-1))
       idx = Chem_MieQueryIdx(gcSS%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

!      Recall -- at present need to divide RH by 100 to get to tables
       do k = 1, km
        do j = j1, j2
         do i = i1, i2
          call Chem_MieQuery(gcSS%mie_tables, idx, 1., &
              w_c%qa(nbeg+n-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
              w_c%rh(i,j,k)/100., tau=tau, ssa=ssa)

!         Integrate in the vertical
          if( associated(exttau%data2d) ) exttau%data2d(i,j) = exttau%data2d(i,j) + tau
          if( associated(exttau25%data2d)) &
                         exttau25%data2d(i,j) = exttau25%data2d(i,j) + tau*fPM25(n)
          if( associated(scatau%data2d) ) scatau%data2d(i,j) = scatau%data2d(i,j) + tau*ssa
          if( associated(scatau25%data2d) ) &
                         scatau25%data2d(i,j) = scatau25%data2d(i,j) + tau*ssa*fPM25(n)


         enddo
        enddo
       enddo

      enddo  ! nbins

   endif


   rc = 0

   end subroutine SS_Compute_Diags

 end subroutine SS_GridCompRun

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SS_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine SS_GridCompFinalize ( gcSS, w_c, impChem, expChem, &
                                    nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(SS_GridComp), intent(inout) :: gcSS   ! Grid Component

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

   character(len=*), parameter :: myname = 'SS_GridCompFinalize'

   rc = 0

   return

 end subroutine SS_GridCompFinalize

 end module SS_GridCompMod

