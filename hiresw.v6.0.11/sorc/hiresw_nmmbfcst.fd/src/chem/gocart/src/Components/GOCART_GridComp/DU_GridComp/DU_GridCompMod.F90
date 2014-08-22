#ifdef GEOS5
#include "MAPL_Generic.h"
#endif

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  DU_GridCompMod --- DU Grid Component Class
!
! !INTERFACE:
!

   module  DU_GridCompMod

! !USES:

#ifdef GEOS5
   USE ESMF_Mod
   USE MAPL_Mod
#endif

   use Chem_Mod              ! Chemistry Base Class
   use Chem_StateMod         ! Chemistry State
   use Chem_SettlingMod      ! Settling
   use Chem_DepositionMod    ! Aerosol Deposition
   use Chem_ConstMod, only: grav, von_karman, cpd, &
                            undefval => undef         ! Constants !
   use Chem_UtilMod          ! I/O
   use Chem_MieMod           ! Aerosol LU Tables, calculator
   use m_inpak90             ! Resource file management
   use m_die, only: die
   use m_mpout

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  DU_GridComp       ! The DU object 

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  DU_GridCompInitialize
   PUBLIC  DU_GridCompRun
   PUBLIC  DU_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the (pre-ESMF) DU Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  16Aug2005 da Silva  Passed ESMF grid to MPread().
!  22Sep2011 Lu        Add NEMS option
!
!EOP
!-------------------------------------------------------------------------

  type DU_GridComp
        character(len=255) :: name
        type(Chem_Mie), pointer :: mie_tables  ! aod LUTs
        integer       :: rhFlag
        integer       :: doing_scav     ! compute tracer scavenging
        real, pointer :: src(:,:)       ! Ginoux dust sources
        real, pointer :: radius(:)      ! particle effective radius [um]
        real, pointer :: rlow(:)        ! particle effective radius lower bound [um]
        real, pointer :: rup(:)         ! particle effective radius upper bound [um]
        real, pointer :: sfrac(:)       ! fraction of total source
        real, pointer :: rhop(:)        ! soil class density [kg m-3]
        integer :: nymd
        character(len=255) :: srcfilen
  end type DU_GridComp

  real, parameter :: OCEAN=0.0, LAND = 1.0, SEA_ICE = 2.0

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  DU_GridCompInitialize --- Initialize DU_GridComp
!
! !INTERFACE:
!

   subroutine DU_GridCompInitialize ( gcDU, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real, intent(in) :: cdt                     ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(DU_GridComp), intent(inout) :: gcDU   ! Grid Component
   type(ESMF_State), intent(inout)  :: impChem  ! Import State
   type(ESMF_State), intent(inout)  :: expChem  ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the DU Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'DU_GridCompInitialize'


   character(len=255) :: rcfilen = 'DU_GridComp.rc'
   integer :: ios, n
   integer :: i1, i2, im, j1, j2, jm, nbins, n1, n2, nbins_rc
   integer, allocatable :: ier(:)
   real, allocatable :: buffer(:,:)
   real :: qmax, qmin
   real :: radius, rlow, rup, rmrat, rmin, rhop, fscav
   integer :: irhFlag
   integer :: idoing_scav  ! option to re-activate convective removal 
   character(len=255) :: CARMA_Services = ' '


   gcDU%name = 'DU Constituent Package'

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   nbins = w_c%reg%n_DU
   n1  = w_c%reg%i_DU
   n2  = w_c%reg%j_DU

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

   call i90_label ( 'number_dust_bins:', ier(1) )
   nbins_rc = i90_gint ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(20)
      return
   end if
   if ( nbins_rc /= nbins ) then
      call final_(25)
      return
   end if


!  Dust source file name
!  ---------------------
   call i90_label ( 'ginoux_dust_source_filename:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcDU%srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

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
      gcDU%radius(n)   = radius
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
      gcDU%rlow(n)        = rlow
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
      gcDU%rup(n)        = rup
      w_c%qa(n1+n-1)%rup = rup * 1.e-6  ! save radius in [m]
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Source fraction
!  ---------------
   call i90_label ( 'source_fraction:', ier(1) )
   do n = 1, nbins
      gcDU%sfrac(n) = i90_gfloat ( ier(n+1) )
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Soil Density
!  ---------------
   call i90_label ( 'soil_density:', ier(1) )
   do n = 1, nbins
      rhop                = i90_gfloat ( ier(n+1) )
      gcDU%rhop(n)        = rhop
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

!  Particle affected by relative humidity?
!  ---------------
   call i90_label ( 'rhFlag:', ier(1) )
   irhFlag                    = i90_gint ( ier(2) )
   w_c%qa(n1+n-1)%irhFlag     = irhFlag
   gcDU%rhFlag                = irhFlag
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if

!                          -------
!  Option to compute convective rainout/washout in GOCART
!  ---------------

   gcDU%doing_scav = 0     ! Default is to compute convective
!                          ! rainout/washout in GFS physics
#ifdef NEMS
   call i90_label ( 'doing_scav:', ier(1) )
   idoing_scav                 = i90_gint ( ier(2) )
   gcDU%doing_scav             = idoing_scav
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if

!  invoke the option to compute convective removal in GOCART
!  set fscav (scav used in GFS RAS) to 0.    
   if ( gcDU%doing_scav == 1 ) then
     do n = 1, nbins
      w_c%reg%fscav(n1+n-1)   = 0.
      w_c%qa(n1+n-1)%fscav    = 0.
     end do
   endif
#endif


!  Initialize date for BCs
!  -----------------------
   gcDU%nymd = -1   ! nothing read yet

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
        call final_(60)
        return
   endif

!  Select fields to be produced in the export state
!  ------------------------------------------------
   n = nbins

!  Emission Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iDUEM001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iDUEM002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iDUEM003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iDUEM004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iDUEM005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iDUEM006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iDUEM007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iDUEM008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif

!  Sedimentation Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iDUSD001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iDUSD002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iDUSD003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iDUSD004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iDUSD005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iDUSD006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iDUSD007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iDUSD008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif

!  Dry Deposition Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iDUDP001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iDUDP002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iDUDP003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iDUDP004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iDUDP005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iDUDP006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iDUDP007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iDUDP008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif

!  Wet Deposition Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iDUWT001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iDUWT002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iDUWT003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iDUWT004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iDUWT005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iDUWT006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iDUWT007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iDUWT008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif


!  Other diagnostics
   call Chem_StateSetNeeded ( expChem, iDUSMASS, .true., ier(1) )
   call Chem_StateSetNeeded ( expChem, iDUCMASS, .true., ier(2) )
   call Chem_StateSetNeeded ( expChem, iDUMASS,  .true., ier(3) )
   call Chem_StateSetNeeded ( expChem, iDUEXTTAU, .true., ier(4) )
   call Chem_StateSetNeeded ( expChem, iDUSCATAU, .true., ier(5) )
   call Chem_StateSetNeeded ( expChem, iDUAERIDX, .true., ier(6) )
   call Chem_StateSetNeeded ( expChem, iDUSM25, .true., ier(7) )
   call Chem_StateSetNeeded ( expChem, iDUCM25, .true., ier(8) )
   call Chem_StateSetNeeded ( expChem, iDUMASS25,  .true., ier(9) )
   call Chem_StateSetNeeded ( expChem, iDUEXTT25, .true., ier(10) )
   call Chem_StateSetNeeded ( expChem, iDUSCAT25, .true., ier(11) )

   if ( any(ier(1:11) /= 0) ) then
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
   allocate ( gcDU%radius(nbins), gcDU%src(i1:i2,j1:j2), &
              gcDU%rlow(nbins), gcDU%rup(nbins), &
              gcDU%sfrac(nbins), gcDU%rhop(nbins), ier(nerr), &
              stat=ios )
   if ( ios /= 0 ) rc = 100
   end subroutine init_

   subroutine final_(ierr)
   integer :: ierr
   integer ios
   deallocate ( gcDU%radius, gcDU%src, gcDU%sfrac, gcDU%rhop, &
                gcDU%rlow, gcDU%rup, ier, stat=ios )
   call i90_release()
   rc = ierr
   end subroutine final_

   end subroutine DU_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  DU_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine DU_GridCompRun ( gcDU, w_c, impChem, expChem, &
                               nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(DU_GridComp), intent(inout) :: gcDU   ! Grid Component
   type(Chem_Bundle), intent(inout) :: w_c    ! Chemical tracer fields   

! !INPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: impChem    ! Import State
   integer, intent(in) :: nymd, nhms          ! time
   real, intent(in) :: cdt                    ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: expChem   ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine implements the so-called DU Driver. That 
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

   character(len=*), parameter :: myname = 'DU_GridCompRun'
   character(len=*), parameter :: Iam = myname
   integer :: ier(32), idiag
   integer :: i1, i2, im, j1, j2, jm, nbins, n1, n2, km, n, ios
   integer :: i, j, k, nymd1, nhms1, ijl, ijkl
   real :: qmax, qmin
   real, pointer :: DU_radius(:), DU_rhop(:)


!  Input fields from fvGCM
!  -----------------------
   real, pointer, dimension(:,:)   ::  fraclake, gwettop, oro, u10m, v10m, &
                                       xlai, ustar, precc, precl, pblh, &
                                       shflux, z0h, hsurf
   real, pointer, dimension(:,:,:) ::  dqcond, tmpu, rhoa, u, v, hghte


#ifdef GEOS5 

#define EXPORT     expChem

#define ptrDUWT       DU_wet
#define ptrDUEM       DU_emis
#define ptrDUDP       DU_dep
#define ptrDUSD       DU_set

#define    DUSMASS    DU_sfcmass
#define    DUCMASS    DU_colmass
#define    DUMASS     DU_mass
#define    DUEXTTAU   DU_exttau
#define    DUSCATAU   DU_scatau
#define    DUSMASS25  DU_sfcmass25
#define    DUCMASS25  DU_colmass25
#define    DUMASS25   DU_mass25
#define    DUEXTT25   DU_exttau25
#define    DUSCAT25   DU_scatau25
#define    DUAERIDX   DU_aeridx

   integer :: STATUS

#include "DU_GetPointer___.h"

#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Quantities to be exported
!  -------------------------
   type(Chem_Array), pointer :: DU_emis(:), DU_set(:), DU_dep(:), DU_wet(:), &
                                DU_sfcmass, DU_colmass, DU_mass, DU_exttau, &
                                DU_scatau, DU_aeridx, &
                                DU_sfcmass25, DU_colmass25, DU_mass25, &
                                DU_exttau25,  DU_scatau25

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

   

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm

   km = w_c%grid%km
   nbins = w_c%reg%n_DU
   n1    = w_c%reg%i_DU
   n2    = w_c%reg%j_DU

   ijl  = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   ijkl = ijl * km

#ifdef GEOS5
   if ( nbins /= NBIN_DUEM .OR. nbins /= NBIN_DUWT .OR. &
        nbins /= NBIN_DUDP .OR. nbins /= NBIN_DUSD ) then
      call die(myname,'inconsistent bins in resource file and registry')
   endif
#endif


! Update emissions/production if necessary (daily)
!  ------------------------------------------
   if(gcDU%nymd < 0) then

!   The dust file is time invariant, and currently hard set
    nymd1 = 19710605
    nhms1 = 0
    call Chem_UtilMPread ( gcDU%srcfilen, 'du_src', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0,       &
                           var2d=gcDU%src, grid = w_c%grid_esmf   )

!   As a safety check, where du_src is undefined set to 0
!   -----------------------------------------------------
    do j = j1, j2
     do i = i1, i2
      if(1.01*gcDU%src(i,j) .gt. undefval) gcDU%src(i,j) = 0.
     enddo
    enddo

#ifdef DEBUG
    call pmaxmin('DU: src', gcDU%src, qmin, qmax, ijl, 1, 1. )
#endif

    gcDU%nymd = nymd

   endif

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Work space for holding dust output
!  ----------------------------------
   allocate ( DU_emis(nbins), DU_set(nbins), DU_dep(nbins),  DU_wet(nbins), &
              DU_sfcmass, DU_colmass, DU_mass, DU_exttau, DU_scatau, &
              DU_sfcmass25, DU_colmass25, DU_mass25, DU_exttau25, & 
              DU_scatau25, DU_aeridx, stat = ios )
   if ( ios /= 0 ) then
      rc = 1
      return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!  Dust particle radius [m] and density [kg m-3]
!  ---------------------------------------------
   allocate( DU_radius(nbins), DU_rhop(nbins) )
   DU_radius = 1.e-6*gcDU%radius
   DU_rhop   = gcDU%rhop


#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_beg', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                   ijl, km, 1. )
   end do
#endif

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
   hsurf => hghte(i1:i2,j1:j2,km) ! Recall: GEOS-5 has edges with k in [0,km]
    

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
   call Chem_StateGetArray2D ( impChem, iHSURF,    hsurf,    ier(13) )

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

   call pmaxmin('DU: fraclake   ', fraclake, qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: gwtop      ', gwettop , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: oro        ', oro     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: u10m       ', u10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: v10m       ', v10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: xlai       ', xlai    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: ustar      ', ustar   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: precc      ', precc   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: precl      ', precl   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: pblh       ', pblh    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: shfflux    ', shflux  , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: z0h        ', z0h     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: hsurf      ', hsurf   , qmin, qmax, ijl,1, 1. )

   call pmaxmin('DU: dqcond     ', dqcond  , qmin, qmax, ijl,km, 1. )
   call pmaxmin('DU: tmpu       ', tmpu    , qmin, qmax, ijl,km, 1. )
   call pmaxmin('DU: rhoa       ', rhoa    , qmin, qmax, ijl,km, 1. )
   call pmaxmin('DU: u          ', u       , qmin, qmax, ijl,km, 1. )
   call pmaxmin('DU: v          ', v       , qmin, qmax, ijl,km, 1. )
   call pmaxmin('DU: hghte      ', hghte   , qmin, qmax, ijl,km, 1. )
   call pmaxmin('DU: rh         ', w_c%rh  , qmin, qmax, ijl,km, 1. )

#endif

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Get pointers to export state
!  ----------------------------
   do n = 1, nbins
      idiag = iDUEM001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, DU_emis(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iDUSD001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, DU_set(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iDUDP001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, DU_dep(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iDUWT001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, DU_wet(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   idiag = iDUSMASS
   call Chem_StateGetArray2D ( expChem, idiag, DU_sfcmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUCMASS
   call Chem_StateGetArray2D ( expChem, idiag, DU_colmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUMASS
   call Chem_StateGetArray3D ( expChem, idiag, DU_mass%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUEXTTAU
   call Chem_StateGetArray2D ( expChem, idiag, DU_exttau%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUSCATAU
   call Chem_StateGetArray2D ( expChem, idiag, DU_scatau%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUSM25
   call Chem_StateGetArray2D ( expChem, idiag, DU_sfcmass25%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUCM25
   call Chem_StateGetArray2D ( expChem, idiag, DU_colmass25%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUMASS25
   call Chem_StateGetArray3D ( expChem, idiag, DU_mass25%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUEXTT25
   call Chem_StateGetArray2D ( expChem, idiag, DU_exttau25%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUSCAT25
   call Chem_StateGetArray2D ( expChem, idiag, DU_scatau25%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iDUAERIDX
   call Chem_StateGetArray2D ( expChem, idiag, DU_aeridx%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif


!  Dust Source
!  -----------
   call DU_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcDU, w_c, &
                      fraclake, gwettop, oro, u10m, v10m, DU_emis, rc )

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_emi', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Dust Settling
!  -----------
!  CARMA potentially provides this service.  Check to see.
   if( index(w_c%qa(n1)%wantServices,":CARMA_Sedimentation:") .eq. 0) then
     call Chem_Settling ( i1, i2, j1, j2, km, n1, n2, nbins, gcDU%rhFlag, &
                          DU_radius, DU_rhop, cdt, w_c, tmpu, rhoa, hsurf,    &
                          hghte, DU_set, rc )
   endif

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_set', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Dust Deposition
!  -----------
   call Chem_Deposition( i1, i2, j1, j2, km, n1, n2, nbins, cdt, w_c, &
                         DU_radius, DU_rhop, tmpu, rhoa, hsurf, hghte, oro, ustar, &
                         u10m, v10m, fraclake, gwettop, pblh, shflux, z0h, DU_dep, rc )

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_dry', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Dust Wet Removal
!  -----------
   call DU_Wet_Removal ( i1, i2, j1, j2, km, nbins, cdt, rhoa, gcDU, w_c, &
                         precc, precl, dqcond, tmpu, DU_wet, rc )


#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_wet', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Compute the desired output diagnostics here
!  Ideally this will go where chemout is called in fvgcm.F since that
!  will reflect the distributions after transport, etc.
!  -----------
   call DU_Compute_Diags(i1, i2, j1, j2, km, nbins, gcDU, w_c, tmpu, rhoa,    &
                         DU_sfcmass,  DU_colmass,   DU_mass, DU_exttau,       &
                         DU_scatau,   DU_sfcmass25, DU_colmass25, DU_mass25,  &
                         DU_exttau25, DU_scatau25,  DU_aeridx, rc)

!  Clean up
!  --------
   deallocate ( DU_radius, DU_rhop, stat=ios )

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

   deallocate (  DU_emis, DU_set, DU_dep,  DU_wet, &
                 DU_sfcmass, DU_colmass, DU_mass, DU_exttau, DU_scatau, &
                 DU_sfcmass25, DU_colmass25, DU_mass25, DU_exttau25,    & 
                 DU_scatau25, DU_aeridx, stat=ios )

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

   return

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  DU_Emission - Adds Dust emission for one timestep
!
! !INTERFACE:
!

   subroutine DU_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcDU, w_c, &
                            fraclake, gwettop, oro, u10m, v10m, DU_emis, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   type(DU_GridComp), intent(in)    :: gcDU       ! DU Grid Component
   real, pointer, dimension(:,:) :: fraclake, gwettop, oro, u10m, v10m

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: DU_emis(nbins) ! Dust emissions, kg/m2/s
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'DU_Emission'

! !DESCRIPTION: Updates the dust concentration with emissions every timestep
!
! !REVISION HISTORY:
!
!  06Nov2003, Colarco
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer  ::  i, j, k, m, n, ios
   integer  ::  n1, n2
   real     ::  emis(i1:i2,j1:j2)       ! Local bin emission
   real, parameter ::  air_dens = 1.25  ! Air density = 1.25 kg m-3
   real            ::  DU_diameter      ! dust effective diameter [m]
   real            ::  DU_rhop          ! density of class [kg m-3]
   real, allocatable ::  u_thresh0(:)
   real            ::  u_thresh
   real            ::  w10m
   real            ::  w10mSave, DU_emisSave
   real :: qmax, qmin


!  Dust emission tuning coefficient. Surface wind speeds vary from
!  GEOS-4 to GEOS-5.  Adjust this coefficient to give reasonably
!  similar dust emissions in both.  In principle this number should
!  probably come from a resource file
#ifdef NEMS
   real, parameter ::  Ch_DU = 0.625e-9  ! Model dependent coefficient for dust
                                         ! emissions [kg s2 m-5]
#elif defined GEOS5
   real, parameter ::  Ch_DU = 0.175e-9  ! Model dependent coefficient for dust
                                         ! emissions [kg s2 m-5]
#else
   real, parameter ::  Ch_DU = 0.375e-9  ! Model dependent coefficient for dust
                                         ! emissions [kg s2 m-5]
#endif


!  Initialize local variables
!  --------------------------
   n1  = w_c%reg%i_DU
   n2  = w_c%reg%j_DU

!  Establish the threshold wind speed for each bin
   allocate(u_thresh0(nbins),stat=ios)

!  Loop over the number of dust bins
   do n = 1, nbins

    emis = 0.0
    if( associated(DU_emis(n)%data2d) ) DU_emis(n)%data2d(i1:i2,j1:j2) = 0.0

!   Calculate the threshold velocity of wind erosion [m/s] for each radius
!   for a dry soil, as in Marticorena et al. [1997].
!   The parameterization includes the air density which is assumed 
!   = 1.25 kg m-3 to speed the calculation.  The error in air density is
!   small compared to errors in other parameters.

!   Temporary: dust radius [micron] to diameter [m]
    DU_diameter = 2.e-6*gcDU%radius(n)
    DU_rhop     = gcDU%rhop(n)
    u_thresh0(n) = 0.13 * sqrt(DU_rhop*grav*DU_diameter/air_dens) &
                        * sqrt(1.+6.e-7/(DU_rhop*grav*DU_diameter**2.5)) &
            / sqrt(1.928*(1331.*(100.*DU_diameter)**1.56+0.38)**0.092 - 1.)

!   Loop over the horizontal grid
    do j = j1, j2
     do i = i1, i2

      if ( oro(i,j) /= LAND ) cycle ! only over LAND gridpoints

      w10m = sqrt(u10m(i,j)**2.+v10m(i,j)**2.)

!     This should give emissions equal to about 200 Tg month-1
!      if(gcDU%src(i,j) .lt. 1.) then
!       DU_emis(n)%data2d(i,j) = &
!           4.3064e-8*gcDU%sfrac(n)*gcDU%src(i,j)
!       w_c%qa(n1+n-1)%data3d(i,j,km) =  w_c%qa(n1+n-1)%data3d(i,j,km) &
!                         + DU_emis(n)%data2d(i,j)*cdt*grav/w_c%delp(i,j,km)
!      endif

!     Modify the threshold depending on soil moisture as in Ginoux et al. [2001]
      if(gwettop(i,j) .lt. 0.5) then
       u_thresh = amax1(0.,u_thresh0(n)* &
        (1.2+0.2*alog10(amax1(1.e-3,gwettop(i,j)))))

       if(w10m .gt. u_thresh) then     
!       Emission of dust [kg m-2 s-1]
        emis(i,j) = &
            Ch_DU*(1.-fraclake(i,j))*gcDU%sfrac(n)*gcDU%src(i,j) &
          * w10m**2. * (w10m-u_thresh)

       endif
      endif

     end do   ! i
    end do    ! j

    w_c%qa(n1+n-1)%data3d(:,:,km) = &
            w_c%qa(n1+n-1)%data3d(:,:,km) + emis*cdt*grav/w_c%delp(:,:,km)

    if( associated(DU_emis(n)%data2d) ) DU_emis(n)%data2d = emis

!!!   call pmaxmin('DU:   emi', emis, qmin, qmax, (i2-i1+1)*(j2-j1+1), 1, 1. )

   end do     ! n

   deallocate(u_thresh0,stat=ios)
   rc = 0

   end subroutine DU_Emission
   
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  DU_Wet_Removal - Removal of dust by precipitation
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

   subroutine DU_Wet_Removal ( i1, i2, j1, j2, km, nbins, cdt, rhoa, gcDU, w_c,&
                               precc, precl, dqcond, tmpu, fluxout, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   type(DU_GridComp), intent(in)   :: gcDU  ! DU Grid Component
   real, pointer, dimension(:,:)   :: precc ! total convective precip [mm day-1]
   real, pointer, dimension(:,:)   :: precl ! total large-scale prec. [mm day-1]
   real, pointer, dimension(:,:,:) :: dqcond  ! change in q due to moist
                                              ! processes [kg kg-1 s-1] 
   real, pointer, dimension(:,:,:) :: tmpu    ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa    ! air density [kg m-3]

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: fluxout(nbins) ! Mass lost by wet 
                                                  ! to surface, kg/m2/s
   integer, intent(out)             :: rc         ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 
   character(len=*), parameter :: myname = 'DU_Wet_Removal'

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
   integer  ::  n1, n2
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

!  Efficiency of dust wet removal (since dust is really not too hygroscopic)
!  Applied only to in-cloud scavenging
#ifdef NEMS
   real, parameter :: effRemoval = 1.0
#else
   real, parameter :: effRemoval = 0.3
#endif

   rc=0

!  Initialize local variables
!  --------------------------
   do n = 1, nbins
    if( associated(fluxout(n)%data2d) ) fluxout(n)%data2d(i1:i2,j1:j2) = 0.0
   end do

   n1  = w_c%reg%i_DU
   n2  = w_c%reg%j_DU

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
      if ( gcDU%doing_scav == 1 ) then
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
!        DC(n) = w_c%qa(n1+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        DC(n) = w_c%qa(n1+n-1)%data3d(i,j,k) * F * effRemoval *(1.-exp(-BT))
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(n1+n-1)%data3d(i,j,k) = w_c%qa(n1+n-1)%data3d(i,j,k)-DC(n)
        if (w_c%qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
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
         DC(n) = w_c%qa(n1+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(n1+n-1)%data3d(i,j,k) = w_c%qa(n1+n-1)%data3d(i,j,k)-DC(n)
         if (w_c%qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) & 
          w_c%qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
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
!        DC(n) = w_c%qa(n1+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        DC(n) = w_c%qa(n1+n-1)%data3d(i,j,k) * F * effRemoval * (1.-exp(-BT))
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(n1+n-1)%data3d(i,j,k) = w_c%qa(n1+n-1)%data3d(i,j,k)-DC(n)
        if (w_c%qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
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
         DC(n) = w_c%qa(n1+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(n1+n-1)%data3d(i,j,k) = w_c%qa(n1+n-1)%data3d(i,j,k)-DC(n)
         if (w_c%qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) & 
          w_c%qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
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
           w_c%qa(n1+n-1)%data3d(i,j,k) = w_c%qa(n1+n-1)%data3d(i,j,k) + DC(n)
           w_c%qa(n1+n-1)%data3d(i,j,k) = max(w_c%qa(n1+n-1)%data3d(i,j,k),1.e-32)
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

   end subroutine DU_Wet_Removal


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  DU_Compute_Diags - Calculate dust 2D diagnostics
!
! !INTERFACE:
!

   subroutine DU_Compute_Diags ( i1, i2, j1, j2, km, nbins, gcDU, w_c, tmpu, rhoa, &
                                 sfcmass, colmass, mass, exttau, scatau, &
                                 sfcmass25, colmass25, mass25, exttau25, scatau25, &
                                 aerindx, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   type(DU_GridComp), intent(inout):: gcDU     ! DU Grid Component
   type(Chem_Bundle), intent(in)   :: w_c      ! Chem Bundle
   real, pointer, dimension(:,:,:) :: tmpu     ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa     ! air density [kg m-3]

! !OUTPUT PARAMETERS:
!  Total mass
   type(Chem_Array), intent(inout)  :: sfcmass ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: colmass ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: mass    ! 3d mass mixing ratio kg/kg
!  Total optical properties
   type(Chem_Array), intent(inout)  :: exttau  ! ext. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: scatau  ! sct. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: sfcmass25 ! sfc mass concentration kg/m3 (pm2.5)
   type(Chem_Array), intent(inout)  :: colmass25 ! col mass density kg/m2 (pm2.5)
   type(Chem_Array), intent(inout)  :: mass25    ! 3d mass mixing ratio kg/kg (pm2.5)
   type(Chem_Array), intent(inout)  :: exttau25  ! ext. AOT at 550 nm (pm2.5)
   type(Chem_Array), intent(inout)  :: scatau25  ! sct. AOT at 550 nm (pm2.5)
   type(Chem_Array), intent(inout)  :: aerindx ! TOMS UV AI
   integer, intent(out)             :: rc      ! Error return code:
                                               !  0 - all is well
                                               !  1 - 

! !DESCRIPTION: Calculates some simple 2d diagnostics from the dust fields
!
! !REVISION HISTORY:
!
!  16APR2004, Colarco
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   character(len=*), parameter :: myname = 'DU_Compute_Diags'
   integer :: i, j, k, n, n1, n2, ios, nch, idx
   real :: tau, ssa
   real :: fPM25(nbins)  ! fraction of bin with particles diameter < 2.5 um
   character(len=255) :: qname

!  Initialize local variables
!  --------------------------
   n1    = w_c%reg%i_DU
   n2    = w_c%reg%j_DU
   nch   = gcDU%mie_tables%nch

!  Compute the PM2.5 bin-wise fractions
!  ------------------------------------
   do n = 1, nbins
    if(gcDU%rup(n) < 1.25) then
     fPM25(n) = 1.
    else
     if(gcDU%rlow(n) < 1.25) then
!     Assume dm/dlnr = constant, i.e., dm/dr ~ 1/r
      fPM25(n) = log(1.25/gcDU%rlow(n)) / log(gcDU%rup(n)/gcDU%rlow(n))
     else
      fPM25(n) = 0.
     endif
    endif
   enddo

   if ( associated(aerindx%data2d) )  aerindx%data2d = 0.0  ! for now

!  Calculate the diagnostic variables if requested
!  -----------------------------------------------

!  Calculate the surface mass concentration
   if( associated(sfcmass%data2d) ) then
      sfcmass%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
         sfcmass%data2d(i1:i2,j1:j2) &
              =   sfcmass%data2d(i1:i2,j1:j2) &
              + w_c%qa(n+n1-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)
      end do
   endif
   if( associated(sfcmass25%data2d) ) then
      sfcmass25%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
         sfcmass25%data2d(i1:i2,j1:j2) &
              =   sfcmass25%data2d(i1:i2,j1:j2) &
              + w_c%qa(n+n1-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)*fPM25(n)
      end do
   endif

!  Calculate the seasalt column loading
   if( associated(colmass%data2d) ) then
      colmass%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
       do k = 1, km
        colmass%data2d(i1:i2,j1:j2) &
         =   colmass%data2d(i1:i2,j1:j2) &
           + w_c%qa(n+n1-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
       end do
      end do
   endif
   if( associated(colmass25%data2d)) then
      colmass25%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
       do k = 1, km
        colmass25%data2d(i1:i2,j1:j2) &
         =   colmass25%data2d(i1:i2,j1:j2) &
           + w_c%qa(n+n1-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav*fPM25(n)
       end do
      end do
   endif

!  Calculate the total mass mixing ratio
   if( associated(mass%data3d) ) then
      mass%data3d(i1:i2,j1:j2,1:km) = 0.
      do n = 1, nbins
       mass%data3d(i1:i2,j1:j2,1:km) &
         =   mass%data3d(i1:i2,j1:j2,1:km) &
           + w_c%qa(n+n1-1)%data3d(i1:i2,j1:j2,1:km)
      end do
   endif
   if( associated(mass25%data3d) ) then
      mass25%data3d(i1:i2,j1:j2,1:km) = 0.
      do n = 1, nbins
       mass25%data3d(i1:i2,j1:j2,1:km) &
         =   mass25%data3d(i1:i2,j1:j2,1:km) &
           + w_c%qa(n+n1-1)%data3d(i1:i2,j1:j2,1:km)*fPM25(n)
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
       qname = trim(w_c%reg%vname(w_c%reg%i_DU+n-1))
       idx = Chem_MieQueryIdx(gcDU%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

!      Recall -- at present need to divide RH by 100 to get to tables
       do k = 1, km
        do j = j1, j2
         do i = i1, i2
          call Chem_MieQuery(gcDU%mie_tables, idx, 1., &
              w_c%qa(n1+n-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
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

   end subroutine DU_Compute_Diags

 end subroutine DU_GridCompRun

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  DU_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine DU_GridCompFinalize ( gcDU, w_c, impChem, expChem, &
                                    nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(DU_GridComp), intent(inout) :: gcDU   ! Grid Component

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(in)  :: w_c      ! Chemical tracer fields   
   integer, intent(in) :: nymd, nhms          ! time
   real, intent(in) :: cdt                    ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: impChem   ! Import State
   type(ESMF_State), intent(inout) :: expChem   ! Export State
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

    rc=0
!   integer :: ios

!   deallocate ( gcDU%radius, gcDU%src, stat=ios )
!   if ( ios /= 0 ) then
!      rc = 1
!      return
!   end if

   return

 end subroutine DU_GridCompFinalize

 end module DU_GridCompMod

