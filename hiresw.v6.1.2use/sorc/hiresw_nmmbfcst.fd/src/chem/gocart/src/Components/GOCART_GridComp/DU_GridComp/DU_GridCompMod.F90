#include "MAPL_Generic.h"

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

   USE ESMF
   USE MAPL_Mod

   use Chem_Mod              ! Chemistry Base Class
   use Chem_StateMod         ! Chemistry State
   use Chem_ConstMod, only: grav, von_karman, cpd, &
                            undefval => undef         ! Constants !
   use Chem_UtilMod          ! I/O
   use Chem_MieMod           ! Aerosol LU Tables, calculator
   use m_inpak90             ! Resource file management
   use m_die, only: die
   use m_mpout
   use DustEmissionMod       ! Emissions
   use Chem_SettlingMod      ! Settling
   use DryDepositionMod      ! Dry Deposition
   use WetRemovalMod         ! Large-scale Wet Removal
   use ConvectionMod         ! Offline convective mixing/scavenging

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
!  30Sep2014 Lu        Remove doing_scav option
!
!EOP
!-------------------------------------------------------------------------

  type DU_GridComp
        character(len=255) :: name
        type(Chem_Mie), pointer :: mie_tables  ! aod LUTs
        integer       :: rhFlag
        logical       :: maringFlag     ! settling velocity correction
        real, pointer :: src(:,:)       ! Ginoux dust sources
        real, pointer :: radius(:)      ! particle effective radius [um]
        real, pointer :: rlow(:)        ! particle effective radius lower bound [um]
        real, pointer :: rup(:)         ! particle effective radius upper bound [um]
        real, pointer :: sfrac(:)       ! fraction of total source
        real, pointer :: rhop(:)        ! soil class density [kg m-3]
        integer :: nymd
        character(len=255) :: srcfilen
	REAL :: Ch_DU                   ! Dust emission tuning coefficient [kg s2 m-5].
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
   real :: radius, rlow, rup, rmrat, rmin, rhop, fscav, fnum, molwght
   integer :: irhFlag
   integer :: imaringFlag

   integer, parameter :: nhres = 5   ! number of horizontal model resolutions: a,b,c,d,e
   real    :: Ch_DU(nhres)           ! emission tuning coefficient buffer


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
!jw
   print *,'in du_gridcmp,number_dust_bins=',ier(1)
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
!jw
   print *,'in du_gridcmp,ginoux, filename=',ier(1)
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

!  Particle radius
!  ---------------
   call i90_label ( 'particle_radius:', ier(1) )
   do n = 1, nbins
      radius               = i90_gfloat ( ier(n+1) )
      gcDU%radius(n)       = radius
      w_c%reg%rmed(n1+n-1) = radius * 1.e-6
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if

!  Particle radius (lower bound)
!  ---------------
   call i90_label ( 'radius_lower:', ier(1) )
   do n = 1, nbins
      rlow                  = i90_gfloat ( ier(n+1) )
      gcDU%rlow(n)          = rlow
      w_c%reg%rlow(n1+n-1)  = rlow * 1.e-6
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if

!  Particle radius (upper bound)
!  ---------------
   call i90_label ( 'radius_upper:', ier(1) )
   do n = 1, nbins
      rup                 = i90_gfloat ( ier(n+1) )
      gcDU%rup(n)         = rup
      w_c%reg%rup(n1+n-1) = rup * 1.e-6
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
      rhop                 = i90_gfloat ( ier(n+1) )
      gcDU%rhop(n)         = rhop
      w_c%reg%rhop(n1+n-1) = rhop
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
   print *,'in du_gridcmp,fscav',ier(1)
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

!  Number to mass conversion factor
!  To be used in droplet activation code
!  ---------------
   call i90_label ( 'fnum:', ier(1) )
   do n = 1, nbins
      fnum                    = i90_gfloat ( ier(n+1) )
      w_c%reg%fnum(n1+n-1)    = fnum
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Molecular weight
!  To be used in droplet activation code
!  ---------------
   call i90_label ( 'molecular_weight:', ier(1) )
   do n = 1, nbins
      molwght                 = i90_gfloat ( ier(n+1) )
      w_c%reg%molwght(n1+n-1) = molwght
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
   gcDU%rhFlag                = irhFlag
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Dust emission tuning coefficient [kg s2 m-5]. NOT bin specific.
!  ---------------------------------------------------------------
   CALL I90_Label ( 'Ch_DU:', ier(1) )
   do n = 1, nhres
      Ch_DU(n) = i90_gfloat ( ier(n+1) )
   end do
   gcDU%Ch_DU = Chem_UtilResVal(im, jm, Ch_DU(:), ier(nhres + 2))
   gcDU%Ch_DU = gcDU%Ch_DU * 1.00E-09
   if ( any(ier(1:nhres+2) /= 0) ) then
      call final_(50)
      return
   end if

!  Settling velocity correction following Maring et al, 2003
!  ---------------
   call i90_label ( 'maringFlag:', ier(1) )
   imaringFlag = i90_gint ( ier(2) )
   if (imaringFlag /= 0) then
      gcDU%maringFlag = .True.
   else
      gcDU%maringFlag = .False.
   end if
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Initialize date for BCs
!  -----------------------
   gcDU%nymd = -1   ! nothing read yet

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
   real, pointer :: emissions(:,:), dqa(:,:), drydepositionfrequency(:,:)
   type(Chem_Array), pointer :: fluxout


!  Input fields from fvGCM
!  -----------------------
   real, pointer, dimension(:,:)   ::  gwettop, oro, u10m, v10m, &
                                       ustar, precc, precl, pblh,          &
                                       shflux, z0h, hsurf, frocean, frseaice
   real, pointer, dimension(:,:,:) ::  dqcond, tmpu, rhoa, u, v, hghte, ple

!  Additional needs for GOCART convective diagnostic
   real, pointer, dimension(:,:,:)     ::  cmfmc, qccu, dtrain
   real, pointer, dimension(:,:)       ::  frlake, area
   real*8, allocatable, dimension(:,:,:) ::  cmfmc_, qccu_, dtrain_, &
                                             airmass_, airmol_, vud_, &
                                             delz_, delp_
   real*8, allocatable                   ::  tc_(:,:,:,:), bcnv_(:,:,:)
   real*8, allocatable                   ::  area_(:,:), frlake_(:,:), &
                                             frocean_(:,:), frseaice_(:,:)
   integer*4                             ::  icdt

#define EXPORT     expChem

#define ptrDUWT       DU_wet
#define ptrDUSV       DU_conv
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
#define    DUFLUXU    DU_fluxu
#define    DUFLUXV    DU_fluxv
#define    DUCONC     DU_conc
#define    DUEXTCOEF  DU_extcoef
#define    DUSCACOEF  DU_scacoef
#define    DUEXTTFM   DU_exttaufm
#define    DUSCATFM   DU_scataufm
#define    DUANGSTR   DU_angstrom

   integer :: STATUS

#include "DU_GetPointer___.h"

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

   if ( nbins /= NBIN_DUEM .OR. nbins /= NBIN_DUWT .OR. &
        nbins /= NBIN_DUDP .OR. nbins /= NBIN_DUSD ) then
      call die(myname,'inconsistent bins in resource file and registry')
   endif

! Update emissions/production if necessary (daily)
!  ------------------------------------------
   if(gcDU%nymd < 0) then

!   The dust file is time invariant, and currently hard set
    nymd1 = 19710605
    nhms1 = 0
    print *,'bf read du_src'
    call Chem_UtilMPread ( gcDU%srcfilen, 'du_src', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0,       &
                           var2d=gcDU%src, grid = w_c%grid_esmf   )
    print *,'af read du_src'

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

!  Dust particle radius [m] and density [kg m-3]
!  ---------------------------------------------
   allocate( DU_radius(nbins), DU_rhop(nbins) )
   DU_radius = 1.e-6*gcDU%radius
   DU_rhop   = gcDU%rhop
   allocate( fluxout )
   allocate( fluxout%data2d(i1:i2,j1:j2), emissions(i1:i2,j1:j2), dqa(i1:i2,j1:j2), &
             drydepositionfrequency(i1:i2,j1:j2), stat=STATUS)
   VERIFY_(STATUS)


#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_beg', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                   ijl, km, 1. )
   end do
#endif

!  Get 2D Imports
!  --------------
   call MAPL_GetPointer ( impChem, frlake, 'FRLAKE',  rc=ier(1) )
   call MAPL_GetPointer ( impChem, gwettop,  'WET1',    rc=ier(2) )
   call MAPL_GetPointer ( impChem, oro,      'LWI',     rc=ier(3) )
   call MAPL_GetPointer ( impChem, u10m,     'U10M',    rc=ier(4) )
   call MAPL_GetPointer ( impChem, v10m,     'V10M',    rc=ier(5) )
   call MAPL_GetPointer ( impChem, ustar,    'USTAR',   rc=ier(6) )
   call MAPL_GetPointer ( impChem, precc,    'CN_PRCP', rc=ier(7) )
   call MAPL_GetPointer ( impChem, precl,    'NCN_PRCP',   rc=ier(8) )
   call MAPL_GetPointer ( impChem, pblh,     'ZPBL',    rc=ier(9) )
   call MAPL_GetPointer ( impChem, shflux,   'SH',      rc=ier(10) )
   call MAPL_GetPointer ( impChem, z0h,      'Z0H',     rc=ier(11) )
   call MAPL_GetPointer ( impChem, area,     'AREA',     rc=ier(12) )
   call MAPL_GetPointer ( impChem, frocean,  'FROCEAN',  rc=ier(13) )
   call MAPL_GetPointer ( impChem, frseaice, 'FRACI',    rc=ier(14) )

!  Get 3D Imports
!  --------------
   call MAPL_GetPointer ( impChem, dqcond, 'DQDT',    rc=ier(15) )
   call MAPL_GetPointer ( impChem, tmpu,   'T',       rc=ier(16) )
   call MAPL_GetPointer ( impChem, rhoa,   'AIRDENS', rc=ier(17) )
   call MAPL_GetPointer ( impChem, u,      'U',       rc=ier(18) )
   call MAPL_GetPointer ( impChem, v,      'V',       rc=ier(19) )
   call MAPL_GetPointer ( impChem, hghte,  'ZLE',     rc=ier(20) )
   call MAPL_GetPointer ( impChem, ple,    'PLE',     rc=ier(21) )
   call MAPL_GetPointer ( impChem, qccu,   'CNV_QC',  rc=ier(22) )
   call MAPL_GetPointer ( impChem, cmfmc,  'CNV_MFC', rc=ier(23) )
   call MAPL_GetPointer ( impChem, dtrain, 'CNV_MFD', rc=ier(24) )

!  Unlike GEOS-4 hghte is defined for km+1
!  ---------------------------------------
   hsurf => hghte(i1:i2,j1:j2,km) ! Recall: GEOS-5 has edges with k in [0,km]
    
   if ( any(ier(1:24) /= 0) ) then
        rc = 10 
        return
   end if

#ifdef DEBUG

   call pmaxmin('DU: frlake     ', frlake  , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: gwtop      ', gwettop , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: oro        ', oro     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: u10m       ', u10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('DU: v10m       ', v10m    , qmin, qmax, ijl,1, 1. )
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

!  Dust Source
!  -----------
   do n = 1, nbins
    emissions = 0.
    dqa = 0.
    call DustEmissionGOCART( i1, i2, j1, j2, km, DU_radius(n), &
                             frlake, gwettop, oro, u10m, v10m, &
                             emissions, rc )
    dqa = gcDU%Ch_DU*gcDU%sfrac(n)*gcDU%src * emissions &
                     * cdt * grav / w_c%delp(:,:,km)
    w_c%qa(n1+n-1)%data3d(:,:,km) = &
            w_c%qa(n1+n-1)%data3d(:,:,km) + dqa

     print *,'in DU comp, check DU_emis=',associated(DU_emis(n)%data2d)
    if( associated(DU_emis(n)%data2d) ) &
     DU_emis(n)%data2d = gcDU%Ch_DU*gcDU%sfrac(n)*gcDU%src * emissions
   end do


#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_emi', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Dust Settling
!  -----------
   call Chem_Settling ( i1, i2, j1, j2, km, n1, n2, nbins, gcDU%rhFlag, &
                        DU_radius, DU_rhop, cdt, w_c, tmpu, rhoa, hsurf,    &
                        hghte, DU_set, rc, correctionMaring=gcDU%maringFlag )

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_set', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Dust Deposition
!  -----------
   do n = 1, nbins
    drydepositionfrequency = 0.
    call DryDepositionGOCART( i1, i2, j1, j2, km, &
                              tmpu, rhoa, hghte, oro, ustar, &
                              pblh, shflux, z0h, drydepositionfrequency, rc, &
                              DU_radius(n), DU_rhop(n), u10m, v10m, frlake, gwettop )
    
    dqa = 0.
    dqa = max(0.0, w_c%qa(n1+n-1)%data3d(:,:,km)*(1.-exp(-drydepositionfrequency*cdt)))
    w_c%qa(n1+n-1)%data3d(:,:,km) = &
            w_c%qa(n1+n-1)%data3d(:,:,km) - dqa
    if( associated(DU_dep(n)%data2d) ) &
     DU_dep(n)%data2d = dqa*w_c%delp(:,:,km)/grav/cdt
   end do

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_dry', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Dust Large-scale Wet Removal
!  ----------------------------
   do n = 1, nbins
    w_c%qa(n1+n-1)%fwet = 0.3
    call WetRemovalGOCART(i1, i2, j1, j2, km, n1+n-1, n1+n-1, cdt, &
                          w_c%qa, ple, tmpu, rhoa, dqcond, precc, precl, &
                          fluxout, rc )
    if(associated(DU_wet(n)%data2d)) DU_wet(n)%data2d = fluxout%data2d
   end do

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('DU: q_wet', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Dust Convective-scale Mixing and Wet Removal
!  --------------------------------------------
   icdt = cdt
   allocate(cmfmc_(i1:i2,j1:j2,km+1), qccu_(i1:i2,j1:j2,km), &
            dtrain_(i1:i2,j1:j2,km), airmass_(i1:i2,j1:j2,km), &
            delz_(i1:i2,j1:j2,km), vud_(i1:i2,j1:j2,km), &
            tc_(i1:i2,j1:j2,km,n1:n2), delp_(i1:i2,j1:j2,km), &
            airmol_(i1:i2,j1:j2,km), bcnv_(i1:i2,j1:j2,n1:n2), &
            area_(i1:i2,j1:j2), frlake_(i1:i2,j1:j2), &
            frocean_(i1:i2,j1:j2), frseaice_(i1:i2,j1:j2), __STAT__ )

   area_            = area
   frlake_          = frlake
   frocean_         = frocean
   frseaice_        = frseaice
   do k = 1, km+1
    cmfmc_(:,:,k)   = cmfmc(:,:,km-k+1)
   end do
   do k = 1, km
    dtrain_(:,:,k)  = dtrain(:,:,km-k+1)
    qccu_(:,:,k)    = qccu(:,:,km-k+1)
    delp_(:,:,k)    = w_c%delp(:,:,km-k+1)/100.
    airmass_(:,:,k) = w_c%delp(:,:,km-k+1)/grav*area_
    airmol_(:,:,k)  = airmass_(:,:,k)*1000./28.966
    delz_(:,:,k)    = w_c%delp(:,:,km-k+1)/grav/rhoa(:,:,km-k+1)
   enddo
   do n = n1, n2
    do k = 1, km
     tc_(:,:,k,n)   = w_c%qa(n)%data3d(:,:,km-k+1)
    enddo
   enddo
   call set_vud(i1, i2, j1, j2, km, frlake_, frocean_, frseaice_, cmfmc_, qccu_, &
                airmass_, delz_, area_, vud_)
   call convection(i1, i2, j1, j2, km, n1, n2, icdt, 'dust', &
                   tc_, cmfmc_, dtrain_, area_, delz_, delp_, vud_, &
                   airmass_, airmol_, &
                   bcnv_) 
!  Return adjusted tracer to mixing ratio
   do n = n1, n2
    do k = 1, km
     w_c%qa(n)%data3d(:,:,km-k+1) = tc_(:,:,k,n)
    enddo
   enddo

!  Note GOCART returns bcnv_ as negative, recast for my diagnostic
   do n = 1, nbins
    if(associated(DU_conv(n)%data2d)) DU_conv(n)%data2d = -bcnv_(:,:,n1+n-1)/area_/icdt
   end do

   deallocate(cmfmc_, qccu_, dtrain_, tc_, airmass_, &
              delz_, vud_, delp_, airmol_, bcnv_, &
              area_, frlake_, frocean_, frseaice_, __STAT__ )


!  Compute the desired output diagnostics here
!  Ideally this will go where chemout is called in fvgcm.F since that
!  will reflect the distributions after transport, etc.
!  -----------
   call DU_Compute_Diags(i1, i2, j1, j2, km, nbins, gcDU, w_c, tmpu, rhoa,    &
                         u, v, DU_sfcmass,  DU_colmass, DU_mass, DU_exttau,   &
                         DU_scatau,   DU_sfcmass25, DU_colmass25, DU_mass25,  &
                         DU_exttau25, DU_scatau25,  DU_aeridx, DU_fluxu,      &
                         DU_fluxv, DU_conc, DU_extcoef, DU_scacoef,           &
                         DU_exttaufm, DU_scataufm, DU_angstrom, rc)

!  Clean up
!  --------
   deallocate ( fluxout%data2d )
   deallocate ( fluxout, DU_radius, DU_rhop, emissions, &
                dqa, drydepositionfrequency, stat=STATUS )

   return

CONTAINS

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
                                 u, v, sfcmass, colmass, mass, exttau, scatau,     &
                                 sfcmass25, colmass25, mass25, exttau25, scatau25, &
                                 aerindx, fluxu, fluxv, conc, extcoef, scacoef,    &
                                 exttaufm, scataufm, angstrom, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   type(DU_GridComp), intent(inout):: gcDU     ! DU Grid Component
   type(Chem_Bundle), intent(in)   :: w_c      ! Chem Bundle
   real, pointer, dimension(:,:,:) :: tmpu     ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa     ! air density [kg m-3]
   real, pointer, dimension(:,:,:) :: u        ! east-west wind [m s-1]
   real, pointer, dimension(:,:,:) :: v        ! north-south wind [m s-1]
   

! !OUTPUT PARAMETERS:
!  Total mass
   type(Chem_Array), intent(inout)  :: sfcmass   ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: colmass   ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: mass      ! 3d mass mixing ratio kg/kg
!  Total optical properties
   type(Chem_Array), intent(inout)  :: exttau    ! ext. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: scatau    ! sct. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: sfcmass25 ! sfc mass concentration kg/m3 (pm2.5)
   type(Chem_Array), intent(inout)  :: colmass25 ! col mass density kg/m2 (pm2.5)
   type(Chem_Array), intent(inout)  :: mass25    ! 3d mass mixing ratio kg/kg (pm2.5)
   type(Chem_Array), intent(inout)  :: exttau25  ! ext. AOT at 550 nm (pm2.5)
   type(Chem_Array), intent(inout)  :: scatau25  ! sct. AOT at 550 nm (pm2.5)
   type(Chem_Array), intent(inout)  :: aerindx   ! TOMS UV AI
   type(Chem_Array), intent(inout)  :: fluxu     ! Column mass flux in x direction
   type(Chem_Array), intent(inout)  :: fluxv     ! Column mass flux in y direction
   type(Chem_Array), intent(inout)  :: conc      ! 3d mass concentration, kg/m3
   type(Chem_Array), intent(inout)  :: extcoef   ! 3d ext. coefficient, 1/m
   type(Chem_Array), intent(inout)  :: scacoef   ! 3d scat.coefficient, 1/m
   type(Chem_Array), intent(inout)  :: exttaufm  ! fine mode (sub-micron) ext. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: scataufm  ! fine mode (sub-micron) sct. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: angstrom  ! 470-870 nm Angstrom parameter
   integer, intent(out)             :: rc        ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: Calculates some simple 2d diagnostics from the dust fields
!
! !REVISION HISTORY:
!
!  16APR2004, Colarco
!  11MAR2010, Nowottnick  
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   character(len=*), parameter :: myname = 'DU_Compute_Diags'
   integer :: i, j, k, n, n1, n2, ios, nch, idx
   real :: ilam550, ilam470, ilam870
   real :: tau, ssa
   real :: fPMfm(nbins)  ! fraction of bin with particles diameter < 1.0 um
   real :: fPM25(nbins)  ! fraction of bin with particles diameter < 2.5 um
   character(len=255) :: qname
   logical :: do_angstrom
   real, dimension(i1:i2,j1:j2) :: tau470, tau870

!  Initialize local variables
!  --------------------------
   n1    = w_c%reg%i_DU
   n2    = w_c%reg%j_DU
   nch   = gcDU%mie_tables%nch

!  Get the wavelength indices
!  --------------------------
!  Must provide ilam550 for AOT calculation
   ilam550 = 1.
   ilam470 = 0.
   ilam870 = 0.
   if(nch .gt. 1) then
    do i = 1, nch
     if ( gcDU%mie_tables%channels(i) .ge. 5.49e-7 .and. &
          gcDU%mie_tables%channels(i) .le. 5.51e-7) ilam550 = i
     if ( gcDU%mie_tables%channels(i) .ge. 4.69e-7 .and. &
          gcDU%mie_tables%channels(i) .le. 4.71e-7) ilam470 = i
     if ( gcDU%mie_tables%channels(i) .ge. 8.69e-7 .and. &
          gcDU%mie_tables%channels(i) .le. 8.71e-7) ilam870 = i
    enddo
   endif

   do_angstrom = .false.
!  If both 470 and 870 channels provided (and not the same) then
!  possibly will do Angstrom parameter calculation
   if(ilam470 .ne. 0. .and. &
      ilam870 .ne. 0. .and. &
      ilam470 .ne. ilam870) do_angstrom = .true.

!  Compute the fine mode (sub-micron) and PM2.5 bin-wise fractions
!  ------------------------------------
   call DU_Binwise_PM_Fractions(fPMfm, 0.50, gcDU%rlow, gcDU%rup, nbins)   ! 2*r < 1.0 um
   call DU_Binwise_PM_Fractions(fPM25, 1.25, gcDU%rlow, gcDU%rup, nbins)   ! 2*r < 2.5 um


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

!  Calculate the dust column loading
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

!  Calculate the total mass concentration
   if( associated(conc%data3d) ) then
      conc%data3d(i1:i2,j1:j2,1:km) = 0.
      do n = 1, nbins
       conc%data3d(i1:i2,j1:j2,1:km) &
         =   conc%data3d(i1:i2,j1:j2,1:km) &
           + w_c%qa(n+n1-1)%data3d(i1:i2,j1:j2,1:km)*rhoa(i1:i2,j1:j2,1:km)
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
   
!  Calculate the column mass flux in x direction
   if( associated(fluxu%data2d) ) then
      fluxu%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
       do k = 1, km
        fluxu%data2d(i1:i2,j1:j2) &
         =   fluxu%data2d(i1:i2,j1:j2) &
           + w_c%qa(n+n1-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav*u(i1:i2,j1:j2,k)
       end do
      end do
   endif   
   
!  Calculate the column mass flux in y direction
   if( associated(fluxv%data2d) ) then
      fluxv%data2d(i1:i2,j1:j2) = 0.
      do n = 1, nbins
       do k = 1, km
        fluxv%data2d(i1:i2,j1:j2) &
         =   fluxv%data2d(i1:i2,j1:j2) &
           + w_c%qa(n+n1-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav*v(i1:i2,j1:j2,k)
       end do
      end do
   endif      

!  Calculate the extinction and/or scattering AOD
   if( associated(exttau%data2d) .or. associated(scatau%data2d) ) then

      if( associated(exttau%data2d)) exttau%data2d(i1:i2,j1:j2) = 0.
      if( associated(scatau%data2d)) scatau%data2d(i1:i2,j1:j2) = 0.

      if( associated(exttau25%data2d)) exttau25%data2d(i1:i2,j1:j2) = 0.
      if( associated(scatau25%data2d)) scatau25%data2d(i1:i2,j1:j2) = 0.

      if( associated(exttaufm%data2d)) exttaufm%data2d(i1:i2,j1:j2) = 0.
      if( associated(scataufm%data2d)) scataufm%data2d(i1:i2,j1:j2) = 0.

      if( associated(extcoef%data3d)) extcoef%data3d(i1:i2,j1:j2,1:km) = 0.
      if( associated(scacoef%data3d)) scacoef%data3d(i1:i2,j1:j2,1:km) = 0.

      do n = 1, nbins

!      Select the name for species
       qname = trim(w_c%reg%vname(w_c%reg%i_DU+n-1))
       idx = Chem_MieQueryIdx(gcDU%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

       do k = 1, km
        do j = j1, j2
         do i = i1, i2
          call Chem_MieQuery(gcDU%mie_tables, idx, ilam550, &
              w_c%qa(n1+n-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
              w_c%rh(i,j,k), tau=tau, ssa=ssa)

!         Calculate the total ext. and scat. coefficients
          if( associated(extcoef%data3d) ) then
              extcoef%data3d(i,j,k) = extcoef%data3d(i,j,k) + &
                                      tau * (grav * rhoa(i,j,k) / w_c%delp(i,j,k))
          endif
          if( associated(scacoef%data3d) ) then
              scacoef%data3d(i,j,k) = scacoef%data3d(i,j,k) + &
                                      ssa * tau * (grav * rhoa(i,j,k) / w_c%delp(i,j,k))
          endif

!         Integrate in the vertical
          if( associated(exttau%data2d) ) exttau%data2d(i,j) = exttau%data2d(i,j) + tau
          if( associated(exttaufm%data2d)) &
                         exttaufm%data2d(i,j) = exttaufm%data2d(i,j) + tau*fPMfm(n)
          if( associated(exttau25%data2d)) &
                         exttau25%data2d(i,j) = exttau25%data2d(i,j) + tau*fPM25(n)

          if( associated(scatau%data2d) ) scatau%data2d(i,j) = scatau%data2d(i,j) + tau*ssa
          if( associated(scataufm%data2d) ) &
                         scataufm%data2d(i,j) = scataufm%data2d(i,j) + tau*ssa*fPMfm(n)
          if( associated(scatau25%data2d) ) &
                         scatau25%data2d(i,j) = scatau25%data2d(i,j) + tau*ssa*fPM25(n)

         enddo
        enddo
       enddo

      enddo  ! nbins

   endif

!  Calculate the 470-870 Angstrom parameter
   if( associated(angstrom%data2d) .and. do_angstrom ) then

      angstrom%data2d(i1:i2,j1:j2) = 0.
!     Set tau to small number by default
      tau470(i1:i2,j1:j2) = tiny(1.0)
      tau870(i1:i2,j1:j2) = tiny(1.0)

      do n = 1, nbins

!      Select the name for species
       qname = trim(w_c%reg%vname(w_c%reg%i_DU+n-1))
       idx = Chem_MieQueryIdx(gcDU%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

       do k = 1, km
        do j = j1, j2
         do i = i1, i2

          call Chem_MieQuery(gcDU%mie_tables, idx, ilam470, &
              w_c%qa(n1+n-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
              w_c%rh(i,j,k), tau=tau)
          tau470(i,j) = tau470(i,j) + tau

          call Chem_MieQuery(gcDU%mie_tables, idx, ilam870, &
              w_c%qa(n1+n-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
              w_c%rh(i,j,k), tau=tau)
          tau870(i,j) = tau870(i,j) + tau

         enddo
        enddo
       enddo

      enddo  ! nbins

      angstrom%data2d(i1:i2,j1:j2) = &
        -log(tau470(i1:i2,j1:j2)/tau870(i1:i2,j1:j2)) / &
         log(470./870.)
   endif

   rc = 0

   end subroutine DU_Compute_Diags


!##############################################################################
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  DU_Binwise_PM_Fractions - Calculate bin-wise PM fractions
!
! !INTERFACE:
!

   subroutine DU_Binwise_PM_Fractions(fPM, rPM, r_low, r_up, nbins)

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

  real, dimension(:), intent(inout) :: fPM     ! bin-wise PM fraction (r < rPM)

! !INPUT PARAMETERS:

   real,    intent(in)              :: rPM     ! PM radius
   integer, intent(in)              :: nbins   ! number of bins
   real, dimension(:), intent(in)   :: r_low   ! bin radii - low bounds
   real, dimension(:), intent(in)   :: r_up    ! bin radii - upper bounds

! !OUTPUT PARAMETERS:
!EOP

! !Local Variables

   integer :: n

   character(len=*), parameter :: myname = 'DU_Binwise_PM_Fractions'

   do n = 1, nbins
     if(r_up(n) < rPM) then
       fPM(n) = 1.0
     else
       if(r_low(n) < rPM) then
!        Assume dm/dlnr = constant, i.e., dm/dr ~ 1/r
         fPM(n) = log(rPM/r_low(n)) / log(r_up(n)/r_low(n))
       else
         fPM(n) = 0.0
       endif
     endif
   enddo

   end subroutine DU_Binwise_PM_Fractions

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

