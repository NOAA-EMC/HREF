#include "MAPL_Generic.h"

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  BC_GridCompMod --- BC Grid Component Class
!
! !INTERFACE:
!

   module  BC_GridCompMod

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
   use Chem_SettlingMod      ! Settling
   use DryDepositionMod      ! Dry Deposition
   use WetRemovalMod         ! Large-scale Wet Removal
   use ConvectionMod         ! Offline convective mixing/scavenging

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  BC_GridComp       ! The BC object 

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  BC_GridCompInitialize
   PUBLIC  BC_GridCompRun
   PUBLIC  BC_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the (pre-ESMF) BC Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  13Mar2013 Lu        Add NEMS option
!
!EOP
!-------------------------------------------------------------------------

  type BC_GridComp
        character(len=255) :: name
        type(Chem_Mie), pointer :: mie_tables  ! aod LUTs
        real, pointer :: biofuel_src(:,:)
        real, pointer :: biomass_src_(:,:) ! before diurnal
        real, pointer :: biomass_src(:,:)
        real, pointer :: ebcant1_src(:,:)  ! level 1
        real, pointer :: ebcant2_src(:,:)  ! level 2
        real, pointer :: bc_ship_src(:,:)
        real :: fHydrophobic         ! Fraction of emissions hydrophobic
        real :: eBiofuel             ! Emission factor of Biofuel to BC aerosol
        real :: eBiomassBurning      ! Emission factor of Biomass Burning to BC
        integer :: nymd   ! date of last emissions/prodction
        integer :: doing_scav        ! compute tracer scavenging for NEMS
        character(len=255) :: bb_srcfilen
        character(len=255) :: bf_srcfilen
        character(len=255) :: ebcant1_srcfilen
        character(len=255) :: ebcant2_srcfilen
        character(len=255) :: bc_ship_srcfilen
        integer :: nymd_bb      = 0
        integer :: nymd_bf      = 0
        integer :: nymd_ebcant1 = 0
        integer :: nymd_ebcant2 = 0
        integer :: nymd_bc_ship = 0
  end type BC_GridComp

  real, parameter :: OCEAN=0.0, LAND = 1.0, SEA_ICE = 2.0
  real, parameter :: radToDeg = 57.2957795

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  BC_GridCompInitialize --- Initialize BC_GridComp
!
! !INTERFACE:
!

   subroutine BC_GridCompInitialize ( gcBC, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c     ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real, intent(in) :: cdt                     ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(BC_GridComp), intent(inout) :: gcBC     ! Grid Component
   type(ESMF_State), intent(inout)  :: impChem  ! Import State
   type(ESMF_State), intent(inout)  :: expChem  ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the BC Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'BC_GridCompInitialize'


   character(len=255) :: rcfilen = 'BC_GridComp.rc'
   integer :: ios, n
   integer :: i1, i2, im, j1, j2, jm, nbins, n1, n2, nbins_rc
   integer :: idoing_scav  ! NEMS option to re-activate convective removal
   integer :: nTimes, begTime, incSecs
   integer, allocatable :: ier(:)
   real, allocatable :: buffer(:,:)
   real :: qmax, qmin

   gcBC%name = 'BC Constituent Package'

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   nbins = w_c%reg%n_BC
   n1  = w_c%reg%i_BC
   n2  = w_c%reg%j_BC

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

   call i90_label ( 'number_bc_classes:', ier(1) )
   nbins_rc = i90_gint ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(20)
      return
   end if
   if ( nbins_rc /= nbins ) then
      call final_(25)
      return
   end if


!  BC sources files
!  ---------------------
   call i90_label ( 'bb_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcBC%bb_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'bf_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcBC%bf_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'ebcant1_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcBC%ebcant1_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'ebcant2_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcBC%ebcant2_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'bc_ship_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcBC%bc_ship_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

!  Hydrophobic fraction
!  ---------------
   call i90_label ( 'hydrophobic_fraction:', ier(1) )
   gcBC%fHydrophobic = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if


!  Biofuel Emission Factor
!  ---------------
   call i90_label ( 'biofuel_emission_factor:', ier(1) )
   gcBC%eBiofuel = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if


!  Biomass Burning Emission Factor
!  ---------------
   call i90_label ( 'biomass_burning_emission_factor:', ier(1) )
   gcBC%eBiomassBurning = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if


!  Scavenging Efficiency
!  To be used in convtran.F90, this parameter
!  is the scavenging efficiency of the tracer [km -1]
!  ---------------
   call i90_label ( 'fscav:', ier(1) )
   do n = 1, nbins
      w_c%reg%fscav(n1+n-1) = i90_gfloat ( ier(n+1) )
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if
! 
!  NEMS Option to compute convective rainout/washout in GOCART
!  ---------------

   gcBC%doing_scav = 0     ! Default is to compute convective
!                          ! rainout/washout in GFS physics
#ifdef NEMS
   call i90_label ( 'doing_scav:', ier(1) )
   idoing_scav                 = i90_gint ( ier(2) )
   gcBC%doing_scav             = idoing_scav
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if

!  invoke the option to compute convective removal in GOCART
!  set fscav (scav used in GFS RAS) to 0.
   if ( gcBC%doing_scav == 1 ) then
     do n = 1, nbins
      w_c%reg%fscav(nbeg+n-1)   = 0.
      w_c%qa(nbeg+n-1)%fscav    = 0.
     end do
   endif
#endif

!                          -------

!  Particle density
!  To be used in droplet activation code
!  ---------------
   call i90_label ( 'particle_density:', ier(1) )
   do n = 1, nbins
      w_c%reg%rhop(n1+n-1)  = i90_gfloat ( ier(n+1) )
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Number median radius
!  To be used in droplet activation code
!  ---------------
   call i90_label ( 'particle_radius_number:', ier(1) )
   do n = 1, nbins
      w_c%reg%rmed(n1+n-1)  = i90_gfloat ( ier(n+1) ) * 1e-6
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Sigma (lognormal mode width)
!  To be used in droplet activation code
!  ---------------
   call i90_label ( 'sigma:', ier(1) )
   do n = 1, nbins
      w_c%reg%sigma(n1+n-1)  = i90_gfloat ( ier(n+1) )
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
      w_c%reg%fnum(n1+n-1)  = i90_gfloat ( ier(n+1) )
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
      w_c%reg%molwght(n1+n-1)  = i90_gfloat ( ier(n+1) )
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if
!                          -------

!  Check initial date of inventory emission/oxidant files
!  ------------------------------------------------------
!  The intent here is that these files are valid for a particular
!  YYYY or YYYYMMDD (if 1x year in file).  We need to request
!  the correct date
   if( index(gcBC%bb_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcBC%bb_srcfilen, gcBC%nymd_bb, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcBC%bf_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcBC%bf_srcfilen, gcBC%nymd_bf, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcBC%ebcant1_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcBC%ebcant1_srcfilen, gcBC%nymd_ebcant1, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcBC%ebcant2_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcBC%ebcant2_srcfilen, gcBC%nymd_ebcant2, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcBC%bc_ship_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcBC%bc_ship_srcfilen, gcBC%nymd_bc_ship, &
                               begTime, nTimes, incSecs )
   endif
   ier(1) = gcBC%nymd_bb
   ier(2) = gcBC%nymd_bf
   ier(3) = gcBC%nymd_ebcant1
   ier(4) = gcBC%nymd_ebcant2
   ier(5) = gcBC%nymd_bc_ship
   if( any(ier(1:5) < 0 ) ) then
     call final_(60)
     return
   endif


!  Initialize date for BCs
!  -----------------------
   gcBC%nymd = -1   ! nothing read yet


!  All done
!  --------
   call i90_release()
   deallocate(ier)

   return


CONTAINS

   subroutine init_()
   integer ios, nerr
   nerr = max ( 32, nbins+1 )
   allocate ( gcBC%biomass_src(i1:i2,j1:j2), gcBC%biofuel_src(i1:i2,j1:j2), &
              gcBC%biomass_src_(i1:i2,j1:j2), &
              gcBC%ebcant1_src(i1:i2,j1:j2), gcBC%ebcant2_src(i1:i2,j1:j2), &
              gcBC%bc_ship_src(i1:i2,j1:j2), ier(nerr), stat=ios )
   if ( ios /= 0 ) rc = 100
   end subroutine init_

   subroutine final_(ierr)
   integer :: ierr
   integer ios
   deallocate ( gcBC%biomass_src, gcBC%biofuel_src, gcBC%bc_ship_src, &
                gcBC%biomass_src_, &
                gcBC%ebcant1_src, gcBC%ebcant2_src, ier, stat=ios )
   call i90_release()
   rc = ierr
   end subroutine final_

   end subroutine BC_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  BC_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine BC_GridCompRun ( gcBC, w_c, impChem, expChem, &
                               nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(BC_GridComp), intent(inout) :: gcBC   ! Grid Component
   type(Chem_Bundle), intent(inout) :: w_c      ! Chemical tracer fields   

! !INPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: impChem ! Import State
   integer, intent(in) :: nymd, nhms          ! time
   real, intent(in) :: cdt                    ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: expChem   ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine implements the so-called BC Driver. That 
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

   character(len=*), parameter :: myname = 'BC_GridCompRun'
   character(len=*), parameter :: Iam = myname

   integer :: ier(32), idiag
   integer :: i1, i2, im, j1, j2, jm, nbins, n1, n2, km, n, ios
   integer :: i, j, k, nymd1, nhms1, ijl, ijkl
   real :: qmax, qmin
   real :: qUpdate, delq
   real, pointer :: dqa(:,:), drydepositionfrequency(:,:)
   type(Chem_Array), pointer :: fluxout

!  Input fields from fvGCM
!  -----------------------
   real, pointer, dimension(:,:)   :: frlake, frocean, frseaice, &
                                      oro, u10m, v10m, &
                                      ustar, precc, precl,                &
                                      pblh, shflux, z0h, hsurf
   real, pointer, dimension(:,:,:) :: dqcond, tmpu, rhoa, u, v, hghte, ple

!  Additional needs for GOCART convective diagnostic
   real, pointer, dimension(:,:,:)       ::  cmfmc, qccu, dtrain
   real, pointer, dimension(:,:)         ::  area
   real*8, allocatable, dimension(:,:,:) ::  cmfmc_, qccu_, dtrain_, &
                                             airmass_, airmol_, vud_, &
                                             delz_, delp_
   real*8, allocatable                   ::  tc_(:,:,:,:), bcnv_(:,:,:)
   real*8, allocatable                   ::  area_(:,:), frlake_(:,:), &
                                             frocean_(:,:), frseaice_(:,:)
   integer*4                             ::  icdt

   real, pointer    :: BC_radius(:), BC_rhop(:)
   integer          :: rhFlag

#define EXPORT     expChem

#define ptrBCWT       BC_wet
#define ptrBCSV       BC_conv
#define ptrBCEM       BC_emis
#define ptrBCDP       BC_dep
#define ptrBCSD       BC_set

#define ptrBCMASS     BC_mass
#define ptrBCEMAN     BC_emisAN
#define ptrBCEMBB     BC_emisBB
#define ptrBCEMBF     BC_emisBF
#define ptrBCHYPHIL   BC_toHydrophilic
#define ptrBCSMASS    BC_sfcmass
#define ptrBCCMASS    BC_colmass
#define ptrBCEXTTAU   BC_exttau
#define ptrBCSCATAU   BC_scatau
#define ptrBCCONC     BC_conc
#define ptrBCEXTCOEF  BC_extcoef
#define ptrBCSCACOEF  BC_scacoef
#define ptrBCANGSTR   BC_angstrom
#define ptrBCFLUXU    BC_fluxu
#define ptrBCFLUXV    BC_fluxv


   
   integer :: STATUS

#include "BC_GetPointer___.h"


!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm

   km = w_c%grid%km
   nbins = w_c%reg%n_BC
   n1  = w_c%reg%i_BC
   n2  = w_c%reg%j_BC

   ijl  = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   ijkl = ijl * km



! Update emissions/production if necessary (daily)
!  -----------------------------------------------
   if(gcBC%nymd .ne. nymd) then

!   Biomass Burning -- select on known inventories
!   ----------------------------------------------
!   Daily files (e.g., MODIS) or GFED v.2 (1997 - 2005 valid)
    if (  index(gcBC%bb_srcfilen,'%') .gt. 0 .or. &
          index(gcBC%bb_srcfilen,'gfed') .gt. 0 ) then  
       nymd1 = nymd
       nhms1 = 120000

!   Assume GFED climatology or Martin (Duncan) climatology
    else                                            
       nymd1 = (gcBC%nymd_bb/10000)*10000 + mod ( nymd, 10000 )
       nhms1 = 120000
    end if

    call Chem_UtilMPread ( gcBC%bb_srcfilen, 'biomass', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcBC%biomass_src, cyclic=.true., &
                           grid = w_c%grid_esmf  )


!   Biofuel and anthropogenic emissions (inventories)
!   -------------------------------------------------
!    nymd1 = (gcBC%nymd_bf/10000)*10000 + mod ( nymd, 10000 )
    nymd1 = nymd
    nhms1 = 120000
    call Chem_UtilMPread ( gcBC%bf_srcfilen, 'biofuel', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcBC%biofuel_src, cyclic=.true., &
                           grid = w_c%grid_esmf  )

!    nymd1 = gcBC%nymd_ebcant1
    nymd1 = nymd
    nhms1 = 120000
    call Chem_UtilMPread ( gcBC%ebcant1_srcfilen, 'antebc1', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcBC%ebcant1_src, cyclic=.true., &
                           grid = w_c%grid_esmf  )

!    nymd1 = gcBC%nymd_ebcant2
    nymd1 = nymd
    nhms1 = 120000
    call Chem_UtilMPread ( gcBC%ebcant2_srcfilen, 'antebc2', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcBC%ebcant2_src, grid = w_c%grid_esmf  )

!   Ship based BC emissions
!    nymd1 = gcBC%nymd_bc_ship
    nymd1 = nymd
    nhms1 = 120000
    call Chem_UtilMPread ( gcBC%bc_ship_srcfilen, 'bc_ship', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcBC%bc_ship_src, cyclic=.true., &
                           grid = w_c%grid_esmf  )



!   As a safety check, where value is undefined set to 0
    do j = j1, j2
     do i = i1, i2
      if(1.01*gcBC%biomass_src(i,j) .gt. undefval) gcBC%biomass_src(i,j) = 0.
      if(1.01*gcBC%biofuel_src(i,j) .gt. undefval) gcBC%biofuel_src(i,j) = 0.
      if(1.01*gcBC%ebcant1_src(i,j) .gt. undefval) gcBC%ebcant1_src(i,j) = 0.
      if(1.01*gcBC%ebcant2_src(i,j) .gt. undefval) gcBC%ebcant2_src(i,j) = 0.
      if(1.01*gcBC%bc_ship_src(i,j) .gt. undefval) gcBC%bc_ship_src(i,j) = 0.
     enddo
    enddo


#ifdef DEBUG
    call pmaxmin ( 'BC: biomass', gcBC%biomass_src, qmin, qmax, ijl,1, 1. )
    call pmaxmin ( 'BC: biofuel', gcBC%biofuel_src, qmin, qmax, ijl,1, 1. )
    call pmaxmin ( 'BC: ebcant1', gcBC%ebcant1_src, qmin, qmax, ijl,1,1.)
    call pmaxmin ( 'BC: ebcant2', gcBC%ebcant2_src, qmin, qmax, ijl,1,1.)
    call pmaxmin ( 'BC: bc_ship', gcBC%bc_ship_src, qmin, qmax, ijl,1,1.)
#endif

!   Save this in case we need to apply diurnal cycle
!   ------------------------------------------------
   if ( w_c%diurnal_bb ) then
        gcBC%biomass_src_(:,:) = gcBC%biomass_src(:,:)
   end if

    gcBC%nymd = nymd

   endif


!  Apply diurnal cycle if so desired
!  ---------------------------------
   if ( w_c%diurnal_bb ) then
      call Chem_BiomassDiurnal ( gcBC%biomass_src, gcBC%biomass_src_,   &
                                 w_c%grid%lon(:,:)*radToDeg, &
                                 w_c%grid%lat(:,:)*radToDeg, nhms, cdt )      
   end if


#ifdef DEBUG
   do n = n1, n2
      call pmaxmin ( 'BC: q_beg', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                   ijl, km, 1. )
   end do
#endif

   allocate( fluxout )
   allocate( fluxout%data2d(i1:i2,j1:j2), dqa(i1:i2,j1:j2), &
             drydepositionfrequency(i1:i2,j1:j2), stat=STATUS)
   VERIFY_(STATUS)


!  Get 2D Imports
!  --------------
   ier = 0
   call MAPL_GetPointer ( impChem, frlake,   'FRLAKE',  rc=ier(1) )
   call MAPL_GetPointer ( impChem, oro,      'LWI',     rc=ier(2) )
   call MAPL_GetPointer ( impChem, u10m,     'U10M',    rc=ier(3) )
   call MAPL_GetPointer ( impChem, v10m,     'V10M',    rc=ier(4) )
   call MAPL_GetPointer ( impChem, ustar,    'USTAR',   rc=ier(5) )
   call MAPL_GetPointer ( impChem, precc,    'CN_PRCP', rc=ier(6) )
   call MAPL_GetPointer ( impChem, precl,    'NCN_PRCP',rc=ier(7) )
   call MAPL_GetPointer ( impChem, pblh,     'ZPBL',    rc=ier(8) )
   call MAPL_GetPointer ( impChem, shflux,   'SH',      rc=ier(9) )
   call MAPL_GetPointer ( impChem, z0h,      'Z0H',     rc=ier(10) )
   call MAPL_GetPointer ( impChem, area,     'AREA',    rc=ier(11) )
   call MAPL_GetPointer ( impChem, frocean,  'FROCEAN', rc=ier(12) )
   call MAPL_GetPointer ( impChem, frseaice, 'FRACI',   rc=ier(13) )

!  Get 3D Imports
!  --------------
   call MAPL_GetPointer ( impChem, dqcond, 'DQDT',    rc=ier(14) )
   call MAPL_GetPointer ( impChem, tmpu,   'T',       rc=ier(15) )
   call MAPL_GetPointer ( impChem, rhoa,   'AIRDENS', rc=ier(16) )
   call MAPL_GetPointer ( impChem, u,      'U',       rc=ier(17) )
   call MAPL_GetPointer ( impChem, v,      'V',       rc=ier(18) )
   call MAPL_GetPointer ( impChem, hghte,  'ZLE',     rc=ier(19) )
   call MAPL_GetPointer ( impChem, ple,    'PLE',     rc=ier(20) )
   call MAPL_GetPointer ( impChem, qccu,   'CNV_QC',  rc=ier(21) )
   call MAPL_GetPointer ( impChem, cmfmc,  'CNV_MFC', rc=ier(22) )
   call MAPL_GetPointer ( impChem, dtrain, 'CNV_MFD', rc=ier(23) )

!  Unlike GEOS-4 hghte is defined for km+1
!  ---------------------------------------
   hsurf => hghte(i1:i2,j1:j2,km) ! Recall: GEOS-5 has edges with k in [0,km]
    

   if ( any(ier(1:23) /= 0) ) then
        rc = 10 
        return
   end if

#ifdef DEBUG

   call pmaxmin('BC: frlake     ', frlake  , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: frocean    ', frocean , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: frseaice   ', frseaice, qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: area       ', area    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: oro        ', oro     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: u10m       ', u10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: v10m       ', v10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: ustar      ', ustar   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: precc      ', precc   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: precl      ', precl   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: pblh       ', pblh    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: shflux     ', shflux  , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: z0h        ', z0h     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: hsurf      ', hsurf   , qmin, qmax, ijl,1, 1. )

   call pmaxmin('BC: dqcond     ', dqcond  , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: tmpu       ', tmpu    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: rhoa       ', rhoa    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: u          ', u       , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: v          ', v       , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: hghte      ', hghte   , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: qccu       ', qccu    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: cmfmc      ', cmfmc   , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: dtrain     ', dtrain  , qmin, qmax, ijkl,1, 1. )

#endif

!  BC Source
!  -----------
   call BC_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcBC, w_c, &
                      pblh, tmpu, rhoa, BC_emis, &
                      BC_emisAN, BC_emisBB, BC_emisBF, rc )

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('BC: q_emi', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), &
                    qmin, qmax, ijl, km, 1. )
   end do
#endif

!  Ad Hoc transfer of hydrophobic to hydrophilic aerosols
!  Following Chin's parameterization, the rate constant is
!  k = 4.63e-6 s-1 (.4 day-1; e-folding time = 2.5 days)
   if(associated(BC_toHydrophilic%data2d)) &
     BC_toHydrophilic%data2d(i1:i2,j1:j2) = 0.0

   do k = 1, km
    do j = j1, j2
     do i = i1, i2
      qUpdate = w_c%qa(n1)%data3d(i,j,k)*exp(-4.63e-6*cdt)
      qUpdate = max(qUpdate,1e-32)
      delq = max(0.,w_c%qa(n1)%data3d(i,j,k)-qUpdate)
      w_c%qa(n1)%data3d(i,j,k) = qUpdate
      w_c%qa(n2)%data3d(i,j,k) = w_c%qa(n2)%data3d(i,j,k)+delq
      if(associated(BC_toHydrophilic%data2d)) &
       BC_toHydrophilic%data2d(i,j) = BC_toHydrophilic%data2d(i,j) &
        + delq*w_c%delp(i,j,k)/grav/cdt
     end do
    end do
   end do

!  BC Settling
!  -----------
   allocate( BC_radius(nbins), BC_rhop(nbins) )
   BC_radius(:) = 0.35e-6  ! radius for settling [m]
   BC_rhop(:)   = 1800.    ! density for setting [kg m-3]
   rhFlag       = 0        ! settle like dry particles
   call Chem_Settling ( i1, i2, j1, j2, km, n1, n2, nbins, rhFlag, &
                        BC_radius, BC_rhop, cdt, w_c, tmpu, rhoa, hsurf,    &
                        hghte, BC_set, rc )
   deallocate( BC_radius, BC_rhop)

!  BC Deposition
!  -----------
   drydepositionfrequency = 0.
   call DryDepositionGOCART( i1, i2, j1, j2, km, &
                             tmpu, rhoa, hghte, oro, ustar, &
                             pblh, shflux, z0h, drydepositionfrequency, rc )
    
   do n = 1, nbins
    dqa = 0.
    dqa = max(0.0, w_c%qa(n1+n-1)%data3d(:,:,km)*(1.-exp(-drydepositionfrequency*cdt)))
    w_c%qa(n1+n-1)%data3d(:,:,km) = &
            w_c%qa(n1+n-1)%data3d(:,:,km) - dqa
    if( associated(BC_dep(n)%data2d) ) &
     BC_dep(n)%data2d = dqa*w_c%delp(:,:,km)/grav/cdt
   end do

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('BC: q_dry', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif


!  BC Large-scale Wet Removal
!  --------------------------
!  Hydrophobic mode (first tracer) is not removed
   if(associated(BC_wet(1)%data2d)) BC_wet(1)%data2d = 0.
!  Hydrophilic mode (second tracer) is removed
   do n = nbins, nbins
    w_c%qa(n1+n-1)%fwet = 1.
    call WetRemovalGOCART(i1, i2, j1, j2, km, n1+n-1, n1+n-1, cdt, &
                          w_c%qa, ple, tmpu, rhoa, dqcond, precc, precl, &
                          fluxout, rc )
    if(associated(BC_wet(n)%data2d)) BC_wet(n)%data2d = fluxout%data2d
   end do

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('BC: q_wet', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Black Carbon Convective-scale Mixing and Wet Removal
!  ----------------------------------------------------
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
   call convection(i1, i2, j1, j2, km, n1, n2, icdt, 'carbon', &
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
   if(associated(BC_conv(1)%data2d)) BC_conv(1)%data2d = 0.0
   if(associated(BC_conv(2)%data2d)) BC_conv(2)%data2d = -bcnv_(:,:,n2)/area_/icdt

   deallocate(cmfmc_, qccu_, dtrain_, tc_, airmass_, &
              delz_, vud_, delp_, airmol_, bcnv_, &
              area_, frlake_, frocean_, frseaice_, __STAT__ )

!  Compute the desired output diagnostics here
!  Ideally this will go where chemout is called in fvgcm.F since that
!  will reflect the distributions after transport, etc.
!  ------------------------------------------------------------------
   call BC_Compute_Diags(i1, i2, j1, j2, km, nbins, gcBC, w_c, tmpu, rhoa, u, v, &
                         BC_sfcmass, BC_colmass, BC_mass, BC_exttau, &
                         BC_scatau, BC_conc, BC_extcoef, BC_scacoef, BC_angstrom, &
                         BC_fluxu, BC_fluxv, rc)


!  Clean up
!  --------
   deallocate(fluxout%data2d)
   deallocate(fluxout, dqa, drydepositionfrequency, stat=ios )

   return

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  BC_Emission - Adds Black Carbon emission for one timestep
!             We have emissions from 4 sources, which are distributed
!             differently in the vertical
!             1) biomass burning - uniformly mixed in PBL
!             2) biofuel sources - emitted into lowest 100 m
!             3) anthropogenic l1 - emitted into lowest 100 m
!             4) anthropogenic l2 - emitted into 100 - 500 m levels
!
! !INTERFACE:
!

   subroutine BC_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcBC, w_c, &
                            pblh, tmpu, rhoa, BC_emis, &
                            BC_emisAN, BC_emisBB, BC_emisBF, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   type(BC_GridComp), intent(in)    :: gcBC       ! BC Grid Component
   real, pointer, dimension(:,:)    :: pblh
   real, pointer, dimension(:,:,:)  :: tmpu
   real, pointer, dimension(:,:,:)  :: rhoa

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: BC_emis(nbins) ! BC emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: BC_emisAN      ! BC emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: BC_emisBB      ! BC emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: BC_emisBF      ! BC emissions, kg/m2/s
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'BC_Emission'

! !DESCRIPTION: Updates the BC concentration with emissions every timestep
!
! !REVISION HISTORY:
!
!  06Nov2003, Colarco
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer  ::  i, j, k, m, n, ios, ijl
   integer  ::  n1, n2
                                       ! pressure at 100m, 500m, & PBLH
   real, dimension(i1:i2,j1:j2) :: p100, p500, pPblh  
   real, dimension(i1:i2,j1:j2) :: p0, z0, ps
   real :: p1, z1, dz, delz, delp, f100, f500, fPblh
   real :: qmax, qmin, eBiofuel, eBiomass

   real, dimension(i1:i2,j1:j2) :: factor, srcHydrophobic, srcHydrophilic
   real, dimension(i1:i2,j1:j2) :: srcBiofuel, srcBiomass, srcAnthro
   real                         :: srcAll, zpbl, maxAll

!  Initialize local variables
!  --------------------------
   n1  = w_c%reg%i_BC
   n2  = w_c%reg%j_BC
   ijl = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   eBiomass = gcBC%eBiomassBurning
   eBiofuel = gcBC%eBiofuel

!  Zero diagnostic accumulators
   do n = 1, nbins
     if( associated(BC_emis(n)%data2d) ) BC_emis(n)%data2d = 0.0
   end do
     if(associated(BC_emisAN%data2d) )   BC_emisAN%data2d  = 0.0
     if(associated(BC_emisBF%data2d) )   BC_emisBF%data2d  = 0.0
     if(associated(BC_emisBB%data2d) )   BC_emisBB%data2d  = 0.0

!  Determine surface pressure
!  AMS Note: pass this in
!  --------------------------
   ps = 0.0
   do k = 1, km
    ps(i1:i2,j1:j2) = ps(i1:i2,j1:j2) + w_c%delp(i1:i2,j1:j2,k)
   end do

!  Find the pressure of the 100m, 500m, and PBLH altitudes
!  AMS Note: this could be greatly simplified by using ze/zm and having a
!      generic routine from the bottom up with an early exit condition
!  -----------------------------------------------------------------------
   p0 = ps  
   z0(i1:i2,j1:j2) = 0.
   do k = km, 1, -1
    do j = j1, j2
     do i = i1, i2
      p1 = p0(i,j) - w_c%delp(i,j,k)
      dz = w_c%delp(i,j,k)/rhoa(i,j,k)/grav
      z1 = z0(i,j)+dz
      if(z0(i,j) .lt. 100 .and. z1 .ge. 100.) then
       delz = z1-100.
       delp = delz*rhoa(i,j,k)*grav
       p100(i,j) = p1+delp
      endif
      if(z0(i,j) .lt. 500 .and. z1 .ge. 500.) then
       delz = z1-500.
       delp = delz*rhoa(i,j,k)*grav
       p500(i,j) = p1+delp
      endif
      zpbl = max ( pblh(i,j), 100. )
      if(z0(i,j) .lt. zpbl .and. z1 .ge. zpbl ) then
       delz = z1-zpbl
       delp = delz*rhoa(i,j,k)*grav
       pPblh(i,j) = p1+delp
      endif
      p0(i,j) = p1
      z0(i,j) = z1
     end do
    end do
   end do

#if 0
   call pmaxmin ( 'BC: p100   ', p100,  qmin, qmax, ijl, 1, 1. )
   call pmaxmin ( 'BC: p500   ', p500,  qmin, qmax, ijl, 1, 1. )
   call pmaxmin ( 'BC: pPBL   ', pPBLh, qmin, qmax, ijl, 1, 1. )
#endif

!  Now update the tracer mixing ratios with the aerosol sources
!  ------------------------------------------------------------
   p0 = ps
K_LOOP: do k = km, 1, -1

!!!    print *, 'BC_Emissions: getting emissions for layer ', k

!   First determine emissions for this layer
!   ----------------------------------------
    do j = j1, j2
     do i = i1, i2

      p1 = p0(i,j) - w_c%delp(i,j,k)

!     Pressure @ 100m
!     ---------------
      f100 = 0.
      if(p1 .ge. p100(i,j)) f100 = w_c%delp(i,j,k)/(ps(i,j)-p100(i,j))
      if(p1 .lt. p100(i,j) .and. p0(i,j) .ge. p100(i,j)) &
       f100 = (p0(i,j)-p100(i,j))/(ps(i,j)-p100(i,j))

!     Pressure @ 500m
!     ---------------
      f500 = 0.
      if ( p0(i,j) .ge. p100(i,j) .and. p1 .lt. p100(i,j) .and. p1 .ge. p500(i,j)) &
       f500 = (p100(i,j)-p1)/(p100(i,j)-p500(i,j))
      if(p0(i,j) .lt. p100(i,j) .and. p1 .ge. p500(i,j)) &
       f500 = w_c%delp(i,j,k)/(p100(i,j)-p500(i,j))
      if(p0(i,j) .ge. p500(i,j) .and. p1 .lt. p500(i,j)) &
       f500 = (p0(i,j)-p500(i,j))/(p100(i,j)-p500(i,j))

!     Pressure @ PBL height
!     ---------------------
      fPblh = 0.
      if(p1 .ge. pPblh(i,j)) fPblh = w_c%delp(i,j,k)/(ps(i,j)-pPblh(i,j))
      if(p1 .lt. pPblh(i,j) .and. p0(i,j) .ge. pPblh(i,j)) &
       fPblh = (p0(i,j)-pPblh(i,j))/(ps(i,j)-pPblh(i,j))

!     Sources by class in kg m-2 s-1
!     ------------------------------
      srcBiofuel(i,j) = f100 *eBiofuel*gcBC%biofuel_src(i,j)
      srcAnthro(i,j)  = f100 *         gcBC%ebcant1_src(i,j) &
                      + f500 *         gcBC%ebcant2_src(i,j) &
                      + f100 *         gcBC%bc_ship_src(i,j)
      srcBiomass(i,j) = fPblh*eBiomass*gcBC%biomass_src(i,j)

      srcAll = srcBiofuel(i,j) + srcAnthro(i,j) + srcBiomass(i,j)
      srcHydrophobic(i,j) =     gcBC%fHydrophobic  * srcAll
      srcHydrophilic(i,j) = (1.-gcBC%fHydrophobic) * srcAll

!     Update pressure of lower level
!     ------------------------------
      p0(i,j) = p1

     end do ! i
    end do  ! j

!   Determine global max/min
!   ------------------------
    call pmaxmin ( 'BC: Phobic ', srcHydrophobic, qmin, qmax, ijl, 1, 0. )
    maxAll = abs(qmax) + abs(qmin)
    call pmaxmin ( 'BC: Philic ', srcHydrophilic, qmin, qmax, ijl, 1, 0. )
    maxAll = max ( maxAll, abs(qmax) + abs(qmin) )

!   If emissions are zero at this level (globally), we are done
!   -----------------------------------------------------------
    if ( maxAll .eq. 0.0 ) exit K_LOOP

!   Update concentrations at this layer
!   The "1" element is hydrophobic 
!   The "2" element is hydrophilic
!   -----------------------------------    
    factor = cdt * grav / w_c%delp(:,:,k)

    w_c%qa(n1)%data3d(:,:,k) = w_c%qa(n1)%data3d(:,:,k) & 
                             + factor * srcHydrophobic 

    w_c%qa(n2)%data3d(:,:,k) = w_c%qa(n2)%data3d(:,:,k) & 
                             + factor * srcHydrophilic

!   Fill in diagnostics if requested
!   --------------------------------
    if ( associated(BC_emis(1)%data2d)) &
                    BC_emis(1)%data2d = BC_emis(1)%data2d + srcHydrophobic

    if ( associated(BC_emis(2)%data2d)) &
                    BC_emis(2)%data2d = BC_emis(2)%data2d + srcHydrophilic

    if ( associated(BC_emisBF%data2d)) &
                    BC_emisBF%data2d  = BC_emisBF%data2d  + srcBiofuel

    if ( associated(BC_emisBB%data2d)) &
                    BC_emisBB%data2d  = BC_emisBB%data2d  + srcBiomass

    if ( associated(BC_emisAN%data2d)) &
                    BC_emisAN%data2d  = BC_emisAN%data2d  + srcAnthro

   end do K_LOOP

   rc = 0

   end subroutine BC_Emission

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  BC_Compute_Diags - Calculate dust 2D diagnostics
!
! !INTERFACE:
!

   subroutine BC_Compute_Diags ( i1, i2, j1, j2, km, nbins, gcBC, w_c, tmpu, rhoa, u, v, &
                                 sfcmass, colmass, mass, exttau, scatau, &
                                 conc, extcoef, scacoef, angstrom, fluxu, fluxv, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   type(BC_GridComp), intent(inout):: gcBC     ! BC Grid Component
   type(Chem_Bundle), intent(in)   :: w_c      ! Chem Bundle
   real, pointer, dimension(:,:,:) :: tmpu     ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa     ! air density [kg m-3]
   real, pointer, dimension(:,:,:) :: u        ! east-west wind [m s-1]
   real, pointer, dimension(:,:,:) :: v        ! north-south wind [m s-1]

! !OUTPUT PARAMETERS:
   type(Chem_Array), intent(inout)  :: sfcmass  ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: colmass  ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: mass     ! 3d mass mixing ratio kg/kg
   type(Chem_Array), intent(inout)  :: exttau   ! ext. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: scatau   ! sct. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: conc     ! 3d mass concentration, kg/m3
   type(Chem_Array), intent(inout)  :: extcoef  ! 3d ext. coefficient, 1/m
   type(Chem_Array), intent(inout)  :: scacoef  ! 3d scat.coefficient, 1/m
   type(Chem_Array), intent(inout)  :: angstrom ! 470-870 nm Angstrom parameter
   type(Chem_Array), intent(inout)  :: fluxu    ! Column mass flux in x direction
   type(Chem_Array), intent(inout)  :: fluxv    ! Column mass flux in y direction
   integer, intent(out)             :: rc       ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Calculates some simple 2d diagnostics from the BC fields
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
   character(len=*), parameter :: myname = 'BC_Compute_Diags'
   integer :: i, j, k, n, n1, n2, ios, nch, idx
   real :: tau, ssa
   character(len=255) :: qname
   real, dimension(i1:i2,j1:j2) :: tau470, tau870
   real    :: ilam550, ilam470, ilam870
   logical :: do_angstrom


!  Initialize local variables
!  --------------------------
   n1  = w_c%reg%i_BC
   n2  = w_c%reg%j_BC
   nch   = gcBC%mie_tables%nch

!  Get the wavelength indices
!  --------------------------
!  Must provide ilam550 for AOT calculation
   ilam550 = 1.
   ilam470 = 0.
   ilam870 = 0.
   if(nch .gt. 1) then
    do i = 1, nch
     if ( gcBC%mie_tables%channels(i) .ge. 5.49e-7 .and. &
          gcBC%mie_tables%channels(i) .le. 5.51e-7) ilam550 = i
     if ( gcBC%mie_tables%channels(i) .ge. 4.69e-7 .and. &
          gcBC%mie_tables%channels(i) .le. 4.71e-7) ilam470 = i
     if ( gcBC%mie_tables%channels(i) .ge. 8.69e-7 .and. &
          gcBC%mie_tables%channels(i) .le. 8.71e-7) ilam870 = i
    enddo
   endif

!  Determine if going to do Angstrom parameter calculation
!  -------------------------------------------------------
   do_angstrom = .false.
!  If both 470 and 870 channels provided (and not the same) then
!  possibly will do Angstrom parameter calculation
   if(ilam470 .ne. 0. .and. &
      ilam870 .ne. 0. .and. &
      ilam470 .ne. ilam870) do_angstrom = .true.


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

      if( associated(exttau%data2d) ) then
       exttau%data2d(i1:i2,j1:j2) = 0.
      endif
      if( associated(scatau%data2d) ) then
       scatau%data2d(i1:i2,j1:j2) = 0.
      endif

      if( associated(extcoef%data3d)) then 
       extcoef%data3d(i1:i2,j1:j2,1:km) = 0.
      endif
      if( associated(scacoef%data3d)) then
       scacoef%data3d(i1:i2,j1:j2,1:km) = 0.
      endif 

      do n = 1, nbins

!      Select the name for species
       qname = trim(w_c%reg%vname(n+n1-1))
       idx = Chem_MieQueryIdx(gcBC%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

       do k = 1, km
        do j = j1, j2
         do i = i1, i2
          call Chem_MieQuery(gcBC%mie_tables, idx, ilam550, &
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
          if( associated(exttau%data2d) ) then
           exttau%data2d(i,j) = exttau%data2d(i,j) + tau
          endif
          if( associated(scatau%data2d) ) then
           scatau%data2d(i,j) = scatau%data2d(i,j) + tau*ssa
          endif

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
       qname = trim(w_c%reg%vname(n+n1-1))
       idx = Chem_MieQueryIdx(gcBC%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

       do k = 1, km
        do j = j1, j2
         do i = i1, i2

          call Chem_MieQuery(gcBC%mie_tables, idx, ilam470, &
              w_c%qa(n+n1-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
              w_c%rh(i,j,k), tau=tau)
          tau470(i,j) = tau470(i,j) + tau

          call Chem_MieQuery(gcBC%mie_tables, idx, ilam870, &
              w_c%qa(n+n1-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
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

   end subroutine BC_Compute_Diags

 end subroutine BC_GridCompRun

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  BC_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine BC_GridCompFinalize ( gcBC, w_c, impChem, expChem, &
                                    nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(BC_GridComp), intent(inout) :: gcBC   ! Grid Component

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

   character(len=*), parameter :: myname = 'BC_GridCompFinalize'
   rc=0
   return

 end subroutine BC_GridCompFinalize

 end module BC_GridCompMod

