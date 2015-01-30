#include "MAPL_Generic.h"

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  SU_GridCompMod --- SU Grid Component Class
!
! !INTERFACE:
!

   module  SU_GridCompMod

! !USES:

   USE ESMF
   USE MAPL_Mod

   use Chem_Mod              ! Chemistry Base Class
   use Chem_StateMod         ! Chemistry State
   use Chem_ConstMod, only: grav, von_karman, cpd, &   ! Constants !
                            undefval => undef, airMolWght => airmw
   use Chem_UtilMod          ! I/O
   use Chem_MieMod           ! Aerosol LU Tables, calculator
   use m_inpak90             ! Resource file management
   use m_die, only: die
   USE m_chars, ONLY: lowercase

   use m_StrTemplate
   use SulfateChemDriverMod
   use ConvectionMod         ! Offline convective mixing/scavenging
   use Chem_SettlingMod      ! Gravitiational Settling

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  SU_GridComp       ! The SU object 
   PUBLIC  SU_GridComp1      ! Single instance SU object

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  SU_GridCompInitialize
   PUBLIC  SU_GridCompRun
   PUBLIC  SU_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the (pre-ESMF) SU Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  18May2006 da Silva  Removed ensure postive, now in GOCART_GridComp
!  25Aug2009 Nielsen   Connections, usage of GMI Combo OH, H2O2, and NO3
!
!EOP
!-------------------------------------------------------------------------

! Note that the dates associated with the input files are a real mess
! Chem_UtilMPread cares about the date!
! Arbitrarily I set so2biomass_src, so2anthro_l1_src, and so2anthro_l2_src to 1971
! (of course these are not really valid for 1971)
! DMSO is valid 2000
! OH, NO3, H2O2 files are valid 2001
! Go figure...this is what happens when I get inputs from other people
! who are not the primary sources (e.g., Mian and Bian instead of
! geoschem...I get what they've got).

  type SU_GridComp1
        character(len=255) :: name
        character(len=255) :: iname           ! instance name
        character(len=255) :: rcfilen         ! resource file name
        character(len=255) :: maskFileName
        character(len=255) :: regionsString   ! Comma-delimited string of regions
        real, pointer      :: regionMask(:,:) ! regional mask
        integer :: instance                   ! instance number

        type(Chem_Mie), pointer :: mie_tables   ! aod LUTs
        real, pointer :: so2biomass_src_(:,:) ! before diurnal
        real, pointer :: so2biomass_src(:,:)
        real, pointer :: so2anthro_l1_src(:,:)  ! level 1
        real, pointer :: so2anthro_l2_src(:,:)  ! level 2
        real, pointer :: so2ship_src(:,:)
        real, pointer :: so4ship_src(:,:)
        real, pointer :: aircraft_fuel_src(:,:,:)
        real, pointer :: dmso_conc(:,:)
!       Special handling for volcanic emissions
        integer :: nvolc = 0
        real, pointer, dimension(:) :: vLat    => null(), &
                                       vLon    => null(), &
                                       vSO2    => null(), &
                                       vElev   => null(), &
                                       vCloud  => null()
!       Note that the OH, NO3, and H2O2 are from a geoschem run
!       Ideally would be from a run of the fv chemistry package!
        real, pointer :: oh_conc(:,:,:)
        real, pointer :: no3_mr(:,:,:)
        real, pointer :: h2o2_mr(:,:,:)
!       OH and NO3 are scaled every timestep.  H2O2 is replaced every
!       3 hours with the monthly value.  Hence, we need to save a value
!       somewhere!  For now we save the instantaneous value here.
        real, pointer :: h2o2_int(:,:,:)
        real :: fSO4ant         ! Fraction of anthropogenic emissions are SO4
        real :: eBiomassBurning ! Emission factor of Biomass Burning to SO2
        real :: eAircraftFuel   ! Emission factor to go from fuel to SO2
        real :: fMassSulfur     ! gram molar weight of S
        real :: fMassSO2        ! gram molar weight of SO2
        real :: fMassSO4        ! gram molar weight of SO4
        real :: fMassDMS        ! gram molar weight of DMS
        real :: fMassMSA        ! gram molar weight of MSA
        integer :: nDMS
        integer :: nSO2
        integer :: nSO4
        integer :: nMSA
        integer :: nymd          ! Update the emissions?
        integer :: nymd_oxidants ! Update the oxidant files?
        character(len=255) :: bb_srcfilen
        character(len=255) :: so2anthro_l1_srcfilen
        character(len=255) :: so2anthro_l2_srcfilen
        character(len=255) :: so2ship_srcfilen
        character(len=255) :: so4ship_srcfilen
        character(len=255) :: aircraft_fuel_srcfilen
        character(len=255) :: dmso_concfilen
        character(len=255) :: volcano_srcfilen
        character(len=255) :: oh_concfilen
        character(len=255) :: no3_mrfilen
        character(len=255) :: h2o2_mrfilen
        integer :: nymd_bb            = 0
        integer :: nymd_sanl1         = 0
        integer :: nymd_sanl2         = 0
        integer :: nymd_so2ship      = 0
        integer :: nymd_so4ship      = 0
        integer :: nymd_aircraft_fuel = 0
        integer :: nymd_dmso          = 0
        integer :: nymd_oh            = 0
        integer :: nymd_no3           = 0
        integer :: nymd_h2o2          = 0
	LOGICAL :: using_GMI_OH
	LOGICAL :: using_GMI_NO3
	LOGICAL :: using_GMI_H2O2
	LOGICAL :: export_H2O2
!       parameters for sulfate gravitational settling
        integer :: rhFlag          !flag for sulfate growth parameterization
        real, pointer :: radius(:) !particle effective radius [um]
        real, pointer :: rhop(:)   ! SU class density [kg m-3]
  end type SU_GridComp1

  type SU_GridComp
     integer                     ::  n          ! number of instances 
     type(Chem_Mie), pointer     ::  mie_tables ! aod LUTs
     type(SU_GridComp1), pointer ::  gcs(:)     ! instances
  end type SU_GridComp

  real, parameter :: OCEAN=0.0, LAND = 1.0, SEA_ICE = 2.0
  real, parameter :: pi = 3.1415, rearth = 6.37e6
  real, parameter :: radTODeg = 57.2957795
  real, parameter :: rH2O2 = 34./airMolWght

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_GridCompInitialize --- Initialize SU_GridComp
!
! !INTERFACE:
!

   subroutine SU_GridCompInitialize ( gcSU, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real, intent(in) :: cdt                     ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(SU_GridComp), intent(inout) :: gcSU   ! Grid Component
   type(ESMF_State), intent(inout)  :: impChem  ! Import State
   type(ESMF_State), intent(inout)  :: expChem  ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the SU Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'SU_GridCompInitialize'
   character(len=255) :: rcbasen = 'SU_GridComp'
   CHARACTER(LEN=255) :: name
   
   integer i, ier, n

!  Load resource file
!  ------------------
   call i90_loadf ( trim(rcbasen)//'.rc', ier )
   if ( ier .NE. 0 ) then
      rc = 10
      return
   end if

!  Parse resource file
!  -------------------
   CALL I90_label ( 'SU_instances:', ier )
   if ( ier .NE. 0 ) then
      rc = 20
      return
   end if

!  First determine how many instances we have
!  ------------------------------------------   
   n = 0
   do while ( ier .EQ. 0 )
      CALL I90_gtoken( name, ier )
      n = n + 1
   end do
   if ( n .EQ. 0 ) then
      rc = 30
      return
   end if
   
!  We have 4 tracers for each instance of SU
!  Chem_Registry provides the number (total)
!  of tracers to be run.  Therefore n*4 must
!  be >= to that number or else we don't have
!  enough instances requested.
!  --------------------------------------------------------
   if ( n*4 .lt. w_c%reg%n_SU ) then
        rc = 35
        return
   end if
   n = min(n,w_c%reg%n_SU/4 )
   gcSU%n = n

!  Next allocate necessary memory
!  ------------------------------
   allocate ( gcSU%gcs(n), stat=ier )    
   if ( ier .NE. 0 ) then
      rc = 40
      return
   end if

!  Record name of each instance
!  ----------------------------
   CALL I90_label ( 'SU_instances:', ier )
   do i = 1, n
      CALL I90_gtoken( name, ier )
      if ( ier .NE. 0 ) then
         rc = 40
         return
      end if
                                            ! resource file name
      gcSU%gcs(i)%rcfilen = trim(rcbasen)//'---'//trim(name)//'.rc'
      gcSU%gcs(i)%instance = i              ! instance number 
      IF(TRIM(name) == "full" ) THEN
       gcSU%gcs(i)%iname = " "              ! blank instance name for full (1)
      ELSE
       gcSU%gcs(i)%iname = TRIM(name)       ! instance name for others
      END IF
   end do    

!  Next initialize each instance
!  -----------------------------
   do i = 1, gcSU%n
      IF(MAPL_AM_I_ROOT()) THEN
       PRINT *," "
       PRINT *,myname,": Initializing instance ",TRIM(gcSU%gcs(i)%iname)," [",gcSU%gcs(i)%instance,"]"
      END IF
      call SU_SingleInstance_ ( SU_GridCompInitialize1_, i, &
                                gcSU%gcs(i), w_c, impChem, expChem,  &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then
         rc = 1000+ier
         return
      end if
      gcSU%gcs(i)%mie_tables => gcSU%mie_tables
   end do

!  All done
!  --------
   CALL I90_FullRelease( ier )
   IF( ier /= 0 ) THEN
    PRINT *,myname,": I90_FullRelease not successful."
    rc = 40
   END IF


 end subroutine SU_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_GridCompRun --- Run SU_GridComp
!
! !INTERFACE:
!

   subroutine SU_GridCompRun ( gcSU, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms           ! time
   REAL,    INTENT(IN) :: cdt                  ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(SU_GridComp), INTENT(INOUT) :: gcSU     ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Runs the CO Grid Component. Multiple instance
!               version.
!
! !REVISION HISTORY:
!
!  27Feb2008  da Silva  Introduced multiple instances
!
!EOP
!-------------------------------------------------------------------------

   integer i, ier

   do i = 1, gcSU%n
      call SU_SingleInstance_ ( SU_GridCompRun1_, i, &
                                gcSU%gcs(i), w_c, impChem, expChem, &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then
         rc = i * 1000+ier
         return
      end if
   end do

 end subroutine SU_GridCompRun


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_GridCompFinalize --- Initialize SU_GridComp
!
! !INTERFACE:
!

   subroutine SU_GridCompFinalize ( gcSU, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms           ! time
   REAL,    INTENT(IN) :: cdt                  ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(SU_GridComp), INTENT(INOUT) :: gcSU     ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Finalizes the SU Grid Component. Multiple instance
!               version.
!
! !REVISION HISTORY:
!
!  27Feb2008  da Silva  Introduced multiple instances
!
!EOP
!-------------------------------------------------------------------------

   integer i, ier

   do i = 1, gcSU%n
      call SU_SingleInstance_ ( SU_GridCompFinalize1_, i, &
                                gcSU%gcs(i), w_c, impChem, expChem, &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then
         rc = i * 1000+ier
         return
      end if
   end do

   deallocate ( gcSU%gcs, stat=ier )    
   gcSU%n = -1

 end subroutine SU_GridCompFinalize


!--------------------------------------------------------------------------

!                      Single Instance Methods

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_GridCompInitialize --- Initialize SU_GridComp
!
! !INTERFACE:
!

   subroutine SU_GridCompInitialize1_ ( gcSU, w_c, impChem, expChem, &
                                        nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real, intent(in) :: cdt                     ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(SU_GridComp1), intent(inout) :: gcSU   ! Grid Component
   type(ESMF_State), intent(inout)   :: impChem  ! Import State
   type(ESMF_State), intent(inout)   :: expChem  ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: Initializes the SU Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'SU_GridCompInitialize1'


   character(len=255) :: rcfilen = 'SU_GridComp.rc'
   integer :: ios, n, nymd1, nhms1
   integer :: i1, i2, im, j1, j2, jm, km, nbins, n1, n2, nbins_rc
   integer :: i, j, k
   integer :: nTimes, begTime, incSecs
   integer, allocatable :: ier(:)
   real, allocatable :: buffer(:,:)
   real :: qmax, qmin
   CHARACTER(LEN=255) :: string
   LOGICAL :: NoRegionalConstraint 
   real    :: radius, rhop
   integer :: irhFlag

   REAL :: limitN, limitS, radtoDeg
   REAL, ALLOCATABLE :: var2d(:,:)


   rcfilen = gcSU%rcfilen
   gcSU%name = 'SU Constituent Package'
   radTODeg = 57.2957795


!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   km = w_c%grid%km
   nbins = w_c%reg%n_SU
   n1  = w_c%reg%i_SU
   n2  = w_c%reg%j_SU

!  Check on the number of bins
   if(nbins .ne. 4) then
    rc = 1
    return
   endif


   call init_()
   if ( rc /= 0 ) return

!  Set the bin assignments to the gcSU grid component
   gcSU%nDMS = 1
   gcSU%nSO2 = 2
   gcSU%nSO4 = 3
   gcSU%nMSA = 4


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

   call i90_label ( 'number_su_classes:', ier(1) )
   nbins_rc = i90_gint ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(20)
      return
   end if
   if ( nbins_rc /= nbins ) then
      call final_(25)
      return
   end if


!  SU sources files
!  ---------------------
   call i90_label ( 'bb_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%bb_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'volcano_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(31)
      return
   else
      call i90_gtoken ( gcSU%volcano_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(41)
         return
      end if
   end if

   call i90_label ( 'so2_anthro_l1_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(32)
      return
   else
      call i90_gtoken ( gcSU%so2anthro_l1_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(42)
         return
      end if
   end if

   call i90_label ( 'so2_anthro_l2_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(33)
      return
   else
      call i90_gtoken ( gcSU%so2anthro_l2_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(43)
         return
      end if
   end if

   call i90_label ( 'so2_ship_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(34)
      return
   else
      call i90_gtoken ( gcSU%so2ship_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(44)
         return
      end if
   end if

   call i90_label ( 'so4_ship_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(35)
      return
   else
      call i90_gtoken ( gcSU%so4ship_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(45)
         return
      end if
   end if

   call i90_label ( 'aircraft_fuel_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(36)
      return
   else
      call i90_gtoken ( gcSU%aircraft_fuel_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(46)
         return
      end if
   end if

   call i90_label ( 'dmso_concfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(37)
      return
   else
      call i90_gtoken ( gcSU%dmso_concfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(47)
         return
      end if
   end if

   call i90_label ( 'oh_concfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(38)
      return
   else
      call i90_gtoken ( gcSU%oh_concfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(48)
         return
      end if
   end if

   call i90_label ( 'no3_mrfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(39)
      return
   else
      call i90_gtoken ( gcSU%no3_mrfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(49)
         return
      end if
   end if

   call i90_label ( 'h2o2_mrfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(80)
      return
   else
      call i90_gtoken ( gcSU%h2o2_mrfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(90)
         return
      end if
   end if


!  Fraction of anthropogenic emissions to SO4
!  ---------------
   call i90_label ( 'so4_anthropogenic_fraction:', ier(1) )
   gcSU%fSO4ant = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if


!  Biomass Burning Emission Factor
!  ---------------
   call i90_label ( 'biomass_burning_emission_factor:', ier(1) )
   gcSU%eBiomassBurning = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(51)
      return
   end if


!  Aircraft Fuel Emission Factor
!  ---------------
   call i90_label ( 'aircraft_fuel_emission_factor:', ier(1) )
   gcSU%eAircraftFuel = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(52)
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
      call final_(53)
      return
   end if

!  Particle radius
!  ---------------
   call i90_label ( 'particle_radius:', ier(1) )
   do n = 1, nbins
      radius           = i90_gfloat ( ier(n+1) )
      gcSU%radius(n)   = radius  ! save radius in [um]
!      w_c%qa(n1+n-1)%r = radius * 1.e-6 !radius in [m]
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if

!  Particle affected by relative humidity?
!  ---------------
   call i90_label ( 'rhFlag:', ier(1) )
   irhFlag                    = i90_gint ( ier(2) )
   gcSU%rhFlag                = irhFlag
   w_c%qa(n1+n-1)%irhFlag     = irhFlag
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if

!  Particle density
!  To be used in droplet activation code
!  ---------------
   call i90_label ( 'particle_density:', ier(1) )
   do n = 1, nbins
      w_c%reg%rhop(n1+n-1)  = i90_gfloat ( ier(n+1) )
      gcSU%rhop(n)          = w_c%reg%rhop(n1+n-1)
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(54)
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
      call final_(55)
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
      call final_(56)
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
      call final_(57)
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
      call final_(58)
      return
   end if
!                          -------


! Switches to select predicted OH H2O2 NO3 from the 
! GMI Combined Stratosphere Troposphere Chemical Mechanism
! --------------------------------------------------------
   gcSU%using_GMI_OH = .FALSE.
   CALL I90_Label("using_GMI_OH:",ier(1))
   IF(ier(1) /= 0) THEN
    CALL final_(81)
    RETURN
   ELSE
    CALL I90_GToken(string,ier(1))
    IF(ier(1) /= 0) THEN
     CALL final_(82)
     RETURN
    END IF
    IF(TRIM(string) == "yes" .AND. w_c%reg%doing_GMI) gcSU%using_GMI_OH = .TRUE.
   END IF

   gcSU%using_GMI_NO3 = .FALSE.
   CALL I90_Label("using_GMI_NO3:",ier(1))
   IF(ier(1) /= 0) THEN
    CALL final_(83)
    RETURN
   ELSE
    CALL I90_GToken(string,ier(1))
    IF(ier(1) /= 0) THEN
     CALL final_(84)
     RETURN
    END IF
    IF(TRIM(string) == "yes" .AND. w_c%reg%doing_GMI) gcSU%using_GMI_NO3 = .TRUE.
   END IF

   gcSU%using_GMI_H2O2 = .FALSE.
   CALL I90_Label("using_GMI_H2O2:",ier(1))
   IF(ier(1) /= 0) THEN
    CALL final_(85)
    RETURN
   ELSE
    CALL I90_GToken(string,ier(1))
    IF(ier(1) /= 0) THEN
     CALL final_(86)
     RETURN
    END IF
    IF(TRIM(string) == "yes" .AND. w_c%reg%doing_GMI) gcSU%using_GMI_H2O2 = .TRUE.
   END IF

   gcSU%export_H2O2 = .FALSE.
   CALL I90_Label("export_H2O2:",ier(1))
   IF(ier(1) /= 0) THEN
    CALL final_(87)
    RETURN
   ELSE
    CALL I90_GToken(string,ier(1))
    IF(ier(1) /= 0) THEN
     CALL final_(88)
     RETURN
    END IF
    IF(TRIM(string) == "yes" .AND. w_c%reg%doing_GMI) gcSU%export_H2O2 = .TRUE.
   END IF

   IF(MAPL_AM_I_ROOT()) THEN
   PRINT *," "
   PRINT *,TRIM(myname)//":"
   PRINT *," Using GMI   OH: ",gcSU%using_GMI_OH
   PRINT *," Using GMI  NO3: ",gcSU%using_GMI_NO3
   PRINT *," Using GMI H2O2: ",gcSU%using_GMI_H2O2
   PRINT *," Exporting updated H2O2 to GMI: ",gcSU%export_H2O2
   PRINT *," "
   END IF

!                          -------

!  Check initial date of inventory emission/oxidant files
!  ------------------------------------------------------
!  The intent here is that these files are valid for a particular
!  YYYY or YYYYMMDD (if 1x year in file).  We need to request
!  the correct date
   if( index(gcSU%bb_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcSU%bb_srcfilen, gcSU%nymd_bb, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcSU%so2anthro_l1_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcSU%so2anthro_l1_srcfilen, gcSU%nymd_sanl1, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcSU%so2anthro_l2_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcSU%so2anthro_l2_srcfilen, gcSU%nymd_sanl2, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcSU%so2ship_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcSU%so2ship_srcfilen, gcSU%nymd_so2ship, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcSU%so4ship_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcSU%so4ship_srcfilen, gcSU%nymd_so4ship, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcSU%aircraft_fuel_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcSU%aircraft_fuel_srcfilen, gcSU%nymd_aircraft_fuel, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcSU%dmso_concfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcSU%dmso_concfilen, gcSU%nymd_dmso, &
                               begTime, nTimes, incSecs )
   endif

!  OH, NO3, and/or H2O2 may arrive from GMI
!  ----------------------------------------
   IF(.NOT. gcSU%using_GMI_OH) THEN
    if( index(gcSU%oh_concfilen,'%') .le. 0) then
     call Chem_UtilGetTimeInfo( gcSU%oh_concfilen, gcSU%nymd_oh, &
                                begTime, nTimes, incSecs )
    endif
   ELSE
    gcSU%nymd_oh = 0
   END IF

   IF(.NOT. gcSU%using_GMI_NO3) THEN
    if( index(gcSU%no3_mrfilen,'%') .le. 0) then
     call Chem_UtilGetTimeInfo( gcSU%no3_mrfilen, gcSU%nymd_no3, &
                                begTime, nTimes, incSecs )
    endif
   ELSE
    gcSU%nymd_no3 = 0
   END IF

   IF(.NOT. gcSU%using_GMI_H2O2) THEN
    if( index(gcSU%h2o2_mrfilen,'%') .le. 0) then
     call Chem_UtilGetTimeInfo( gcSU%h2o2_mrfilen, gcSU%nymd_h2o2, &
                                begTime, nTimes, incSecs )
    endif
   ELSE
    gcSU%nymd_h2o2 = 0
   END IF

   ier(1) = gcSU%nymd_bb
   ier(2) = gcSU%nymd_sanl1
   ier(3) = gcSU%nymd_sanl2
   ier(4) = gcSU%nymd_so2ship
   ier(5) = gcSU%nymd_so4ship
   ier(6) = gcSU%nymd_aircraft_fuel
   ier(7) = gcSU%nymd_dmso
   ier(8) = gcSU%nymd_oh
   ier(9) = gcSU%nymd_no3
   ier(10) = gcSU%nymd_h2o2
   if( any(ier(1:10) < 0 ) ) then
     call final_(60)
     return
   endif

!                          -------

! Initialize H2O2 unless importing it from GMICHEM
! ------------------------------------------------
   IF(.NOT. gcSU%using_GMI_H2O2) THEN

!  Do an initial read of the H2O2 fields for the convection routine
!  ----------------------------------------------------------------
!  Comes from a climatology
   nymd1 = (gcSU%nymd_h2o2/10000)*10000 + mod ( nymd, 10000 )
   if(index(gcSU%h2o2_mrfilen,'%') .gt. 0 ) nymd1 = nymd
   nhms1 = 120000
   call Chem_UtilMPread ( gcSU%h2o2_mrfilen, 'h2o2', nymd1, nhms1, &
                          i1, i2, 0, im, j1, j2, 0, jm, km, &
                          var3d=gcSU%h2o2_mr, cyclic=.true., &
                          grid = w_c%grid_esmf )

!  As a safety check, where values are undefined set to 0
!  ------------------------------------------------------
   do k = 1, km
    do j = j1, j2
     do i = i1, i2
      if(1.01*gcSU%h2o2_mr(i,j,k) > undefval) gcSU%h2o2_mr(i,j,k) = 0.
     enddo
    enddo
   enddo

   gcSU%h2o2_int = gcSU%h2o2_mr

   END IF

!  Set the gram molecular weights of the species
!  ---------------------------------------------
   gcSU%fMassSulfur = 32.
   gcSU%fMassSO2 = 64.
   gcSU%fMassSO4 = 96.
   gcSU%fMassDMS = 62.
   gcSU%fMassMSA = 96.

!  Initialize date for BCs
!  -----------------------
   gcSU%nymd = -1          ! nothing read yet
   gcSU%nymd_oxidants = -1

!  Obtain geographical region mask
!  -------------------------------
   CALL I90_label ( 'SU_regions:', ier(1) )
   CALL I90_gtoken( gcSU%maskFileName, ier(2) )
   if( any(ier(1:2) < 0 ) ) then
     call final_(41)
     return
   endif
   ier(:)=0

   call Chem_UtilGetTimeInfo( gcSU%maskFileName, nymd1, &
                              begTime, nTimes, incSecs )
   if(nymd1 < 0) call final_(15)
   nhms1 = 120000
   CALL Chem_UtilMPread ( gcSU%maskFileName, 'REGION_MASK', nymd1, nhms1, &
                          i1, i2, 0, im, j1, j2, 0, jm, 0, &
                          var2d=gcSU%regionMask, grid=w_c%grid_esmf, &
                          voting=.true. )


!  Grab the region string.
!  -----------------------
   call i90_label ( 'SU_regions_indices:', ier(1) )
   CALL I90_gtoken( gcSU%regionsString, ier(2) )
   IF( ANY(ier(1:2) < 0 ) ) THEN
    CALL final_(51)
    RETURN
   END IF

!  Is a latitude mask desired INSTEAD?  NOTE: Not a fatal error if not specified.
!  ------------------------------------------------------------------------------
   CALL I90_label ( 'doZoneMasking:', ier(1) )
   n = I90_gint ( ier(2) )

   ZoneMasking: IF(n == 1) THEN

!  BOTH a south and a north latitude limit must be specified on the .rc file.
!  --------------------------------------------------------------------------
    CALL I90_label ( 'LatitudeSouth:', ier(1) )
    limitS = i90_gfloat ( ier(2) )
    CALL I90_label ( 'LatitudeNorth:', ier(3) )
    limitN = i90_gfloat ( ier(4) )

    SpecCheck: IF( ANY(ier(1:4) < 0) .OR. limitN < limitS ) THEN
     PRINT *,myname,": Latitude bounds for zone-masking not properly specified."
     CALL final_(61)
     RETURN
    ELSE

     IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Latitude zone masking is ACTIVE."
     IF(MAPL_AM_I_ROOT()) PRINT *,myname,":  Range: ",limitS," to ",limitN
     ier(:)=0

!  Reset the regions string to 1, which will be the "on" integer in the mask.
!  --------------------------------------------------------------------------
     gcSU%regionsString = "1"
     ALLOCATE(var2d(i1:i2,j1:j2), STAT=ios)
     IF(ios /= 0) THEN
      PRINT *,myname,": Unable to allocate var2d."
      CALL final_(62)
     END IF
     var2d(i1:i2,j1:j2) = 0.00

!  Within the latitude range specified, set land boxes to 1
!  --------------------------------------------------------
      WHERE(gcSU%regionMask > 0 .AND. &
            (limitS <= w_c%grid%lat*radTODeg .AND. &
             w_c%grid%lat*radTODeg <= limitN) ) var2d = 1.00

!  Reset the region mask in gcSU
!  -----------------------------
     gcSU%regionMask(i1:i2,j1:j2) = var2d(i1:i2,j1:j2)

     DEALLOCATE(var2d, STAT=ios)
     IF(ios /= 0) THEN
      PRINT *,myname,": Unable to deallocate var2d."
      CALL final_(63)
     END IF

    END IF SpecCheck

   END IF zoneMasking

!  Is this instantiation a global case?
!  -----------------------------------
   IF(gcSU%regionsString(1:2) == "-1") THEN
    NoRegionalConstraint = .TRUE.
   ELSE
    SELECT CASE (lowercase(gcSU%regionsString(1:2)))
     CASE ("gl") 
      NoRegionalConstraint = .TRUE.
     CASE ("al") 
      NoRegionalConstraint = .TRUE.
     CASE DEFAULT
      NoRegionalConstraint = .FALSE.
    END SELECT
   END IF

!  Set regionsString to "-1" for the global case
!  ---------------------------------------------
   IF(NoRegionalConstraint) gcSU%regionsString = "-1"

   IF(MAPL_AM_I_ROOT()) THEN
    IF(NoRegionalConstraint) THEN
     PRINT *,myname,": This instantiation has no regional constraints."
    ELSE
     PRINT *,myname,": This instantiation is regionally constrained."
     PRINT *,myname,": List of region numbers included: ",TRIM(gcSU%regionsString)
    END IF
   END IF

!  All done
!  --------
   call i90_release()
   deallocate(ier)

   return


CONTAINS

   subroutine init_()
   integer ios, nerr
   nerr = max ( 32, nbins+1 )
   allocate ( gcSU%so2biomass_src(i1:i2,j1:j2), gcSU%so2anthro_l1_src(i1:i2,j1:j2), &
              gcSU%so2biomass_src_(i1:i2,j1:j2), &
              gcSU%so2anthro_l2_src(i1:i2,j1:j2), gcSU%dmso_conc(i1:i2,j1:j2), &
              gcSU%so2ship_src(i1:i2,j1:j2), gcSU%so4ship_src(i1:i2,j1:j2), &
              gcSU%oh_conc(i1:i2,j1:j2,km), gcSU%no3_mr(i1:i2,j1:j2,km), &
              gcSU%h2o2_mr(i1:i2,j1:j2,km), gcSU%h2o2_int(i1:i2,j1:j2,km), &
              gcSU%aircraft_fuel_src(i1:i2,j1:j2,km), &
              gcSU%regionMask(i1:i2,j1:j2), &
              gcSU%radius(nbins), gcSU%rhop(nbins), &
              ier(nerr), stat=ios )
   if ( ios /= 0 ) rc = 100
   end subroutine init_

   subroutine final_(ierr)
   integer :: ierr
   integer ios
   deallocate ( gcSU%so2biomass_src, gcSU%so2anthro_l1_src, gcSU%so2anthro_l2_src, &
                gcSU%so2biomass_src_, &
                gcSU%dmso_conc, gcSU%oh_conc, gcSU%no3_mr, &
                gcSU%so2ship_src, gcSU%so4ship_src, &
                gcSU%h2o2_mr, gcSU%h2o2_int, gcSU%aircraft_fuel_src, &
                gcSU%regionMask, gcSU%radius, gcSU%rhop, &
                ier, stat=ios )

   call i90_release()
   rc = ierr
   end subroutine final_

   end subroutine SU_GridCompInitialize1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine SU_GridCompRun1_ ( gcSU, w_c, impChem, expChem, &
                                 nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(SU_GridComp1), intent(inout) :: gcSU   ! Grid Component
   type(Chem_Bundle), intent(inout)  :: w_c    ! Chemical tracer fields   

! !INPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: impChem    ! Import State
   integer, intent(in) :: nymd, nhms          ! time
   real, intent(in) :: cdt                    ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: expChem   ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine implements the so-called SU Driver. That 
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

   character(len=*), parameter :: myname = 'SU_GridCompRun1_'
   character(len=*), parameter :: Iam = myname

   integer :: ier(32), idiag
   integer :: i1, i2, im, j1, j2, jm, nbins, n1, n2, km, n, ios
   integer :: i, j, k, nymd1, nhms1, ijl, ijkl
   real :: qmax, qmin
   real :: qUpdate, delq
   real, pointer :: SU_radius(:), SU_rhop(:)

!  Input fields from fvGCM
!  -----------------------
   real, pointer, dimension(:,:)   ::  frlake, frocean, frseaice, &
                                       pblh, oro, shflux, ustar, precc, &
                                       precl, u10m, v10m, hsurf, z0h
   real, pointer, dimension(:,:,:) ::  dqcond, tmpu, cloud, rhoa, u, v, hghte, dqrl

   REAL, POINTER, DIMENSION(:,:,:) ::  GMI_H2O2mr, GMI_OHmr, GMI_NO3mr
   real, pointer, dimension(:,:,:) ::  xoh, xno3, xh2o2

!  Additional needs for GOCART convective diagnostic
   real, pointer, dimension(:,:,:)       ::  cmfmc, qccu, dtrain
   real, pointer, dimension(:,:)         ::  area
   real*8, allocatable, dimension(:,:,:) ::  cmfmc_, qccu_, dtrain_, &
                                             airmass_, airmol_, vud_, &
                                             delz_, delp_, h2o2_
   real*8, allocatable                   ::  tc_(:,:,:,:), bcnv_(:,:,:)
   real*8, allocatable                   ::  area_(:,:), frlake_(:,:), &
                                             frocean_(:,:), frseaice_(:,:)
   integer*4                             ::  icdt

#define EXPORT     expChem
#define iNAME    TRIM(gcSU%iname)

#define ptrSUEM       SU_emis
#define ptrSUWT       SU_wet
#define ptrSUSV       SU_conv
#define ptrSUDP       SU_dep
#define ptrSUSD       SU_set

#define ptrSO4EMAN    SU_SO4eman
#define ptrSO2EMAN    SU_SO2eman
#define ptrSO2EMBB    SU_SO2embb
#define ptrSO2EMVN    SU_SO2emvn
#define ptrSO2EMVE    SU_SO2emve
#define ptrSUPSO2     SU_PSO2
#define ptrSUPSO4G    SU_PSO4g
#define ptrSUPSO4AQ   SU_PSO4aq
#define ptrSUPSO4WT   SU_PSO4wet
#define ptrSUPMSA     SU_PMSA

#define ptrSO2SMASS   SU_SO2sfcmass
#define ptrSO2CMASS   SU_SO2colmass
#define ptrSO4SMASS   SU_SO4sfcmass
#define ptrSO4CMASS   SU_SO4colmass
#define ptrDMSSMASS   SU_DMSsfcmass
#define ptrDMSCMASS   SU_DMScolmass
#define ptrSUCONC     SU_conc
#define ptrSUEXTCOEF  SU_extcoef
#define ptrSUSCACOEF  SU_scacoef
#define ptrSUANGSTR   SU_angstrom
#define ptrSUFLUXU    SU_fluxu
#define ptrSUFLUXV    SU_fluxv
#define ptrSO4MASS    SU_so4mass
#define ptrSUEXTTAU   SU_exttau
#define ptrSUSCATAU   SU_scatau

   integer :: STATUS

#include "SU_GetPointer___.h"

!  Initialize local variables
!  --------------------------
   ier(:)=0
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm

   km = w_c%grid%km
   nbins = w_c%reg%n_SU
   n1  = w_c%reg%i_SU
   n2  = w_c%reg%j_SU

   ijl  = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   ijkl = ijl * km

   allocate( xoh(i1:i2,j1:j2,km), xno3(i1:i2,j1:j2,km), xh2o2(i1:i2,j1:j2,km), &
             stat=STATUS)
   VERIFY_(STATUS)
   xoh = 0.
   xno3 = 0.
   xh2o2 = gcSU%h2o2_int


   call SulfateUpdateEmissions (i1, i2, im, j1, j2, jm, km, cdt, &
                                nymd, nhms, &
                                w_c%grid_esmf, w_c%grid%lon, w_c%grid%lat, &
                                gcSU%nymd, gcSU%nymd_bb, gcSU%nymd_sanl1, &
                                gcSU%nymd_sanl2, gcSU%nymd_dmso, gcSU%nymd_aircraft_fuel, &
                                w_c%diurnal_bb, &
                                gcSU%bb_srcfilen, gcSU%so2biomass_src, gcSU%so2biomass_src_, &
                                gcSU%so2anthro_l1_src, gcSU%so2anthro_l1_srcfilen, &
                                gcSU%so2anthro_l2_src, gcSU%so2anthro_l2_srcfilen, &
                                gcSU%so2ship_src, gcSU%so2ship_srcfilen, &
                                gcSU%so4ship_src, gcSU%so4ship_srcfilen, &
                                gcSU%dmso_conc, gcSU%dmso_concfilen, &
                                gcSU%aircraft_fuel_srcfilen, &
                                gcSU%aircraft_fuel_src, &
                                gcSU%volcano_srcfilen, &
                                gcSU%nvolc, gcSU%vLat, gcSU%vLon, &
                                gcSU%vElev, gcSU%vCloud, gcSU%vSO2, &
                                maskString=TRIM(gcSU%regionsString), &
                                gridMask=gcSU%regionMask)

                           
!  Get 2D Imports
!  --------------
   call MAPL_GetPointer ( impChem, pblh,     'ZPBL',    rc=ier(1) )
   call MAPL_GetPointer ( impChem, oro,      'LWI',     rc=ier(2) )
   call MAPL_GetPointer ( impChem, shflux,   'SH',      rc=ier(3) )
   call MAPL_GetPointer ( impChem, ustar,    'USTAR',   rc=ier(4) )
   call MAPL_GetPointer ( impChem, precc,    'CN_PRCP', rc=ier(5) )
   call MAPL_GetPointer ( impChem, precl,    'NCN_PRCP',rc=ier(6) )
   call MAPL_GetPointer ( impChem, u10m,     'U10M',    rc=ier(7) )
   call MAPL_GetPointer ( impChem, v10m,     'V10M',    rc=ier(8) )
   call MAPL_GetPointer ( impChem, z0h,      'Z0H',     rc=ier(9) )
   call MAPL_GetPointer ( impChem, area,     'AREA',    rc=ier(10) )
   call MAPL_GetPointer ( impChem, frocean,  'FROCEAN', rc=ier(11) )
   call MAPL_GetPointer ( impChem, frseaice, 'FRACI',   rc=ier(12) )
   call MAPL_GetPointer ( impChem, frlake,   'FRLAKE',  rc=ier(13) )

!  Get 3D Imports
!  --------------
   call MAPL_GetPointer ( impChem, dqcond, 'DQDT',    rc=ier(14) )
   call MAPL_GetPointer ( impChem, tmpu,   'T',       rc=ier(15) )
   call MAPL_GetPointer ( impChem, cloud,  'FCLD',    rc=ier(16) )
   call MAPL_GetPointer ( impChem, rhoa,   'AIRDENS', rc=ier(17) )
   call MAPL_GetPointer ( impChem, u,      'U',       rc=ier(18) )
   call MAPL_GetPointer ( impChem, v,      'V',       rc=ier(19) )
   call MAPL_GetPointer ( impChem, hghte,  'ZLE',     rc=ier(20) )
   call MAPL_GetPointer ( impChem, qccu,   'CNV_QC',  rc=ier(21) )
   call MAPL_GetPointer ( impChem, cmfmc,  'CNV_MFC', rc=ier(22) )
   call MAPL_GetPointer ( impChem, dtrain, 'CNV_MFD', rc=ier(23) )
   call MAPL_GetPointer ( impChem, dqrl,   'DQRL',    rc=ier(24) )

!  Oxidants from GMICHEM.  Get pointers first ...
!  ----------------------------------------------
   IF(  gcSU%using_GMI_OH) CALL MAPL_GetPointer(impChem,   GMI_OHmr,   "OH", rc=ier(25))
   IF( gcSU%using_GMI_NO3) CALL MAPL_GetPointer(impChem,  GMI_NO3mr,  "NO3", rc=ier(26))
   IF(gcSU%using_GMI_H2O2) CALL MAPL_GetPointer(impChem, GMI_H2O2mr, "H2O2", rc=ier(27))

   IF(ANY(ier(1:27) /= 0)) THEN
    rc = 10 
    RETURN
   END IF

!  Unlike GEOS-4 hghte is defined for km+1
!  ---------------------------------------
   hsurf => hghte(i1:i2,j1:j2,km) ! in GEOS-5 hghte is in [0,km]

#ifdef DEBUG

   call pmaxmin('SU: oro        ', oro     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SU: u10m       ', u10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SU: v10m       ', v10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SU: ustar      ', ustar   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SU: precc      ', precc   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SU: precl      ', precl   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SU: pblh       ', pblh    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SU: shfflux    ', shflux  , qmin, qmax, ijl,1, 1. )
   call pmaxmin('SU: hsurf      ', hsurf   , qmin, qmax, ijl,1, 1. )

   call pmaxmin('SU: dqcond     ', dqcond  , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SU: tmpu       ', tmpu    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SU: rhoa       ', rhoa    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SU: u          ', u       , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SU: v          ', v       , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('SU: hghte      ', hghte   , qmin, qmax, ijkl,1, 1. )

#endif

!  SU Source
!  -----------
   call SulfateDistributeEmissions ( i1, i2, j1, j2, km, nbins, cdt, &
                                     gcSU%fSO4ant, gcSU%eBiomassBurning, &
                                     gcSU%eAircraftFuel, &
                                     gcSU%so2anthro_l1_src, gcSU%so2anthro_l2_src, &
                                     gcSU%so2biomass_src, gcSU%dmso_conc, &
                                     gcSU%so2ship_src, gcSU%so4ship_src, &
                                     gcSU%aircraft_fuel_src, &
                                     gcSU%nvolc, gcSU%vLat, gcSU%vLon, &
                                     gcSU%vElev, gcSU%vCloud, gcSU%vSO2, &
                                     w_c%qa(n1+gcSU%nDMS-1)%data3d, &
                                     w_c%qa(n1+gcSU%nSO2-1)%data3d, &
                                     w_c%qa(n1+gcSU%nSO4-1)%data3d, &
                                     oro, u10m, v10m, hsurf, hghte, pblh, &
                                     tmpu, rhoa, w_c%delp, &
                                     w_c%grid%cell_area, &
                                     w_c%grid_esmf, &
                                     SU_emis, &
                                     SU_SO4eman, SU_SO2eman, SU_SO2embb, &
                                     SU_SO2emvn, SU_SO2emve, &
                                     rc )

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('SU: q_emi', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
   call pmaxmin('SU: h2o2', gcSU%h2o2_int(i1:i2,j1:j2,1:km), qmin, qmax, &
                 ijl, km, 1. )
   call pmaxmin('SU: oh', gcSU%oh_conc(i1:i2,j1:j2,1:km), qmin, qmax, &
                 ijl, km, 1. )
   call pmaxmin('SU: no3', gcSU%no3_mr(i1:i2,j1:j2,1:km), qmin, qmax, &
                 ijl, km, 1. )

#endif

   call SulfateUpdateOxidants ( i1, i2, im, j1, j2, jm, km, cdt, &
                                gcSU%using_GMI_OH, gcSU%using_GMI_NO3, &
                                gcSU%using_GMI_H2O2, &
                                GMI_OHmr, GMI_NO3mr, GMI_H2O2mr, &
                                nymd, nhms, &
                                w_c%grid_esmf, w_c%grid%lon, w_c%grid%lat, &
                                rhoa, &
                                gcSU%nymd_oxidants, gcSU%nymd_oh, &
                                gcSU%nymd_no3, gcSU%nymd_h2o2, &
                                gcSU%oh_concfilen, gcSU%no3_mrfilen, &
                                gcSU%h2o2_mrfilen, &
                                gcSU%oh_conc, gcSU%no3_mr, gcSU%h2o2_mr, &
                                xoh, xno3, xh2o2 )

#ifdef DEBUG
   CALL pmaxmin('SU: OH_conc', gcSU%oh_conc, qmin, qmax, ijl,km, 1. )
   CALL pmaxmin('SU: NO3_mr ', gcSU%no3_mr, qmin, qmax, ijl,km, 1. )
   CALL pmaxmin('SU: H2O2_mr', gcSU%h2o2_mr, qmin, qmax, ijl,km, 1. )
   CALL pmaxmin('SU: OH     ', xoh, qmin, qmax, ijl,km, 1. )
   CALL pmaxmin('SU: NO3    ', xno3, qmin, qmax, ijl,km, 1. )
   CALL pmaxmin('SU: H2O2   ', xh2o2, qmin, qmax, ijl,km, 1. )
#endif

!  Settling calculation
!  Sulfate particle radius [m] and density [kg m-3]
!  ---------------------------------------------
   allocate( SU_radius(nbins), SU_rhop(nbins) )
   SU_radius = 1.e-6*gcSU%radius
   SU_rhop   = gcSU%rhop
   call Chem_Settling ( i1, i2, j1, j2, km, n1, n2, nbins, gcSU%rhFlag, &
                          SU_radius, SU_rhop, cdt, w_c, tmpu, rhoa, hsurf,    &
                          hghte, SU_set, rc )


!  SU Chemistry Driver (dry deposition and chemistry)
!  -----------
   call SU_ChemDrv ( i1, i2, j1, j2, km, nbins, cdt, nymd, nhms, gcSU, w_c, &
                     ustar, u, v, shflux, oro, pblh, tmpu, cloud, rhoa, hghte, &
                     SU_dep, SU_PSO2, SU_PMSA, SU_PSO4g, SU_PSO4aq, & ! 2d diagnostics
                     pso2, pmsa, pso4g, pso4aq,  &                    ! 3d diagnostics
                     xoh, xno3, xh2o2, su_emis, &                              ! oxidants
                     rc)

!  Sulfate Large-scale Wet Removal
!  -------------------------------
   call SU_Wet_Removal ( i1, i2, j1, j2, km, nbins, cdt, rhoa, gcSU, w_c, &
                         precc, precl, dqcond, dqrl, tmpu, SU_wet, SU_pso4wet, &
                         pso4wet, rc )

!  Sulfate Convective-scale Mixing and Wet Removal
!  -----------------------------------------------
   icdt = cdt
   allocate(cmfmc_(i1:i2,j1:j2,km+1), qccu_(i1:i2,j1:j2,km), &
            dtrain_(i1:i2,j1:j2,km), airmass_(i1:i2,j1:j2,km), &
            delz_(i1:i2,j1:j2,km), vud_(i1:i2,j1:j2,km), &
            tc_(i1:i2,j1:j2,km,n1:n2), delp_(i1:i2,j1:j2,km), &
            airmol_(i1:i2,j1:j2,km), bcnv_(i1:i2,j1:j2,n1:n2), &
            area_(i1:i2,j1:j2), frlake_(i1:i2,j1:j2), &
            frocean_(i1:i2,j1:j2), frseaice_(i1:i2,j1:j2),&
            h2o2_(i1:i2,j1:j2,km), __STAT__ )

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
!  H2O2 is in vmr and SU are mmr.  Convert H2O2 to mmr
   do k = 1, km
     h2o2_(:,:,k)                = gcSU%h2o2_int(:,:,km-k+1)*rH2O2
   enddo
   call set_vud(i1, i2, j1, j2, km, frlake_, frocean_, frseaice_, cmfmc_, qccu_, &
                airmass_, delz_, area_, vud_)
   call convection(i1, i2, j1, j2, km, n1, n2, icdt, 'sulfur', &
                   tc_, cmfmc_, dtrain_, area_, delz_, delp_, vud_, &
                   airmass_, airmol_, &
                   bcnv_, h2o2_)

!  Return adjusted tracer to mixing ratio
   do n = n1, n2
    do k = 1, km
     w_c%qa(n)%data3d(:,:,km-k+1) = tc_(:,:,k,n)
    enddo
   enddo
!  Return adjusted h2o2
   do k = 1, km
     gcSU%h2o2_int(:,:,km-k+1) = h2o2_(:,:,k)/rH2O2
   enddo

!  Note GOCART returns bcnv_ as negative, recast for my diagnostic
   if(associated(SU_conv(1)%data2d)) SU_conv(1)%data2d = 0.0
   if(associated(SU_conv(2)%data2d)) SU_conv(2)%data2d = -bcnv_(:,:,n1+gcSU%nSO2-1)/area_/icdt
   if(associated(SU_conv(3)%data2d)) SU_conv(3)%data2d = -bcnv_(:,:,n1+gcSU%nSO4-1)/area_/icdt
   if(associated(SU_conv(4)%data2d)) SU_conv(4)%data2d = -bcnv_(:,:,n1+gcSU%nMSA-1)/area_/icdt

   deallocate(cmfmc_, qccu_, dtrain_, tc_, airmass_, &
              delz_, vud_, delp_, airmol_, bcnv_, &
              area_, frlake_, frocean_, frseaice_, h2o2_, __STAT__ )

!  Compute the desired output diagnostics here
!  Ideally this will go where chemout is called in fvgcm.F since that
!  will reflect the distributions after transport, etc.
!  -----------
   call SU_Compute_Diags(i1, i2, j1, j2, km, nbins, gcSU, w_c, tmpu, rhoa, u, v, &
                         SU_DMSsfcmass, SU_DMScolmass, &
                         SU_SO2sfcmass, SU_SO2colmass, &
                         SU_SO4sfcmass, SU_SO4colmass, &
                         SU_exttau, SU_scatau, SU_SO4mass,  &
                         SU_conc, SU_extcoef, SU_scacoef, &
                         SU_angstrom, SU_fluxu, SU_fluxv, rc)

! Update GMI Combo oxidants before exiting.
! Note: H2O2 is the only one modified as of this writing.
! -------------------------------------------------------
   IF(gcSU%using_GMI_H2O2 .AND. gcSU%export_H2O2) &
       GMI_H2O2mr(i1:i2,j1:j2,1:km) = gcSU%h2o2_int(i1:i2,j1:j2,1:km)

   deallocate(xoh, xno3, xh2o2, SU_radius, SU_rhop, stat=STATUS)
   VERIFY_(STATUS)

   RETURN

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_ChemDrv - Do SU cycle chemistry following GOCART
!
! !INTERFACE:
!

   subroutine SU_ChemDrv ( i1, i2, j1, j2, km, nbins, cdt, nymd, nhms, gcSU, &
                           w_c, ustar, u, v, shflux, oro, pblh, tmpu, &
                           cloud, rhoa, hghte, &
                           su_dep, &
                           su_pSO2, su_pMSA, su_pSO4g, su_pSO4aq, &   ! 2d diagnostics
                           pSO2, pMSA, pSO4g, pSO4aq,  &              ! 3d diagnostics
                           xoh, xno3, xh2o2, &
                           su_emis, &
                           rc)

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins, nymd, nhms
   real, intent(in)    :: cdt
   type(SU_GridComp1), intent(inout)   :: gcSU       ! SU Grid Component
   real, pointer, dimension(:,:,:)     :: tmpu, cloud, rhoa, u, v, hghte
   real, pointer, dimension(:,:)       :: ustar, shflux, oro, pblh
   real, pointer, dimension(:,:,:)     :: xoh, xno3, xh2o2

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: su_dep(nbins), su_emis(nbins)  ! Mass lost by deposition
                                                      ! to surface, kg/m2/s
!  chemical production terms d(mixing ratio) /s
   type(Chem_Array), intent(inout)  :: su_pSO2, su_pMSA, su_pSO4g, su_pSO4aq
   type(Chem_Array), intent(inout)  :: pSO2, pMSA, pSO4g, pSO4aq 

   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'SU_ChemDrv'

! !DESCRIPTION: Updates the SU concentration due to chemistry
!  The SU grid component is currently established with 4 different
!  species (bins) following this convection:
!   1) DMS
!   2) SO2
!   3) SO4
!   4) MSA
!  Accordingly we have 4 chemical cycles to follow through, which are
!  sub-subroutines under this one.
!  The chemistry is a function of OH, NO3, and H2O2 concentrations
!  as well as DMS, SO2, SO4, MSA concentrations.  It is also a function
!  of solar zenith angle and temperature.  We pass in temperature.  SZA
!  will be a function of time of day and lat/lon.  For now we simply add
!  this to the grid component before calculating it.  I bet this is
!  somewhere else in the model.
!
! !REVISION HISTORY:
!
!  06Nov2003, Colarco
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer :: ndystep, i, j, k, im
   real :: pSO2_DMS(i1:i2,j1:j2,1:km), pMSA_DMS(i1:i2,j1:j2,1:km), &
           pSO4g_SO2(i1:i2,j1:j2,1:km), pSO4aq_SO2(i1:i2,j1:j2,1:km)

!  Variables used in chemistry step
   real :: drydepf(i1:i2,j1:j2)
   real :: qmin, qmax
   integer :: ijl, ijkl, n1, STATUS
   integer :: nDMS, nSO2, nSO4, nMSA

   ijl  = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   ijkl = ijl * km
   n1 = w_c%reg%i_SU

   nDMS = gcSU%nDMS
   nSO2 = gcSU%nSO2
   nSO4 = gcSU%nSO4
   nMSA = gcSU%nMSA

!  Reset the production terms
   pSO2_DMS(i1:i2,j1:j2,1:km) = 0.
   pMSA_DMS(i1:i2,j1:j2,1:km) = 0.
   pSO4g_SO2(i1:i2,j1:j2,1:km) = 0.
   pSO4aq_SO2(i1:i2,j1:j2,1:km) = 0.
   if( associated(su_pSO2%data2d) )   su_pSO2%data2d(i1:i2,j1:j2) = 0.
   if( associated(su_pMSA%data2d) )   su_pMSA%data2d(i1:i2,j1:j2) = 0.
   if( associated(su_pSO4g%data2d) )  su_pSO4g%data2d(i1:i2,j1:j2) = 0.
   if( associated(su_pSO4aq%data2d) ) su_pSO4aq%data2d(i1:i2,j1:j2) = 0.
   if( associated(pSO2%data3d) )      pSO2%data3d(i1:i2,j1:j2,1:km) = 0.
   if( associated(pMSA%data3d) )      pMSA%data3d(i1:i2,j1:j2,1:km) = 0.
   if( associated(pSO4g%data3d) )     pSO4g%data3d(i1:i2,j1:j2,1:km) = 0.
   if( associated(pSO4aq%data3d) )    pSO4aq%data3d(i1:i2,j1:j2,1:km) = 0.


!  Now call the chemistry packages...
!  ----------------------------------
   call SulfateChemDriverGOCART ( i1, i2, j1, j2, km, n1, &
                                  nbins, cdt, nymd, nhms, &
                                  w_c%grid%lon, w_c%grid%lat, &
                                  w_c%qa(n1+nDMS-1)%data3d, &
                                  w_c%qa(n1+nSO2-1)%data3d, &
                                  w_c%qa(n1+nSO4-1)%data3d, &
                                  w_c%qa(n1+nMSA-1)%data3d, &
                                  xoh, xno3, xh2o2, &
                                  u, v, w_c%delp, tmpu, cloud, rhoa, hghte, &
                                  ustar, shflux, oro, pblh, z0h, &
                                  SU_dep, SU_PSO2, SU_PMSA, &
                                  SU_PSO4g, SU_PSO4aq, &        ! 2d diagnostics
                                  pso2, pmsa, pso4g, pso4aq,  & ! 3d diagnostics
                                  rc)




!  Save the h2o2 value after chemistry
   gcSU%h2o2_int = xh2o2

#ifdef DEBUG
   if(associated(su_pso2%data2d)) call pmaxmin('SU: su_pso2',su_pso2%data2d,qmin,qmax,ijl,1,1.)
   if(associated(su_pmsa%data2d)) call pmaxmin('SU: su_pmsa',su_pmsa%data2d,qmin,qmax,ijl,1,1.)
   if(associated(su_pso4g%data2d)) call pmaxmin('SU: su_pso4g',su_pso4g%data2d,qmin,qmax,ijl,1,1.)
   if(associated(su_pso4aq%data2d)) call pmaxmin('SU: su_pso4aq',su_pso4aq%data2d,qmin,qmax,ijl,1,1.)
   call pmaxmin('SU:  pSO4g_SO2',  pSO4g_SO2, qmin, qmax, ijl, km, 1. )
   call pmaxmin('SU: pSO4aq_SO2', pSO4aq_SO2, qmin, qmax, ijl, km, 1. )
   call pmaxmin('SU:   pSO2_DMS',   pSO2_DMS, qmin, qmax, ijl, km, 1. )
   call pmaxmin('SU:   pMSA_DMS',   pMSA_DMS, qmin, qmax, ijl, km, 1. )
#endif

   rc = 0

   end subroutine SU_ChemDrv

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_Wet_Removal - Removal of dust by precipitation
!  NOTE: For the removal term, fluxout is the sum of the in-cloud
!        convective and large-scale washout and the total flux across
!        the surface due to below-cloud (rainout) convective and
!        large-scale precipitation reaching the surface.  The fluxout
!        is initialized to zero at the beginning and then at each i, j
!        grid point it is added to.
!        See Chin et al. 1996 for some of the logic of this.  SO4 and
!        MSA are scavenged "normally."  DMS is not scavenged at all.
!        SO2 is weakly soluble in water, but some fraction can be
!        removed because of rapid aqueous phase reaction with H2O2.
!        Accordingly, we compare the mixing ratios of H2O2 and SO2 and
!        only scavenge that fraction of SO2 that is less than the
!        H2O2 mixing ratio.  If any of the scavenged SO2 is released
!        by re-evaporation is emerges as SO4
!        
!
! !INTERFACE:
!

   subroutine SU_Wet_Removal ( i1, i2, j1, j2, km, nbins, cdt, rhoa, gcSU, w_c,&
                               precc, precl, dqcond, dqrl, tmpu, fluxout, pSO4wet_colflux, &
                               pso4wet, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   real, pointer, dimension(:,:)   :: precc ! total convective precip [mm day-1]
   real, pointer, dimension(:,:)   :: precl ! total large-scale prec. [mm day-1]
   real, pointer, dimension(:,:,:) :: dqcond  ! change in q due to moist
                                              ! processes [kg kg-1 s-1] 
   real, pointer, dimension(:,:,:) :: dqrl    ! large-scale rainwater source [kg kg-1 s-1]
   real, pointer, dimension(:,:,:) :: tmpu    ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa    ! air density [kg m-3]

! !OUTPUT PARAMETERS:

   type(SU_GridComp1), intent(inout) :: gcSU  ! SU Grid Component
   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: fluxout(nbins) ! Mass lost by wet dep
                                                  ! to surface, kg/m2/s
   type(Chem_Array), intent(inout)  :: pSO4wet_colflux  ! aqueous chemical production of SO4 from SO2 (column integrated)
   type(Chem_Array), intent(inout)  :: pSO4wet    ! aqueous chemical production of SO4 from SO2
   integer, intent(out)             :: rc         ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 
   character(len=*), parameter :: myname = 'SU_Wet_Removal'

! !DESCRIPTION: Updates the dust concentration in each vertical layer
!               due to wet removal
!
! !REVISION HISTORY:
!
!  17Nov2003, Colarco
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer  ::  i, j, k, iit, n, LH, kk, ios
   integer  ::  n1, n2
   real :: pdog(i1:i2,j1:j2,km)      ! air mass factor dp/g [kg m-2]
   real*8 :: Td_ls, Td_cv              ! ls and cv timescales [s]
   real*8 :: pls, pcv, pac             ! ls, cv, tot precip [mm day-1]
   real*8 :: qls(km), qcv(km)          ! ls, cv portion dqcond [kg m-3 s-1]
   real*8 :: qmx, qd, A                ! temporary variables on moisture
   real*8 :: F, B, BT                  ! temporary variables on cloud, freq.
   real*8, allocatable :: fd(:,:)      ! flux across layers [kg m-2]
   real*8, allocatable :: DC(:)        ! scavenge change in mass mixing ratio
   real, parameter :: kb = 1.3807e-23 ! Boltzmann constant [kg m2 s-1 K-1 mol-1]
   real, parameter :: m_air = 4.8096e-26 ! Mass of <avg> air molecule [kg]

!  Rain parameters (from where?)
   real, parameter :: B0_ls = 1.0e-4
   real, parameter :: F0_ls = 1.0
   real, parameter :: XL_ls = 5.0e-4
   real, parameter :: B0_cv = 1.5e-3
   real, parameter :: F0_cv = 0.3
   real, parameter :: XL_cv = 2.0e-3
   real, parameter :: one = 1.0, zero = 0.0

!  Integer locations of SO2, etc. species
   integer :: nDMS, nSO2, nSO4, nMSA

!  Conversion of SO2 mmr to SO2 vmr (since H2O2 is carried around like
!  a volume mixing ratio)
   real*8 :: fmr, SO2Soluble
   fMR = airMolWght / gcSU%fMassSO2

   rc=0

!  Initialize local variables
!  --------------------------
   do n = 1, nbins
    if( associated(fluxout(n)%data2d) ) fluxout(n)%data2d(i1:i2,j1:j2) = 0.0
   end do
   if( associated(pso4wet_colflux%data2d)) pso4wet_colflux%data2d(i1:i2,j1:j2) = 0.
   if( associated(pso4wet%data3d) ) pso4wet%data3d(i1:i2,j1:j2,1:km) = 0.  

   n1  = w_c%reg%i_SU
   n2  = w_c%reg%j_SU
   nDMS = gcSU%nDMS
   nSO2 = gcSU%nSO2
   nSO4 = gcSU%nSO4
   nMSA = gcSU%nMSA

!  Allocate the dynamic arrays
   allocate(fd(km,nbins),stat=ios)
   if(ios .ne. 0) stop
   allocate(dc(nbins),stat=ios)
   if(ios .ne. 0) stop

!  Duration of rain: ls = model timestep, cv = 1800 s (<= cdt)
   Td_ls = cdt
   Td_cv = 1800.

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
     Dc(:)   = 0.

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
!      What is the soluble amount of SO2?
       SO2Soluble = min(fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k),gcSU%h2o2_int(i,j,k)*one)/fmr
       if(SO2Soluble .lt. 0.) SO2Soluble = 0.

!      Adjust SU amounts
       DC(nDMS) = 0.
       DC(nSO2) = SO2Soluble * F * (1.-exp(-BT))
       DC(nSO4) = w_c%qa(n1+nSO4-1)%data3d(i,j,k) * F * (1.-exp(-BT))
       DC(nMSA) = w_c%qa(n1+nMSA-1)%data3d(i,j,k) * F * (1.-exp(-BT))

!      Adjust H2O2 concentration in cloudy portion of cell
       if(fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k) .gt. gcSU%h2o2_int(i,j,k)) then
!        gcSU%h2o2_int(i,j,k) = max(zero,(1.-F)*gcSU%h2o2_int(i,j,k))
! GOCART removes all
        gcSU%h2o2_int(i,j,k) = 0.
       else
        gcSU%h2o2_int(i,j,k) &
          = gcSU%h2o2_int(i,j,k) - F*fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k)
       endif

       do n = 1, nbins
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(n1+n-1)%data3d(i,j,k) = w_c%qa(n1+n-1)%data3d(i,j,k)-DC(n)
        if (w_c%qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
       end do

!      Flux down:  unit is kg m-2
!      Formulated in terms of production in the layer.  In the revaporation step
!      we consider possibly adding flux from above...
       do n = 1, nbins
        Fd(k,n) = DC(n) * pdog(i,j,k)
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

!       Adjust SO2 for H2O2 oxidation
        SO2Soluble = min(fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k),gcSU%h2o2_int(i,j,k)*one)/fmr
        if(SO2Soluble .lt. 0.) SO2Soluble = 0.

!       Adjust SU amounts
        DC(nDMS) = 0.
        DC(nSO2) = SO2Soluble * F * (1.-exp(-BT))
        DC(nSO4) = w_c%qa(n1+nSO4-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        DC(nMSA) = w_c%qa(n1+nMSA-1)%data3d(i,j,k) * F * (1.-exp(-BT))

!       Adjust H2O2 concentration in cloudy portion of cell
        if(fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k) .gt. gcSU%h2o2_int(i,j,k)) then
         gcSU%h2o2_int(i,j,k) = max(zero,(one-F)*gcSU%h2o2_int(i,j,k))
!  GOCART removes all
!         gcSU%h2o2_int(i,j,k) = 0.
        else
         gcSU%h2o2_int(i,j,k) &
           = gcSU%h2o2_int(i,j,k) - F*fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k)
        endif
 
        do n = 1, nbins
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(n1+n-1)%data3d(i,j,k) = w_c%qa(n1+n-1)%data3d(i,j,k)-DC(n)
         if (w_c%qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
        end do
 
        do n = 1, nbins
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

!      Adjust SO2 for H2O2 oxidation
       SO2Soluble = min(fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k),gcSU%h2o2_int(i,j,k)*one)/fmr
       if(SO2Soluble .lt. 0.) SO2Soluble = 0.

!      Adjust SU amounts
       DC(nDMS) = 0.
       DC(nSO2) = SO2Soluble * F * (1.-exp(-BT))
       DC(nSO4) = w_c%qa(n1+nSO4-1)%data3d(i,j,k) * F * (1.-exp(-BT))
       DC(nMSA) = w_c%qa(n1+nMSA-1)%data3d(i,j,k) * F * (1.-exp(-BT))
       DC(nSO4) = 0.
       DC(nMSA) = 0.

!      Adjust H2O2 concentration in cloudy portion of cell
       if(fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k) .gt. gcSU%h2o2_int(i,j,k)) then
        gcSU%h2o2_int(i,j,k) = max(zero,(one-F)*gcSU%h2o2_int(i,j,k))
       else
        gcSU%h2o2_int(i,j,k) &
          = gcSU%h2o2_int(i,j,k) - F*fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k)
       endif

       do n = 1, nbins
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(n1+n-1)%data3d(i,j,k) = w_c%qa(n1+n-1)%data3d(i,j,k)-DC(n)
        if (w_c%qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
       end do

!      Flux down:  unit is kg m-2
!      Formulated in terms of production in the layer.  In the revaporation step
!      we consider possibly adding flux from above...
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

!       Adjust SO2 for H2O2 oxidation
        SO2Soluble = min(fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k),gcSU%h2o2_int(i,j,k)*one)/fmr
        if(SO2Soluble .lt. 0.) SO2Soluble = 0.

!       Adjust SU amounts
        DC(nDMS) = 0.
        DC(nSO2) = SO2Soluble * F * (1.-exp(-BT))
! Sulfate scavenged in moist
!        DC(nSO4) = w_c%qa(n1+nSO4-1)%data3d(i,j,k) * F * (1.-exp(-BT))
!        DC(nMSA) = w_c%qa(n1+nMSA-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        DC(nSO4) = 0.
        DC(nMSA) = 0.

!       Adjust H2O2 concentration in cloudy portion of cell
        if(fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k) .gt. gcSU%h2o2_int(i,j,k)) then
         gcSU%h2o2_int(i,j,k) = max(zero,(one-F)*gcSU%h2o2_int(i,j,k))
        else
         gcSU%h2o2_int(i,j,k) &
           = gcSU%h2o2_int(i,j,k) - F*fmr*w_c%qa(n1+nSO2-1)%data3d(i,j,k)
        endif
 
        do n = 1, nbins
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(n1+n-1)%data3d(i,j,k) = w_c%qa(n1+n-1)%data3d(i,j,k)-DC(n)
         if (w_c%qa(n1+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(n1+n-1)%data3d(i,j,k) = 1.0E-32
        end do

        do n = 1, nbins
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
!         For the SO2 tracer we do not allow re-evaporation.
!         We compute DC(nSO2) solely to add this to DC(nSO4) and to remove
!         from Fd(k,nSO2)
!         Instead, the SO2 gets re-evaporated to the SO4 bin because of
!         previous H2O2 oxidation

          DC(nDMS) = 0.
          DC(nSO2) = Fd(k-1,nSO2) / pdog(i,j,k) * A
          DC(nSO4) = Fd(k-1,nSO4) / pdog(i,j,k) * A
          DC(nMSA) = Fd(k-1,nMSA) / pdog(i,j,k) * A
          do n = 1, nbins
           if (DC(n).lt.0.) DC(n) = 0.
          end do

          w_c%qa(n1+nMSA-1)%data3d(i,j,k) = w_c%qa(n1+nMSA-1)%data3d(i,j,k) + DC(nMSA)

!         SO2 gets added to SO4, but remember to remove the SO2 from FD!
          w_c%qa(n1+nSO4-1)%data3d(i,j,k) =  w_c%qa(n1+nSO4-1)%data3d(i,j,k) + DC(nSO4) &
                                    + DC(nSO2)*gcSU%fMassSO4/gcSU%fMassSO2
          if( associated(pso4wet_colflux%data2d)) &
             pso4wet_colflux%data2d(i,j) = pso4wet_colflux%data2d(i,j) &
              + DC(nSO2)*gcSU%fMassSO4/gcSU%fMassSO2 / cdt * w_c%delp(i,j,k)/grav
          if( associated(pso4wet%data3d) ) &
             pso4wet%data3d(i,j,k) = DC(nSO2)*gcSU%fMassSO4/gcSU%fMassSO2 / cdt

!         Adjust the flux out of the bottom of the layer--remove SO2 here!
          do n = 1, nbins
           w_c%qa(n1+n-1)%data3d(i,j,k) = &
             max(w_c%qa(n1+n-1)%data3d(i,j,k),tiny(1.0))
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

   end subroutine SU_Wet_Removal

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_Compute_Diags - Calculate dust 2D diagnostics
!
! !INTERFACE:
!

   subroutine SU_Compute_Diags ( i1, i2, j1, j2, km, nbins, gcSU, w_c, tmpu, rhoa, u, v, &
                                 dmssfcmass, dmscolmass, so2sfcmass, &
                                 so2colmass, so4sfcmass, so4colmass, &
                                 exttau, scatau, so4mass, so4conc, extcoef, &
                                 scacoef, angstrom, fluxu, fluxv, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   type(SU_GridComp1), intent(inout):: gcSU     ! SU Grid Component
   type(Chem_Bundle), intent(in)   :: w_c
   real, pointer, dimension(:,:,:) :: tmpu      ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa      ! air density [kg m-3]
   real, pointer, dimension(:,:,:) :: u         ! east-west wind [m s-1]
   real, pointer, dimension(:,:,:) :: v         ! north-south wind [m s-1]

! !OUTPUT PARAMETERS:
   type(Chem_Array), intent(inout)  :: dmssfcmass ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: dmscolmass ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: so2sfcmass ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: so2colmass ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: so4sfcmass ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: so4colmass ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: exttau     ! ext. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: scatau     ! sct. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: so4mass    ! 3D sulfate mass mr
   type(Chem_Array), intent(inout)  :: so4conc    ! 3D mass concentration, kg/m3
   type(Chem_Array), intent(inout)  :: extcoef    ! 3D ext. coefficient, 1/m
   type(Chem_Array), intent(inout)  :: scacoef    ! 3D scat.coefficient, 1/m
   type(Chem_Array), intent(inout)  :: angstrom   ! 470-870 nm Angstrom parameter
   type(Chem_Array), intent(inout)  :: fluxu      ! Column mass flux in x direction
   type(Chem_Array), intent(inout)  :: fluxv      ! Column mass flux in y direction
   integer, intent(out)             :: rc         ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 

! !DESCRIPTION: Calculates some simple 2d diagnostics from the SU fields
!  NOTE: For now this operates solely on the sulfate bin!!!!
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
   character(len=*), parameter :: myname = 'SU_Compute_Diags'
   integer :: i, j, k, n, n1, n2, nSO4, nSO2, nDMS, ios, nch, idx
   real :: tau, ssa
   character(len=255) :: qname
   real, dimension(i1:i2,j1:j2) :: tau470, tau870
   real    :: ilam550, ilam470, ilam870
   logical :: do_angstrom


!  Initialize local variables
!  --------------------------
   n1  = w_c%reg%i_SU
   n2  = w_c%reg%j_SU
   nch   = gcSU%mie_tables%nch
   nSO4  = gcSU%nSO4
   nSO2  = gcSU%nSO2
   nDMS  = gcSU%nDMS

!  Get the wavelength indices
!  --------------------------
!  Must provide ilam550 for AOT calculation
   ilam550 = 1.
   ilam470 = 0.
   ilam870 = 0.
   if(nch .gt. 1) then
    do i = 1, nch
     if ( gcSU%mie_tables%channels(i) .ge. 5.49e-7 .and. &
          gcSU%mie_tables%channels(i) .le. 5.51e-7) ilam550 = i
     if ( gcSU%mie_tables%channels(i) .ge. 4.69e-7 .and. &
          gcSU%mie_tables%channels(i) .le. 4.71e-7) ilam470 = i
     if ( gcSU%mie_tables%channels(i) .ge. 8.69e-7 .and. &
          gcSU%mie_tables%channels(i) .le. 8.71e-7) ilam870 = i
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
   if( associated(so4sfcmass%data2d) ) then
      so4sfcmass%data2d(i1:i2,j1:j2) = 0.
      so4sfcmass%data2d(i1:i2,j1:j2) &
       =   w_c%qa(nSO4+n1-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)
   endif
   if( associated(so2sfcmass%data2d) ) then
      so2sfcmass%data2d(i1:i2,j1:j2) = 0.
      so2sfcmass%data2d(i1:i2,j1:j2) &
       =   w_c%qa(nSO2+n1-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)
   endif
   if( associated(dmssfcmass%data2d) ) then
      dmssfcmass%data2d(i1:i2,j1:j2) = 0.
      dmssfcmass%data2d(i1:i2,j1:j2) &
       =   w_c%qa(nDMS+n1-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)
   endif


!  Initialize the diagnostic variables
!  -----------------------------------

!  Calculate the column loading
   if( associated(so4colmass%data2d) ) then
      so4colmass%data2d(i1:i2,j1:j2) = 0.
      do k = 1, km
       so4colmass%data2d(i1:i2,j1:j2) &
        =   so4colmass%data2d(i1:i2,j1:j2) &
          + w_c%qa(nSO4+n1-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
      enddo
   endif
   if( associated(so2colmass%data2d) ) then
      so2colmass%data2d(i1:i2,j1:j2) = 0.
      do k = 1, km
       so2colmass%data2d(i1:i2,j1:j2) &
        =   so2colmass%data2d(i1:i2,j1:j2) &
          + w_c%qa(nSO2+n1-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
      enddo
   endif
   if( associated(dmscolmass%data2d) ) then
      dmscolmass%data2d(i1:i2,j1:j2) = 0.
      do k = 1, km
       dmscolmass%data2d(i1:i2,j1:j2) &
        =   dmscolmass%data2d(i1:i2,j1:j2) &
          + w_c%qa(nDMS+n1-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
      enddo
   endif


!  Calculate the mass concentration of sulfate 
   if( associated(so4conc%data3d) ) then
      so4conc%data3d(i1:i2,j1:j2,1:km) = 0.
      so4conc%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nSO4+n1-1)%data3d(i1:i2,j1:j2,1:km)*rhoa(i1:i2,j1:j2,1:km)
   endif

!  Mass mixing ratio of sulfate
   if( associated(so4mass%data3d) ) then
      so4mass%data3d(i1:i2,j1:j2,1:km) = 0.
      so4mass%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nSO4+n1-1)%data3d(i1:i2,j1:j2,1:km)
   endif

!  Calculate the column mass flux in x direction
   if( associated(fluxu%data2d) ) then
      fluxu%data2d(i1:i2,j1:j2) = 0.
       do k = 1, km
        fluxu%data2d(i1:i2,j1:j2) &
         =   fluxu%data2d(i1:i2,j1:j2) &
           + w_c%qa(nSO4+n1-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav*u(i1:i2,j1:j2,k)
       end do
   endif   
   
!  Calculate the column mass flux in y direction
   if( associated(fluxv%data2d) ) then
      fluxv%data2d(i1:i2,j1:j2) = 0.
       do k = 1, km
        fluxv%data2d(i1:i2,j1:j2) &
         =   fluxv%data2d(i1:i2,j1:j2) &
           + w_c%qa(nSO4+n1-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav*v(i1:i2,j1:j2,k)
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

      if( associated(extcoef%data3d) ) then
          extcoef%data3d(i1:i2,j1:j2,1:km) = 0.
      endif
      if( associated(scacoef%data3d) ) then
          scacoef%data3d(i1:i2,j1:j2,1:km) = 0.
      endif

!     Note the binning is different for SO4
      do n = nSO4, nSO4

!      Select the name for species
       qname = trim(w_c%reg%vname(w_c%reg%i_SU+n-1))
       idx = Chem_MieQueryIdx(gcSU%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

       do k = 1, km
        do j = j1, j2
         do i = i1, i2
          call Chem_MieQuery(gcSU%mie_tables, idx, ilam550, &
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

      do n = nSO4, nSO4

!      Select the name for species
       qname = trim(w_c%reg%vname(n+n1-1))
       idx = Chem_MieQueryIdx(gcSU%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

       do k = 1, km
        do j = j1, j2
         do i = i1, i2

          call Chem_MieQuery(gcSU%mie_tables, idx, ilam470, &
              w_c%qa(n+n1-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
              w_c%rh(i,j,k), tau=tau)
          tau470(i,j) = tau470(i,j) + tau

          call Chem_MieQuery(gcSU%mie_tables, idx, ilam870, &
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

   end subroutine SU_Compute_Diags

 end subroutine SU_GridCompRun1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine SU_GridCompFinalize1_ ( gcSU, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(SU_GridComp1), intent(inout) :: gcSU   ! Grid Component

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

   integer :: ios
   character(len=*), parameter :: myname = 'SU_GridCompFinalize1_'

!  If initialized volcanic emissions from daily tables, clean-up
   if(associated(gcSU%vLat))    deallocate(gcSU%vLat, stat=ios)
   if(associated(gcSU%vLon))    deallocate(gcSU%vLon, stat=ios)
   if(associated(gcSU%vSO2))    deallocate(gcSU%vSO2, stat=ios)
   if(associated(gcSU%vElev))   deallocate(gcSU%vElev, stat=ios)
   if(associated(gcSU%vCloud))  deallocate(gcSU%vCloud, stat=ios)
   rc=0
   return

 end subroutine SU_GridCompFinalize1_

 end module SU_GridCompMod

!-----------------------------------------------------------------------

!                     Single Instance Wrapper

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_SingleInstance_ --- Runs single instance of method
!
! !INTERFACE:
!
  subroutine SU_SingleInstance_ ( Method_, instance, &
                                  gcSU, w_c, impChem, expChem, &
                                  nymd, nhms, cdt, rc )

! !USES:

  Use SU_GridCompMod
  Use ESMF
  Use MAPL_Mod
  Use Chem_Mod 

  IMPLICIT NONE

! !INPUT PARAMETERS:

!  Input "function pointer"
!  -----------------------
   interface 
     subroutine Method_ (gc, w, imp, exp, ymd, hms, dt, rcode )
       Use SU_GridCompMod
       Use ESMF
       Use MAPL_Mod
       Use Chem_Mod 
       type(SU_GridComp1),  intent(inout)  :: gc
       type(Chem_Bundle),   intent(in)     :: w
       type(ESMF_State),    intent(inout)  :: imp
       type(ESMF_State),    intent(inout)  :: exp
       integer,             intent(in)     :: ymd, hms
       real,                intent(in)     :: dt
       integer,             intent(out)    :: rcode
     end subroutine Method_
   end interface

   integer, intent(in)           :: instance   ! instance number

   TYPE(Chem_Bundle), intent(inout) :: w_c     ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms           ! time
   REAL,    INTENT(IN) :: cdt                  ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(SU_GridComp1), INTENT(INOUT) :: gcSU    ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Finalizes the CO Grid Component. Multiple instance
!               version.
!
! !REVISION HISTORY:
!
!  27Feb2008  da Silva  Introduced multiple instances
!
!EOP
!-------------------------------------------------------------------------

  integer n_SU, i_SU, j_SU
  character(len=255) :: dmsname, so2name, so4name, msaname

! Save overall CO indices
! -----------------------
  n_SU = w_c%reg%n_SU
  i_SU = w_c%reg%i_SU
  j_SU = w_c%reg%j_SU

! Save the name of the variables in this instance
! -----------------------------------------------
  dmsname = trim(w_c%reg%vname(i_SU + 4*(instance - 1)))
  so2name = trim(w_c%reg%vname(i_SU + 4*(instance - 1) + 1))
  so4name = trim(w_c%reg%vname(i_SU + 4*(instance - 1) + 2))
  msaname = trim(w_c%reg%vname(i_SU + 4*(instance - 1) + 3))
  
! Customize indices for this particular instance
! ----------------------------------------------
  w_c%reg%n_SU = 4
  w_c%reg%i_SU = i_SU + 4*(instance - 1)
  w_c%reg%j_SU = i_SU + 4*(instance - 1) + 3
  w_c%reg%vname(i_SU + 4*(instance - 1))     = w_c%reg%vname(i_SU)
  w_c%reg%vname(i_SU + 4*(instance - 1) + 1) = w_c%reg%vname(i_SU + 1)
  w_c%reg%vname(i_SU + 4*(instance - 1) + 2) = w_c%reg%vname(i_SU + 2)
  w_c%reg%vname(i_SU + 4*(instance - 1) + 3) = w_c%reg%vname(i_SU + 3)
  
! Execute the instance method
! ---------------------------
  call Method_ ( gcSU, w_c, impChem, expChem, &
                 nymd, nhms, cdt, rc )

! Restore the overall SU indices
! ------------------------------
  w_c%reg%vname(i_SU + 4*(instance - 1))     = dmsname
  w_c%reg%vname(i_SU + 4*(instance - 1) + 1) = so2name
  w_c%reg%vname(i_SU + 4*(instance - 1) + 2) = so4name
  w_c%reg%vname(i_SU + 4*(instance - 1) + 3) = msaname
  w_c%reg%n_SU = n_SU
  w_c%reg%i_SU = i_SU
  w_c%reg%j_SU = j_SU

  end subroutine SU_SingleInstance_

!-----------------------------------------------------------------------
