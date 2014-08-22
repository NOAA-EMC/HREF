#ifdef GEOS5
#include "MAPL_Generic.h"
#endif

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

#ifdef GEOS5
   USE ESMF_Mod
   USE MAPL_Mod
#endif

   use Chem_Mod              ! Chemistry Base Class
   use Chem_StateMod         ! Chemistry State
   use Chem_ConstMod, only: grav, von_karman, cpd, &   ! Constants !
                            undefval => undef, airMolWght => airmw
   use Chem_UtilMod          ! I/O
   use Chem_MieMod           ! Aerosol LU Tables, calculator
   use m_inpak90             ! Resource file management
   use m_die, only: die

   use m_StrTemplate

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  SU_GridComp       ! The SU object 

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
!  17Aug2010 S. Lu     Ensure postive for no3
!  13Mar2013 Lu        Add NEMS option

!
!EOP
!-------------------------------------------------------------------------

! Note that the dates associated with the input files are a real mess
! Chem_UtilMPread cares about the date!
! Arbitrarily I set biomass_src, sanl1_src, and sanl2_src to 1971
! (of course these are not really valid for 1971)
! DMSO is valid 2000
! OH, NO3, H2O2 files are valid 2001
! Go figure...this is what happens when I get inputs from other people
! who are not the primary sources (e.g., Mian and Bian instead of
! geoschem...I get what they've got).

  type SU_GridComp
        character(len=255) :: name
        type(Chem_Mie), pointer :: mie_tables   ! aod LUTs
        real, pointer :: biomass_src_(:,:) ! before diurnal
        real, pointer :: biomass_src(:,:)
        real, pointer :: sanl1_src(:,:)  ! level 1
        real, pointer :: sanl2_src(:,:)  ! level 2
        real, pointer :: so2_ship_src(:,:)
        real, pointer :: so4_ship_src(:,:)
        real, pointer :: aircraft_fuel_src(:,:,:)
        real, pointer :: dmso_conc(:,:)
        integer       :: doing_scav     ! compute tracer scavenging (NEMS option)
!       Special handling for volcanic emissions
        logical :: volcanicDailyTables = .false.
        integer :: nvolcdaily = 0
        real, pointer, dimension(:) :: vLat    => null(), &
                                       vLon    => null(), &
                                       vSulfur => null(), &
                                       vElev   => null(), &
                                       vCloud  => null()
        real, pointer :: sulfur_volcnon(:,:)    ! volcanic emission
        real, pointer :: sulfur_volcnonhup(:,:) ! volcanic upper height
        real, pointer :: sulfur_volcnonhlow(:,:) ! volcanic lower height
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
        integer :: nymd
        character(len=255) :: bb_srcfilen
        character(len=255) :: sanl1_srcfilen
        character(len=255) :: sanl2_srcfilen
        character(len=255) :: so2_ship_srcfilen
        character(len=255) :: so4_ship_srcfilen
        character(len=255) :: aircraft_fuel_srcfilen
        character(len=255) :: dmso_concfilen
        character(len=255) :: volcnon_srcfilen
        character(len=255) :: oh_concfilen
        character(len=255) :: no3_mrfilen
        character(len=255) :: h2o2_mrfilen
        integer :: nymd_sanl1
        integer :: nymd_sanl2
        integer :: nymd_so2_ship
        integer :: nymd_so4_ship
        integer :: nymd_aircraft_fuel
        integer :: nymd_dmso
        integer :: nymd_oh
        integer :: nymd_no3
        integer :: nymd_h2o2
        integer :: nymd_volcnon = 0
  end type SU_GridComp

  real, parameter :: OCEAN=0.0, LAND = 1.0, SEA_ICE = 2.0
  real, parameter :: pi = 3.1415, rearth = 6.37e6
  real, parameter ::  radTODeg = 57.2957795

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


   character(len=255) :: rcfilen = 'SU_GridComp.rc'
   integer :: ios, n, nymd1, nhms1
   integer :: i1, i2, im, j1, j2, jm, km, nbins, nbeg, nend, nbins_rc
   integer :: i, j, k
   integer :: nTimes, begTime, incSecs
   integer, allocatable :: ier(:)
   integer :: idoing_scav  ! NEMS option to re-activate convective removal 
   real, allocatable :: buffer(:,:)
   real :: qmax, qmin


   gcSU%name = 'SU Constituent Package'

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   km = w_c%grid%km
   nbins = w_c%reg%n_SU
   nbeg  = w_c%reg%i_SU
   nend  = w_c%reg%j_SU

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

   call i90_label ( 'volcnon_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%volcnon_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if
   if(index(gcSU%volcnon_srcfilen,'%') .gt. 0) gcSU%volcanicDailyTables = .true.

   call i90_label ( 'sanl1_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%sanl1_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'sanl2_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%sanl2_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'so2_ship_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%so2_ship_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'so4_ship_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%so4_ship_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'aircraft_fuel_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%aircraft_fuel_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'dmso_concfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%dmso_concfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'oh_concfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%oh_concfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'no3_mrfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%no3_mrfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'h2o2_mrfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcSU%h2o2_mrfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
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
      call final_(50)
      return
   end if


!  Aircraft Fuel Emission Factor
!  ---------------
   call i90_label ( 'aircraft_fuel_emission_factor:', ier(1) )
   gcSU%eAircraftFuel = i90_gfloat ( ier(2) )
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
      w_c%reg%fscav(nbeg+n-1) = i90_gfloat ( ier(n+1) )
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if

!                          -------
!  NEMS Option to compute convective rainout/washout in GOCART
!  ---------------

   gcSU%doing_scav = 0     ! Default is to compute convective
!                          ! rainout/washout in GFS physics
#ifdef NEMS
   call i90_label ( 'doing_scav:', ier(1) )
   idoing_scav                 = i90_gint ( ier(2) )
   gcSU%doing_scav             = idoing_scav
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if

!  invoke the option to compute convective removal in GOCART
!  set fscav (scav used in GFS RAS) to 0.
   if ( gcSU%doing_scav  == 1 ) then
     do n = 1, nbins
      w_c%reg%fscav(nbeg+n-1)   = 0.
      w_c%qa(nbeg+n-1)%fscav    = 0.
     end do
   endif
#endif

!                          -------

!  Check initial date of inventory emission/oxidant files
!  ------------------------------------------------------
!  The intent here is that these files are valid for a particular
!  YYYY or YYYYMMDD (if 1x year in file).  We need to request
!  the correct date
   call Chem_UtilGetTimeInfo( gcSU%sanl1_srcfilen, gcSU%nymd_sanl1, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcSU%sanl2_srcfilen, gcSU%nymd_sanl2, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcSU%so2_ship_srcfilen, gcSU%nymd_so2_ship, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcSU%so4_ship_srcfilen, gcSU%nymd_so4_ship, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcSU%aircraft_fuel_srcfilen, gcSU%nymd_aircraft_fuel, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcSU%dmso_concfilen, gcSU%nymd_dmso, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcSU%oh_concfilen, gcSU%nymd_oh, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcSU%no3_mrfilen, gcSU%nymd_no3, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcSU%h2o2_mrfilen, gcSU%nymd_h2o2, &
                              begTime, nTimes, incSecs )
!  If using ascii tables, don't look for the timing information
   if(.not. gcSU%volcanicDailyTables) &
    call Chem_UtilGetTimeInfo( gcSU%volcnon_srcfilen, gcSU%nymd_volcnon, &
                               begTime, nTimes, incSecs )
   ier(1) = gcSU%nymd_sanl1
   ier(2) = gcSU%nymd_sanl2
   ier(3) = gcSU%nymd_so2_ship
   ier(4) = gcSU%nymd_so4_ship
   ier(5) = gcSU%nymd_aircraft_fuel
   ier(6) = gcSU%nymd_dmso
   ier(7) = gcSU%nymd_oh
   ier(8) = gcSU%nymd_no3
   ier(9) = gcSU%nymd_h2o2
   ier(10) = gcSU%nymd_volcnon
   if( any(ier(1:10) < 0 ) ) then
     call final_(60)
     return
   endif

!                          -------


!  Do an initial read of the H2O2 fields for the convection routine
!  ----------------------------------------------------------------
   nymd1 = (gcSU%nymd_h2o2/10000)*10000 + mod ( nymd, 10000 )
   nhms1 = 120000
   call Chem_UtilMPread ( gcSU%h2o2_mrfilen, 'h2o2', nymd1, nhms1, &
                          i1, i2, 0, im, j1, j2, 0, jm, km, &
                          var3d=gcSU%h2o2_mr, cyclic=.true., &
                          grid = w_c%grid_esmf )

!  As a safety check, where values are undefined set to 0
   do k = 1, km
    do j = j1, j2
     do i = i1, i2
      if(1.01*gcSU%h2o2_mr(i,j,k) .gt. undefval) gcSU%h2o2_mr(i,j,k) = 0.
     enddo
    enddo
   enddo

   gcSU%h2o2_int = gcSU%h2o2_mr


!  Set the gram molecular weights of the species
!  ---------------
   gcSU%fMassSulfur = 32.
   gcSU%fMassSO2 = 64.
   gcSU%fMassSO4 = 96.
   gcSU%fMassDMS = 62.
   gcSU%fMassMSA = 96.

!  Initialize date for BCs
!  -----------------------
   gcSU%nymd = -1   ! nothing read yet

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Set which fvGCM fields are needed
!  ---------------------------------
   call Chem_StateSetNeeded ( impChem, iPBLH,     .true., ier(1) )
   call Chem_StateSetNeeded ( impChem, iORO,      .true., ier(2) )
   call Chem_StateSetNeeded ( impChem, iSHFX,     .true., ier(3) )
   call Chem_StateSetNeeded ( impChem, iUSTAR,    .true., ier(4) )
   call Chem_StateSetNeeded ( impChem, iPRECC,    .true., ier(5) )
   call Chem_StateSetNeeded ( impChem, iPRECL,    .true., ier(6) )
   call Chem_StateSetNeeded ( impChem, iU10M,     .true., ier(7) )
   call Chem_StateSetNeeded ( impChem, iV10M,     .true., ier(8) )
   call Chem_StateSetNeeded ( impChem, iHSURF,    .true., ier(9) )
   call Chem_StateSetNeeded ( impChem, iDQCOND,   .true., ier(10) )
   call Chem_StateSetNeeded ( impChem, iT,        .true., ier(11) )
   call Chem_StateSetNeeded ( impChem, iCLOUD,    .true., ier(12) )
   call Chem_StateSetNeeded ( impChem, iAIRDENS,  .true., ier(13) )
   call Chem_StateSetNeeded ( impChem, iU,        .true., ier(14) )
   call Chem_StateSetNeeded ( impChem, iV,        .true., ier(15) )
   call Chem_StateSetNeeded ( impChem, iHGHTE,    .true., ier(16) )

   if ( any(ier(1:16) /= 0) ) then
        call final_(60)
        return
   endif

!  Select fields to be produced in the export state
!  ------------------------------------------------
   n = nbins

!  Emission Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iSUEM001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iSUEM002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iSUEM003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iSUEM004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iSUEM005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iSUEM006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iSUEM007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iSUEM008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif

!  Dry Deposition Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iSUDP001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iSUDP002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iSUDP003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iSUDP004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iSUDP005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iSUDP006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iSUDP007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iSUDP008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif

!  Wet Deposition Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iSUWT001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iSUWT002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iSUWT003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iSUWT004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iSUWT005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iSUWT006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iSUWT007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iSUWT008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif


!  Other diagnostics
!  The convention here is that the fields beginning iSU... are
!  two dimensional, and the others are three dimensional.
   call Chem_StateSetNeeded ( expChem, iSUSO2SMASS, .true., ier(1) )
   call Chem_StateSetNeeded ( expChem, iSUSO2CMASS, .true., ier(2) )
   call Chem_StateSetNeeded ( expChem, iSUSO4SMASS, .true., ier(3) )
   call Chem_StateSetNeeded ( expChem, iSUSO4CMASS, .true., ier(4) )
   call Chem_StateSetNeeded ( expChem, iSUDMSSMASS, .true., ier(5) )
   call Chem_StateSetNeeded ( expChem, iSUDMSCMASS, .true., ier(6) )
   call Chem_StateSetNeeded ( expChem, iSUPSO2,     .true., ier(7) )
   call Chem_StateSetNeeded ( expChem, iSUPSO4g,    .true., ier(8) )
   call Chem_StateSetNeeded ( expChem, iSUPSO4aq,   .true., ier(9) )
   call Chem_StateSetNeeded ( expChem, iSUPMSA,     .true., ier(10) )
   call Chem_StateSetNeeded ( expChem, iSUPSO4wet,  .true., ier(11) )
   call Chem_StateSetNeeded ( expChem, iSUEXTTAU,   .true., ier(12) )
   call Chem_StateSetNeeded ( expChem, iSUSCATAU,   .true., ier(13) )
   call Chem_StateSetNeeded ( expChem, iSO4MASS,    .true., ier(14) )
   call Chem_StateSetNeeded ( expChem, iPSO2,       .true., ier(15) )
   call Chem_StateSetNeeded ( expChem, iPMSA,       .true., ier(16) )
   call Chem_StateSetNeeded ( expChem, iPSO4g,      .true., ier(17) )
   call Chem_StateSetNeeded ( expChem, iPSO4aq,     .true., ier(18) )
   call Chem_StateSetNeeded ( expChem, iPSO4wet,    .true., ier(19) )
   call Chem_StateSetNeeded ( expChem, iSUEMSO4AN,  .true., ier(20) )
   call Chem_StateSetNeeded ( expChem, iSUEMSO2AN,  .true., ier(21) )
   call Chem_StateSetNeeded ( expChem, iSUEMSO2BB,  .true., ier(22) )
   call Chem_StateSetNeeded ( expChem, iSUEMSO2VN,  .true., ier(23) )
   call Chem_StateSetNeeded ( expChem, iSUEMSO2VE,  .true., ier(24) )

   if ( any(ier(1:24) /= 0) ) then
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
   allocate ( gcSU%biomass_src(i1:i2,j1:j2), gcSU%sanl1_src(i1:i2,j1:j2), &
              gcSU%biomass_src_(i1:i2,j1:j2), &
              gcSU%sanl2_src(i1:i2,j1:j2), gcSU%dmso_conc(i1:i2,j1:j2), &
              gcSU%so2_ship_src(i1:i2,j1:j2), gcSU%so4_ship_src(i1:i2,j1:j2), &
              gcSU%oh_conc(i1:i2,j1:j2,km), gcSU%no3_mr(i1:i2,j1:j2,km), &
              gcSU%h2o2_mr(i1:i2,j1:j2,km), gcSU%h2o2_int(i1:i2,j1:j2,km), &
              gcSU%sulfur_volcnon(i1:i2,j1:j2), gcSU%sulfur_volcnonhup(i1:i2,j1:j2), &
              gcSU%sulfur_volcnonhlow(i1:i2,j1:j2), &
              gcSU%aircraft_fuel_src(i1:i2,j1:j2,km), &
              ier(nerr), stat=ios )
   if ( ios /= 0 ) rc = 100
   end subroutine init_

   subroutine final_(ierr)
   integer :: ierr
   integer ios
   deallocate ( gcSU%biomass_src, gcSU%sanl1_src, gcSU%sanl2_src, &
                gcSU%biomass_src_, &
                gcSU%dmso_conc, gcSU%oh_conc, gcSU%no3_mr, &
                gcSU%sulfur_volcnon, gcSU%sulfur_volcnonhup, &
                gcSU%sulfur_volcnonhlow, gcSU%so2_ship_src, gcSU%so4_ship_src, &
                gcSU%h2o2_mr, gcSU%h2o2_int, gcSU%aircraft_fuel_src, ier, stat=ios )

   call i90_release()
   rc = ierr
   end subroutine final_

   end subroutine SU_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine SU_GridCompRun ( gcSU, w_c, impChem, expChem, &
                               nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(SU_GridComp), intent(inout) :: gcSU   ! Grid Component
   type(Chem_Bundle), intent(inout) :: w_c      ! Chemical tracer fields   

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

   character(len=*), parameter :: myname = 'SU_GridCompRun'
   character(len=*), parameter :: Iam = myname

   integer :: ier(32), idiag
   integer :: i1, i2, im, j1, j2, jm, nbins, nbeg, nend, km, n, ios
   integer :: i, j, k, nymd1, nhms1, ijl, ijkl
   integer :: jday
   real :: qmax, qmin, xhour
   real :: qUpdate, delq

!  Input fields from fvGCM
!  -----------------------
   real, pointer, dimension(:,:)   ::  pblh, oro, shflux, ustar, precc, &
                                       precl, u10m, v10m, hsurf
   real, pointer, dimension(:,:,:) ::  dqcond, tmpu, cloud, rhoa, u, v, hghte


#ifdef GEOS5 

#define EXPORT     expChem

#define ptrSUWT       SU_wet
#define ptrSUEM       SU_emis
#define ptrSUDP       SU_dep

#define ptrSO2SMASS   SU_SO2sfcmass
#define ptrSO2CMASS   SU_SO2colmass
#define ptrSO4SMASS   SU_SO4sfcmass
#define ptrSO4CMASS   SU_SO4colmass
#define ptrDMSSMASS   SU_DMSsfcmass
#define ptrDMSCMASS   SU_DMScolmass
#define ptrSUPSO2     SU_PSO2
#define ptrSUPSO4g    SU_PSO4g
#define ptrSUPSO4aq   SU_PSO4aq
#define ptrSUPSO4wt   SU_PSO4wet
#define ptrSUPMSA     SU_PMSA
#define ptrSO4EMAN    SU_SO4eman
#define ptrSO2EMAN    SU_SO2eman
#define ptrSO2EMBB    SU_SO2embb
#define ptrSO2EMVN    SU_SO2emvn
#define ptrSO2EMVE    SU_SO2emve

   integer :: STATUS

#include "SU_GetPointer___.h"

#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Quantities to be exported
!  The convention here is that the fields beginning SU_ contain 2D
!  structures, while the others are containing 3D structures
!  ---------------------------------------------------------------
   type(Chem_Array), pointer :: SU_emis(:), SU_dep(:), SU_wet(:), &
                                SU_SO2sfcmass, SU_SO2colmass, &
                                SU_SO4sfcmass, SU_SO4colmass, &
                                SU_DMSsfcmass, SU_DMScolmass, &
                                SU_PSO2, SU_PSO4g, SU_PSO4aq, &
                                SU_PSO4wet, SU_PMSA, &
                                SUexttau, SUscatau, &
                                pso2, pmsa, pso4g, pso4aq, pso4wet, &
                                SO4mass, &
                                SU_SO4eman, SU_SO2eman, &
                                SU_SO2embb, SU_SO2emvn, SU_SO2emve

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm

   km = w_c%grid%km
   nbins = w_c%reg%n_SU
   nbeg  = w_c%reg%i_SU
   nend  = w_c%reg%j_SU

   ijl  = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   ijkl = ijl * km

! Update emissions/production if necessary (daily)
!  ------------------------------------------
   if(gcSU%nymd .ne. nymd) then

!   Biomass Burning -- select on known inventories
!   ----------------------------------------------

!   Daily files (e.g., MODIS) or GFED v.2 (1997 - 2005 valid)
    if (  index(gcSU%bb_srcfilen,'%') .gt. 0 .or. &
         (index(gcSU%bb_srcfilen,'gfed') .gt. 0 .and. &
          index(gcSU%bb_srcfilen,'v2')   .gt. 0) ) then  
       nymd1 = nymd
       nhms1 = 120000

!   Assume GFED climatology or Martin (Duncan) climatology
    else                                            
       nymd1 = 1971*10000 + mod ( nymd, 10000 )  ! assumes 1971
!       nymd1 = nymd
       nhms1 = 120000
    end if

    call Chem_UtilMPread ( gcSU%bb_srcfilen, 'biomass', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcSU%biomass_src, cyclic=.true., &
                           grid = w_c%grid_esmf ) 

!   Volcanic emissions -- select on known inventories
!   -------------------------------------------------
!   There are considerations at this point:
!   1) The continuous outgassing source is probably not correct;
!      I don't think I properly handled distributions of multiple
!      volcanos within a single grid box.
!   2) The explosive inventory (as represented as "data" statements
!      likewise suffered the same problem as (1) above.
!   3) Introduce a new methodology: daily emission files represented
!      as text files containing ~1171 volcanos/day which are all
!      either continuous outgassing or explosive (so the whole data
!      set is present in a single file).  If using this data set
!      (check for "%" in the filename) then we omit the code
!      dealing with the previous gridded outgassing volcanos.

!   First, get the point source volcanos.  Can be either the old
!   explosive inventory (data statements) or the new daily files
!   containing both explosive and continous outgassing sources.
    call getvolcpoints(gcSU)

!   Handle the continuous outgassing source
    if( gcSU%volcanicDailyTables ) then
     gcSU%sulfur_volcnon(:,:) = 0.
!   Read from the previous inventory of non-explosive volcanoes
    else
     nymd1 = gcSU%nymd_volcnon
     nhms1 = 120000
     call Chem_UtilMPread ( gcSU%volcnon_srcfilen, 'sulfur', nymd1, nhms1, &
                            i1, i2, 0, im, j1, j2, 0, jm, 0, &
                            var2d=gcSU%sulfur_volcnon, grid = w_c%grid_esmf  )
     call Chem_UtilMPread ( gcSU%volcnon_srcfilen, 'hupper', nymd1, nhms1, &
                            i1, i2, 0, im, j1, j2, 0, jm, 0, &
                            var2d=gcSU%sulfur_volcnonhup, grid = w_c%grid_esmf )
     call Chem_UtilMPread ( gcSU%volcnon_srcfilen, 'hlower', nymd1, nhms1, &
                            i1, i2, 0, im, j1, j2, 0, jm, 0, &
                            var2d=gcSU%sulfur_volcnonhlow, grid = w_c%grid_esmf )
    endif

!   Anthropogenic emissions
!   -----------------------
!   Assume 1x per year on file
    nymd1 = gcSU%nymd_sanl1
    nhms1 = 120000
    call Chem_UtilMPread ( gcSU%sanl1_srcfilen, 'sanl1', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcSU%sanl1_src, grid = w_c%grid_esmf )

!   Functionality for only a single layer of emissions
    if(index(gcSU%sanl2_srcfilen,'--') .gt. 0) then
     gcSU%sanl2_src(i1:i2,j1:j2) = 0.
    else
     nymd1 = gcSU%nymd_sanl2
     nhms1 = 120000   
     call Chem_UtilMPread ( gcSU%sanl2_srcfilen, 'sanl2', nymd1, nhms1, &
                            i1, i2, 0, im, j1, j2, 0, jm, 0, &
                            var2d=gcSU%sanl2_src, grid = w_c%grid_esmf )
    endif

!   EDGAR based ship emissions of SO2 and SO4
    nymd1 = gcSU%nymd_so2_ship
    nhms1 = 120000
    call Chem_UtilMPread ( gcSU%so2_ship_srcfilen, 'so2_ship', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcSU%so2_ship_src, grid = w_c%grid_esmf )

    nymd1 = gcSU%nymd_so4_ship
    nhms1 = 120000
    call Chem_UtilMPread ( gcSU%so4_ship_srcfilen, 'so4_ship', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcSU%so4_ship_src, grid = w_c%grid_esmf )



!   DMS concentrations
    nymd1 = (gcSU%nymd_dmso/10000)*10000 + mod ( nymd, 10000 )
    nhms1 = 120000
    call Chem_UtilMPread ( gcSU%dmso_concfilen, 'conc', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcSU%dmso_conc, cyclic=.true., grid = w_c%grid_esmf )

!   As a safety check, where values are undefined set to 0
    do j = j1, j2
     do i = i1, i2
      if(1.01*gcSU%biomass_src(i,j) .gt. undefval) gcSU%biomass_src(i,j) = 0.
      if(1.01*gcSU%dmso_conc(i,j) .gt. undefval) gcSU%dmso_conc(i,j) = 0.
      if(1.01*gcSU%sanl1_src(i,j) .gt. undefval) gcSU%sanl1_src(i,j) = 0.
      if(1.01*gcSU%sanl2_src(i,j) .gt. undefval) gcSU%sanl2_src(i,j) = 0.
      if(1.01*gcSU%so2_ship_src(i,j) .gt. undefval) gcSU%so2_ship_src(i,j) = 0.
      if(1.01*gcSU%so4_ship_src(i,j) .gt. undefval) gcSU%so4_ship_src(i,j) = 0.
     enddo
    enddo


!   Aircraft fuel source
    nymd1 = (gcSU%nymd_aircraft_fuel/10000)*10000 + mod ( nymd, 10000 )
    nhms1 = 120000
    call Chem_UtilMPread ( gcSU%aircraft_fuel_srcfilen, 'fuel', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, km, &
                           var3d=gcSU%aircraft_fuel_src, cyclic=.true., grid = w_c%grid_esmf )

!   Oxidant fields
    nymd1 = (gcSU%nymd_oh/10000)*10000 + mod ( nymd, 10000 )
    nhms1 = 120000
    call Chem_UtilMPread ( gcSU%oh_concfilen, 'oh', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, km, &
                           var3d=gcSU%oh_conc, cyclic=.true., grid = w_c%grid_esmf )

    nymd1 = (gcSU%nymd_no3/10000)*10000 + mod ( nymd, 10000 )
    nhms1 = 120000
    call Chem_UtilMPread ( gcSU%no3_mrfilen, 'no3', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, km, &
                           var3d=gcSU%no3_mr, cyclic=.true., grid = w_c%grid_esmf )

    nymd1 = (gcSU%nymd_h2o2/10000)*10000 + mod ( nymd, 10000 )
    nhms1 = 120000
    call Chem_UtilMPread ( gcSU%h2o2_mrfilen, 'h2o2', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, km, &
                           var3d=gcSU%h2o2_mr, cyclic=.true., grid = w_c%grid_esmf )

!   As a safety check, where values are undefined set to 0
    do k = 1, km
     do j = j1, j2
      do i = i1, i2
       if(1.01*gcSU%h2o2_mr(i,j,k) .gt. undefval) gcSU%h2o2_mr(i,j,k) = 0.
       if(1.01*gcSU%no3_mr(i,j,k) .gt. undefval) gcSU%no3_mr(i,j,k) = 0.
       if(1.01*gcSU%oh_conc(i,j,k) .gt. undefval) gcSU%oh_conc(i,j,k) = 0.
       if(1.01*gcSU%aircraft_fuel_src(i,j,k) .gt. undefval) gcSU%aircraft_fuel_src(i,j,k) = 0.
      enddo
     enddo
    enddo

#ifdef DEBUG
    call pmaxmin('SU: bb_src',    gcSU%biomass_src, qmin, qmax, ijl,1, 1.  )
    call pmaxmin('SU: sanl1_src', gcSU%sanl1_src,   qmin, qmax, ijl,1, 1.  )
    call pmaxmin('SU: sanl2_src', gcSU%sanl2_src,   qmin, qmax, ijl,1, 1.  )
    call pmaxmin('SU: so2_ship_src', gcSU%so2_ship_src, qmin, qmax, ijl,1, 1.  )
    call pmaxmin('SU: so4_ship_src', gcSU%so4_ship_src, qmin, qmax, ijl,1, 1.  )
    call pmaxmin('SU: DMSO_conc', gcSU%dmso_conc,   qmin, qmax, ijl,1, 1.  )
    call pmaxmin('SU: fuel',      gcSU%aircraft_fuel_src, qmin, qmax, ijl,km, 1. )
    call pmaxmin('SU: OH_conc',   gcSU%oh_conc,     qmin, qmax, ijl,km, 1. )
    call pmaxmin('SU: NO3_mr',    gcSU%no3_mr,      qmin, qmax, ijl,km, 1. )
    call pmaxmin('SU: H2O2_mr',   gcSU%h2o2_mr,     qmin, qmax, ijl,km, 1. )
#endif

!  Save this in case we need to apply diurnal cycle
!  ------------------------------------------------
   if ( w_c%diurnal_bb ) then
        gcSU%biomass_src_(:,:) = gcSU%biomass_src(:,:)
   end if

    gcSU%nymd = nymd

!   The first time through the reads we will save the h2o2 monthly
!   average in the instantaneous field
!   ---------------------------------
    gcSU%h2o2_int = gcSU%h2o2_mr

   endif


!  Find the day number of the year and hour (needed for later doing sza)
!  ----------------------------------
   jday = idaynum(nymd)
   xhour = (  real(nhms/10000)*3600. &
            + real(mod(nhms,10000)/100)*60. &
            + real(mod(nhms,100)) &
           ) / 3600.

!  If the hour is divisible by 3, reset the h2o2 values to the monthly
!  average
!  ----------------------------------
   if(mod(nhms/10000,3) .eq. 0) then
    gcSU%h2o2_int = gcSU%h2o2_mr
   endif

!  Apply diurnal cycle if so desired
!  ---------------------------------
   if ( w_c%diurnal_bb ) then
      call Chem_BiomassDiurnal ( gcSU%biomass_src, gcSU%biomass_src_,   &
                                 w_c%grid%lon(:), w_c%grid%lat(:), nhms, cdt )      
   end if

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Work space for holding SU output
!  ----------------------------------
   allocate ( SU_emis(nbins), SU_dep(nbins), SU_wet(nbins), stat = ios )
   if ( ios /= 0 ) then
      rc = 1
      return
   end if

   allocate ( SU_SO2sfcmass, SU_SO2colmass, SU_SO4sfcmass, SU_SO4colmass, &
              SU_DMSsfcmass, SU_DMScolmass, SUexttau, SUscatau, &
              SU_PSO2, SU_PSO4g, SU_PSO4aq, SU_PSO4wet, SU_PMSA, &
              pso2, pmsa, pso4g, pso4aq, pso4wet, SO4mass, &
              SU_SO4eman, SU_SO2eman, SU_SO2embb, SU_SO2emvn, SU_SO2emve, &
              stat = ios )
   if ( ios /= 0 ) then
      rc = 1
      return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif


#ifdef DEBUG
   do n = nbeg, nend
      call pmaxmin('SU:q_beg', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                   ijl, km, 1. )
   end do
#endif

#ifdef GEOS5

!  Get 2D Imports
!  --------------
   call MAPL_GetPointer ( impChem, pblh,     'ZPBL',    rc=ier(1) )
   call MAPL_GetPointer ( impChem, oro,      'LWI',     rc=ier(2) )
   call MAPL_GetPointer ( impChem, shflux,   'SH',      rc=ier(3) )
   call MAPL_GetPointer ( impChem, ustar,    'USTAR',   rc=ier(4) )
   call MAPL_GetPointer ( impChem, precc,    'CN_PRCP', rc=ier(5) )
   call MAPL_GetPointer ( impChem, precl,    'NCN_PRCP',   rc=ier(6) )
   call MAPL_GetPointer ( impChem, u10m,     'U10M',    rc=ier(7) )
   call MAPL_GetPointer ( impChem, v10m,     'V10M',    rc=ier(8) )
   ier(9) = 0 ! see below for hsurf

!  Get 3D Imports
!  --------------
   call MAPL_GetPointer ( impChem, dqcond, 'DQDT',    rc=ier(10) )
   call MAPL_GetPointer ( impChem, tmpu,   'T',       rc=ier(11) )
   call MAPL_GetPointer ( impChem, cloud,  'FCLD',    rc=ier(12) )
   call MAPL_GetPointer ( impChem, rhoa,   'AIRDENS', rc=ier(13) )
   call MAPL_GetPointer ( impChem, u,      'U',       rc=ier(14) )
   call MAPL_GetPointer ( impChem, v,      'V',       rc=ier(15) )
   call MAPL_GetPointer ( impChem, hghte,  'ZLE',     rc=ier(16) )

!  Unlike GEOS-4 hghte is defined for km+1
!  ---------------------------------------
   hsurf => hghte(i1:i2,j1:j2,km) ! in GEOS-5 hghte is in [0,km]
    

#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Get input fvGCM 2D diagnostics
!  ------------------------------
   call Chem_StateGetArray2D ( impChem, iPBLH,     pblh,     ier(1) )
   call Chem_StateGetArray2D ( impChem, iORO,      oro,      ier(2) )
   call Chem_StateGetArray2D ( impChem, iSHFX,     shflux,   ier(3) )
   call Chem_StateGetArray2D ( impChem, iUSTAR,    ustar,    ier(4) )
   call Chem_StateGetArray2D ( impChem, iPRECC,    precc,    ier(5) )
   call Chem_StateGetArray2D ( impChem, iPRECL,    precl,    ier(6) )
   call Chem_StateGetArray2D ( impChem, iU10M,     u10m,     ier(7) )
   call Chem_StateGetArray2D ( impChem, iV10M,     v10m,     ier(8) )
   call Chem_StateGetArray2D ( impChem, iHSURF,    hsurf,    ier(9) )

!  Get input fvGCM 3D diagnostics
!  ------------------------------
   call Chem_StateGetArray3D ( impChem, iDQCOND,   dqcond,   ier(10) )
   call Chem_StateGetArray3D ( impChem, iT,        tmpu,     ier(11) )
   call Chem_StateGetArray3D ( impChem, iCLOUD,    cloud,    ier(12) )
   call Chem_StateGetArray3D ( impChem, iAIRDENS,  rhoa,     ier(13) )
   call Chem_StateGetArray3D ( impChem, iU,        u,        ier(14) )
   call Chem_StateGetArray3D ( impChem, iV,        v,        ier(15) )
   call Chem_StateGetArray3D ( impChem, iHGHTE,    hghte,    ier(16) )

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

    if ( any(ier(1:16) /= 0) ) then
        rc = 10 
        return
   end if

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


#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Get pointers to export state
!  ----------------------------
   do n = 1, nbins
      idiag = iSUEM001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, SU_emis(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iSUDP001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, SU_dep(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iSUWT001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, SU_wet(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   idiag = iSUSO2SMASS
   call Chem_StateGetArray2D ( expChem, idiag, SU_SO2sfcmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUSO2CMASS
   call Chem_StateGetArray2D ( expChem, idiag, SU_SO2colmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUSO4SMASS
   call Chem_StateGetArray2D ( expChem, idiag, SU_SO4sfcmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUSO4CMASS
   call Chem_StateGetArray2D ( expChem, idiag, SU_SO4colmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUDMSSMASS
   call Chem_StateGetArray2D ( expChem, idiag, SU_DMSsfcmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUDMSCMASS
   call Chem_StateGetArray2D ( expChem, idiag, SU_DMScolmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUPSO2
   call Chem_StateGetArray2D ( expChem, idiag, SU_PSO2%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUPSO4g
   call Chem_StateGetArray2D ( expChem, idiag, SU_PSO4g%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUPSO4aq
   call Chem_StateGetArray2D ( expChem, idiag, SU_PSO4aq%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUPSO4wet
   call Chem_StateGetArray2D ( expChem, idiag, SU_PSO4wet%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUPMSA
   call Chem_StateGetArray2D ( expChem, idiag, SU_PMSA%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUEXTTAU
   call Chem_StateGetArray2D ( expChem, idiag, SUexttau%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUSCATAU
   call Chem_StateGetArray2D ( expChem, idiag, SUscatau%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iPSO2
   call Chem_StateGetArray3D ( expChem, idiag, pso2%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iPMSA
   call Chem_StateGetArray3D ( expChem, idiag, pmsa%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iPSO4g
   call Chem_StateGetArray3D ( expChem, idiag, pso4g%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iPSO4aq
   call Chem_StateGetArray3D ( expChem, idiag, pso4aq%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iPSO4wet
   call Chem_StateGetArray3D ( expChem, idiag, pso4wet%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSO4MASS
   call Chem_StateGetArray3D ( expChem, idiag, SO4mass%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUEMSO4AN
   call Chem_StateGetArray2D ( expChem, idiag, SU_SO4eman%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUEMSO2AN
   call Chem_StateGetArray2D ( expChem, idiag, SU_SO2eman%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUEMSO2BB
   call Chem_StateGetArray2D ( expChem, idiag, SU_SO2embb%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUEMSO2VN
   call Chem_StateGetArray2D ( expChem, idiag, SU_SO2emvn%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iSUEMSO2VE
   call Chem_StateGetArray2D ( expChem, idiag, SU_SO2emve%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif


!  SU Source
!  -----------
   call SU_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcSU, w_c, &
                      oro, u10m, v10m, hsurf, hghte, pblh, tmpu, rhoa, SU_emis, &
                      SU_SO4eman, SU_SO2eman, SU_SO2embb, &
                      SU_SO2emvn, SU_SO2emve, &
                      rc )

#ifdef DEBUG
   do n = nbeg, nend
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

!  SU Chemistry Driver (dry deposition and chemistry)
!  -----------
   call SU_ChemDrv ( i1, i2, j1, j2, km, nbins, jday, cdt, xhour, gcSU, w_c, &
                     ustar, u, v, shflux, oro, pblh, tmpu, cloud, rhoa, &
                     SU_dep, SU_PSO2, SU_PMSA, SU_PSO4g, SU_PSO4aq, & ! 2d diagnostics
                     pso2, pmsa, pso4g, pso4aq,  &                    ! 3d diagnostics
                     rc)

!  SU Wet Removal
!  -----------
   call SU_Wet_Removal ( i1, i2, j1, j2, km, nbins, cdt, rhoa, gcSU, w_c, &
                         precc, precl, dqcond, tmpu, SU_wet, SU_pso4wet, &
                         pso4wet, rc )


!  Compute the desired output diagnostics here
!  Ideally this will go where chemout is called in fvgcm.F since that
!  will reflect the distributions after transport, etc.
!  -----------
   call SU_Compute_Diags(i1, i2, j1, j2, km, nbins, gcSU, w_c, tmpu, rhoa, &
                         SU_DMSsfcmass, SU_DMScolmass, &
                         SU_SO2sfcmass, SU_SO2colmass, &
                         SU_SO4sfcmass, SU_SO4colmass, &
                         SUexttau, SUscatau, SO4mass, rc)

!  Clean up
!  --------
#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

   deallocate ( SU_emis, SU_dep, SU_wet, SU_SO2sfcmass, SU_SO2colmass, &
                SU_SO4sfcmass, SU_SO4colmass, &
                SU_DMSsfcmass, SU_DMScolmass, SUexttau, SUscatau, &
                SU_PSO2, SU_PSO4g, SU_PSO4aq, SU_PSO4wet, SU_PMSA, &
                pso2, pmsa, pso4g, pso4aq, pso4wet, SO4mass, &
                SU_SO4eman, SU_SO2eman, SU_SO2embb, SU_SO2emvn, SU_SO2emve, &
                stat = ios )
   if ( ios /= 0 ) then
      rc = 1
      return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

   return

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  idaynum -- given nymd compute the day number of the year
!
! Colarco, July 29, 2004

   integer function idaynum (nymd)
   integer :: nymd, yyyy, mm, dd, imon, isleapyr
   integer :: ndays(12)

   data ndays /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

   yyyy = nymd / 10000
   mm = mod(nymd,10000) / 100
   dd = mod(nymd,100)

!  Is it a leap year?
   isleapyr = 0
   if(mod(yyyy,4) .eq. 0) then
    isleapyr = 1
    if(mod(yyyy,100) .eq. 0) then
     isleapyr = 0
     if(mod(yyyy,400) .eq. 0) then
      isleapyr = 1
     endif
    endif
   endif

!  What day number
   idaynum = 0
   if(mm .eq. 1) then
    idaynum = dd
   else
    do imon = 1, mm-1
     if(imon .eq. 2 .and. isleapyr .eq. 1) then
      idaynum = idaynum+29
     else
      idaynum = idaynum + ndays(imon)
     endif
    enddo
    idaynum = idaynum + dd
   endif

   return
   end function idaynum



!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  szangle -- given locations and hour find the sza
!                        from GOCART (source?)
!
! Colarco, July 29, 2004

   subroutine szangle(jday,xhour,w_c,sza,cossza,i1,i2,j1,j2)

   type(Chem_Bundle), intent(in) :: w_c         ! Chemical tracer fields
   integer :: jday, i1, i2, j1, j2, i, j
   real :: a0, a1, a2, a3, b1, b2, b3, r, dec
   real :: pi, timloc, ahr, xlon, xlat, rlat, xHour
   real :: cossza(i1:i2,j1:j2), sza(i1:i2,j1:j2)
   data pi / 3.1415926 /

   a0 = 0.006918
   a1 = 0.399912
   a2 = 0.006758
   a3 = 0.002697
   b1 = 0.070257
   b2 = 0.000907
   b3 = 0.000148
   r  = 2.*pi*float(jday-1)/365. ! where jday is day # of the year

!  dec is the solar declination in radians
   dec = a0 - a1*cos(   r) + b1*sin(   r) &
            - a2*cos(2.*r) + b2*sin(2.*r) &
            - a3*cos(3.*r) + b3*sin(3.*r)

   do i = i1, i2
!    timloc is the local time in hours
     xlon = w_c%grid%lon(i)*radToDeg    ! put longitude into degrees
     timloc = xhour + xlon/15.
     if(timloc .lt. 0.)  timloc = timloc+24.
     if(timloc .gt. 24.) timloc = timloc-24.
!    ahr is the hour angle in radians
     ahr = abs(timloc - 12.)*15.*pi/180.
     do j = j1, j2
      rlat = w_c%grid%lat(j)
      cossza(i,j) =   sin(rlat)*sin(dec) &
                    + cos(rlat)*cos(dec)*cos(ahr)
      sza(i,j)    = acos(cossza(i,j)) * radToDeg
      if(cossza(i,j) .lt. 0.) cossza(i,j) = 0.
     end do
   end do

   end subroutine szangle


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_Emission - Adds sulfate source emission for one timestep
!             We have emissions from 4 sources, which are distributed
!             differently in the vertical
!             1) biomass burning - uniformly mixed in PBL (SO2)
!             2) anthropogenic l1 - emitted into lowest 100 m (SO2,SO4)
!             3) anthropogenic l2 - emitted into 100 - 500 m levels (SO2,SO4)
!             4) volcanic emissions
!             Additionally have a source of DMS from transfer from seawater
!             into lowest model layer
!             Consider factors in conversion: we estimate that 5% of sulfur
!             from anthropogenic sources (by mass) goes directly to SO4.
!
! !INTERFACE:
!

   subroutine SU_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcSU, w_c, &
                            oro, u10m, v10m, hsurf, hghte, pblh, tmpu, rhoa, SU_emis, &
                            SU_SO4eman, SU_SO2eman, SU_SO2embb, &
                            SU_SO2emvn, SU_SO2emve, &
                            rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   type(SU_GridComp), intent(in)    :: gcSU       ! SU Grid Component
   real, pointer, dimension(:,:)    :: oro, u10m, v10m, pblh, hsurf
   real, pointer, dimension(:,:,:)  :: tmpu, rhoa, hghte

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c   ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: SU_emis(nbins)  ! SU emissions, kg/m2/s
   type(Chem_Array), intent(inout)   :: SU_SO4eman  ! SO4 anthro emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: SU_SO2eman  ! SO2 anthro emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: SU_SO2embb  ! SO2 bioburn emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: SU_SO2emvn  ! SO2 volcanic (non-explosive) emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: SU_SO2emve  ! SO2 volcanic (explosive) emissions, kg/m2/s
   integer, intent(out)             :: rc    ! Error return code:
                                             !  0 - all is well
                                             !  1 - 
   character(len=*), parameter :: myname = 'SU_Emission'

! !DESCRIPTION: Updates the SU concentration with emissions every timestep
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
   integer  ::  nbeg, nend
   real :: p1, z1, dz, delz, delp, f100, f500, fPblh
   real :: sCO2, schmidt, w10m, akw, sst
   real :: qmax, qmin, eBiomass, zpbl

   integer :: nDMS, nSO2, nSO4, nMSA

                                 ! pressure at 100m, 500m, & PBLH
   real, dimension(i1:i2,j1:j2) :: p100, p500, pPblh  
   real, dimension(i1:i2,j1:j2) :: p0, z0, ps

   real, dimension(i1:i2,j1:j2) :: SO2VolcExp
   real, dimension(i1:i2,j1:j2) :: ElvVolcExp
   real, dimension(i1:i2,j1:j2) :: cElvVolcExp

   real, dimension(i1:i2,j1:j2) :: srcSO2
   real, dimension(i1:i2,j1:j2) :: srcSO4
   real, dimension(i1:i2,j1:j2) :: srcDMS  
   real, dimension(i1:i2,j1:j2) :: srcSO4anthro
   real, dimension(i1:i2,j1:j2) :: srcSO2anthro
   real, dimension(i1:i2,j1:j2) :: srcSO2bioburn
   real, dimension(i1:i2,j1:j2) :: srcSO2volc
   real, dimension(i1:i2,j1:j2) :: srcSO2volce
   real, dimension(i1:i2,j1:j2) :: so2srcvolc 

   integer :: it
   real :: vemis, hup, hlow, dzvolc, suVolcnon
   real :: dx, dy, cellarea, deltaSO2v, so2volcano
   real :: xlon, xlat

!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_SU
   nend  = w_c%reg%j_SU
   eBiomass = gcSU%eBiomassBurning
   nDMS = gcSU%nDMS
   nSO2 = gcSU%nSO2
   nSO4 = gcSU%nSO4
   nMSA = gcSU%nMSA

   srcSO2 = 0.0
   srcSO4 = 0.0
   srcDMS = 0.0
   srcSO2volc = 0.0
   srcSO2volce = 0.0
   so2srcvolc  = 0.0

   do n = 1, nbins
    if( associated(SU_emis(n)%data2d) ) SU_emis(n)%data2d(i1:i2,j1:j2) = 0.0
   end do
   if( associated(SU_SO4eman%data2d)) SU_SO4eman%data2d(i1:i2,j1:j2) = 0.0
   if( associated(SU_SO2eman%data2d)) SU_SO2eman%data2d(i1:i2,j1:j2) = 0.0
   if( associated(SU_SO2embb%data2d)) SU_SO2embb%data2d(i1:i2,j1:j2) = 0.0
   if( associated(SU_SO2emvn%data2d)) SU_SO2emvn%data2d(i1:i2,j1:j2) = 0.0
   if( associated(SU_SO2emve%data2d)) SU_SO2emve%data2d(i1:i2,j1:j2) = 0.0

!  Find the pressure of the 100m, 500m, and PBLH altitudes
   ps = 0.0
   do k = 1, km
    ps(i1:i2,j1:j2) = ps(i1:i2,j1:j2) + w_c%delp(i1:i2,j1:j2,k)
   end do
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
      if(z0(i,j) .lt. zpbl .and. z1 .ge. zpbl) then
       delz = z1-zpbl
       delp = delz*rhoa(i,j,k)*grav
       pPblh(i,j) = p1+delp
      endif
      p0(i,j) = p1
      z0(i,j) = z1
     end do
    end do
   end do

!  Find the explosive volcanic emissions if any
   SO2Volcexp(:,:) = 0.
   ElvVolcexp(:,:) = 0.
   cElvVolcexp(:,:) = 0.
   if(.not. gcSU%volcanicDailyTables) &
    call getvolcexp(gcSU, w_c, i1, i2, j1, j2, SO2VolcExp, ElvVolcExp, cElvVolcExp)

!  Determine total volcanic source
   so2srcvolc = 0.
   do j = j1, j2
     do i = i1, i2
      if(SO2volcexp(i,j) .gt. 0) then
       so2srcvolc(i,j) = SO2VolcExp(i,j)
      else
!      convert non-volc from sulfur mass to SO2
       so2srcvolc(i,j) = gcSU%fMassSO2/gcSU%fMassSulfur * gcSU%sulfur_volcnon(i,j)
      endif
     end do
   end do

   if(associated(SU_SO2emve%data2d)) then
       where ( SO2volcexp .gt. 0) 
               SU_SO2emve%data2d = so2srcvolc
       elsewhere
               SU_SO2emve%data2d = 0.0
       endwhere
    endif

    if(associated(SU_SO2emvn%data2d)) then
       where ( SO2volcexp .gt. 0) 
               SU_SO2emvn%data2d = 0.0
       elsewhere
               SU_SO2emvn%data2d = so2srcvolc
       endwhere
    end if

!  Now update the tracer mixing ratios with the aerosol sources
   p0 = ps
   z0 = hsurf
   do k = km, 1, -1

    do j = j1, j2
     do i = i1, i2

      p1 = p0(i,j) - w_c%delp(i,j,k)
      f100 = 0.
      if(p1 .ge. p100(i,j)) f100 = w_c%delp(i,j,k)/(ps(i,j)-p100(i,j))
      if(p1 .lt. p100(i,j) .and. p0(i,j) .ge. p100(i,j)) &
       f100 = (p0(i,j)-p100(i,j))/(ps(i,j)-p100(i,j))

      f500 = 0.
      if(p0(i,j) .ge. p100(i,j) .and. p1 .lt. p100(i,j) .and. p1 .ge. p500(i,j)) &
       f500 = (p100(i,j)-p1)/(p100(i,j)-p500(i,j))
      if(p0(i,j) .lt. p100(i,j) .and. p1 .ge. p500(i,j)) &
       f500 = w_c%delp(i,j,k)/(p100(i,j)-p500(i,j))
      if(p0(i,j) .ge. p500(i,j) .and. p1 .lt. p500(i,j)) &
       f500 = (p0(i,j)-p500(i,j))/(p100(i,j)-p500(i,j))

      fPblh = 0.
      if(p1 .ge. pPblh(i,j)) fPblh = w_c%delp(i,j,k)/(ps(i,j)-pPblh(i,j))
      if(p1 .lt. pPblh(i,j) .and. p0(i,j) .ge. pPblh(i,j)) &
       fPblh = (p0(i,j)-pPblh(i,j))/(ps(i,j)-pPblh(i,j))

!     Total source in kg m-2 s-1
      srcSO4anthro(i,j) = gcSU%fSO4ant * gcSU%fMassSO4/gcSU%fMassSulfur * & 
                (   f100 *         gcSU%sanl1_src(i,j) &
                  + f500 *         gcSU%sanl2_src(i,j) )
      srcSO2anthro(i,j) = (1.-gcSU%fSO4ant) * gcSU%fMassSO2/gcSU%fMassSulfur * & 
                (   f100 *         gcSU%sanl1_src(i,j) &
                  + f500 *         gcSU%sanl2_src(i,j) )
      srcSO2bioburn(i,j) = fPblh*eBiomass*gcSU%biomass_src(i,j)

!     Add the ship emissions to anthro (already in SO2/SO4 mass units)
      srcSO2anthro(i,j) = srcSO2anthro(i,j) + f100*gcSU%so2_ship_src(i,j)
      srcSO4anthro(i,j) = srcSO4anthro(i,j) + f100*gcSU%so4_ship_src(i,j)

!     Add the aircraft fuel emissions to anthro SO2
      srcSO2anthro(i,j) = srcSO2anthro(i,j) + &
       gcSU%eAircraftFuel * gcSU%aircraft_fuel_src(i,j,k)

!     Volcanic sources
!     If explosive volcanic eruption provided, then zero out the
!     non-explosive contribution; distribute from volcano elev
!     to top cloud height
!     Else use the non-explosive data
      if(.not. gcSU%volcanicDailyTables) then

      if(SO2volcexp(i,j) .gt. 0) then
       hlow = ElvVolcExp(i,j)
       hup  = cElvVolcExp(i,j)
      else
       hlow = gcSU%sulfur_volcnonhlow(i,j)
       hup  = gcSU%sulfur_volcnonhup(i,j)
      endif

      srcSO2volc(i,j) = 0. ! contrib to tracer tendency in this layer  
      if(so2srcvolc(i,j) .gt. 0) then
        dzvolc = hup-hlow
!       Emissions per unit height
        vemis = so2srcvolc(i,j) / dzvolc
        dz = w_c%delp(i,j,k)/rhoa(i,j,k)/grav
        z1 = z0(i,j) + dz
        if(k .eq. km .and. z0(i,j) .gt. hup) then
         srcSO2volc(i,j) = vemis*dzvolc        ! all volcano beneath surface
        else if(z1 .lt. hlow) then
         srcSO2volc(i,j) = 0.                  ! volcano above this level
        else if(z0(i,j) .gt. hup) then
         srcSO2volc(i,j) = 0.                  ! volcano below this level
        else if(z0(i,j) .lt. hlow .and. z1 .gt. hlow) then
         if(z1 .le. hup) then
          srcSO2volc(i,j) = vemis*(z1-hlow)    ! bottom of volcano in this level
         else
          srcSO2volc(i,j) = vemis*dzvolc       ! all of volcano in this level
         endif
        else if(z0(i,j) .gt. hlow) then
         if(z1 .lt. hup) then
          srcSO2volc(i,j) = vemis * dz            ! whole level in plume
         else
          srcSO2volc(i,j) = vemis * (hup-z0(i,j)) ! volcano top in this level
         endif
        endif
        z0(i,j) = z1
      endif

      endif  ! end of using old volcano method

      srcSO4(i,j) = srcSO4anthro(i,j)
      srcSO2(i,j) = srcSO2anthro(i,j)+srcSO2bioburn(i,j)+srcSO2volc(i,j)

      w_c%qa(nbeg+nSO2-1)%data3d(i,j,k)   =   w_c%qa(nbeg+nSO2-1)%data3d(i,j,k)   &
                            + srcSO2(i,j)*cdt*grav/w_c%delp(i,j,k)
      w_c%qa(nbeg+nSO4-1)%data3d(i,j,k) =   w_c%qa(nbeg+nSO4-1)%data3d(i,j,k) &
                            + srcSO4(i,j)*cdt*grav/w_c%delp(i,j,k)

      p0(i,j) = p1

     end do ! i
    end do  ! j

    if( associated(SU_emis(nSO2)%data2d) ) &
                   SU_emis(nSO2)%data2d =  SU_emis(nSO2)%data2d + srcSO2
    if( associated(SU_emis(nSO4)%data2d) ) &
                   SU_emis(nSO4)%data2d =  SU_emis(nSO4)%data2d + srcSO4
    if( associated(SU_SO4eman%data2d) ) &
                   SU_SO4eman%data2d    =  SU_SO4eman%data2d    + srcSO4anthro
    if( associated(SU_SO2eman%data2d) ) &
                   SU_SO2eman%data2d    =  SU_SO2eman%data2d    + srcSO2anthro
    if( associated(SU_SO2embb%data2d) ) &
                   SU_SO2embb%data2d    =  SU_SO2embb%data2d    + srcSO2bioburn

#ifdef DEBUG
   if ( k >= km-1 ) then
      call pmaxmin('SU: srcSO2        ', srcSO2 , qmin, qmax, ijl, 1, 1. )
      call pmaxmin('SU: srcSO4        ', srcSO4 , qmin, qmax, ijl, 1, 1. )
      call pmaxmin('SU: srcSO4anthro  ', srcSO4anthro , qmin, qmax, ijl, 1, 1. )
      call pmaxmin('SU: srcSO2anthro  ', srcSO2anthro , qmin, qmax, ijl, 1, 1. )
      call pmaxmin('SU: srcSO2bioburn ', srcSO2bioburn , qmin, qmax, ijl, 1, 1. )
   end if
#endif

   end do ! k

!  Add the volcanic source (if any)
!  --------------------------------
   if(gcSU%nvolcDaily .gt. 0) then

!  Note: the grid%lat and grid%lon are wired in radians
!  but the grid%lon_del and grid%lat_del are in degrees
!  Is your name not Bruce?  That could cause a bit of
!  confusion
   dx = w_c%grid%lon_del
   dy = w_c%grid%lat_del

!  Point source volcanos (loop over each volcano)
   srcSO2volc(:,:) = 0.
   srcSO2volce(:,:) = 0.
   z0 = hghte(:,:,km)

   do it = 1, gcSU%nvolcDaily
    so2volcano = 0.
    do j = j1, j2
     xlat = w_c%grid%lat(j)*radToDeg
     if(gcSU%vLat(it) .lt. xlat-dy/2 .or. &
        gcSU%vLat(it) .ge. xlat+dy/2 ) cycle
     do i = i1, i2
      xlon = w_c%grid%lon(i)*radToDeg
      if(gcSU%vLon(it) .lt. xlon-dx/2 .or. &
         gcSU%vLon(it) .ge. xlon+dx/2 ) cycle

!     Compute grid-box surface area
!     This could lead to divide by zero; need a better way to get area
      cellarea = (2.*pi*rearth/360.)**2. * dx*dy * cos(w_c%grid%lat(j))

!     Emissions per volcano
!     This check will omit volcanos in very small grid boxes (i.e., pole; area in m2)
      if(cellarea .gt. 1.) then
       so2volcano = gcSU%vSulfur(it) * 1./cellarea &
                  * gcSU%fMassSO2/gcSU%fMassSulfur     ! to kg SO2/sec/m2
       so2volcano = max(so2volcano,tiny(so2volcano))
      endif



!     Distribute in the vertical
      hup  = gcSU%vCloud(it)
      hlow = gcSU%vElev(it)
      if (hup .ne. hlow) then
	 hlow = hup - (hup-hlow)/3.
      endif

!     Diagnostic - sum of volcanos
      if (hup .eq. hlow) then
	 srcSO2volc(i,j) = srcSO2volc(i,j) + so2volcano
      else
         srcSO2volce(i,j) = srcSO2volce(i,j) + so2volcano
      endif

      

      dzvolc = hup-hlow
      do k = km, 1, -1
        z1 = hghte(i,j,k-1)
        dz = z1-z0(i,j)
        deltaSO2v = 0.

!       Volcano is above this level
        if(z1 .lt. hlow) then
         z0(i,j) = z1
         cycle
        endif

!       Volcano is below this level
        if(z0(i,j) .gt. hup) then
         z0(i,j) = z1
         cycle
        endif

!       Volcano is in this level
        if( (k .eq. km .and. z0(i,j) .gt. hup) .or. &    ! below surface
            (z0(i,j) .le. hlow .and. z1 .ge. hup) ) then ! in level
            deltaSO2v = so2volcano
!       Volcano only partly in level                     ! Cell:
        elseif(z0(i,j) .lt. hlow .and. z1 .lt. hup) then ! has bottom of cloud
         deltaSO2v = (z1-hlow)/dzvolc*so2volcano
        elseif(z0(i,j) .gt. hlow .and. z1 .gt. hup) then ! has top of cloud
         deltaSO2v = (hup-z0(i,j))/dzvolc*so2volcano
        else                                             ! is filled with cloud
         deltaSO2v = dz/dzvolc*so2volcano
        endif

        z0(i,j) = z1
        w_c%qa(nbeg+nSO2-1)%data3d(i,j,k) = w_c%qa(nbeg+nSO2-1)%data3d(i,j,k) &
                            + deltaSO2v*cdt*grav/w_c%delp(i,j,k)
      end do ! k
     enddo   ! i
    enddo    ! j
   enddo     ! it

!  Diagnostics -- this is really the point defined volcanos
   if(associated(SU_SO2emve%data2d)) then
      SU_SO2emve%data2d = srcSO2volce
   endif
   if(associated(SU_SO2emvn%data2d)) then
      SU_SO2emvn%data2d = srcSO2volc
   endif

#ifdef DEBUG
      call pmaxmin('SU: srcSO2volcExp ', srcSO2volc , qmin, qmax, ijl, 1, 1. )
#endif

   endif  ! checking for point volcanos


!  Add in the DMS source, which is the "1" element
!  -----------------------------------------------
!  DMS emissions go into the lowest model layer only
!  The transfer of DMS from the ocean surface to the atmosphere is
!  a function of surface temperature and wind speed.
!  For now we use the lowest atmospheric temperature (really want SST)
!  and the 10-m wind speed.
!  This code follows from GOCART with the following notes:
!  :the Schmidt number for CO2 is assumed to be 600
!  :the Schmidt number of DMSo follows Saltzman et al., 1993
!  :the Schmidt number dependence breaks for high SST
!  :following www.knmi.nl/~velthove/TM/input we introduce a maximum
!   temperature of 28 C for the calculation
!  :the w10m dependence is from Liss and Merlivat (1986)
!  All this needs some thorough checking!
   k = km
   sCO2 = 600.
   do j = j1, j2
    do i = i1, i2 
     sst = tmpu(i,j,k)-273.15
     if(sst .gt. 28.) sst = 28.
!    only valid for ocean and warm enough temperatures
     if( (oro(i,j) /= OCEAN) .or. (sst .lt. -20.)) cycle
     schmidt = 2764.0 - 147.12*sst + 3.726*(sst**2.) - 0.038*(sst**3.)
!    w10m is the 10-m wind speed in m s-1
     w10m = sqrt(u10m(i,j)**2. + v10m(i,j)**2.)
     if(w10m .le. 3.6) then
      akw = 0.17*w10m*((sCO2/schmidt)**0.667)
     else if (w10m .le. 13.) then
      akw = (2.85*w10m - 9.65)*sqrt(sCO2/schmidt)
     else
      akw = (5.90*w10m - 49.3)*sqrt(sCO2/schmidt)
     endif
!    This parameterization has put akw in units cm hr-1 -> goto m s-1
     akw = akw/100./3600.
!    DMSo concentration is nMol/L
!    Want to put the source into units of kg m-2 s-1
     srcDMS(i,j) = akw * (gcSU%fmassDMS/1000.)*(gcSU%dmso_conc(i,j)*1.e-9/1.e-3)
     w_c%qa(nbeg+nDMS-1)%data3d(i,j,k) =  w_c%qa(nbeg+nDMS-1)%data3d(i,j,k) &
               + srcDMS(i,j)*cdt*grav/w_c%delp(i,j,k)
     end do
   end do

   if( associated(SU_emis(nDMS)%data2d) ) SU_emis(nDMS)%data2d =  srcDMS

#ifdef DEBUG
   call pmaxmin('SU: srcDMS        ', srcDMS , qmin, qmax, ijl, 1, 1. )
#endif

   rc = 0


   end subroutine SU_Emission

  subroutine getvolcpoints(gcSU)

! Data for volcanic emissions comes from one of two places: either
! the daily inventory of all volcanos (as represented by the text
! tables) or as the sum of the gridded continuous outgassing source
! and the inventory of prior explosive volcanos (as points).  Here
! we check to see which method is being used.  If the ascii tables
! are selected we return all the volcanic emissions (as points, per
! volcano).  If not, we return just the explosive volcanos (as
! points, per volcano).

  type(SU_GridComp) :: gcSU
  integer :: nymd1, nhms1, i, it, ios
  character(len=257) :: fname
  type(ESMF_Config)  :: cf
  integer :: nLines, nCols
  real, pointer, dimension(:) :: vData

! If previous instance of volcano point data tables exist, deallocate it
  if(associated(gcSU%vLat))    deallocate(gcSU%vLat, stat=ios)
  if(associated(gcSU%vLon))    deallocate(gcSU%vLon, stat=ios)
  if(associated(gcSU%vSulfur)) deallocate(gcSU%vSulfur, stat=ios)
  if(associated(gcSU%vElev))   deallocate(gcSU%vElev, stat=ios)
  if(associated(gcSU%vCloud))  deallocate(gcSU%vCloud, stat=ios)

! Daily files (e.g., from AEROCOM)
! --------------------------------
  if( gcSU%volcanicDailyTables ) then
     nymd1 = nymd
     nhms1 = 120000
     call StrTemplate ( fname, gcSU%volcnon_srcfilen, xid='unknown', &
                        nymd=nymd1, nhms=nhms1 )
     cf = ESMF_ConfigCreate()
     call ESMF_ConfigLoadFile(cf, fileName=trim(fname), rc=STATUS)
     VERIFY_(STATUS)
     call ESMF_ConfigGetDim(cf, nLines, nCols, 'volcano::', rc=STATUS)
     gcSU%nvolcDaily = nLines
     allocate(vData(nCols), gcSU%vLat(nLines), gcSU%vLon(nLines), &
              gcSU%vSulfur(nLines), gcSU%vElev(nLines), &
              gcSU%vCloud(nLines), stat=ios)
     call ESMF_ConfigFindLabel(cf, 'volcano::',rc=STATUS)
     do i = 1, nLines
      call ESMF_ConfigNextLine(cf, rc=rc)
      do j = 1, nCols
       call ESMF_ConfigGetAttribute(cf, vData(j), default=0.)
      end do
      gcSU%vLat(i)    = vData(1)
      gcSU%vLon(i)    = vData(2)
      gcSU%vSulfur(i) = vData(3)
      gcSU%vElev(i)   = vData(4)
      gcSU%vCloud(i)  = vData(5)
     end do

     call ESMF_ConfigDestroy(cf)
     deallocate(vData, stat=ios)

! Otherwise, let's fill the emissions with the data table values
  else
!    Do nothing for now
  endif

end subroutine getvolcpoints


  subroutine getvolcexp(gcSU, w_c, i1, i2, j1, j2, &
                        SO2volcexp, ElvVolcexp, cElvVolcexp)

! PRC
! Hack to resuse thise code to insert daily table volcanic emissions
! which are provided essentially like the volcanic explosion
! database below.  If (volcanicDailyTables = true) then we replace
! the relevant values in the data tables with those read from the
! daily ascii files.

! Data for volcanic explosions
! Provided by Thomas Diehl.  I have converted to kt SO2 event-1 to
! kt SO2 day-1 over the eruption.  Below this gets converted to a
! kg SO2 m-2 s-1 needed in emissions.

  integer :: i1, i2, j1, j2
  type(SU_GridComp), intent(in) :: gcSU
  type(Chem_Bundle), intent(in) :: w_c
  real, dimension(i1:i2,j1:j2), intent(inout)  :: SO2volcexp, ElvVolcExp, cElvVolcExp
  real, parameter :: pi = 3.1415, rearth = 6.37e6
  real :: dx, dy, cellarea
  integer :: it, i, j

  integer :: nvolcDaily

! ======= Replace code between like symbols with data statements ========
  integer, parameter :: nvolc = 349
  integer :: startday(nvolc), endday(nvolc)
  real    :: so2exp(nvolc), lon(nvolc), lat(nvolc), & 
             velev(nvolc), celev(nvolc)

  data startday / &
    20051231, 20051231, 20051231, 20061231, 20001015, 20061231,   &
    20000301, 20021019, 20010717, 20061231, 20010905, 20001220,   &
    20050619, 20061231, 20020615, 19990815, 20000425, 20010525,   &
    19990815, 19991109, 19990719, 20000315, 20001214, 19990915,   &
    19990224, 19990815, 19990712, 19990225, 19990418, 19990915,   &
    20031028, 20000615, 19990329, 19990402, 19990701, 19990417,   &
    19990412, 19990915, 19990421, 19990527, 19990915, 20000615,   &
    19990514, 20001209, 20000415, 19990915, 19990630, 20000319,   &
    19990628, 19991023, 19990720, 20010729, 19990807, 20061231,   &
    19991115, 19991020, 19991116, 20010805, 20000302, 19991229,   &
    20000315, 20000127, 20000210, 20000208, 20000214, 20000304,   &
    20000224, 20000226, 20000229, 20000306, 20000905, 20000403,   &
    20000326, 20000518, 20010915, 20001029, 20010818, 20030831,   &
    20010215, 20000610, 20001030, 20000604, 20001113, 20000818,   &
    20001018, 20001015, 20000831, 20001104, 20000721, 20000922,   &
    20010705, 20010115, 20000820, 20000823, 20000827, 20000904,   &
    20000910, 20001108, 20000926, 20061231, 20000930, 20001101,   &
    20010721, 20010416, 20001129, 20001130, 20010115, 20001215,   &
    20001218, 20001222, 20040705, 20010428, 20010808, 20021124,   &
    20010429, 20010218, 20010302, 20010219, 20010219, 20010415,   &
    20010405, 20010404, 20010605, 20010425, 20010429, 20031122,   &
    20010501, 20010503, 20011209, 20020827, 20011003, 20010619,   &
    20010707, 20010809, 20010915, 20010809, 20010730, 20030712,   &
    20010806, 20061231, 20010828, 20011115, 20011019, 20011115,   &
    20011005, 20061231, 20011031, 20061231, 20011126, 20020106,   &
    20021026, 20061231, 20020116, 20020521, 20020116, 20020117,   &
    20020203, 20020422, 20020515, 20020315, 20020609, 20040415,   &
    20020715, 20021006, 20030409, 20021216, 20020617, 20020607,   &
    20020825, 20021115, 20020725, 20020802, 20020815, 20020802,   &
    20020915, 20020820, 20021103, 20021015, 20020925, 20020926,   &
    20020929, 20040217, 20021106, 20021011, 20021207, 20021012,   &
    20021026, 20021027, 20021030, 20021103, 20021103, 20021120,   &
    20030128, 20021202, 20021112, 20021114, 20021219, 20021116,   &
    20021118, 20021203, 20030110, 20021128, 20021228, 20030401,   &
    20030101, 20040408, 20030228, 20030418, 20031015, 20030528,   &
    20030723, 20031109, 20030514, 20030416, 20031010, 20030417,   &
    20030703, 20030506, 20030510, 20030513, 20030523, 20030523,   &
    20040325, 20030602, 20061231, 20040110, 20030901, 20030712,   &
    20030608, 20030609, 20040614, 20030616, 20030723, 20030714,   &
    20030715, 20031008, 20030801, 20031002, 20030915, 20030901,   &
    20030904, 20030912, 20031115, 20031011, 20040328, 20031209,   &
    20040114, 20050215, 20040127, 20040205, 20040214, 20040224,   &
    20040915, 20040502, 20040925, 20050805, 20040414, 20050405,   &
    20041008, 20041003, 20040528, 20050222, 20040517, 20040526,   &
    20040607, 20040815, 20040912, 20040608, 20040609, 20040624,   &
    20041024, 20040624, 20040916, 20040704, 20050207, 20050911,   &
    20040730, 20040805, 20061231, 20040914, 20050315, 20040915,   &
    20040915, 20041209, 20041005, 20061231, 20041212, 20061231,   &
    20041110, 20041104, 20061231, 20041111, 20041115, 20041123,   &
    20041125, 20041225, 20041126, 20041127, 20041219, 20041209,   &
    20041213, 20041227, 20041220, 20050127, 20050214, 20061231,   &
    20050407, 20061231, 20050525, 20050128, 20061231, 20050216,   &
    20050331, 20050227, 20050225, 20050223, 20050407, 20050701,   &
    20050406, 20050903, 20050718, 20050518, 20050414, 20061231,   &
    20050418, 20061231, 20050718, 20050504, 20050529, 20061231,   &
    20050616, 20051007, 20050815, 20051112, 20051104, 20050929,   &
    20051001, 20051005, 20061231, 20051117, 20051030, 20061231,   &
    20051208, 20061231, 20061231, 20051201, 20061231, 20051222,   &
    20061231  &
    /
  data endday / &
    20051231, 20051231, 20051231, 20061231, 20001015, 20061231,   &
    20000301, 20021019, 20010717, 20061231, 20010905, 20001220,   &
    20050619, 20061231, 20020615, 19990815, 20000425, 20010525,   &
    19990815, 19991109, 19990719, 20000315, 20001214, 19990915,   &
    19990224, 19990815, 19990712, 19990225, 19990418, 19990915,   &
    20031028, 20000615, 19990329, 19990402, 19990701, 19990417,   &
    19990412, 19990915, 19990421, 19990527, 19990915, 20000615,   &
    19990514, 20001209, 20000415, 19990915, 19990630, 20000319,   &
    19990628, 19991023, 19990720, 20010729, 19990807, 20061231,   &
    19991115, 19991020, 19991116, 20010805, 20000302, 19991229,   &
    20000315, 20000127, 20000210, 20000208, 20000214, 20000304,   &
    20000224, 20000226, 20000229, 20000306, 20000905, 20000403,   &
    20000326, 20000518, 20010915, 20001029, 20010818, 20030831,   &
    20010215, 20000610, 20001030, 20000604, 20001113, 20000818,   &
    20001018, 20001015, 20000831, 20001104, 20000721, 20000922,   &
    20010705, 20010115, 20000820, 20000823, 20000827, 20000904,   &
    20000910, 20001108, 20000926, 20061231, 20000930, 20001101,   &
    20010721, 20010416, 20001129, 20001130, 20010115, 20001215,   &
    20001218, 20001222, 20040705, 20010428, 20010808, 20021124,   &
    20010429, 20010218, 20010302, 20010219, 20010219, 20010415,   &
    20010405, 20010404, 20010605, 20010425, 20010429, 20031122,   &
    20010501, 20010503, 20011209, 20020827, 20011003, 20010619,   &
    20010707, 20010809, 20010915, 20010809, 20010730, 20030712,   &
    20010806, 20061231, 20010828, 20011115, 20011019, 20011115,   &
    20011005, 20061231, 20011031, 20061231, 20011126, 20020106,   &
    20021026, 20061231, 20020116, 20020521, 20020116, 20020117,   &
    20020203, 20020422, 20020515, 20020315, 20020609, 20040415,   &
    20020715, 20021006, 20030409, 20021216, 20020617, 20020607,   &
    20020825, 20021115, 20020725, 20020802, 20020815, 20020802,   &
    20020915, 20020820, 20021103, 20021015, 20020925, 20020926,   &
    20020929, 20040217, 20021106, 20021011, 20021207, 20021012,   &
    20021026, 20021027, 20021030, 20021103, 20021103, 20021120,   &
    20030128, 20021202, 20021112, 20021114, 20021219, 20021116,   &
    20021118, 20021203, 20030110, 20021128, 20021228, 20030401,   &
    20030101, 20040408, 20030228, 20030418, 20031015, 20030528,   &
    20030723, 20031109, 20030514, 20030416, 20031010, 20030417,   &
    20030703, 20030506, 20030510, 20030513, 20030523, 20030523,   &
    20040325, 20030602, 20061231, 20040110, 20030901, 20030712,   &
    20030608, 20030609, 20040614, 20030616, 20030723, 20030714,   &
    20030715, 20031008, 20030801, 20031002, 20030915, 20030901,   &
    20030904, 20030912, 20031115, 20031011, 20040328, 20031209,   &
    20040114, 20050215, 20040127, 20040205, 20040214, 20040224,   &
    20040915, 20040502, 20040925, 20050805, 20040414, 20050405,   &
    20041008, 20041003, 20040528, 20050222, 20040517, 20040526,   &
    20040607, 20040815, 20040912, 20040608, 20040609, 20040624,   &
    20041024, 20040624, 20040916, 20040704, 20050207, 20050911,   &
    20040730, 20040805, 20061231, 20040914, 20050315, 20040915,   &
    20040915, 20041209, 20041005, 20061231, 20041212, 20061231,   &
    20041110, 20041104, 20061231, 20041111, 20041115, 20041123,   &
    20041125, 20041225, 20041126, 20041127, 20041219, 20041209,   &
    20041213, 20041227, 20041220, 20050127, 20050214, 20061231,   &
    20050407, 20061231, 20050525, 20050128, 20061231, 20050216,   &
    20050331, 20050227, 20050225, 20050223, 20050407, 20050701,   &
    20050406, 20050903, 20050718, 20050518, 20050414, 20061231,   &
    20050418, 20061231, 20050718, 20050504, 20050529, 20061231,   &
    20050616, 20051007, 20050815, 20051112, 20051104, 20050929,   &
    20051001, 20051005, 20061231, 20051117, 20051030, 20061231,   &
    20051208, 20061231, 20061231, 20051201, 20061231, 20051222,   &
    20061231  &
    /
  data so2exp / &
       0.004,    0.004,    0.008,    0.001,    0.011,    0.022,   &
       0.031,    0.004,    0.044,    0.042,    0.008,    0.063,   &
       0.001,    0.035,    0.001,    0.036,    0.177,    0.112,   &
       0.047,    0.012,    0.038,    0.001,    0.041,    0.062,   &
       0.044,    0.089,    0.108,   17.000,    1.513,    0.092,   &
       0.068,    0.005,   30.952,   30.952,    0.183,   30.952,   &
       1.700,    0.110,   21.000,    1.513,    0.016,    0.041,   &
     190.000,    0.030,    0.051,    0.002,    0.141,    0.423,   &
       2.250,    1.959,   16.000,    0.038,    5.667,    0.223,   &
       0.006,    2.250,    3.000,    0.006,    0.022,    0.750,   &
       0.279,   43.333,   43.333,    2.833,    9.167,    9.500,   &
      17.000,  250.000,  250.000,  250.000,    0.628,    0.708,   &
       1.308,    0.038,    0.032,    0.086,    0.036,    0.095,   &
       2.434,   46.429,    0.015,    8.500,    1.319,    1.250,   &
       0.155,    0.024,    0.362,    0.155,    8.500,    0.298,   &
       0.007,    0.002,   11.500,    1.250,    0.375,   20.583,   &
       0.225,    0.034,    1.250,    0.001,   17.143,   17.143,   &
       0.008,    0.014,    0.354,    9.000,    0.354,   23.000,   &
       0.041,   10.000,    0.089,    0.041,    0.540,    0.025,   &
       1.065,    8.219,   28.000,    8.219,   17.000,    8.219,   &
      11.017,   21.111,    0.315,    0.750,    4.000,    0.041,   &
      15.000,    1.065,    0.011,    0.036,    0.002,    9.583,   &
       7.037,    0.708,    0.039,    6.389,   33.000,    0.038,   &
       3.000,    0.006,   17.000,    0.004,    0.078,    0.043,   &
       2.250,    0.001,    2.250,    0.061,    2.250,    0.607,   &
       0.007,    0.009,   15.833,    0.891,    2.250,   30.000,   &
      10.556,    0.193,    0.177,    2.250,    0.274,    0.003,   &
       0.258,    0.385,    0.053,    0.011,    0.118,    2.250,   &
       0.236,    0.002,   12.264,   90.000,    0.125,   17.000,   &
      12.264,    1.889,    0.230,    0.102,  120.000,  120.000,   &
     120.000,    0.034,    8.387,    2.250,    0.039,    2.250,   &
       1.211,    8.500,   10.000,   10.000,   10.000,   15.278,   &
       1.211,    0.385,    0.436,    5.000,    0.436,   36.111,   &
      36.111,   36.111,    8.696,    2.250,    5.000,    0.170,   &
       1.700,    0.036,    0.385,    0.031,    0.070,   11.236,   &
       0.016,    0.009,    0.288,    2.125,    0.094,    2.250,   &
       0.221,    0.250,    1.797,   38.000,    1.797,    0.321,   &
       0.007,   36.000,    0.385,    0.841,    0.179,    1.797,   &
      12.778,   12.778,    0.108,   12.778,    0.061,    3.400,   &
      33.333,    0.038,   16.429,    0.266,    0.125,   16.000,   &
       0.095,  115.000,    0.005,    0.062,    0.015,    2.250,   &
       2.250,    0.288,    2.125,    0.281,    0.750,    1.889,   &
       0.080,    1.885,    0.083,    0.035,    5.667,    0.225,   &
       0.096,    0.258,   52.581,    0.001,    1.125,   17.000,   &
      52.581,    0.227,    0.022,    1.000,   30.000,    1.000,   &
       0.136,  190.000,    0.224,    2.250,    0.556,    0.005,   &
      20.000,   17.000,    0.003,    0.170,    0.012,   17.000,   &
     100.000,    0.170,    3.400,    0.021,    1.620,    0.021,   &
       0.751,  625.000,    0.022,    8.000,    0.450,    0.751,   &
      55.000,    0.531,    0.751,    7.000,    0.751,    0.225,   &
      15.000,    1.620,   40.000,    0.751,    0.405,    0.024,   &
       0.218,    0.024,    0.140,  140.000,    0.751,    0.895,   &
       0.279,    0.102,    4.444,    2.250,    0.083,    0.175,   &
      70.000,    0.225,    0.173,    0.061,    2.250,    0.027,   &
       5.667,    0.027,    0.187,  115.000,   38.235,    0.029,   &
       2.250,    0.177,    0.708,    0.157,    0.038,   28.750,   &
     115.000,    0.062,    0.088,    0.515,  277.778,    0.039,   &
       7.667,    0.042,    1.625,    5.667,    0.006,    0.321,   &
       0.006  &
    /
  data velev / &
        1185,     5230,     3676,     3794,     1330,     1222,   &
        2552,     2968,     3350,     2960,      688,     1536,   &
        1334,     3850,     2847,      704,     1413,     4784,   &
         321,     1807,      915,     1023,     5426,     1325,   &
         799,      813,     4835,     2882,     2857,     3800,   &
        1784,     1717,     4095,     4095,     1703,     4095,   &
        3283,     2891,     2857,     2857,     3428,     1745,   &
        4317,     3763,     1061,     1018,      799,     2462,   &
        2799,     2631,      915,      915,      728,     3283,   &
        5023,     2334,     5023,     5023,      635,     1700,   &
         704,     3058,     3058,     4835,     3058,     2631,   &
         799,     1491,     1491,     1491,      321,     2891,   &
        2882,     4276,      737,     5967,     1580,     1784,   &
        2745,     4095,      813,     1807,     2631,      815,   &
        2997,     3428,     2462,     2882,     5592,     4835,   &
        2552,      990,      815,      815,     1952,      815,   &
        2799,     1131,      815,     1750,     2334,     2334,   &
         704,      851,     2329,     2329,     2329,     5426,   &
        5426,     5426,      799,     5426,     2462,      815,   &
        2334,     1730,     3058,     1730,      321,     1730,   &
        3058,     2631,     2891,      635,     5426,     5426,   &
        2334,     2334,     1745,     3800,     1325,     1413,   &
        2631,     3350,      813,     2882,      915,      915,   &
        5023,     5023,     2334,      990,      161,     2597,   &
        2741,     1370,     2552,     1536,     4784,     2882,   &
        3350,     3763,     2631,     1807,     2130,     3470,   &
        3470,     1816,     1580,      833,     4835,     4784,   &
         704,     3470,     1330,     1745,     2552,     4276,   &
        3332,      990,     3058,     3058,     2799,      140,   &
        3058,      394,     2334,     2507,      725,      725,   &
         725,      688,     3470,     2462,     4784,     1703,   &
        3350,     5592,     3350,     3350,     3350,     3562,   &
        3350,     3470,     2665,     2665,     2665,     2631,   &
        2631,     2631,     3562,     2435,     3470,     1580,   &
        2882,     4835,     3470,     2568,      704,     3470,   &
        2435,     3350,     2462,     3125,     2334,     4784,   &
        1816,      990,      790,      790,      790,     1807,   &
        2847,      790,     3470,     2631,     1703,      790,   &
        1413,     1413,     2745,     1413,     1745,     1592,   &
         915,      915,     2882,     1715,     3212,     1784,   &
        1784,     1580,      635,     2462,     1807,     5592,   &
        1592,     2882,     1330,     1703,     3350,      833,   &
        2507,      915,      704,     1784,     2334,      790,   &
        3332,     2631,     3058,     1325,     2857,     5426,   &
        3058,     1320,     2462,     2329,     2329,     2329,   &
        3800,     1230,     1703,      635,     4276,     2552,   &
        2060,     2891,     2847,     2568,     3350,     1413,   &
        2568,     2568,     3726,     2549,     1784,      799,   &
        1807,     1725,     3562,     1807,     1061,     1807,   &
        1807,     1330,     1807,     1807,     1807,      815,   &
        1784,     1784,     1807,     1807,     2507,     5426,   &
        4835,      688,     2435,     1807,     1807,     1156,   &
        1413,     1703,     2631,     1533,     1816,     2334,   &
         790,      790,     2597,      815,     1592,      915,   &
        2361,     1330,     1784,     5592,     1476,      354,   &
        2381,     1730,     3332,     1700,     2507,     1442,   &
        2381,      990,     2631,      564,     1490,     1413,   &
        2361,     4276,     1496,     2882,     1252,     3350,   &
        1784  &
    /
  data celev / &
        9000,     9000,     9000,     6794,     9000,     1772,   &
        9000,     5968,     9000,     3510,     3688,     9000,   &
        1884,     9000,     3397,     3704,     9000,     9000,   &
        3321,     9000,     9000,     1073,     9000,     4325,   &
        1349,     3813,     7835,     5882,     9000,     6800,   &
        9000,     2267,     7095,     7095,     4703,     7095,   &
        6283,     5891,     9000,     9000,     3978,     4745,   &
        4867,     6763,     4061,     1068,     1349,     9000,   &
        3349,     3181,     9000,     9000,     3728,    18000,   &
        8023,     2884,     8023,     8023,     1185,     2250,   &
        3704,     6058,     6058,     7835,     6058,     3181,   &
        3799,     9000,     9000,     9000,     9000,     5891,   &
        5882,     4826,     3737,     8967,     4580,     9000,   &
        5745,     7095,     1363,     4807,     3181,     9000,   &
        5997,     3978,     5462,     5882,     8592,     7835,   &
        3102,     1040,     9000,     9000,     2502,     9000,   &
        3349,     1681,     9000,     2300,    18000,    18000,   &
        1254,     1401,     5329,     5329,     5329,     9000,   &
        9000,     9000,     9000,     9000,     9000,     3815,   &
        9000,    18000,     6058,    18000,     3321,    18000,   &
        6058,     3181,     5891,     1185,     9000,     9000,   &
        9000,     9000,     2295,     6800,     1375,     9000,   &
        3181,     6350,     1363,     9000,     9000,     9000,   &
        8023,     8023,     5334,     1040,      711,     3147,   &
        3291,     1920,     3102,     9000,     5334,     5882,   &
        3900,     6763,     3181,     9000,     2680,     4020,   &
        4020,     4816,     4580,     1383,     7835,     5334,   &
        3704,     6470,     4330,     2295,     3102,     4826,   &
        6332,     1040,     6058,     6058,     3349,     3140,   &
        6058,     3394,     5334,     3057,    18000,    18000,   &
       18000,     3688,     6470,     3012,     5334,     2253,   &
        9000,     8592,     3900,     9000,     3900,    18000,   &
        9000,     6470,     5665,     5665,     5665,     5631,   &
        5631,     5631,    18000,     2985,     6470,     4580,   &
        5882,     7835,     6470,     3118,     3704,     6470,   &
        2985,     3900,     5462,     6125,     5334,     5334,   &
        4816,     1040,     9000,     9000,     9000,     2357,   &
        3397,     9000,     6470,     3181,     4703,     9000,   &
        9000,     9000,     2795,     9000,     2295,     4592,   &
        9000,     9000,     9000,     4715,     3762,     9000,   &
        9000,     9000,      685,     2512,     2357,     6142,   &
        2142,     9000,     4330,     2253,     3900,     3833,   &
        5507,     9000,     3704,     4784,     5334,     9000,   &
        6332,     2681,     6058,     1375,     3407,     8426,   &
        6058,     4320,     3012,     5329,     5329,     5329,   &
        6800,     1780,     4703,     1185,     9000,     3102,   &
        2110,     5891,     3397,     5568,     3900,     4413,   &
        5568,     5568,     6726,     5549,     9000,     3799,   &
       18000,     9000,     6562,    18000,     1611,    18000,   &
       18000,     4330,    18000,    18000,    18000,     1365,   &
        9000,     9000,    18000,    18000,     5507,     8426,   &
        7835,     3688,     5435,    18000,    18000,     4156,   &
        4413,     2253,     2681,     2083,     2366,     5334,   &
        9000,     9000,     5597,     1365,     2142,     3915,   &
        5361,     4330,     4784,     9000,     4476,     3354,   &
        2931,     4730,     6332,     4700,     3057,     9000,   &
        9000,     1040,     2681,     3564,     9000,     4413,   &
        9000,     7276,     4496,     5882,     1802,     3900,   &
        2334  &
    /
  data lon / &
     127.880,  281.659,  112.920,  167.170,  148.420,  204.708,   &
     269.399,  110.442,   15.004,   35.902,  152.203,  159.430,   &
     168.120,  256.380,  288.070,  130.308,  168.346,  281.402,   &
     177.180,  145.061,  297.820,  332.680,  261.378,  127.642,   &
     129.716,  105.423,  160.638,  160.587,  196.030,  101.264,   &
     125.400,  115.375,    9.170,    9.170,  122.775,    9.170,   &
     161.360,  100.473,  196.030,  196.030,  109.208,  272.996,   &
     215.980,  269.120,  273.155,  123.590,  129.716,  123.685,   &
     114.242,   55.713,  297.820,  297.820,  273.298,  161.360,   &
     281.558,  151.330,  281.558,  281.558,  273.839,  274.378,   &
     130.308,   29.200,   29.200,  160.638,   29.200,   55.713,   &
     129.716,  340.300,  340.300,  340.300,  177.180,  100.473,   &
     160.587,  282.630,  140.843,  288.150,  124.792,  124.725,   &
      73.513,    9.170,  105.423,  145.061,   55.713,  139.529,   &
     288.830,  109.208,  123.685,  160.587,  292.270,  160.638,   &
     269.399,  333.550,  139.529,  139.529,  102.620,  139.529,   &
     114.242,  140.681,  139.529,  155.195,  151.330,  151.330,   &
     130.308,  165.800,  112.950,  112.950,  112.950,  261.378,   &
     261.378,  261.378,  129.716,  261.378,  123.685,  139.529,   &
     151.330,  190.056,   29.200,  190.056,  177.180,  190.056,   &
      29.200,   55.713,  100.473,  273.839,  261.378,  261.378,   &
     151.330,  151.330,  272.996,  101.264,  127.642,  168.346,   &
      55.713,   15.004,  105.423,  160.587,  297.820,  297.820,   &
     281.558,  281.558,  151.330,  333.550,  141.290,  100.679,   &
     158.830,  333.670,  269.399,  159.430,  281.402,  160.587,   &
      15.004,  269.120,   55.713,  145.061,  271.731,   29.250,   &
      29.250,  155.458,  124.792,  168.370,  160.638,  281.402,   &
     130.308,   29.250,  148.420,  272.996,  269.399,  282.630,   &
     114.042,  333.550,   29.200,   29.200,  114.242,  148.121,   &
      29.200,  140.306,  151.330,  200.620,  125.425,  125.425,   &
     125.425,  152.203,   29.250,  123.685,  281.402,  122.775,   &
      15.004,  292.270,   15.004,   15.004,   15.004,  282.344,   &
      15.004,   29.250,  107.730,  107.730,  107.730,   55.713,   &
      55.713,   55.713,  282.344,  123.132,   29.250,  124.792,   &
     160.587,  160.638,   29.250,  138.526,  130.308,   29.250,   &
     123.132,   15.004,  123.685,  288.271,  151.330,  281.402,   &
     155.458,  333.550,  145.670,  145.670,  145.670,  145.061,   &
     288.070,  145.670,   29.250,   55.713,  122.775,  145.670,   &
     168.346,  168.346,   73.513,  168.346,  272.996,  131.106,   &
     297.820,  297.820,  160.587,  127.325,  288.623,  124.725,   &
     124.725,  124.792,  273.839,  123.685,  145.061,  292.270,   &
     131.106,  160.587,  148.420,  122.450,   15.004,  168.370,   &
     200.620,  297.820,  130.308,  125.400,  151.330,  145.670,   &
     114.042,   55.713,   29.200,  127.642,  196.030,  261.378,   &
      29.200,  125.500,  123.685,  112.950,  112.950,  112.950,   &
     101.264,   37.750,  122.450,  273.839,  282.630,  269.399,   &
     347.720,  100.473,  288.070,  138.526,   15.004,  168.346,   &
     138.526,  138.526,  116.470,  237.820,  124.725,  129.716,   &
     145.061,  342.670,  282.344,  145.061,  273.155,  145.061,   &
     145.061,  148.420,  145.061,  145.061,  145.061,  139.529,   &
     124.725,  124.725,  145.061,  145.061,  200.620,  261.378,   &
     160.638,  152.203,  123.132,  145.061,  145.061,  156.020,   &
     168.346,  122.450,   55.713,  185.846,  155.458,  151.330,   &
     145.670,  145.670,  100.679,  139.529,  131.106,  297.820,   &
      43.380,  148.420,  124.725,  292.270,  268.450,   93.858,   &
     270.370,  190.056,  114.042,  274.378,  200.620,   40.480,   &
     270.370,  333.550,   55.713,  150.030,  268.830,  168.346,   &
      43.380,  282.630,  167.830,  160.587,  206.570,   15.004,   &
     124.725  &
    /
  data lat / &
       1.680,   -2.002,   -8.108,  -77.530,   -5.525,   19.425,   &
      14.381,   -7.542,   37.734,   -2.751,   -4.271,   54.050,   &
     -16.250,   19.514,  -39.420,   30.789,  -16.507,   -0.171,   &
     -37.520,   -4.100,   16.720,   38.730,   19.023,    1.475,   &
      29.635,   -6.102,   56.057,   55.978,   54.756,   -1.814,   &
       2.780,   -8.242,    4.203,    4.203,   -8.530,    4.203,   &
      56.653,   -0.381,   54.756,   54.756,   -7.242,   12.702,   &
      62.000,   14.473,   12.602,   -8.540,   29.635,   13.257,   &
      -8.058,  -21.229,   16.720,   16.720,   12.506,   56.653,   &
      -1.467,   -5.050,   -1.467,   -1.467,   11.984,   11.538,   &
      30.789,   -1.408,   -1.408,   56.057,   -1.408,  -21.229,   &
      29.635,   63.980,   63.980,   63.980,  -37.520,   -0.381,   &
      55.978,    1.220,   42.541,  -15.780,    1.358,    1.108,   &
     -53.106,    4.203,   -6.102,   -4.100,  -21.229,   34.079,   &
     -37.850,   -7.242,   13.257,   55.978,  -23.370,   56.057,   &
      14.381,  -57.780,   34.079,   34.079,   -3.520,   34.079,   &
      -8.058,   42.061,   34.079,   -6.140,   -5.050,   -5.050,   &
      30.789,  -10.380,   -7.942,   -7.942,   -7.942,   19.023,   &
      19.023,   19.023,   29.635,   19.023,   13.257,   34.079,   &
      -5.050,   52.825,   -1.408,   52.825,  -37.520,   52.825,   &
      -1.408,  -21.229,   -0.381,   11.984,   19.023,   19.023,   &
      -5.050,   -5.050,   12.702,   -1.814,    1.475,  -16.507,   &
     -21.229,   37.734,   -6.102,   55.978,   16.720,   16.720,   &
      -1.467,   -1.467,   -5.050,  -57.780,   24.754,   -0.978,   &
      53.255,  -58.420,   14.381,   54.050,   -0.171,   55.978,   &
      37.734,   14.473,  -21.229,   -4.100,   13.434,   -1.520,   &
      -1.520,   50.325,    1.358,  -16.680,   56.057,   -0.171,   &
      30.789,   -1.520,   -5.525,   12.702,   14.381,    1.220,   &
      -8.125,  -57.780,   -1.408,   -1.408,   -8.058,   -5.520,   &
      -1.408,   30.480,   -5.050,   56.170,    2.280,    2.280,   &
       2.280,   -4.271,   -1.520,   13.257,   -0.171,   -8.530,   &
      37.734,  -23.370,   37.734,   37.734,   37.734,   -0.077,   &
      37.734,   -1.520,   -7.320,   -7.320,   -7.320,  -21.229,   &
     -21.229,  -21.229,   -0.077,   10.412,   -1.520,    1.358,   &
      55.978,   56.057,   -1.520,   36.403,   30.789,   -1.520,   &
      10.412,   37.734,   13.257,  -38.692,   -5.050,   -0.171,   &
      50.325,  -57.780,   16.350,   16.350,   16.350,   -4.100,   &
     -39.420,   16.350,   -1.520,  -21.229,   -8.530,   16.350,   &
     -16.507,  -16.507,  -53.106,  -16.507,   12.702,   32.881,   &
      16.720,   16.720,   55.978,    0.800,  -36.863,    1.108,   &
       1.108,    1.358,   11.984,   13.257,   -4.100,  -23.370,   &
      32.881,   55.978,   -5.525,   -8.670,   37.734,  -16.680,   &
      56.170,   16.720,   30.789,    2.780,   -5.050,   16.350,   &
      -8.125,  -21.229,   -1.408,    1.475,   54.756,   19.023,   &
      -1.408,    3.670,   13.257,   -7.942,   -7.942,   -7.942,   &
      -1.814,  -46.900,   -8.670,   11.984,    1.220,   14.381,   &
     -37.092,   -0.381,  -39.420,   36.403,   37.734,  -16.507,   &
      36.403,   36.403,   -8.420,   46.200,    1.108,   29.635,   &
      -4.100,   64.420,   -0.077,   -4.100,   12.602,   -4.100,   &
      -4.100,   -5.525,   -4.100,   -4.100,   -4.100,   34.079,   &
       1.108,    1.108,   -4.100,   -4.100,   56.170,   19.023,   &
      56.057,   -4.271,   10.412,   -4.100,   -4.100,   50.680,   &
     -16.507,   -8.670,  -21.229,   52.381,   50.325,   -5.050,   &
      16.350,   16.350,   -0.978,   34.079,   32.881,   16.720,   &
     -11.750,   -5.525,    1.108,  -23.370,   -0.370,   12.278,   &
      13.853,   52.825,   -8.125,   11.538,   56.170,   12.600,   &
      13.853,  -57.780,  -21.229,   -5.450,   -0.830,  -16.507,   &
     -11.750,    1.220,  -15.400,   55.978,   59.363,   37.734,   &
       1.108  &
    /

! ======= Replace code between like symbols with data statements ========
! Note: I know now that this code below for searching on lon/lat is
! wrong because GEOS-5 uses radians now to represent lon/lat
  dx = w_c%grid%lon_del
  dy = w_c%grid%lat_del

! If getting from daily tables
   do it = 1, nvolc
!    print *, it, gcSU%nymd, startday(it), endday(it)
    if(gcSU%nymd .lt. startday(it) .or. gcSU%nymd .gt. endday(it)) cycle
    do j = j1, j2
     if(lat(it) .lt. w_c%grid%lat(j)-dy/2 .or. &
       lat(it) .ge. w_c%grid%lat(j)+dy/2 ) cycle
     do i = i1, i2
      if(lon(it) .lt. w_c%grid%lon(i)-dx/2 .or. &
         lon(it) .ge. w_c%grid%lon(i)+dx/2 ) cycle
      cellarea = (2.*pi*rearth/360.)**2. * dx*dy * cos(2.*pi/360. * w_c%grid%lat(j))
      SO2Volcexp(i,j) = so2exp(it) * 1.e6/86400/cellarea     ! to kg SO2/sec/m2
      ElvVolcexp(i,j) = velev(it)
      cElvVolcexp(i,j) = celev(it)
     enddo
    enddo
   enddo

end subroutine getvolcexp




!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_ChemDrv - Do SU cycle chemistry following GOCART
!
! !INTERFACE:
!

   subroutine SU_ChemDrv ( i1, i2, j1, j2, km, nbins, jday, cdt, xhour, gcSU, &
                           w_c, ustar, u, v, shflux, oro, pblh, tmpu, &
                           cloud, rhoa, &
                           su_dep, &
                           su_pSO2, su_pMSA, su_pSO4g, su_pSO4aq, &   ! 2d diagnostics
                           pSO2, pMSA, pSO4g, pSO4aq,  &              ! 3d diagnostics
                           rc)

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins, jday
   real, intent(in)    :: cdt, xhour
   type(SU_GridComp), intent(inout)    :: gcSU       ! SU Grid Component
   real, pointer, dimension(:,:,:)     :: tmpu, cloud, rhoa, u, v
   real, pointer, dimension(:,:)       :: ustar, shflux, oro, pblh

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: su_dep(nbins)  ! Mass lost by deposition
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
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
!  Variables for SZA which we calculate simply following szangle code
!  from GOCART without worrying about diurnal variability -- the SZA
!  should come from elsewhere in the code, or possibly be stored in the
!  chem bundle w_c
   real :: xHourUse
   real :: cossza(i1:i2,j1:j2), sza(i1:i2,j1:j2)
   real :: tcosz(i1:i2,j1:j2), tday(i1:i2,j1:j2)
   integer :: ndystep, i, j, k, im
   real :: pSO2_DMS(i1:i2,j1:j2,1:km), pMSA_DMS(i1:i2,j1:j2,1:km), &
           pSO4g_SO2(i1:i2,j1:j2,1:km), pSO4aq_SO2(i1:i2,j1:j2,1:km)

!  Variables used in chemistry step
   real :: drydepf(i1:i2,j1:j2)
   real :: xoh(i1:i2,j1:j2,km), xno3(i1:i2,j1:j2,km), xh2o2(i1:i2,j1:j2,km)
   real :: qmin, qmax, tnight
   integer :: ijl, ijkl

   ijl  = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   ijkl = ijl * km

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

!  Reset the dry deposition fluxes & frequencies
   do n = 1, nbins
    if( associated(su_dep(n)%data2d) ) su_dep(n)%data2d(i1:i2,j1:j2) = 0.0
   end do
   call SU_DepFreq ( i1, i2, j1, j2, km, nbins, cdt, w_c, &
                     tmpu, rhoa, ustar, u, v, shflux, oro, &
                     pblh, drydepf, rc )

!  Set the initial values for the chemical fields
!  H2O2 will use the gcSU%h2o2_int value (which is reset above every 3 hours)
   xh2o2 = gcSU%h2o2_int

!  defaults
   xoh   = gcSU%oh_conc
   xno3  = gcSU%no3_mr
   cossza(:,:) = 0.

!  Want to find the sum of the cos(sza) for use in scaling OH diurnal variation
!  tcosz is the sum of cossza over the whole day
!  tday is the time of day spent in light
!  Requires integrating over future times, so cannot use w_c%cosz
   xHourUse = xHour
   ndystep = 86400. / cdt
   tcosz(:,:) = 0.
   tday(:,:) = 0.
   do n = 1, ndystep
    call szangle(jday, xHourUse, w_c, sza, cossza, i1, i2, j1, j2)
    tcosz = tcosz + cossza
    xHourUse = xHourUse + cdt/3600.
    if(xHourUse .gt. 24.) xHourUse = xHourUse - 24.
!   Find the daylight portion of the day
    do j = j1, j2
     do i = i1, i2
      if(cossza(i,j) .gt. 0.) tday(i,j) = tday(i,j) + cdt
     end do
    end do
   end do

!  Find the cos(sza) now for use in scaling OH and NO3
   if( associated(w_c%cosz) ) then
      do j = j1, j2
       do i = i1, i2
        cossza(i,j) = w_c%cosz(i,j)
       enddo
      enddo
   else
      call szangle(jday,xHour,w_c,sza,cossza,i1,i2,j1,j2)
   endif

!  Compute the diurnal variation in OH and NO3
   do k = 1, km
    do j = j1, j2
     do i = i1, i2
      if(tcosz(i,j) .gt. 0.) then
       xoh(i,j,k) = gcSU%oh_conc(i,j,k)*(86400./cdt)*cossza(i,j) / tcosz(i,j)
      else
       xoh(i,j,k) = 0.
      endif
      if(xoh(i,j,k) .lt. 0.) xoh(i,j,k) = 0.
!     If there is daylight then no3 is small (assume zero) and the
!     average is distributed only over the night time portion
      tnight = (86400.-tday(i,j))
      if( cossza(i,j) .gt. 0. .OR. tnight < tiny(1.0)) then
       xno3(i,j,k) = 0.
      else
       xno3(i,j,k) = gcSU%no3_mr(i,j,k) * 86400./ tnight
      endif
      if(xno3(i,j,k) .lt. 0.) xno3(i,j,k) = 0.     !! Sarah Lu
     end do
    end do
   end do

!  Now call the chemistry packages...
!  ----------------------------------

!  DMS source and oxidation to SO2 and MSA
   call SU_ChemDrv_DMS( i1, i2, j1, j2, km, nbins, cdt, xoh, xno3, rhoa, &
                        gcSU, w_c, tmpu, cossza, pSO2_DMS, pMSA_DMS, &
                        drydepf, su_dep, rc)
   if( associated(pSO2%data3d) ) &
     pSO2%data3d(i1:i2,j1:j2,1:km) = pSO2_DMS(i1:i2,j1:j2,1:km)
   if( associated(su_pSO2%data2d)) then
     do k = 1, km
      su_pSO2%data2d(i1:i2,j1:j2) &
        =   su_pSO2%data2d(i1:i2,j1:j2) &
          + pSO2_DMS(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
     enddo
   endif

   if( associated(pMSA%data3d) ) &
     pMSA%data3d(i1:i2,j1:j2,1:km) = pMSA_DMS(i1:i2,j1:j2,1:km)
   if( associated(su_pMSA%data2d)) then
     do k = 1, km
      su_pMSA%data2d(i1:i2,j1:j2) &
        =   su_pMSA%data2d(i1:i2,j1:j2) &
          + pMSA_DMS(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
     enddo
   endif


!  SO2 source and oxidation to SO4
   call SU_ChemDrv_SO2( i1, i2, j1, j2, km, nbins, cdt, xoh, xh2o2, rhoa, &
                        gcSU, w_c, tmpu, cloud, pSO2_DMS, pSO4g_SO2, &
                        pSO4aq_SO2, drydepf, oro, su_dep, rc)
   if( associated(pSO4g%data3d) ) &
     pSO4g%data3d(i1:i2,j1:j2,1:km) = pSO4g_SO2(i1:i2,j1:j2,1:km)
   if( associated(su_pSO4g%data2d)) then
     do k = 1, km
      su_pSO4g%data2d(i1:i2,j1:j2) &
        =   su_pSO4g%data2d(i1:i2,j1:j2) &
          + pSO4g_SO2(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
     enddo
   endif

   if( associated(pSO4aq%data3d) ) &
     pSO4aq%data3d(i1:i2,j1:j2,1:km) = pSO4aq_SO2(i1:i2,j1:j2,1:km)
   if( associated(su_pSO4aq%data2d)) then
     do k = 1, km
      su_pSO4aq%data2d(i1:i2,j1:j2) &
        =   su_pSO4aq%data2d(i1:i2,j1:j2) &
          + pSO4aq_SO2(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
     enddo
   endif

!  SO4 source and loss
   call SU_ChemDrv_SO4( i1, i2, j1, j2, km, nbins, cdt, gcSU, w_c, &
                        pSO4g_SO2, pSO4aq_SO2, drydepf, su_dep, rc)


!  MSA source and loss
   call SU_ChemDrv_MSA( i1, i2, j1, j2, km, nbins, cdt, gcSU, w_c, &
                        pMSA_DMS, drydepf, su_dep, rc)

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
! !IROUTINE:  SU_DepFreq - Calculate SU dry deposition frequency
!                          for lowest layer
!
! !INTERFACE:
!

   subroutine SU_DepFreq ( i1, i2, j1, j2, km, nbins, cdt, w_c, &
                           tmpu, rhoa, ustar, u, v, shflux, oro, &
                           pblh, drydepf, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   real, pointer, dimension(:,:,:) :: tmpu      ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa      ! temperature [K]
   real, pointer, dimension(:,:)   :: ustar     ! friction speed
   real, pointer, dimension(:,:,:) :: u         ! u-wind component [m s-1]
   real, pointer, dimension(:,:,:) :: v         ! v-wind component [m s-1]
   real, pointer, dimension(:,:)   :: pblh      ! planetary boundary layer
                                                ! height [m]
   real, pointer, dimension(:,:)   :: shflux    ! surface sensible heat flux
                                                ! [W m-2]
   real, pointer, dimension(:,:)   :: oro       ! surface type flag
   type(Chem_Bundle), intent(in)   :: w_c

! !OUTPUT PARAMETERS:
   real, intent(out)                :: drydepf(i1:i2,j1:j2)
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 

! !DESCRIPTION: Calculates the deposition velocity for the lowest model
!               layer for removal of aerosol and gaseous species.  The
!               formulation comes from Chin's GOCART module, but there
!               some notes worth making here.  For one, Chin's comments
!               in her code claim to separate gaseous from particulate
!               deposition.  This separation does not appear to be 
!               implemented, and we ignore it here.  There are comments
!               pointing to other references for how better to do this
!               but for the moment we follow her method.  Additionally,
!               GOCART is attached to a land-surface type database which
!               is used in practice only to set a maximum allowable
!               deposition velocity (0.1 cm s-1 for desert and water and
!               1 cm s-1 everywhere else).  I haven't attached this
!               database, so I will rely on the orography flag to set
!               a maximum deposition velocity of 1 cm s-1 over water and
!               0.1 cm s-1 everywhere else.
!  NOTE: Here we just compute the deposition velocity (and, equivalently,
!        the deposition frequency).  The actual deposition calculation is
!        done in the chemistry routine.
!        The deposition frequency is *now* independent of species!
!        A more sophisticated treatment would actually use information from
!        the LSM and about the species to handle this!
!
! !REVISION HISTORY:
!
!  31JUL2004, Colarco
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   character(len=*), parameter :: myname = 'SU_Deposition'
   integer :: i, j, k, n, nbeg, nend
   real :: pm(i1:i2,j1:j2)           ! pressure [Pa]
   real :: dz(i1:i2,j1:j2)           ! lowest layer thickness
   real :: vdep        ! Deposition speed [m s-1]
   real :: wSfc        ! lowest layer wind speed
   real :: Ra          ! aerodynamic resistance
   real :: Rs          ! surface resistance
   real :: obk         ! Monin-Obhukov length
   real :: czh         ! mixing depth term
   real :: vds         ! surface resistance deposition velocity
   real :: vdsMax      ! surface dependent maximum value of vds
   real :: Rttl        ! total surface resistance

!  Calculate the pressure, air density, and thickness of the surface level
   pm = 0.5*w_c%delp(i1:i2,j1:j2,1)
   do k = 2, km
    pm = pm + 0.5*(w_c%delp(i1:i2,j1:j2,k)+w_c%delp(i1:i2,j1:j2,k-1))
   end do
   dz = w_c%delp(i1:i2,j1:j2,km)/rhoa(i1:i2,j1:j2,km)/grav

!  Loop over space
   do j = j1, j2
    do i = i1, i2

!     Calculate the aerodynamic resistance assuming the drag coefficient for
!     momentum is appropriate
      wSfc = sqrt(u(i,j,km)**2. + v(i,j,km)**2.)
      Ra = wSfc/ustar(i,j)**2.

!     Calculate the Monin-Obhukov length:
!            -Air denisity * Cp * T(surface) * Ustar^3
!     OBK = -------------------------------------------
!                 vK * g * Sensible heat flux
!     vK = 0.4               von Karman constant
!     Cp = 1000 J kg-1 K-1   specific heat of air at constant pressure
!     If OBK < 0 the air is unstable; if OBK > 0 the air is stable
!     For sensible heat flux of zero OBK goes to infinity (set to 1.e5)
      if(shflux(i,j) .eq. 0.) then
       obk = 1.e5
      else
       obk =   -rhoa(i,j,km)*cpd*tmpu(i,j,km)*ustar(i,j)**3. &
             / (von_karman*grav*shflux(i,j))
      endif

!     Calculate the surface resistance term
      vds = 0.002*ustar(i,j)
      if(obk .lt. 0.) vds = vds*(1.+(-300./obk)**0.6667)
      czh = pblh(i,j)/obk
      if(czh .lt. -30.) vds = 0.0009*ustar(i,j)*(-czh)**0.6667
      if(oro(i,j) .eq. OCEAN) then
       vdsMax = 0.001
      else
       vdsMax = 0.01
      endif
      Rs = 1./min(vds,vdsmax)

!     Now what is the deposition velocity
      Rttl = Ra + Rs
      vdep = 1./Rttl

!     Set a minimum value of deposition velocity
      vdep = max(vdep,1.e-4)

!     Save the dry deposition frequency for the chemical removal terms
!     in units of s-1
      drydepf(i,j) = vdep / dz(i,j)

    end do   ! i
   end do    ! j

   rc = 0

   end subroutine SU_DepFreq



!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_ChemDrv_DMS - Do SU cycle chemistry following GOCART
!  NOTE: This is the DMS oxidation subroutine.  This follows from GOCART:

!
! !INTERFACE:
!

   subroutine SU_ChemDrv_DMS( i1, i2, j1, j2, km, nbins, cdt, xoh, xno3, rhoa, &
                              gcSU, w_c, tmpu, cossza, pSO2_DMS, pMSA_DMS, &
                              drydepf, fluxOut, rc) 

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   real, pointer, dimension(:,:,:)  :: tmpu, rhoa
   real, intent(in)    :: drydepf(i1:i2,j1:j2)
   real, intent(in)    :: xoh(i1:i2,j1:j2,km), xno3(i1:i2,j1:j2,km)
   real, intent(in)    :: cossza(i1:i2,j1:j2)
   type(SU_GridComp), intent(in)    :: gcSU       ! SU Grid Component


! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: fluxout(nbins)  ! Mass lost by deposition
                                                   ! to surface, kg/m2/s
   real, intent(out)   :: pSO2_DMS(i1:i2,j1:j2,km), pMSA_DMS(i1:i2,j1:j2,km)
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'SU_ChemDrv'

! !DESCRIPTION: Computes the production of SO2 and MSA due to DMS oxidation
!
!   R1:    DMS + OH  -> a*SO2 + b*MSA                OH addition channel
!          k1 = { 1.7d-42*exp(7810/T)*[O2] / (1+5.5e-31*exp(7460/T)*[O2] }
!          a = 0.75, b = 0.25
!
!   R2:    DMS + OH  ->   SO2 + ...                  OH abstraction channel
!          k2 = 1.2e-11*exp(-260/T)
!
!      DMS_OH = DMS0 * exp(-(r1+r2)*NDT1)
!          where DMS0 is the DMS concentration at the beginning,
!          r1 = k1*[OH], r2 = k2*[OH]
!
!   R3:    DMS + NO3 ->   SO2 + ...
!          k3 = 1.9e-13*exp(500/T)
!
!      DMS = DMS_OH * exp(-r3*NDT1)
!          where r3 = k3*[NO3]
!
!   R4:    DMS + X   ->   SO2 + ...
!          assume to be at the rate of DMS+OH and DMS+NO3 combined.
!
!   The production of SO2 and MSA here, PSO2_DMS and PMSA_DMS, are saved
!   for use in CHEM_SO2 and CHEM_MSA subroutines as a source term.  They
!   are in unit of MixingRatio/second.
!
! !REVISION HISTORY:
!
!  06Nov2003, Colarco
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer :: i, j, k, n, nbeg, nend
   real*8  :: Fx, a, b, eff
   real*8  :: rk1, rk2, rk3, rk4
   real*8  :: tk, o2, oh, no3, air
   real*8  :: dms, dms0, dms_oh
   real, parameter :: nAvagadro  = 6.022e23 ! molecules per mole of air
   integer :: nDMS, nSO2, nSO4, nMSA
   real :: qmin, qmax

   data Fx  / 1.0 /
   data a   / 0.75 /
   data b   / 0.25 /
   data eff / 1. /
   
!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_SU
   nend  = w_c%reg%j_SU
   nDMS = gcSU%nDMS
   nSO2 = gcSU%nSO2
   nSO4 = gcSU%nSO4
   nMSA = gcSU%nMSA


!  spatial loop 
   do k = 1, km
    do j = j1, j2
     do i = i1, i2

      rk1 = 0.
      rk2 = 0.
      rk3 = 0.
      rk4 = 0.

      tk  = tmpu(i,j,k)
      oh  = xoh(i,j,k)
!     air molecules in # cm-3
      air = 1000.*rhoa(i,j,k) / airMolWght * nAvagadro * 1.e-6
!     oxygen molecules in # cm-3
      o2 = 0.21 * air
!     no3 -> go from volume mixing ratio to # cm-3
      no3 = xno3(i,j,k) * air

!     initial DMS concentration (kg kg-1)
      dms0 = w_c%qa(nbeg+nDMS-1)%data3d(i,j,k)
      dms0 = max(dms0,tiny(dms0))

!     1 & 2) DMS + OH: RK1 = addition, RK2 = abstraction
      if(oh .gt. 0.) then
       rk1 = (1.7d-42 * exp(7810./tk) * o2) / &
             (1. + 5.5e-31 * exp(7460./tk) * o2) * oh
       rk2 = (1.2e-11 * exp(-260./tk)) * oh
      endif

!     3) DMS + NO3: only happens at night
      if(cossza(i,j) .le. 0.) then
       rk3 = (1.9e-13 * exp(500./tk)) * no3
      endif

!     Now do the DMS loss
      dms_oh = dms0   * exp( -(rk1+rk2)* Fx * cdt)
      dms    = dms_oh * exp( -(rk3)    * Fx * cdt)

!     SO2 and MSA production terms
!     MSA is formed from the DMS+OH addition step
!     Production should go as mass mixing ratio change in MSA
      if( (rk1+rk2) .eq. 0.) then
       pMSA_DMS(i,j,k) = 0.
      else
       pMSA_DMS(i,j,k) =  (dms0 - dms_oh) * b*rk1/((rk1+rk2)*Fx) * eff &
                         * (gcSU%fMassMSA/gcSU%fMassDMS) / cdt
      endif

!     Everything else goes into SO2 formation step
      pSO2_DMS(i,j,k) = ( dms0 - dms - &
                          pMSA_DMS(i,j,k)*cdt*(gcSU%fMassDMS/gcSU%fMassMSA) &
                        ) * (gcSU%fMassSO2/gcSU%fMassDMS) / cdt


!     4) Dry deposition of DMS (not in GOCART?)
!      if(k .eq. km) rk4 = drydepf(i,j)
!      dms0 = dms
!      dms  = dms0 * exp(-rk4*cdt)
!      dms    = max(dms,1.e-32)

!     Update the mass mixing ratio and the dry deposition flux out of DMS
      dms    = max(dms,tiny(dms))
      w_c%qa(nbeg+nDMS-1)%data3d(i,j,k) = dms

     end do ! i
    end do  ! j
    if(k .eq. km .and. associated(fluxout(nDMS)%data2d) ) fluxout(nDMS)%data2d = 0.
   end do   ! k


   rc = 0

   end subroutine SU_ChemDrv_DMS


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_ChemDrv_SO2 - Do SU cycle chemistry following GOCART
!  NOTE: This is the SO2 oxidation subroutine.  This follows from GOCART:

!
! !INTERFACE:
!

   subroutine SU_ChemDrv_SO2( i1, i2, j1, j2, km, nbins, cdt, xoh, xh2o2, rhoa,&
                              gcSU, w_c, tmpu, cloud, pSO2_DMS, pSO4g_SO2, &
                              pSO4aq_SO2, drydepf, oro, fluxOut, rc) 

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   real, pointer, dimension(:,:,:)  :: tmpu, cloud, rhoa
   real, pointer, dimension(:,:)    :: oro
   real, intent(in)    :: drydepf(i1:i2,j1:j2)
   real, intent(in)    :: pSO2_DMS(i1:i2,j1:j2,km)
   real, intent(inout) :: xoh(i1:i2,j1:j2,km), xh2o2(i1:i2,j1:j2,km)
   type(SU_GridComp), intent(in)    :: gcSU       ! SU Grid Component

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: fluxout(nbins)  ! Mass lost by deposition
                                                   ! to surface, kg/m2/s
   real, intent(out)   :: pSO4g_SO2(i1:i2,j1:j2,km)
   real, intent(out)   :: pSO4aq_SO2(i1:i2,j1:j2,km)
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'SU_ChemDrv'

! !DESCRIPTION: Computes the concentration of SO2 and production of SO4
!
!  SO2 production:
!    DMS + OH, DMS + NO3 (saved in SU_ChemDrv_DMS)
!
!  SO2 loss:
!    SO2 + OH  -> SO4
!    SO2       -> drydep
!    SO2 + H2O2 or O3 (aq) -> SO4
!
!  SO2 = SO2_0 * exp(-bt)
!      + PSO2_DMS*dt/bt * [1-exp(-bt)]
!    where b is the sum of the reaction rate of SO2 + OH and the dry
!    deposition rate of SO2, PSO2_DMS is SO2 production from DMS in
!    MixingRatio/timestep.
!
!  If there is cloud in the gridbox (fraction = fc), then the aqueous
!  phase chemistry also takes place in cloud. The amount of SO2 oxidized
!  by H2O2 in cloud is limited by the available H2O2; the rest may be
!  oxidized due to additional chemistry, e.g, reaction with O3 or O2
!  (catalyzed by trace metal).
!
! !REVISION HISTORY:
!
!  06Nov2003, Colarco
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer :: i, j, k, n, nbeg, nend
   real*8  :: rk1, rk2, rk, rkt, f1
   real*8  :: L1, L2, Ld, SO2, SO2_cd, fc, fMR
   real*8  :: oh, h2o2, SO20, tk, air, k0, ki, kk
   real*8  :: dms, dms0, dms_oh
   real    :: qmin, qmax
   real, parameter :: nAvagadro  = 6.022e23 ! molecules per mole of air
   integer :: nDMS, nSO2, nSO4, nMSA
   real, dimension(i1:i2,j1:j2) :: fout

   data ki / 1.5e-12 /

!  Conversion of SO2 mmr to SO2 vmr
   fMR = airMolWght / gcSU%fMassSO2
   
!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_SU
   nend  = w_c%reg%j_SU
   nDMS = gcSU%nDMS
   nSO2 = gcSU%nSO2
   nSO4 = gcSU%nSO4
   nMSA = gcSU%nMSA

!  spatial loop 
   fout = 0.
   do k = 1, km
    do j = j1, j2
     do i = i1, i2

      rk1 = 0.
      rk2 = 0.
      L1  = 0.
      L2  = 0.
      Ld  = 0.

      tk   = tmpu(i,j,k)
      oh   = xoh(i,j,k)
      h2o2 = max(xh2o2(i,j,k),tiny(xh2o2(i,j,k)))

!     air molecules in # cm-3
      air  = 1000.*rhoa(i,j,k) / airMolWght * nAvagadro * 1.e-6
!     1) SO2 + OH(g) in s-1
      k0 = 3.0e-31 * (300./tk)**3.3
      kk = k0 * air / ki
      f1 = (1. + (log10(kk))**2.)**(-1.)
      rk1 = ( (k0*air/(1.+kk)) * 0.6**f1) * oh

!     2) rk2 is the loss of SO2 due to dry deposition.
      if(k .eq. km) then
!      drydepf calculated for aerosol
!      follow Walcek: ocean drydepf_so2 = 10*drydepf_aer
!      or if land drydepf_so2 = 3*drydepf_aer
       if(oro(i,j) .eq. OCEAN) then
        rk2 = 10.*drydepf(i,j)
       else
        rk2 = 3.*drydepf(i,j)
       endif
!       rk2 = drydepf(i,j)
      else
       rk2 = 0.
      endif

      rk = (rk1 + rk2)
      rkt = rk*cdt

!     Update the SO2 concentration
!     Originally this was solved like a simple exponential solution
!     after Jacobson eq. 13.38, which is more accurate but not mass
!     conserving.  We've already timesplit everything, so accuracy is
!     out to lunch, and I'd prefer to conserve mass.

!     initial SO2 concentration (kg kg-1) after adding source
      SO20 = w_c%qa(nbeg+nSO2-1)%data3d(i,j,k) + pSO2_DMS(i,j,k)*cdt
      SO20 = max(SO20,tiny(SO20))

      if(rk .gt. 0.) then
       SO2_cd =  SO20 * exp(-rkt)
       L1     = (SO20 - SO2_cd) * rk1/rk
       if(k .eq. km) then
        Ld    = (SO20 - SO2_cd) * rk2/rk
        fout(i,j) = Ld * w_c%delp(i,j,km)/grav/cdt
       else
        Ld    = 0.
       endif
      else
       SO2_cd = SO20
       L1     = 0.
      endif


!     Update SO2 concentration after cloud chemistry, if it occurs
      fc = cloud(i,j,k)
      if(fc .gt. 0. .and. SO2_cd .gt. 0. .and. tk .gt. 258.) then
!      Check on H2O2 vmr -> is SO2 vmr greater?
       if(fMr * SO2_cd .gt. h2o2) then
        fc = fc*(h2o2/(fMR*SO2_cd))
        h2o2 = h2o2*(1.-cloud(i,j,k))
       else
        h2o2 = h2o2*(1.-cloud(i,j,k))*(fMR*SO2_cd)/h2o2
       endif
       SO2 = SO2_cd*(1.-fc)
!      aqueous loss rate (mixing ratio/timestep)
       L2 = SO2_cd * fc
      else
       SO2 = SO2_cd
       L2 = 0.
      endif

!     Ideally you would update the H2O2 mixing ratio at this point
!     and then reset it periodically
      xh2o2(i,j,k) = max(h2o2,tiny(h2o2))

      SO2 = max(SO2,tiny(SO2))
      w_c%qa(nbeg+nSO2-1)%data3d(i,j,k) = SO2
      pSO4g_SO2(i,j,k) = L1 * (gcSU%fMassSO4/gcSU%fMassSO2) / cdt
      pSO4aq_SO2(i,j,k) = L2 * (gcSU%fMassSO4/gcSU%fMassSO2) / cdt

     end do
    end do
   end do

   if( associated(fluxout(nSO2)%data2d) ) fluxout(nSO2)%data2d = fout

   rc = 0

   end subroutine SU_ChemDrv_SO2

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_ChemDrv_SO4 - Do SU cycle chemistry following GOCART
!  NOTE: This is the SO4 production by SO2 oxidation subroutine.

!
! !INTERFACE:
!

   subroutine SU_ChemDrv_SO4( i1, i2, j1, j2, km, nbins, cdt, gcSU, w_c, &
                              pSO4g_SO2, pSO4aq_SO2, drydepf, fluxOut, rc) 

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   real, intent(in)    :: drydepf(i1:i2,j1:j2)
   real, intent(in)    :: pSO4g_SO2(i1:i2,j1:j2,km)
   real, intent(in)    :: pSO4aq_SO2(i1:i2,j1:j2,km)
   type(SU_GridComp), intent(in)    :: gcSU       ! SU Grid Component

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: fluxout(nbins)  ! Mass lost by deposition
                                                   ! to surface, kg/m2/s
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'SU_ChemDrv'

! !DESCRIPTION: Computes the concentration of SO4
!
!  SO4 production:
!    The only production term is due to SO2 oxidation.
!    GOCART includes a loss from dry deposition here (not currently in place).
!    SO4 = SO4_0 * exp(-kt) + pSO4_SO2/kt * (1.-exp(-kt))
!     where k is the dry deposition
!
! !REVISION HISTORY:
!
!  06Nov2003, Colarco
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer :: i, j, k, n, nbeg, nend
   real*8  :: rk, rkt, Ld
   real*8  :: SO4, SO40, pSO4
   integer :: nDMS, nSO2, nSO4, nMSA
   real, dimension(i1:i2,j1:j2) :: fout


!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_SU
   nend  = w_c%reg%j_SU
   nDMS = gcSU%nDMS
   nSO2 = gcSU%nSO2
   nSO4 = gcSU%nSO4
   nMSA = gcSU%nMSA

   fout = 0.
!  spatial loop 
   do k = 1, km
    do j = j1, j2
     do i = i1, i2

      pSO4 = pSO4g_SO2(i,j,k)+pSO4aq_SO2(i,j,k)

!     initial SO4 concentration (kg kg-1)
      SO40 = w_c%qa(nbeg+nSO4-1)%data3d(i,j,k)
      SO40 = max(SO40,tiny(SO40))

!     Update the SO4 concentration
!     Originally this was solved like a simple exponential solution
!     after Jacobson eq. 13.38, which is more accurate but not mass
!     conserving.  We've already timesplit everything, so accuracy is
!     out to lunch, and I'd prefer to conserve mass.
!     RK is the dry deposition frequency
      if(k .eq. km) then
       RK = drydepf(i,j)
       RKT = RK*cdt
       SO4 = (SO40 + pSO4*cdt) * exp(-rkt)
       Ld  = (SO40 - SO4 + pSO4*cdt)
       fout(i,j) = Ld * w_c%delp(i,j,km)/grav/cdt
      else
       SO4 = SO40 + pSO4*cdt
       Ld = 0.
      endif

      SO4 = max(SO4,tiny(SO4))
      w_c%qa(nbeg+nSO4-1)%data3d(i,j,k) = SO4

     end do
    end do
   end do

   if( associated(fluxout(nSO4)%data2d) ) fluxout(nSO4)%data2d = fout

   rc = 0

   end subroutine SU_ChemDrv_SO4


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_ChemDrv_MSA - Do SU cycle chemistry following GOCART
!  NOTE: This is the MSA production by DMS oxidation subroutine.

!
! !INTERFACE:
!

   subroutine SU_ChemDrv_MSA( i1, i2, j1, j2, km, nbins, cdt, gcSU, w_c, &
                              pMSA_DMS, drydepf, fluxOut, rc) 

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   real, intent(in)    :: drydepf(i1:i2,j1:j2)
   real, intent(in)    :: pMSA_DMS(i1:i2,j1:j2,km)
   type(SU_GridComp), intent(in)    :: gcSU       ! SU Grid Component

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: fluxout(nbins)  ! Mass lost by deposition
                                                   ! to surface, kg/m2/s
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'SU_ChemDrv'

! !DESCRIPTION: Computes the concentration of SO4
!
!  MSA production:
!    The only production term is due to DMS oxidation.
!    GOCART includes a loss from dry deposition here (not currently in place).
!    MSA = MSA_0 * exp(-kt) + pSO4_SO2/kt * (1.-exp(-kt))
!     where k is the dry deposition
!
! !REVISION HISTORY:
!
!  06Nov2003, Colarco
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer :: i, j, k, n, nbeg, nend
   real*8  :: rk, rkt, Ld
   real*8  :: MSA, MSA0
   integer :: nDMS, nSO2, nSO4, nMSA
   real, dimension(i1:i2,j1:j2) :: fout

!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_SU
   nend  = w_c%reg%j_SU
   nDMS = gcSU%nDMS
   nSO2 = gcSU%nSO2
   nSO4 = gcSU%nSO4
   nMSA = gcSU%nMSA

   fout = 0.
!  spatial loop 
   do k = 1, km
    do j = j1, j2
     do i = i1, i2

!     initial MSA concentration (kg kg-1)
      MSA0 = w_c%qa(nbeg+nMSA-1)%data3d(i,j,k)
      MSA0 = max(MSA0,tiny(MSA0))

!     Update the MSA concentration
!     Originally this was solved like a simple exponential solution
!     after Jacobson eq. 13.38, which is more accurate but not mass
!     conserving.  We've already timesplit everything, so accuracy is
!     out to lunch, and I'd prefer to conserve mass.
!     RK is the dry deposition frequency
      if(k .eq. km) then
       RK = drydepf(i,j)
       RKT = RK*cdt
       MSA = (MSA0 + pMSA_DMS(i,j,k)*cdt) * exp(-rkt)
       Ld  = (MSA0 + pMSA_DMS(i,j,k)*cdt - MSA)
       fout(i,j) = Ld * w_c%delp(i,j,km)/grav/cdt
      else
       MSA = MSA0 + pMSA_DMS(i,j,k)*cdt
       Ld = 0.
      endif

      MSA = max(MSA,tiny(MSA))
      w_c%qa(nbeg+nMSA-1)%data3d(i,j,k) = MSA

     end do
    end do
   end do

   if( associated(fluxout(nMSA)%data2d) ) fluxout(nMSA)%data2d = fout

   rc = 0

   end subroutine SU_ChemDrv_MSA




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
                               precc, precl, dqcond, tmpu, fluxout, pSO4wet_colflux, &
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
   real, pointer, dimension(:,:,:) :: tmpu    ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa    ! air density [kg m-3]

! !OUTPUT PARAMETERS:

   type(SU_GridComp), intent(inout) :: gcSU  ! SU Grid Component
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
!  Based on Ginoux
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer  ::  i, j, k, iit, n, LH, kk, ios
   integer  ::  nbeg, nend
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

   nbeg  = w_c%reg%i_SU
   nend  = w_c%reg%j_SU
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
#ifdef NEMS
      if ( gcSU%doing_scav == 1 ) then
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
!      What is the soluble amount of SO2?
       SO2Soluble = min(fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k),gcSU%h2o2_int(i,j,k)*one)/fmr
       if(SO2Soluble .lt. 0.) SO2Soluble = 0.

!      Adjust SU amounts
       DC(nDMS) = 0.
       DC(nSO2) = SO2Soluble * F * (1.-exp(-BT))
       DC(nSO4) = w_c%qa(nbeg+nSO4-1)%data3d(i,j,k) * F * (1.-exp(-BT))
       DC(nMSA) = w_c%qa(nbeg+nMSA-1)%data3d(i,j,k) * F * (1.-exp(-BT))

!      Adjust H2O2 concentration in cloudy portion of cell
       if(fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k) .gt. gcSU%h2o2_int(i,j,k)) then
        gcSU%h2o2_int(i,j,k) = max(zero,(1.-F)*gcSU%h2o2_int(i,j,k))
! GOCART removes all
!        gcSU%h2o2_int(i,j,k) = 0.
       else
        gcSU%h2o2_int(i,j,k) &
          = gcSU%h2o2_int(i,j,k) - F*fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k)
       endif

       do n = 1, nbins
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
        if (w_c%qa(nbeg+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(nbeg+n-1)%data3d(i,j,k) = 1.0E-32
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
        SO2Soluble = min(fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k),gcSU%h2o2_int(i,j,k)*one)/fmr
        if(SO2Soluble .lt. 0.) SO2Soluble = 0.

!       Adjust SU amounts
        DC(nDMS) = 0.
        DC(nSO2) = SO2Soluble * F * (1.-exp(-BT))
        DC(nSO4) = w_c%qa(nbeg+nSO4-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        DC(nMSA) = w_c%qa(nbeg+nMSA-1)%data3d(i,j,k) * F * (1.-exp(-BT))

!       Adjust H2O2 concentration in cloudy portion of cell
        if(fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k) .gt. gcSU%h2o2_int(i,j,k)) then
         gcSU%h2o2_int(i,j,k) = max(zero,(one-F)*gcSU%h2o2_int(i,j,k))
!  GOCART removes all
!         gcSU%h2o2_int(i,j,k) = 0.
        else
         gcSU%h2o2_int(i,j,k) &
           = gcSU%h2o2_int(i,j,k) - F*fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k)
        endif
 
        do n = 1, nbins
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
         if (w_c%qa(nbeg+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(nbeg+n-1)%data3d(i,j,k) = 1.0E-32
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
       SO2Soluble = min(fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k),gcSU%h2o2_int(i,j,k)*one)/fmr
       if(SO2Soluble .lt. 0.) SO2Soluble = 0.

!      Adjust SU amounts
       DC(nDMS) = 0.
       DC(nSO2) = SO2Soluble * F * (1.-exp(-BT))
       DC(nSO4) = w_c%qa(nbeg+nSO4-1)%data3d(i,j,k) * F * (1.-exp(-BT))
       DC(nMSA) = w_c%qa(nbeg+nMSA-1)%data3d(i,j,k) * F * (1.-exp(-BT))

!      Adjust H2O2 concentration in cloudy portion of cell
       if(fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k) .gt. gcSU%h2o2_int(i,j,k)) then
        gcSU%h2o2_int(i,j,k) = max(zero,(one-F)*gcSU%h2o2_int(i,j,k))
       else
        gcSU%h2o2_int(i,j,k) &
          = gcSU%h2o2_int(i,j,k) - F*fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k)
       endif

       do n = 1, nbins
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
        if (w_c%qa(nbeg+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(nbeg+n-1)%data3d(i,j,k) = 1.0E-32
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
        SO2Soluble = min(fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k),gcSU%h2o2_int(i,j,k)*one)/fmr
        if(SO2Soluble .lt. 0.) SO2Soluble = 0.

!       Adjust SU amounts
        DC(nDMS) = 0.
        DC(nSO2) = SO2Soluble * F * (1.-exp(-BT))
        DC(nSO4) = w_c%qa(nbeg+nSO4-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        DC(nMSA) = w_c%qa(nbeg+nMSA-1)%data3d(i,j,k) * F * (1.-exp(-BT))

!       Adjust H2O2 concentration in cloudy portion of cell
        if(fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k) .gt. gcSU%h2o2_int(i,j,k)) then
         gcSU%h2o2_int(i,j,k) = max(zero,(one-F)*gcSU%h2o2_int(i,j,k))
        else
         gcSU%h2o2_int(i,j,k) &
           = gcSU%h2o2_int(i,j,k) - F*fmr*w_c%qa(nbeg+nSO2-1)%data3d(i,j,k)
        endif
 
        do n = 1, nbins
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
         if (w_c%qa(nbeg+n-1)%data3d(i,j,k) .lt. 1.0E-32) w_c%qa(nbeg+n-1)%data3d(i,j,k) = 1.0E-32
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

          w_c%qa(nbeg+nMSA-1)%data3d(i,j,k) = w_c%qa(nbeg+nMSA-1)%data3d(i,j,k) + DC(nMSA)

!         SO2 gets added to SO4, but remember to remove the SO2 from FD!
          w_c%qa(nbeg+nSO4-1)%data3d(i,j,k) =  w_c%qa(nbeg+nSO4-1)%data3d(i,j,k) + DC(nSO4) &
                                    + DC(nSO2)*gcSU%fMassSO4/gcSU%fMassSO2
          if( associated(pso4wet_colflux%data2d)) &
             pso4wet_colflux%data2d(i,j) = pso4wet_colflux%data2d(i,j) &
              + DC(nSO2)*gcSU%fMassSO4/gcSU%fMassSO2 / cdt * w_c%delp(i,j,k)/grav
          if( associated(pso4wet%data3d) ) &
             pso4wet%data3d(i,j,k) = DC(nSO2)*gcSU%fMassSO4/gcSU%fMassSO2 / cdt

!         Adjust the flux out of the bottom of the layer--remove SO2 here!
          do n = 1, nbins
           w_c%qa(nbeg+n-1)%data3d(i,j,k) = &
             max(w_c%qa(nbeg+n-1)%data3d(i,j,k),tiny(1.0))
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

   subroutine SU_Compute_Diags ( i1, i2, j1, j2, km, nbins, gcSU, w_c, tmpu, &
                                 rhoa, dmssfcmass, dmscolmass, so2sfcmass, &
                                 so2colmass, so4sfcmass, so4colmass, &
                                 exttau, scatau, so4mass, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   type(SU_GridComp), intent(inout):: gcSU     ! SU Grid Component
   type(Chem_Bundle), intent(in)   :: w_c
   real, pointer, dimension(:,:,:) :: tmpu      ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa      ! air density [kg m-3]

! !OUTPUT PARAMETERS:
   type(Chem_Array), intent(inout)  :: dmssfcmass ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: dmscolmass ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: so2sfcmass ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: so2colmass ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: so4sfcmass ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: so4colmass ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: exttau ! ext. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: scatau ! sct. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: so4mass         ! 3D sulfate mass mr
   integer, intent(out)             :: rc          ! Error return code:
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
   integer :: i, j, k, n, nbeg, nend, nSO4, nSO2, nDMS, ios, nch, idx
   real :: tau, ssa
   character(len=255) :: qname


!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_SU
   nend  = w_c%reg%j_SU
   nch   = gcSU%mie_tables%nch
   nSO4  = gcSU%nSO4
   nSO2  = gcSU%nSO2
   nDMS  = gcSU%nDMS

!  Calculate the diagnostic variables if requested
!  -----------------------------------------------

!  Calculate the surface mass concentration
   if( associated(so4sfcmass%data2d) ) then
      so4sfcmass%data2d(i1:i2,j1:j2) = 0.
      so4sfcmass%data2d(i1:i2,j1:j2) &
       =   w_c%qa(nSO4+nbeg-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)
   endif
   if( associated(so2sfcmass%data2d) ) then
      so2sfcmass%data2d(i1:i2,j1:j2) = 0.
      so2sfcmass%data2d(i1:i2,j1:j2) &
       =   w_c%qa(nSO2+nbeg-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)
   endif
   if( associated(dmssfcmass%data2d) ) then
      dmssfcmass%data2d(i1:i2,j1:j2) = 0.
      dmssfcmass%data2d(i1:i2,j1:j2) &
       =   w_c%qa(nDMS+nbeg-1)%data3d(i1:i2,j1:j2,km)*rhoa(i1:i2,j1:j2,km)
   endif


!  Initialize the diagnostic variables
!  -----------------------------------

!  Calculate the column loading
   if( associated(so4colmass%data2d) ) then
      so4colmass%data2d(i1:i2,j1:j2) = 0.
      do k = 1, km
       so4colmass%data2d(i1:i2,j1:j2) &
        =   so4colmass%data2d(i1:i2,j1:j2) &
          + w_c%qa(nSO4+nbeg-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
      enddo
   endif
   if( associated(so2colmass%data2d) ) then
      so2colmass%data2d(i1:i2,j1:j2) = 0.
      do k = 1, km
       so2colmass%data2d(i1:i2,j1:j2) &
        =   so2colmass%data2d(i1:i2,j1:j2) &
          + w_c%qa(nSO2+nbeg-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
      enddo
   endif
   if( associated(dmscolmass%data2d) ) then
      dmscolmass%data2d(i1:i2,j1:j2) = 0.
      do k = 1, km
       dmscolmass%data2d(i1:i2,j1:j2) &
        =   dmscolmass%data2d(i1:i2,j1:j2) &
          + w_c%qa(nDMS+nbeg-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)/grav
      enddo
   endif


!  Mass mixing ratio of sulfate
   if( associated(so4mass%data3d) ) then
      so4mass%data3d(i1:i2,j1:j2,1:km) = 0.
      so4mass%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nSO4+nbeg-1)%data3d(i1:i2,j1:j2,1:km)
   endif

!  Calculate the extinction and/or scattering AOD
   if( associated(exttau%data2d) .or. associated(scatau%data2d) ) then

      if( associated(exttau%data2d) ) then
       exttau%data2d(i1:i2,j1:j2) = 0.
      endif
      if( associated(scatau%data2d) ) then
       scatau%data2d(i1:i2,j1:j2) = 0.
      endif

!     Note the binning is different for SO4
      do n = nSO4, nSO4

!      Select the name for species
       qname = trim(w_c%reg%vname(w_c%reg%i_SU+n-1))
       idx = Chem_MieQueryIdx(gcSU%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

!      Recall -- at present need to divide RH by 100 to get to tables
       do k = 1, km
        do j = j1, j2
         do i = i1, i2
          call Chem_MieQuery(gcSU%mie_tables, idx, 1., &
              w_c%qa(nbeg+n-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
              w_c%rh(i,j,k)/100., tau=tau, ssa=ssa)

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


   rc = 0

   end subroutine SU_Compute_Diags

 end subroutine SU_GridCompRun

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  SU_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine SU_GridCompFinalize ( gcSU, w_c, impChem, expChem, &
                                    nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(SU_GridComp), intent(inout) :: gcSU   ! Grid Component

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
   character(len=*), parameter :: myname = 'SU_GridCompFinalize'

!  If initialized volcanic emissions from daily tables, clean-up
   if(associated(gcSU%vLat))    deallocate(gcSU%vLat, stat=ios)
   if(associated(gcSU%vLon))    deallocate(gcSU%vLon, stat=ios)
   if(associated(gcSU%vSulfur)) deallocate(gcSU%vSulfur, stat=ios)
   if(associated(gcSU%vElev))   deallocate(gcSU%vElev, stat=ios)
   if(associated(gcSU%vCloud))  deallocate(gcSU%vCloud, stat=ios)
   rc=0
   return

 end subroutine SU_GridCompFinalize

 end module SU_GridCompMod

