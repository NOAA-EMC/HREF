#include "MAPL_Generic.h"

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  OC_GridCompMod --- OC Grid Component Class
!
! !INTERFACE:
!

   module  OC_GridCompMod

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
   USE m_chars, ONLY: lowercase
   use Chem_SettlingMod      ! Settling
   use DryDepositionMod
   use WetRemovalMod
   use ConvectionMod         ! Offline convective mixing/scavenging

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  OC_GridComp       ! The OC object 
   PUBLIC  OC_GridComp1      ! Single instance OC object 

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  OC_GridCompInitialize
   PUBLIC  OC_GridCompRun
   PUBLIC  OC_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the (pre-ESMF) OC Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  30Sep2014 Lu        Remove doing_scav option
!
!EOP
!-------------------------------------------------------------------------

  type OC_GridComp1
        character(len=255) :: name
        character(len=255) :: iname           ! instance name
        character(len=255) :: rcfilen         ! resource file name
        character(len=255) :: maskFileName
        character(len=255) :: regionsString   ! Comma-delimited string of regions

        integer :: instance                   ! instance number

        type(Chem_Mie), pointer :: mie_tables     ! aod LUTs
        real, pointer :: biofuel_src(:,:)
        real, pointer :: biomass_src(:,:)
        real, pointer :: biomass_src_(:,:)
        real, pointer :: eocant1_src(:,:)  ! level 1
        real, pointer :: eocant2_src(:,:)  ! level 2
        real, pointer :: terpene_src(:,:)  ! level 2
        real, pointer :: oc_ship_src(:,:)
        REAL, POINTER ::  regionMask(:,:)    ! regional mask
        real :: ratPOM               ! Ratio of POM to OC mass
        real :: fHydrophobic         ! Fraction of emissions hydrophobic
        real :: eBiofuel             ! Emission factor of Biofuel to OC aerosol
        real :: eBiomassBurning      ! Emission factor of Biomass Burning to OC
        real :: fTerpene             ! Fraction of terpene emissions -> aerosol
        integer :: nymd   ! date of last emissions/prodction
        character(len=255) :: bb_srcfilen
        character(len=255) :: bf_srcfilen
        character(len=255) :: eocant1_srcfilen
        character(len=255) :: eocant2_srcfilen
        character(len=255) :: terpene_srcfilen
        character(len=255) :: oc_ship_srcfilen
        integer :: nymd_bb      = 0
        integer :: nymd_bf      = 0
        integer :: nymd_eocant1 = 0
        integer :: nymd_eocant2 = 0
        integer :: nymd_terpene = 0
        integer :: nymd_oc_ship = 0
  end type OC_GridComp1

  type OC_GridComp
     integer                     ::  n          ! number of instances 
     type(Chem_Mie), pointer     :: mie_tables  ! aod LUTs
     type(OC_GridComp1), pointer ::  gcs(:)     ! instances
  end type OC_GridComp

  real, parameter :: OCEAN=0.0, LAND = 1.0, SEA_ICE = 2.0
  real, parameter :: radToDeg = 57.2957795

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  OC_GridCompInitialize --- Initialize OC_GridComp
!
! !INTERFACE:
!

   subroutine OC_GridCompInitialize ( gcOC, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real, intent(in) :: cdt                     ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(OC_GridComp), intent(inout) :: gcOC   ! Grid Component
   type(ESMF_State), intent(inout)  :: impChem  ! Import State
   type(ESMF_State), intent(inout)  :: expChem  ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the OC Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'OC_GridCompInitialize'
   character(len=255) :: rcbasen = 'OC_GridComp'
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
   CALL I90_label ( 'OC_instances:', ier )
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
   
!  We have 2 tracers for each instance of OC
!  We cannot have fewer instances than half the number of
!   OC bins in the registry (it is OK to have less, though)
!  --------------------------------------------------------
   if ( n .LT. w_c%reg%n_OC/2 ) then
        rc = 35
        return
   else if ( n .GT. w_c%reg%n_OC/2 ) then
        if (MAPL_AM_I_ROOT()) &
        print *, trim(myname)// &
                 ': fewer OC bin sets than possible OC instances'//&
                 ' (2 bins per instance): ',&
                 n, w_c%reg%n_OC
   end if
   n = min(n,w_c%reg%n_OC/2 )
   gcOC%n = n

!  Next allocate necessary memory
!  ------------------------------
   allocate ( gcOC%gcs(n), stat=ier )    
   if ( ier .NE. 0 ) then
      rc = 40
      return
   end if

!  Record name of each instance
!  ----------------------------
   CALL I90_label ( 'OC_instances:', ier )
   do i = 1, n
      CALL I90_gtoken( name, ier )
      if ( ier .NE. 0 ) then
         rc = 40
         return
      end if
                                            ! resource file name
      gcOC%gcs(i)%rcfilen = trim(rcbasen)//'---'//trim(name)//'.rc'
      gcOC%gcs(i)%instance = i              ! instance number 
      IF(TRIM(name) == "full" ) THEN
       gcOC%gcs(i)%iname = " "              ! blank instance name for full (1)
      ELSE
       gcOC%gcs(i)%iname = TRIM(name)       ! instance name for others
      END IF
   end do    

!  Next initialize each instance
!  -----------------------------
   do i = 1, gcOC%n
      IF(MAPL_AM_I_ROOT()) THEN
       PRINT *," "
       PRINT *,myname,": Initializing instance ",TRIM(gcOC%gcs(i)%iname)," [",gcOC%gcs(i)%instance,"]"
      END IF
      call OC_SingleInstance_ ( OC_GridCompInitialize1_, i, &
                                gcOC%gcs(i), w_c, impChem, expChem,  &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then
         rc = 1000+ier
         return
      end if
      gcOC%gcs(i)%mie_tables => gcOC%mie_tables
   end do

!  All done
!  --------
   CALL I90_FullRelease( ier )
   IF( ier /= 0 ) THEN
    PRINT *,myname,": I90_FullRelease not successful."
    rc = 40
   END IF


 end subroutine OC_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  OC_GridCompRun --- Run OC_GridComp
!
! !INTERFACE:
!

   subroutine OC_GridCompRun ( gcOC, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms           ! time
   REAL,    INTENT(IN) :: cdt                  ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(OC_GridComp), INTENT(INOUT) :: gcOC     ! Grid Component
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

   do i = 1, gcOC%n
      call OC_SingleInstance_ ( OC_GridCompRun1_, i, &
                                gcOC%gcs(i), w_c, impChem, expChem, &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then
         rc = i * 1000+ier
         return
      end if
   end do

 end subroutine OC_GridCompRun


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  OC_GridCompFinalize --- Initialize OC_GridComp
!
! !INTERFACE:
!

   subroutine OC_GridCompFinalize ( gcOC, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms           ! time
   REAL,    INTENT(IN) :: cdt                  ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(OC_GridComp), INTENT(INOUT) :: gcOC     ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Finalizes the OC Grid Component. Multiple instance
!               version.
!
! !REVISION HISTORY:
!
!  27Feb2008  da Silva  Introduced multiple instances
!
!EOP
!-------------------------------------------------------------------------

   integer i, ier

   do i = 1, gcOC%n
      call OC_SingleInstance_ ( OC_GridCompFinalize1_, i, &
                                gcOC%gcs(i), w_c, impChem, expChem, &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then
         rc = i * 1000+ier
         return
      end if
   end do

   deallocate ( gcOC%gcs, stat=ier )    
   gcOC%n = -1

 end subroutine OC_GridCompFinalize


!--------------------------------------------------------------------------

!                      Single Instance Methods

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  OC_GridCompInitialize --- Initialize OC_GridComp
!
! !INTERFACE:
!

   subroutine OC_GridCompInitialize1_ ( gcOC, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c     ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real, intent(in) :: cdt                     ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(OC_GridComp1), intent(inout) :: gcOC    ! Grid Component
   type(ESMF_State), intent(inout)  :: impChem  ! Import State
   type(ESMF_State), intent(inout)  :: expChem  ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the OC Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'OC_GridCompInitialize1'


   character(len=255) :: rcfilen
   integer :: ios, n
   integer :: i1, i2, im, j1, j2, jm, nbins, n1, n2, nbins_rc
   integer :: nymd1, nhms1, j
   integer :: nTimes, begTime, incSecs
   integer, allocatable :: ier(:)
   real, allocatable :: buffer(:,:)
   real :: qmax, qmin
   LOGICAL :: NoRegionalConstraint 

   REAL :: limitN, limitS, radtoDeg
   REAL, ALLOCATABLE :: var2d(:,:)


   rcfilen = gcOC%rcfilen
   gcOC%name = 'OC Constituent Package'
   radTODeg = 57.2957795

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   nbins = w_c%reg%n_OC
   n1  = w_c%reg%i_OC
   n2  = w_c%reg%j_OC

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

   call i90_label ( 'number_oc_classes:', ier(1) )
   nbins_rc = i90_gint ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(20)
      return
   end if
   if ( nbins_rc /= nbins ) then
      call final_(25)
      return
   end if


!  OC sources files
!  ---------------------
   call i90_label ( 'bb_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcOC%bb_srcfilen, ier(1) )
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
      call i90_gtoken ( gcOC%bf_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'eocant1_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcOC%eocant1_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'eocant2_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcOC%eocant2_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'terpene_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcOC%terpene_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

   call i90_label ( 'oc_ship_srcfilen:', ier(1) )
   if ( ier(1) /= 0 ) then
      call final_(30)
      return
   else
      call i90_gtoken ( gcOC%oc_ship_srcfilen, ier(1) )
      if ( ier(1) /= 0 ) then
         call final_(40)
         return
      end if
   end if

!  Ratio of POM to OC mass
!  -----------------------
   call i90_label ( 'pom_oc_ratio:', ier(1) )
   gcOC%ratPOM = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if


!  Hydrophilic fraction
!  ---------------
   call i90_label ( 'hydrophobic_fraction:', ier(1) )
   gcOC%fHydrophobic = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if


!  Biofuel Emission Factor
!  ---------------
   call i90_label ( 'biofuel_emission_factor:', ier(1) )
   gcOC%eBiofuel = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if


!  Biomass Burning Emission Factor
!  ---------------
   call i90_label ( 'biomass_burning_emission_factor:', ier(1) )
   gcOC%eBiomassBurning = i90_gfloat ( ier(2) )
   if ( any(ier(1:2) /= 0) ) then
      call final_(50)
      return
   end if


!  Terpene Emission Factor
!  ---------------
   call i90_label ( 'terpene_emission_fraction:', ier(1) )
   gcOC%fTerpene = i90_gfloat ( ier(2) )
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
   if( index(gcOC%bb_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcOC%bb_srcfilen, gcOC%nymd_bb, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcOC%bf_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcOC%bf_srcfilen, gcOC%nymd_bf, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcOC%eocant1_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcOC%eocant1_srcfilen, gcOC%nymd_eocant1, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcOC%eocant2_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcOC%eocant2_srcfilen, gcOC%nymd_eocant2, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcOC%oc_ship_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcOC%oc_ship_srcfilen, gcOC%nymd_oc_ship, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcOC%terpene_srcfilen,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcOC%terpene_srcfilen, gcOC%nymd_terpene, &
                               begTime, nTimes, incSecs )
   endif
   ier(1) = gcOC%nymd_bb
   ier(2) = gcOC%nymd_bf
   ier(3) = gcOC%nymd_eocant1
   ier(4) = gcOC%nymd_eocant2
   ier(5) = gcOC%nymd_terpene
   ier(6) = gcOC%nymd_oc_ship
   if( any(ier(1:6) < 0 ) ) then
     call final_(60)
     return
   endif


!  Initialize date for BCs
!  -----------------------
   gcOC%nymd = -1   ! nothing read yet


!  Obtain geographical region mask
!  -------------------------------
   CALL I90_label ( 'OC_regions:', ier(1) )
   CALL I90_gtoken( gcOC%maskFileName, ier(2) )
   if( any(ier(1:2) < 0 ) ) then
     call final_(41)
     return
   endif
   ier(:)=0

   call Chem_UtilGetTimeInfo( gcOC%maskFileName, nymd1, &
                              begTime, nTimes, incSecs )
   if(nymd1 < 0) call final_(15)
   nhms1 = 120000
   CALL Chem_UtilMPread ( gcOC%maskFileName, 'REGION_MASK', nymd1, nhms1, &
                          i1, i2, 0, im, j1, j2, 0, jm, 0, &
                          var2d=gcOC%regionMask, grid=w_c%grid_esmf, &
                          voting=.true. )

!  Grab the region string.
!  -----------------------
   call i90_label ( 'OC_regions_indices:', ier(1) )
   CALL I90_gtoken( gcOC%regionsString, ier(2) )
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
     gcOC%regionsString = "1"
     ALLOCATE(var2d(i1:i2,j1:j2), STAT=ios)
     IF(ios /= 0) THEN
      PRINT *,myname,": Unable to allocate var2d."
      CALL final_(62)
     END IF
     var2d(i1:i2,j1:j2) = 0.00

!  Within the latitude range specified, set land boxes to 1
!  --------------------------------------------------------
      WHERE ( (gcOC%regionMask > 0) .AND. &
            ((limitS <= w_c%grid%lat*radTODeg) .AND. (w_c%grid%lat*radTODeg <= limitN) ) )   
             var2d = 1.00
      END WHERE

!  Reset the region mask in gcOC
!  -----------------------------
     gcOC%regionMask(i1:i2,j1:j2) = var2d(i1:i2,j1:j2)

     DEALLOCATE(var2d, STAT=ios)
     IF(ios /= 0) THEN
      PRINT *,myname,": Unable to deallocate var2d."
      CALL final_(63)
     END IF

    END IF SpecCheck

   END IF zoneMasking

!  Is this instantiation a global case?
!  -----------------------------------
   IF(gcOC%regionsString(1:2) == "-1") THEN
    NoRegionalConstraint = .TRUE.
   ELSE
    SELECT CASE (lowercase(gcOC%regionsString(1:2)))
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
   IF(NoRegionalConstraint) gcOC%regionsString = "-1"

   IF(MAPL_AM_I_ROOT()) THEN
    IF(NoRegionalConstraint) THEN
     PRINT *,myname,": This instantiation has no regional constraints."
    ELSE
     PRINT *,myname,": This instantiation is regionally constrained."
     PRINT *,myname,": List of region numbers included: ",TRIM(gcOC%regionsString)
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
   allocate ( gcOC%biomass_src(i1:i2,j1:j2), gcOC%biofuel_src(i1:i2,j1:j2), &
              gcOC%biomass_src_(i1:i2,j1:j2), &
              gcOC%eocant1_src(i1:i2,j1:j2), gcOC%eocant2_src(i1:i2,j1:j2), &
              gcOC%terpene_src(i1:i2,j1:j2), gcOC%oc_ship_src(i1:i2,j1:j2), &
              gcOC%regionmask(i1:i2,j1:j2), ier(nerr), stat=ios )
   if ( ios /= 0 ) rc = 100
   end subroutine init_

   subroutine final_(ierr)
   integer :: ierr
   integer ios
   deallocate ( gcOC%biomass_src, gcOC%biofuel_src, &
                gcOC%biomass_src_, &
                gcOC%eocant1_src, gcOC%eocant2_src, &
                gcOC%terpene_src, gcOC%oc_ship_src, &
                gcOC%regionmask, ier, stat=ios )
   call i90_release()
   rc = ierr
   end subroutine final_

   end subroutine OC_GridCompInitialize1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  OC_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine OC_GridCompRun1_ ( gcOC, w_c, impChem, expChem, &
                                 nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(OC_GridComp1), intent(inout) :: gcOC   ! Grid Component
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
 
! !DESCRIPTION: This routine implements the so-called OC Driver. That 
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

   character(len=*), parameter :: myname = 'OC_GridCompRun'
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

   real, pointer    :: OC_radius(:), OC_rhop(:)
   integer          :: rhFlag

#define EXPORT     expChem
#define iNAME    TRIM(gcOC%iname)

#define ptrOCWT       OC_wet
#define ptrOCSV       OC_conv
#define ptrOCEM       OC_emis
#define ptrOCDP       OC_dep
#define ptrOCSD       OC_set

#define ptrOCMASS     OC_mass
#define ptrOCEMAN     OC_emisAN
#define ptrOCEMBB     OC_emisBB
#define ptrOCEMBF     OC_emisBF
#define ptrOCEMBG     OC_emisBG
#define ptrOCHYPHIL   OC_toHydrophilic
#define ptrOCSMASS    OC_sfcmass
#define ptrOCCMASS    OC_colmass
#define ptrOCEXTTAU   OC_exttau
#define ptrOCSCATAU   OC_scatau
#define ptrOCCONC     OC_conc
#define ptrOCEXTCOEF  OC_extcoef
#define ptrOCSCACOEF  OC_scacoef
#define ptrOCANGSTR   OC_angstrom
#define ptrOCFLUXU    OC_fluxu
#define ptrOCFLUXV    OC_fluxv

   integer :: STATUS

#include "OC_GetPointer___.h"


!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm

   km = w_c%grid%km
   nbins = w_c%reg%n_OC
   n1  = w_c%reg%i_OC
   n2  = w_c%reg%j_OC

   ijl  = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )
   ijkl = ijl * km

! Update emissions/production if necessary (daily)
!  ------------------------------------------
   if(gcOC%nymd .ne. nymd) then

!   Biomass Burning -- select on known inventories
!   ----------------------------------------------

!   Daily files (e.g., MODIS) or GFED v.2 (1997 - 2005 valid)
    if (  index(gcOC%bb_srcfilen,'%') .gt. 0 .or. &
          index(gcOC%bb_srcfilen,'gfed') .gt. 0 ) then  
       nymd1 = nymd
       nhms1 = 120000

!   Assume GFED climatology or Martin (Duncan) climatology
    else                                            
       nymd1 = (gcOC%nymd_bb/10000)*10000 + mod ( nymd, 10000 )
       nhms1 = 120000
    end if

    call Chem_UtilMPread ( gcOC%bb_srcfilen, 'biomass', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcOC%biomass_src, cyclic=.true., &
                           grid = w_c%grid_esmf, maskString=TRIM(gcOC%regionsString), &
                           gridMask=gcOC%regionMask  )



!   Terpene, biofuel and anthropogenic emissions (inventories)
!   ----------------------------------------------------------
!   This one is from a climatology in all instances
    nymd1 = (gcOC%nymd_terpene/10000)*10000 + mod ( nymd, 10000 )
    nhms1 = 120000
    call Chem_UtilMPread ( gcOC%terpene_srcfilen, 'terpene', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcOC%terpene_src, cyclic=.true., &
                           grid = w_c%grid_esmf, maskString=TRIM(gcOC%regionsString), &
                           gridMask=gcOC%regionMask  )

!    nymd1 = (gcOC%nymd_bf/10000)*10000 + mod ( nymd, 10000 )
    nymd1 = nymd
    nhms1 = 120000
    call Chem_UtilMPread ( gcOC%bf_srcfilen, 'biofuel', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcOC%biofuel_src, cyclic=.true., &
                           grid = w_c%grid_esmf, maskString=TRIM(gcOC%regionsString), &
                           gridMask=gcOC%regionMask  )


!    nymd1 = gcOC%nymd_eocant1
    nymd1 = nymd
    nhms1 = 120000
    call Chem_UtilMPread ( gcOC%eocant1_srcfilen, 'anteoc1', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcOC%eocant1_src, cyclic=.true., &
                           grid = w_c%grid_esmf, maskString=TRIM(gcOC%regionsString), &
                           gridMask=gcOC%regionMask  )


!    nymd1 = gcOC%nymd_eocant1
    nymd1 = nymd
    nhms1 = 120000
    call Chem_UtilMPread ( gcOC%eocant2_srcfilen, 'anteoc2', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcOC%eocant2_src, & 
                           grid = w_c%grid_esmf, maskString=TRIM(gcOC%regionsString), &
                           gridMask=gcOC%regionMask  )

!   Ship based OC emissions
!    nymd1 = gcOC%nymd_oc_ship
    nymd1 = nymd
    nhms1 = 120000
    call Chem_UtilMPread ( gcOC%oc_ship_srcfilen, 'oc_ship', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcOC%oc_ship_src, cyclic=.true., &
                           grid = w_c%grid_esmf, maskString=TRIM(gcOC%regionsString), &
                           gridMask=gcOC%regionMask  )

!   As a safety check, where value is undefined set to 0
    do j = j1, j2
     do i = i1, i2
      if(1.01*gcOC%biomass_src(i,j) .gt. undefval) gcOC%biomass_src(i,j) = 0.
      if(1.01*gcOC%terpene_src(i,j) .gt. undefval) gcOC%terpene_src(i,j) = 0.
      if(1.01*gcOC%biofuel_src(i,j) .gt. undefval) gcOC%biofuel_src(i,j) = 0.
      if(1.01*gcOC%eocant1_src(i,j) .gt. undefval) gcOC%eocant1_src(i,j) = 0.
      if(1.01*gcOC%eocant2_src(i,j) .gt. undefval) gcOC%eocant2_src(i,j) = 0.
      if(1.01*gcOC%oc_ship_src(i,j) .gt. undefval) gcOC%oc_ship_src(i,j) = 0.
     enddo
    enddo


#ifdef DEBUG
    call pmaxmin('OC: biomass', gcOC%biomass_src, qmin, qmax, ijl,1, 1. )
    call pmaxmin('OC: biofuel', gcOC%biofuel_src, qmin, qmax, ijl,1, 1. )
    call pmaxmin('OC: eocant1', gcOC%eocant1_src, qmin, qmax, ijl,1,1.)
    call pmaxmin('OC: eocant2', gcOC%eocant2_src, qmin, qmax, ijl,1,1.)
    call pmaxmin('OC: terpene', gcOC%terpene_src, qmin, qmax, ijl,1, 1.)
    call pmaxmin('OC: oc_ship', gcOC%oc_ship_src, qmin, qmax, ijl,1, 1.)
#endif

!   Save this in case we need to apply diurnal cycle
!   ------------------------------------------------
   if ( w_c%diurnal_bb ) then
        gcOC%biomass_src_(:,:) = gcOC%biomass_src(:,:)
   end if

    gcOC%nymd = nymd

   endif

!  Apply diurnal cycle if so desired
!  ---------------------------------
   if ( w_c%diurnal_bb ) then
      call Chem_BiomassDiurnal ( gcOC%biomass_src, gcOC%biomass_src_,   &
                                 w_c%grid%lon(:,:)*radToDeg, &
                                 w_c%grid%lat(:,:)*radToDeg, nhms, cdt )      
   end if

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('OC: q_beg', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
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

!  OC Source
!  -----------
   call OC_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcOC, w_c, &
                      pblh, tmpu, rhoa, OC_emis, &
                      OC_emisAN, OC_emisBB, OC_emisBF, OC_emisBG, rc )
#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('OC: q_emi', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Ad Hoc transfer of hydrophobic to hydrophilic aerosols
!  Following Chin's parameterization, the rate constant is
!  k = 4.63e-6 s-1 (.4 day-1; e-folding time = 2.5 days)
   if(associated(OC_toHydrophilic%data2d)) &
     OC_toHydrophilic%data2d(i1:i2,j1:j2) = 0.0

   do k = 1, km
    do j = j1, j2
     do i = i1, i2
      qUpdate = w_c%qa(n1)%data3d(i,j,k)*exp(-4.63e-6*cdt)
      qUpdate = max(qUpdate,1.e-32)
      delq = max(0.,w_c%qa(n1)%data3d(i,j,k)-qUpdate)
      w_c%qa(n1)%data3d(i,j,k) = qUpdate
      w_c%qa(n2)%data3d(i,j,k) = w_c%qa(n2)%data3d(i,j,k)+delq
      if(associated(OC_toHydrophilic%data2d)) &
       OC_toHydrophilic%data2d(i,j) = OC_toHydrophilic%data2d(i,j) &
        + delq*w_c%delp(i,j,k)/grav/cdt
     end do
    end do
   end do

!  OC Settling
!  -----------
   allocate( OC_radius(nbins), OC_rhop(nbins) )
   OC_radius(:) = 0.35e-6  ! radius for settling [m]
   OC_rhop(:)   = 1800.    ! density for setting [kg m-3]
   rhFlag       = 0        ! settle like dry particles
   call Chem_Settling ( i1, i2, j1, j2, km, n1, n2, nbins, rhFlag, &
                        OC_radius, OC_rhop, cdt, w_c, tmpu, rhoa, hsurf,    &
                        hghte, OC_set, rc )
   deallocate( OC_radius, OC_rhop)

!  OC Deposition
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
    if( associated(OC_dep(n)%data2d) ) &
     OC_dep(n)%data2d = dqa*w_c%delp(:,:,km)/grav/cdt
   end do

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('OC: q_dry', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif


!  Organic Carbon Large-scale Wet Removal
!  --------------------------------------
!  Hydrophobic mode (first tracer) is not removed
   if(associated(OC_wet(1)%data2d)) OC_wet(1)%data2d = 0.
!  Hydrophilic mode (second tracer) is removed
   do n = nbins, nbins
    w_c%qa(n1+n-1)%fwet = 1.
    call WetRemovalGOCART(i1, i2, j1, j2, km, n1+n-1, n1+n-1, cdt, &
                          w_c%qa, ple, tmpu, rhoa, dqcond, precc, precl, &
                          fluxout, rc )
    if(associated(OC_wet(n)%data2d)) OC_wet(n)%data2d = fluxout%data2d
   end do

#ifdef DEBUG
   do n = n1, n2
      call pmaxmin('OC: q_wet', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                    ijl, km, 1. )
   end do
#endif

!  Organic Carbon Convective-scale Mixing and Wet Removal
!  ------------------------------------------------------
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
   if(associated(OC_conv(1)%data2d)) OC_conv(1)%data2d = 0.0
   if(associated(OC_conv(2)%data2d)) OC_conv(2)%data2d = -bcnv_(:,:,n2)/area_/icdt

   deallocate(cmfmc_, qccu_, dtrain_, tc_, airmass_, &
              delz_, vud_, delp_, airmol_, bcnv_, &
              area_, frlake_, frocean_, frseaice_, __STAT__ )



!  Compute the desired output diagnostics here
!  Ideally this will go where chemout is called in fvgcm.F since that
!  will reflect the distributions after transport, etc.
!  -----------
   call OC_Compute_Diags(i1, i2, j1, j2, km, nbins, gcOC, w_c, tmpu, rhoa, u, v, &
                         OC_sfcmass, OC_colmass, OC_mass, OC_exttau, &
                         OC_scatau, OC_conc, OC_extcoef, OC_scacoef, OC_angstrom, &
                         OC_fluxu, OC_fluxv, rc)

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
! !IROUTINE:  OC_Emission - Adds Organic Carbon emission for one timestep
!             We have emissions from 5 sources, which are distributed
!             differently in the vertical
!             1) biomass burning - uniformly mixed in PBL
!             2) biofuel sources - emitted into lowest 100 m
!             3) anthropogenic l1 - emitted into lowest 100 m
!             4) anthropogenic l2 - emitted into 100 - 500 m levels
!             5) terpene - emitted to surface (hydrophilic only)
!
! !INTERFACE:
!

   subroutine OC_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcOC, w_c, &
                            pblh, tmpu, rhoa, OC_emis, &
                            OC_emisAN, OC_emisBB, OC_emisBF, OC_emisBG, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   type(OC_GridComp1), intent(in)    :: gcOC       ! OC Grid Component
   real, pointer, dimension(:,:)    :: pblh
   real, pointer, dimension(:,:,:)  :: tmpu
   real, pointer, dimension(:,:,:)  :: rhoa

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: OC_emis(nbins) ! OC emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: OC_emisAN      ! OC emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: OC_emisBB      ! OC emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: OC_emisBF      ! OC emissions, kg/m2/s
   type(Chem_Array), intent(inout)  :: OC_emisBG      ! OC emissions, kg/m2/s
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'OC_Emission'

! !DESCRIPTION: Updates the OC concentration with emissions every timestep
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
   real, dimension(i1:i2,j1:j2) :: p100, p500, pPBL  
   real, dimension(i1:i2,j1:j2) :: p0, z0, ps
   real :: p1, z1, dz, delz, delp, f100, f500, fPBL, fBot
   real :: qmax, qmin, eBiofuel, eBiomass, eTerpene, eAnthro

   real, dimension(i1:i2,j1:j2) :: factor, srcHydrophobic, srcHydrophilic
   real, dimension(i1:i2,j1:j2) :: srcBiofuel, srcBiomass, srcAnthro, srcBiogenic
   real                         :: srcTmp, zpbl, maxAll

!  Initialize local variables
!  --------------------------
   n1  = w_c%reg%i_OC
   n2  = w_c%reg%j_OC
   ijl = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )

!  Emission factors scaling from source files to desired mass quantity
   eBiomass = gcOC%ratPOM * gcOC%eBiomassBurning
   eBiofuel = gcOC%ratPOM * gcOC%eBiofuel
   eTerpene = gcOC%ratPOM * gcOC%fTerpene
   eAnthro  = gcOC%ratPOM

!  Zero diagnostic accumulators
   do n = 1, nbins
     if( associated(OC_emis(n)%data2d) ) OC_emis(n)%data2d = 0.0
   end do
     if(associated(OC_emisAN%data2d) )   OC_emisAN%data2d  = 0.0
     if(associated(OC_emisBF%data2d) )   OC_emisBF%data2d  = 0.0
     if(associated(OC_emisBB%data2d) )   OC_emisBB%data2d  = 0.0
     if(associated(OC_emisBG%data2d) )   OC_emisBG%data2d  = 0.0

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
       pPBL(i,j) = p1+delp
      endif
      p0(i,j) = p1
      z0(i,j) = z1
     end do
    end do
   end do

#if 0
   call pmaxmin ( 'OC: p100   ', p100,  qmin, qmax, ijl, 1, 1. )
   call pmaxmin ( 'OC: p500   ', p500,  qmin, qmax, ijl, 1, 1. )
   call pmaxmin ( 'OC: pPBL   ', pPBLh, qmin, qmax, ijl, 1, 1. )
#endif

!  Now update the tracer mixing ratios with the aerosol sources
!  ------------------------------------------------------------
   p0 = ps
K_LOOP: do k = km, 1, -1

!!!    print *, 'OC_Emissions: getting emissions for layer ', k

!   First determine emissions for this layer
!   ----------------------------------------
    maxAll = 0.0
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
      fPBL = 0.
      if(p1 .ge. pPBL(i,j)) fPBL = w_c%delp(i,j,k)/(ps(i,j)-pPBL(i,j))
      if(p1 .lt. pPBL(i,j) .and. p0(i,j) .ge. pPBL(i,j)) &
       fPBL = (p0(i,j)-pPBL(i,j))/(ps(i,j)-pPBL(i,j))

!     Terpene is tree-top emission; only add in bottom layer
!     ------------------------------------------------------
      if ( k .eq. km ) then
           fBot = 1.0
      else
           fBot = 0.0
      end if

!     Sources by class in kg m-2 s-1
!     ------------------------------
      srcBiofuel(i,j)  = f100 * eBiofuel * gcOC%biofuel_src(i,j)
      srcAnthro(i,j)   = f100 * eAnthro  * gcOC%eocant1_src(i,j) &
                       + f500 * eAnthro  * gcOC%eocant2_src(i,j) &
                       + f100 * eAnthro  * gcOC%oc_ship_src(i,j)
      srcBiomass(i,j)  = fPBL * eBiomass * gcOC%biomass_src(i,j)
      srcBiogenic(i,j) = fBot * eTerpene * gcOC%terpene_src(i,j) 

      srcTmp = srcBiofuel(i,j) + srcAnthro(i,j) &
             + srcBiomass(i,j)

      srcHydrophobic(i,j) =     gcOC%fHydrophobic  * srcTmp
      srcHydrophilic(i,j) = (1.-gcOC%fHydrophobic) * srcTmp + srcBiogenic(i,j)

!     Update pressure of lower level
!     ------------------------------
      p0(i,j) = p1

     end do ! i
    end do  ! j

!   Determine global max/min
!   ------------------------
    call pmaxmin ( 'OC: Phobic ', srcHydrophobic, qmin, qmax, ijl, 1, 0. )
    maxAll = abs(qmax) + abs(qmin)
    call pmaxmin ( 'OC: Philic ', srcHydrophilic, qmin, qmax, ijl, 1, 0. )
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
    if ( associated(OC_emis(1)%data2d)) &
                    OC_emis(1)%data2d = OC_emis(1)%data2d + srcHydrophobic

    if ( associated(OC_emis(2)%data2d)) &
                    OC_emis(2)%data2d = OC_emis(2)%data2d + srcHydrophilic

    if ( associated(OC_emisBF%data2d)) &
                    OC_emisBF%data2d  = OC_emisBF%data2d  + srcBiofuel

    if ( associated(OC_emisBB%data2d)) &
                    OC_emisBB%data2d  = OC_emisBB%data2d  + srcBiomass

    if ( associated(OC_emisAN%data2d)) &
                    OC_emisAN%data2d  = OC_emisAN%data2d  + srcAnthro

   if ( associated(OC_emisBG%data2d)) &
                   OC_emisBG%data2d   = OC_emisBG%data2d  + srcBiogenic 

   end do K_LOOP

   rc = 0

   end subroutine OC_Emission

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  OC_Compute_Diags - Calculate dust 2D diagnostics
!
! !INTERFACE:
!

   subroutine OC_Compute_Diags ( i1, i2, j1, j2, km, nbins, gcOC, w_c, tmpu, rhoa, u, v, &
                                 sfcmass, colmass, mass, exttau, scatau, &
                                 conc, extcoef, scacoef, angstrom, fluxu, fluxv, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   type(OC_GridComp1), intent(inout):: gcOC     ! OC Grid Component
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

! !DESCRIPTION: Calculates some simple 2d diagnostics from the OC fields
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
   character(len=*), parameter :: myname = 'OC_Compute_Diags'
   integer :: i, j, k, n, n1, n2, ios, nch, idx
   real :: tau, ssa
   character(len=255) :: qname
   real, dimension(i1:i2,j1:j2) :: tau470, tau870
   real    :: ilam550, ilam470, ilam870
   logical :: do_angstrom


!  Initialize local variables
!  --------------------------
   n1  = w_c%reg%i_OC
   n2  = w_c%reg%j_OC
   nch   = gcOC%mie_tables%nch

!  Get the wavelength indices
!  --------------------------
!  Must provide ilam550 for AOT calculation
   ilam550 = 1.
   ilam470 = 0.
   ilam870 = 0.
   if(nch .gt. 1) then
    do i = 1, nch
     if ( gcOC%mie_tables%channels(i) .ge. 5.49e-7 .and. &
          gcOC%mie_tables%channels(i) .le. 5.51e-7) ilam550 = i
     if ( gcOC%mie_tables%channels(i) .ge. 4.69e-7 .and. &
          gcOC%mie_tables%channels(i) .le. 4.71e-7) ilam470 = i
     if ( gcOC%mie_tables%channels(i) .ge. 8.69e-7 .and. &
          gcOC%mie_tables%channels(i) .le. 8.71e-7) ilam870 = i
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

!      Select the name for species and the index
       qname = trim(w_c%reg%vname(n1+n-1))
       idx = Chem_MieQueryIdx(gcOC%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

       do k = 1, km
        do j = j1, j2
         do i = i1, i2
          call Chem_MieQuery(gcOC%mie_tables, idx, ilam550, &
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
       idx = Chem_MieQueryIdx(gcOC%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

       do k = 1, km
        do j = j1, j2
         do i = i1, i2

          call Chem_MieQuery(gcOC%mie_tables, idx, ilam470, &
              w_c%qa(n+n1-1)%data3d(i,j,k)*w_c%delp(i,j,k)/grav, &
              w_c%rh(i,j,k), tau=tau)
          tau470(i,j) = tau470(i,j) + tau

          call Chem_MieQuery(gcOC%mie_tables, idx, ilam870, &
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

   end subroutine OC_Compute_Diags

 end subroutine OC_GridCompRun1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  OC_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine OC_GridCompFinalize1_ ( gcOC, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(OC_GridComp1), intent(inout) :: gcOC   ! Grid Component

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

   character(len=*), parameter :: myname = 'OC_GridCompFinalize'
   rc=0
   return

 end subroutine OC_GridCompFinalize1_

 end module OC_GridCompMod


!-----------------------------------------------------------------------

!                     Single Instance Wrapper

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  OC_SingleInstance_ --- Runs single instance of method
!
! !INTERFACE:
!
  subroutine OC_SingleInstance_ ( Method_, instance, &
                                  gcOC, w_c, impChem, expChem, &
                                  nymd, nhms, cdt, rc )

! !USES:

  Use OC_GridCompMod
  Use ESMF
  Use MAPL_Mod
  Use Chem_Mod 

  IMPLICIT NONE

! !INPUT PARAMETERS:

!  Input "function pointer"
!  -----------------------
   interface 
     subroutine Method_ (gc, w, imp, exp, ymd, hms, dt, rcode )
       Use OC_GridCompMod
       Use ESMF
       Use MAPL_Mod
       Use Chem_Mod 
       type(OC_GridComp1),  intent(inout)  :: gc
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

   TYPE(OC_GridComp1), INTENT(INOUT) :: gcOC    ! Grid Component
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

  integer n_OC, i_OC, j_OC
  character(len=255) :: i_qname, j_qname

! Save overall CO indices
! -----------------------
  n_OC = w_c%reg%n_OC
  i_OC = w_c%reg%i_OC
  j_OC = w_c%reg%j_OC

! Save the name of the variables in this instance
! -----------------------------------------------
  i_qname = trim(w_c%reg%vname(i_OC + 2*(instance - 1)))
  j_qname = trim(w_c%reg%vname(i_OC + 2*(instance - 1) + 1))
  
! Customize indices for this particular instance
! ----------------------------------------------
  w_c%reg%n_OC = 2
  w_c%reg%i_OC = i_OC + 2*(instance - 1)
  w_c%reg%j_OC = i_OC + 2*(instance - 1) + 1
  w_c%reg%vname(i_OC + 2*(instance - 1))     = w_c%reg%vname(i_OC)
  w_c%reg%vname(i_OC + 2*(instance - 1) + 1) = w_c%reg%vname(i_OC + 1)
  
! Execute the instance method
! ---------------------------
  call Method_ ( gcOC, w_c, impChem, expChem, &
                 nymd, nhms, cdt, rc )

! Restore the overall OC indices
! ------------------------------
  w_c%reg%vname(i_OC + 2*(instance - 1))     = i_qname
  w_c%reg%vname(i_OC + 2*(instance - 1) + 1) = j_qname
  w_c%reg%n_OC = n_OC
  w_c%reg%i_OC = i_OC
  w_c%reg%j_OC = j_OC

  end subroutine OC_SingleInstance_

!-----------------------------------------------------------------------
