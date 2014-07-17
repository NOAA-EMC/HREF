#ifdef GEOS5
#include "MAPL_Generic.h"
#endif

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

#ifdef GEOS5
   USE ESMF_Mod
   USE MAPL_Mod
#endif

   use Chem_Mod              ! Chemistry Base Class
   use Chem_StateMod         ! Chemistry State
   use Chem_DepositionMod    ! Aerosol Deposition
   use Chem_ConstMod, only: grav, von_karman, cpd, &
                            undefval => undef         ! Constants !
   use Chem_UtilMod          ! I/O
   use Chem_MieMod           ! Aerosol LU Tables, calculator
   use m_inpak90             ! Resource file management
   use m_die, only: die

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
        integer :: nymd_bf
        integer :: nymd_ebcant1
        integer :: nymd_ebcant2
	integer :: nymd_bc_ship
  end type BC_GridComp

  real, parameter :: OCEAN=0.0, LAND = 1.0, SEA_ICE = 2.0

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

   type(Chem_Bundle), intent(inout) :: w_c        ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real, intent(in) :: cdt                     ! chemistry timestep (secs)

! !OUTPUT PARAMETERS:

   type(BC_GridComp), intent(inout) :: gcBC   ! Grid Component
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
   integer :: i1, i2, im, j1, j2, jm, nbins, nbeg, nend, nbins_rc
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
   nbeg  = w_c%reg%i_BC
   nend  = w_c%reg%j_BC

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
      w_c%reg%fscav(nbeg+n-1) = i90_gfloat ( ier(n+1) )
   end do
   if ( any(ier(1:nbins+1) /= 0) ) then
      call final_(50)
      return
   end if

!                          -------
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

!  Check initial date of inventory emission/oxidant files
!  ------------------------------------------------------
!  The intent here is that these files are valid for a particular
!  YYYY or YYYYMMDD (if 1x year in file).  We need to request
!  the correct date
   call Chem_UtilGetTimeInfo( gcBC%bf_srcfilen, gcBC%nymd_bf, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcBC%ebcant1_srcfilen, gcBC%nymd_ebcant1, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcBC%ebcant2_srcfilen, gcBC%nymd_ebcant2, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcBC%bc_ship_srcfilen, gcBC%nymd_bc_ship, &
                              begTime, nTimes, incSecs )
   ier(1) = gcBC%nymd_bf
   ier(2) = gcBC%nymd_ebcant1
   ier(3) = gcBC%nymd_ebcant2
   ier(4) = gcBC%nymd_bc_ship
   if( any(ier(1:4) < 0 ) ) then
     call final_(60)
     return
   endif


!  Initialize date for BCs
!  -----------------------
   gcBC%nymd = -1   ! nothing read yet

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
   if(n>0) call Chem_StateSetNeeded ( expChem, iBCEM001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iBCEM002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iBCEM003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iBCEM004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iBCEM005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iBCEM006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iBCEM007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iBCEM008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif

!  Dry Deposition Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iBCDP001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iBCDP002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iBCDP003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iBCDP004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iBCDP005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iBCDP006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iBCDP007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iBCDP008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif

!  Wet Deposition Flux
   if(n>0) call Chem_StateSetNeeded ( expChem, iBCWT001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iBCWT002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iBCWT003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iBCWT004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iBCWT005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iBCWT006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iBCWT007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iBCWT008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   if ( any(ier(1:9) /= 0) ) then
        call final_(70)
        return
   endif


!  Other diagnostics
   call Chem_StateSetNeeded ( expChem, iBCSMASS, .true., ier(1) )
   call Chem_StateSetNeeded ( expChem, iBCCMASS, .true., ier(2) )
   call Chem_StateSetNeeded ( expChem, iBCMASS, .true., ier(3) )
   call Chem_StateSetNeeded ( expChem, iBCEXTTAU, .true., ier(4) )
   call Chem_StateSetNeeded ( expChem, iBCSCATAU, .true., ier(5) )
   call Chem_StateSetNeeded ( expChem, iBCEMAN, .true., ier(6))
   call Chem_StateSetNeeded ( expChem, iBCEMBB, .true., ier(7))
   call Chem_StateSetNeeded ( expChem, iBCEMBF, .true., ier(8))
   call Chem_StateSetNeeded ( expChem, iBCHYPHIL, .true., ier(9))

   if ( any(ier(1:9) /= 0) ) then
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

   integer :: ier(20), idiag
   integer :: i1, i2, im, j1, j2, jm, nbins, nbeg, nend, km, n, ios
   integer :: i, j, k, nymd1, nhms1, ijl, ijkl
   real :: qmax, qmin
   real :: qUpdate, delq
   real, pointer :: BC_radius(:), BC_rhop(:)


!  Input fields from fvGCM
!  -----------------------
   real, pointer, dimension(:,:) :: fraclake, gwettop, oro, u10m, v10m, &
                                    xlai, ustar, precc, precl,          &
                                    pblh, shflux, z0h, hsurf
   real, pointer, dimension(:,:,:) ::  dqcond, tmpu, rhoa, u, v, hghte

#ifdef GEOS5 

#define EXPORT     expChem

#define ptrBCWT       BC_wet
#define ptrBCEM       BC_emis
#define ptrBCDP       BC_dep

#define ptrBCMASS     BC_mass
#define ptrBCEMAN     BC_emisAN
#define ptrBCEMBB     BC_emisBB
#define ptrBCEMBF     BC_emisBF
#define ptrBCHYPHIL   BC_toHydrophilic
#define ptrBCSMASS    BC_sfcmass
#define ptrBCCMASS    BC_colmass
#define ptrBCEXTTAU   BC_exttau
#define ptrBCSCATAU   BC_scatau

   integer :: STATUS

#include "BC_GetPointer___.h"

#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Quantities to be exported
!  -------------------------
   type(Chem_Array), pointer :: BC_emis(:), BC_dep(:), BC_wet(:), &
                                BC_sfcmass, BC_colmass, BC_mass, BC_exttau, &
                                BC_scatau, BC_emisAN, BC_emisBB, BC_emisBF, &
                                BC_toHydrophilic

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm

   km = w_c%grid%km
   nbins = w_c%reg%n_BC
   nbeg  = w_c%reg%i_BC
   nend  = w_c%reg%j_BC

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
       nymd1 = 1971*10000 + mod ( nymd, 10000 )  ! assumes 1971
!       nymd1 = nymd
       nhms1 = 120000
    end if

    call Chem_UtilMPread ( gcBC%bb_srcfilen, 'biomass', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcBC%biomass_src, cyclic=.true., &
                           grid = w_c%grid_esmf  )


!   Biofuel and anthropogenic emissions (inventories)
!   -------------------------------------------------
    nymd1 = (gcBC%nymd_bf/10000)*10000 + mod ( nymd, 10000 )
    nhms1 = 120000
    call Chem_UtilMPread ( gcBC%bf_srcfilen, 'biofuel', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcBC%biofuel_src, cyclic=.true., &
                           grid = w_c%grid_esmf  )

    nymd1 = gcBC%nymd_ebcant1
    nhms1 = 120000
    call Chem_UtilMPread ( gcBC%ebcant1_srcfilen, 'antebc1', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcBC%ebcant1_src, cyclic=.true., &
                           grid = w_c%grid_esmf  )

!   Functionality for only a single layer of emissions
    if( index(gcBC%ebcant2_srcfilen,'--') .gt. 0) then
     gcBC%ebcant2_src(i1:i2,j1:j2) = 0.
    else
     nymd1 = gcBC%nymd_ebcant2
     nhms1 = 120000
     call Chem_UtilMPread ( gcBC%ebcant2_srcfilen, 'antebc2', nymd1, nhms1, &
                            i1, i2, 0, im, j1, j2, 0, jm, 0, &
                            var2d=gcBC%ebcant2_src, grid = w_c%grid_esmf  )

    endif

!   Ship based BC emissions
    nymd1 = gcBC%nymd_bc_ship
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
                                 w_c%grid%lon(:), w_c%grid%lat(:), nhms, cdt )      
   end if

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Work space for holding bc output
!  ----------------------------------
   allocate ( BC_emis(nbins),  BC_dep(nbins), BC_wet(nbins), &
              BC_sfcmass, BC_colmass, BC_mass, BC_exttau, BC_scatau, &
              BC_emisAN, BC_emisBB, BC_emisBF, BC_toHydrophilic, &
              stat = ios )
   if ( ios /= 0 ) then
      rc = 1
      return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

#ifdef DEBUG
   do n = nbeg, nend
      call pmaxmin ( 'BC: q_beg', w_c%qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, &
                   ijl, km, 1. )
   end do
#endif

!  BC particle radius [m] and density [kg m-3]
!  ---------------------------------------------
!  For now these are dummy values and unused below
   allocate( BC_radius(nbins), BC_rhop(nbins) )
   BC_radius(:) = 0.5e-6
   BC_rhop(:)   = 1000.


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
    

   if ( .not. associated(gwettop) ) &
        call write_parallel('gwettop NOT associated')

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

   if ( any(ier(1:19) /= 0) ) then
        rc = 10 
        return
   end if

!  Make sure LAI has values over ocean
!  -----------------------------------
   where ( oro /= LAND  )  xlai = 0.0

#ifdef DEBUG

   call pmaxmin('BC: fraclake   ', fraclake, qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: gwtop      ', gwettop , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: oro        ', oro     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: u10m       ', u10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: v10m       ', v10m    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: xlai       ', xlai    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: ustar      ', ustar   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: precc      ', precc   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: precl      ', precl   , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: pblh       ', pblh    , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: shfflux    ', shflux  , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: z0h        ', z0h     , qmin, qmax, ijl,1, 1. )
   call pmaxmin('BC: hsurf      ', hsurf   , qmin, qmax, ijl,1, 1. )

   call pmaxmin('BC: dqcond     ', dqcond  , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: tmpu       ', tmpu    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: rhoa       ', rhoa    , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: u          ', u       , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: v          ', v       , qmin, qmax, ijkl,1, 1. )
   call pmaxmin('BC: hghte      ', hghte   , qmin, qmax, ijkl,1, 1. )

#endif

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 


!  Get pointers to export state
!  ----------------------------
   do n = 1, nbins
      idiag = iBCEM001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, BC_emis(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iBCDP001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, BC_dep(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iBCWT001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, BC_wet(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   idiag = iBCSMASS
   call Chem_StateGetArray2D ( expChem, idiag, BC_sfcmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iBCCMASS
   call Chem_StateGetArray2D ( expChem, idiag, BC_colmass%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iBCMASS
   call Chem_StateGetArray3D ( expChem, idiag, BC_mass%data3d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iBCEXTTAU
   call Chem_StateGetArray2D ( expChem, idiag, BC_exttau%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iBCSCATAU
   call Chem_StateGetArray2D ( expChem, idiag, BC_scatau%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iBCEMAN
   call Chem_StateGetArray2D ( expChem, idiag, BC_emisAN%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iBCEMBF
   call Chem_StateGetArray2D ( expChem, idiag, BC_emisBF%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iBCEMBB
   call Chem_StateGetArray2D ( expChem, idiag, BC_emisBB%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

   idiag = iBCHYPHIL
   call Chem_StateGetArray2D ( expChem, idiag, BC_toHydrophilic%data2d, ier(1) )
   if ( ier(1) /= 0 ) then
        rc = 15 
        return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif


!  BC Source
!  -----------
   call BC_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcBC, w_c, &
                      pblh, tmpu, rhoa, BC_emis, &
                      BC_emisAN, BC_emisBB, BC_emisBF, rc )

#ifdef DEBUG
   do n = nbeg, nend
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
      qUpdate = w_c%qa(nbeg)%data3d(i,j,k)*exp(-4.63e-6*cdt)
      qUpdate = max(qUpdate,1e-32)
      delq = max(0.,w_c%qa(nbeg)%data3d(i,j,k)-qUpdate)
      w_c%qa(nbeg)%data3d(i,j,k) = qUpdate
      w_c%qa(nend)%data3d(i,j,k) = w_c%qa(nend)%data3d(i,j,k)+delq
      if(associated(BC_toHydrophilic%data2d)) &
       BC_toHydrophilic%data2d(i,j) = BC_toHydrophilic%data2d(i,j) &
        + delq*w_c%delp(i,j,k)/grav/cdt
     end do
    end do
   end do

!  BC Deposition
!  -----------
   call Chem_Deposition( i1, i2, j1, j2, km, nbeg, nend, nbins, cdt, w_c, &
                         BC_radius, BC_rhop, tmpu, rhoa, hsurf, hghte, oro, ustar, &
                         u10m, v10m, fraclake, gwettop, pblh, shflux, z0h, BC_dep, rc )

!  BC Wet Removal
!  -----------
   call BC_Wet_Removal ( i1, i2, j1, j2, km, nbins, cdt, rhoa, gcBC, w_c, &
                         precc, precl, dqcond, tmpu, BC_wet, rc )

!  Compute the desired output diagnostics here
!  Ideally this will go where chemout is called in fvgcm.F since that
!  will reflect the distributions after transport, etc.
!  ------------------------------------------------------------------
   call BC_Compute_Diags(i1, i2, j1, j2, km, nbins, gcBC, w_c, tmpu, rhoa, &
                         BC_sfcmass, BC_colmass, BC_mass, BC_exttau, &
                         BC_scatau, rc)

!  Clean up
!  --------
   deallocate(BC_radius, BC_rhop, stat=ios)

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

   deallocate(BC_emis, BC_dep, BC_wet, BC_sfcmass, BC_colmass, BC_mass, &
              BC_emisAN, BC_emisBB, BC_emisBF, BC_toHydrophilic, &
              BC_exttau, BC_scatau, stat=ios)

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

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

#ifndef GEOS5
      maxAll = max( maxAll, max( srcHydrophobic(i,j), srcHydrophilic(i,j)))
#endif

     end do ! i
    end do  ! j

#ifdef GEOS5
!   Determine global max/min
!   ------------------------
    call pmaxmin ( 'BC: Phobic ', srcHydrophobic, qmin, qmax, ijl, 1, 0. )
    maxAll = abs(qmax) + abs(qmin)
    call pmaxmin ( 'BC: Philic ', srcHydrophilic, qmin, qmax, ijl, 1, 0. )
    maxAll = max ( maxAll, abs(qmax) + abs(qmin) )
#endif

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
! !IROUTINE:  BC_Wet_Removal - Removal of dust by precipitation
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

   subroutine BC_Wet_Removal ( i1, i2, j1, j2, km, nbins, cdt, rhoa,gcBC, w_c, &
                               precc, precl, dqcond, tmpu, fluxout, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   type(BC_GridComp), intent(in)   :: gcBC  ! BC Grid Component
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
   character(len=*), parameter :: myname = 'BC_Wet_Removal'

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
   integer  ::  nHydrophilic
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

   rc=0

!  Initialize local variables
!  --------------------------
   do n = 1, nbins
    if( associated(fluxout(n)%data2d) ) fluxout(n)%data2d(i1:i2,j1:j2) = 0.0
   end do

   nbeg  = w_c%reg%i_BC
   nend  = w_c%reg%j_BC
   nHydrophilic = 2

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
      if ( gcBC%doing_scav == 1 ) then
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
       do n = nHydrophilic, nHydrophilic
        DC(n) = w_c%qa(nbeg+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
        w_c%qa(nbeg+n-1)%data3d(i,j,k) = max(w_c%qa(nbeg+n-1)%data3d(i,j,k),1.e-32)
       end do
!      Flux down:  unit is kg m-2
!      Formulated in terms of production in the layer.  In the revaporation step
!      we consider possibly adding flux from above...
       do n = nHydrophilic, nHydrophilic
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

!       Adjust du level:
        do n = nHydrophilic, nHydrophilic
         DC(n) = w_c%qa(nbeg+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
         w_c%qa(nbeg+n-1)%data3d(i,j,k) = max(w_c%qa(nbeg+n-1)%data3d(i,j,k),1.e-32)
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
       do n = nHydrophilic, nHydrophilic
        DC(n) = w_c%qa(nbeg+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
        if (DC(n).lt.0.) DC(n) = 0.
        w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
        w_c%qa(nbeg+n-1)%data3d(i,j,k) = max(w_c%qa(nbeg+n-1)%data3d(i,j,k),1.e-32)
       end do

!------  Flux down:  unit is kg. Including both ls and cv.
       do n = nHydrophilic, nHydrophilic
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
        do n = nHydrophilic, nHydrophilic
         DC(n) = w_c%qa(nbeg+n-1)%data3d(i,j,k) * F * (1.-exp(-BT))
         if (DC(n).lt.0.) DC(n) = 0.
         w_c%qa(nbeg+n-1)%data3d(i,j,k) = w_c%qa(nbeg+n-1)%data3d(i,j,k)-DC(n)
         w_c%qa(nbeg+n-1)%data3d(i,j,k) = max(w_c%qa(nbeg+n-1)%data3d(i,j,k),1.e-32)
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
          do n = nHydrophilic, nHydrophilic
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

     do n = nHydrophilic, nHydrophilic
      if( associated(fluxout(n)%data2d) ) then
       fluxout(n)%data2d(i,j) = fluxout(n)%data2d(i,j)+Fd(km,n)/cdt
      endif
     end do

 100 continue
    end do   ! i
   end do    ! j

   deallocate(fd,DC,stat=ios)

   end subroutine BC_Wet_Removal

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  BC_Compute_Diags - Calculate dust 2D diagnostics
!
! !INTERFACE:
!

   subroutine BC_Compute_Diags ( i1, i2, j1, j2, km, nbins, gcBC, w_c, tmpu, &
                                 rhoa, sfcmass, colmass, mass, exttau, scatau, &
                                 rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:
   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   type(BC_GridComp), intent(inout):: gcBC     ! BC Grid Component
   type(Chem_Bundle), intent(in)   :: w_c      ! Chem Bundle
   real, pointer, dimension(:,:,:) :: tmpu     ! temperature [K]
   real, pointer, dimension(:,:,:) :: rhoa     ! air density [kg m-3]

! !OUTPUT PARAMETERS:
   type(Chem_Array), intent(inout)  :: sfcmass ! sfc mass concentration kg/m3
   type(Chem_Array), intent(inout)  :: colmass ! col mass density kg/m2
   type(Chem_Array), intent(inout)  :: mass    ! 3d mass mixing ratio kg/kg
   type(Chem_Array), intent(inout)  :: exttau  ! ext. AOT at 550 nm
   type(Chem_Array), intent(inout)  :: scatau  ! sct. AOT at 550 nm
   integer, intent(out)             :: rc      ! Error return code:
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
   integer :: i, j, k, n, nbeg, nend, ios, nch, idx
   real :: tau, ssa
   character(len=255) :: qname


!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_BC
   nend  = w_c%reg%j_BC
   nch   = gcBC%mie_tables%nch

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

!  Calculate the dust column loading
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

!  Calculate the total mass mixing ratio
   if( associated(mass%data3d) ) then
      mass%data3d(i1:i2,j1:j2,1:km) = 0.
      do n = 1, nbins
       mass%data3d(i1:i2,j1:j2,1:km) &
         =   mass%data3d(i1:i2,j1:j2,1:km) &
           + w_c%qa(n+nbeg-1)%data3d(i1:i2,j1:j2,1:km)
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

      do n = 1, nbins

!      Select the name for species
       qname = trim(w_c%reg%vname(w_c%reg%i_BC+n-1))
       idx = Chem_MieQueryIdx(gcBC%mie_tables,qname,rc)
       if(rc .ne. 0) call die(myname, 'cannot find proper Mie table index')

!      Recall -- at present need to divide RH by 100 to get to tables
       do k = 1, km
        do j = j1, j2
         do i = i1, i2
          call Chem_MieQuery(gcBC%mie_tables, idx, 1., &
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

