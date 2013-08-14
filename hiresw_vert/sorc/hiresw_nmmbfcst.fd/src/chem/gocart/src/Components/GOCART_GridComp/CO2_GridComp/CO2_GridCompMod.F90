#ifdef GEOS5
#include "MAPL_Generic.h"
#endif

!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  CO2_GridCompMod --- CO2 Grid Component Class
!
! !INTERFACE:
!

   module  CO2_GridCompMod

! !USES:

#ifdef GEOS5
   USE ESMF_Mod
   USE MAPL_Mod
#endif

   use Chem_Mod              ! Chemistry Base Class
   use Chem_StateMod         ! Chemistry State
   use Chem_ConstMod, only: grav  
   use Chem_UtilMod
   use m_inpak90             ! Resource file management

#if defined(SPMD)
   use mod_comm, only: gid   ! FvGCM communication library
#endif

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  CO2_GridComp       ! The CO2 object 

!
! !PUBLIIC MEMBER FUNCTIONS:
!

   PUBLIC  CO2_GridCompInitialize
   PUBLIC  CO2_GridCompRun
   PUBLIC  CO2_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the (pre-ESMF) CO2 Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  24OCT2005     Bian  tag CO2 to 4 regions 
!                      (total, north america, south america, africa)
!  19dec2005 da Silva  Activated 3D diags for output
!                      
!EOP
!-------------------------------------------------------------------------

  type CO2_GridComp
        character(len=255) :: name
        CHARACTER(LEN=255) :: eFilen_biomass          ! biomass emissions
        CHARACTER(LEN=255) :: eFilen                  ! Other emissions
        CHARACTER(LEN=255) :: maskFileName

        INTEGER :: nymd_eFilen

        INTEGER :: BCnymd   ! Date of last emissions/prodction read
        REAL    :: BBconFac ! conversion factor of BB emissions to CO2

        REAL, POINTER ::    eCO2_FF(:,:)    ! kgC/m2/s, Earth surface
        REAL, POINTER ::   eCO2_NEP(:,:)    ! kgC/m2/s, Earth surface
        REAL, POINTER ::   eCO2_OCN(:,:)    ! kgC/m2/s, Earth surface
        REAL, POINTER ::    eCO2_BB_(:,:)   ! kgC/m2/s, PBL (before diurnal)
        REAL, POINTER ::    eCO2_BB(:,:)    ! kgC/m2/s, PBL
        REAL, POINTER :: regionMask(:,:)    ! regional mask
        INTEGER, POINTER ::  regionIndex(:) ! desired regions from mask

  end type CO2_GridComp

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO2_GridCompInitialize --- Initialize CO2_GridComp
!
! !INTERFACE:
!

   subroutine CO2_GridCompInitialize ( gcCO2, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real,    intent(in) :: cdt                  ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   type(CO2_GridComp), intent(inout) :: gcCO2   ! Grid Component
   type(ESMF_State), intent(inout)  :: impChem  ! Import State
   type(ESMF_State), intent(inout)  :: expChem  ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the CO2 Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  24OCT2005     Bian  Mods for 5 tagged CO2  
!                      (total, fossil fuel, ecosystem, oceanic, and biomass)
!  25OCT2005     Bian  Mods for 5 regions
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'CO2_GridCompInitialize'


   character(len=255) :: rcfilen = 'CO2_GridComp.rc'
   integer :: ios, n
   integer, allocatable :: ier(:)
   integer :: i1, i2, im, j1, j2, jm, km, ijl
   integer :: nbins, nbeg, nend, nbins_rc, nymd1, nhms1
   integer :: nTimes, begTime, incSecs
   real    :: qmin, qmax

   gcCO2%name = 'CO2 Constituent Package'
   gcCO2%BCnymd = -1

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   km = w_c%grid%km
   nbins = w_c%reg%n_CO2;  nbeg  = w_c%reg%i_CO2; nend  = w_c%reg%j_CO2

   ijl = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )

   call init_()
   if ( rc /= 0 ) return

   ier(:) =0

!                       -------------------
!                       Parse resource file
!                       -------------------

!  Load resource file
!  ------------------
   call i90_loadf ( TRIM(rcfilen), ier(1) )
   if ( ier(1) .ne. 0 ) then
      call final_(10)
      return
   end if
   ier(:)=0

   call i90_label ( 'number_CO2_bins:', ier(1) )
   nbins_rc = i90_gint ( ier(2) )
   if ( nbins_rc /= nbins ) then
      call final_(11)
      return
   end if


   CALL I90_label ( 'CO2_biomass_emission_filename:', ier(3) )
   CALL I90_gtoken( gcCO2%eFilen_biomass, ier(4) )
   CALL I90_label ( 'CO2_emissions_filename:', ier(5) )
   CALL I90_gtoken( gcCO2%eFilen, ier(6) )
   CALL I90_label ( 'CO2_biomass_emission_factor:', ier(7) )
     gcCO2%BBconFac = i90_gfloat ( ier(8) )


!  Get the desired regions to run on
   CALL I90_label ( 'CO2_regions:', ier(7) )
   CALL I90_gtoken( gcCO2%maskFileName, ier(8) )
   call i90_label ( 'CO2_regions_indices:', ier(9) )
   do n = 1, nbins
      gcCO2%regionIndex(n) = i90_gint ( ier(9+n) )
   end do

   IF( ANY( ier(:) /= 0 ) ) THEN
    CALL final_(12)
    RETURN
   END IF

!  Check initial date of inventory emission/oxidant files
!  ------------------------------------------------------
!  The intent here is that these files are valid for a particular
!  YYYY or YYYYMMDD (if 1x year in file).  We need to request
!  the correct date
   call Chem_UtilGetTimeInfo( gcCO2%eFilen, gcCO2%nymd_eFilen, &
                              begTime, nTimes, incSecs )

   ier(1) = gcCO2%nymd_eFilen
   if( any(ier(1:1) < 0 ) ) then
     call final_(60)
     return
   endif

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

   ier(:)=0

!  Set which fvGCM fields are needed for the CO chemistry driver
!  -------------------------------------------------------------
   CALL Chem_StateSetNeeded ( impChem, iPBLH,    .true., ier(1) )
   CALL Chem_StateSetNeeded ( impChem, iT  ,     .true., ier(2) )
   CALL Chem_StateSetNeeded ( impChem, iAIRDENS, .true., ier(3) )

   IF( ANY( ier(:) /= 0 ) ) THEN
    CALL final_(13)
    RETURN
   END IF
   ier(:)=0

!  Select fields to be produced in the export state.
!  ----------------------------------------------------------------
!  Emission Flux
   n = nbins
   if(n>0) call Chem_StateSetNeeded ( expChem, iCO2EM001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iCO2EM002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iCO2EM003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iCO2EM004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iCO2EM005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iCO2EM006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iCO2EM007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iCO2EM008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

!  Column Burden
   if(n>0) call Chem_StateSetNeeded ( expChem, iCO2CL001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iCO2CL002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iCO2CL003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iCO2CL004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iCO2CL005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iCO2CL006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iCO2CL007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iCO2CL008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

!  Surface Mixing Ratio
   ier(:)=0
   if(n>0) call Chem_StateSetNeeded ( expChem, iCO2SC001, .true., ier(1) )
   if(n>1) call Chem_StateSetNeeded ( expChem, iCO2SC002, .true., ier(2) )
   if(n>2) call Chem_StateSetNeeded ( expChem, iCO2SC003, .true., ier(3) )
   if(n>3) call Chem_StateSetNeeded ( expChem, iCO2SC004, .true., ier(4) )
   if(n>4) call Chem_StateSetNeeded ( expChem, iCO2SC005, .true., ier(5) )
   if(n>5) call Chem_StateSetNeeded ( expChem, iCO2SC006, .true., ier(6) )
   if(n>6) call Chem_StateSetNeeded ( expChem, iCO2SC007, .true., ier(7) )
   if(n>7) call Chem_StateSetNeeded ( expChem, iCO2SC008, .true., ier(8) )
   if(n>8) ier(9) = 1 ! not enough bins - need to change mod_diag.F

   IF( ANY( ier(:) /= 0 ) ) THEN
    CALL final_(14)
    RETURN
   END IF

!  Select fields to be produced in the export state.
!  ----------------------------------------------------------------
   ier(:)=0
   if(n>0) CALL Chem_StateSetNeeded ( expChem, iCO2      , .TRUE., ier(1) )
   if(n>1) CALL Chem_StateSetNeeded ( expChem, iCO2NAMER , .TRUE., ier(2) )
   if(n>2) CALL Chem_StateSetNeeded ( expChem, iCO2SAMER , .TRUE., ier(3) )
   if(n>3) CALL Chem_StateSetNeeded ( expChem, iCO2AFRIC , .TRUE., ier(4) )

   IF( ANY( ier(:) /= 0 ) ) THEN
    CALL final_(15)
    RETURN
   END IF

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif


   ier(:)=0

!  Obtain geographical region mask
!  -------------------------------
   call Chem_UtilGetTimeInfo( gcCO2%maskFileName, nymd1, &
                              begTime, nTimes, incSecs )
   if(nymd1 < 0) call final_(15)
   nhms1 = 120000
   CALL Chem_UtilMPread ( gcCO2%maskFileName, 'COMASK', nymd1, nhms1, &
                          i1, i2, 0, im, j1, j2, 0, jm, 0, &
                          var2d=gcCO2%regionMask, grid=w_c%grid_esmf )

#ifdef DEBUG
    CALL pmaxmin('CO2: Mask', gcCO2%regionMask, qmin, qmax, &
                 ijl,1, 1. )
#endif

   DEALLOCATE(ier)

   return

CONTAINS

   subroutine init_()
   integer ios, nerr
   nerr = max ( 32, nbins+1 )
   allocate (   gcCO2%eCO2_FF(i1:i2,j1:j2), & 
               gcCO2%eCO2_NEP(i1:i2,j1:j2), & 
               gcCO2%eCO2_OCN(i1:i2,j1:j2), & 
                gcCO2%eCO2_BB(i1:i2,j1:j2), & 
                gcCO2%eCO2_BB_(i1:i2,j1:j2), & 
             gcCO2%regionMask(i1:i2,j1:j2), &
             gcCO2%regionIndex(nbins), &
                      ier(nerr), stat=ios )

   if ( ios /= 0 ) rc = 100
   end subroutine init_

   subroutine final_(ierr)
   integer :: ierr
   integer ios
   deallocate ( gcCO2%eCO2_FF, gcCO2%eCO2_NEP, gcCO2%eCO2_OCN, &
                gcCO2%eCO2_BB, gcCO2%regionMask, gcCO2%regionIndex, &
                gcCO2%eCO2_BB_, &
                ier, stat=ios )
   call i90_release()
   rc = ierr
   end subroutine final_

   end subroutine CO2_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO2_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine CO2_GridCompRun ( gcCO2, w_c, impChem, expChem, &
                               nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(CO2_GridComp), intent(inout) :: gcCO2   ! Grid Component
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
 
! !DESCRIPTION: This routine implements the so-called CO2 Driver. That 
!               is, adds chemical tendencies to each of the constituents,
!  Note: water wapor, the first constituent is not considered a chemical
!  constituents.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  24OCT2005     Bian  Mods for 5 tagged CO2  
!                      (total, fossil fuel, ecosystem, oceanic, and biomass)
!  25OCT2005     Bian  Mods for 5 regions

!  Mask  Region
!  ----  -------------
!    1   North America
!    2   Mexico
!    3   Europe
!    4   Asia
!    5   Africa
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'CO2_GridCompRun'
   character(len=*), parameter :: Iam = myname

!  Input fields from fvGCM
!  -----------------------
   REAL, POINTER, DIMENSION(:,:)   ::  PBLH
   REAL, POINTER, DIMENSION(:,:,:) ::  T
   REAL, POINTER, DIMENSION(:,:,:) ::  RHOA

   integer :: i1, i2, im, j1, j2, jm, km, idiag, ios, ijl
   integer :: i, j, k, n, nbins, nbeg, nend
   INTEGER :: nymd1, nhms1, ier(7)

   REAL    :: qmin, qmax, BBconFac, c2co2

   REAL, PARAMETER :: mwtAir=28.97
   REAL, PARAMETER :: mwtCO2=44.00

#ifdef GEOS5 

#define EXPORT     expChem

#define ptrCO2EM      CO2_emis
#define ptrCO2CL      CO2_column
#define ptrCO2SC      CO2_surface

   integer :: STATUS

#include "CO2_GetPointer___.h"

#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Quantities to be exported
!  -------------------------
   type(Chem_Array), pointer :: CO2_emis(:), CO2_column(:), CO2_surface(:), &
                                CO2,CO2NAMER,CO2SAMER,CO2AFRIC

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   km = w_c%grid%km
   ijl = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )

   nbins = w_c%reg%n_CO2;  nbeg  = w_c%reg%i_CO2; nend  = w_c%reg%j_CO2

   c2co2=44.00/12.00

#ifdef GEOS5
   if ( any((/NBIN_CO2CL,NBIN_CO2SC/)/=NBIN_CO2EM)) then
      call die(myname,'all emissions in registry must have same number of bins')
   endif
   if ( nbins > NBIN_CO2EM ) then
      call die(myname,'nbins in chem registry must be <= those in component registry')
   end if
#endif


!  Update emissions once each day.
!  -----------------------------------------------------
 IF ( gcCO2%BCnymd /= nymd ) THEN

!   Selections based on biomass burning emission set chosen
!   Currently, parse on:
!    bian      -> kg C m-2 s-1
!    modisfire -> kg CO2 m-2 s-1
!    else      -> based on dry matter consumed

!   Biomass Burning -- select on known inventories
!   ----------------------------------------------

!   Biomass burning climatology, is in kg C m^-2 s^-1
!   ------------------------------------------------- 
    IF ( index(gcCO2%eFilen_biomass,'bian') .GT. 0 ) then  
       nymd1 = 2000*10000 + MOD ( nymd, 10000 )  ! assumes 2000
       nhms1 = 120000
       BBconFac = c2co2
       CALL Chem_UtilMPread ( gcCO2%eFilen_biomass, 'emco2bb', nymd1, nhms1, &
                              i1, i2, 0, im, j1, j2, 0, jm, 0, &
                              var2d=gcCO2%eCO2_BB, cyclic=.true., &
                              grid=w_c%grid_esmf )

    ELSE  ! time-varying biomass burning
 
!      Biomass burning daily files, currently in kg m^-2 s^-1
!      -------------------------------------------------------
!      Daily files (e.g., MODIS) or GFED v.2 (1997 - 2005 valid)
       BBconFac = gcCO2%BBconFac
       if (  index(gcCO2%eFilen_biomass,'%') .gt. 0 .or. &
            (index(gcCO2%eFilen_biomass,'gfed') .gt. 0 .and. &
             index(gcCO2%eFilen_biomass,'v2')   .gt. 0) ) then  
          nymd1 = nymd
          nhms1 = 120000

!      Assume GFED climatology or Martin (Duncan) climatology
       else                                            
          nymd1 = 1971*10000 + mod ( nymd, 10000 )  ! assumes 1971
!          nymd1 = nymd
          nhms1 = 120000
       end if

       CALL Chem_UtilMPread ( gcCO2%eFilen_biomass, 'biomass', nymd1, nhms1, &
                              i1, i2, 0, im, j1, j2, 0, jm, 0, &
                              var2d=gcCO2%eCO2_BB, cyclic=.true., &
                              grid=w_c%grid_esmf )
    ENDIF ! type of biomass burning


    nymd1 = (gcCO2%nymd_eFilen/10000)*10000 + MOD ( nymd, 10000 )
    nhms1 = 120000
    CALL Chem_UtilMPread ( gcCO2%eFilen, 'emco2ff', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcCO2%eCO2_FF, cyclic=.true., &
                           grid=w_c%grid_esmf )
    CALL Chem_UtilMPread ( gcCO2%eFilen, 'emco2nep', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcCO2%eCO2_NEP, cyclic=.true., &
                           grid=w_c%grid_esmf )
    CALL Chem_UtilMPread ( gcCO2%eFilen, 'emco2ocn', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, 0, &
                           var2d=gcCO2%eCO2_OCN, cyclic=.true., &
                           grid=w_c%grid_esmf )

#ifdef DEBUG
    CALL pmaxmin('CO2: e_ff',  gcCO2%eCO2_FF,  qmin, qmax, ijl, 1, 1. )
    CALL pmaxmin('CO2: e_nep', gcCO2%eCO2_NEP, qmin, qmax, ijl, 1, 1. )
    CALL pmaxmin('CO2: e_ocn', gcCO2%eCO2_OCN, qmin, qmax, ijl, 1, 1. )
    CALL pmaxmin('CO2: e_bb',  gcCO2%eCO2_BB,  qmin, qmax, ijl, 1, 1. )
#endif

    gcCO2%BCnymd = nymd

!  Units for surface flux must be kgCO2 m^-2 s^-1
!  -------------------------------------------
    gcCO2%eCO2_FF(i1:i2,j1:j2) = gcCO2%eCO2_FF(i1:i2,j1:j2)*c2co2

!   Bian says that we need to adjust the uptake flux of CO2 in the
!   ecosystem database to reflect the emissions from biomass burning.
!   In principle this adds a factor which needs to be balanced on an
!   interannual basis.  For year 2000 TRMM (GFED v 1.2) emissions this
!   factor is 1.2448
!   ------------------------------------------------------------------
    WHERE(gcCO2%eCO2_NEP(i1:i2,j1:j2) .gt. 0.0) &
        gcCO2%eCO2_NEP(i1:i2,j1:j2) = gcCO2%eCO2_NEP(i1:i2,j1:j2)*c2co2
    WHERE(gcCO2%eCO2_NEP(i1:i2,j1:j2) .le. 0.0) &
        gcCO2%eCO2_NEP(i1:i2,j1:j2) = gcCO2%eCO2_NEP(i1:i2,j1:j2)*c2co2*1.2448
    gcCO2%eCO2_OCN(i1:i2,j1:j2) = gcCO2%eCO2_OCN(i1:i2,j1:j2)*c2co2
    gcCO2%eCO2_BB(i1:i2,j1:j2) = gcCO2%eCO2_BB(i1:i2,j1:j2)*BBconFac

!   Save this in case we need to apply diurnal cycle
!   ------------------------------------------------
   if ( w_c%diurnal_bb ) then
        gcCO2%eCO2_BB_(:,:) = gcCO2%eCO2_BB(:,:) 
   end if

 END IF  ! time to update biomass burning

!  Apply diurnal cycle if so desired
!  ---------------------------------
   if ( w_c%diurnal_bb ) then
      call Chem_BiomassDiurnal ( gcCO2%eCO2_BB, gcCO2%eCO2_BB_,   &
                                 w_c%grid%lon(:), w_c%grid%lat(:), nhms, cdt )      
   end if

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Work space for holding CO output
!  ----------------------------------
   allocate ( CO2_emis(nbins), CO2_column(nbins), CO2_surface(nbins), &
              CO2, CO2NAMER, CO2SAMER, CO2AFRIC, &
              stat = ios )
   if ( ios /= 0 ) then
      rc = 1
      return
   end if

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!  Get imports
!  -----------
#ifdef GEOS5
   call MAPL_GetPointer ( impChem, pblh, 'ZPBL',    rc=ier(1) ) 
   call MAPL_GetPointer ( impChem, T,    'T',       rc=ier(2) ) 
   call MAPL_GetPointer ( impChem, rhoa, 'AIRDENS', rc=ier(3) ) 
#else

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 
   CALL Chem_StateGetArray2D ( impChem, iPBLH,     pblh,     ier(1) )
   CALL Chem_StateGetArray3D ( impChem, iT,           T,     ier(2) )
   call Chem_StateGetArray3D ( impChem, iAIRDENS,  rhoa,     ier(3) )
!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

#ifdef DEBUG
    CALL pmaxmin('CO2: pblh', pblh, qmin, qmax, ijl, 1, 1. )
    CALL pmaxmin('CO2: T',    T,    qmin, qmax, ijl, 1, 1. )
    CALL pmaxmin('CO2: rhoa', rhoa, qmin, qmax, ijl, 1, 1. )
#endif

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Get pointers to export state
!  ----------------------------
   do n = 1, nbins
      idiag = iCO2EM001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, CO2_emis(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iCO2CL001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, CO2_column(n)%data2d, ier(n) )
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   do n = 1, nbins
      idiag = iCO2SC001 + n - 1
      call Chem_StateGetArray2D ( expChem, idiag, CO2_surface(n)%data2d, ier(n))
   end do
   if ( any(ier(1:nbins) /= 0) ) then
        rc = 15 
        return
   end if

   n = nbins
   ier = 0
   if(n>0) CALL Chem_StateGetArray3D( expChem,      iCO2,      CO2%data3d, ier(1) )
   if(n>1) CALL Chem_StateGetArray3D( expChem, iCO2NAMER, CO2NAMER%data3d, ier(2) )
   if(n>2) CALL Chem_StateGetArray3D( expChem, iCO2SAMER, CO2SAMER%data3d, ier(3) )
   if(n>3) CALL Chem_StateGetArray3D( expChem, iCO2AFRIC, CO2AFRIC%data3d, ier(4) )

   IF( ANY( ier(1:4) /= 0 ) ) THEN
      rc = 20
   END IF

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!  CO2 Emissions
!  -------------
   call CO2_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcCO2, w_c, &
                      pblh, T, rhoa, CO2_emis, rc)


!  Fill the export states
!  ----------------------
!  Surface concentration in PPMv
   do n = 1, nbins
    if(associated(CO2_surface(n)%data2d)) &
      CO2_surface(n)%data2d(i1:i2,j1:j2) = w_c%qa(nbeg+n-1)%data3d(i1:i2,j1:j2,km)*1.e6
   enddo

!  Column burden in kg m-2
!  -----------------------
   do n = 1, nbins
     if(associated(CO2_column(n)%data2d)) then
      CO2_column(n)%data2d(i1:i2,j1:j2) = 0.
      do k = 1, km
       CO2_column(n)%data2d(i1:i2,j1:j2) &
        =   CO2_column(n)%data2d(i1:i2,j1:j2) &
          +   w_c%qa(nbeg+n-1)%data3d(i1:i2,j1:j2,k)*mwtCO2/mwtAir &
            * w_c%delp(i1:i2,j1:j2,k)/grav
     enddo
    endif
   enddo

!  Fill the export state with current mixing ratios
!  ------------------------------------------------
   if(associated(CO2%data3d)) &
      CO2%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km)
   if(associated(CO2NAMER%data3d)) &
      CO2NAMER%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nbeg+1)%data3d(i1:i2,j1:j2,1:km)
   if(associated(CO2SAMER%data3d)) &
      CO2SAMER%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nbeg+2)%data3d(i1:i2,j1:j2,1:km)
   if(associated(CO2AFRIC%data3d)) &
      CO2AFRIC%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nbeg+3)%data3d(i1:i2,j1:j2,1:km)

#ifndef GEOS5

!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

   deallocate( CO2_emis, CO2_column, CO2_surface, &
               CO2, CO2NAMER, CO2SAMER, CO2AFRIC, stat=ios)

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

   return

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  CO2_Emission - Adds emissions for CO2 for one timestep
!             We have emissions from 4 sources, which are distributed
!             differently in the vertical
!             1) fossil fuel - emitted at surface
!             2) ecosystem   - fluxes at surface
!             3) oceanic     - fluxes at surface
!             4) biomass burning - uniformly mixed in PBL
!
! !INTERFACE:
!

   subroutine CO2_Emission ( i1, i2, j1, j2, km, nbins, cdt, gcCO2, w_c, &
                            pblh, T, rhoa, CO2_emis, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km, nbins
   real, intent(in)    :: cdt
   type(CO2_GridComp), intent(in)   :: gcCO2       ! CO2 Grid Component
   real, pointer, dimension(:,:)    :: pblh
   real, pointer, dimension(:,:,:)  :: T
   real, pointer, dimension(:,:,:)  :: rhoa

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   type(Chem_Array), intent(inout)  :: CO2_emis(nbins) ! CO2 emissions, kg/m2/s
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 -
   character(len=*), parameter :: myname = 'CO2_Emission'

! !DESCRIPTION: Updates the CO2 concentration with emissions every timestep
!
! !REVISION HISTORY:
!
!  24Oct2005, Bian
!
!EOP
!-------------------------------------------------------------------------

! !Local Variables
   integer  ::  i, j, k, m, n, ios
   integer  ::  nbeg, nend
   real, dimension(i1:i2,j1:j2) :: pPblh  ! pressure at PBLH
   real, dimension(i1:i2,j1:j2) :: p0, z0, ps
   real :: p1, z1, dz, delz, delp, fPblh, fs, fu
   real :: zpbl
   real :: srctot
   integer :: iregWant

   REAL, PARAMETER :: mwtAir=28.97
   REAL, PARAMETER :: mwtCO2=44.00

!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_CO2
   nend  = w_c%reg%j_CO2

!  Zero out CO2 emissions
!  ----------------------
   do n = 1, nbins
    if(associated(CO2_emis(n)%data2d)) CO2_emis(n)%data2d(i1:i2,j1:j2) = 0.
   enddo

!  Find the pressure of PBLH altitudes
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
      zpbl = max ( 100., pblh(i,j) ) 
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

!  Now update the tracer mixing ratios with the CO2 sources
   p0 = ps
   do k = km, 1, -1
      if ( k .eq. km) fs = 1.00
      if ( k .ne. km) fs = 0.00
    do j = j1, j2
     do i = i1, i2
      p1 = p0(i,j) - w_c%delp(i,j,k)

      fPblh = 0.
      if(p1 .ge. pPblh(i,j)) fPblh = w_c%delp(i,j,k)/(ps(i,j)-pPblh(i,j))
      if(p1 .lt. pPblh(i,j) .and. p0(i,j) .ge. pPblh(i,j)) &
       fPblh = (p0(i,j)-pPblh(i,j))/(ps(i,j)-pPblh(i,j))

!  Convert emission from Kg CO2/m2/s to mixing ratio/s (TVVMM/(delz*airden))
!  --------------------------
   fu = (mwtAir/mwtCO2)/(w_c%delp(i,j,k)/grav)

!     Total source in kg m-2 s-1
      srctot = (gcCO2%eCO2_FF(i,j) * fs  &
             + gcCO2%eCO2_NEP(i,j) * fs  &
             + gcCO2%eCO2_OCN(i,j) * fs  &
             + gcCO2%eCO2_BB(i,j) * fPblh ) * fu 
! Get tagged CO2 to regions
      do n = 1, nbins
       iregWant = gcCO2%regionIndex(n)
       if(iregWant .eq. -1) then
        w_c%qa(nbeg+n-1)%data3d(i,j,k)   =   w_c%qa(nbeg+n-1)%data3d(i,j,k)   &
                                  + srctot*cdt
        if(associated(CO2_emis(n)%data2d)) &
         CO2_emis(n)%data2d(i,j) = CO2_emis(n)%data2d(i,j) + srctot/fu
       else
        if(int(gcCO2%regionMask(i,j)) .EQ. iregWant) then
           w_c%qa(nbeg+n-1)%data3d(i,j,k) =   w_c%qa(nbeg+n-1)%data3d(i,j,k) + srctot*cdt
           if(associated(CO2_emis(n)%data2d)) &
            CO2_emis(n)%data2d(i,j) = CO2_emis(n)%data2d(i,j) + srctot/fu
        endif
       endif
      enddo
      p0(i,j) = p1
     end do
    end do
   end do

   rc = 0

   end subroutine CO2_Emission

 end subroutine CO2_GridCompRun

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO2_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine CO2_GridCompFinalize ( gcCO2, w_c, impChem, expChem, &
                                    nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(CO2_GridComp), intent(inout) :: gcCO2   ! Grid Component

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

   character(len=*), parameter :: myname = 'CO2_GridCompFinalize'
   INTEGER :: ios

   DEALLOCATE ( gcCO2%eCO2_FF, gcCO2%eCO2_NEP, gcCO2%eCO2_OCN, &
                gcCO2%eCO2_BB, gcCO2%regionMask, STAT=ios )
   rc = 0
   IF ( ios /= 0 ) rc = 1

   return

 end subroutine CO2_GridCompFinalize

 end module CO2_GridCompMod

