#include "MAPL_Generic.h"

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: GOCART_GridCompMod - The GOCART Aerosol Grid Component
!
! !INTERFACE:
!
   Module GOCART_GridCompMod
!
! !USES:
!
   use ESMF_Mod
   use MAPL_Mod
   use MAPL_GenericMod

   use Chem_Mod              ! Chemistry Base Class
   use Chem_UtilMod, only: Chem_UtilNegFiller
   use Aero_GridCompMod      ! Parent Aerosol component with
                             !   IRF methods but no SetServices()
   USE m_chars, ONLY: uppercase

   implicit none
   private
!
! !PUBLIC MEMBER FUNCTIONS:

   public GOCART_SetServices
!   public SetServices
!
! !DESCRIPTION: 
!
!   {\tt GOCART} is a gridded component from the GOCART model and includes 
!  dust, sea salt, sulfates, organic and black carbon. In addition, we
!  also include closely related components for CO and CO2 with relatively
!  simple parameterization of the chemical processes, but sharing
!  consistent emissions with the aerosols.
!
!  This code derives from the pre-ESMF Chem component from GEOS-4. This
!  GEOS-4 Chem "component" used ESMF like constructs (Chem component class, 
!  import/export states, etc) but no ESMF specific data types because of 
!  an odd incompatibility with the fvGCM code (the so-called 
!  {\tt oldworld} library. Unlike GEOS-4, the Stratospheric Chemistry
!  component is treated separately here.
!
! !REVISION HISTORY:
!
!  25feb2005  da Silva  First crack.
!  19jul2006  da Silva  First separate GOCART component.
!  18jun2010  Lu        Add gaseous species (dms,so2,msa) to AERO bundle
!  16oct2010  Lu        Add fscav to iAERO bundle 
!  14nov2010  Lu        Add extract_gfs_
!
!EOP
!-------------------------------------------------------------------------

  type GOCART_State
     private
     type(Chem_Registry), pointer :: chemReg => null()
     type(Aero_GridComp), pointer :: gcChem => null()
     type(Chem_Bundle), pointer   :: w_c    => null()
   end type GOCART_State

  type GOCART_WRAP
     type (GOCART_State), pointer :: PTR => null()
  end type GOCART_WRAP

CONTAINS


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: GOCART_SetServices --- Sets IRF services for GOCART Grid Component
!
! !INTERFACE:

   subroutine GOCART_SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, intent(out)               :: RC  ! return code
!    integer, optional                  :: RC  ! return code

! !DESCRIPTION: Sets Initialize, Run and Finalize services. 
!
! !REVISION HISTORY:
!
!  25feb2005  da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------


!   ErrLog Variables
!   ----------------
    character(len=ESMF_MAXSTR)      :: IAm = 'GOCART_SetServices'
    integer                         :: STATUS
    character(len=ESMF_MAXSTR)      :: COMP_NAME, answer

!   Local derived type aliases
!   --------------------------
    type (ESMF_Config)              :: CF
    type (GOCART_State), pointer  :: state   ! internal, that is
    type (GOCART_wrap)            :: wrap
    type(Chem_Registry), pointer  :: r

    integer                       :: n, nq, i_XX, j_XX
    logical                       :: GOCART_OWNS_TRACERS   

!                              ------------

    RC = 0


!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, CONFIG=CF, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = TRIM(COMP_NAME) // '::' // TRIM(Iam)

!   Get GFS parameters from GC and CF (Sarah Lu)
!   -----------------------------------------
    call extract_gfs_ ( gc, GOCART_OWNS_TRACERS, STATUS )
    VERIFY_(STATUS)

!   Wrap internal state for storing in GC; rename legacyState
!   -------------------------------------
    allocate ( state, stat=STATUS )
    VERIFY_(STATUS)
    wrap%ptr => state
 
!   Start by loading the Chem Registry
!   ----------------------------------
    allocate ( state%chemReg )
    state%chemReg = Chem_RegistryCreate ( STATUS )
    VERIFY_(STATUS)

    r => state%chemReg   ! short hand


!                       ------------------------
!                       ESMF Functional Services
!                       ------------------------

!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
     if ( r%doing_GOCART ) then

        IF(MAPL_AM_I_ROOT()) THEN
         PRINT *, TRIM(Iam)//': ACTIVE'
         PRINT *,' '
         CALL Chem_RegistryPrint(state%chemReg)
        END IF

        if ( GOCART_OWNS_TRACERS ) then
           call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETINIT,  InitializeSingle_, &
                RC=STATUS)
           VERIFY_(STATUS)
        else
           call ESMF_GridCompSetEntryPoint ( GC, ESMF_SETINIT,  Initialize1_, &
                                             PHASE=1, RC=STATUS)
           VERIFY_(STATUS)
           call ESMF_GridCompSetEntryPoint ( GC, ESMF_SETINIT,  Initialize2_, &
                                             PHASE=2, RC=STATUS)
           VERIFY_(STATUS)
        end if

        call MAPL_GridCompSetEntryPoint ( GC,  ESMF_SETRUN,  Run_,        &
             RC=STATUS)
        VERIFY_(STATUS)
        
        call MAPL_GridCompSetEntryPoint ( GC,  ESMF_SETFINAL,  Finalize_,  &
             RC=STATUS)
        VERIFY_(STATUS)
        
!       Store internal state in GC
!       --------------------------
        call ESMF_UserCompSetInternalState ( GC, 'GOCART_state', wrap, STATUS )
        VERIFY_(STATUS)

     else

        if (MAPL_AM_I_ROOT()) &
        print *, trim(Iam)//': NOT ACTIVE, defaulting to Generic No-op stubs'

     endif


!                         ------------------
!                         GEOS Data Services
!                         ------------------

!  NOTE: For now, always define import state to avoid breaking connectivities.

!BOP
!
! !IMPORT STATE:

!    3-D Quantities
!    --------------    

!    delp: derive from this
!    ----------------------
 
    call MAPL_AddImportSpec(GC,                                    &
         SHORT_NAME = 'PLE',                                       &
         LONG_NAME  = 'air_pressure',                              &
         UNITS      = 'Pa',                                        &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT    = MAPL_UNDEF,                                  &
         DIMS       =  MAPL_DimsHorzVert,                          &
         VLOCATION  =  MAPL_VLocationEdge,                         &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

!   Height at the edges
!   -------------------
    call MAPL_AddImportSpec(GC,                               &
         SHORT_NAME = 'ZLE',                                       &
         LONG_NAME  = 'geopotential_height',                       &
         UNITS      = 'm',                                         &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT            = MAPL_UNDEF,                          &
         DIMS       =  MAPL_DimsHorzVert,                          &
         VLOCATION  =  MAPL_VLocationEdge,                         &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

!    AIRDENS: it would be nice if we could add this
!    ----------------------------------------------
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'AIRDENS',                           &
        LONG_NAME          = 'air_density',                       &
        UNITS              = 'kg/m^3',                            &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT            = MAPL_UNDEF,                          &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,       RC=STATUS  )
     VERIFY_(STATUS)

!    CLOUD
!    -----
     call MAPL_AddImportSpec(GC,                              &
         SHORT_NAME='FCLD'  ,                                      &
         LONG_NAME ='Cloud fraction for radiation',                &
         UNITS     ='1',                                           &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT   = MAPL_UNDEF,                                   &
         DIMS      = MAPL_DimsHorzVert,                            &
         VLOCATION = MAPL_VLocationCenter,              RC=STATUS  )
     VERIFY_(STATUS)

!    DQCOND: this is Q moist tendency (REVISE)
!    ----------------------------------------
     call MAPL_AddImportSpec(GC,                              &
         SHORT_NAME='DQDT',                                        &
         LONG_NAME ='Q tendency - moist physics',                  &
         UNITS     ='kg/kg/s',                                     &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT   = MAPL_UNDEF,                                   &
         DIMS      = MAPL_DimsHorzVert,                            &
         VLOCATION = MAPL_VLocationCenter,              RC=STATUS  )
    VERIFY_(STATUS)

!   T
!   -
    call MAPL_AddImportSpec ( gc,                             &
         SHORT_NAME = 'T',                                         &
         LONG_NAME  = 'air_temperature',                           &
         UNITS      = 'K',                                         &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT    = MAPL_UNDEF,                                  &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
     VERIFY_(STATUS)

!   U
!   -
    call MAPL_AddImportSpec ( gc,                             &
         SHORT_NAME = 'U',                                         &
         LONG_NAME  = 'eastward_wind',                             &
         UNITS      = 'm s-1',                                     &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT    = MAPL_UNDEF,                                  &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
     VERIFY_(STATUS)

!   V
!   -
    call MAPL_AddImportSpec ( gc,                             &
         SHORT_NAME = 'V',                                         &
         LONG_NAME  = 'northward_wind',                            &
         UNITS      = 'm s-1',                                     &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT    = MAPL_UNDEF,                                  &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
     VERIFY_(STATUS)

!   Ozone from PCHEM for CFC-12 photolysis
!   --------------------------------------
    call MAPL_AddImportSpec ( gc,                             &
         SHORT_NAME = 'O3',                                        &
         LONG_NAME  = 'ozone_mass_mixing_ratio',                   &
         UNITS      = 'kg/kg',                                     &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT    = MAPL_UNDEF,                                  &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
     VERIFY_(STATUS)

!   RH: is this between 0 and 1 or between 0 and 100?
!   -------------------------------------------------
    call MAPL_AddImportSpec(GC,                               &
         SHORT_NAME='RH2',                                         &
         LONG_NAME ='Rel_Hum_after_moist',                         &
         UNITS     ='1',                                           &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT   = MAPL_UNDEF,                                   &
         DIMS      = MAPL_DimsHorzVert,                            &
         VLOCATION = MAPL_VLocationCenter,              RC=STATUS  )
    VERIFY_(STATUS)


!    2-D Quantities
!    --------------    

!    TROPP - Connectivity from SDYN to PHYS is TROPP_BLENDED to TROPP
!    ----------------------------------------------------------------
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'TROPP',                        &
        LONG_NAME          = 'tropopause_pressure_based_on_blended_estimate', &
        UNITS              = 'Pa',                                &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT            = MAPL_UNDEF,                          &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!    LWI
!    ---
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'LWI',                               &
        LONG_NAME          = 'land-ocean-ice_mask',               &
        UNITS              = '1',                                 &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT            = MAPL_UNDEF,                          &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!   PBL 
!   ---
    call MAPL_AddImportSpec(GC,                               &
         SHORT_NAME='ZPBL',                                        &
         LONG_NAME ='Planetary boundary layer height',             &
         UNITS     ='m',                                           &
         PRECISION  = KIND(0.0),                                   &
         DIMS      = MAPL_DimsHorzOnly,                            &
         VLOCATION = MAPL_VLocationNone,              RC=STATUS  )
    VERIFY_(STATUS)


!    FRACLAKE
!    --------
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'FRLAKE',                            &
        LONG_NAME          = 'fraction_of_lake',                  &
        UNITS              = '1',                                 &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT            = MAPL_UNDEF,                          &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!    FRACI
!    -----
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'FRACI',                             &
        LONG_NAME          = 'ice_covered_fraction_of_tile',      &
        UNITS              = '1',                                 &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT            = MAPL_UNDEF,                          &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!    GWETTOP
!    -------
     call MAPL_AddImportSpec(GC                             ,&
        SHORT_NAME         = 'WET1'                              ,&
        LONG_NAME          = 'surface_soil_wetness'              ,&
        UNITS              = '1'                                 ,&
        PRECISION  = KIND(0.0),                                   &
        DEFAULT            = MAPL_UNDEF,                          &
        DIMS               = MAPL_DimsHorzOnly                   ,&
        VLOCATION          = MAPL_VLocationNone                  ,&
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!    LAI
!    ---
     call MAPL_AddImportSpec(GC                             ,&
        SHORT_NAME         = 'LAI'                               ,&
        LONG_NAME          = 'leaf_area_index'                   ,&
        UNITS              = '1'                                 ,&
        PRECISION  = KIND(0.0),                                   &
        DEFAULT   = MAPL_UNDEF,                                   &
        DIMS               = MAPL_DimsHorzOnly                   ,&
        VLOCATION          = MAPL_VLocationNone                  ,&
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!    This could be useful,  but it is not needed now
!    -----------------------------------------------
     call MAPL_AddImportSpec(GC                             ,&
        SHORT_NAME         = 'GRN'                               ,&
        LONG_NAME          = 'greeness_fraction'                 ,&
        UNITS              = '1'                                 ,&
        PRECISION  = KIND(0.0),                                   &
        DEFAULT   = MAPL_UNDEF,                                   &
        DIMS               = MAPL_DimsHorzOnly                   ,&
        VLOCATION          = MAPL_VLocationNone                  ,&
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!   PRECC: I hope this is defined over oceans
!   -----------------------------------------
    call MAPL_AddImportSpec(GC,                               &
         SHORT_NAME='CN_PRCP',                                     &
         LONG_NAME ='Surface Conv. rain flux needed by land',      &
         UNITS     ='kg/m^2/s',                                    &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT   = MAPL_UNDEF,                                   &
         DIMS      = MAPL_DimsHorzOnly,                            &
         VLOCATION = MAPL_VLocationNone,                RC=STATUS  )
    VERIFY_(STATUS)

!   PRECL: Non-convective precip, provided by Cinderella
!   ----------------------------------------------------
    call MAPL_AddImportSpec(GC,                               &
         SHORT_NAME='NCN_PRCP',                                    &
         LONG_NAME ='Non-convective precipitation',                &
         UNITS     ='kg/m^2/s',                                    &
         PRECISION  = KIND(0.0),                                   &
         DEFAULT   = MAPL_UNDEF,                                   &
         DIMS      = MAPL_DimsHorzOnly,                            &
         VLOCATION = MAPL_VLocationNone,                RC=STATUS  )
    VERIFY_(STATUS)

!    PS -- from where???
!    ----
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'PS',                                &
        LONG_NAME          = 'surface_pressure',                  &
        UNITS              = 'Pa',                                &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT   = MAPL_UNDEF,                                   &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!    SHFX (pos is up) - why not evap, Ri, ???
!    ----
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'SH',                                &
        LONG_NAME          = 'sensible_heat_flux_from_turbulence',&
        UNITS              = 'W m-2',                             &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT   = MAPL_UNDEF,                                   &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!    TA -- Surface Air Temperature
!    ----
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'TA',                                &
        LONG_NAME          = 'surface_temperature_from_surface',  &
        UNITS              = 'K',                                 &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT   = MAPL_UNDEF,                                   &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!    TSOIL1, from SURFACE
!    --------------------
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'TSOIL1',                            &
        LONG_NAME          = 'soil_temperatures_layer_1',         &
        UNITS              = 'K',                                 &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT   = MAPL_UNDEF,                                   &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!    U10M
!    ----
     call MAPL_AddImportSpec(GC,         &
       SHORT_NAME = 'U10M',                   &
       LONG_NAME  = '10-meter_eastward_wind', &
       UNITS      = 'm s-1',                  &
       DEFAULT   = MAPL_UNDEF,                &
       DIMS       = MAPL_DimsHorzOnly,        &
       PRECISION  = KIND(0.0),                &
       VLOCATION  = MAPL_VLocationNone,       &
       RC         = STATUS  )
     VERIFY_(STATUS)

!   V10M
!   ----
    call MAPL_AddImportSpec(GC,           &
       SHORT_NAME = 'V10M',                    &
       LONG_NAME  = '10-meter_northward_wind', &
       UNITS      = 'm s-1',                   &
       PRECISION  = KIND(0.0),                 &
       DEFAULT   = MAPL_UNDEF,                 &
       DIMS       = MAPL_DimsHorzOnly,         &
       VLOCATION  = MAPL_VLocationNone,        &
       RC=STATUS  )
    VERIFY_(STATUS)

!   USTAR
!   -----
    call MAPL_AddImportSpec(GC,                              &
        SHORT_NAME         = 'USTAR',                             &
        LONG_NAME          = 'surface_velocity_scale',            &
        UNITS              = 'm s-1',                             &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT            = MAPL_UNDEF,                          &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
    VERIFY_(STATUS)

!   Z0H
!   ---
    call MAPL_AddImportSpec(GC,                              &
        SHORT_NAME         = 'Z0H',                               &
        LONG_NAME          = 'surface_roughness_for_heat',        &
        UNITS              = 'm',                                 &
        PRECISION  = KIND(0.0),                                   &
        DEFAULT            = MAPL_UNDEF,                          &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!   When GOCART does own its tracers, it must come in a
!   Bundle within the import state
!   ----------------------------------------------------
    if ( .not. GOCART_OWNS_TRACERS ) then
       call MAPL_AddImportSpec(GC,                                &
        SHORT_NAME         = 'iAERO',                             &
        LONG_NAME          = 'aerosol_mass_mixing_ratios',        &
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        DATATYPE           = MAPL_BundleItem,                     &
                                                       __RC__     )
    end if



if ( r%doing_GOCART ) then


! !INTERNAL STATE:

!
!  NOTES: 
!    1)  vtitle as it stands is as the CF definition of long name.
!        I may need to create a "standard name" in chemReg and pass
!        this to GEOS Generic
!    2)  Host model MUST provide convective transport as well
!

    nq = r%nq     ! total number of chemical tracers
    
!   Loop over all constituents on registry
!   --------------------------------------
   if ( GOCART_OWNS_TRACERS ) then 

     do n = r%i_GOCART, r%j_GOCART 

!      Aerosol Tracers to be transported
!      ---------------------------------
       call MAPL_AddInternalSpec(GC,                         &
               SHORT_NAME  = trim(COMP_NAME)// '::'               &
                          // trim(r%vname(n)),                    &
               LONG_NAME   = r%vtitle(n),                         &
               UNITS       = r%vunits(n),                         &     
               PRECISION   = KIND(0.0),                           &
               FRIENDLYTO  = 'DYNAMICS:TURBULENCE:MOIST',         &
               DIMS        = MAPL_DimsHorzVert,                   &
               VLOCATION   = MAPL_VLocationCenter,     RC=STATUS  )
          VERIFY_(STATUS)

     end do

  end if

!   This bundle is needed by radiation - It will contain the 
!   basically the same as the internal state
!   --------------------------------------------------------
    call MAPL_AddExportSpec(GC,                                 &
        SHORT_NAME         = 'AERO',                              &
        LONG_NAME          = 'aerosol_mass_mixing_ratios',        &
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        DATATYPE           = MAPL_BundleItem,                     &
                                                       RC=STATUS  )
    VERIFY_(STATUS)

! !EXPORT STATE:

   if ( r%doing_O3 ) then
#       include "O3_ExportSpec___.h"
   endif

   if ( r%doing_DU ) then
#       include "DU_ExportSpec___.h"
   endif

   if ( r%doing_SS ) then
#       include "SS_ExportSpec___.h"
   endif

   if ( r%doing_SU ) then
#       include "SU_ExportSpec___.h"
   endif

   if ( r%doing_BC ) then
#       include "BC_ExportSpec___.h"
   endif

   if ( r%doing_OC ) then
#       include "OC_ExportSpec___.h"
   endif

   if ( r%doing_CO ) then
#       include "CO_ExportSpec___.h"
   endif

   if ( r%doing_CO2 ) then
#       include "CO2_ExportSpec___.h"
   endif

   if ( r%doing_CFC ) then
#       include "CFC_ExportSpec___.h"
   endif

   if ( r%doing_Rn ) then
#       include "Rn_ExportSpec___.h"
   endif

!!!   if ( r%doing_CARMA ) then
!!!#       include "CARMA_ExportSpec___.h"
!!!   endif


!EOP

!   Set the Profiling timers
!   ------------------------
    call MAPL_TimerAdd ( GC, name = "RUN", RC=STATUS )
    VERIFY_(STATUS)

end if ! doing GOCART


!   Generic Set Services
!   --------------------
    call MAPL_GenericSetServices ( GC, RC=STATUS )
    VERIFY_(STATUS)

!   All done
!   --------

    RETURN_(ESMF_SUCCESS)
  
  end subroutine GOCART_SetServices

!
!  In order to couple with the 2 phase-initialization required by GFS,
!  we create simple wrappers around the Initialize method.
!
   subroutine InitializeSingle_ ( gc, impChem, expChem, clock, rc)
     implicit NONE
     type(ESMF_Clock),  intent(inout) :: clock      ! The clock
     type(ESMF_GridComp), intent(inout)  :: gc      ! Grid Component
     type(ESMF_State), intent(inout) :: impChem     ! Import State
     type(ESMF_State), intent(inout) :: expChem     ! Export State
     integer, intent(out) ::  rc                    ! Error return code
     call Initialize__ ( gc, impChem, expChem, clock, 0, rc)
   end subroutine InitializeSingle_

   subroutine Initialize1_ ( gc, impChem, expChem, clock, rc)
     implicit NONE
     type(ESMF_Clock),  intent(inout) :: clock      ! The clock
     type(ESMF_GridComp), intent(inout)  :: gc      ! Grid Component
     type(ESMF_State), intent(inout) :: impChem     ! Import State
     type(ESMF_State), intent(inout) :: expChem     ! Export State
     integer, intent(out) ::  rc                    ! Error return code
     call Initialize__ ( gc, impChem, expChem, clock, 1, rc)
   end subroutine Initialize1_

   subroutine Initialize2_ ( gc, impChem, expChem, clock, rc)
     implicit NONE
     type(ESMF_Clock),  intent(inout) :: clock      ! The clock
     type(ESMF_GridComp), intent(inout)  :: gc      ! Grid Component
     type(ESMF_State), intent(inout) :: impChem     ! Import State
     type(ESMF_State), intent(inout) :: expChem     ! Export State
     integer, intent(out) ::  rc                    ! Error return code
     call Initialize__ ( gc, impChem, expChem, clock, 2, rc)
   end subroutine Initialize2_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Initialize_ --- Initialize Aero_GridComp (ESMF)
!
! !INTERFACE:
!

   subroutine Initialize__ ( gc, impChem, expChem, clock, option, rc)

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: clock      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: gc      ! Grid Component
   type(ESMF_State), intent(inout) :: impChem     ! Import State
   type(ESMF_State), intent(inout) :: expChem     ! Export State
   integer, intent(in)  ::  option                ! Controls multi-phase options
                                                  ! 0 - single phase (as in GEOS-5)
                                                  ! 1 - phase 1 out of 2 (as in GFS)
                                                  ! 2 - phase 2 out of 2 (as in GFS)
   integer, intent(out) ::  rc                    ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 

! !DESCRIPTION: This is the lowe level Initialize routine. Notice that it is not
!               a bona-fide ESMF component; you should register the wrapper
! rotines InitializeSingle_(),  Initialize1_() or Initialize2_(). This device is
! needed because the GFS requires a 2-phase initialization due to the fact that
! GOCART does now owns the tracers in that case.
!
! !REVISION HISTORY:
!
!  27Feb2005 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

!  ErrLog Variables
!  ----------------
   character(len=ESMF_MAXSTR)      :: IAm = 'Initialize_'
   integer                         :: STATUS
   character(len=ESMF_MAXSTR)      :: COMP_NAME

   type(Chem_Registry), pointer    :: chemReg
   type(Aero_GridComp), pointer    :: gcChem      ! Grid Component
   type(Chem_Bundle), pointer      :: w_c         ! Chemical tracer fields     
   integer                         :: nymd, nhms  ! time
   real                            :: cdt         ! chemistry timestep (secs)

   type(ESMF_Grid)                 :: grid        
 
   integer                         :: i1=1, i2, ig=0, im  ! dist grid indices
   integer                         :: j1=1, j2, jg=0, jm  ! dist grid indices
   integer                         :: km, nq              ! dist grid indices
   integer                         :: n, dims(3), l

   type(ESMF_Config)               :: CF
   character(len=ESMF_MAXSTR)      :: diurnal_bb

   type(Chem_Array), pointer       :: q(:)          ! array of pointers
   type(MAPL_MetaComp), pointer :: ggState      ! GEOS Generic State
   type(ESMF_State)                 :: internal
   type(ESMF_Field)                 :: field
   type(ESMF_FieldBundle)           :: Bundle, iBundle
   type(MAPL_VarSpec), pointer      :: InternalSpec(:)

   real(ESMF_KIND_R4), pointer, dimension(:,:)  :: LATS
   real(ESMF_KIND_R4), pointer, dimension(:,:)  :: LONS

   character(len=ESMF_MAXSTR)       :: short_name, answer
   logical                          :: GOCART_OWNS_TRACERS

   real, parameter :: one = 1.0

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
   VERIFY_(STATUS)
   Iam = trim(COMP_NAME) // '::' // 'Initialize_'

!  Get my internal MAPL_Generic state
!  -----------------------------------
   call MAPL_GetObjectFromGC ( GC, ggState, RC=STATUS)
   VERIFY_(STATUS)

!                                --------
!                                Phase I
!                                --------
!   Initialize GEOS Generic
!   ------------------------
    if ( option==0 .OR. option==1 ) then
       call MAPL_GenericInitialize ( gc, impChem, expChem, clock,  RC=STATUS )
       VERIFY_(STATUS)
    end if

!   Stop here if first phase of a 2-phase initialization
!   ----------------------------------------------------
    if ( option==1 )  then
         RETURN_(ESMF_SUCCESS)
    end if

!                                --------
!                                Phase II
!                                --------

!  Get pre-ESMF parameters from gc and clock
!  -----------------------------------------
   call extract_ ( gc, clock, chemReg, gcChem, w_c, nymd, nhms, cdt, STATUS )
   VERIFY_(STATUS)

!  Get GFS parameters from gc and cf (Sarah Lu)
!  -----------------------------------------
   call extract_gfs_ ( gc, GOCART_OWNS_TRACERS, STATUS, &
                       impChem=impChem, cdt=cdt )
   VERIFY_(STATUS)

!  Create Chem Bundle
!  ------------------
   call ESMF_GridCompGet ( GC, grid=grid, rc=STATUS)
   VERIFY_(STATUS)

   call MAPL_GridGet ( grid, globalCellCountPerDim=DIMS, RC=STATUS)
   VERIFY_(STATUS)

   im = dims(1)
   jm = dims(2)
   nq = chemReg%nq

   call ESMF_GridGet(GRID, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        computationalCount=DIMS, RC=STATUS)
   VERIFY_(STATUS)

!  Associate the Internal State fields with our legact state 
!  (when we own the tracers)
!  ---------------------------------------------------------
   call MAPL_Get ( ggSTATE, &
        LONS      = LONS,   &
        LATS      = LATS,   &
        RC=STATUS  )
   VERIFY_(STATUS)
   if ( GOCART_OWNS_TRACERS ) then
      call MAPL_Get ( ggSTATE, INTERNALSPEC=InternalSpec, &
           INTERNAL_ESMF_STATE=internal, &
           RC=STATUS  )
      VERIFY_(STATUS)
   end if

! Local sizes of three dimensions
!--------------------------------
   i2 = dims(1)
   j2 = dims(2)
   km = dims(3)

!  Initalize the legacy state but do not allocate memory for arrays
!  ----------------------------------------------------------------
   call Chem_BundleCreate_ ( chemReg, i1, i2, ig, im, j1, j2, jg, jm, km,  &
                             w_c, lon=one*LONS(:,1), lat=one*LATS(1,:), &
                             skipAlloc=.true., rc=STATUS )
   VERIFY_(STATUS)

   w_c%grid_esmf = grid  ! Will need this for I/O later

!   Check whether to de-activate diurnal biomass burning (default is *on*)
!   ----------------------------------------------------------------------
    call ESMF_ConfigGetAttribute ( CF, diurnal_bb, Label="DIURNAL_BIOMASS_BURNING:", &
                                   default="yes",  RC=STATUS )
    VERIFY_(STATUS)

    if ( diurnal_bb(1:3) .eq. 'yes' .or. diurnal_bb(1:3) .eq. 'YES' .or. &
         diurnal_bb(1:3) .eq. 'Yes' ) then
         if (MAPL_AM_I_ROOT()) print *, trim(Iam)//': Diurnal Biomass Burning is ON'
         w_c%diurnal_bb = .true.
    else
         if (MAPL_AM_I_ROOT()) print *, trim(Iam)//': Diurnal Biomass Burning is OFF'
         w_c%diurnal_bb = .false.
    endif

!  Allocate these because they are not friendly tracers
!  ----------------------------------------------------
   allocate(w_c%delp(i1:i2,j1:j2,km), &
            w_c%rh(i1:i2,j1:j2,km), &
            stat=STATUS)
   VERIFY_(STATUS)

!  When we own the tracers, w_c is associated with our internal state
!  ------------------------------------------------------------------
   if ( GOCART_OWNS_TRACERS ) then

      ASSERT_ ( size(InternalSpec) == chemReg%n_GOCART )

      do L = 1, size(InternalSpec)

         call MAPL_VarSpecGet ( InternalSpec(L),          &
              SHORT_NAME = short_name,  &
              RC=STATUS )
         VERIFY_(STATUS)
         
         N = chemReg%i_GOCART + L - 1
         call MAPL_GetPointer ( internal, NAME=short_name, ptr=w_c%qa(N)%data3d, &
              rc = STATUS )
         VERIFY_(STATUS)

      end do

!  Otherwise, associate w_c with tracers in import iAERO bundle
!  ------------------------------------------------------------
   else

      call ESMF_StateGet(impChem, 'iAERO', iBundle, RC=STATUS)
      VERIFY_(STATUS)

      do L = 1, chemReg%n_GOCART

         N = chemReg%i_GOCART + L - 1

         call ESMF_FieldBundleGet(iBundle, NAME=chemReg%vname(N), &
                                  FIELD=FIELD, rc = STATUS )
         VERIFY_(STATUS)
         call ESMF_FieldGet(FIELD, localDE=0, farray=w_c%qa(N)%data3d, rc=rc)
         VERIFY_(STATUS)

      end do

   end if


#ifdef PRINT_STATES

   if (MAPL_AM_I_ROOT()) then
      if ( GOCART_OWNS_TRACERS ) then
         print *, trim(Iam)//': INTERNAL State during Initialize():' 
         call ESMF_StatePrint ( internal )
      end if
      print *, trim(Iam)//': IMPORT   State during Initialize():'
      call ESMF_StatePrint ( impChem  )
      print *, trim(Iam)//': EXPORT   State during Initialize():' 
      call ESMF_StatePrint ( expChem  )
    end if

#endif

!   Call Legacy Initialize
!   ----------------------
    call Aero_GridCompInitialize ( gcChem, w_c, impChem, expChem, &
                                   nymd, nhms, cdt, STATUS )
    VERIFY_(STATUS)

!   Only at this point we have the scavenging coefficients filled,
!    so annotate the convection friendly internal state
!   Note: Move this to AddInternalSpec but first we need to have
!         the subcomponents as bonafide ESMF components
!   --------------------------------------------------------------
    if ( GOCART_OWNS_TRACERS ) then

       do n = ChemReg%i_GOCART, ChemReg%j_GOCART 

          call ESMF_StateGet ( internal,                     &
                            trim(COMP_NAME) // '::'//     &
                            trim(ChemReg%vname(n)),       &
                            field, rc=STATUS )
          VERIFY_(STATUS)

          call ESMF_AttributeSet (field, NAME  = "ScavengingFractionPerKm", &
                               VALUE = ChemReg%fscav(n),          &
                               RC = STATUS )
          VERIFY_(STATUS)

       end do

!!! Add ChemReg%fscav to impChem so GFS can use it (Sarah Lu)
    else

       do n = ChemReg%i_GOCART, ChemReg%j_GOCART 

        call ESMF_FieldBundleGet(iBundle,NAME=trim(ChemReg%Vname(n)), &
                  FIELD=FIELD, rc = STATUS )
        VERIFY_(STATUS)

        call ESMF_AttributeSet (FIELD, NAME  = "ScavengingFractionPerKm", &
                                VALUE = ChemReg%fscav(n),          &
                                RC = STATUS )
        VERIFY_(STATUS)
       end do

!!!
    end if

!


!   Now that the internal state is nice and ready add its contents to the
!   AERO bundle needed by radiation
!   ---------------------------------------------------------------------
    call ESMF_StateGet(expChem, 'AERO', bundle, RC=STATUS)
    VERIFY_(STATUS)
    do n = ChemReg%i_GOCART, ChemReg%j_GOCART 

       short_name = uppercase(trim(ChemReg%vname(n)))

       if ( short_name(1:2) .eq. 'DU'        .or. &
            short_name(1:2) .eq. 'SS'        .or. &
            short_name(1:8) .eq. 'OCPHOBIC'  .or. &
            short_name(1:8) .eq. 'OCPHILIC'  .or. &
            short_name(1:2) .eq. 'BC'        .or. &
! -- add gaseous species needed by GFS (Sarah Lu)
            short_name(1:3) .eq. 'DMS'       .or. &
            short_name(1:3) .eq. 'SO2'       .or. &
            short_name(1:3) .eq. 'MSA'       .or. &

            short_name(1:3) .eq. 'SO4'            ) then

          if ( GOCART_OWNS_TRACERS ) then

             call ESMF_StateGet ( INTERNAL,                     &
                  trim(COMP_NAME) // '::'//     &
                  trim(ChemReg%vname(n)),       &
                  FIELD, RC=STATUS )
             VERIFY_(STATUS)
             
          else

             call ESMF_FieldBundleGet(iBundle,NAME=trim(ChemReg%vname(n)), &
                  FIELD=FIELD, rc = STATUS )
             VERIFY_(STATUS)

          end if

          call ESMF_FieldBundleAdd ( BUNDLE, FIELD, RC=STATUS )
          VERIFY_(STATUS)

       end if

#ifdef PRINT_STATES

   if (MAPL_AM_I_ROOT()) then
       print *, trim(Iam)//': AERO Bundle during Initialize():' 
       call ESMF_FieldBundlePrint ( bundle )
   end if

#endif

    end do


    RETURN_(ESMF_SUCCESS)

  end subroutine Initialize__


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Run_ --- Runs Aero_GridComp (ESMF)
!
! !INTERFACE:
!

   subroutine Run_ ( gc, impChem, expChem, clock, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: clock      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: gc      ! Grid Component
   type(ESMF_State), intent(inout) :: impChem     ! Import State
   type(ESMF_State), intent(inout) :: expChem     ! Export State
   integer, intent(out) ::  rc                    ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  27Feb2005 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

!  ErrLog Variables
!  ----------------
   character(len=ESMF_MAXSTR)      :: IAm = 'Run_'
   integer                         :: STATUS
   character(len=ESMF_MAXSTR)      :: COMP_NAME

   type(Chem_Registry), pointer    :: chemReg
   type(Aero_GridComp), pointer    :: gcChem      ! Grid Component
   type(Chem_Bundle), pointer      :: w_c         ! Chemical tracer fields     
   integer                         :: nymd, nhms  ! time
   real                            :: cdt         ! chemistry timestep (secs)
   real, pointer                   :: var(:,:,:)
   integer                         :: n

   type(ESMF_Config)               :: CF
   type(ESMF_Grid)                 :: grid
   type(ESMF_Time)                 :: TIME

   type(MAPL_MetaComp), pointer :: ggState      ! GEOS Generic State

   real(ESMF_KIND_R4), pointer, dimension(:,:)  :: LATS
   real(ESMF_KIND_R4), pointer, dimension(:,:)  :: LONS

   type (MAPL_SunOrbit)            :: ORBIT
   real, allocatable, target       :: ZTH(:,:) ! can be R8
   real(ESMF_KIND_R4), allocatable :: r4ZTH(:,:)
   real(ESMF_KIND_R4), allocatable :: SLR(:,:)

   real, pointer                   :: rh2(:,:,:)
   integer                         :: in, jn
   integer                         :: iLeft, iRight, jBottom, jTop

   logical                         :: GOCART_OWNS_TRACERS

   REAL :: dayOfYear
   REAL(ESMF_KIND_R8) :: dayOfYear_r8
!                               ---

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
   VERIFY_(STATUS)
   Iam = trim(COMP_NAME) // '::' // 'Run_'

!  Get my internal MAPL_Generic state
!  -----------------------------------
   call MAPL_GetObjectFromGC ( GC, ggState, RC=STATUS)
   VERIFY_(STATUS)

!  Get parameters from generic state.
!  ----------------------------------
   call MAPL_Get(ggState,           &
        LONS      = LONS,                       &
        LATS      = LATS,                       &
        ORBIT     = ORBIT,                      &
        RC=STATUS )
   VERIFY_(STATUS)

   allocate(r4ZTH(SIZE(LATS,1), SIZE(LATS,2)), STAT=STATUS)
   VERIFY_(STATUS)
   allocate(ZTH(SIZE(LATS,1), SIZE(LATS,2)), STAT=STATUS)
   VERIFY_(STATUS)
   allocate(SLR(SIZE(LATS,1), SIZE(LATS,2)), STAT=STATUS)
   VERIFY_(STATUS)

!  Update solar zenith angle
!  --------------------------
   call MAPL_SunGetInsolation(LONS, LATS,  &
        ORBIT, r4ZTH, SLR, CLOCK=CLOCK,      &
        RC=STATUS  )
   VERIFY_(STATUS)

!  Get pre-ESMF parameters from gc and clock
!  -----------------------------------------
   call extract_ ( gc, clock, chemReg, gcChem, w_c, nymd, nhms, cdt, rc=status )
   VERIFY_(STATUS)

!  Get GFS parameters from gc and cf (Sarah Lu)
!  -----------------------------------------
   call extract_gfs_ ( gc, GOCART_OWNS_TRACERS, STATUS, &
                       impChem=impChem, cdt = cdt )
   VERIFY_(STATUS)

!  Set pointers for sine/cosine zenith angle
!  -----------------------------------------

!  w_c%sinz => ...
   ZTH = r4ZTH
   w_c%cosz => zth

!  Fill in delp
!  ------------
   call MAPL_GetPointer ( impChem, var, 'PLE', RC=STATUS)
   VERIFY_(STATUS)
   n = size(var,3)
   w_c%delp = var(:,:,1:n)-var(:,:,0:n-1)

!  Fill in RH
!  ----------
   call MAPL_GetPointer ( impChem, rh2, 'RH2', RC=STATUS)
   w_c%rh = 100. * rh2   ! like in GEOS-4
   VERIFY_(STATUS)

!  Make sure tracers remain positive
!  ---------------------------------
   in = size(w_c%delp,1);   jn = size(w_c%delp,2)

   do n = ChemReg%i_GOCART, ChemReg%j_GOCART 
      call Chem_UtilNegFiller ( w_c%qa(n)%data3d, w_c%delp, in, jn, &
                                qmin=tiny(1.0) )
   end do

!  Call pre-ESMF version
!  ---------------------
   call Aero_GridCompRun ( gcChem, w_c, impChem, expChem, &
                           nymd, nhms, cdt, STATUS )

   VERIFY_(STATUS)

   deallocate(SLR)
   deallocate(r4ZTH)

   RETURN_(ESMF_SUCCESS)

   end subroutine Run_


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Finalize_ --- Finalize Aero_GridComp (ESMF)
!
! !INTERFACE:
!

   subroutine Finalize_ ( gc, impChem, expChem, clock, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: clock      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: gc      ! Grid Component
   type(ESMF_State), intent(inout) :: impChem     ! Import State
   type(ESMF_State), intent(inout) :: expChem     ! Export State
   integer, intent(out) ::  rc                    ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  27Feb2005 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------


!  ErrLog Variables
!  ----------------
   character(len=ESMF_MAXSTR)      :: IAm = 'Finalize_'
   integer                         :: STATUS
   character(len=ESMF_MAXSTR)      :: COMP_NAME

   type(Chem_Registry), pointer    :: chemReg
   type(Aero_GridComp), pointer    :: gcChem      ! Grid Component
   type(Chem_Bundle), pointer      :: w_c         ! Chemical tracer fields     
   integer                         :: nymd, nhms  ! time
   real                            :: cdt         ! chemistry timestep (secs)

    type(GOCART_state), pointer  :: state
 
   logical                         :: GOCART_OWNS_TRACERS

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
   VERIFY_(STATUS)
   Iam = trim(COMP_NAME) // '::' // 'Finalize_'

!  Get pre-ESMF parameters from gc and clock
!  -----------------------------------------
   call extract_ ( gc, clock, chemReg, gcChem, w_c, nymd, nhms, cdt, STATUS, &
                   state = state )
   VERIFY_(STATUS)

!  Get GFS parameters from gc and cf (Sarah Lu)
!  -----------------------------------------
   call extract_gfs_ ( gc, GOCART_OWNS_TRACERS, STATUS, &
                       impChem=impChem, cdt = cdt )
   VERIFY_(STATUS)

!  Call pre-ESMF version
!  ---------------------
   call Aero_GridCompFinalize ( gcChem, w_c, impChem, expChem, &
                                nymd, nhms, cdt, STATUS )
   VERIFY_(STATUS)


!  Finalize GEOS Generic
!  ---------------------
!ALT: don not dealloc "forein objects"
   call MAPL_GenericFinalize ( gc, impChem, expChem, clock,  RC=STATUS )
   VERIFY_(STATUS)

!  Destroy Chem_Bundle
!  -------------------
   call Chem_BundleDestroy ( w_c, STATUS )
   VERIFY_(STATUS)

!  Destroy Chem_Registry
!  ---------------------
   call Chem_RegistryDestroy ( chemReg, STATUS ) 
   VERIFY_(STATUS)

!  Destroy Legacy state
!  --------------------
   deallocate ( state%chemReg, state%gcChem, state%w_c, stat = STATUS )
   VERIFY_(STATUS)

   RETURN_(ESMF_SUCCESS)

   end subroutine Finalize_


!.......................................................................

    subroutine extract_ ( gc, clock, chemReg, gcChem, w_c, nymd, nhms, cdt, &
                          rc, state )

    type(ESMF_GridComp), intent(INout)  :: gc
    type(ESMF_Clock), intent(in)     :: clock
    type(Chem_Registry), pointer     :: chemReg
    type(Aero_GridComp), pointer     :: gcChem
    type(Chem_Bundle), pointer       :: w_c
    integer, intent(out)             :: nymd, nhms
    real, intent(out)                :: cdt
    integer, intent(out)             :: rc
    type(GOCART_state), pointer, optional   :: state


    type(GOCART_state), pointer    :: myState

!   ErrLog Variables
!   ----------------
    character(len=ESMF_MAXSTR)      :: IAm
    integer                         :: STATUS
    character(len=ESMF_MAXSTR)      :: COMP_NAME

    type(ESMF_Time)      :: TIME
    type(ESMF_Config)    :: CF
    type(GOCART_Wrap)    :: wrap
    integer              :: IYR, IMM, IDD, IHR, IMN, ISC


!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // '::' // 'extract_'

    rc = 0

!   Get my internal state
!   ---------------------
    call ESMF_UserCompGetInternalState(gc, 'GOCART_state', WRAP, STATUS)
    VERIFY_(STATUS)
    myState => wrap%ptr
    if ( present(state) ) then
         state => wrap%ptr
    end if

!   This is likely to be allocated during initialize only
!   -----------------------------------------------------
    if ( .not. associated(myState%chemReg) ) then
         allocate ( myState%chemReg, stat=STATUS )
         VERIFY_(STATUS)
    end if
    if ( .not. associated(myState%gcChem) ) then
         allocate ( myState%gcChem, stat=STATUS )
         VERIFY_(STATUS)
    end if
    if ( .not. associated(myState%w_c) ) then
         allocate ( myState%w_c, stat=STATUS )
         VERIFY_(STATUS)
    end if

    chemReg => myState%chemReg
    gcChem  => myState%gcChem
    w_c     => myState%w_c

!   Get the configuration
!   ---------------------
    call ESMF_GridCompGet ( GC, CONFIG = CF, RC=STATUS )
    VERIFY_(STATUS)

!   Get time step
!   -------------
    call ESMF_ConfigGetAttribute ( CF, cdt, Label="RUN_DT:", RC=STATUS )
    VERIFY_(STATUS)

!   Need code to extract nymd(20050205), nhms(120000) from clock
!   ------------------------------------------

    call ESMF_ClockGet(CLOCK,currTIME=TIME,rc=STATUS)
    VERIFY_(STATUS)

    call ESMF_TimeGet(TIME ,YY=IYR, MM=IMM, DD=IDD, H=IHR, M=IMN, S=ISC, rc=STATUS)
    VERIFY_(STATUS)

    call MAPL_PackTime(NYMD,IYR,IMM,IDD)
    call MAPL_PackTime(NHMS,IHR,IMN,ISC)

    RETURN_(ESMF_SUCCESS)

   end subroutine extract_

!.......................................................................

    subroutine extract_gfs_ ( gc, GOCART_OWNS_TRACERS, rc, impChem, cdt)

    type(ESMF_GridComp), intent(inout)       :: gc          ! Grid Component
    logical, intent(out)                     :: GOCART_OWNS_TRACERS
    integer, intent(out)                     :: rc          ! Error return code
    type(ESMF_State), optional, intent(in)   :: impChem     ! Import State
    real, optional, intent(out)              :: cdt         ! Time step


!   ErrLog Variables
!   ----------------
    character(len=ESMF_MAXSTR)      :: IAm
    integer                         :: STATUS
    character(len=ESMF_MAXSTR)      :: COMP_NAME

    type(ESMF_Config)               :: CF

    character(len=ESMF_MAXSTR)      :: answer     
    real                            :: deltim


!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // '::' // 'extract_gfs_'

    rc = 0

!   Get the configuration
!   ---------------------
    call ESMF_GridCompGet ( GC, CONFIG = CF, RC=STATUS )
    VERIFY_(STATUS)

!   Determine from RC file if GOCART owns the aerosol tracers
!   NOTE: Under normal circunstances, GOCART owns its internal
!         state. However, when running under the GFS at NCEP, the
!   tracers are owned by Dynamics, so we cope with it.
!   -------------------------------------------------------------
    call ESMF_ConfigGetAttribute ( CF, answer, Label="GOCART_OWNS_TRACERS:", &
                                   default="yes",  __RC__ )
    if ( answer(1:3) .eq. 'yes' .or. answer(1:3) .eq. 'YES' .or. &
         answer(1:3) .eq. 'Yes' ) then
         if (MAPL_AM_I_ROOT()) print *, trim(Iam)//': GOCART owns its tracers (OK for GEOS-5)'
         GOCART_OWNS_TRACERS = .true.
    else
         if (MAPL_AM_I_ROOT()) print *, trim(Iam)//': GOCART does NOT own its tracers (OK for GFS)'
         GOCART_OWNS_TRACERS = .false.
    endif

!   Get time step
!   -------------
    if ( present ( cdt ) ) then
     if (  GOCART_OWNS_TRACERS ) then
      call ESMF_ConfigGetAttribute ( CF, cdt, Label="RUN_DT:", RC=STATUS )
      VERIFY_(STATUS)
     else
      CALL ESMF_AttributeGet(impChem, name = 'deltim',  &
                             value = deltim , rc=RC)
      cdt = deltim
     endif
    endif

    RETURN_(ESMF_SUCCESS)

   end subroutine extract_gfs_

end module GOCART_GridCompMod

