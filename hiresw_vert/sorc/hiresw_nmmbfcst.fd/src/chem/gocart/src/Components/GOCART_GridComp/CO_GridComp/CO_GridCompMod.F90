#include "MAPL_Generic.h"

!!! TO DO: Please revise Prologues!!!!

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  CO_GridCompMod --- CO Grid Component Class
!
! !INTERFACE:
!

   MODULE  CO_GridCompMod

! !USES:

   USE ESMF_Mod
   USE MAPL_Mod

   USE Chem_Mod 	     ! Chemistry Base Class
   USE Chem_StateMod	     ! Chemistry State
   USE Chem_ConstMod, only: grav
   USE Chem_UtilMod	     ! I/O

   USE m_inpak90	     ! Resource file management
   USE m_die, ONLY: die
   USE m_chars, ONLY: lowercase

   IMPLICIT NONE

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  CO_GridComp       ! Multiple instance CO object 
   PUBLIC  CO_GridComp1      ! Single instance CO object

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  CO_GridCompInitialize
   PUBLIC  CO_GridCompRun
   PUBLIC  CO_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the (pre-ESMF) CO Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  31May2005  Nielsen  Mods for 7 CO bins, 5 region masks
!  31May2005 da Silva  Seperate file for biomass emissions; option for
!                       daily templatable files
!  31May2005 da Silva  Moved reading of region mask to init, specified 
!                      fixed time.
!  17Oct2005     Bian  add biogenic emission and CH4 oxidation, two options 
!                      for updating emissions 
!  19dec2005 da Silva  Activated 3D diags for output
!  14Apr2006     Bian  Add CO tagged to fossil fuel, biofuel, biomass burning
!                      and biogenic
!    Oct2006     Bian  Evaluate total and tagged CO performace in GEOS4 system 
!                      with emissions and oxident fields described in 
!                      Bian et al., [2007]. The observations included GMD ground
!                      surface and aircraft measurements, TRACE-P aircraft 
!                      measurements, and satellite MOPITT and AIRS retrieves. 
!  01Aug2006 da Silva  Extensions for GEOS-5.
!  10Mar2008 da Silva  Multiple instances for ARCTAS.
!
!EOP
!-------------------------------------------------------------------------

  TYPE CO_GridComp1

        CHARACTER(LEN=255) :: name            ! generic name of the package
        CHARACTER(LEN=255) :: iname           ! instance name
        CHARACTER(LEN=255) :: rcfilen         ! resource file name
        CHARACTER(LEN=255) :: eFilen_biomass  ! Biomass emission
        CHARACTER(LEN=255) :: ffFilen         ! Fossil fuel 
        CHARACTER(LEN=255) :: bfFilen         ! Biofuel
        CHARACTER(LEN=255) :: pFilen
        CHARACTER(LEN=255) :: OHFileName
        CHARACTER(LEN=255) :: CH4FileName
        CHARACTER(LEN=255) :: maskFileName
        CHARACTER(LEN=255) :: regionsString   ! Comma-delimited string of regions

        INTEGER :: instance                   ! instance number

        INTEGER :: nymd_ff
        INTEGER :: nymd_bf
        INTEGER :: nymd_oh
        INTEGER :: nymd_ch4
        INTEGER :: nymd_p

        INTEGER :: BCnymd   ! Date of last emissions/prodction read
        REAL    :: BBconFac ! conversion factor of BB emissions to CO

        REAL, POINTER :: eCO_bioburn_(:,:)   ! molec/cm2/s  (before diurnal)
        REAL, POINTER :: eCO_bioburn(:,:)    ! molec/cm2/s
        REAL, POINTER :: eCO_biofuel(:,:)    ! molec/cm2/s
        REAL, POINTER :: eCO_fosfuel(:,:)    ! molec/cm2/s
        REAL, POINTER ::     eCO_iso(:,:)    ! mgC/m2/s, Earth surface
        REAL, POINTER ::     eCO_mon(:,:)    ! mgC/m2/s, Earth surface
        REAL, POINTER ::     eCO_mtn(:,:)    ! mgC/m2/s, Earth surface
        REAL, POINTER ::  regionMask(:,:)    ! regional mask

        REAL, POINTER ::      CH4(:,:,:)     ! CH4 mixing ratio (ppb)
        REAL, POINTER ::      OHnd(:,:,:)    ! OH number density (#/cm3)

        REAL, POINTER :: COsfcFlux(:,:)      ! CO surface flux kg m^-2 s^-1

  END TYPE CO_GridComp1

  TYPE CO_GridComp
     integer                     ::  n        ! number of instances 
     TYPE(CO_GridComp1), pointer ::  gcs(:)   ! instances
  END TYPE CO_GridComp

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO_GridCompInitialize --- Initialize CO_GridComp
!
! !INTERFACE:
!

   subroutine CO_GridCompInitialize ( gcCO, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(CO_GridComp), INTENT(INOUT) :: gcCO   ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the CO Grid Component. Multiple instance
!               version.
!
! !REVISION HISTORY:
!
!  27Feb2008  da Silva  Introduced multiple instances
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'CO_GridCompInitialize'
   CHARACTER(LEN=255) :: rcbasen = 'CO_GridComp'
   CHARACTER(LEN=255) :: name
   
   integer i, ier, n

!  Load resource file
!  ------------------
   CALL I90_loadf ( TRIM(rcbasen)//'.rc', ier )
   if ( ier .NE. 0 ) then
      rc = 10
      return
   end if

!  Parse resource file
!  -------------------
   CALL I90_label ( 'CO_instances:', ier )
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
   
!  We cannot have fewer instances than the number of
!   CO bins in the registry (it is OK to have less, though)
!  --------------------------------------------------------
   if ( n .LT. w_c%reg%n_CO ) then
        rc = 35
        return
   else if ( n .GT. w_c%reg%n_CO ) then
        if (MAPL_AM_I_ROOT()) &
        print *, trim(myname)// &
                 ': fewer CO bins than possible CO instances: ',&
                 n, w_c%reg%n_CO
   end if
   n = min(n,w_c%reg%n_CO )
   gcCO%n = n

!  Next allocate necessary memory
!  ------------------------------
   allocate ( gcCO%gcs(n), stat=ier )    
   if ( ier .NE. 0 ) then
      rc = 40
      return
   end if

!  Record name of each instance
!  ----------------------------
   CALL I90_label ( 'CO_instances:', ier )
   do i = 1, n
      CALL I90_gtoken( name, ier )
      if ( ier .NE. 0 ) then
         rc = 40
         return
      end if
                                            ! resource file name
      gcCO%gcs(i)%rcfilen = trim(rcbasen)//'---'//trim(name)//'.rc'
      gcCO%gcs(i)%instance = i              ! instance number 
      IF(TRIM(name) == "full" ) THEN
       gcCO%gcs(i)%iname = " "              ! blank instance name for full (1)
      ELSE
       gcCO%gcs(i)%iname = TRIM(name)       ! instance name for others
      END IF
   end do    

!  Next initialize each instance
!  -----------------------------
   do i = 1, gcCO%n
      IF(MAPL_AM_I_ROOT()) THEN
       PRINT *," "
       PRINT *,myname,": Initializing instance ",TRIM(gcCO%gcs(i)%iname)," [",gcCO%gcs(i)%instance,"]"
      END IF
      call CO_SingleInstance_ ( CO_GridCompInitialize1_, i, &
                                gcCO%gcs(i), w_c, impChem, expChem,  &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then
         rc = 1000+ier
         return
      end if
   end do

!  All done
!  --------
   CALL I90_FullRelease( ier )
   IF( ier /= 0 ) THEN
    PRINT *,myname,": I90_FullRelease not successful."
    rc = 40
   END IF


 end subroutine CO_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO_GridCompRun --- Run CO_GridComp
!
! !INTERFACE:
!

   subroutine CO_GridCompRun ( gcCO, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(CO_GridComp), INTENT(INOUT) :: gcCO   ! Grid Component
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

   do i = 1, gcCO%n
      call CO_SingleInstance_ ( CO_GridCompRun1_, i, &
                                gcCO%gcs(i), w_c, impChem, expChem, &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then
         rc = i * 1000+ier
         return
      end if
   end do

 end subroutine CO_GridCompRun


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO_GridCompFinalize --- Initialize CO_GridComp
!
! !INTERFACE:
!

   subroutine CO_GridCompFinalize ( gcCO, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(CO_GridComp), INTENT(INOUT) :: gcCO   ! Grid Component
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

   integer i, ier

   do i = 1, gcCO%n
      call CO_SingleInstance_ ( CO_GridCompFinalize1_, i, &
                                gcCO%gcs(i), w_c, impChem, expChem, &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then
         rc = i * 1000+ier
         return
      end if
   end do

   deallocate ( gcCO%gcs, stat=ier )    
   gcCO%n = -1

 end subroutine CO_GridCompFinalize

!--------------------------------------------------------------------------

!                      Single Instance Methods

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO_GridCompInitialize --- Initialize CO_GridComp
!
! !INTERFACE:
!

   subroutine CO_GridCompInitialize1_ ( gcCO, w_c, impChem, expChem, &
                                        nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(CO_GridComp1), INTENT(INOUT) :: gcCO   ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the CO Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  31May2005  Nielsen  Mods for 7 CO bins, 5 region masks
!  04Nov2005     Bian  CO tagged to 4 regions 
!                      (global, North America, South America, and Africa)
!                      for CR-AVE
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'CO_GridCompInitialize1'

   CHARACTER(LEN=255) :: rcfilen 

   INTEGER :: ios, j, n
   INTEGER, ALLOCATABLE :: ier(:)
   INTEGER :: i1, i2, im, j1, j2, jm, km
   INTEGER :: nTimes, begTime, incSecs
   INTEGER :: nbeg, nend, nymd1, nhms1
   LOGICAL :: NoRegionalConstraint 

   REAL :: limitN, limitS, radTODeg
   REAL, ALLOCATABLE :: var2d(:,:)

   rcfilen = gcCO%rcfilen
   gcCO%name = 'GEOS-5/GOCART Parameterized CO Package'
   gcCO%BCnymd = -1
   radTODeg = 57.2957795

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1
   i2 = w_c%grid%i2
   im = w_c%grid%im

   j1 = w_c%grid%j1
   j2 = w_c%grid%j2
   jm = w_c%grid%jm

   km = w_c%grid%km

   nbeg  = w_c%reg%i_CO
   nend  = w_c%reg%j_CO

!  It requires 1 bin
!  -----------------
   if ( nbeg /= nend ) then
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Must have only 1 bin at the single instance level"
      rc = 1
      return 
   end if

!  Allocate memory, etc
!  --------------------
   CALL INIT_()
   IF ( rc /= 0 ) RETURN

!  Load resource file
!  ------------------
   CALL I90_loadf ( TRIM(rcfilen), ier(1) )
   IF ( ier(1) .NE. 0 ) THEN
      CALL final_(10)
      RETURN
   END IF
   ier(:)=0

!  Parse resource file
!  -------------------
   CALL I90_label ( 'OH_concentration_filename:', ier(3) )
   CALL I90_gtoken( gcCO%OHFileName, ier(4) )

   CALL I90_label ( 'CO_biomass_emission_filename:', ier(5) )
   CALL I90_gtoken( gcCO%eFilen_biomass, ier(6) )

   CALL I90_label ( 'CO_fossilfuel_filename:', ier(7) )
   CALL I90_gtoken( gcCO%ffFilen, ier(8) )

   CALL I90_label ( 'CO_biofuel_filename:', ier(9) )
   CALL I90_gtoken( gcCO%bfFilen, ier(10) )

   CALL I90_label ( 'CO_biogenic_filename:', ier(11) )
   CALL I90_gtoken( gcCO%pFilen, ier(12) )

   CALL I90_label ( 'CO_biomass_emission_factor:', ier(13) )
   gcCO%BBconFac = i90_gfloat ( ier(14) )

   CALL I90_label ( 'CH4_concentration_filename:', ier(15) )
   CALL I90_gtoken( gcCO%CH4FileName, ier(16) )

   IF( ANY( ier(:) /= 0 ) ) THEN
    CALL final_(21)
    RETURN
   END IF

!  Check initial date of inventory emission/oxidant files.  These files are valid for a
!  particular YYYY or YYYYMMDD (if 1x year in file).  We need to request the correct date.
!  ---------------------------------------------------------------------------------------
   call Chem_UtilGetTimeInfo( gcCO%OHFileName, gcCO%nymd_oh, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcCO%ffFilen, gcCO%nymd_ff, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcCO%bfFilen, gcCO%nymd_bf, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcCO%pFilen, gcCO%nymd_p, &
                              begTime, nTimes, incSecs )
   call Chem_UtilGetTimeInfo( gcCO%CH4FileName, gcCO%nymd_ch4, &
                              begTime, nTimes, incSecs )
   ier(1) = gcCO%nymd_oh
   ier(2) = gcCO%nymd_ff
   ier(3) = gcCO%nymd_bf
   ier(4) = gcCO%nymd_p
   ier(5) = gcCO%nymd_ch4

   if( any(ier(1:5) < 0 ) ) then
     call final_(31)
     return
   endif

   ier(:)=0

!  Obtain geographical region mask
!  -------------------------------
   CALL I90_label ( 'CO_regions:', ier(1) )
   CALL I90_gtoken( gcCO%maskFileName, ier(2) )
   if( any(ier(1:2) < 0 ) ) then
     call final_(41)
     return
   endif
   ier(:)=0

   call Chem_UtilGetTimeInfo( gcCO%maskFileName, nymd1, &
                              begTime, nTimes, incSecs )
   if(nymd1 < 0) call final_(15)
   nhms1 = 120000
   CALL Chem_UtilMPread ( gcCO%maskFileName, 'REGION_MASK', nymd1, nhms1, &
   			  i1, i2, 0, im, j1, j2, 0, jm, 0, &
   			  var2d=gcCO%regionMask, grid=w_c%grid_esmf )

!  Grab the region string.
!  -----------------------
   call i90_label ( 'CO_regions_indices:', ier(1) )
   CALL I90_gtoken( gcCO%regionsString, ier(2) )
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
     gcCO%regionsString = "1"
     ALLOCATE(var2d(i1:i2,j1:j2), STAT=ios)
     IF(ios /= 0) THEN
      PRINT *,myname,": Unable to allocate var2d."
      CALL final_(62)
     END IF
     var2d(i1:i2,j1:j2) = 0.00

!  Within the latitude range specified, set land boxes to 1
!  --------------------------------------------------------
     DO j = j1,j2
      WHERE(gcCO%regionMask(i1:i2,j) > 0 .AND. &
            (limitS <= w_c%grid%lat(j)*radTODeg .AND. &
	     w_c%grid%lat(j)*radTODeg <= limitN) ) var2d(i1:i2,j) = 1.00
     END DO

!  Reset the region mask in gcCO
!  -----------------------------
     gcCO%regionMask(i1:i2,j1:j2) = var2d(i1:i2,j1:j2)

     DEALLOCATE(var2d, STAT=ios)
     IF(ios /= 0) THEN
      PRINT *,myname,": Unable to deallocate var2d."
      CALL final_(63)
     END IF

    END IF SpecCheck

   END IF zoneMasking

!  Is this instantiation a global case?
!  -----------------------------------
   IF(gcCO%regionsString(1:2) == "-1") THEN
    NoRegionalConstraint = .TRUE.
   ELSE
    SELECT CASE (lowercase(gcCO%regionsString(1:2)))
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
   IF(NoRegionalConstraint) gcCO%regionsString = "-1"

   IF(MAPL_AM_I_ROOT()) THEN
    IF(NoRegionalConstraint) THEN
     PRINT *,myname,": This instantiation has no regional constraints."
    ELSE
     PRINT *,myname,": This instantiation is regionally constrained."
     PRINT *,myname,": List of region numbers included: ",TRIM(gcCO%regionsString)
    END IF
   END IF

!  Set the initial CO surface fluxes to zero
!  -----------------------------------------
   gcCO%COsfcFlux(i1:i2,j1:j2) = 0.00

   DEALLOCATE(ier)

   RETURN

CONTAINS

   SUBROUTINE init_()

   INTEGER ios, nerr
   nerr = 128
   ALLOCATE ( gcCO%eCO_bioburn(i1:i2,j1:j2), & 
              gcCO%eCO_bioburn_(i1:i2,j1:j2), & 
              gcCO%eCO_biofuel(i1:i2,j1:j2), &
              gcCO%eCO_fosfuel(i1:i2,j1:j2), &
              gcCO%COsfcFlux(i1:i2,j1:j2), &
              gcCO%eCO_iso(i1:i2,j1:j2), &
              gcCO%eCO_mon(i1:i2,j1:j2), &
              gcCO%eCO_mtn(i1:i2,j1:j2), &
              gcCO%regionMask(i1:i2,j1:j2), &
              gcCO%CH4(i1:i2,j1:j2,km), &
              gcCO%OHnd(i1:i2,j1:j2,km), &
	      ier(nerr),STAT=ios )
   IF ( ios /= 0 ) rc = 100
   END SUBROUTINE init_

   SUBROUTINE final_(ierr)
   INTEGER :: ierr
   INTEGER ios
   DEALLOCATE ( gcCO%eCO_bioburn, gcCO%eCO_biofuel, gcCO%eCO_fosfuel, & 
                gcCO%eCO_bioburn_, &
                gcCO%COsfcFlux, gcCO%eCO_iso, gcCO%eCO_mon, &
                gcCO%eCO_mtn, gcCO%regionMask, gcCO%CH4, gcCO%OHnd, &
                ier, STAT=ios )
   CALL I90_release()
   rc = ierr
   END SUBROUTINE final_

 END SUBROUTINE CO_GridCompInitialize1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   SUBROUTINE CO_GridCompRun1_ ( gcCO, w_c, impChem, expChem, &
                                 nymd, nhms, cdt, rc )

#define MR_PBL
!#define SFLUX_PBL

! !USES:

  IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:

   TYPE(CO_GridComp1), INTENT(INOUT) :: gcCO   ! Grid Component
   TYPE(Chem_Bundle), INTENT(INOUT) :: w_c	! Chemical tracer fields   

! !INPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(inout) :: impChem    ! Import State
   INTEGER, INTENT(IN) :: nymd, nhms	      ! time
   REAL,    INTENT(IN) :: cdt		      ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(ESMF_State), intent(inout) :: expChem     ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine implements the CO Driver for INTEX. That 
!               is, adds chemical tendencies to each of the constituents,
!  Note: water wapor, the first constituent is not considered a chemical
!  constituents.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  31May2005  Nielsen  Mods for 7 tags, 5 regions
!  04Nov2005     Bian  CO tagged to 4 regions    
!  13Apr2005     Bian  CO tagged to emissions    
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'CO_GridCompRun'
   CHARACTER(LEN=*), PARAMETER :: Iam = myname

!  Input fields from fvGCM
!  -----------------------
   REAL, POINTER, DIMENSION(:,:)   ::  PBLH  => null()
   REAL, POINTER, DIMENSION(:,:,:) ::  T     => null()
   REAL, POINTER, DIMENSION(:,:,:) ::  RHOA  => null()

   INTEGER :: i1, i2, im, j1, j2, jm, km, ios, idiag, iXj
   INTEGER :: i, j, k, kReverse, n, nbeg, nend
   INTEGER :: nymd1, nhms1, ier(8)
   integer :: iregWant

   REAL, PARAMETER :: nsuba=6.022E+26
   REAL, PARAMETER :: mwtAir=28.97
   REAL, PARAMETER :: mwtCO=28.01
   REAL, PARAMETER :: rstar=8.3143E+03
   REAL, PARAMETER :: rpstd=1.00E-05

   REAL    :: qmin, qmax, toMass, BBconFac, c2co
   REAL    :: fiso, fmtn, fmon

   REAL, ALLOCATABLE :: CH4nd(:,:,:)
   REAL, ALLOCATABLE :: pe(:,:,:),p(:,:,:),nd(:,:,:)
   REAL, ALLOCATABLE :: rkoh(:,:,:),rkch4(:,:,:)

#define EXPORT   expChem
#define iNAME    TRIM(gcCO%iname)

#define COEM     CO_emis
#define COCL     CO_column
#define COSC     CO_surface
#define COPD     CO_prod
#define COLS     CO_loss

   integer :: STATUS

#include "CO_GetPointer___.h"

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1
   i2 = w_c%grid%i2
   im = w_c%grid%im

   j1 = w_c%grid%j1
   j2 = w_c%grid%j2
   jm = w_c%grid%jm
   km = w_c%grid%km

   iXj = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )

   nbeg  = w_c%reg%i_CO
   nend  = w_c%reg%j_CO

!  It requires 1 bin
!  -----------------
   if ( nbeg /= nend ) then
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Must have only 1 bin at the single instance level"
      rc = 1
      return 
   end if

!  Conversion factor, molecules CO cm^-2 s^-1 to kg CO m^-2 s^-1
!  -------------------------------------------------------------
   toMass = 1.00E+04*mwtCO/nsuba
   c2co   = mwtCO/12.00

!  Update emissions and OH number density once each day.
!  The latter appears to be in molecules cm^-3.
!  -----------------------------------------------------
   UpdateBCs: IF ( gcCO%BCnymd /= nymd ) THEN
   
!   Selections based on biomass burning emission set chosen
!   Currently, parse on:
!    harvard   -> molecules cm-2 s-1, need to convert to mass
!    modisfire -> kg CO m-2 s-1
!    else      -> based on dry matter consumed

!   Biomass Burning -- select on known inventories
!   ----------------------------------------------

!   Harvard biomass burning climatology, is in molecules cm^-2 s^-1
!   ---------------------------------------------------------------
    IF ( index(gcCO%eFilen_biomass,'harvard') .GT. 0 ) then  
     nymd1 = 2001*10000 + MOD ( nymd, 10000 )  ! assumes 2001
     nhms1 = 120000
     BBconFac = toMass
     CALL Chem_UtilMPread ( gcCO%eFilen_biomass, 'emcobu', nymd1, nhms1, &
        		    i1, i2, 0, im, j1, j2, 0, jm, 0, &
        		    var2d=gcCO%eCO_bioburn, cyclic=.true., &
        		    grid=w_c%grid_esmf, maskString=TRIM(gcCO%regionsString), &
                            gridMask=gcCO%regionMask)
    ELSE
 
!    Biomass burning daily files, currently in kg m^-2 s^-1.
!    Note: We need to take care of the /dev/null case, too.
!    -------------------------------------------------------
     IF(INDEX(gcCO%eFilen_biomass,    '%') .GT. 0 .OR. INDEX(gcCO%eFilen_biomass, 'gfed') .GT. 0 .OR. &
     	INDEX(gcCO%eFilen_biomass,'GFED2') .GT. 0 .OR. INDEX(gcCO%eFilen_biomass, 'null') .GT. 0) THEN  
      nymd1 = nymd      ! gfed: 1997-2004, modisfire: 2005, qfed: 2006
      if (nymd == 20041231)  nymd1=20050101
      if (nymd == 20051231)  nymd1=20060101
      nhms1 = 120000
      BBconFac = gcCO%BBconFac
     ELSE   ! Assume Duncan/Martin climatology
      nymd1 = 1971*10000 + mod ( nymd, 10000 )  ! assumes 1971
      nhms1 = 120000
      BBconFac = gcCO%BBconFac
     ENDIF

     CALL Chem_UtilMPread ( gcCO%eFilen_biomass, 'biomass', nymd1, nhms1, &
        		    i1, i2, 0, im, j1, j2, 0, jm, 0, &
        		    var2d=gcCO%eCO_bioburn, cyclic=.true., &
        		    grid=w_c%grid_esmf, maskString=TRIM(gcCO%regionsString), &
     			    gridMask=gcCO%regionMask)
    ENDIF

! Background OH, for loss term
! ----------------------------
    nymd1 = (gcCO%nymd_oh/10000)*10000 + MOD ( nymd, 10000 )
    nhms1 = 120000
    CALL Chem_UtilMPread ( gcCO%OHFileName, 'oh', nymd1, nhms1, &
                           i1, i2, 0, im, j1, j2, 0, jm, km, &
                           var3d=gcCO%OHnd, cyclic=.true., &
                           grid=w_c%grid_esmf )

! Background CH4, for source term.
! NOTE: Return zeroes in all but the global instantiation.
! --------------------------------------------------------
    CALL Chem_UtilMPread ( gcCO%OHFileName, 'ch4', nymd1, nhms1, &
    			   i1, i2, 0, im, j1, j2, 0, jm, km, &
    			   var3d=gcCO%CH4, cyclic=.true., &
    			   grid=w_c%grid_esmf, maskString=TRIM(gcCO%regionsString), &
    			   gridMask=gcCO%regionMask)

! Biofuel source
! --------------
    nymd1 = (gcCO%nymd_bf/10000)*10000 + MOD ( nymd, 10000 )
    nhms1 = 120000
    CALL Chem_UtilMPread ( gcCO%bfFilen, 'emcobf', nymd1, nhms1, &
        		   i1, i2, 0, im, j1, j2, 0, jm, 0, &
        		   var2d=gcCO%eCO_biofuel, cyclic=.true., &
    			   grid=w_c%grid_esmf, maskString=TRIM(gcCO%regionsString), &
    			   gridMask=gcCO%regionMask)

! Fossil fuel source
! ------------------
    nymd1 = (gcCO%nymd_ff/10000)*10000 + MOD ( nymd, 10000 )
    nhms1 = 120000
    CALL Chem_UtilMPread ( gcCO%ffFilen, 'emcofs', nymd1, nhms1, &
    			   i1, i2, 0, im, j1, j2, 0, jm, 0, &
    			   var2d=gcCO%eCO_fosfuel, cyclic=.true., &
    			   grid=w_c%grid_esmf, maskString=TRIM(gcCO%regionsString), &
    			   gridMask=gcCO%regionMask)

! Isoprene source
! ---------------
    nymd1 = (gcCO%nymd_p/10000)*10000 + MOD ( nymd, 10000 )
    nhms1 = 120000
    CALL Chem_UtilMPread ( gcCO%pFilen, 'emcoisop', nymd1, nhms1, &
    			   i1, i2, 0, im, j1, j2, 0, jm, 0, &
    			   var2d=gcCO%eCO_iso, cyclic=.true., &
    			   grid=w_c%grid_esmf, maskString=TRIM(gcCO%regionsString), &
    			   gridMask=gcCO%regionMask)

! VOC source
! ----------
    CALL Chem_UtilMPread ( gcCO%pFilen, 'emconvoc', nymd1, nhms1, &
    			   i1, i2, 0, im, j1, j2, 0, jm, 0, &
    			   var2d=gcCO%eCO_mon, cyclic=.true., &
    			   grid=w_c%grid_esmf, maskString=TRIM(gcCO%regionsString), &
    			   gridMask=gcCO%regionMask)

! Monoterpene source
! ------------------
    CALL Chem_UtilMPread ( gcCO%pFilen, 'emcoterp', nymd1, nhms1, &
    			   i1, i2, 0, im, j1, j2, 0, jm, 0, &
    			   var2d=gcCO%eCO_mtn, cyclic=.true., &
    			   grid=w_c%grid_esmf, maskString=TRIM(gcCO%regionsString), &
    			   gridMask=gcCO%regionMask)

! Amplification/supression factors
! PRC: Huisheng: check here to see if you like these
!      Now that I had to change the emission names.
! --------------------------------------------------
    IF ( index(gcCO%ffFilen,'bian')          .GT. 0   .OR. &
         index(gcCO%ffFilen,'co_fossilfuel') .GT. 0   .OR. &
         index(gcCO%bfFilen,'co_biofuel')    .GT. 0 ) THEN  
       toMass = 1.0
    ENDIF
    IF      ( index(gcCO%pFilen,'fvgcm') .GT. 0 ) THEN  
       fiso = 0.2 * 1.00E-06 * c2co
       fmtn = 1.0 * 1.00E-06 * c2co
       fmon = 1.0 * 1.00E-06 * c2co
    ELSE IF ( index(gcCO%pFilen,'geia') .GT. 0 ) THEN  
       fiso = 0.15 * 1.00E-06 * c2co
       fmtn = 0.2  * 1.00E-06 * c2co
       fmon = 0.2  * 1.00E-06 * c2co
    ELSE IF ( index(gcCO%pFilen,'co_biogenic') .GT. 0 ) THEN  
       fiso = 0.2 * c2co
       fmtn = 1.0 * c2co
       fmon = 1.0 * c2co
    ELSE IF ( index(gcCO%pFilen,'null') .GT. 0 ) THEN  
       fiso = 0.00
       fmtn = 0.00
       fmon = 0.00
    ENDIF

#ifdef DEBUG
    CALL pmaxmin('CO: eCO_bioburn', gcCO%eCO_bioburn, qmin, qmax, iXj,1, 1. )
    CALL pmaxmin('CO: eCO_biofuel', gcCO%eCO_biofuel, qmin, qmax, iXj,1, 1. )
    CALL pmaxmin('CO: eCO_fosfuel', gcCO%eCO_fosfuel, qmin, qmax, iXj,1, 1. )
    CALL pmaxmin('CO: eCO_iso',     gcCO%eCO_iso,     qmin, qmax, iXj,1, 1. )
    CALL pmaxmin('CO: eCO_mon',     gcCO%eCO_mon,     qmin, qmax, iXj,1, 1. )
    CALL pmaxmin('CO: eCO_mtn',     gcCO%eCO_mtn,     qmin, qmax, iXj,1, 1. )
#endif

    gcCO%BCnymd = nymd 

!  Units for surface flux must be kgCO m^-2 s^-1
!  -------------------------------------------
    gcCO%eCO_fosfuel(i1:i2,j1:j2) = gcCO%eCO_fosfuel(i1:i2,j1:j2)*toMass
    gcCO%eCO_biofuel(i1:i2,j1:j2) = gcCO%eCO_biofuel(i1:i2,j1:j2)*toMass
    gcCO%eCO_bioburn(i1:i2,j1:j2) = gcCO%eCO_bioburn(i1:i2,j1:j2)*BBconFac
    gcCO%eCO_iso(i1:i2,j1:j2) = gcCO%eCO_iso(i1:i2,j1:j2) * fiso
    gcCO%eCO_mon(i1:i2,j1:j2) = gcCO%eCO_mon(i1:i2,j1:j2) * fmtn
    gcCO%eCO_mtn(i1:i2,j1:j2) = gcCO%eCO_mtn(i1:i2,j1:j2) * fmon

!  OH number density from molecules cm^-3 to molecules m^-3
!  --------------------------------------------------------
    gcCO%OHnd(i1:i2,j1:j2,1:km) = gcCO%OHnd(i1:i2,j1:j2,1:km)*1.00E+06

! In ARCTAS, the vertical order in the OH and CH4 input file is ground-to-lid.
! ----------------------------------------------------------------------------
    ALLOCATE(CH4nd(i1:i2,j1:j2,km), STAT=ios)
    IF(ios /= 0) THEN
      rc = 1
      PRINT *,myname,": Unable to ALLOCATE reversing array."
      RETURN
    END IF

    ch4nd(i1:i2,j1:j2,1:km) = gcCO%OHnd(i1:i2,j1:j2,1:km)
    DO k=1,km
     kReverse = km-k+1
     gcCO%OHnd(i1:i2,j1:j2,k) = ch4nd(i1:i2,j1:j2,kReverse)
    END DO

    ch4nd(i1:i2,j1:j2,1:km) = gcCO%CH4(i1:i2,j1:j2,1:km)
    DO k=1,km
     kReverse = km-k+1
     gcCO%CH4(i1:i2,j1:j2,k) = ch4nd(i1:i2,j1:j2,kReverse)
    END DO

    DEALLOCATE(CH4nd, STAT=ios)
    IF(ios /= 0) THEN
      rc = 2
      PRINT *,myname,": Unable to DEALLOCATE reversing array."
      RETURN
    END IF

!   Save this in case we need to apply diurnal cycle
!   ------------------------------------------------
   if ( w_c%diurnal_bb ) then
        gcCO%eCO_bioburn_(:,:) = gcCO%eCO_bioburn(:,:)
   end if

   END IF UpdateBCs

!  Apply diurnal cycle if so desired
!  ---------------------------------
   if ( w_c%diurnal_bb ) then
      call Chem_BiomassDiurnal ( gcCO%eCO_bioburn, gcCO%eCO_bioburn_,   &
                                 w_c%grid%lon(:), w_c%grid%lat(:), nhms, cdt )      
   end if


!  Allocate temporary workspace
!  ----------------------------
   allocate ( pe(i1:i2,j1:j2,km+1), p(i1:i2,j1:j2,km), nd(i1:i2,j1:j2,km), &
              rkoh(i1:i2,j1:j2,km), rkch4(i1:i2,j1:j2,km), &
              CH4nd(i1:i2,j1:j2,km), stat = ios )

   if ( ios /= 0 ) then
      rc = 3
      return
   end if

!  Layer interface pressures
!  -------------------------
   pe(i1:i2,j1:j2,1)=w_c%grid%ptop
   DO k=2,km+1
    pe(i1:i2,j1:j2,k)=pe(i1:i2,j1:j2,k-1)+w_c%delp(i1:i2,j1:j2,k-1)
   END DO

!  Layer mean pressures
!  --------------------
   DO k=1,km
    p(i1:i2,j1:j2,k)=(pe(i1:i2,j1:j2,k)+pe(i1:i2,j1:j2,k+1))*0.50
   END DO
 
!  Get imports
!  -----------
   call MAPL_GetPointer( impChem, pblh,  'ZPBL',    rc=ier(1) ) 
   call MAPL_GetPointer( impChem, T,     'T',       rc=ier(2) ) 
   call MAPL_GetPointer( impChem, rhoa,  'AIRDENS', rc=ier(3) ) 

   if ( any(ier(1:3) /= 0) ) then
        rc = 10
        return
   end if

#ifdef DEBUG
    CALL pmaxmin('CO: pblh',   pblh, qmin, qmax, iXj,  1, 1. )
    CALL pmaxmin('CO:  T  ',      T, qmin, qmax, iXj, km, 1. )
    CALL pmaxmin('CO: rhoa',   rhoa, qmin, qmax, iXj, km, 1. )
#endif

!  Number density
!  --------------
   nd(i1:i2,j1:j2,1:km)= nsuba*p(i1:i2,j1:j2,1:km)/ &
                        (rstar*t(i1:i2,j1:j2,1:km))

!  CH4 number density.  CH4 on file is in mole fraction.
!  -----------------------------------------------------
   CH4nd(i1:i2,j1:j2,1:km)=gcCO%CH4(i1:i2,j1:j2,1:km)* &
                                 nd(i1:i2,j1:j2,1:km)
 
!  Clear surface flux array
!  ------------------------
   gcCO%COsfcFlux(i1:i2,j1:j2) = 0.0

#if defined( MR_PBL)
!  Emissions, direct update of mixing ratio 
!  ----------------------------------------
   call CO_Emission ( i1, i2, j1, j2, km, cdt, gcCO, w_c, &
                      pblh, T, rhoa, rc)
#endif

!  Convert carbon monoxide from mole fraction to number density
!  ------------------------------------------------------------
   w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km) = &
                  w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km)*nd(i1:i2,j1:j2,1:km)

!  Loss due to OH.
!  ---------------
   rkoh(i1:i2,j1:j2,1:km) = &
                  1.5E-13*1.00E-06*(1.00+0.6*p(i1:i2,j1:j2,1:km)*rpstd)
   n = gcCO%instance 
   IF(ASSOCIATED(CO_loss)) CO_loss(i1:i2,j1:j2) = 0.
   DO k = 1, km

    IF(ASSOCIATED(CO_loss)) CO_loss(i1:i2,j1:j2) = CO_loss(i1:i2,j1:j2) &
   	 + w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k)*rkoh(i1:i2,j1:j2,k) &
   	 * gcCO%OHnd(i1:i2,j1:j2,k)/nd(i1:i2,j1:j2,k) &
   	 * mwtCO/mwtAir*w_c%delp(i1:i2,j1:j2,k)/grav

    w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k) = &
   		     w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k)*(1.00-cdt* &
   		     rkoh(i1:i2,j1:j2,k)*gcCO%OHnd(i1:i2,j1:j2,k))

   END DO ! Next layer, k

!  CH4 production
!  --------------
   rkch4(i1:i2,j1:j2,1:km)= &
                 2.45e-12*1.00E-06*exp(-1775./t(i1:i2,j1:j2,1:km))
   n = gcCO%instance 
   IF(ASSOCIATED(CO_prod)) CO_prod(i1:i2,j1:j2) = 0.
   DO k = 1, km

    IF(ASSOCIATED(CO_prod)) CO_prod(i1:i2,j1:j2) = CO_prod(i1:i2,j1:j2) &
       + rkch4(i1:i2,j1:j2,k)*gcCO%OHnd(i1:i2,j1:j2,k)*CH4nd(i1:i2,j1:j2,k) &
       / nd(i1:i2,j1:j2,k) * mwtCO/mwtAir*w_c%delp(i1:i2,j1:j2,k)/grav

    w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k)=w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k)+cdt* &
   		      rkch4(i1:i2,j1:j2,k)*gcCO%OHnd(i1:i2,j1:j2,k)* &
   		      CH4nd(i1:i2,j1:j2,k)

   END DO ! Next layer, k

!  Return to mole fraction
!  -----------------------
   w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km)=w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km)/nd(i1:i2,j1:j2,1:km)

!  Surface concentration in ppbv
!  -----------------------------
   n = gcCO%instance 
    if(associated(CO_surface)) &
      CO_surface(i1:i2,j1:j2) = w_c%qa(nbeg)%data3d(i1:i2,j1:j2,km)*1.e9

!  Column burden in kg m-2
!  -----------------------
   n = gcCO%instance 
    if(associated(CO_column)) then
     CO_column(i1:i2,j1:j2) = 0.
     do k = 1, km
      CO_column(i1:i2,j1:j2) &
       =   CO_column(i1:i2,j1:j2) &
         +   w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k)*mwtCO/mwtAir &
           * w_c%delp(i1:i2,j1:j2,k)/grav
     enddo
    endif

!  CO Surface Emission Flux in kg m-2 s-1
!  --------------------------------------
    n = gcCO%instance 
    if(associated(CO_emis)) &
         CO_emis(i1:i2,j1:j2) = gcCO%COsfcFlux(i1:i2,j1:j2)

#ifdef DEBUG
     n = gcCO%instance 
     if(associated(CO_emis)) &
     CALL pmaxmin('CO: emis', CO_emis(i1:i2,j1:j2), qmin, qmax, &
                   iXj,1, 1. )
     if(associated(CO_loss)) &
     CALL pmaxmin('CO: loss', CO_loss(i1:i2,j1:j2), qmin, qmax, &
                   iXj,1, 1. )
     if(associated(CO_prod)) &
     CALL pmaxmin('CO: prod', CO_prod(i1:i2,j1:j2), qmin, qmax, &
                   iXj,1, 1. )
     if(associated(CO_column)) &
     CALL pmaxmin('CO: column', CO_column(i1:i2,j1:j2), qmin, qmax, &
                   iXj,1, 1. )
     if(associated(CO_surface)) &
     CALL pmaxmin('CO: surface', CO_surface(i1:i2,j1:j2), qmin, qmax,&
                   iXj,1, 1. )
#endif

!  Housekeeping
!  ------------
   DEALLOCATE(nd,p,pe,rkoh,rkch4,CH4nd,STAT=ier(1))

   RETURN

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
! !DESCRIPTION: Updates the CO concentration with emissions every timestep
!
! !REVISION HISTORY:
!
!  17Oct2005, Bian   
!
! !IROUTINE:  CO_Emission - Adds emissions for CO for one timestep
!             We have emissions from 4 sources, which are distributed
!             differently in the vertical
!             1) fossil fuel - emitted at surface
!             2) biofuel sources - emitted at surface 
!             3) biomass burning - uniformly mixed in PBL
!             4) biogenic - emitted at surface
!                           include: isoprene, converting factor 0.15
!                                    terpene,  converting factor 0.2
!                                    nvoc,     converting factor 0.2
!  14Apr2006, Bian: Add indirect NMHC from FF (0.20), BF (0.19), BB (0.11)
!                   Add seasonality for FF
!                   Modify FF & BF over Asia region (1.39) for Streets' data
!
! !INTERFACE:
!
!EOP

   subroutine CO_Emission ( i1, i2, j1, j2, km, cdt, gcCO, w_c, &
                            pblh, T, rhoa, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   integer, intent(in) :: i1, i2, j1, j2, km
   real, intent(in)    :: cdt
   type(CO_GridComp1), intent(inout)    :: gcCO       ! CO Grid Component
   real, pointer, dimension(:,:)    :: pblh
   real, pointer, dimension(:,:,:)  :: T
   real, pointer, dimension(:,:,:)  :: rhoa

! !OUTPUT PARAMETERS:

   type(Chem_Bundle), intent(inout) :: w_c         ! Chemical tracer fields
   integer, intent(out)             :: rc          ! Error return code:
                                                   !  0 - all is well
                                                   !  1 - 
   character(len=*), parameter :: myname = 'CO_Emission'

! !Local Variables
   integer  ::  i, j, k, m, n, ios
   integer  ::  nbeg, nend
   real, dimension(i1:i2,j1:j2) :: pPblh  ! pressure at PBLH
   real, dimension(i1:i2,j1:j2) :: p0, z0, ps
   real :: p1, z1, dz, delz, delp, fPblh, fs
   real :: fff, fbf, fbb, fu
   real :: qmax, qmin, eBiofuel, eBiomass, fTerpene
   real :: srctot, srcff, srcbf, srcbb, srcbi, src

   REAL, PARAMETER :: mwtAir=28.97
   REAL, PARAMETER :: mwtCO=28.01

   LOGICAL:: doingBB

!  Initialize local variables
!  --------------------------
   nbeg  = w_c%reg%i_CO
   nend  = w_c%reg%j_CO
   fff   = 1.20
   fbf   = 1.19
   fbb   = 1.11

!  It requires 1 bin
!  -----------------
   if ( nbeg /= nend ) then
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Must have only 1 bin at the single instance level"
      rc = 1
      return 
   end if

!  Is this a biomass burning instantiation?
!  ----------------------------------------
   IF(INDEX(gcCO%eFilen_biomass,'null') > 0 ) THEN
    doingBB = .FALSE.
   ELSE
    doingBB = .TRUE.
   END IF

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
      if(z0(i,j) .lt. pblh(i,j) .and. z1 .ge. pblh(i,j)) then
       delz = z1-pblh(i,j)
       delp = delz*rhoa(i,j,k)*grav
       pPblh(i,j) = p1+delp
      endif
      p0(i,j) = p1
      z0(i,j) = z1
     end do
    end do
   end do

!  Now update the tracer mixing ratios with the CO sources
   p0 = ps
   do k = km, 1, -1
      if ( k .eq. km) fs = 1.00
      if ( k .ne. km) fs = 0.00
    do j = j1, j2
     do i = i1, i2
      p1 = p0(i,j) - w_c%delp(i,j,k)

      fPblh = 0.

!  Provision to skip this time-consuming calculation when instantiation is not BB
!  ------------------------------------------------------------------------------
      IF(doingBB) THEN
       if(p1 .ge. pPblh(i,j)) fPblh = w_c%delp(i,j,k)/(ps(i,j)-pPblh(i,j))
       if(p1 .lt. pPblh(i,j) .and. p0(i,j) .ge. pPblh(i,j)) &
                fPblh = (p0(i,j)-pPblh(i,j))/(ps(i,j)-pPblh(i,j))
      END IF

!  Convert emission from Kg CO/m2/s to mixing ratio/m2/s (TVVMM/(delz*airden))
!  -----------------------------------------------------
   fu = (mwtAir/mwtCO)/(w_c%delp(i,j,k)/grav)

      srcbi = (gcCO%eCO_iso(i,j)*fiso+gcCO%eCO_mon(i,j)*fmon+gcCO%eCO_mtn(i,j)*fmtn)*fs*fu
      srcff = (gcCO%eCO_fosfuel(i,j) * fff * fs ) * fu
      srcbf = (gcCO%eCO_biofuel(i,j) * fbf * fs  ) * fu
      srcbb = (gcCO%eCO_bioburn(i,j) * fbb * fPblh ) * fu

      srctot = srcff + srcbf + srcbb + srcbi

!
! Get tagged CO to regions
! -----------------------
      w_c%qa(nbeg)%data3d(i,j,k) = w_c%qa(nbeg)%data3d(i,j,k) + srctot*cdt
      gcCO%COsfcFlux(i,j) = gcCO%COsfcFlux(i,j) + srctot/fu

      p0(i,j) = p1 

     end do
    end do
   end do

   rc = 0

   end subroutine CO_Emission

 END SUBROUTINE CO_GridCompRun1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   SUBROUTINE CO_GridCompFinalize1_ ( gcCO, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:

   TYPE(CO_GridComp1), INTENT(INOUT) :: gcCO   ! Grid Component

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), INTENT(IN)  :: w_c      ! Chemical tracer fields   
   INTEGER, INTENT(IN) :: nymd, nhms	      ! time
   REAL,    INTENT(IN) :: cdt		      ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(INOUT) :: impChem	! Import State
   TYPE(ESMF_State), INTENT(INOUT) :: expChem	! Import State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
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

   CHARACTER(LEN=*), PARAMETER :: myname = 'CO_GridCompFinalize'
   INTEGER :: ios

   DEALLOCATE ( gcCO%eCO_bioburn, gcCO%eCO_biofuel, gcCO%eCO_fosfuel, & 
                gcCO%COsfcFlux, gcCO%eCO_iso, gcCO%eCO_mon, &
                gcCO%eCO_mtn, gcCO%regionMask, gcCO%CH4, gcCO%OHnd, &
                STAT=ios )
   rc = 0
   IF ( ios /= 0 ) rc = 1

   RETURN

 END SUBROUTINE CO_GridCompFinalize1_

 END MODULE CO_GridCompMod

!-----------------------------------------------------------------------

!                     Single Instance Wrapper

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO_SingleInstance_ --- Runs single instance of method
!
! !INTERFACE:
!
  subroutine CO_SingleInstance_ ( Method_, instance, &
                                  gcCO, w_c, impChem, expChem, &
                                  nymd, nhms, cdt, rc )

! !USES:

  Use CO_GridCompMod
  Use ESMF_Mod
  Use MAPL_Mod
  Use Chem_Mod 

  IMPLICIT NONE

! !INPUT PARAMETERS:

!  Input "function pointer"
!  -----------------------
   interface 
     subroutine Method_ (gc, w, imp, exp, ymd, hms, dt, rcode )
       Use CO_GridCompMod
       Use ESMF_Mod
       Use MAPL_Mod
       Use Chem_Mod 
       type(CO_GridComp1),  intent(inout)  :: gc
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
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(CO_GridComp1), INTENT(INOUT) :: gcCO    ! Grid Component
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

  integer n_CO, i_CO, j_CO

! Save overall CO indices
! -----------------------
  n_CO = w_c%reg%n_CO
  i_CO = w_c%reg%i_CO
  j_CO = w_c%reg%j_CO
  
! Customize indices for this particular instance
! ----------------------------------------------
  w_c%reg%n_CO = 1
  w_c%reg%i_CO = i_CO + instance - 1
  w_c%reg%j_CO = i_CO + instance - 1
  
! Execute the instance method
! ---------------------------
  call Method_ ( gcCO, w_c, impChem, expChem, &
                 nymd, nhms, cdt, rc )

! Restore the overall CO indices
! ------------------------------
  w_c%reg%n_CO = n_CO
  w_c%reg%i_CO = i_CO
  w_c%reg%j_CO = j_CO

  end subroutine CO_SingleInstance_

!-----------------------------------------------------------------------
