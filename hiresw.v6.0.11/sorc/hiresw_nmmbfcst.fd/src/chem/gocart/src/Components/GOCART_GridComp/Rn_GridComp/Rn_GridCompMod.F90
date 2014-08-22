#include "MAPL_Generic.h"

!!! TO DO: Please revise Prologues!!!!

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  Rn_GridCompMod --- Rn Grid Component Class
!
! !INTERFACE:
!

   MODULE  Rn_GridCompMod

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
   PUBLIC  Rn_GridComp       ! Multiple instance Radon object 
   PUBLIC  Rn_GridComp1      ! Single instance Radon object

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  Rn_GridCompInitialize
   PUBLIC  Rn_GridCompRun
   PUBLIC  Rn_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the Rn Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  01Aug2006 da Silva  Extensions for GEOS-5.
!  10Mar2008 da Silva  Multiple instances for ARCTAS.
!  12Apr2008 Nielsen   Configured for radon.
!
!EOP
!-------------------------------------------------------------------------

  TYPE Rn_GridComp1

        CHARACTER(LEN=255) :: name            ! generic name of the package
        CHARACTER(LEN=255) :: iname           ! instance name
        CHARACTER(LEN=255) :: rcfilen         ! resource file name
        CHARACTER(LEN=255) :: maskFileName    ! File that contains the land mask
        CHARACTER(LEN=255) :: regionsString   ! Comma-delimited string of regions
        CHARACTER(LEN=255) :: emisFileName    ! Radon emission file name

        INTEGER :: instance                   ! instance number
        INTEGER :: BCnymd                     ! Date of last emissions update

        REAL :: halfLife                      ! Half-life
        CHARACTER(LEN=255) :: halfLifeUnit    ! Half-life unit: years, days, or seconds
        REAL :: decayConstant                 ! Decay constant, inverse seconds.
	REAL :: emission                      ! kg m^{-2} s^{-1}

        REAL, POINTER :: regionMask(:,:)      ! regional mask
        REAL, POINTER :: RnsfcFlux(:,:)       ! Rn surface flux kg m^-2 s^-1
        REAL, POINTER :: ScheryEmission(:,:)  ! Monthly mean emission mBq m^{-2} s^{-1}

  END TYPE Rn_GridComp1

  TYPE Rn_GridComp
     integer                     ::  n        ! number of instances 
     TYPE(Rn_GridComp1), pointer ::  gcs(:)   ! instances
  END TYPE Rn_GridComp

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Rn_GridCompInitialize --- Initialize Rn_GridComp
!
! !INTERFACE:
!

   subroutine Rn_GridCompInitialize ( gcRn, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(Rn_GridComp), INTENT(INOUT) :: gcRn   ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the Rn Grid Component. Multiple instance
!               version.
!
! !REVISION HISTORY:
!
!  27Feb2008  da Silva  Introduced multiple instances
!  12Apr2008  Nielsen   Configured for radon
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'Rn_GridCompInitialize'
   CHARACTER(LEN=255) :: rcbasen = 'Rn_GridComp'
   CHARACTER(LEN=255) :: name
   
   integer i, ier, n

!  Load resource file
!  ------------------
   CALL I90_loadf ( TRIM(rcbasen)//'.rc', ier )
   if ( ier .NE. 0 ) then; rc = 10; return; end if

!  Parse resource file
!  -------------------
   CALL I90_label ( 'Rn_instances:', ier )
   if ( ier .NE. 0 ) then; rc = 20; return; end if

!  First determine how many instances we have
!  ------------------------------------------   
   n = 0
   do while ( ier .EQ. 0 )
      CALL I90_gtoken( name, ier )
      n = n + 1
   end do
   if ( n .EQ. 0 ) then; rc = 30; return; end if
   
!  We cannot have fewer instances than the number of
!  Rn bins in the registry (it is OK to have less, though)
!  --------------------------------------------------------
   if ( n .LT. w_c%reg%n_Rn ) then
        rc = 35
        return
   else if ( n .GT. w_c%reg%n_Rn ) then
        if (MAPL_AM_I_ROOT()) &
        print *, trim(myname)// &
                 ': fewer Rn bins than possible Rn instances: ',&
                 n, w_c%reg%n_Rn
   end if
   n = min(n,w_c%reg%n_Rn )
   gcRn%n = n

!  Next allocate necessary memory
!  ------------------------------
   allocate ( gcRn%gcs(n), stat=ier )    
   if ( ier .NE. 0 ) then; rc = 40; return; end if

!  Record name of each instance
!  ----------------------------
   CALL I90_label ( 'Rn_instances:', ier )
   do i = 1, n
      CALL I90_gtoken( name, ier )
      if ( ier .NE. 0 ) then; rc = 40; return; end if
                                            ! resource file name
      gcRn%gcs(i)%rcfilen = trim(rcbasen)//'---'//trim(name)//'.rc'
      gcRn%gcs(i)%instance = i              ! instance number 
      IF(TRIM(name) == "full" ) THEN
       gcRn%gcs(i)%iname = " "              ! blank instance name for full (1)
      ELSE
       gcRn%gcs(i)%iname = TRIM(name)       ! instance name for others
      END IF
   end do    

!  Next initialize each instance
!  -----------------------------
   do i = 1, gcRn%n
      IF(MAPL_AM_I_ROOT()) THEN
       PRINT *," "
       PRINT *,myname,": Initializing instance ",TRIM(gcRn%gcs(i)%iname)," [",gcRn%gcs(i)%instance,"]"
      END IF
      call Rn_SingleInstance_ ( Rn_GridCompInitialize1_, i, &
                                gcRn%gcs(i), w_c, impChem, expChem,  &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then; rc = 1000+ier; return; end if
   end do

!  All done
!  --------
   CALL I90_FullRelease( ier )
   IF( ier /= 0 ) THEN
    PRINT *,myname,": I90_FullRelease not successful."
    rc = 40
   END IF


 end subroutine Rn_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Rn_GridCompRun --- Run Rn_GridComp
!
! !INTERFACE:
!

   subroutine Rn_GridCompRun ( gcRn, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(Rn_GridComp), INTENT(INOUT) :: gcRn   ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Runs the Rn Grid Component. Multiple instance
!               version.
!
! !REVISION HISTORY:
!
!  27Feb2008  da Silva  Introduced multiple instances
!  12Apr2008  Nielsen   Configured for radon
!
!EOP
!-------------------------------------------------------------------------

   integer i, ier

   do i = 1, gcRn%n
      call Rn_SingleInstance_ ( Rn_GridCompRun1_, i, &
                                gcRn%gcs(i), w_c, impChem, expChem, &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then; rc = i * 1000+ier; return; end if
   end do

 end subroutine Rn_GridCompRun


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Rn_GridCompFinalize --- Initialize Rn_GridComp
!
! !INTERFACE:
!

   subroutine Rn_GridCompFinalize ( gcRn, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(Rn_GridComp), INTENT(INOUT) :: gcRn   ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Finalizes the Rn Grid Component. Multiple instance
!               version.
!
! !REVISION HISTORY:
!
!  27Feb2008  da Silva  Introduced multiple instances
!  12Apr2008  Nielsen   Configured for radon
!
!EOP
!-------------------------------------------------------------------------

   integer i, ier

   do i = 1, gcRn%n
      call Rn_SingleInstance_ ( Rn_GridCompFinalize1_, i, &
                                gcRn%gcs(i), w_c, impChem, expChem, &
                                nymd, nhms, cdt, ier )
      if ( ier .NE. 0 ) then; rc = i * 1000+ier; return; end if
   end do

   deallocate ( gcRn%gcs, stat=ier )    
   gcRn%n = -1

 end subroutine Rn_GridCompFinalize

!--------------------------------------------------------------------------

!                      Single Instance Methods

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Rn_GridCompInitialize --- Initialize Rn_GridComp
!
! !INTERFACE:
!

   subroutine Rn_GridCompInitialize1_ ( gcRn, w_c, impChem, expChem, &
                                        nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(Rn_GridComp1), INTENT(INOUT) :: gcRn   ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the Rn Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  31May2005  Nielsen  Mods for 7 CO bins, 5 region masks
!  04Nov2005     Bian  CO tagged to 4 regions 
!                      (global, North America, South America, and Africa)
!                      for CR-AVE
!  12Apr2008  Nielsen  Configured for radon
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'Rn_GridCompInitialize1'

   CHARACTER(LEN=255) :: rcfilen 

   INTEGER :: ios, j, n
   INTEGER, ALLOCATABLE :: ier(:)
   INTEGER :: i1, i2, im, j1, j2, jm, km
   INTEGER :: nTimes, begTime, incSecs
   INTEGER :: nbeg, nend, nymd1, nhms1
   LOGICAL :: NoRegionalConstraint
   LOGICAL :: unitOK

   REAL :: conFac, limitN, limitS, log10Emission, radTODeg
   REAL, ALLOCATABLE :: var2d(:,:)

   rcfilen = gcRn%rcfilen
   gcRn%name = 'GEOS-5/GOCART Parameterized Radon Package'
   radTODeg = 57.2957795
   gcRn%BCnymd = -1

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

   nbeg  = w_c%reg%i_Rn
   nend  = w_c%reg%j_Rn

!  It requires 1 bin
!  -----------------
   IF( nbeg /= nend ) THEN
    IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Must have only 1 bin at the single instance level"
    rc = 1
    RETURN 
   END IF

!  Allocate memory, etc
!  --------------------
   CALL INIT_()
   IF ( rc /= 0 ) RETURN

!  Load resource file
!  ------------------
   CALL I90_loadf ( TRIM(rcfilen), ier(1) )
   IF( ier(1) /= 0 ) THEN
    CALL final_(11)
    RETURN
   END IF
   ier(:)=0

!  Obtain half life.  Unit must be "years", "days", or "seconds".
!  --------------------------------------------------------------
   CALL I90_label ( 'HalfLife:', ier(1) )
   gcRn%halfLife = I90_gfloat ( ier(2) )
   CALL I90_label ( 'HalfLifeUnit:', ier(3) )
   CALL I90_gtoken( gcRn%halfLifeUnit, ier(4) )

!  Emission, mBq m^{-2} s^{-1}
!  ---------------------------
   CALL I90_label ( 'Rn_emission_file_name:', ier(5) )
   CALL I90_gtoken( gcRn%emisFileName, ier(6) )

   IF( ANY(ier(1:6) < 0 ) ) THEN
    CALL final_(21)
    RETURN
   END IF
   ier(:)=0

!  Validate the specified half-life and units, and find
!  the constant needed to convert half-life to seconds.
!  ----------------------------------------------------
   unitOK=.FALSE.
   IF(TRIM(gcRn%halfLifeUnit) ==   "years") THEN
    ier(1) = 1
    conFac = 86400.00*365.25
   END IF
   unitOK=.FALSE.
   IF(TRIM(gcRn%halfLifeUnit) ==    "days") THEN
    ier(2) = 1
    conFac = 86400.00
   END IF
   unitOK=.FALSE.
   IF(TRIM(gcRn%halfLifeUnit) == "seconds") THEN
    ier(3) = 1
    conFac = 1.00
   END IF
   IF( ALL(ier(1:3) == 0 ) ) THEN
    IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid unit specified for radon half-life."
    CALL final_(31)
    RETURN
   END IF
   IF(gcRn%halfLife <= 0.00) THEN
    IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Radon half-life must be greater than zero."
    CALL final_(32)
    RETURN
   END IF
   ier(:)=0

!  Compute the decay constant (inverse seconds) from the half-life:
!    ln(N/No) = ln(1/2) = -decayConstant * halfLife
!  ----------------------------------------------------------------
   gcRn%decayConstant = 0.693147/(gcRn%halfLife*conFac)

!  Obtain geographical region mask
!  -------------------------------
   CALL I90_label ( 'Rn_regions:', ier(5) )
   CALL I90_gtoken( gcRn%maskFileName, ier(6) )

   IF( ANY(ier(1:6) < 0 ) ) THEN
     CALL final_(41)
     RETURN
   END IF
   ier(:)=0

   CALL Chem_UtilGetTimeInfo( gcRn%maskFileName, nymd1, &
                              begTime, nTimes, incSecs )
   IF(nymd1 < 0) CALL final_(15)
   nhms1 = 120000
   CALL Chem_UtilMPread ( TRIM(gcRn%maskFileName), 'REGION_MASK', nymd1, nhms1, &
   			  i1, i2, 0, im, j1, j2, 0, jm, 0, &
   			  var2d=gcRn%regionMask, grid=w_c%grid_esmf )

!  Grab the region string.
!  -----------------------
   CALL I90_label ( 'Rn_regions_indices:', ier(1) )
   CALL I90_gtoken( gcRn%regionsString, ier(2) )
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
     gcRn%regionsString = "1"
     ALLOCATE(var2d(i1:i2,j1:j2), STAT=ios)
     IF(ios /= 0) THEN
      PRINT *,myname,": Unable to allocate var2d."
      CALL final_(62)
     END IF
     var2d(i1:i2,j1:j2) = 0.00

!  Within the latitude range specified, set land boxes to 1
!  --------------------------------------------------------
     DO j = j1,j2
      WHERE(gcRn%regionMask(i1:i2,j) > 0 .AND. &
            (limitS <= w_c%grid%lat(j)*radTODeg .AND. &
	     w_c%grid%lat(j)*radTODeg <= limitN) ) var2d(i1:i2,j) = 1.00
     END DO

!  Reset the region mask in gcRn
!  -----------------------------
     gcRn%regionMask(i1:i2,j1:j2) = var2d(i1:i2,j1:j2)

     DEALLOCATE(var2d, STAT=ios)
     IF(ios /= 0) THEN
      PRINT *,myname,": Unable to deallocate var2d."
      CALL final_(63)
     END IF

    END IF SpecCheck

   END IF zoneMasking

!  Is this instantiation a global case?
!  -----------------------------------
   IF(gcRn%regionsString(1:2) == "-1") THEN
    NoRegionalConstraint = .TRUE.
   ELSE
    SELECT CASE (lowercase(gcRn%regionsString(1:2)))
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
   IF(NoRegionalConstraint) gcRn%regionsString = "-1"

   IF(MAPL_AM_I_ROOT()) THEN
    IF(NoRegionalConstraint) THEN
     PRINT *,myname,": This instantiation has no regional constraints."
    ELSE
     PRINT *,myname,": This instantiation is regionally constrained."
     PRINT *,myname,": List of region numbers included: ",TRIM(gcRn%regionsString)
    END IF
   END IF

!  Set the initial radon surface fluxes to zero
!  --------------------------------------------
   gcRn%RnsfcFlux(i1:i2,j1:j2) = 0.00

   DEALLOCATE(ier)

   RETURN

CONTAINS

   SUBROUTINE init_()

   INTEGER ios, nerr
   nerr = 128
   ALLOCATE ( gcRn%RnsfcFlux(i1:i2,j1:j2), &
              gcRn%regionMask(i1:i2,j1:j2), &
	      gcRn%ScheryEmission(i1:i2,j1:j2), &
              ier(nerr),STAT=ios )

   IF ( ios /= 0 ) rc = 100
   END SUBROUTINE init_

   SUBROUTINE final_(ierr)
   INTEGER :: ierr
   INTEGER ios
   DEALLOCATE ( gcRn%RnsfcFlux, gcRn%regionMask, gcRn%ScheryEmission, ier, STAT=ios )
   CALL I90_release()
   rc = ierr
   END SUBROUTINE final_

 END SUBROUTINE Rn_GridCompInitialize1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Rn_GridCompRun
!
! !INTERFACE:
!

   SUBROUTINE Rn_GridCompRun1_ ( gcRn, w_c, impChem, expChem, &
                                 nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:

   TYPE(Rn_GridComp1), INTENT(INOUT) :: gcRn   ! Grid Component
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
 
! !DESCRIPTION: This routine implements the Rn driver.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  12Apr2008  Nielsen  Configured for radon
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'Rn_GridCompRun'
   CHARACTER(LEN=*), PARAMETER :: Iam = myname

!  Input fields from fvGCM
!  -----------------------
   REAL, POINTER, DIMENSION(:,:,:) :: T       => null()
   REAL, POINTER, DIMENSION(:,:,:) :: zle     => null()
   REAL, POINTER, DIMENSION(:,:)   :: soilT   => null()
   REAL, POINTER, DIMENSION(:,:)   :: fracIce => null()

   INTEGER :: i1, i2, im, j1, j2, jm, km, ios, idiag, iXj
   INTEGER :: i, j, k, kReverse, n, nbeg, nend, nymd1
   INTEGER :: ier(8)

   REAL, PARAMETER :: nsuba=6.022E+26
   REAL, PARAMETER :: mwtAir=28.97
   REAL, PARAMETER :: mwtRn=222.00
   REAL, PARAMETER :: rstar=8.3143E+03
   REAL, PARAMETER :: rpstd=1.00E-05

   REAL    :: decadence, qmin, qmax, toND
   REAL, ALLOCATABLE :: F(:,:),nd(:,:,:),p(:,:,:),pe(:,:,:),dZ(:,:,:)
   INTEGER, ALLOCATABLE :: mask(:,:)

#define EXPORT   expChem
#define iNAME    TRIM(gcRn%iname)

#define RnEM     Rn_emis
#define RnCL     Rn_column
#define RnSC     Rn_surface
#define RnLS     Rn_loss

   integer :: STATUS

#include "Rn_GetPointer___.h"

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

   nbeg  = w_c%reg%i_Rn
   nend  = w_c%reg%j_Rn

!  It requires 1 bin
!  -----------------
   if ( nbeg /= nend ) then
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Must have only 1 bin at the single instance level"
      rc = 1
      return 
   end if

!  Update emissions once each day.  Expecting units mBq m^{-2} s^{-1}.
!  -------------------------------------------------------------------
   UpdateBCs: IF( gcRn%BCnymd /= nymd ) THEN
    nymd1 = 2001*10000 + MOD(nymd,10000)
    CALL Chem_UtilMPread ( TRIM(gcRn%emisFileName), 'Rn_EMISSION', nymd1, 120000, &
    			   i1, i2, 0, im, j1, j2, 0, jm, 0, &
    			   var2d=gcRn%ScheryEmission, cyclic=.TRUE., &
    			   grid=w_c%grid_esmf)
    gcRn%BCnymd = nymd 
   END IF UpdateBCs

!  Conversion factor: mBq m^{-2} s^{-1} to atoms cm^{-2} s^{-1} to atoms m^{-2} s^{-1}
!  -----------------------------------------------------------------------------------
   toND = 4.7644E-02*1.00E+04

!  Allocate temporary workspace
!  ----------------------------
   ALLOCATE(pe(i1:i2,j1:j2,km+1), p(i1:i2,j1:j2,km), nd(i1:i2,j1:j2,km), &
            dZ(i1:i2,j1:j2,km), F(i1:i2,j1:j2), mask(i1:i2,j1:j2), STAT=ios)
   IF(ios /= 0) THEN
    rc = 3
    RETURN
   END IF

!  Get imports
!  -----------
   call MAPL_GetPointer( impChem,       T,      'T', rc=ier(1) ) 
   call MAPL_GetPointer( impChem,     zle,    'ZLE', rc=ier(2) ) 
   call MAPL_GetPointer( impChem,   soilT, 'TSOIL1', rc=ier(3) ) 
   call MAPL_GetPointer( impChem, fracIce,  'FRACI', rc=ier(4) ) 

   IF(ANY(ier(1:4) /= 0)) THEN
    rc = 4
    RETURN
   END IF

!  Layer thicknesses.  ZLE(:,:,0:km).
!  ----------------------------------
   DO k=1,km
    dZ(i1:i2,j1:j2,k) = zle(i1:i2,j1:j2,k-1)-zle(i1:i2,j1:j2,k)
   END DO

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
 
!  Number density
!  --------------
   nd(i1:i2,j1:j2,1:km)= nsuba*p(i1:i2,j1:j2,1:km)/ &
                        (rstar*T(i1:i2,j1:j2,1:km))
 
#ifdef DEBUG
    CALL pmaxmin('Rn: T     ',      T, qmin, qmax, iXj,   km, 1. )
    CALL pmaxmin('Rn: TSOIL1',  soilT, qmin, qmax, iXj,    1, 1. )
    CALL pmaxmin('Rn: FRACI ',fracIce, qmin, qmax, iXj,    1, 1. )
    CALL pmaxmin('Rn: ZLE   ',    zle, qmin, qmax, iXj, km+1, 1. )
    CALL pmaxmin('Rn: dZ    ',     dZ, qmin, qmax, iXj,   km, 1. )
    CALL pmaxmin('Rn: Edge p',     pe, qmin, qmax, iXj,   km, 1. )
    CALL pmaxmin('Rn: Mid p ',      p, qmin, qmax, iXj,   km, 1. )
    CALL pmaxmin('Rn: Numden',     nd, qmin, qmax, iXj,   km, 1. )
#endif

!  Convert Radon from mole fraction to number density
!  --------------------------------------------------
   w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km) = &
                  w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km)*nd(i1:i2,j1:j2,1:km)

!  Clear surface flux diagnostic
!  -----------------------------
   IF(ASSOCIATED(Rn_emis)) Rn_emis(i1:i2,j1:j2) = 0.00

!  Fraction of emissions to accept from each box
!  ---------------------------------------------
   F(i1:i2,j1:j2) = 0.00

!  Find land boxes from regional mask file.
!  ----------------------------------------
   CALL setLandMask

!  For the global intantiation, include ocean emissions.
!  -----------------------------------------------------
   IF(gcRn%instance == 1) THEN
    WHERE(mask(i1:i2,j1:j2) == 0) F(i1:i2,j1:j2) = 1.00-fracIce(i1:i2,j1:j2)
   END IF

!  Assume frozen soil emits no radon
!  ---------------------------------
   WHERE(mask(i1:i2,j1:j2) == 1 .AND. soilT(i1:i2,j1:j2) < 273.00) mask(i1:i2,j1:j2) = 0

!  Account for bad-valued soil temperatures
!  ----------------------------------------
   WHERE(mask(i1:i2,j1:j2) == 1 .AND. soilT(i1:i2,j1:j2) > 500.00) mask(i1:i2,j1:j2) = 0

!  Finalize fraction from land boxes.
!  ----------------------------------
   WHERE(mask(i1:i2,j1:j2) == 1) F(i1:i2,j1:j2) = 1.00

!  Place emissions into the surface layer, adding the number of
!  atoms released in one time step to the surface layer number density.
!  --------------------------------------------------------------------
   w_c%qa(nbeg)%data3d(i1:i2,j1:j2,km)=w_c%qa(nbeg)%data3d(i1:i2,j1:j2,km)+F(i1:i2,j1:j2)* &
                                       toND*gcRn%ScheryEmission(i1:i2,j1:j2)*cdt/dZ(i1:i2,j1:j2,km)

!  Diagnostic emissions, kg m^{-2} s^{-1}.
!  ---------------------------------------
   IF(ASSOCIATED(Rn_emis)) THEN
    Rn_emis(i1:i2,j1:j2) = F(i1:i2,j1:j2)*toND*mwtRn*gcRn%ScheryEmission(i1:i2,j1:j2)/nsuba
   END IF

!  Diagnostic loss, vertically integrated, kg m^{-2} s^{-1}.  Compute
!  before applying radioactive decay to the three-dimensional radon field.
!  -----------------------------------------------------------------------
   n = gcRn%instance
   decadence = EXP(-gcRn%decayConstant*cdt)
   IF(ASSOCIATED(Rn_loss)) Rn_loss(i1:i2,j1:j2) = 0.00
   DO k = 1, km

    IF(ASSOCIATED(Rn_loss)) &
    Rn_loss(i1:i2,j1:j2) = w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k)*(1.00-decadence)* &
   	                   mwtRn*dZ(i1:i2,j1:j2,k)/(nsuba*cdt)

!  Apply radioactive decay, q(f) = q(i)EXP(-c delta t), to number density.
!  -----------------------------------------------------------------------
    w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k) = w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k)*decadence

   END DO ! Next layer, k

!  Column burden in kg m^{-2}
!  --------------------------
   n = gcRn%instance 
   IF(ASSOCIATED(Rn_column)) then
    Rn_column(i1:i2,j1:j2) = 0.
    DO k = 1, km
     Rn_column(i1:i2,j1:j2) = Rn_column(i1:i2,j1:j2) +  w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k)* &
   			      mwtRn*dZ(i1:i2,j1:j2,k)/nsuba
    END DO
   END IF

!  Return to mole fraction
!  -----------------------
   w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km)=w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km)/nd(i1:i2,j1:j2,1:km)

!  Surface concentration in mole fraction
!  --------------------------------------
   n = gcRn%instance 
   IF(ASSOCIATED(Rn_surface)) Rn_surface(i1:i2,j1:j2) = w_c%qa(nbeg)%data3d(i1:i2,j1:j2,km)

#ifdef DEBUG
   n = gcRn%instance 
   IF(ASSOCIATED(   Rn_emis)) &
   CALL pmaxmin(   'Rn_emis',	 Rn_emis(i1:i2,j1:j2), qmin, qmax, iXj, 1, 1. )
   IF(ASSOCIATED(   Rn_loss)) &
   CALL pmaxmin(   'Rn_loss',	 Rn_loss(i1:i2,j1:j2), qmin, qmax, iXj, 1, 1. )
   IF(ASSOCIATED( Rn_column)) &
   CALL pmaxmin( 'Rn_column',  Rn_column(i1:i2,j1:j2), qmin, qmax, iXj, 1, 1. )
   IF(ASSOCIATED(Rn_surface)) &
   CALL pmaxmin('Rn:surface', Rn_surface(i1:i2,j1:j2), qmin, qmax, iXj, 1, 1. )
#endif

!  Housekeeping
!  ------------
   DEALLOCATE(F, mask, dZ, nd, p, pe, STAT=ier(1))

   RETURN

CONTAINS

  SUBROUTINE setLandMask
   IMPLICIT NONE
   INTEGER :: ios, k
   INTEGER, ALLOCATABLE :: regionNumbers(:),flag(:)

   k = 32
   ALLOCATE(regionNumbers(k),flag(k),STAT=ios)
   IF ( ios /= 0 ) CALL die ( myname, ": Cannot allocate for masking.")

! Obtain region numbers from delimited list of integers
! -----------------------------------------------------
   regionNumbers(:) = 0
   CALL Chem_UtilExtractIntegers(gcRn%regionsString,k,regionNumbers,RC=ios)
   IF ( ios /= 0 ) CALL die ( myname, "_setLandMask: Unable to extract integers for regionNumbers.")

! How many integers were found?
! -----------------------------
   flag(:) = 1
   WHERE(regionNumbers(:) == 0) flag(:) = 0
   k = SUM(flag)
   DEALLOCATE(flag,STAT=ios)
   IF ( ios /= 0 ) CALL die ( myname, "_setLandMask: Cannot dallocate flag.")

! Set local mask to 1 where gridMask matches each integer (within precision!).
! ----------------------------------------------------------------------------
   mask(i1:i2,j1:j2) = 0
   IF(regionNumbers(1) == -1) THEN
    WHERE(gcRn%regionMask(i1:i2,j1:j2) /= 0) mask(i1:i2,j1:j2) = 1
   ELSE
    DO ios=1,k
     WHERE(     regionNumbers(ios)-0.01 <= gcRn%regionMask(i1:i2,j1:j2) .AND. &
           gcRn%regionMask(i1:i2,j1:j2) <= regionNumbers(ios)+0.01) mask(i1:i2,j1:j2) = 1
    END DO
   END IF

   RETURN
  END SUBROUTINE setLandMask

 END SUBROUTINE Rn_GridCompRun1_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Rn_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   SUBROUTINE Rn_GridCompFinalize1_ ( gcRn, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:

   TYPE(Rn_GridComp1), INTENT(INOUT) :: gcRn   ! Grid Component

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

   CHARACTER(LEN=*), PARAMETER :: myname = 'Rn_GridCompFinalize'
   INTEGER :: ios

   DEALLOCATE ( gcRn%RnsfcFlux,  gcRn%regionMask, gcRn%ScheryEmission, STAT=ios )
   rc = 0
   IF ( ios /= 0 ) rc = 1

   RETURN

 END SUBROUTINE Rn_GridCompFinalize1_

 END MODULE Rn_GridCompMod

!-----------------------------------------------------------------------

!                     Single Instance Wrapper

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Rn_SingleInstance_ --- Runs single instance of method
!
! !INTERFACE:
!
  subroutine Rn_SingleInstance_ ( Method_, instance, &
                                  gcRn, w_c, impChem, expChem, &
                                  nymd, nhms, cdt, rc )

! !USES:

  Use Rn_GridCompMod
  Use ESMF_Mod
  Use MAPL_Mod
  Use Chem_Mod 

  IMPLICIT NONE

! !INPUT PARAMETERS:

!  Input "function pointer"
!  -----------------------
   interface 
     subroutine Method_ (gc, w, imp, exp, ymd, hms, dt, rcode )
       Use Rn_GridCompMod
       Use ESMF_Mod
       Use MAPL_Mod
       Use Chem_Mod 
       type(Rn_GridComp1),  intent(inout)  :: gc
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

   TYPE(Rn_GridComp1), INTENT(INOUT) :: gcRn    ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Finalizes the Rn Grid Component. Multiple instance
!               version.
!
! !REVISION HISTORY:
!
!  27Feb2008  da Silva  Introduced multiple instances
!  12Apr2008  Nielsen   Configured for radon.
!
!EOP
!-------------------------------------------------------------------------

  integer n_Rn, i_Rn, j_Rn

! Save overall Rn indices
! -----------------------
  n_Rn = w_c%reg%n_Rn
  i_Rn = w_c%reg%i_Rn
  j_Rn = w_c%reg%j_Rn
  
! Customize indices for this particular instance
! ----------------------------------------------
  w_c%reg%n_Rn = 1
  w_c%reg%i_Rn = i_Rn + instance - 1
  w_c%reg%j_Rn = i_Rn + instance - 1
  
! Execute the instance method
! ---------------------------
  call Method_ ( gcRn, w_c, impChem, expChem, &
                 nymd, nhms, cdt, rc )

! Restore the overall Rn indices
! ------------------------------
  w_c%reg%n_Rn = n_Rn
  w_c%reg%i_Rn = i_Rn
  w_c%reg%j_Rn = j_Rn

  end subroutine Rn_SingleInstance_

!-----------------------------------------------------------------------
