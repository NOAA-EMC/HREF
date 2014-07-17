#include "MAPL_Generic.h"
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  CFC_GridCompMod --- CFC Grid Component Class
!
! !INTERFACE:
!

   MODULE  CFC_GridCompMod

! !USES:

   USE ESMF_Mod
   USE MAPL_Mod
   USE Chem_Mod 	     ! Chemistry Base Class
   USE Chem_StateMod	     ! Chemistry State
   USE Chem_ConstMod, ONLY: grav
   USE Chem_UtilMod	     ! I/O
   USE m_inpak90	     ! Resource file management
   USE m_die, ONLY: die

   IMPLICIT NONE

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  CFC_GridComp       ! The CFC object 

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  CFC_GridCompInitialize
   PUBLIC  CFC_GridCompRun
   PUBLIC  CFC_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the CFC Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  01Aug2006 da Silva  Extensions for GEOS-5.
!   1Jan2008  Nielsen  CFC-12 configuration for ARCTAS.
!   8Feb2008  Nielsen  Standard configuration call(s) from AeroChem.
!
!EOP
!-------------------------------------------------------------------------

  TYPE CFC_GridComp

    CHARACTER(LEN=255) :: name

! For CFC-12 photolysis
! ---------------------
    INTEGER :: nlam
    INTEGER :: nsza
    INTEGER :: numo3
    INTEGER :: nx
    INTEGER :: nxdo
    INTEGER :: nts

    REAL(KIND=4), POINTER :: sdat(:,:,:,:)
    REAL(KIND=4), POINTER :: xtab(:,:)
    REAL(KIND=4), POINTER :: o3_tab(:,:)
    REAL(KIND=4), POINTER :: sza_tab(:)

    REAL, POINTER :: CFCsfcFlux(:,:)	 ! CFC-12 surface flux kg m^-2 s^-1
    REAL, POINTER :: CFCloss(:,:,:,:)	 ! CFC loss due to photolysis m^-3 s^-1

  END TYPE CFC_GridComp

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CFC_GridCompInitialize --- Initialize CFC_GridComp
!
! !INTERFACE:
!

   SUBROUTINE CFC_GridCompInitialize( gcCFC, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   INTEGER, INTENT(IN) :: nymd, nhms	       ! time
   REAL,    INTENT(IN) :: cdt		       ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(CFC_GridComp), INTENT(INOUT) :: gcCFC   ! Grid Component
   TYPE(ESMF_State), INTENT(INOUT)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(INOUT)  :: expChem  ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the CFC Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  31May2005  Nielsen  Mods for 7 CO bins, 5 region masks
!  04Nov2005     Bian  CO tagged to 4 regions 
!                      (global, North America, South America, and Africa)
!                      for CR-AVE
!  12Feb2005  Nielsen  8 regions for INTEX-B 2006
!   1Jan2008  Nielsen  CFC-12 configuration for ARCTAS
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'CFC_GridCompInitialize'
   
   CHARACTER(LEN=255) :: rcfilen = 'CFC_GridComp.rc'

   CHARACTER(LEN=255) :: dir4files
   CHARACTER(LEN=255) :: fnO2Jdat
   CHARACTER(LEN=255) :: fnO3SZA
   CHARACTER(LEN=255) :: fnXsectJPL
   CHARACTER(LEN=255) :: eFileName

   INTEGER :: ier(128)
   INTEGER :: i, i1, i2, im, j1, j2, jm, km, nbins

   gcCFC%name = 'CFC-12 Chemistry for ARCTAS'

   rc = 0
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
   
   nbins = w_c%reg%n_CFC
   ier(:)=0

!  Initialize photolysis variables
!  -------------------------------
   gcCFC%nlam  =  79
   gcCFC%nsza  =  20
   gcCFC%numo3 =  12
   gcCFC%nx    =  35
   gcCFC%nxdo  =  33
   gcCFC%nts   = 200

!  Load resource file
!  ------------------
   CALL I90_loadf ( TRIM(rcfilen), ier(1) )
   IF ( ier(1) .NE. 0 ) THEN
    rc = 10
    RETURN
   END IF

   CALL I90_label ( 'directory:', ier(30) )
   CALL I90_Gtoken( dir4files, ier(31) )

   CALL I90_label ( 'O2Jtable:', ier(32) )
   CALL I90_Gtoken( fnO2Jdat, ier(33) )

   CALL I90_label ( 'O3&SZAtables:', ier(36) )
   CALL I90_Gtoken( fnO3SZA, ier(37) )

   CALL I90_label ( 'JPLXsections:', ier(40) )
   CALL I90_Gtoken( fnXsectJPL, ier(41) )

   CALL I90_label ( 'CFC_emission_filename:', ier(50) )
   CALL I90_Gtoken( eFileName, ier(51) )

   IF( ANY( ier(1:128) /= 0 ) ) THEN
    rc = 12
    RETURN
   END IF
   ier(:)=0

! Allocate space for grid-size dependent arrays
! ---------------------------------------------
   ALLOCATE(gcCFC%sdat(gcCFC%nsza,gcCFC%numo3,km,gcCFC%nlam), stat=ier( 9) )
   ALLOCATE(	            gcCFC%xtab(gcCFC%nlam,gcCFC%nts), stat=ier(10) )
   ALLOCATE(	                gcCFC%o3_tab(gcCFC%numo3,km), stat=ier(11) )
   ALLOCATE(	                   gcCFC%sza_tab(gcCFC%nsza), stat=ier(12) )
   ALLOCATE(                   gcCFC%CFCsfcFlux(i1:i2,j1:j2), stat=ier(13) )
   ALLOCATE(           gcCFC%CFCloss(i1:i2,j1:j2,1:km,nbins), stat=ier(14) )

   IF( ANY( ier(1:128) /= 0 ) ) THEN
    rc = 14
    RETURN
   END IF
   ier(:)=0

!  Acquire the CFC-12 emissions
!  ----------------------------
   CALL Chem_UtilMPread ( TRIM(eFileName), 'CFC-12_EMISSION', 20080101, &
   			  120000, i1, i2, 0, im, j1, j2, 0, jm, 0, &
   			  var2d=gcCFC%CFCsfcFlux, cyclic=.true., &
   			  grid=w_c%grid_esmf )

!  Read the tables
!  ---------------
   CALL rdPhotFiles(km,dir4files,fnO2Jdat,fnO3SZA,fnXsectJPL)

   RETURN
   CONTAINS
   SUBROUTINE rdPhotFiles(km,dir,fnO2Jdat,fnO3SZA,fnXsectJPL)
!---------------------------------------------------------------------------
!
! Read several external files for the photolysis.
!
! Input parameters:
!
! km    Grid dimensions
! dir	Directory on which these files reside
! fns   File names
!
! Output parameters:
!
!  None.
!
! Restrictions:
!
!  This runs on each processor
!
!  This version requires input data at jnp latitudes.
!
!-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: km
      
      CHARACTER(LEN=*), INTENT(IN) :: dir
      CHARACTER(LEN=*), INTENT(IN) :: fnO2Jdat
      CHARACTER(LEN=*), INTENT(IN) :: fnO3SZA
      CHARACTER(LEN=*), INTENT(IN) :: fnXsectJPL

      REAL(KIND=4), ALLOCATABLE :: dxtab(:,:,:)

      INTEGER :: i, j, k, l, ierr, iunit, iuchem, kReverse
      INTEGER :: npr_in, nlam_in, nsza_in, no3_in
      REAL :: deg2Rad, pi
      REAL (KIND=4) :: pr_tab(km)
      REAL (KIND=4) :: rlam(gcCFC%nlam)

      LOGICAL :: exists,open,found

      pi = 4.00*ATAN(1.00)
      deg2Rad = pi/180.00

! Find an available logical unit 
! ------------------------------
      found=.FALSE.
      iunit=11

      DO WHILE (.NOT. found .AND. iunit <= 99)
       INQUIRE(UNIT=iunit,EXIST=exists,OPENED=open)
       IF(exists .AND. .NOT. open) THEN
        found=.TRUE.
        iuchem=iunit
       END IF
       iunit=iunit+1
      END DO

      IF(.NOT. found) THEN
       WRITE(*,FMT="(/,'rdPhotFiles: No available logical units.')")
       STOP
      ELSE
       IF(MAPL_AM_I_ROOT()) THEN
        WRITE(*,FMT="(' ')")
        WRITE(*,FMT="(' ','rdPhotFiles: Reading from UNIT ',I3)") iuchem
	END IF
      END IF
      
! Read in sdat(nsza,numo3,levels,nlam)
! ------------------------------------
      OPEN(iuchem,FILE=TRIM(dir)//'/'//TRIM(fnO2Jdat),STATUS='old', &
            FORM='unformatted',ACTION='read')
      READ(iuchem) gcCFC%sdat
      CLOSE(iuchem)
  
! Read solar zenith angle and O3 references after checking sizes
! --------------------------------------------------------------
      OPEN(iuchem,FILE = TRIM(dir)//'/'//TRIM(fnO3SZA),STATUS='old', &
           FORM='unformatted',ACTION='read')
      READ(iuchem) npr_in,nlam_in,nsza_in,no3_in
      READ(iuchem) pr_tab
      READ(iuchem) rlam
      READ(iuchem) gcCFC%sza_tab
      READ(iuchem) gcCFC%o3_tab
      CLOSE(iuchem)
      IF(( npr_in .NE.         km) .OR. (nlam_in .NE.  gcCFC%nlam) .OR. &
         (nsza_in .NE. gcCFC%nsza) .OR. ( no3_in .NE. gcCFC%numo3)) THEN
         PRINT *,'rdPhotFiles: Array sizes of table do not match ', &
                 ' those expected:',npr_in,km,nlam_in,gcCFC%nlam, &
                 nsza_in,gcCFC%nsza,no3_in,gcCFC%numo3
     	 STOP
      END IF
 
! Convert sza_tab(nsza) to radians
! --------------------------------
      DO i=1,gcCFC%nsza
       gcCFC%sza_tab(i) = gcCFC%sza_tab(i)*deg2Rad
      END DO

! Reverse sdat(nsza,numo3,levels,nlam) in the vertical to accomodate GEOS-5
! -------------------------------------------------------------------------
      DO l=1,gcCFC%nlam				       
       DO j=1,gcCFC%numo3				       
        DO i=1,gcCFC%nsza				       
         pr_tab(1:km) = gcCFC%sdat(i,j,1:km,l)		       
         DO k=1,km					       
          kReverse = km-k+1				       
          gcCFC%sdat(i,j,k,l) = pr_tab(kReverse)
         END DO						       
        END DO						       
       END DO						       
      END DO						       

! Reverse o3_tab(numo3,km) in the vertical to accomodate GEOS-5
! -------------------------------------------------------------
      DO j=1,gcCFC%numo3					 
       pr_tab(1:km) = gcCFC%o3_tab(j,1:km)			 
       DO k=1,km						 
        kReverse = km-k+1					 
        gcCFC%o3_tab(j,k) = pr_tab(kReverse)
       END DO 						 
      END DO  						 

      ALLOCATE(dxtab(gcCFC%nlam,gcCFC%nts,gcCFC%nx),STAT=ierr)

! JPL cross sections
! ------------------ 
      OPEN(iuchem,FILE=TRIM(dir)//'/'//TRIM(fnXsectJPL), &
           STATUS='old',ACTION='read',FORM='unformatted')
      READ(iuchem) dxtab
      CLOSE(iuchem)

! Need only #25
! -------------
      k=25
      DO j=1,gcCFC%nts
       DO i=1,gcCFC%nlam
        gcCFC%xtab(i,j) = dxtab(i,j,k)
       END DO
      END DO

      DEALLOCATE(dxtab,STAT=ierr)

      IF(MAPL_AM_I_ROOT()) THEN
       print *,'rdPhotFiles: Done'
       print *,' '
      END IF

      RETURN
      END SUBROUTINE rdPhotFiles

   END SUBROUTINE CFC_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CFC_GridCompRun --- The CFC Driver 
!
! !INTERFACE:
!

   SUBROUTINE CFC_GridCompRun( gcCFC, w_c, impChem, expChem, nymd, nhms, &
                               cdt, rc)

! !USES:

  IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:

   TYPE(CFC_GridComp), INTENT(INOUT) :: gcCFC   ! Grid Component
   TYPE(Chem_Bundle), INTENT(INOUT) :: w_c	! Chemical tracer fields   

! !INPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(INOUT) :: impChem   ! Import State
   INTEGER, INTENT(IN) :: nymd, nhms	        ! time
   REAL,    INTENT(IN) :: cdt		        ! chemical timestep (secs)

! !OUTPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(INOUT) :: expChem   ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -

   CHARACTER(LEN=*), PARAMETER :: myname = 'CFC_GridCompRun'
   CHARACTER(LEN=*), PARAMETER :: Iam = myname

   INTEGER :: ier(128)
   INTEGER ::  i1, i2, im, iXj, j1, j2, jm, km, status
   INTEGER ::  i, indt, j, k, m, n, nbeg, nbins, nend
   REAL :: o3c, qmin, qmax, r, rg, szan

!  Imports
!  -------
   REAL, POINTER, DIMENSION(:,:,:) ::  T  => null()
   REAL, POINTER, DIMENSION(:,:,:) ::  O3 => null()
   REAL, POINTER, DIMENSION(:,:)   ::  tropp => null()

!  Local Variables
!  ---------------
   REAL, PARAMETER :: badVal=2.00E+05
   REAL, PARAMETER :: grav=9.80
   REAL, PARAMETER :: mwtAir=28.97
   REAL, PARAMETER :: mwtCFC12=120.917
   REAL, PARAMETER :: Nsuba=6.022E+26
   REAL, PARAMETER :: rstar=8.3143E+03
   REAL, PARAMETER :: O3abv80km = 1.10E+15 !m^{-2}

   REAL, ALLOCATABLE :: emit2vmr(:,:)
   REAL, ALLOCATABLE :: tropPa(:,:)
   REAL, ALLOCATABLE :: pPa(:,:,:)
   REAL, ALLOCATABLE :: nd(:,:,:)
   REAL, ALLOCATABLE :: O3Col(:,:,:)
   REAL, ALLOCATABLE :: photoRate(:,:,:)
   REAL, ALLOCATABLE :: s(:,:,:,:)

! Disable the ACG'd CFC_GetPointer___.h for now. [Maybe fix it soon.]
! -------------------------------------------------------------------
#define EXPORT     expChem
#define ptrCFCEM   CFC_emis
#define ptrCFCLS   CFC_loss
#define ptrCFCCL   CFC_column

!JEN#include "CFC_GetPointer___.h"

!  Bin sizes
!  ---------
   integer, parameter		   :: NBIN_CFCEM = 1 ! CFC Emission
   integer, parameter		   :: NBIN_CFCLS = 2 ! CFC Loss due to photolysis
   integer, parameter		   :: NBIN_CFCCL = 2 ! CFC Column

!  Bin-indexed Chem Arrays
!  -----------------------
   type(Chem_Array), target	   ::	 CFCEM(NBIN_CFCEM) ! Export: CFC Surface flux
   type(Chem_Array), pointer	   :: ptrCFCEM(:)	   ! Export: CFC Surface flux
   type(Chem_Array), target	   ::	 CFCLS(NBIN_CFCLS) ! Export: CFC Loss due to photolysis
   type(Chem_Array), pointer	   :: ptrCFCLS(:)	   ! Export: CFC Loss due to photolysis
   type(Chem_Array), target	   ::	 CFCCL(NBIN_CFCCL) ! Export: CFC Column
   type(Chem_Array), pointer	   :: ptrCFCCL(:)	   ! Export: CFC Column

!  Local array referencing the Import/Export states
!  ------------------------------------------------
   type(Chem_Array), target	   ::	 CFC12S ! Export: Stratospheric CFC-12 (CCl2F2)
   type(Chem_Array), pointer	   :: ptrCFC12S ! Export: Stratospheric CFC-12 (CCl2F2)
   type(Chem_Array), target	   ::	 CFC12T ! Export: Tropospheric CFC-12 (CCl2F2)
   type(Chem_Array), pointer	   :: ptrCFC12T ! Export: Tropospheric CFC-12 (CCl2F2)

!  Get pointers to data in state
!  -----------------------------
   ptrCFC12S => CFC12S   ! Stratospheric CFC-12 (CCl2F2)
   call MAPL_GetPointer ( EXPORT, CFC12S%data3d,  'CFC12S', RC=STATUS )
   VERIFY_(STATUS)

   ptrCFC12T => CFC12T   ! Tropospheric CFC-12 (CCl2F2)
   call MAPL_GetPointer ( EXPORT, CFC12T%data3d,  'CFC12T', RC=STATUS )
   VERIFY_(STATUS)

   ptrCFCEM => CFCEM	 ! CFC-12 Surface flux
   call MAPL_GetPointer ( EXPORT, CFCEM(1)%data2d,  'CFC12EM', RC=STATUS )
   VERIFY_(STATUS)

   ptrCFCLS => CFCLS	 ! CFC-12 Loss due to photolysis
   call MAPL_GetPointer ( EXPORT, CFCLS(1)%data3d,  'CFC12SLS', RC=STATUS )
   VERIFY_(STATUS)
   call MAPL_GetPointer ( EXPORT, CFCLS(2)%data3d,  'CFC12TLS', RC=STATUS )
   VERIFY_(STATUS)

   ptrCFCCL => CFCCL	 ! CFC-12 Column mass density
   call MAPL_GetPointer ( EXPORT, CFCCL(1)%data2d,  'CFC12SCL', RC=STATUS )
   VERIFY_(STATUS)
   call MAPL_GetPointer ( EXPORT, CFCCL(2)%data2d,  'CFC12TCL', RC=STATUS )
   VERIFY_(STATUS)

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

   nbins = w_c%reg%n_CFC
   nbeg  = w_c%reg%i_CFC
   nend  = w_c%reg%j_CFC
   
!  Imports
!  -------
   call MAPL_GetPointer( impChem,     T,     'T', rc=ier(1) ) 
   call MAPL_GetPointer( impChem,    O3,    'O3', rc=ier(2) ) 
   call MAPL_GetPointer( impChem, tropp, 'TROPP', rc=ier(3) ) 

   IF(ANY(ier(:) /= 0 )) THEN
    rc = 1
    RETURN
   END IF
   ier(:)=0

#ifdef DEBUG
   CALL pmaxmin('    T',     T, qmin, qmax, iXj, km, 1. )
   CALL pmaxmin('   O3',    O3, qmin, qmax, iXj, km, 1. )
   CALL pmaxmin('TROPP', tropp, qmin, qmax, iXj,  1, 1. )
#endif

!  Allocate temporary workspace
!  ----------------------------
   ALLOCATE(    emit2vmr(i1:i2,j1:j2), STAT=ier(1))
   ALLOCATE(      tropPa(i1:i2,j1:j2), STAT=ier(1))
   ALLOCATE(      pPa(i1:i2,j1:j2,km), STAT=ier(2))
   ALLOCATE(       nd(i1:i2,j1:j2,km), STAT=ier(3))
   ALLOCATE(    O3Col(i1:i2,j1:j2,km), STAT=ier(4))
   ALLOCATE(photoRate(i1:i2,j1:j2,km), STAT=ier(5))

   IF(ANY(ier(:) /= 0 )) THEN
    rc = 10
    RETURN
   END IF
   ier(:)=0

!  Fix bad tropopause pressure values if they exist.
!  -------------------------------------------------
   CALL Chem_UtilTroppFixer(i2, j2, tropp, VERBOSE=.TRUE., &
                            NEWTROPP=tropPa, RC=STATUS)
   VERIFY_(STATUS)

!  Find the pressure at mid-layer
!  ------------------------------
   pPa(i1:i2,j1:j2,1) = w_c%grid%ptop + 0.50*w_c%delp(i1:i2,j1:j2,1)
   DO k = 2, km
    pPa(i1:i2,j1:j2,k) = pPa(i1:i2,j1:j2,k-1) + 0.50* &
                         (w_c%delp(i1:i2,j1:j2,k-1)+w_c%delp(i1:i2,j1:j2,k))
   END DO

!  Number density
!  --------------
   nd(i1:i2,j1:j2,1:km)= nsuba*pPa(i1:i2,j1:j2,1:km)/(rstar*T(i1:i2,j1:j2,1:km))

!  Compute the overlying ozone from mole fraction.  Result: m^{-2}
!  ---------------------------------------------------------------
   r = Nsuba*0.50/(mwtAir*grav)
   O3col(i1:i2,j1:j2,1) = O3abv80km + O3(i1:i2,j1:j2,1)*w_c%delp(i1:i2,j1:j2,1)*r
   DO k=2,km
    O3col(i1:i2,j1:j2,k) = O3col(i1:i2,j1:j2,k-1) + &
                             (O3(i1:i2,j1:j2,k-1) * w_c%delp(i1:i2,j1:j2,k-1) + &
                              O3(i1:i2,j1:j2,  k) * w_c%delp(i1:i2,j1:j2,  k))*r
   END DO

!  Enable the conversion from emission [kg CFC m^{-2} s^{-1}] 
!  to an incremental change in the mixing ratio [s^{-1}].
!  ----------------------------------------------------------
   emit2vmr(i1:i2,j1:j2) = mwtAir*grav/(mwtCFC12*w_c%delp(i1:i2,j1:j2,km))

!  Increment mixing ratio in surface layer of tropospheric CFC-12
!  --------------------------------------------------------------
   w_c%qa(nbeg+1)%data3d(i1:i2,j1:j2,km) = w_c%qa(nbeg+1)%data3d(i1:i2,j1:j2,km)+cdt* &
                                           gcCFC%CFCsfcFlux(i1:i2,j1:j2)*emit2vmr(i1:i2,j1:j2)

!  When tropospheric CFC-12 migrates to the stratosphere, reassign it
!  ------------------------------------------------------------------
   DO k = 1, km
    WHERE(pPa(i1:i2,j1:j2,k) < tropPa(i1:i2,j1:j2) .AND. &
          w_c%qa(nbeg+1)%data3d(i1:i2,j1:j2,k) > 0.00 )
     w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k) = w_c%qa(nbeg)%data3d(i1:i2,j1:j2,k) + &
                                          w_c%qa(nbeg+1)%data3d(i1:i2,j1:j2,k)
     w_c%qa(nbeg+1)%data3d(i1:i2,j1:j2,k) = 0.00
    END WHERE
   END DO

!  Convert CFC-12 to number density
!  --------------------------------
   DO n=nbeg,nend
    w_c%qa(n)%data3d(i1:i2,j1:j2,1:km) = w_c%qa(n)%data3d(i1:i2,j1:j2,1:km)* &
                                         nd(i1:i2,j1:j2,1:km)
   END DO

   ALLOCATE(s(gcCFC%nlam,i1:i2,j1:j2,1:km), STAT=ier(1))

!  Photolysis:  Loop over horizontal domain
!  ----------------------------------------
   DO j=j1,j2
    DO i=i1,i2

!  Solar zenith angle (radians).  w_c%cosz has no negative values,
!  which are required for correct interpolation in the S-dat tables.
!  -----------------------------------------------------------------
     IF(w_c%cosz(i,j) <= 1.00E-06) THEN
      szan = ACOS(-0.50)
     ELSE
      szan = ACOS(w_c%cosz(i,j))
     END IF

     DO k=1,km
      o3c = O3Col(i,j,k)*1.00E-04 !to cm^{-2}

!  Interpolate radiative flux function values.
!  Call getS even when sun is below the horizon.
!  ---------------------------------------------
      CALL getS(k,km,szan,o3c,s(:,i,j,k))
      indt = T(i,j,k)-148.5
      indt = MAX(1,indt)
      indt = MIN(indt,200)

!  Rate constant is sum over wavelengths
!  -------------------------------------
      photoRate(i,j,k) = SUM(s(1:gcCFC%nlam,i,j,k)*gcCFC%xtab(1:gcCFC%nlam,indt))

     END DO ! Layer
    END DO  ! Longitude
   END DO   ! Latitude

   DEALLOCATE(s, STAT=ier(2))
   m = 0

!  Apply photolysis
!  ----------------
   DO n=nbeg,nend
    m = m+1
    w_c%qa(n)%data3d(i1:i2,j1:j2,1:km) = w_c%qa(n)%data3d(i1:i2,j1:j2,1:km) - cdt * &
                                         w_c%qa(n)%data3d(i1:i2,j1:j2,1:km) * &
                                         photoRate(i1:i2,j1:j2,1:km)
    gcCFC%CFCloss(i1:i2,j1:j2,1:km,m) = w_c%qa(n)%data3d(i1:i2,j1:j2,1:km) * &
                                        photoRate(i1:i2,j1:j2,1:km)
   END DO
 
!  Return CFC-12 to mole fraction
!  ------------------------------
   DO n=nbeg,nend
    w_c%qa(n)%data3d(i1:i2,j1:j2,1:km) = w_c%qa(n)%data3d(i1:i2,j1:j2,1:km)/ &
                                         nd(i1:i2,j1:j2,1:km)
   END DO

!  Fill the export states.

!  CFC-12 Surface emission in kg m^{-2} s^{-1}
!  -------------------------------------------
   IF(ASSOCIATED(CFC_emis(1)%data2d)) &
    CFC_emis(1)%data2d(i1:i2,j1:j2) = gcCFC%CFCsfcFlux(i1:i2,j1:j2)

!  Loss due to photolysis: Currently m^{-3} s^(-1), and positive for loss.
!  -----------------------------------------------------------------------
   DO n = 1, nbins
    IF(ASSOCIATED(CFC_loss(n)%data3d)) &
     CFC_loss(n)%data3d(i1:i2,j1:j2,1:km) = gcCFC%CFCloss(i1:i2,j1:j2,1:km,n)
   END DO

!  Column burden in kg m(^-2)
!  --------------------------
   DO n = 1, nbins
     IF(ASSOCIATED(CFC_column(n)%data2d)) THEN
      CFC_column(n)%data2d(i1:i2,j1:j2) = 0.
      DO k = 1, km
       CFC_column(n)%data2d(i1:i2,j1:j2) &
        =   CFC_column(n)%data2d(i1:i2,j1:j2) &
          +   w_c%qa(nbeg+n-1)%data3d(i1:i2,j1:j2,k)*mwtCFC12/mwtAir &
            * w_c%delp(i1:i2,j1:j2,k)/grav
     END DO
    END IF
   END DO

!  Clean up
!  --------
   DEALLOCATE( emit2vmr, STAT=ier(1))
   DEALLOCATE(      pPa, STAT=ier(2))
   DEALLOCATE(       nd, STAT=ier(3))
   DEALLOCATE(    O3Col, STAT=ier(4))
   DEALLOCATE(photoRate, STAT=ier(5))

   IF(ANY(ier(:) /= 0 )) THEN
    rc = 99
    RETURN
   END IF
   ier(:)=0

   RETURN

   CONTAINS

   SUBROUTINE getS(ik,levels,sza,o3column,s)
! --------------------------------------------------------------------------
! NAME:
!   interp_s
! PURPOSE:
!   Interpolate s values for each wavelength in table to specified O3
!   col and zenith angles
! CATEGORY:
! CALLING SEQUENCE:
!   Call interp_s(nlam,sza,o3column,s)
! INPUTS:
!   nlam     --  number of wavelength intervals used
!   sza      --  zenith angle
!   o3column --  overhead o3 column value 
! OPTIONAL INPUT PARAMETERS:
! OUTPUTS:
!   s -- array of s values (nlam) for each wavelength 
!	 at model p-level interpolated to o3column and sza values
! INTERNAL VARIABLES
!   sza_tab -- values of sza corresponding to sdat table values
!   o3_tab  -- array of overhead O3 values at each p-level (numo3s,np_ctm)
!		used to index sdat
!   sdat    -- input array of values of radiative source function 
!	       (nzens,numo3,np_ctm,nlam) gridded to ctm p layers
! COMMON BLOCKS:
! SIDE EFFECTS:
! PROCEDURE:
!   bi-linear interpolation, for sza>94 s=0, for o3 out of range use min/max
! RESTRICTIONS:
! REQUIRED ROUTINES:
! MODIFICATION HISTORY: 
!   Created 930825 - SR Kawa
!   Modified 960710 for 28 levels and to handle J(O2) separately
!   1Jan2008  Nielsen  CFC-12 configuration for ARCTAS.
! --------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN) :: ik,levels
   REAL, INTENT(IN) :: sza,o3column 
   REAL, INTENT(OUT) :: s(gcCFC%nlam)

   INTEGER :: ijj,ikk,ikkm,il,is
   REAL :: omt,omu,t,u
       
! For each input solar zenith angle, find the first element of
! tabled sza_tab values that is greater than it.  Use this
! table element and previous table element to determined
! interpolated value.
! ------------------------------------------------------------
   DO is=1,gcCFC%nsza
      ijj = is 
      if(gcCFC%sza_tab(is) > sza) EXIT
   END DO
      
! Location is dark, set s/jo2=0        
! -----------------------------
   IF(sza > gcCFC%sza_tab(gcCFC%nsza)) THEN
      s(1:gcCFC%nlam) = 0.
   ELSE
      t = (sza-gcCFC%sza_tab(ijj-1))/(gcCFC%sza_tab(ijj)-gcCFC%sza_tab(ijj-1))
      omt = 1.-t
         
! For each input overhead o3 column find the first element
! of tabled o3_tab values that is > than it.  Use this
! table element and previous table element to determine
! interpolated value
! --------------------------------------------------------
      DO is=1,gcCFC%numo3
  	 ikk = is 
  	 IF (gcCFC%o3_tab(is,ik) > o3column) EXIT
      END DO 

      ikkm = ikk-1

      IF(ikk > 1 .AND. o3column <= gcCFC%o3_tab(gcCFC%numo3,ik)) THEN 
  	 u = (o3column-gcCFC%o3_tab(ikkm,ik))/ &
  	     (gcCFC%o3_tab(ikk,ik)-gcCFC%o3_tab(ikkm,ik))
  	 omu = 1.-u

! Bilinear interpolation at ik for each wavelength
! ------------------------------------------------
    	 DO il=1,gcCFC%nlam	    
    	    s(il) = omt*omu*gcCFC%sdat(ijj-1,ikkm,ik,il) &
    		 +t*omu*gcCFC%sdat(ijj,ikkm,ik,il) &
    		 +t*u*gcCFC%sdat(ijj,ikk,ik,il) &
    		 +omt*u*gcCFC%sdat(ijj-1,ikk,ik,il)
    	 END DO
    
! Extrapolate before table
! ------------------------
      ELSE IF (ikk == 1) THEN
   	 DO il=1,gcCFC%nlam
   	    s(il) = omt*gcCFC%sdat(ijj-1,1,ik,il)+t*gcCFC%sdat(ijj,1,ik,il)
   	 END DO

! Extrapolate past table
! ----------------------
      ELSE 
  	 DO il=1,gcCFC%nlam
  	    s(il) = omt*gcCFC%sdat(ijj-1,gcCFC%numo3,ik,il)+ &
  		    t*gcCFC%sdat(ijj,gcCFC%numo3,ik,il)
  	 END DO 
      END IF  
   END IF
   
   RETURN
   END SUBROUTINE getS

   END SUBROUTINE CFC_GridCompRun

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CFC_GridCompFinalize
!
! !INTERFACE:
!

   SUBROUTINE CFC_GridCompFinalize( gcCFC, w_c, impChem, expChem, &
                                    nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:

   TYPE(CFC_GridComp), INTENT(INOUT) :: gcCFC ! Grid Component

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

   CHARACTER(LEN=*), PARAMETER :: myname = 'CFC_GridCompFinalize'
   INTEGER :: ios

   rc = 0

   DEALLOCATE(gcCFC%sdat, gcCFC%xtab, gcCFC%o3_tab, gcCFC%sza_tab, &
              gcCFC%CFCloss, gcCFC%CFCsfcFlux, STAT=ios )

   IF( ios /= 0 ) THEN
    rc = 1
    IF(MAPL_AM_I_ROOT()) PRINT *,myname,': DEALLOCATE return code is ',ios
   END IF

   RETURN
   END SUBROUTINE CFC_GridCompFinalize

 END MODULE CFC_GridCompMod

