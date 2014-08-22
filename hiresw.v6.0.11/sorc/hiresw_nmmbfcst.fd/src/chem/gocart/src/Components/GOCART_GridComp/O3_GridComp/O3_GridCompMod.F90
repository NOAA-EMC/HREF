#ifdef GEOS5
#include "MAPL_Generic.h"
#endif

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  O3_GridCompMod
!
! Grid Component class for parameterized Chemistry for 7 species:
!
!  O3      Ozone
!  Ox      Odd oxygen
!  N2O     Nitrous oxide
!  CFC11   CFC-11 (CCl3F)
!  CFC12   CFC-12 (CCl2F2)
!  CH4     Methane
!  HCFC22  HCFC-22 (CHClF2)
!  
!
! !INTERFACE:
!

   MODULE  O3_GridCompMod

! !USES:

#ifdef GEOS5
   USE ESMF_Mod
   USE MAPL_Mod
#endif

   USE mod_diag, ONLY: iO3PARAM,iOX
   USE Chem_Mod 	     ! Chemistry Base Class
   USE Chem_StateMod	     ! Chemistry State
   USE Chem_UtilMod          ! Utilities
   USE m_inpak90	     ! Resource file management
   USE m_ioutil, only: luavail  

   IMPLICIT NONE

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  O3_GridComp       ! The O3 object 

!
! !PUBLIC MEMBER FUNCTIONS:
!

   PUBLIC  O3_GridCompInitialize
   PUBLIC  O3_GridCompRun
   PUBLIC  O3_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements a parameterized chemistry for several radiatively
!  active species: Ox, N2O, CFC11, CFC12, CH4, and HCFC22.
!  
!  WARNING: This routine is tested only with SC_GridComp turned OFF.
!
! !REVISION HISTORY:
!
!       2000 Nielsen   Initial coding
!   4Mar2005 Nielsen   Implementation of parameterized ozone chemistry
!  16Sep2005 da Silva  1) No longer gets nForO3/Ox from RC file - retrieves it
!                      from chem registry instead; 2) no longer enforces 
!                      1D decomp; only fills export if associated
!  20Sep2005 Nielsen   Added N2O, CFC-11, CFC-12, CH4, HCFC-22.
!
!EOP
!-------------------------------------------------------------------------

  TYPE O3_GridComp

  CHARACTER(LEN=255) :: name = "Parameterized ozone chemistry"

!  Items in O3_GridComp.rc

!  Diagnostic print outs

        LOGICAL :: verbose

! Ozone is derived from parameterized odd-oxygen. Ox must transported.
! gcO3%nForO3 specifies position in q [w_c%qa(nForOx)%data3d(:,:,:)] for Ox.

        INTEGER :: nForOx

! If gcO3%nForO3 is less than or equal to zero, then the ozone climatology 
! that is specified in ccmrun.namelist is used in the radiative transfer
! scheme.  If gcO3%nForO3 is greater than zero, then the model will replace
! the ozone climatology with the species [hopefully ozone!] that resides 
! in q(:,:,:,gcO3%nForO3).

        INTEGER :: nForO3

! There may be more than one active grid component module doing ozone 
! chemistry.  So, when gcO3%nForO3 is greater than zero the model must 
! determine which module will be allowed to award its modifications to
! q(:,:,:,gcO3%nForO3).  Each module doing ozone chemistry has its own
! linkO3 switch.  The user chooses the module that supplies the desired 
! ozone by setting the linkO3 switch to 1 in that module's .rc file.  
! To avoid ambiguity, one and only one linkO3 can be turned on.

        LOGICAL :: linkO3

! Is ozone that is passed to the physics driver [w_c%qa(nForO3)%data3d(:,:,:)] 
! in parts per million by volume?  Note: The radiative transfer scheme
! requires ozone to be in volume mixing ratio (mole fraction).

        LOGICAL :: O3inPPMV

! Time step length

        REAL :: cdt

  END TYPE O3_GridComp

CONTAINS

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  O3_GridCompInitialize --- Initialize O3_GridComp
!
! !INTERFACE:
!

   SUBROUTINE O3_GridCompInitialize ( gcO3, w_c, impChem, expChem, &
                                      nymd, nhms, tdt, rc )

! !USES:

!  ----------------------------------------------------------
!  This is not thread safe. Please fix it as soon as possible.
!  ------------------------------
   USE O3_data, ONLY : prod, loss
!  ----------------------------------------------------------

  IMPLICIT none

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), INTENT(in) :: w_c     ! Chemical tracer fields, delp, +
   INTEGER, INTENT(in) :: nymd, nhms	    ! time
   REAL,    INTENT(in) :: tdt		    ! chemistry time step (secs)


! !OUTPUT PARAMETERS:

   TYPE(O3_GridComp), INTENT(inout) :: gcO3     ! Grid Component
   TYPE(ESMF_State), INTENT(inout)  :: impChem  ! Import State
   TYPE(ESMF_State), INTENT(inout)  :: expChem  ! Export State
   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the O3 Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!   4Mar2005 Nielsen   Implementation of parameterized ozone chemistry
!  20Sep2005 Nielsen   Added N2O, CFC-11, CFC-12, CH4, HCFC-22.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'O3_GridCompInitialize'

   CHARACTER(LEN=255) :: rcfilen = 'O3_GridComp.rc'

   CHARACTER(LEN=255) :: filename

   INTEGER :: ios, n
   INTEGER, ALLOCATABLE :: ier(:)
   INTEGER :: i, i1, i2, im, j1, j2, jm, km

   gcO3%name = 'Four-species parameterized chemistry'

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

   CALL init_()
   IF ( rc /= 0 ) RETURN
   
   ier(:)=0

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
   CALL I90_label ( 'verbose:', ier(2) )
   i = I90_gint( ier(3) )
   IF(i == 0) THEN
    gcO3%verbose = .FALSE.
   ELSE
    gcO3%verbose = .TRUE.
   END IF

   CALL I90_label ( 'O3inPPMV:', ier(4) )
   i = I90_gint( ier(5) )
   IF(i == 0) THEN
    gcO3%O3inPPMV = .FALSE.
   ELSE
    gcO3%O3inPPMV = .TRUE.
   END IF

   CALL I90_label ( 'linkO3:', ier(6) )
   i = I90_gint( ier(7) )
   IF(i == 0) THEN
    gcO3%linkO3 = .FALSE.
   ELSE
    gcO3%linkO3 = .TRUE.
   END IF

#ifdef VERY_DANGEROUS

! ------------------------------------------------------------------------------
! Yes, dangerous, but ...
!
! Parsing nForO3 from O3GridComp.rc instead of from Chem_Registry.rc
! was implemented in GEOS-4 to enable the running of more than one
! chemistry grid component with an ozone.  In GEOS-4's fvgcm.F a rather
! cumbersome logic was implemented that allows the user to choose which of
! the O3s will be used in the radiative transfer.  Read the comments in
! O3_GridCompInitialize (above) to gain insight.
!
! This can (should?) be changed, but it would destroy backward compatability.
! On the other hand, it is not probable that multiple O3-containing grid 
! components will ever be run in the same experiment.  
! ------------------------------------------------------------------------------

   CALL I90_Label ( 'nForO3:', ier(8) )
   gcO3%nForO3 = I90_Gint( ier(9) )

   CALL I90_Label ( 'nForOx:', ier(10) )
   gcO3%nForOx = I90_Gint( ier(11) )

#else

   gcO3%nForO3     = w_c%reg%i_O3
   gcO3%nForOx     = w_c%reg%i_O3 + 1

#endif

   CALL I90_label ( 'PandLFile:', ier(12) )
   CALL I90_Gtoken( filename, ier(13) )

   IF( ANY( ier(1:128) /= 0 ) ) THEN
    CALL final_(11)
    RETURN
   END IF
   ier(:)=0

#ifndef GEOS5
!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

! Select fields to be produced in the export state.
! -------------------------------------------------
   CALL Chem_StateSetNeeded ( expChem, iO3PARAM, .TRUE., ier( 1) )
   CALL Chem_StateSetNeeded ( expChem, iOX     , .TRUE., ier( 2) )

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut---/\

#endif

! Allocate space for production rates and loss frequencies
! --------------------------------------------------------
   n = w_c%reg%n_O3 -1 !!!!- w_c%reg%j_O3 - 2
   ALLOCATE( prod(jm,km,n,12), stat=ier( 3) )
   ALLOCATE( loss(jm,km,n,12), stat=ier( 4) )

   IF( ANY( ier(1:128) /= 0 ) ) THEN
    CALL final_(12)
    RETURN
   END IF
   ier(:)=0

!  Determine time step length (seconds)
!  ------------------------------------
   gcO3%cdt = tdt

!  Read the production rates and loss frequencies
!  ----------------------------------------------
   IF(gcO3%linkO3) CALL rd_pl6(jm,km,filename,gcO3)

   RETURN

CONTAINS

   SUBROUTINE init_()
   INTEGER :: ios, n
   n=128
   ios=0
   ALLOCATE ( ier(n), stat=ios )
   IF ( ios /= 0 ) rc = 100
   END SUBROUTINE init_

   SUBROUTINE final_(ierr)
   INTEGER :: ios, ierr
   DEALLOCATE ( ier, stat=ios )
   CALL I90_release()
   rc = ierr
   END SUBROUTINE final_

   SUBROUTINE rd_pl6(jm,km,filename,gcO3)
! ----------------------------------------------------------------------
!  Read the parameterized chemistry photochemical production rates and 
!  loss frequencies for the following six species in the given order:
!  Ox, N2O, CFC-11, CFC-12, CH4, and HCFC-22.
! ----------------------------------------------------------------------

!ALT this is very bad style. We need to store prod in an internal state!!!!
   USE O3_data, ONLY : prod, loss

   IMPLICIT NONE

   INTEGER, INTENT(IN) :: jm,km
   TYPE(O3_GridComp), INTENT(IN) :: gcO3     ! Grid Component
   CHARACTER(LEN=255), INTENT(IN) :: filename

   INTEGER :: iuchem,jnp,m,nl,nspecies,s,status,iunit
   REAL(KIND=4), ALLOCATABLE :: lat(:),prs(:),buffer(:,:)

! Find an available logical unit 
! ------------------------------
   iunit = luavail()

! Open the file for reading
! -------------------------
   OPEN(UNIT=iunit,FILE=TRIM(filename),STATUS='old', &
        FORM='unformatted',ACTION='read',ACCESS='sequential')

! Read number of latitudes, number of layers, and
! number of species that are contained in the file
! ------------------------------------------------
   READ(iunit) jnp,nl,nspecies
   
   ALLOCATE(lat(jnp),STAT=status)
   ALLOCATE(prs( nl),STAT=status)
   ALLOCATE(buffer(jnp,nl),STAT=status)

!ALT should check return status
   ALLOCATE(prod(jm,km,nspecies,12), stat=status)
   ALLOCATE(loss(jm,km,nspecies,12), stat=status)

! Read the latitudes and pressures for
! the production rates and loss frequencies
! -----------------------------------------
   READ(iunit) lat
   READ(iunit) prs

! For each month and species, read (1) monthly mean volume mixing
! ratio, (2) the production rates, and (3) the loss frequencies
! ---------------------------------------------------------------
   DO m=1,12
    DO s=1,nspecies
     READ(iunit) buffer
     READ(iunit) buffer
     prod(1:jm,1:km,s,m)=buffer(1:jm,1:km)
     READ(iunit) buffer
     loss(1:jm,1:km,s,m)=buffer(1:jm,1:km)
    END DO
   END DO

! Clean up
! --------   
   CLOSE(UNIT=iunit)

   DEALLOCATE(lat,STAT=status)
   DEALLOCATE(prs,STAT=status)
   DEALLOCATE(buffer,STAT=status)

   RETURN
   END SUBROUTINE rd_pl6

  END SUBROUTINE O3_GridCompInitialize

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  O3_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   SUBROUTINE O3_GridCompRun ( gcO3, w_c, impChem, expChem, &
                               nymd, nhms, tdt, rc )

! !USES:

  IMPLICIT none

! !INPUT/OUTPUT PARAMETERS:

   TYPE(O3_GridComp), INTENT(IN)    :: gcO3   ! Grid Component
   TYPE(Chem_Bundle), INTENT(INOUT) :: w_c    ! Chemical tracer fields   

! !INPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(IN) :: impChem    ! Import State
   INTEGER, INTENT(IN) :: nymd, nhms	      ! time
   REAL,    INTENT(IN) :: tdt		      ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(INOUT) :: expChem   ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine implements a parameterized chemistry for
!               ozone. 
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!   4Mar2005 Nielsen   Implementation of parameterized ozone chemistry
!  20Sep2005 Nielsen   Added N2O, CFC-11, CFC-12, CH4, HCFC-22.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'O3_GridCompRun'

!  Quantities to be exported
!  -------------------------
   REAL, POINTER, DIMENSION(:,:,:) :: O3PARAM => null()
   REAL, POINTER, DIMENSION(:,:,:) :: OX      => null()

!  Local
!  -----
   INTEGER :: i1, i2, im, ier(8)
   INTEGER :: j1, j2, jm, km

   ier(:)=0

!  Grid specs from Chem_Bundle%grid
!  --------------------------------
   rc = 0
   i1 = w_c%grid%i1
   i2 = w_c%grid%i2
   im = w_c%grid%im
   
   j1 = w_c%grid%j1
   j2 = w_c%grid%j2
   jm = w_c%grid%jm
   
   km = w_c%grid%km

   IF( ANY(ier(:) /= 0) ) THEN
    rc = 31 
    RETURN
   END IF
   ier(:)=0

#ifndef GEOS5
!\/--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---\/ 

!  Assign pointers to 3D arrays for the export state
!  -------------------------------------------------
    CALL Chem_StateGetArray3D( expChem, iO3PARAM, O3PARAM, ier( 1) )
    CALL Chem_StateGetArray3D( expChem, iOX     , OX     , ier( 2) )

!/\--- cut --- --- cut --- --- cut --- --- cut --- --- cut --- --- cut ---/\

#endif

!  Update the species with parameterized chemistry.
!  ------------------------------------------------
   CALL pc6(i1,i2,im,j1,j2,km,nhms,nymd,gcO3,w_c)

!  Fill the export state with updated mixing ratios
!  ------------------------------------------------
   if(associated(O3PARAM)) O3PARAM(i1:i2,j1:j2,:) = w_c%qa(gcO3%nForO3)%data3d(i1:i2,j1:j2,:)
   if(associated(OX)) OX(i1:i2,j1:j2,:) = w_c%qa(gcO3%nForOx)%data3d(i1:i2,j1:j2,:)

   RETURN

CONTAINS

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !SUBROUTINE pc6
!
!  Run the parameterized chemistry for the six species: Ox, N2O, CFC-11,
!  CFC-12, CH4, and HCFC-22.  Ozone is derived from Ox.
!
! !INTERFACE:
!
   SUBROUTINE pc6(i1,i2,im,j1,j2,km,nhms,nymd,gcO3,w_c)

! !USES:

  USE O3_data, ONLY : prod, loss
!#if defined( SPMD )
  USE mod_comm, ONLY : gid
!#endif

  IMPLICIT NONE

! !DESCRIPTION:
!
!  This module implements a parameterized chemistry for ozone.  The
!  file that contains the production rates and loss frequencies, however,
!  has coefficients for 4 species:  Nitrous oxide, methane, odd-oxygen, 
!  and sulfur hexaflouride.
!
!  Advection produces the "intermediate" constituent distribution
!  before this routine is called.
!  
!  Important parameters
!
!  i1,i2,j1,j2  Latitude, longitude slice limits
!	 im,km  Number of longitudes,layers
!	  nhms  Time of day, hhmmss
!	  nymd  Date, ccyymmdd
!	   pPa  Pressures [Pa]
!	  loss  Loss frequencies, [1/sec]
!	  prod  Production rates, [parts/sec]
!         gcO3  O3 grid component
!        w_c%q  Constituent array
!           Ox  Parameterized odd oxygen
!        ro3ox  Ozone-to-odd oxygen ratio
!
!  USAGE NOTES
!
!  The coefficients are assumed to be provided once per month. 
!  Interpolation to the current day's values is based on using the 
!  previous and the current month's values before midmonth, and 
!  the current and the next month's values after midmonth.  Setting
!  verbose=.TRUE. below allows the user to check the date and time 
!  and the weighting.
!
!  The resulting O3 mole fraction is the product of the Ox mole fraction
!  multiplied by the O3-to-Ox ratio, ro3ox. At pressures greater than
!  approximately 1 hPa, ro3ox = 1 everywhere.  At pressures less than 
!  approximately 0.1 hPa, Ox is mostly O3 at night and ro3ox = 1.  During 
!  the day, ro3ox in this region depends to first order on pressure.
!
!  WAENINGS:
!
!  Programmed for GEOS-4 with a 1-D decomposition only.  For pressures less 
!  than or equal to 1 hPa, the layer mean and layer interface pressures must 
!  be isobaric.
!
! !REVISION HISTORY:
!
!       1999 Nielsen   Initial coding for S.-J. Lin and the fvGCM.
!   4Mar2005 Nielsen   Adaptation to the O3 grid component module in GEOS4.
!  16Sep2005 da Silva  GEOS-5 mods.
!  20Sep2005 Nielsen   Added N2O, CFC-11, CFC-12, CH4, HCFC-22.
!
!EOP
!-------------------------------------------------------------------------

  TYPE(Chem_Bundle), INTENT(INOUT):: w_c      ! Chemical tracer fields	
  TYPE(O3_GridComp), INTENT(IN) :: gcO3     ! Grid Component

  INTEGER, INTENT(IN) :: i1,i2,im,j1,j2,km,nymd,nhms
  
  INTEGER :: ic,j,k,k100Pa,m,m1,m2,nc,nextmon,nO3,nowday,nOx
  INTEGER :: nowhrs,nowmin,nowmon,nowsec,nowyrs,pastmon
  
  INTEGER :: iflag(km)

  REAL :: pPa(i1:i2,j1:j2,km),Ox(i1:i2,j1:j2,km)
  REAL :: cos(i1:i2),cosineZA(i1:i2,j1:j2),ro3ox(i1:i2,j1:j2,km)

  REAL :: c,calday,deg2rad,doy,dtchem,etyrs,f,fday,latRad
  REAL :: midmonth,wgt1,wgt2
  
  CHARACTER(LEN=3), SAVE :: monthname(12)=(/'Jan','Feb','Mar','Apr', &
                                            'May','Jun','Jul','Aug', &
                                            'Sep','Oct','Nov','Dec'/)
  INTEGER, SAVE :: monthdays(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)

! Determine current year, month, and day plus time of day and
! fractional day.
! -----------------------------------------------------------
  nowyrs=nymd/10000
  nowmon=(nymd-nowyrs*10000)/100
  nowday=nymd-nowyrs*10000-nowmon*100

  nowhrs=nhms/10000
  nowmin=(nhms-nowhrs*10000)/100
  nowsec=nhms-nowhrs*10000-nowmin*100

  fday=nowhrs/24.00+nowmin/1440.00+nowsec/86400.00
  calday=nowday+fday-1.00

! Determine current day of year
! -----------------------------
  IF(nowmon == 1) THEN
   doy = calday + 1
  ELSE
   doy = calday + 1 + SUM(monthdays(1:nowmon-1))
  END IF

! Find midpoint in current month
! ------------------------------
  midmonth=monthdays(nowmon)*0.50

! Find previous and next month numbers
! ------------------------------------
  pastmon=nowmon-1
  IF(pastmon .LE. 0) pastmon=12

  nextmon=nowmon+1
  IF(nextmon .GE. 12) nextmon=1

! Before midmonth, use past month's data with wgt1 and m1 and
! current month's data with wgt2 and m2.
! After midmonth, use current month's data with wgt1 and m1 and
! next month's data with wgt2 and m2.
! -------------------------------------------------------------
  IF(calday .LE. midmonth) THEN
   wgt2=0.50*(1.00+calday/midmonth)
   wgt1=1.00-wgt2
   m2=nowmon
   m1=pastmon
  ELSE
   wgt1=1.00-0.50*(calday-midmonth)/(monthdays(nowmon)-midmonth)
   wgt2=1.00-wgt1
   m1=nowmon
   m2=nextmon
  END IF
  
  nc=1

! Apply production and loss to N20, CFC-11, CFC-12, CH4,
! and HCFC-22. Note: There is no P and/or L for O3.
! ------------------------------------------------------
  DO ic=w_c%reg%i_O3+2,w_c%reg%i_O3+6
   nc=nc+1
   DO k=1,km
     DO j=j1,j2
      w_c%qa(ic)%data3d(i1:i2,j,k)=w_c%qa(ic)%data3d(i1:i2,j,k)+gcO3%cdt* &
                          (wgt1*prod(j,k,nc,m1)+wgt2*prod(j,k,nc,m2))
      w_c%qa(ic)%data3d(i1:i2,j,k)=w_c%qa(ic)%data3d(i1:i2,j,k)/(1.00+gcO3%cdt* &
        	          (wgt1*loss(j,k,nc,m1)+wgt2*loss(j,k,nc,m2)))
    END DO
   END DO
  END DO

! Apply production and loss to Ox
! -------------------------------
  nO3=gcO3%nForO3
  nOx=gcO3%nForOx
  ic=1

! Convert ozone from ppmv to mole fraction (volume mixing ratio).
! ---------------------------------------------------------------
  IF(gcO3%O3inPPMV) w_c%qa(nO3)%data3d(i1:i2,j1:j2,:)=w_c%qa(nO3)%data3d(i1:i2,j1:j2,:)*1.00E-06
 
! Update Ox and O3
! ----------------
  DO k=1,km
   DO j=j1,j2
    Ox(i1:i2,j,k)=w_c%qa(nOx)%data3d(i1:i2,j,k)+gcO3%cdt* &
                       (wgt1*prod(j,k,ic,m1)+wgt2*prod(j,k,ic,m2))
    Ox(i1:i2,j,k)= Ox(i1:i2,j,k)/(1.00+gcO3%cdt* &
        	       (wgt1*loss(j,k,ic,m1)+wgt2*loss(j,k,ic,m2)))
   END DO
  END DO

! Define the O3-to-Ox ratio
! -------------------------
  ro3ox(i1:i2,j1:j2,:)=1.00

! The following section modifies ro3ox based on pressure 
! and zenith angle. It is skipped if ptop > 1 hPa.
! ------------------------------------------------------
  Mods: IF(w_c%grid%ptop < 100.00) THEN

! Find zenith angle either by GEOS-4 or GEOS-5 
! --------------------------------------------
   IF(ASSOCIATED(w_c%cosz)) THEN
    cosineZA = w_c%cosz
   ELSE
    deg2rad=0.0174533
    DO j=j1,j2
       latRad=(w_c%grid%lat_min+(j-1)*w_c%grid%lat_del)*deg2rad
       CALL zenith(doy,.FALSE.,latRad,cos)
       cosineZA(i1:i2,j)=cos(i1:i2)
    END DO
   END IF

! Get pressure from delp and ptop
! -------------------------------
   pPa(i1:i2,j1:j2,1)=w_c%grid%ptop+0.50*w_c%delp(i1:i2,j1:j2,1)
   DO k=2,km
    pPa(i1:i2,j1:j2,k) = pPa(i1:i2,j1:j2,k-1)+0.50*w_c%delp(i1:i2,j1:j2,k-1)+ &
                                              0.50*w_c%delp(i1:i2,j1:j2,k  )
   END DO

! Restrict work area to the top k100Pa layers
! -------------------------------------------
   iflag(:)=0
   DO k=1,km
    IF(pPa(i1,j1,k) .LE. 100.00) iflag(k)=1
   END DO
   k100Pa=SUM(iflag)
   
! Modify the O3-to-Ox ratio in the daytime
! ----------------------------------------
   DO k=1,k100Pa
    f=EXP(-1.5*(LOG10(0.01*pPa(i1,j1,k)))**2)
    WHERE(cosineZA(i1:i2,j1:j2) > -0.10) ro3ox(i1:i2,j1:j2,k)=f
   END DO

  END IF Mods

! Apply Ox-to-O3 weighting to Ox to generate ozone.
! -------------------------------------------------
  w_c%qa(nO3)%data3d(i1:i2,j1:j2,:)=ro3ox(i1:i2,j1:j2,:)*Ox(i1:i2,j1:j2,:)

! Convert ozone from mole fraction (volume mixing ratio) to ppmv.
! ---------------------------------------------------------------
  IF(gcO3%O3inPPMV) w_c%qa(nO3)%data3d(i1:i2,j1:j2,:)=w_c%qa(nO3)%data3d(i1:i2,j1:j2,:)*1.00E+06

! Finally, put Ox back into the bundle.
! -------------------------------------
  w_c%qa(nOx)%data3d(i1:i2,j1:j2,:)=Ox(i1:i2,j1:j2,:)

! Verify month selection and weighting if verbose is .true.
! ---------------------------------------------------------
  IF(gcO3%verbose .AND. gid .EQ. 0) THEN

   ic=3
   PRINT 101
   PRINT 102, nowyrs,nowmon,nowday,nowhrs,nowmin,nowsec, &
              calday,midmonth,m1,wgt1,m2,wgt2
  END IF

  101 FORMAT(/,'PC6: Date and weights:',/, &
     	    ' Year Month Day Hour Minute Second CalDay Midmonth ', &
     	    'Month 1 Weight Month 2 Weight',/, &
     	    ' ---- ----- --- ---- ------ ------ ------ -------- ', &
     	    '------- ------ ------- ------')
  102 FORMAT(' ',i4,i6,i4,i5,i7,i7,1x,f6.3,1x,f8.2,2(1x,i7,1x,f6.3))

  RETURN
  END SUBROUTINE pc6

 END SUBROUTINE O3_GridCompRun

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 900.3, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  O3_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   SUBROUTINE O3_GridCompFinalize ( gcO3, w_c, impChem, expChem, &
                                    nymd, nhms, cdt, rc )

! !USES:

  USE O3_data, ONLY : prod, loss

  IMPLICIT none

! !INPUT/OUTPUT PARAMETERS:

   TYPE(O3_GridComp), INTENT(inout) :: gcO3   ! Grid Component

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), INTENT(in)  :: w_c      ! Chemical tracer fields   
   INTEGER, INTENT(in) :: nymd, nhms	      ! time
   REAL,    INTENT(in) :: cdt  	              ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(inout) :: impChem	! Import State
   TYPE(ESMF_State), INTENT(inout) :: expChem	! Import State
   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine finalizes this Grid Component.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!   4Mar2005 Nielsen   Added deallocates for Ox P and L
!  20Sep2005 Nielsen   Added N2O, CFC-11, CFC-12, CH4, HCFC-22.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'O3_GridCompFinalize'
   
   rc=0

   DEALLOCATE( prod , stat=rc )
   DEALLOCATE( loss , stat=rc )

   RETURN

 END SUBROUTINE O3_GridCompFinalize

 END MODULE O3_GridCompMod


