!
      PROGRAM HSGRID_SELECT
!
!----------------------------------------------------------------------
!
      USE MODULE_GRID_CENTERS
!
!----------------------------------------------------------------------
!
!***  CHOOSE THE GRID WHOSE CENTER IS NEAREST TO THE INPUT LAT/LON.
!
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,PARAMETER :: NUNIT_LATLON=5   ! Unit number of namelist  
                                            ! input file with the
                                            ! lat/lon of interest
! 
      REAL,PARAMETER :: DEG2RAD=1.74532925E-2
!----------------------------------------------------------------------
!
!***  SCALARS
!
      INTEGER :: NG,NG_MIN
!
      REAL :: DISTANCE,GLAT,GLON,RLATX,RLONX,XLATITUDE,XLONGITUDE
!
      REAL :: DIST_MIN=1.E10
!----------------------------------------------------------------------
!
!***  DECLARE NAMELIST
!
      NAMELIST /LATLON_INPUT/ XLATITUDE,XLONGITUDE
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
!***  RETRIEVE THE LAT/LON (DEGREES) OF THE POINT OF INTEREST
!
!----------------------------------------------------------------------
      READ(NUNIT_LATLON,LATLON_INPUT)
!
!***  CONVERT TO RADIANS
!
      RLATX=XLATITUDE*DEG2RAD
      RLONX=XLONGITUDE*DEG2RAD
!----------------------------------------------------------------------
!
!***  LOOP THROUGH THE POSSIBLE GRIDS
!
!----------------------------------------------------------------------
!
      CALL ASSIGN_CENTERS
!
      DO NG=1,NGRIDS
        GLAT=GRID_LATITUDE(NG)*DEG2RAD
        GLON=GRID_LONGITUDE(NG)*DEG2RAD
!
        DISTANCE=ACOS(COS(RLATX)*COS(GLAT)*COS(RLONX-GLON)             &
                     +SIN(RLATX)*SIN(GLAT))
!
        IF(DISTANCE.LT.DIST_MIN)THEN
          DIST_MIN=DISTANCE
          NG_MIN=NG
        ENDIF
      ENDDO
!----------------------------------------------------------------------
!
      IF(NG_MIN.LT.10)THEN
        WRITE(6,11)NG_MIN
      ELSE   
        WRITE(6,10)NG_MIN
      ENDIF
   10 FORMAT('hs',I2)
   11 FORMAT('hs0',I1)
!
!----------------------------------------------------------------------
      END PROGRAM HSGRID_SELECT
