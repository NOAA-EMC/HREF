      SUBROUTINE GD2TLL(GDLAT,GDLON,GDTLAT,GDTLON,IMOUT,JMOUT)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    GD2TLL      COMPUTE TRANSFORMED (LAT,LON)
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-23       
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES THE TRANSFORMED LATITUDE OF (LAT,
C     LON) OF OUTPUT GRID POINTS.  THE TRANSFORMED (LAT,LON)
C     ARE WITH RESPECT TO THE INPUT E-GRID.
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  DAVID PLUMMER - SUBROUTINE CGTLL IN ETAPACKC
C   92-12-23  RUSS TREADON - GENERALIZED TO HANDLE VARIABLE
C                               OUTPUT GRIDS.
C     
C USAGE:    CALL GD2TLL(GDLAT,GDLON,GDTLAT,GDTLON,IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     GDLAT    - GEODETIC LATITUDE OF OUTPUT GRID POINTS.
C     GDLON    - GEODETIC LONGTUDE OF OUTPUT GRID POINTS.
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     GDTLAT   - TRANSFORMED LATITUDE OF OUTPUT GRID POINTS.
C     GDTLON   - TRANSFORMED LONGITUDE OF OUTPUT GRID POINTS.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - EGRID
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     INCLUDE ETA GRID DIMENSIONS
      INCLUDE "parmeta"
      INCLUDE "parmout"
      PARAMETER (LP1=LM+1)
C
C     SET CONVERSION CONSTANTS.
      PARAMETER (R2D=57.29578,D2R=1./R2D)
C
C     DECLARE VARIABLES AND COMMONS.

      LOGICAL  NORTH
      REAL GDLAT(IMX,JMX), GDLON(IMX,JMX)
      REAL GDTLAT(IMX,JMX), GDTLON(IMX,JMX)
C     
C     INCLUDE ETA GRID SPECIFICATIONS IN COMMON MAPOT.
      INCLUDE "EGRID.comm"
C     
C     DECLARE EQUIVALENCES.
      EQUIVALENCE (YLATC,ALONVT), (DPHI, POLEI)
      EQUIVALENCE (WLONC, POLEJ), (DLAM,XMESHL)
C     
C*******************************************************************
C     START GD2TLL HERE.
C     
C     SET CONSTANTS.
C     
      ELAM0=WLONC+360.
      EPHI0=YLATC
      SPHI0=SINPH0
      CPHI0=COSPH0
C     
C     LOOP TO COMPUTE TRANSFORMED (LAT,LON).
C
      DO 20 J = 1,JMOUT
         DO 10 I = 1,IMOUT
            WLON  = GDLON(I,J)
            YLAT  = GDLAT(I,J)
            RELM  = (WLON-ELAM0)*D2R
            SRLM  = SIN(RELM)
            CRLM  = COS(RELM)
            APH   = YLAT*D2R
            SPH   = SIN(APH)
            CPH   = COS(APH)
            CC    = CPH*CRLM
            ANUM  = -CPH*SRLM
            DENOM = CPHI0*CC+SPHI0*SPH
            TLM   = ATAN2(ANUM,DENOM)/D2R
            TPH   = ASIN(CPHI0*SPH-SPHI0*CC)/D2R
            GDTLAT(I,J) = TPH
            GDTLON(I,J) = TLM
 10      CONTINUE
 20   CONTINUE
C     
C     END OF ROUTINE.
C
      RETURN
      END
