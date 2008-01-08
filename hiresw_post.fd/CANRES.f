      SUBROUTINE CANRES(SOLAR,SFCTMP,Q2,SFCPRS,SMC,
     &                  GC,RC,IVEG,ISOIL,
     &                  RSMIN,NROOTS,SMCWLT,SMCREF,
     &                  RCS,RCQ,RCT,RCSOIL,SLDPTH)
!      IMPLICIT NONE

C ######################################################################
C                        SUBROUTINE CANRES
C                        -----------------
C       THIS ROUTINE CALCULATES THE CANOPY RESISTANCE WHICH DEPENDS ON
C       INCOMING SOLAR RADIATION, AIR TEMPERATURE, ATMOSPHERIC WATER
C       VAPOR PRESSURE DEFICIT AT THE LOWEST MODEL LEVEL, AND SOIL
C       MOISTURE (PREFERABLY UNFROZEN SOIL MOISTURE RATHER THAN TOTAL)
C ----------------------------------------------------------------------
C        SOURCE:  JARVIS (1976), JACQUEMIN AND NOILHAN (1990 BLM)
C ----------------------------------------------------------------------
C PROGRAM HISTORY LOG:
C   03-01-17  M EK AND H CHUANG - LIFTED IT FROM MODEL FOR POST 
C ----------------------------------------------------------------------
C INPUT:  SOLAR: INCOMING SOLAR RADIATION
C 	  CH:	  SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
C 	  SFCTMP: AIR TEMPERATURE AT 1ST LEVEL ABOVE GROUND
C 	  Q2:	  AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
C 	  Q2SAT:  SATURATION AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
C 	  SFCPRS: SURFACE PRESSURE
C 	  SMC:    VOLUMETRIC SOIL MOISTURE 
C 	  ZSOIL:  SOIL DEPTH (NEGATIVE SIGN, AS IT IS BELOW GROUND)
C 	  NSOIL:  NO. OF SOIL LAYERS
C 	  IROOT:  NO. OF SOIL LAYERS IN ROOT ZONE (1.LE.NROOT.LE.NSOIL)
C 	  XLAI:   LEAF AREA INDEX
C 	  SMCWLT: WILTING POINT
C 	  SMCREF: REFERENCE SOIL MOISTURE
C 		  (WHERE SOIL WATER DEFICIT STRESS SETS IN)
C
C RSMIN, RSMAX, TOPT, RGL, HS: CANOPY STRESS PARAMETERS SET IN
C SUBROUTINE REDPRM
C
C  (SEE EQNS 12-14 AND TABLE 2 OF SEC. 3.1.2 OF 
C       CHEN ET AL., 1996, JGR, VOL 101(D3), 7251-7268)               
C
C        OUTPUT:  PC: PLANT COEFFICIENT
C                 RC: CANOPY RESISTANCE
C                 GC: CANOPY CONDUCTANCE
C ----------------------------------------------------------------------
C ######################################################################

!      INCLUDE "parmeta"
!      INCLUDE "parmsoil"
      INCLUDE "params"
      INCLUDE "CTLBLK.comm"
      parameter (nosoiltype=19,novegtype=27)
      INTEGER K
!      INTEGER NROOT(13)
      INTEGER IROOT(novegtype)
!      INTEGER NSOIL
      INTEGER IVEG
      INTEGER ISOIL

!      REAL SIGMA, RD, CP, SLV
      REAL SOLAR, CH, SFCTMP, Q2, SFCPRS 
      REAL SMC(NSOIL), ZSOIL(NSOIL), PART(NSOIL),SLDPTH(NSOIL) 
      REAL SMWLT(nosoiltype),SMREF(nosoiltype),RSMN(novegtype)
     + ,RC,PC,Q2SAT,GC
      REAL TOPT,RSMAX,RGL(novegtype),HS(novegtype),RCS,RCT
     + ,RCQ,RCSOIL,FF
      REAL P,QS,GX,TAIR4,ST1,SLVCP,RR,DELTA
!      COMMON /LEAF/ XLAI

!      PARAMETER (CP=1004.5, SLV=2.501000E6)

!      DATA XLAI /2.0/   ! NOW DEFINED IN PARAMS
      DATA RSMAX /5000./
      DATA TOPT /298.0/

      DATA IROOT /1,3,3,3,3,3,3,3,3,3,
     &            4,4,4,4,4,0,2,2,1,3,
     &            3,3,2,1,1,1,1/
!      DATA ZSOIL /-0.1,-0.4,-1.0,-2.0/
C  SSIB VEGETATION TYPES (DORMAN AND SELLERS, 1989; JAM)
C
C   1   Urban and Built-Up Land
C   2   Dryland Cropland and Pasture
C   3   Irrigated Cropland and Pasture
C   4   Mixed Dryland/Irrigated Cropland and Pasture
C   5   Cropland/Grassland Mosaic
C   6   Cropland/Woodland Mosaic
C   7   Grassland
C   8   Shrubland
C   9   Mixed Shrubland/Grassland
C  10   Savanna
C  11   Deciduous Broadleaf Forest
C  12   Deciduous Needleleaf Forest
C  13   Evergreen Broadleaf Forest
C  14   Evergreen Needleleaf Forest
C  15   Mixed Forest
C  16   Water Bodies
C  17   Herbaceous Wetland
C  18   Wooded Wetland
C  19   Barren or Sparsely Vegetated
C  20   Herbaceous Tundra
C  21   Wooded Tundra
C  22   Mixed Tundra
C  23   Bare Ground Tundra
C  24   Snow or Ice
C  25   Playa
C  26   Lava
C  27   White Sand
C
      DATA RSMN /200.0,  70.0,  70.0,  70.0,  70.0,  70.0,
     &            70.0, 300.0, 170.0,  70.0, 100.0, 150.0,
     &           150.0, 125.0, 125.0, 100.0,  40.0, 100.0,
     &           300.0, 150.0, 150.0, 150.0, 200.0, 200.0,
     &            40.0, 100.0, 300.0/
C     
      DATA RGL /100.0, 100.0, 100.0, 100.0, 100.0,  65.0,
     &          100.0, 100.0, 100.0,  65.0,  30.0,  30.0,
     &           30.0,  30.0,  30.0,  30.0, 100.0,  30.0,
     &          100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
     &          100.0, 100.0, 100.0/
C     
      DATA HS /42.00, 36.25, 36.25, 36.25, 36.25, 44.14,
     &         36.35, 42.00, 39.18, 54.53, 54.53, 47.35,
     &         41.69, 47.35, 51.93, 51.75, 60.00, 51.93,
     &         42.00, 42.00, 42.00, 42.00, 42.00, 42.00,
     &         36.25, 42.00, 42.00/
C
C SOIL TYPES   ZOBLER (1986)      COSBY ET AL (1984) (quartz cont.(1))
C ----  -------
C   1   SAND
C   2   LOAMY SAND
C   3   SANDY LOAM
C   4   SILT LOAM
C   5   SILT
C   6   LOAM
C   7   SANDY CLAY LOAM
C   8   SILTY CLAY LOAM
C   9   CLAY LOAM
C  10   SANDY CLAY
C  11   SILTY CLAY
C  12   CLAY
C  13   ORGANIC MATERIAL
C  14   WATER
C  15   BEDROCK
C  16   OTHER(land-ice)
C  17   PLAYA
C  18   LAVA
C  19   WHITE SAND

      DATA SMREF /0.196, 0.248, 0.282, 0.332, 0.332, 0.301,
     &           0.293, 0.368, 0.361, 0.320, 0.388, 0.389,
     &           0.319, 0.000, 0.116, 0.248, 0.389, 0.116,
     &           0.196/
C     
      DATA SMWLT /0.023, 0.028, 0.047, 0.084, 0.084, 0.066,
     &           0.069, 0.120, 0.103, 0.100, 0.126, 0.135,
     &           0.069, 0.000, 0.012, 0.028, 0.135, 0.012,
     &           0.023/

C ----------------------------------------------------------------------
C INITIALIZE CANOPY CONDUCTANCE TERMS
C ----------------------------------------------------------------------
      RCS = 0.0
      RCT = 0.0
      RCQ = 0.0
      RCSOIL = 0.0
      RC = 0.0

!      ZSOIL(1)=-0.1
!      ZSOIL(2)=-0.4
!      ZSOIL(3)=-1.0
!      ZSOIL(4)=-2.0
      
      DO N=1,NSOIL
       IF(N.EQ.1)THEN
        ZSOIL(N)=-1.0*SLDPTH(N)
       ELSE
        ZSOIL(N)=ZSOIL(N-1)-SLDPTH(N)
       END IF
      END DO 		
C ----------------------------------------------------------------------
C SET SMCWLT, SMCREF, RSMIN, NROOTS VALUES
C ----------------------------------------------------------------------
      SMCWLT = SMWLT(ISOIL)
      SMCREF = SMREF(ISOIL)
      RSMIN = RSMN(IVEG)
      NROOTS = IROOT(IVEG)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO INCOMING SOLAR RADIATION
C ----------------------------------------------------------------------

      FF = 0.55*2.0*SOLAR/(RGL(IVEG)*XLAI)
      RCS = (FF + RSMIN/RSMAX) / (1.0 + FF)

      RCS = MAX(RCS,0.0001)
      RCS = MIN(RCS,1.0)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO AIR TEMPERATURE AT FIRST MODEL LEVEL ABOVE GROUND
C ----------------------------------------------------------------------

      RCT = 1.0 - 0.0016*((TOPT-SFCTMP)**2.0)

      RCT = MAX(RCT,0.0001)
      RCT = MIN(RCT,1.0)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO VAPOR PRESSURE DEFICIT AT FIRST MODEL LEVEL.
C ----------------------------------------------------------------------

c      P = SFCPRS
C Insert QSAT computation used in ETA2P
      TBLO=SFCTMP
      Q2SAT=PQ0/SFCPRS*EXP(A2*(TBLO-A3)/(TBLO-A4)) 
      QS = Q2SAT
C RCQ EXPRESSION FROM SSIB 
      RCQ = 1.0/(1.0+HS(IVEG)*(QS-Q2))

c      RCQ = MAX(RCQ,0.01)
      RCQ = MAX(RCQ,0.0001)
      RCQ = MIN(RCQ,1.0)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO SOIL MOISTURE AVAILABILITY.
C DETERMINE CONTRIBUTION FROM EACH SOIL LAYER, THEN ADD THEM UP.
C ----------------------------------------------------------------------

      GX = (SMC(1)-SMCWLT)/(SMCREF-SMCWLT)
      IF (GX .GT. 1.) GX = 1.
      IF (GX .LT. 0.) GX = 0.

C####   USING SOIL DEPTH AS WEIGHTING FACTOR
      PART(1) = (ZSOIL(1)/ZSOIL(NROOTS)) * GX

C#### USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
CC      PART(1) = RTDIS(1) * GX
      
      IF (NROOTS .GT. 1) THEN
       DO K = 2, NROOTS
        GX = (SMC(K)-SMCWLT)/(SMCREF-SMCWLT)
        IF (GX .GT. 1.) GX = 1.
        IF (GX .LT. 0.) GX = 0.
C####   USING SOIL DEPTH AS WEIGHTING FACTOR        
        PART(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOTS)) * GX

C#### USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
CC         PART(K) = RTDIS(K) * GX 
               
       END DO
      ENDIF
      DO K = 1, NROOTS
        RCSOIL = RCSOIL+PART(K)
      END DO

      RCSOIL = MAX(RCSOIL,0.0001)
      RCSOIL = MIN(RCSOIL,1.0)

C ----------------------------------------------------------------------
C         DETERMINE CANOPY RESISTANCE DUE TO ALL FACTORS.
C         CONVERT CANOPY RESISTANCE (RC) TO PLANT COEFFICIENT (PC).
C ----------------------------------------------------------------------

CC/98/01/05/........RC = RCMIN/(RCS*RCT*RCQ*RCSOIL)
c      RC = RCMIN(IVEG)/(XLAI*RCS*RCT*RCQ*RCSOIL)

      RCMIN = RSMIN/XLAI
      RCMAX = RSMAX/XLAI
      RC = RCMIN/(RCS*RCT*RCQ*RCSOIL)

      RC = MAX(RCMIN,MIN(RC,RCMAX))

      GC = 1./RC
      
      RETURN
      END
