      SUBROUTINE ETCALC(ETA,ETP,ESD,VEGFAC,ISOIL,SMC,CMC,
     &                  EC,EDIR,ETRANS,ESNOW,SMCDRY,SMCMAX)
C ----------------------------------------------------------------------
C PROGRAM HISTORY LOG:
C   03-01-17  M EK AND H CHUANG - LIFTED IT FROM MODEL FOR POST
C ----------------------------------------------------------------------
C DETERMINE INDIVIDUAL COMPONENTS OF SURFACE EVAPORATION
C INPUT:
C   ETA    = TOTAL SURFACE EVAPORATION (W/m2)
C   ETP    = POTENTIAL EVAPORATION (W/m2)
C   ESD    = WATER EQUIVALENT SNOW DEPTH (m)
C   VEGFAC = GREEN VEGETATION FRACTION (fraction ...or percent?)
C   ISOIL  = SOIL TYPE (1-19)
C   SMC    = UPPER SOIL LAYER (0-10 CM) SOIL MOISTURE (VOLUMETRIC)
C   CMC    = CANOPY WATER CONTENT (m)
C OUTPUT:
C   EC     = EVAPORATION OF CANOPY WATER (W/m2)
C   EDIR   = DIRECT SOIL EVAPORATION (W/m2)
C   ETRANS = TRANSPIRATION (W/m2)
C   ESNOW  = SNOW SUBLIMATION (W/m2)
C ----------------------------------------------------------------------
      parameter (nosoiltype=19)
      REAL SMDRY(nosoiltype),SMMAX(nosoiltype)

      DATA CFACTR,CMCMAX /0.5,0.5E-3/
C ----------------------------------------------------------------------
C SOIL TYPES   ZOBLER (1986), COSBY ET AL (1984)
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
C ----------------------------------------------------------------------
      DATA SMDRY /0.023, 0.028, 0.047, 0.084, 0.084, 0.066,
     &           0.069, 0.120, 0.103, 0.100, 0.126, 0.135,
     &           0.069, 0.000, 0.012, 0.028, 0.135, 0.012,
     &           0.023/
C
      DATA SMMAX /0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
     &           0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
     &           0.464, 0.000, 0.200, 0.421, 0.457, 0.200,
     &           0.395/
C
      DATA FXEXP /2.0/

C ----------------------------------------------------------------------
C INITIALIZE EVAPORATION COMPONENTS
C ----------------------------------------------------------------------
      EC     = 0.0
      EDIR   = 0.0
      ETRANS = 0.0
      ESNOW  = 0.0

C ----------------------------------------------------------------------
C SET SMCDRY AND SMCMAX VALUES
C ----------------------------------------------------------------------
      SMCDRY = SMDRY(ISOIL)
      SMCMAX = SMMAX(ISOIL)

C ----------------------------------------------------------------------
C DETERMINE INDIVIDUAL COMPONENTS OF EVAPORATION
C NO SURFACE EVAPORATION COMPONENTS IF POTENTIAL (ETP)<0
C IF SNOW ON THE GROUND (ESD>0), ALL EVAPORATION IS SNOW SUBLIMATION,
C ELSE IT IT A SUM OF CANOPY EVAP, DIRECT SOIL EVAP AND TRANSPIRATION
C ----------------------------------------------------------------------
      IF (ETP .GT. 0.) THEN      
        IF (ESD .GT. 0.) THEN
          ESNOW = ETA
        ELSE

C ----------------------------------------------------------------------
C CANOPY EVAPORATION
C ----------------------------------------------------------------------
          IF (CMC .GT. 0) THEN
            IF (CMC .GT. CMCMAX) CMC = CMCMAX
            EC = VEGFAC*((CMC/CMCMAX)**CFACTR)*ETP
          ENDIF

C ----------------------------------------------------------------------
C DIRECT SOIL EVAPORATION A FUNCTION OF RELATIVE SOIL MOISTURE
C AVAILABILITY, LINEAR WHEN FXEXP=1.
C ----------------------------------------------------------------------
          SRATIO = (SMC-SMCDRY)/(SMCMAX-SMCDRY)
          IF (SRATIO .GT. 0.) THEN
            FX = SRATIO**FXEXP
            FX = MAX(0.,MIN(FX,1.))
          ELSE
            FX = 0.
          ENDIF
          EDIR = FX*(1.0-VEGFAC)*ETP

C ----------------------------------------------------------------------
C CALCULATE TRANSPIRATION AS A RESIDUAL OF THE TOTAL MINUS EDIR AND EC
C ----------------------------------------------------------------------
          ETRANS = ETA - EDIR - EC
        ENDIF
        IF (ETRANS .LT. 0.) ETRANS = 0.

      ENDIF

      RETURN
      END
