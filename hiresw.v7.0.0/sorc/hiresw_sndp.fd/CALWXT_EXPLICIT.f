      SUBROUTINE CALWXT_EXPLICIT(LMHK,TSKIN,PPT,SR,F_RIMEF,IWX,ISTAT)
C 
C     FILE: CALWXT.f
C     WRITTEN: 24 MARCH 2006, G MANIKIN and B FERRIER 
C
C     ROUTINE TO COMPUTE PRECIPITATION TYPE USING EXPLICIT FIELDS
C       FROM THE MODEL MICROPHYSICS

      PARAMETER(PTHRES=0.02)
C
C   SKIP THIS POINT IF NO PRECIP THIS TIME STEP 
C
      IWX = 0
      IF (PPT.LE.PTHRES) RETURN
 
C
C  A SNOW RATIO LESS THAN 0.5 ELIMINATES SNOW AND SLEET
C   USE THE SKIN TEMPERATURE TO DISTINGUISH RAIN FROM FREEZING RAIN
C   NOTE THAT 2-M TEMPERATURE MAY BE A BETTER CHOICE IF THE MODEL
C   HAS A COLD BIAS FOR SKIN TEMPERATURE
C 
      IF (SR.LT.0.5) THEN
C        SURFACE (SKIN) POTENTIAL TEMPERATURE AND TEMPERATURE.

       IF (TSKIN.LT.273.15) THEN
C          FREEZING RAIN = 4
           IWX=IWX+4
       ELSE
C          RAIN = 8
           IWX=IWX+8
       ENDIF
       ELSE
C  
C  DISTINGUISH SNOW FROM SLEET WITH THE RIME FACTOR
C 
        IF(F_RimeF.GE.10) THEN
C          SLEET = 2
           IWX=IWX+2
        ELSE
           SNOW = 1
           IWX=IWX+1 
       ENDIF
      ENDIF
 800  CONTINUE
 810  RETURN 
      END