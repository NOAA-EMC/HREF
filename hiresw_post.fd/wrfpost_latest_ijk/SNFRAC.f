      SUBROUTINE SNFRAC (SNEQV,IVEGx,SNCOVR)

!      IMPLICIT NONE
       include 'mpif.h'
      
C ----------------------------------------------------------------------
C SUBROUTINE SNFRAC
C ----------------------------------------------------------------------
C CALCULATE SNOW FRACTION (0 -> 1)
C SNEQV   SNOW WATER EQUIVALENT (M)
C IVEG    VEGETATION TYPE
C SNCOVR  FRACTIONAL SNOW COVER
C SNUP    THRESHOLD SNEQV DEPTH ABOVE WHICH SNCOVR=1
C SALP    TUNING PARAMETER
C ----------------------------------------------------------------------
      REAL SNEQV,SALP,SNUP(27),SNCOVR,RSNOW

      DATA SALP /4.0/
      DATA SNUP /0.020, 0.020, 0.020, 0.020, 0.020, 0.020,
     &            0.020, 0.020, 0.020, 0.040, 0.040, 0.040,
     &            0.040, 0.040, 0.040, 0.010, 0.013, 0.020,
     &            0.013, 0.020, 0.020, 0.020, 0.020, 0.013,
     &            0.013, 0.013, 0.013/
     
C ----------------------------------------------------------------------
C SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD ABOVE WHICH SNOCVR=1.
C ----------------------------------------------------------------------
Cjjt
        IVEG = IVEGx
        IF ( IVEG .gt. 27 .or. IVEG .lt. 1 ) then
c          print *, ' PROBLEM in SNFRAC, IVEG = ',iveg
           IVEG = 1
        END IF
        IF (SNEQV .LT. SNUP(IVEG)) THEN
          RSNOW = SNEQV/SNUP(IVEG)
          SNCOVR = 1. - (EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
        ELSE
          SNCOVR = 1.0
        ENDIF
        SNCOVR = MAX(0.,MIN(SNCOVR,1.))

      RETURN
      END
