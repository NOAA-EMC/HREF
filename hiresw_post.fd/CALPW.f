      SUBROUTINE CALPW(PW,IDECID)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALPW       COMPUTES 
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-24       
C     
C ABSTRACT:  
C     THIS ROUTINE COMPUTES PRECIPITABLE WATER IN A COLUMN
C     EXTENDING FROM THE FIRST ATMOSPHERIC ETA LAYER TO THE
C     MODEL TOP.  THE DEFINITION USED IS
C                                 TOP
C            PRECIPITABLE WATER = SUM (Q+CLDW) DP*HTM/G
C                                 BOT
C     WHERE,
C        BOT IS THE FIRST ETA LAYER,
C        TOP IS THE MODEL TOP,
C        Q IS THE SPECIFIC HUMIDITY (KG/KG) IN THE LAYER
C        CLDW IS THE CLOUD WATER (KG/KG) IN THE LAYER
C        DP (Pa) IS THE LAYER THICKNESS.
C        HTM IS THE HEIGHT MASK AT THAT LAYER (=0 IF BELOW GROUND)
C        G IS THE GRAVITATIONAL CONSTANT
C     
C PROGRAM HISTORY LOG:
C   92-12-24  RUSS TREADON
C   96-03-04  MIKE BALDWIN - ADD CLOUD WATER AND SPEED UP CODE
C   98-06-15  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION                 
C   02-06-19  MIKE BALDWIN - WRF VERSION 
C   04-12-30  H CHUANG      - UPDATE TO CALCULATE TOTAL COLUMN FOR OTHER
C                                     HYDROMETEORS                
C     
C USAGE:    CALL CALPW(PW)
C   INPUT ARGUMENT LIST:
C     PW       - ARRAY OF PRECIPITABLE WATER.
C
C   OUTPUT ARGUMENT LIST: 
C     NONE     
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - LOOPS
C                  MASKS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
      use vrbls3d
      use masks
C     
C     INCLUDE/SET PARAMETERS.
!      INCLUDE "parmeta"
      INCLUDE "params"
C
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
C     
C     SET DENSITY OF WATER AT 1 ATMOSPHERE PRESSURE, 0C.
C     UNITS ARE KG/M**3.
      PARAMETER (RHOWAT=1.E3)
C     
C     DECLARE VARIABLES.
C     
      INTEGER LLMH
      REAL ALPM,DZ,PM,PWSUM,RHOAIR
      REAL PW(IM,JM),QDUM(IM,JM)
C
C***************************************************************
C     START CALPW HERE.
C
C     INITIALIZE PW TO 0.    
C     
      PW = 0.
C     
C     OUTER LOOP OVER VERTICAL DIMENSION.
C     INNER LOOP OVER HORIZONTAL GRID.
C     
!$omp  parallel do
!$omp& private(dp)
      DO L = 1,LM
        IF (IDECID .LE. 1) THEN
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=Q(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID .EQ. 2) THEN
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=QQW(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID .EQ. 3) THEN
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=QQI(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID .EQ. 4) THEN
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=QQR(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID .EQ. 5) THEN
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=QQS(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID .EQ. 6) THEN
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=CWM(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID .EQ. 7) THEN
!-- Total supercooled liquid
          DO J=JSTA,JEND
            DO I=1,IM
              IF (T(I,J,L) .GE. TFRZ) THEN
                Qdum(I,J)=0.
              ELSE
                Qdum(I,J)=QQW(I,J,L)+QQR(I,J,L)
              ENDIF
            ENDDO
          ENDDO
        ELSE IF (IDECID .EQ. 8) THEN
!-- Total melting ice
          DO J=JSTA,JEND
            DO I=1,IM
              IF (T(I,J,L) .LE. TFRZ) THEN
                Qdum(I,J)=0.
              ELSE
                Qdum(I,J)=QQI(I,J,L)+QQS(I,J,L)
              ENDIF
            ENDDO
          ENDDO
        ELSE IF (IDECID .EQ. 9) THEN
! SHORT WAVE T TENDENCY
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=RSWTT(I,J,L)
            ENDDO
          ENDDO
        ELSE IF (IDECID .EQ. 10) THEN
! LONG WAVE T TENDENCY
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=RLWTT(I,J,L)
            ENDDO
          ENDDO	  
        ELSE IF (IDECID .EQ. 11) THEN
! LATENT HEATING FROM GRID SCALE RAIN/EVAP
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=TRAIN(I,J,L)
            ENDDO
          ENDDO	  
        ELSE IF (IDECID .EQ. 12) THEN
! LATENT HEATING FROM CONVECTION
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=TCUCN(I,J,L)
            ENDDO
          ENDDO	  	  
        ELSE IF (IDECID .EQ. 13) THEN
! MOISTURE CONVERGENCE
          DO J=JSTA,JEND
            DO I=1,IM
              Qdum(I,J)=MCVG(I,J,L)
            ENDDO
          ENDDO
        ENDIF

        DO J=JSTA,JEND
          DO I=1,IM
            DP   =PINT(I,J,L+1)-PINT(I,J,L)
            PW(I,J)=PW(I,J)+Qdum(I,J)*DP*GI*HTM(I,J,L)
          ENDDO
        ENDDO
      ENDDO
C     
C     END OF ROUTINE.
C     
      RETURN
      END
