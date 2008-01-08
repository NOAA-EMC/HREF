      SUBROUTINE NGMFLD(RH4710,RH4796,RH1847,RH8498,QM8510)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    NGMFLD      COMPUTES LAYER MEAN NGM FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES A HANDFUL OF NGM LAYER MEAN 
C     FIELDS.  THIS IS DONE TO PROVIDE A FULLY COMPLETE 
C     ETA NGM LOOK-ALIKE OUTPUT FILE.  THE SIGMA (LAYER)
C     FIELDS COMPUTED BY THIS ROUTINE ARE TABULATED BELOW.
C     
C           SIGMA (LAYER)         FIELD(S)
C          ---------------     --------------
C          0.47191-1.00000          RH
C          0.47171-0.96470          RH
C          0.18019-0.47191          RH
C          0.84368-0.98230          RH
C          0.85000-1.00000         MCONV
C     WHERE 
C          RH    = RELATIVE HUMIDITY
C          MCONV = MOISTURE CONVERGENCE
C
C     LAYER MEANS ARE A SUMMATION OVER ETA LAYERS MAPPING INTO
C     THE PRESSURE RANGE CORRESPONDING TO THE SIGMA RANGE ABOVE.
C     THE CALCULATION OF THESE BOUNDING PRESSURES IS DONE AT 
C     EACH HORIZONTAL GRID POINT BASED ON THE SURFACE PRESSURE.
C     EACH TERM IN THE SUMMATION IS WEIGHTED BY THE THICKNESS OF
C     THE ETA LAYER.  THE FINAL LAYER MEAN IS THIS SUM NORMALIZED
C     BY THE TOTAL DEPTH OF THE LAYER.

C
C     
C PROGRAM HISTORY LOG:
C   92-12-22  RUSS TREADON
C   93-07-27  RUSS TREADON - MODIFIED SUMMATION LIMITS FROM
C                            0.66*PSFC TO 0.75*PSFC AND 0.33*PSFC 
C                            TO 0.50*PSFC, WHERE PSFC IS THE
C                            SURFACES PRESSURE.  THE REASON FOR
C                            THIS CHANGE WAS RECOGNITION THAT IN 
C                            THE LFM 0.33 AND 0.66 WERE MEASURED
C                            FROM THE SURFACE TO THE TROPOPAUSE,
C                            NOT THE TOP OF THE MODEL.
C   93-09-13  RUSS TREADON - RH CALCULATIONS WERE MADE INTERNAL
C                            TO THE ROUTINE.
C   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-08-18  MIKE BALDWIN - COMPUTE RH OVER ICE
C   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
C   00-01-04  JIM TUCCILLO - MPI VERSION
C   02-04-24  MIKE BALDWIN - WRF VERSION
C     
C     
C USAGE:    CALL NGMFLD(RH4710,RH4796,RH1847,RH8498,QM8510)
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST: 
C     RH4710   - SIGMA LAYER 0.47-1.00 MEAN RELATIVE HUMIDITY.
C     RH4796   - SIGMA LAYER 0.47-0.96 MEAN RELATIVE HUMIDITY.
C     RH1847   - SIGMA LAYER 0.18-0.47 MEAN RELATIVE HUMIDITY.
C     RH8498   - SIGMA LAYER 0.84-0.98 MEAN RELATIVE HUMIDITY.
C     QM8510   - SIGMA LAYER 0.85-1.00 MEAN MOISTURE CONVERGENCE.
C     
C   OUTPUT FILES:
C     NONE
C     
C   LIBRARY:
C     COMMON   - 
C                MASKS
C                OPTIONS
C                LOOPS
C                MAPOT
C                DYNAMD
C                INDX
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     
C     INCLUDE PARAMETERS
      use vrbls3d
      use masks
C
!      INCLUDE "parmeta"
      INCLUDE "params"
C
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
C
      PARAMETER (SIG100=1.00000, SIG98=0.98230, SIG96=0.96470)
      PARAMETER (SIG89 =0.89671, SIG85=0.85000, SIG84=0.84368)
      PARAMETER (SIG78 =0.78483, SIG47=0.47191, SIG18=0.18018)
!      PARAMETER (SMALL = 1.E-6)
C     
C     DECLARE VARIABLES.
      LOGICAL GOT8510,GOT4710,GOT4796,GOT1847,GOT8498
      REAL QM8510(IM,JM),RH4710(IM,JM),RH8498(IM,JM)
      REAL RH4796(IM,JM),RH1847(IM,JM)
      REAL Z8510(IM,JM),Z4710(IM,JM),Z8498(IM,JM)
      REAL Z4796(IM,JM),Z1847(IM,JM)
      REAL Q1D(IM,JM),U1D(IM,JM),V1D(IM,JM),QCNVG(IM,JM)
C
C********************************************************************
C     START NGMFLD HERE.
C     
C     INITIALIZE ARRAYS.
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
         QM8510(I,J) = D00
         RH4710(I,J) = D00
         RH8498(I,J) = D00
         RH4796(I,J) = D00
         RH1847(I,J) = D00
         Z8510(I,J)  = D00
         Z8498(I,J)  = D00
         Z4710(I,J)  = D00
         Z4796(I,J)  = D00
         Z1847(I,J)  = D00
      ENDDO
      ENDDO
C     
C     LOOP OVER HORIZONTAL GRID.
C     
!$omp  parallel do
!$omp& private(dz,p100,p18,p47,p84,p85,
!$omp&         p96,p98,pm,qdiv,qk,qkhn,qkhs,qkm1,qm,qm8510,
!$omp&         qmcvg,qs,qudx,qvdy,r2dx,r2dy,rh,rh1847,rh4710,
!$omp&         rh4796,rh8498,tm,tmt0,tmt15,z1847,z4710,z4796,
!$omp&         z8498,z8510,q1d,u1d,v1d,qcnvg)
      DO L=1,LM
C          COMPUTE MOISTURE CONVERGENCE
       DO J=JSTA_2L,JEND_2U
       DO I=1,IM
        Q1D(I,J)=Q(I,J,L)
        U1D(I,J)=UH(I,J,L)
        V1D(I,J)=VH(I,J,L)
       ENDDO
       ENDDO
       CALL CALMCVG(Q1D,U1D,V1D,QCNVG)
C          COMPUTE MOISTURE CONVERGENCE
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
C
C        SET TARGET PRESSURES.
         
         P100  = PINT(I,J,NINT(LMH(I,J)))
         P98   = SIG98*P100
         P96   = SIG96*P100
         P85   = SIG85*P100
         P84   = SIG84*P100
         P47   = SIG47*P100
         P18   = SIG18*P100
C     
C     
C        COMPUTE LAYER MEAN FIELDS AT THE GIVEN K.
C
C          COMPUTE P, Z, T, AND Q AT THE MIDPOINT OF THE CURRENT ETA LAYER.
           ALPM = D50*(ALPINT(I,J,L)+ALPINT(I,J,L+1))
           DZ   = ZINT(I,J,L)-ZINT(I,J,L+1)
           PM   = EXP(ALPM)
           TM   = T(I,J,L)
           QM   = Q(I,J,L)
           QM   = AMAX1(QM,H1M12)
           QMCVG= QCNVG(I,J)
C
C     
C          COMPUTE RELATIVE HUMIDITY.
C
           QS=PQ0/PM*EXP(A2*(TM-A3)/(TM-A4))
C
           RH   = QM/QS
           IF (RH.GT.H1) THEN
              RH = H1
              QM = RH*QS
           ENDIF
           IF (RH.LT.D01) THEN
              RH = D01
              QM = RH*QS
           ENDIF
C     
C          SIGMA 0.85-1.00 MOISTURE CONVERGENCE.
           IF ((PM.LE.P100).AND.(PM.GE.P85)) THEN
              Z8510(I,J)  = Z8510(I,J) + DZ
              QM8510(I,J) = QM8510(I,J) + QMCVG*DZ
           ENDIF
C    
C          SIGMA 0.47-1.00 RELATIVE HUMIDITY.
           IF ((PM.LE.P100).AND.(PM.GE.P47)) THEN
              Z4710(I,J)  = Z4710(I,J) + DZ
              RH4710(I,J) = RH4710(I,J) + RH*DZ
           ENDIF
C
C          SIGMA 0.84-0.98 RELATIVE HUMIDITY.
           IF ((PM.LE.P98).AND.(PM.GE.P84)) THEN
              Z8498(I,J)  = Z8498(I,J) + DZ
              RH8498(I,J) = RH8498(I,J) + RH*DZ
           ENDIF
C     
C          SIGMA 0.47-0.96 RELATIVE HUMIDITY.
           IF ((PM.LE.P96).AND.(PM.GE.P47)) THEN
              Z4796(I,J)  = Z4796(I,J) + DZ
              RH4796(I,J) = RH4796(I,J) + RH*DZ
           ENDIF
C     
C          SIGMA 0.18-0.47 RELATIVE HUMIDITY.
           IF ((PM.LE.P47).AND.(PM.GE.P18)) THEN
              Z1847(I,J)  = Z1847(I,J) + DZ
              RH1847(I,J) = RH1847(I,J) + RH*DZ
           ENDIF
C
      ENDDO
      ENDDO
      ENDDO
C     
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
C        NORMALIZE TO GET LAYER MEAN VALUES.
         IF (Z8510(I,J).GT.0) THEN
            QM8510(I,J) = QM8510(I,J)/Z8510(I,J)
         ELSE
            QM8510(I,J) = SPVAL
         ENDIF
         IF (ABS(QM8510(I,J)-SPVAL).LT.SMALL)QM8510(I,J)=H1M12
C
         IF (Z4710(I,J).GT.0) THEN
            RH4710(I,J) = RH4710(I,J)/Z4710(I,J)
         ELSE
            RH4710(I,J) = SPVAL
         ENDIF
C
         IF (Z8498(I,J).GT.0) THEN
            RH8498(I,J) = RH8498(I,J)/Z8498(I,J)
         ELSE
            RH8498(I,J) = SPVAL
         ENDIF
C
         IF (Z4796(I,J).GT.0) THEN
            RH4796(I,J) = RH4796(I,J)/Z4796(I,J)
         ELSE
            RH4796(I,J) = SPVAL
         ENDIF
C
         IF (Z1847(I,J).GT.0) THEN
            RH1847(I,J) = RH1847(I,J)/Z1847(I,J)
         ELSE
            RH1847(I,J) = SPVAL
         ENDIF
      ENDDO
      ENDDO
C
C     
C     END OF ROUTINE.
C     
      RETURN
      END

