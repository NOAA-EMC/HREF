      SUBROUTINE CALMCVG(Q1D,U1D,V1D,QCNVG)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALMCVG     COMPUTES MOISTURE CONVERGENCE
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-01-22       
C     
C ABSTRACT:
C     GIVEN SPECIFIC HUMIDITY, Q, AND THE U-V WIND COMPONENTS
C     THIS ROUTINE EVALUATES THE VECTOR OPERATION, 
C                      DEL DOT (Q*VEC)
C     WHERE,
C        DEL IS THE VECTOR GRADIENT OPERATOR,
C        DOT IS THE STANDARD DOT PRODUCT OPERATOR, AND
C        VEC IS THE VECTOR WIND.
C     MINUS ONE TIMES THE RESULTING SCALAR FIELD IS THE 
C     MOISTURE CONVERGENCE WHICH IS RETURNED BY THIS ROUTINE.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-01-22  RUSS TREADON
C   98-06-08  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION              
C   02-04-23  MIKE BALDWIN - WRF C-GRID VERSION     
C   05-07-07  BINBIN ZHOU - ADD RSM A GRID
C   06-04-25  H CHUANG - BUG FIXES TO CORECTLY COMPUTE MC AT BOUNDARIES 
C   
C USAGE:    CALL CALMCVG(Q1D,U1D,V1D,QCNVG)
C   INPUT ARGUMENT LIST:
C     Q1D      - SPECIFIC HUMIDITY AT P-POINTS (KG/KG)
C     U1D      - U WIND COMPONENT (M/S) AT P-POINTS
C     V1D      - V WIND COMPONENT (M/S) AT P-POINTS
C
C   OUTPUT ARGUMENT LIST: 
C     QCNVG    - MOISTURE CONVERGENCE (1/S) AT P-POINTS
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - MASKS
C                  DYNAM
C                  OPTIONS
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C
C     
C     
      use masks
C     
!      INCLUDE "parmeta"
      INCLUDE "params"
C
C     DECLARE COMMONS.
      INCLUDE "CTLBLK.comm"
C
C     DECLARE VARIABLES.
C     
      REAL QDIV, R2DY, R2DX
      REAL Q1D(IM,JM), U1D(IM,JM), V1D(IM,JM)
      REAL QCNVG(IM,JM), UWND(IM,JM), VWND(IM,JM),QV(IM,JM)
      INTEGER IHE(JM),IHW(JM),IVE(JM),IVW(JM)
      CHARACTER*1 AGRID
C     
C     
C***************************************************************************
C     START CALMCVG HERE.
C
C     
C     INITIALIZE MOISTURE CONVERGENCE ARRAY.  LOAD TEMPORARY WIND ARRAYS.
C     
!$omp  parallel do
      DO J=JSTA_2L,JEND_2U
      DO I=1,IM
        QCNVG(I,J) = D00
        UWND(I,J)  = U1D(I,J)
        VWND(I,J)  = V1D(I,J)
        IF (UWND(I,J).EQ.SPVAL) UWND(I,J) = D00
        IF (VWND(I,J).EQ.SPVAL) VWND(I,J) = D00
      ENDDO
      ENDDO
      
      CALL EXCH_F(Q1D)
      CALL EXCH_F(VWND)
C
!$omp  parallel do
!$omp& private(qdiv,qudx,qvdy,r2dx,r2dy)
      IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM')THEN
       DO J=JSTA_M,JEND_M
        DO I=2,IM-1
         IF(VWND(I,J+1).LT.SPVAL.AND.VWND(I,J-1).LT.SPVAL.AND.
     &      UWND(I+1,J).LT.SPVAL.AND.UWND(I-1,J).LT.SPVAL) THEN
          R2DX   = 1./(2.*DX(I,J))   !MEB DX?
          R2DY   = 1./(2.*DY(I,J))   !MEB DY?  
          QUDX   = (Q1D(I+1,J)*UWND(I+1,J)-Q1D(I-1,J)*UWND(I-1,J))*R2DX
          QVDY   = (Q1D(I,J+1)*VWND(I,J+1)-Q1D(I,J-1)*VWND(I,J-1))*R2DY
          QDIV   = QUDX + QVDY
          QCNVG(I,J) = -1.*QDIV
         ELSE
          QCNVG(I,J) = SPVAL
         ENDIF
        ENDDO
       ENDDO
      ELSE IF(MODELNAME .EQ. 'NMM')THEN

       DO J=JSTA_M,JEND_M
        IHE(J)=MOD(J+1,2)
        IHW(J)=IHE(J)-1
        IVE(J)=MOD(J,2)
        IVW(J)=IVE(J)-1 
       END DO
     
       DO J=JSTA_M,JEND_M
       ISTA=1+MOD(J+1,2)
       IEND=IM-MOD(J,2)
       DO I=ISTA,IEND
         QV(I,J) = D25*(Q1D(I,J-1)+Q1D(I+IVW(J),J)
     1                 +Q1D(I+IVE(J),J)+Q1D(I,J+1))
       END DO
       END DO

       CALL EXCH_F(QV)
!       CALL EXCH_F(VWND)

C
!$omp  parallel do
!$omp& private(iend,qdiv,qudx,qvdy,r2dx,r2dy)
       DO J=JSTA_M2,JEND_M2
        IEND=IM-1-MOD(J,2)
        DO I=2,IEND
          R2DX   = 1./(2.*DX(I,J))
          R2DY   = 1./(2.*DY(I,J))
          QUDX   = (QV(I+IHE(J),J)*UWND(I+IHE(J),J)
     1             -QV(I+IHW(J),J)*UWND(I+IHW(J),J))*R2DX
          QVDY   = (QV(I,J+1)*VWND(I,J+1)-QV(I,J-1)*VWND(I,J-1))*R2DY
          QDIV   = QUDX + QVDY

!	  IF(I.LE.2 .OR. I .GE. IEND )THEN
!	   IF(HBM2(I,J) .GT. 0.)THEN 
!	    PRINT*,'converting hbm2 to zero ',i,j
!	    HBM2(I,J)=0.0
!	   END IF
!	  END IF   

          QCNVG(I,J) = -1.*QDIV*HBM2(I,J)
        ENDDO
       ENDDO

      ENDIF
!meb not sure about the indexing for the c-grid
C
C     END OF ROUTINE.
C     
      RETURN
      END

