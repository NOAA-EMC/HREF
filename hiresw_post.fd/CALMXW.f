      SUBROUTINE CALMXW(MXWP,MXWZ,MXWU,MXWV)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALMXW      COMPUTE MAX WIND LEVEL 
C   PRGRMMR: MANIKIN        ORG: W/NP2   DATE: 97-03-04       
C     
C ABSTRACT:  
C     THIS ROUTINE COMPUTES MAX WIND LEVEL.  AT EACH POINT,
C   IT FINDS THE MAX WIND ABOVE 500 MB AND DETERMINES THE
C   PRESSURE AND HEIGHT AT THAT LEVEL.
C     
C     
C PROGRAM HISTORY LOG:
C   97-03-04 GEOFF MANIKIN
C   98-06-15 T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-02 JIM TUCCILLO - MPI VERSION
C   02-01-15  MIKE BALDWIN - WRF VERSION
C   05-02-24 H CHUANG - ADD WRF NMM COMPONENTS 
C   05-07-07 BINBIN ZHOU - ADD RSM 
C   
C USAGE:    CALL  CALMXW(MXWP,MXWZ,MXWU,MXWV)
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     MXWP    - PRESSURE LEVEL OF THE MAX WIND
C     MXWZ    - HEIGHT OF THE MAX WIND
C     MXWU    - U COMPONENT OF THE ACTUAL MAX WIND 
C     MXWV    - V COMPONENT OF THE ACTUAL MAX WIND
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C
C     LIBRARY:
C       COMMON   - 
C                  LOOPS
C                  OPTIONS
C                  MASKS
C                  INDX
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C     
C     
      use vrbls3d
      use masks
C 
C     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
C
!      INCLUDE "parmeta"
      INCLUDE "params"
C
      INCLUDE "CTLBLK.comm"
C     
C     DECLARE VARIABLES.
C     
      REAL MXWP(IM,JM),MXWZ(IM,JM),MXWU(IM,JM),MXWV(IM,JM),MXWW 
      INTEGER IHE(JM),IHW(JM) 
C     
C     
C*****************************************************************************
C     START CALMXW HERE.
C     
C     LOOP OVER THE GRID.
C    
      CRITP=5.0E4
C
      DO J=JSTA,JEND
      DO I=1,IM
        MXWU(I,J) = SPVAL
        MXWV(I,J) = SPVAL
        MXWP(I,J) = SPVAL
        MXWZ(I,J) = SPVAL 
      ENDDO
      ENDDO
C
!$omp  parallel do
!$omp& private(ie,iw,mxww,u0,v0,wind)
      IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM')THEN 
       DO 20 J=JSTA,JEND
       DO 20 I=1,IM
        MXWW  = -1000.
        LLMH=NINT(LMH(I,J))
C
        DO 10 L= LLMH-1,1,-1
         U0 = UH(I,J,L)
         V0 = VH(I,J,L)
         WIND = SQRT(U0**2 + V0**2)

C  MAX WIND LEVEL MUST BE ABOVE THE 500 MB 

         IF (WIND .GT. MXWW .and. PMID(I,J,L) .LT. CRITP) THEN
           MXWU(I,J) = U0
           MXWV(I,J) = V0
           MXWW = WIND 
           MXWP(I,J) = PMID(I,J,L)
           MXWZ(I,J) = ZMID(I,J,L)
         ENDIF
   10   CONTINUE
   20  CONTINUE

      ELSE IF(MODELNAME .EQ. 'NMM')THEN
       
       DO J=JSTA_M,JEND_M
        IHE(J)=MOD(J+1,2)
        IHW(J)=IHE(J)-1
       ENDDO
       
       DO 40 J=JSTA_M,JEND_M
       DO 40 I=2,IM-1
        IE=I+IHE(J)
        IW=I+IHW(J)
        MXWW  = -1000.
        LLMH=NINT(LMH(I,J))
C
        DO 30 L= LLMH-1,1,-1
         U0 = D25*(U(I,J-1,L)+U(IW,J,L)+
     X             U(IE,J,L)+U(I,J+1,L))
         V0 = D25*(V(I,J-1,L)+V(IW,J,L)+
     X             V(IE,J,L)+V(I,J+1,L))
         WIND = SQRT(U0**2 + V0**2)
	 
	 IF (WIND .GT. MXWW .and. PMID(I,J,L) .LT. CRITP) THEN
           MXWU(I,J) = U0
           MXWV(I,J) = V0
           MXWW = WIND
           MXWP(I,J) = PMID(I,J,L) 
           MXWZ(I,J)=ZMID(I,J,L)
         END IF
   30   CONTINUE
   40  CONTINUE

      END IF
C     END OF ROUTINE.
C     
      RETURN
      END
