      SUBROUTINE CALGUST(LPBL,ZPBL,GUST)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALGUST      COMPUTE MAX WIND LEVEL 
C   PRGRMMR: MANIKIN        ORG: W/NP2   DATE: 97-03-04       
C     
C ABSTRACT:  
C     THIS ROUTINE COMPUTES SURFACE WIND GUST BY MIXING
C  DOWN MOMENTUM FROM THE LEVEL AT THE HEIGHT OF THE PBL
C     
C     
C PROGRAM HISTORY LOG:
C   03-10-15 GEOFF MANIKIN
C   05-03-09 H CHUANG - WRF VERSION
C   05-07-07 BINBIN ZHOU - ADD RSM   
C   
C USAGE:    CALL CALGUST(GUST) 
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     GUST    - SPEED OF THE MAXIMUM SFC WIND GUST 
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       H2V     
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
      use vrbls2d 
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
      INTEGER LPBL(IM,JM)
      REAL ZPBL(IM,jsta_2l:jend_2u),GUST(IM,JM)
C     
C     
C*****************************************************************************
C     START CALMXW HERE.
C     
C     LOOP OVER THE GRID.
C    
      DO J=JSTA,JEND
      DO I=1,IM
        GUST(I,J) = SPVAL 
      ENDDO
      ENDDO
C
C     ASSUME THAT U AND V HAVE UPDATED HALOS
C
!$omp  parallel do
!$omp& private(ie,iw,mxww,u0,v0,wind)
      DO 20 J=JSTA_M,JEND_M
      DO 20 I=2,IM-1
       L=LPBL(I,J) 
       IF(MODELNAME .EQ. 'NMM')THEN
        IE=I+MOD(J+1,2) 
        IW=I+MOD(J+1,2)-1
	
        USFC=D25*(U10(I,J-1)+U10(IW,J)+
     X             U10(IE,J)+U10(I,J+1)) 
        VSFC=D25*(V10(I,J-1)+V10(IW,J)+
     X             V10(IE,J)+V10(I,J+1))
        SFCWIND=SQRT(USFC**2 + VSFC**2)
        U0 = D25*(U(I,J-1,L)+U(IW,J,L)+
     X             U(IE,J,L)+U(I,J+1,L))
        V0 = D25*(V(I,J-1,L)+V(IW,J,L)+
     X             V(IE,J,L)+V(I,J+1,L))
        WIND=SQRT(U0**2 + V0**2)
!	if(i.eq.105.and.j.eq.58)print*,'U10s V10s= ',
!     x  U10(I,J-1),U10(IW,J),U10(IE,J),U10(I,J+1),
!     x	V10(I,J-1),V10(IW,J),V10(IE,J),V10(I,J+1)
        
       ELSE IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM')THEN
        USFC=U10(I,J)
        VSFC=V10(I,J)
        SFCWIND=SQRT(USFC**2 + VSFC**2) 
        U0=U(I,J,L)
        V0=V(I,J,L)
        WIND=SQRT(U0**2 + V0**2)
       END IF
       DELWIND=WIND - SFCWIND
       ZSFC=FIS(I,J)*GI
c      DZ=ZPBL(I,J)-ZSFC
       DELWIND=DELWIND*(1.0-AMIN1(0.5,ZPBL(I,J)/2000.))
!       if(i.eq.105.and.j.eq.58)print*,'DELWIND,ZPBL ',
!     x  DELWIND,ZPBL(I,J)
       GUST(I,J)=SFCWIND+DELWIND
   10 CONTINUE
   20 CONTINUE

C     END OF ROUTINE.
C     
      RETURN
      END
