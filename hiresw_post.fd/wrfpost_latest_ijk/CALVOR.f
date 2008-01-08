      SUBROUTINE CALVOR(UWND,VWND,ABSV)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALVOR      COMPUTES ABSOLUTE VORTICITY
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
C     
C ABSTRACT:  
C     THIS ROUTINE COMPUTES THE ABSOLUTE VORTICITY.
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-22  RUSS TREADON
C   98-06-08  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION
C   02-01-15  MIKE BALDWIN - WRF VERSION C-GRID
C   05-03-01  H CHUANG - ADD NMM E GRID
C   05-05-17  H CHUANG - ADD POTENTIAL VORTICITY CALCULATION
C   05-07-07  B ZHOU   - ADD RSM IN COMPUTING DVDX, DUDY AND UAVG

C     
C USAGE:    CALL CALVOR(UWND,VWND,ABSV)
C   INPUT ARGUMENT LIST:
C     UWND     - U WIND (M/S) MASS-POINTS
C     VWND     - V WIND (M/S) MASS-POINTS
C
C   OUTPUT ARGUMENT LIST: 
C     ABSV     - ABSOLUTE VORTICITY (1/S) MASS-POINTS
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C
      use vrbls2d
      use masks
C
C     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
C     
!      INCLUDE "parmeta"
      INCLUDE "params"
      PARAMETER (OMEGA=7.292E-5,TWOMG=2.*OMEGA)
C
C     DECLARE COMMONS.
      INCLUDE "CTLBLK.comm"
      INCLUDE "GRIDSPEC.comm"
C
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESTRT,SIGMA,OLDRD,STRD
      REAL ABSV(IM,JM), UWND(IM,JM), VWND(IM,JM)
      INTEGER IHE(JM),IHW(JM)
        INTEGER DXVAL,DYVAL,CENLAT,CENLON,TRUELAT1,TRUELAT2
        INTEGER LATSTART,LONSTART,LATLAST,LONLAST 
C     
C***************************************************************************
C     START CALVOR HERE.
C     
C     LOOP TO COMPUTE ABSOLUTE VORTICITY FROM WINDS.
C     
!$omp  parallel do
      DO J=JSTA_2L,JEND_2U
      DO I=1,IM
        ABSV(I,J) = D00
      ENDDO
      ENDDO

      DO J=JSTA_2L,JEND_2U
        IHW(J)=-MOD(J,2)
        IHE(J)=IHW(J)+1
      ENDDO
      print*,'dyval in CALVOR= ',DYVAL 
  
      CALL EXCH_F(UWND)
C
!$omp  parallel do
!$omp& private(r2dx,r2dy,dvdx,dudy)
      print*,'jsta_m, jend_m in calvor= ',jsta_m,jend_m
      DO J=JSTA_M,JEND_M
        JMT2=JM/2+1
        TPHI=(J-JMT2)*(DYVAL/1000.)*DTR
        DO I=2,IM-1
         IF(VWND(I+1,J).LT.SPVAL.AND.VWND(I-1,J).LT.SPVAL.AND.
     &      UWND(I,J+1).LT.SPVAL.AND.UWND(I,J-1).LT.SPVAL) THEN          
          R2DX   = 1./(2.*DX(I,J))
          R2DY   = 1./(2.*DY(I,J))
	  IF(MODELNAME .EQ. 'NCAR' .OR.MODELNAME.EQ.'RSM')THEN
           DVDX   = (VWND(I+1,J)-VWND(I-1,J))*R2DX
           DUDY   = (UWND(I,J+1)-UWND(I,J-1))*R2DY
	   UAVG=0.25*(UWND(I+1,J)+UWND(I-1,J)
     1              +UWND(I,J+1)+UWND(I,J-1))
!  is there a (f+tan(phi)/erad)*u term?
           ABSV(I,J)=DVDX-DUDY+F(I,J)+UAVG*TAN(GDLAT(I,J)*DTR)/ERAD  ! not sure about this???
	  ELSE IF (MODELNAME .EQ. 'NMM')THEN
	   DVDX   = (VWND(I+IHE(J),J)-VWND(I+IHW(J),J))*R2DX
           DUDY   = (UWND(I,J+1)-UWND(I,J-1))*R2DY
	   UAVG=0.25*(UWND(I+IHE(J),J)+UWND(I+IHW(J),J)
     1              +UWND(I,J+1)+UWND(I,J-1))
!  is there a (f+tan(phi)/erad)*u term?
           ABSV(I,J)=DVDX-DUDY+F(I,J)+UAVG*TAN(TPHI)/ERAD
!           ABSV(I,J)=DVDX-DUDY+F(I,J)
	  ELSE
           PRINT*,'UNKNOW MODEL-> bitmask out vorticity'
	   ABSV(I,J)=SPVAL
	  END IF 
!  is there a (f+tan(phi)/erad)*u term?
         ELSE
          ABSV(I,J)=SPVAL
         ENDIF
        ENDDO
      ENDDO
C     
C     END OF ROUTINE.
C     
      RETURN
      END
