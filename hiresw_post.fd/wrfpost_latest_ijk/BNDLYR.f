      SUBROUTINE BNDLYR(PBND,TBND,QBND,RHBND,UBND,VBND,
     X     WBND,OMGBND,PWTBND,QCNVBND,LVLBND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    BNDLYR      COMPUTES CONSTANT MASS MEAN FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-01-29
C     
C ABSTRACT:  THIS ROUTINE COMPUTES CONSTANT MASS (BOUNDARY LAYER)
C   FIELDS.  THE FIELDS ARE A MEAN OVER LAYERS PARAMETER DPBND
C   (PASCALS) THICK.  THERE ARE NBND CONSTANT MASS LAYERS, EACH
C   DPBND THICK STARTING FROM THE SURFACE UP.  COMPUTED BOUNDARY 
C   LAYER FIELDS ARE PRESSURE, TEMPERATURE, SPECIFIC HUMIDITY,
C   RELATIVE HUMIDITY, U AND V WINDS, VERTICAL VELOCITY,
C   AND PRECIPITABLE WATER.  GIVEN THESE FUNDAMENTAL VARIABLES
C   OTHER FIELDS MAY BE COMPUTED.
C
C   ***WARNING*** IF YOU CHANGE PARAMETER NBND IN THIS ROUTINE 
C                 DON'T FOREGET TO CHANGE IT ALSO IN THE CALLING
C                 SUBPROGRAM, MISCLN.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-01-29  RUSS TREADON
C   93-05-07  RUSS TREADON - ADDED DOC BLOCK AND MORE COMMENTS.
C   93-06-19  RUSS TREADON - ADDED LVLBND TO PARAMETER LIST.
C   96-03-07  MIKE BALDWIN - CHANGE PWTR CALC TO INCLUDE CLD WTR
C                            SPEED UP CODE
C   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-08-18  MIKE BALDWIN - CHANGE QSBND TO RHBND IN CALL,
C                            COMPUTE RH OVER ICE
C   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
C   00-01-04  JIM TUCCILLO - MPI VERSION 
C   02-01-15  MIKE BALDWIN - WRF VERSION
C     
C     USAGE:    CALL BNDLYR(PBND,TBND,QBND,RHBND,UBND,VBND,
C                            WBND,OMGBND,PWTBND,QCNVBND)
C           
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     PBND     - LAYER MEAN PRESSURE IN NBND BOUNDARY LAYERS (NBL).
C     TBND     - LAYER MEAN TEMPERATURE IN NBL.
C     QBND     - LAYER MEAN SPECIFIC HUMIDITY IN NBL.
C     RHBND    - LAYER MEAN RELATIVE HUM. (QBND/QSBND) IN  NBL.
C     UBND     - LAYER MEAN U WIND COMPONENT IN NBL.
C     VBND     - LAYER MEAN V WIND COMPONENT IN NBL.
C     WBND     - LAYER MEAN W WIND COMPONENT IN NBL.
C     OMGBND   - LAYER MEAN VERTICAL VELOCITY IN NBL.
C     PWTBND   - LAYER PRECIPITABLE WATER IN NBL.
C     LVLBND   - ETA LAYER AT MIDPOINT OF NBL
C     QCNVBND  - LAYER MOISTURE CONVERGENCE IN NBL.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C
C     LIBRARY:
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C     
C
      use vrbls3d
      use vrbls2d
      use masks
C
C     INCLUDE GLOBAL PARAMETERS.  SET LOCAL PARAMETERS.
C        DPBND:  DEPTH OF BOUNDARY LAYER (IN PASCALS).
C     
!      INCLUDE "parmeta"
      INCLUDE "params"
      INCLUDE "CTLBLK.comm"
C
C
C
      PARAMETER (DPBND=30.E2)
      PARAMETER (NBND=6)
C     
C     DECLARE VARIABLES.
C     
      INTEGER LVLBND(IM,JM,NBND)
      REAL Q1D(IM,JM),V1D(IM,JM),U1D(IM,JM),QCNV1D(IM,JM)
      REAL PBND(IM,JM,NBND),TBND(IM,JM,NBND),QBND(IM,JM,NBND)
      REAL UBND(IM,JM,NBND),VBND(IM,JM,NBND),OMGBND(IM,JM,NBND)
      REAL WBND(IM,JM,NBND),PWTBND(IM,JM,NBND),RHBND(IM,JM,NBND)
      REAL QCNVBND(IM,JM,NBND)
C
      REAL, ALLOCATABLE :: PBINT(:,:,:),QSBND(:,:,:)
      REAL, ALLOCATABLE :: PSUM(:,:,:),QCNVG(:,:,:)
      REAL, ALLOCATABLE :: PVSUM(:,:,:),NSUM(:,:,:)
C
C     
C     INCLUDE COMMONS.
C
C*****************************************************************************
C     START BNDLYR HERE
C
      ALLOCATE (PBINT(IM,JSTA_2L:JEND_2U,NBND+1))
      ALLOCATE (QSBND(IM,JSTA_2L:JEND_2U,NBND))
      ALLOCATE (PSUM(IM,JSTA_2L:JEND_2U,NBND))
      ALLOCATE (QCNVG(IM,JSTA_2L:JEND_2U,LM))
      ALLOCATE (PVSUM(IM,JSTA_2L:JEND_2U,NBND))
      ALLOCATE (NSUM(IM,JSTA_2L:JEND_2U,NBND))
C
C
C     LOOP OVER HORIZONTAL GRID.  AT EACH MASS POINT COMPUTE
C     PRESSURE AT THE INTERFACE OF EACH BOUNDARY LAYER.
C     
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        PBINT(I,J,1)=PINT(I,J,NINT(LMH(I,J))+1)
      ENDDO
      ENDDO
C
      DO LBND=2,NBND+1
!$omp  parallel do
        DO J=JSTA,JEND
        DO I=1,IM
          PBINT(I,J,LBND)=PBINT(I,J,LBND-1)-DPBND
        ENDDO
        ENDDO
      ENDDO

C          COMPUTE MOISTURE CONVERGENCE FOR EVERY LEVEL
      DO L=1,LM
          DO J=JSTA_2L,JEND_2U
           DO I=1,IM
            Q1D(I,J)=Q(I,J,L)
            U1D(I,J)=UH(I,J,L)
            V1D(I,J)=VH(I,J,L)
           ENDDO
          ENDDO
          CALL CALMCVG(Q1D,U1D,V1D,QCNV1D)
          DO J=JSTA,JEND
           DO I=1,IM
            QCNVG(I,J,L)=QCNV1D(I,J)
           ENDDO
          ENDDO
      ENDDO

C     
C     LOOP OVER HORIZONTAL.  AT EACH MASS POINT COMPUTE 
C     MASS WEIGHTED LAYER MEAN P, T, Q, U, V, OMEGA, 
C     WAND PRECIPITABLE WATER IN EACH BOUNDARY LAYER FROM THE SURFACE UP.
C     
!$omp  parallel do
!$omp& private(dp,pm,qsat)
      DO LBND=1,NBND
        DO J=JSTA,JEND
        DO I=1,IM
          PBND(I,J,LBND)   = D00
          TBND(I,J,LBND)   = D00
          QBND(I,J,LBND)   = D00
          QSBND(I,J,LBND)  = D00
          RHBND(I,J,LBND)  = D00
          UBND(I,J,LBND)   = D00
          VBND(I,J,LBND)   = D00
          WBND(I,J,LBND)   = D00
          OMGBND(I,J,LBND) = D00
	  LVLBND(I,J,LBND) = D00
          NSUM(I,J,LBND)   = D00
          PSUM(I,J,LBND)   = D00
	  PVSUM(I,J,LBND)  = D00
          PWTBND(I,J,LBND) = D00
          QCNVBND(I,J,LBND)= D00
        ENDDO
        ENDDO
C
        DO L=1,LM
          DO J=JSTA,JEND
          DO I=1,IM
C
            PM=PMID(I,J,L)
            IF((PBINT(I,J,LBND).GE.PM).AND.
     1         (PBINT(I,J,LBND+1).LE.PM)) THEN
              DP     = PINT(I,J,L+1)-PINT(I,J,L)
              PSUM(I,J,LBND)  =PSUM(I,J,LBND)+DP
	      NSUM(I,J,LBND)  =NSUM(I,J,LBND)+1
	      LVLBND(I,J,LBND)=LVLBND(I,J,LBND)+L
              TBND(I,J,LBND)  =TBND(I,J,LBND)+T(I,J,L)*DP
              QBND(I,J,LBND)  =QBND(I,J,LBND)+Q(I,J,L)*DP
              OMGBND(I,J,LBND)=OMGBND(I,J,LBND)+OMGA(I,J,L)*DP
	      IF(MODELNAME .EQ. 'NCAR')THEN
               UBND(I,J,LBND)  =UBND(I,J,LBND)+UH(I,J,L)*DP
               VBND(I,J,LBND)  =VBND(I,J,LBND)+VH(I,J,L)*DP
	      END IF
              WBND(I,J,LBND)  =WBND(I,J,LBND)+WH(I,J,L)*DP
              QCNVBND(I,J,LBND)=QCNVBND(I,J,LBND)+QCNVG(I,J,L)*DP
              PWTBND(I,J,LBND)=PWTBND(I,J,LBND)
     1                        +(Q(I,J,L)+CWM(I,J,L))*DP*GI
              QSAT=PQ0/PM
     1          *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
              QSBND(I,J,LBND)=QSBND(I,J,LBND)+QSAT*DP
            ENDIF
          ENDDO
          ENDDO
        ENDDO


       IF(MODELNAME .EQ. 'NMM')THEN
        DO L=1,LM
C
          DO J=JSTA_M,JEND_M
          DO I=2,IM-1
            IE=I+MOD(J,2)
            IW=I+MOD(J,2)-1
            PV1=0.25*(PINT(IW,J,L)+PINT(IE,J,L)
     1               +PINT(I,J+1,L)+PINT(I,J-1,L))
            PV2=0.25*(PINT(IW,J,L+1)+PINT(IE,J,L+1)
     1               +PINT(I,J+1,L+1)+PINT(I,J-1,L+1))
            DP=PV2-PV1
            PMV=0.5*(PV1+PV2)
            IF((PBINT(IW,J,LBND).GE.PMV).AND.
     X         (PBINT(IW,J,LBND+1).LE.PMV)) THEN
              PVSUM(I,J,LBND)=PVSUM(I,J,LBND)+DP
              UBND(I,J,LBND)=UBND(I,J,LBND)+U(I,J,L)*DP
              VBND(I,J,LBND)=VBND(I,J,LBND)+V(I,J,L)*DP
            ENDIF
C
          ENDDO
          ENDDO
         ENDDO
       END IF
      ENDDO
C
!$omp  parallel do
!$omp& private(rpsum)
      DO LBND=1,NBND
        DO J=JSTA,JEND
        DO I=1,IM
          IF(PSUM(I,J,LBND).NE.0.)THEN
            RPSUM           = 1./PSUM(I,J,LBND)
	    LVLBND(I,J,LBND)= LVLBND(I,J,LBND)/NSUM(I,J,LBND)
            PBND(I,J,LBND)  = (PBINT(I,J,LBND)+PBINT(I,J,LBND+1))*0.5
            TBND(I,J,LBND)  = TBND(I,J,LBND)*RPSUM
            QBND(I,J,LBND)  = QBND(I,J,LBND)*RPSUM
            QSBND(I,J,LBND) = QSBND(I,J,LBND)*RPSUM
            OMGBND(I,J,LBND)= OMGBND(I,J,LBND)*RPSUM
	    IF(MODELNAME .EQ. 'NCAR')THEN
             UBND(I,J,LBND)  = UBND(I,J,LBND)*RPSUM
             VBND(I,J,LBND)  = VBND(I,J,LBND)*RPSUM
	    END IF 
            WBND(I,J,LBND)  = WBND(I,J,LBND)*RPSUM
            QCNVBND(I,J,LBND)  = QCNVBND(I,J,LBND)*RPSUM
          ENDIF
        ENDDO
        ENDDO
	
        IF(MODELNAME .EQ. 'NMM')THEN
         DO J=JSTA_M,JEND_M
         DO I=2,IM-1
          IF(PVSUM(I,J,LBND).NE.0.)THEN
            RPVSUM      =1./PVSUM(I,J,LBND)
            UBND(I,J,LBND)=UBND(I,J,LBND)*RPVSUM
            VBND(I,J,LBND)=VBND(I,J,LBND)*RPVSUM
          ENDIF
         ENDDO
         ENDDO
	END IF 
      ENDDO
C
C  IF NO ETA MID LAYER PRESSURES FELL WITHIN A BND LYR,
C   FIND THE CLOSEST LAYER TO THE BND LYR AND ASSIGN THE VALUES THERE
C
!$omp  parallel do
!$omp& private(delp,dp,l,pm,pmin,qsat)
!$omp&         
      DO LBND=1,NBND
        DO J=JSTA,JEND
        DO I=1,IM
          IF(PSUM(I,J,LBND).EQ.0.)THEN
            L=LM
            PMIN=9999999.
            PBND(I,J,LBND)=(PBINT(I,J,LBND)+PBINT(I,J,LBND+1))*0.5
C
            DO LL=1,LM
              PM=PMID(I,J,LL)
              DELP=ABS(PM-PBND(I,J,LBND))
              IF(DELP.LT.PMIN)THEN
                PMIN=DELP
                L=LL
              ENDIF
            ENDDO
C
            DP=PINT(I,J,L+1)-PINT(I,J,L)
            PM=PMID(I,J,L)
	    LVLBND(I,J,LBND)=L
            TBND(I,J,LBND)=T(I,J,L)
            QBND(I,J,LBND)=Q(I,J,L)
	    IF(MODELNAME .EQ. 'NCAR')THEN
             UBND(I,J,LBND)=UH(I,J,L)
             VBND(I,J,LBND)=VH(I,J,L)
	    END IF 
            WBND(I,J,LBND)=WH(I,J,L)
            QCNVBND(I,J,LBND)=QCNVG(I,J,L)
            QSAT=PQ0/PM
     1          *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
            QSBND(I,J,LBND)=QSAT
            OMGBND(I,J,LBND)=OMGA(I,J,L)
            PWTBND(I,J,LBND)=(Q(I,J,L)+CWM(I,J,L))*DP*GI
          ENDIF
C
C   RH, BOUNDS CHECK
C
          RHBND(I,J,LBND)=QBND(I,J,LBND)/QSBND(I,J,LBND)
          IF (RHBND(I,J,LBND).GT.1.0) THEN
            RHBND(I,J,LBND)=1.0
            QBND(I,J,LBND)=RHBND(I,J,LBND)*QSBND(I,J,LBND)
          ENDIF
          IF (RHBND(I,J,LBND).LT.0.01) THEN
            RHBND(I,J,LBND)=0.01
            QBND(I,J,LBND)=RHBND(I,J,LBND)*QSBND(I,J,LBND)
          ENDIF
        ENDDO
        ENDDO
C
        IF(MODELNAME .EQ. 'NMM')THEN
	 DO J=JSTA_M,JEND_M
         DO I=2,IM-1
          IF(PVSUM(I,J,LBND).EQ.0.)THEN
            LV=LM
            PMINV=9999999.
            IE=I+MOD(J,2)
            IW=I+MOD(J,2)-1
C
C           PINT HALOS UPDATED ALREADY
C
            DO LL=1,LM
              PMV=0.125*(PINT(IW,J,LL)+PINT(IE,J,LL)+
     1              PINT(I,J+1,LL)+PINT(I,J-1,LL)+
     2              PINT(IW,J,LL+1)+PINT(IE,J,LL+1)+
     3              PINT(I,J+1,LL+1)+PINT(I,J-1,LL+1))
              DELPV=ABS(PMV-PBND(I,J,LBND))
              IF(DELPV.LT.PMINV)THEN
                PMINV=DELPV
                LV=LL
              ENDIF
            ENDDO
C
            UBND(I,J,LBND)=U(I,J,LV)
            VBND(I,J,LBND)=V(I,J,LV)
          ENDIF
         ENDDO
         ENDDO     
        END IF 
      ENDDO
C
      DEALLOCATE (PBINT)
      DEALLOCATE (QSBND)
      DEALLOCATE (PSUM)
      DEALLOCATE (PVSUM)
      DEALLOCATE (QCNVG)
      DEALLOCATE (NSUM)
C
C     END OF ROUTINE
C     
      RETURN
      END
C
