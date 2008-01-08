      SUBROUTINE MIXLEN(EL0,EL)
C
C     CALCULATES LAYER-AVERAGED BLACKADAR'S MIXING LENGTH, AND PBL TOP
C     AS CPBLT*(ASYMPTOTIC EL); AND THEN EL, ACCOUNT TAKEN OF STABILITY,
C     PBL TOP AND VERTICAL GRID DISTANCE RESTRICTIONS (SEE BELOW)
C
C     SET FROM EXISTING CODES BY L. LOBOCKI, JUNE 5, 1992
C       MODIFIED BY FEDOR MESINGER, OCTOBER 13, NOVEMBER 19
C       MODIFIED BY JIM TUCCILLO FOR MPI IMPLEMENTATION
C   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-06-19  MIKE BALDWIN - WRF VERSION
C
C     INPUT:
C     ------
C
C     ZINT (IM,jsta_2l:jend_2u,LP1) - ETA INTERFACES HEIGHT FIELD
C     T    (IM,jsta_2l:jend_2u,LM)  - TEMPERATURE
C     PMID (IM,jsta_2l:jend_2u,LM)  - PRESSURE IN LAYERS
C     Q2   (IM,jsta_2l:jend_2u,LM)  - TURBULENCE KINETIC ENERGY * 2
C     HGT  (IM,jsta_2l:jend_2u)     - SURFACE ELEVATION ARRAY
C     HTM  (IM,jsta_2l:jend_2u,LM)  - HEIGHT TOPOGRAPHY MASK ARRAY
C     EL0  (IM,JM)     - ARRAY OF ASYMPTOTIC VALUES FOR MIXING LENGTH
C
C     OUTPUT:
C     -------
C
C     EL   (IM,jsta_2l:jend_2u,LM) - FIELD OF RESULTING MASTER LENGTH SCALES
C
C
C     SCRATCH AREAS:
C     --------------
C
C     VKRMZ(IM,JM)
C
C     RELEVANT CONSTANTS:
C     -------------------
C
C     VON KARMAN CONSTANT:
      use vrbls3d
      use masks
      PARAMETER (VKRM=0.4)
C     CONSTANTS NEEDED FOR THE EL(BL,ST,ZI) SCHEME:
      PARAMETER (FRG=4.*9.8,DRDRFF=0.54,CPBLT=10.,CSH=0.23*0.5
     &, EPSN2=1.E-7,EPSQ2=1.E-4,CAPA=0.28589641)
C
C     ------------------------------------------------------------------
C
!       INCLUDE "parmeta"
!       PARAMETER(LM1=LM-1)
       INCLUDE "CTLBLK.comm"
      DIMENSION EL(IM,jsta_2l:jend_2u,LM),EL0(IM,JM)
     &,         HGT(IM,JM)
     &,         APE(IM,JM,2)
C***********************************************************************
C
!$omp  parallel do
      DO L=1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          EL(I,J,L)=0.
        ENDDO
        ENDDO
      ENDDO
        DO J=JSTA,JEND
        DO I=1,IM
          HGT(I,J)=ZINT(I,J,NINT(LMH(I,J))+1)
        ENDDO
        ENDDO
C
C---THE AVERAGE EL SCHEME---------------------------(FM, AUGUST 19 MEMO)
C   FIRST GET EL IN THE LAYERS
C
!$omp  parallel do
!$omp& private(vkrmz,zl)
      DO 260 L=1,LM
      DO J=JSTA,JEND
      DO I=1,IM
        ZL=0.5*(ZINT(I,J,L)+ZINT(I,J,L+1))
        VKRMZ=(ZL-HGT(I,J))*VKRM
        EL(I,J,L)=EL0(I,J)*VKRMZ/(EL0(I,J)+VKRMZ)
      ENDDO
      ENDDO
  260 CONTINUE
C***
C***  GET NOW THE INTERFACE EL BY TWO-POINT AVERAGING OF LAYER VALUES
C***
      DO 280 L=1,LM1
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        EL(I,J,L)=0.5*(EL(I,J,L)+EL(I,J,L+1))*HTM(I,J,L+1)
      ENDDO
      ENDDO
 280  CONTINUE
C
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        EL(I,J,LM)=0.0
      ENDDO
      ENDDO
C---STABILITY, PBL TOP, AND VERTICAL GRID DISTANCE RESTRICTIONS:--------
C   COMPUTE EL STABLE AND
C   * USE THE SMALLER OF EL BLACKADAR, EL STABLE IF WITHIN PBL;
C   * USE THE SMALLEST OF EL STABLE, ELVGD, AND VKRMZ IF ABOVE PBL
C       (ASSUME PBL TOP IS AT CPBLT*EL0(K));
      DO J=JSTA_M,JEND_M
      DO I=1,IM
        APE(I,J,1)=(1.E5/PMID(I,J,1))**CAPA
      ENDDO
      ENDDO
C
      DO 380 L=1,LM1
!$omp  parallel do
!$omp& private(elst,elvgd,ensq,q2kl,ziag)
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        APE(I,J,2)=(1.E5/PMID(I,J,L+1))**CAPA
        ENSQ=HTM(I,J,L+1)*
     1       FRG*(T(I,J,L)*APE(I,J,1)-T(I,J,L+1)*APE(I,J,2))/
     2       ((T(I,J,L)*APE(I,J,1)+T(I,J,L+1)*APE(I,J,2))*
     3        (ZINT(I,J,L)-ZINT(I,J,L+2))+EPSN2)
        ENSQ=AMAX1(ENSQ,EPSN2)
        Q2KL=AMAX1(EPSQ2,Q2(I,J,L))
        ELST=DRDRFF*SQRT(Q2KL/ENSQ)
CWAS    ELST=DRDRFF*SQRT(Q2(I,J,L)/ENSQ)
        ZIAG=ZINT(I,J,L+1)-HGT(I,J)
C
        IF(ZIAG.LT.CPBLT*EL0(I,J))THEN
          EL(I,J,L)=AMIN1(EL(I,J,L),ELST)
        ELSE
          ELVGD=CSH*(ZINT(I,J,L)-ZINT(I,J,L+2))
          EL(I,J,L)=AMIN1(ELST,ELVGD,VKRM*ZIAG)
        ENDIF
        APE(I,J,1)=APE(I,J,2)
      ENDDO
      ENDDO
  380 CONTINUE
C
      RETURN
      END

