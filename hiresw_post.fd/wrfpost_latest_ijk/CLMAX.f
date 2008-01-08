      SUBROUTINE CLMAX(EL0,SQZ,SQ,RQ2L,RQ2H)
C
C     CALCULATES THE FREE-ATMOSPHERE ASYMPTOTE OF THE TURBULENCE LENGTH
C     SCALE (L-INF IN THE BLACKADAR's FORMULA) FROM THE DISTRIBUTION
C     OF THE TURBULENT ENERGY (see MY82)
C
C     EXTRACTED FROM EXISTING CODE BY L. LOBOCKI, JULY 28, 1992
C   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-06-19  MIKE BALDWIN - WRF VERSION
C
C     INPUT:
C     ------
C
C     PINT (IM,jsta_2l:jend_2u,LP1)  - PRESSURE ON INTERFACES 
C     HTM  (IM,jsta_2l:jend_2u,LM)  - HEIGHT TOPOGRAPHY MASK ARRAY
C     Q2   (IM,jsta_2l:jend_2u,LM)  - TWICE THE TURBULENT ENERGY FIELD
C     ZINT (IM,jsta_2l:jend_2u,LP1) - ETA INTERFACES HEIGHT FIELD
C     SM   (IM,jsta_2l:jend_2u)     - SEA MASK
C     HGT  (IM,jsta_2l:jend_2u)     - SURFACE ELEVATION ARRAY
C     LMH  (IM,jsta_2l:jend_2u)     - TOPOGRAPHY INDEXES ARRAY
C
C     OUTPUT:
C     -------
C
C     EL0 (IM,JM)      - ARRAY OF RESULTING ASYMPTOTIC MIXING LENGTHS
C
C
C     SCRATCH AREAS:
C     --------------
C
C     SQZ(IM,JM),SQ(IM,JM),RQ2L(IM,JM),RQ2H(IM,JM)
C
C
C     RELEVANT CONSTANTS:
C     -------------------
C
C     PROPORTIONALITY CONSTANT BETWEEN ASYMPTOTIC MIXING LENGTH AND THE
C     S.D. OF Q DISTRIBUTION, FOR LAND AND SEA AREAS, CORRESPONDINGLY:
      use vrbls3d
      use vrbls2d
      use masks
      PARAMETER (ALPHAL=0.2, ALPHAS=0.2)
C
C     ASYMPTOTIC MIXING LENGTH LIMITATIONS:
      PARAMETER (EL0M=300.0, ELMIN=11.0)
C
C     MINIMAL VALUE OF TURBULENT ENERGY:
      PARAMETER (EPSQ2=0.2)
C
C     ------------------------------------------------------------------
C
!      INCLUDE "parmeta"
!      PARAMETER (LP1=LM+1)
      INCLUDE "CTLBLK.comm"
                         D I M E N S I O N
     & EL0(IM,JM),SQZ(IM,JM),SQ(IM,JM),RQ2L(IM,JM),RQ2H(IM,JM)
     &,HGT(IM,JM)
C     ------------------------------------------------------------------
C
C
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        SQZ(I,J)=0.0
        SQ(I,J)=0.0
        RQ2H(I,J)=0.0
        HGT(I,J)=ZINT(I,J,NINT(LMH(I,J)))
      ENDDO
      ENDDO
C
      DO 220 L=1,LM
!$omp  parallel do
!$omp& private(dp,rq2m)
      DO J=JSTA,JEND
      DO I=1,IM
        IF(Q2(I,J,L).LE.EPSQ2) THEN
          RQ2L(I,J)=0.0
        ELSE
          RQ2L(I,J)=SQRT(Q2(I,J,L))
        ENDIF
C
C         -----------------------------------------------------------------
C         THIS PART OF THE CODE IS LEFT FOR TESTING OTHER PARAMETERIZATION
C         SCHEMES 
C
C         IF (L.GE.LMH(I,J)) GOTO 215
c         RQ2L(I,J)=SQRT(Q2(I,J,L))
c         IF(Q2(I,J,L).LT.0.0)THEN
c           write(3,*)'NEGATIVE Q2 AT (I,J,L)=(',I,',',J,',',L,'): ',
c                     Q2(I,J,L)
c           STOP
c         ENDIF
C         -----------------------------------------------------------------
C
          DP=PINT(I,J,L+1)-PINT(I,J,L)
C***
C***      SUM OF Q2 AT BOTH LOWER & UPPER SURFACES:
C***
          RQ2M=(RQ2H(I,J)+RQ2L(I,J))
C***
C***      INTEGRAL OF Q*Z OVER DP
C***
          SQZ(I,J)=((ZINT(I,J,L)+ZINT(I,J,L+1))*0.5-HGT(I,J))*RQ2M*DP
     1              +SQZ(I,J)
C***
C***      INTEGRAL OF Q OVER DP:
C***
          SQ(I,J)=RQ2M*DP+SQ(I,J)
          RQ2H(I,J)=RQ2L(I,J)
      ENDDO
      ENDDO
c215  CONTINUE
 220  CONTINUE
C***
C***    CLIPPING & APPLYING DIFFERENT VALUES OF THE PROPORTIONALITY 
C***    CONSTANT ALPHA FOR THE LAND AND SEA AREA:
C***
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        EL0(I,J)= AMAX1(AMIN1(
     1    ((SM(I,J)*ALPHAS+(1.0-SM(I,J))*ALPHAL)*SQZ(I,J)
     2     /(SQ(I,J)+EPSQ2)),
     3    EL0M),ELMIN)
      ENDDO
      ENDDO
C
      RETURN
      END

