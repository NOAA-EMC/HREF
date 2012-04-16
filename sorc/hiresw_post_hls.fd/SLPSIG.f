      SUBROUTINE SLPSIG(PD,FIS,SM,T,Q,FI,IMP,JMP,SPL,LSL,DETA,PT,PSLP)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  SLPSIG      SLP REDUCTION
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-04-22
C
C ABSTRACT:  THIS ROUTINE COMPUTES THE SEA LEVEL PRESSURE
C            REDUCTION USING EITHER THE MESINGER RELAXATION
C            METHOD OR THE STANDARD NCEP REDUCTION FOR
C            SIGMA COORDINATES.  A BY-PRODUCT IS THE
C            SET OF VALUES FOR THE UNDERGROUND TEMPERATURES
C            ON THE SPECIFIED PRESSURE LEVELS
C
C PROGRAM HISTORY LOG:
C   99-09-23  T BLACK - REWRITTEN FROM ROUTINE SLP (ETA
C                       COORDINATES)
C
C USAGE:  CALL SLPSIG FROM SUBROUITNE ETA2P
C
C   INPUT ARGUMENT LIST:
C     PD   - SFC PRESSURE MINUS PTOP
C     FIS  - SURFACE GEOPOTENTIAL
C     T    - TEMPERATURE 
C     Q    - SPECIFIC HUMIDITY
C     FI   - GEOPOTENTIAL
C     PT   - TOP PRESSURE OF DOMAIN
C
C   OUTPUT ARGUMENT LIST:
C     PSLP - THE FINAL REDUCED SEA LEVEL PRESSURE ARRAY
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C             NONE
C
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
C-----------------------------------------------------------------------
                            P A R A M E T E R
     & (LP1=LSM+1,IMJM=IM*JM-JM/2,JM2=JM-2)
                            P A R A M E T E R
     & (NFILL=6,NRLX1=250,NRLX2=100,KSLPD=1
     &, OVERRC=1.75,AD05=OVERRC*0.05,CFT0=OVERRC-1.
     &, RD=287.04,ROG=RD/9.806)
C-----------------------------------------------------------------------
                            R E A L
     & PD(IM,JM),FIS(IM,JM),SM(IM,JM)
     &,HTM(IM,JM,LSM),T(IMP,JMP,LSL),Q(IMP,JMP,LSL),FI(IMP,JMP,LSL)
                            R E A L
     & PSLP(IM,JM),TTV(IM,JM)
     &,PBI(IM,JM),SLPX(IM,JM),P1(IM,JM)
                            R E A L
     & SPL(LSM),SPLI(LSM)
                            R E A L
     & DETA(LM),RDETA(LM),AETA(LM),F4Q2(LM),ETA(LM+1),DFL(LM+1)
C-----------------------------------------------------------------------
      real tempt(7,7)
C-----------------------------------------------------------------------
                            I N T E G E R
     & KMNTM(LSM),IMNT(IMJM,LSM),JMNT(IMJM,LSM)
     &,LMH(IM,JM)
                            I N T E G E R
     & IHE(JM),IHW(JM),IVE(JM),IVW(JM)
C-----------------------------------------------------------------------
                            L O G I C A L
     & SIGMA,STDRD,DONE(IM,JM)
C-----------------------------------------------------------------------
      STDRD=.FALSE.
C-----------------------------------------------------------------------
C***
C***  CALCULATE THE I-INDEX EAST-WEST INCREMENTS
C***
      DO J=1,JM
        IHE(J)=MOD(J+1,2)
        IHW(J)=IHE(J)-1
        IVE(J)=MOD(J,2)
        IVW(J)=IVE(J)-1
      ENDDO
C-----------------------------------------------------------------------
C***
C***  INITIALIZE ARRAYS.  LOAD SLP ARRAY WITH SURFACE PRESSURE.
C***
!$omp parallel do 
      DO J=1,JM
      DO I=1,IM
        PSLP(I,J)=PD(I,J)+PT
        PBI (I,J)=PSLP(I,J)
        TTV(I,J)=0.
        LMH(I,J)=0
      ENDDO
      ENDDO
C
C***  CALCULATE SEA LEVEL PRESSURE FOR PROFILES (AND POSSIBLY
C***  FOR POSTING BY POST PROCESSOR).
C
C***  "STDRD" REFERS TO THE "STANDARD" SLP REDUCTION SCHEME.
C
      IF(STDRD)GO TO 400
C--------------------------------------------------------------------
C***
C***  CREATE A 3-D "HEIGHT MASK" FOR THE SPECIFIED PRESSURE LEVELS
C***  (1 => ABOVE GROUND) AND A 2-D INDICATOR ARRAY THAT SAYS 
C***  WHICH PRESSURE LEVEL IS THE LOWEST ONE ABOVE THE GROUND
C***
      DO 100 L=1,LSM
      SPLL=SPL(L)
C       
      DO J=1,JM
      DO I=1,IM
        PSFC=PD(I,J)+PT
        PCHK=PSFC
        IF(NFILL.GT.0)THEN
          DO LL=1,NFILL
            PCHK=PCHK-DETA(LM+1-LL)*PD(I,J)
          ENDDO
        ENDIF
        IF(SM(I,J).GT.0.5.AND.FIS(I,J).LT.10.)PCHK=PSFC
C
c       IF(SPLL.LT.PSFC)THEN
        IF(SPLL.LT.PCHK)THEN
          HTM(I,J,L)=1.
        ELSE
          HTM(I,J,L)=0.
          IF(L.GT.1.AND.HTM(I,J,L-1).GT.0.5)LMH(I,J)=L-1
        ENDIF
C
        IF(L.EQ.LSM.AND.HTM(I,J,L).GT.0.5)LMH(I,J)=LSM
      ENDDO
      ENDDO
C
  100 CONTINUE
C--------------------------------------------------------------------
C***
C***  WE REACH THIS LINE IF WE WANT THE MESINGER ETA SLP REDUCTION
C***  BASED ON RELAXATION TEMPERATURES.  THE FIRST STEP IS TO
C***  FIND THE HIGHEST LAYER CONTAINING MOUNTAINS.
C***
      DO 210 L=LSM,1,-1
C
      DO J=1,JM
      DO I=1,IM
        IF(HTM(I,J,L).LT.0.5)GO TO 210
      ENDDO
      ENDDO
C
      LHMNT=L+1
      GO TO 220
  210 CONTINUE
C
  220 CONTINUE
      IF(LHMNT.EQ.LP1)THEN
        GO TO 430
      ENDIF
C***
C***  NOW GATHER THE ADDRESSES OF ALL THE UNDERGROUND POINTS.
C***
!$omp parallel do private(kmn,kount)
      DO 250 L=LHMNT,LSM
      KMN=0
      KMNTM(L)=0
      KOUNT=0
      DO 240 J=3,JM-2
      DO 240 I=2,IM-1
      KOUNT=KOUNT+1
      IMNT(KOUNT,L)=0
      JMNT(KOUNT,L)=0
      IF(HTM(I,J,L).GT.0.5)GO TO 240
      KMN=KMN+1
      IMNT(KMN,L)=I
      JMNT(KMN,L)=J
  240 CONTINUE
      KMNTM(L)=KMN
  250 CONTINUE
C
C***  AS THE FIRST GUESS, SET THE UNDERGROUND TEMPERATURES EQUAL
C***  TO 0.0C.
C
c     IF(NTSD.EQ.1)THEN
        KMM=KMNTM(LSM)
!$omp parallel do private(i,j,lmap1),shared(t)
        DO 260 KM=1,KMM
        I=IMNT(KM,LSM)
        J=JMNT(KM,LSM)
        LMAP1=LMH(I,J)+1
        DO 260 L=LMAP1,LSM
        T(I,J,L)=273.15
  260   CONTINUE
c     ENDIF
C
C***  CREATE A TEMPORARY TV ARRAY, AND FOLLOW BY SEQUENTIAL
C***  OVERRELAXATION, DOING NRLX PASSES.
C
c     IF(NTSD.EQ.1)THEN
        NRLX=NRLX1
c     ELSE
c       NRLX=NRLX2
c     ENDIF
C
!$omp parallel do private(i,j,tinit,ttv)
      DO 300 L=LHMNT,LSM
C
      DO 270 J=1,JM
      DO 270 I=1,IM
      TTV(I,J)=T(I,J,L)
  270 CONTINUE
C
C***  FOR GRID BOXES NEXT TO MOUNTAINS, COMPUTE TV TO USE AS
C***  BOUNDARY CONDITIONS FOR THE RELAXATION UNDERGROUND
C
      DO J=3,JM-2
      DO I=2,IM-1
        IF(HTM(I,J,L).GT.0.5.AND.
     1     HTM(I+IHW(J),J-1,L)*HTM(I+IHE(J),J-1,L)
     2    *HTM(I+IHW(J),J+1,L)*HTM(I+IHE(J),J+1,L)
     3    *HTM(I-1     ,J  ,L)*HTM(I+1     ,J  ,L)
     4    *HTM(I       ,J-2,L)*HTM(I       ,J+2,L).LT.0.5)THEN
          TTV(I,J)=T(I,J,L)*(1.+0.608*Q(I,J,L))
        ENDIF
      ENDDO
      ENDDO
C
      KMM=KMNTM(L)
C
      DO 285 N=1,NRLX
      DO 280 KM=1,KMM
      I=IMNT(KM,L)
      J=JMNT(KM,L)
      TINIT=TTV(I,J)
      TTV(I,J)=AD05*(4.*(TTV(I+IHW(J),J-1)+TTV(I+IHE(J),J-1)
     1                  +TTV(I+IHW(J),J+1)+TTV(I+IHE(J),J+1))
     2                  +TTV(I-1,J)       +TTV(I+1,J)
     3                  +TTV(I,J-2)       +TTV(I,J+2))
     4                  -CFT0*TTV(I,J)
  280 CONTINUE
C
  285 CONTINUE
C
      DO 290 KM=1,KMM
      I=IMNT(KM,L)
      J=JMNT(KM,L)
      T(I,J,L)=TTV(I,J)
  290 CONTINUE
  300 CONTINUE
C----------------------------------------------------------------
C***
C***  CALCULATE THE SEA LEVEL PRESSURE AS PER THE NEW SCHEME.
C***  INTEGRATE THE HYDROSTATIC EQUATION DOWNWARD FROM THE
C***  GROUND THROUGH EACH OUTPUT PRESSURE LEVEL (WHERE TV
C***  IS NOW KNOWN) TO FIND GZ AT THE NEXT MIDPOINT BETWEEN
C***  PRESSURE LEVELS.  WHEN GZ=0 IS REACHED, SOLVE FOR THE
C***  PRESSURE.
C***
C
C***  COUNT THE POINTS WHERE SLP IS DONE BELOW EACH OUTPUT LEVEL
C
      KOUNT=0
      DO J=1,JM
      DO I=1,IM
        P1(I,J)=SPL(LMH(I,J))
        DONE(I,J)=.FALSE.
        IF(FIS(I,J).LT.10.)THEN
          PSLP(I,J)=PD(I,J)+PT
          DONE(I,J)=.TRUE.
          KOUNT=KOUNT+1
        ENDIF
      ENDDO
      ENDDO
C
      KMM=KMNTM(LSM)
!$omp parallel do private(gz1,gz2,i,j,lmap1,p1,p2),shared(pslp)
      DO 320 KM=1,KMM
      I=IMNT(KM,LSM)
      J=JMNT(KM,LSM)
      LMHIJ=LMH(I,J)
      GZ1=FI(I,J,LMHIJ)
      P1(I,J)=SPL(LMHIJ)
C
      LMAP1=LMHIJ+1
      DO L=LMAP1,LSM
        P2=SPL(L)
        TLYR=0.5*(T(I,J,L)+T(I,J,L-1))
        GZ2=GZ1+RD*TLYR*ALOG(P1(I,J)/P2)
        FI(I,J,L)=GZ2
        IF(GZ2.LE.0.)THEN
          PSLP(I,J)=P1(I,J)/EXP(-GZ1/(RD*T(I,J,L-1)))
          DONE(I,J)=.TRUE.
          KOUNT=KOUNT+1
          GO TO 320
        ENDIF
        P1(I,J)=P2
        GZ1=GZ2
      ENDDO
  320 CONTINUE
C
C***  WHEN SEA LEVEL IS BELOW THE LOWEST OUTPUT PRESSURE LEVEL,
C***  SOLVE THE HYDROSTATIC EQUATION BY CHOOSING A TEMPERATURE
C***  AT THE MIDPOINT OF THE LAYER BETWEEN THAT LOWEST PRESSURE
C***  LEVEL AND THE GROUND BY EXTRAPOLATING DOWNWARD FROM T ON
C***  THE LOWEST PRESSURE LEVEL USING THE DT/DFI BETWEEN THE
C***  LOWEST PRESSURE LEVEL AND THE ONE ABOVE IT.
C
      TOTAL=(IM-2)*(JM-4)
C
      DO 340 LP=LSM,1,-1
      IF(KOUNT.EQ.TOTAL)GO TO 350
      DO 330 J=1,JM
      DO 330 I=1,IM
      IF(FI(I,J,LP).LT.0..OR.DONE(I,J))GO TO 330
      SLOPE=(T(I,J,LP)-T(I,J,LP-1))/(FI(I,J,LP)-FI(I,J,LP-1))
c     SLOPE=-6.6E-4
      TLYR=T(I,J,LP)-0.5*FI(I,J,LP)*SLOPE
      PSLP(I,J)=P1(I,J)/EXP(-FI(I,J,LP)/(RD*TLYR))
      DONE(I,J)=.TRUE.
      KOUNT=KOUNT+1
  330 CONTINUE
  340 CONTINUE
C
  350 CONTINUE
C--------------------------------------------------------------------
C     SKIP THE STANDARD SCHEME.
C--------------------------------------------------------------------
      GO TO 430
C--------------------------------------------------------------------
C***
C***  IF YOU WANT THE "STANDARD" ETA/SIGMA REDUCTION
C***  THIS IS WHERE IT IS DONE.
C***
  400 CONTINUE
C
      DO 410 J=1,JM
      DO 410 I=1,IM
      IF(FIS(I,J).GE.1.)THEN
        LMA=LMH(I,J)
        ALPP1=ALOG(PDSL1(I,J)*ETA(LMA+1)+PT)
        SLOP=0.0065*ROG*T(I,J,LMA)
        IF(SLOP.LT.0.50)THEN
          SLPP=ALPP1+FIS(I,J)/(R*T(I,J,LMA))
        ELSE
          TTT=-(ALOG(PDSL1(I,J)*ETA(LMA)+PT)+ALPP1)
     1         *SLOP*0.50+T(I,J,LMA)
          SLPP=(-TTT+SQRT(TTT*TTT+2.*SLOP*
     1          (FIS(I,J)/R+
     2          (TTT+0.50*SLOP*ALPP1)*ALPP1)))/SLOP
        ENDIF
        PSLP(I,J)=EXP(SLPP)
      ENDIF
  410 CONTINUE
C
C****************************************************************
C     AT THIS POINT WE HAVE A SEA LEVEL PRESSURE FIELD BY
C     EITHER METHOD.  5-POINT AVERAGE THE FIELD ON THE E-GRID.
C****************************************************************
C
  430 CONTINUE
C
C***  EXTRAPOLATE VALUES TO THE OUTER 2 ROWS
C
      DO J=1,2
      IEND=IM-1-MOD(J+1,2)
      DO I=2,IEND
        PSLP(I,J)=1.5*PSLP(I,J+2)-0.5*PSLP(I,J+4)
      ENDDO
      ENDDO
C
      DO J=JM-1,JM
      IEND=IM-1-MOD(J+1,2)
      DO I=2,IEND
        PSLP(I,J)=1.5*PSLP(I,J-2)-0.5*PSLP(I,J-4)
      ENDDO
      ENDDO
C
      DO J=1,JM
        PSLP(1,J)=1.5*PSLP(2,J)-0.5*PSLP(3,J)
      ENDDO
      DO J=1,JM
        I=IM-MOD(J+1,2)
        PSLP(I,J)=1.5*PSLP(I-1,J)-0.5*PSLP(I-2,J)
      ENDDO
C
!$omp parallel do 
      DO 440 J=1,JM
      DO 440 I=1,IM
      SLPX(I,J)=PSLP(I,J)
  440 CONTINUE
C
      DO 480 KS=1,KSLPD
C
!$omp parallel do private(ihh2)
      DO 460 J=3,JM2
      IHH2=IM-1-MOD(J+1,2)
      DO 460 I=2,IHH2
C
C***  EXTRA AVERAGING UNDER MOUNTAINS TAKEN OUT, FM, MARCH 96
C
      SLPX(I,J)=0.125*(PSLP(I+IHW(J),J-1)+PSLP(I+IHE(J),J-1)
     1                +PSLP(I+IHW(J),J+1)+PSLP(I+IHE(J),J+1)
     2                +4.*PSLP(I,J))
  460 CONTINUE
C
!$omp parallel do
      DO J=1,JM
      DO I=1,IM
        PSLP(I,J)=SLPX(I,J)
      ENDDO
      ENDDO
C
  480 CONTINUE
C
      DO L=LHMNT,LSM
        DO KM=1,KMM
          I=IMNT(KM,L)
          J=JMNT(KM,L)
          T(I,J,L)=T(I,J,L)/(1.+0.608*Q(I,J,L))
        ENDDO
      ENDDO
C----------------------------------------------------------------
      RETURN
      END
