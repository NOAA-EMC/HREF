       SUBROUTINE CALWXT(T,Q,PMID,PINT,HTM,LMH,PREC,ZINT,IWX
     +,ZWET)
C 
C     FILE: CALWXT.f
C     WRITTEN: 11 NOVEMBER 1993, MICHAEL BALDWIN
C     REVISIONS:
C               30 SEPT 1994-SETUP NEW DECISION TREE (M BALDWIN)
C               12 JUNE 1998-CONVERSION TO 2-D (T BLACK)
C     01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C     02-01-15  MIKE BALDWIN - WRF VERSION
C     05-07-07  BINBIN ZHOU  - ADD PREC FOR RSM
C                              
C
C     ROUTINE TO COMPUTE PRECIPITATION TYPE USING A DECISION TREE
C     APPROACH THAT USES VARIABLES SUCH AS INTEGRATED WET BULB TEMP
C     BELOW FREEZING AND LOWEST LAYER TEMPERATURE
C
C     SEE BALDWIN AND CONTORNO PREPRINT FROM 13TH WEATHER ANALYSIS
C     AND FORECASTING CONFERENCE FOR MORE DETAILS
C     (OR BALDWIN ET AL, 10TH NWP CONFERENCE PREPRINT)
C 
!      INCLUDE "parmeta"
      INCLUDE "params"
C
CHC      PARAMETER (LP1=LM+1)
CHC      PARAMETER (H1M12=1.E-12)
      PARAMETER (PTHRESH=0.000004)
      INCLUDE "CTLBLK.comm"
C
C  LIST OF VARIABLES NEEDED
C    PARAMETERS:
C      D608,ROG,H1,D00
CHC       PARAMETER(D608=0.608,ROG=287.04/9.8,H1=1.0,D00=0.0)
C
C    INPUT:
C      T,Q,PMID,HTM,LMH,PREC,ZINT
      REAL T(IM,jsta_2l:jend_2u,LM),Q(IM,jsta_2l:jend_2u,LM)
     &,      PMID(IM,jsta_2l:jend_2u,LM),ZINT(IM,jsta_2l:jend_2u,LP1)
     &,      HTM(IM,jsta_2l:jend_2u,LM),PINT(IM,jsta_2l:jend_2u,LP1)
      REAL LMH(IM,jsta_2l:jend_2u),PREC(IM,jsta_2l:jend_2u)
C    OUTPUT:
C      IWX - INSTANTANEOUS WEATHER TYPE.
C        ACTS LIKE A 4 BIT BINARY
C          1111 = RAIN/FREEZING RAIN/ICE PELLETS/SNOW
C          WHERE THE ONE'S DIGIT IS FOR SNOW
C                THE TWO'S DIGIT IS FOR ICE PELLETS
C                THE FOUR'S DIGIT IS FOR FREEZING RAIN
C            AND THE EIGHT'S DIGIT IS FOR RAIN
      DIMENSION IWX(IM,JM),ZWET(IM,JM)
C    INTERNAL:
C
      REAL, ALLOCATABLE :: TWET(:,:,:)
      DIMENSION KARR(IM,JM)
      DIMENSION LICEE(IM,JM),TCOLD(IM,JM),TWARM(IM,JM)
C    SUBROUTINES CALLED:
C     WETBULB
C     
C
C     INITIALIZE WEATHER TYPE ARRAY TO ZERO (IE, OFF).
C     WE DO THIS SINCE WE WANT IWX TO REPRESENT THE
C     INSTANTANEOUS WEATHER TYPE ON RETURN.
C     
C
C     ALLOCATE LOCAL STORAGE
C
      ALLOCATE ( TWET(IM,JSTA_2L:JEND_2U,LM) )
C
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        IWX(I,J) = 0
        ZWET(I,J)=SPVAL
        if (I .eq. 324 .and. J .eq. 390) then
        LMHK=NINT(LMH(I,J))
         DO L=LMHK,1,-1
!           print *, 'tprof ', L, T(I,J,L)
         ENDDO
        endif
      ENDDO
      ENDDO

      IF(MODELNAME.eq.'RSM') THEN          !add by Binbin because of different unit
       DO J=JSTA,JEND
       DO I=1,IM
        PREC(I,J) = PREC(I,J)*3*3600.0
       ENDDO
       ENDDO
      END IF


C
!$omp  parallel do
!$omp& private(a,lmhk,pkl,psfck,qkl,tdchk,tdkl,tdpre,tkl)
      DO 800 J=JSTA,JEND
      DO 800 I=1,IM
      LMHK=NINT(LMH(I,J))
C
C   SKIP THIS POINT IF NO PRECIP THIS TIME STEP 
C
      IF (PREC(I,J).LE.PTHRESH) GOTO 800
C
C   FIND COLDEST AND WARMEST TEMPS IN SATURATED LAYER BETWEEN
C   70 MB ABOVE GROUND AND 500 MB
C   ALSO FIND HIGHEST SATURATED LAYER IN THAT RANGE
C
!meb
      PSFCK=PINT(I,J,LMHK+1)
!meb
      TDCHK=2.0
  760 TCOLD(I,J)=T(I,J,LMHK)
      TWARM(I,J)=T(I,J,LMHK)
      LICEE(I,J)=LMHK
C
      DO 775 L=1,LMHK
      QKL=Q(I,J,L)
      QKL=AMAX1(H1M12,QKL)
      TKL=T(I,J,L)
      PKL=PMID(I,J,L)
C
C   SKIP PAST THIS IF THE LAYER IS NOT BETWEEN 70 MB ABOVE GROUND
C       AND 500 MB
C
      IF (PKL.LT.50000.0.OR.PKL.GT.PSFCK-7000.0) GOTO 775
      A=ALOG(QKL*PKL/(6.1078*(0.378*QKL+0.622)))
      TDKL=(237.3*A)/(17.269-A)+273.15
      TDPRE=TKL-TDKL
      IF (TDPRE.LT.TDCHK.AND.TKL.LT.TCOLD(I,J)) TCOLD(I,J)=TKL
      IF (TDPRE.LT.TDCHK.AND.TKL.GT.TWARM(I,J)) TWARM(I,J)=TKL
      IF (TDPRE.LT.TDCHK.AND.L.LT.LICEE(I,J)) LICEE(I,J)=L
  775 CONTINUE
C
C    IF NO SAT LAYER AT DEW POINT DEP=TDCHK, INCREASE TDCHK
C     AND START AGAIN (BUT DON'T MAKE TDCHK > 6)
C
      IF (TCOLD(I,J).EQ.T(I,J,LMHK).AND.TDCHK.LT.6.0) THEN
        TDCHK=TDCHK+2.0
        GOTO 760
      ENDIF
  800 CONTINUE
C
C    LOWEST LAYER T
C
      DO 850 J=JSTA,JEND
      DO 850 I=1,IM
      KARR(I,J)=0
      IF (PREC(I,J).LE.PTHRESH) GOTO 850
      LMHK=NINT(LMH(I,J))
      TLMHK=T(I,J,LMHK)
C
C    DECISION TREE TIME
C
      IF (TCOLD(I,J).GT.269.15) THEN
          IF (TLMHK.LE.273.15) THEN
C             TURN ON THE FLAG FOR
C             FREEZING RAIN = 4
C             IF ITS NOT ON ALREADY
C             IZR=MOD(IWX(I,J),8)/4
C             IF (IZR.LT.1) IWX(I,J)=IWX(I,J)+4
              IWX(I,J)=IWX(I,J)+4
            GOTO 850
          ELSE
C             TURN ON THE FLAG FOR
C             RAIN = 8
C             IF ITS NOT ON ALREADY
C             IRAIN=IWX(I,J)/8
C             IF (IRAIN.LT.1) IWX(I,J)=IWX(I,J)+8
              IWX(I,J)=IWX(I,J)+8
            GOTO 850
          ENDIF
      ENDIF
      KARR(I,J)=1
  850 CONTINUE
C
C   COMPUTE WET BULB ONLY AT POINTS THAT NEED IT
C
      CALL WETBULB(T,Q,PMID,HTM,KARR,TWET)
      CALL WETFRZLVL(TWET,ZWET)
C
!$omp  parallel do
!$omp& private(area1,areap4,areas8,dzkl,ifrzl,iwrml,lice,
!$omp&         lmhk,pintk1,pintk2,pm150,psfck,surfc,surfw,
!$omp&         tlmhk,twrmk)
      DO 1900 J=JSTA,JEND
      DO 1900 I=1,IM
       IF (I .EQ. 324 .AND. J .EQ. 390) THEN
          LMHK=NINT(LMH(I,J))
          DO L=LMHK,1,-1          
           print *, 'TW NCEP ', TWET(I,J,L)
          ENDDO
       ENDIF
      IF(KARR(I,J).GT.0)THEN
        LMHK=NINT(LMH(I,J))
        LICE=LICEE(I,J)
!meb
        PSFCK=PINT(I,J,LMHK+1)
!meb
        TLMHK=T(I,J,LMHK)
        TWRMK=TWARM(I,J)
C
C    TWET AREA VARIABLES
C     CALCULATE ONLY WHAT IS NEEDED
C      FROM GROUND TO 150 MB ABOVE SURFACE
C      FROM GROUND TO TCOLD LAYER
C      AND FROM GROUND TO 1ST LAYER WHERE WET BULB T < 0.0
C
C     PINTK1 IS THE PRESSURE AT THE BOTTOM OF THE LAYER
C     PINTK2 IS THE PRESSURE AT THE TOP OF THE LAYER
C
C     AREAP4 IS THE AREA OF TWET ABOVE -4 C BELOW HIGHEST SAT LYR 
C
        AREAS8=D00
        AREAP4=D00
        SURFW =D00
        SURFC =D00
C
        DO 1945 L=LMHK,LICE,-1
        DZKL=ZINT(I,J,L)-ZINT(I,J,L+1)
        AREA1=(TWET(I,J,L)-269.15)*DZKL
        IF (TWET(I,J,L).GE.269.15) AREAP4=AREAP4+AREA1
 1945   CONTINUE
C
        IF (AREAP4.LT.3000.0) THEN
C             TURN ON THE FLAG FOR
C             SNOW = 1
C             IF ITS NOT ON ALREADY
C             ISNO=MOD(IWX(I,J),2)
C             IF (ISNO.LT.1) IWX(I,J)=IWX(I,J)+1
          IWX(I,J)=IWX(I,J)+1
          GO TO 1900
        ENDIF
C
C     AREAS8 IS THE NET AREA OF TWET W.R.T. FREEZING IN LOWEST 150MB
C
        PINTK1=PSFCK
        PM150=PSFCK-15000.
C
        DO 1955 L=LMHK,1,-1
        PINTK2=PINT(I,J,L)
        IF(PINTK1.LT.PM150)GO TO 1950
        DZKL=ZINT(I,J,L)-ZINT(I,J,L+1)
C
C    SUM PARTIAL LAYER IF IN 150 MB AGL LAYER
C
        IF(PINTK2.LT.PM150) 
     &    DZKL=T(I,J,L)*(Q(I,J,L)*D608+H1)*ROG*
     1         ALOG(PINTK1/PM150)
        AREA1=(TWET(I,J,L)-273.15)*DZKL
        AREAS8=AREAS8+AREA1
 1950   PINTK1=PINTK2
 1955   CONTINUE
C
C     SURFW IS THE AREA OF TWET ABOVE FREEZING BETWEEN THE GROUND
C       AND THE FIRST LAYER ABOVE GROUND BELOW FREEZING
C     SURFC IS THE AREA OF TWET BELOW FREEZING BETWEEN THE GROUND
C       AND THE WARMEST SAT LAYER
C
        IFRZL=0
        IWRML=0
C
        DO 2050 L=LMHK,1,-1
        IF (IFRZL.EQ.0.AND.T(I,J,L).LT.273.15) IFRZL=1
        IF (IWRML.EQ.0.AND.T(I,J,L).GE.TWRMK) IWRML=1
C
        IF (IWRML.EQ.0.OR.IFRZL.EQ.0) THEN
          DZKL=ZINT(I,J,L)-ZINT(I,J,L+1)
          AREA1=(TWET(I,J,L)-273.15)*DZKL
          IF(IFRZL.EQ.0.AND.TWET(I,J,L).GE.273.15)SURFW=SURFW+AREA1
          IF(IWRML.EQ.0.AND.TWET(I,J,L).LE.273.15)SURFC=SURFC+AREA1
        ENDIF
 2050   CONTINUE
        IF(SURFC.LT.-3000.0.OR.
     &    (AREAS8.LT.-3000.0.AND.SURFW.LT.50.0)) THEN
C             TURN ON THE FLAG FOR
C             ICE PELLETS = 2
C             IF ITS NOT ON ALREADY
C             IIP=MOD(IWX(I,J),4)/2
C             IF (IIP.LT.1) IWX(I,J)=IWX(I,J)+2
          IWX(I,J)=IWX(I,J)+2
          GOTO 1900
        ENDIF
C
        IF(TLMHK.LT.273.15) THEN
C             TURN ON THE FLAG FOR
C             FREEZING RAIN = 4
C             IF ITS NOT ON ALREADY
C             IZR=MOD(IWX(K),8)/4
C             IF (IZR.LT.1) IWX(K)=IWX(K)+4
          IWX(I,J)=IWX(I,J)+4
        ELSE
C             TURN ON THE FLAG FOR
C             RAIN = 8
C             IF ITS NOT ON ALREADY
C             IRAIN=IWX(K)/8
C             IF (IRAIN.LT.1) IWX(K)=IWX(K)+8
          IWX(I,J)=IWX(I,J)+8
        ENDIF
      ENDIF
 1900 CONTINUE
C---------------------------------------------------------
      DEALLOCATE (TWET)

      IF(MODELNAME.eq.'RSM') THEN    !add by Binbin, change back
       DO J=JSTA,JEND
       DO I=1,IM
        PREC(I,J) = PREC(I,J)/(3*3600.0)
       ENDDO
       ENDDO
      END IF


      RETURN
      END
