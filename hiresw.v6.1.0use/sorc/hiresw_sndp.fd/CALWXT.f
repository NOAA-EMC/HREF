       SUBROUTINE CALWXT(T,Q,P,PINT,LMH,LM,PPT,IWX)
C 
C     FILE: CALWXT.f
C     WRITTEN: 11 NOVEMBER 1993, MICHAEL BALDWIN
C     REVISIONS: 4 April 94 - 1-d version intended for obs soundings
C               16 Sept  94 - compute all variables for possible
C                             future decsion tree modifications
C               14 Oct   94 - clean up 1-d version, use new 
C                             decision tree
C               31 July  95 - sounding post-processor version
c               06 Dec   97 - changed threshold value - GSM
C
C     ROUTINE TO COMPUTE PRECIPITATION TYPE USING A DECISION TREE
C     APPROACH THAT USES VARIABLES SUCH AS INTEGRATED WET BULB TEMP
C     BELOW FREEZING AND LOWEST LAYER TEMPERATURE
C
C     SEE BALDWIN AND CONTORNO PREPRINT FROM 13TH WEATHER ANALYSIS
C     AND FORECASTING CONFERENCE FOR MORE DETAILS
C 
      PARAMETER (H1M12=1.E-12)
C
C  LIST OF VARIABLES NEEDED
C    PARAMETERS:
C      D608,ROG,H1,D00
       PARAMETER(D608=0.608,ROG=287.04/9.8,H1=1.0,D00=0.0)

CGSM  PTHRES changed from 0.0 to 0.02  2/18/97

       PARAMETER(PTHRES=0.02)
C
C    INPUT:
C      T,Q,P,PINT,LMH
C
C      T  - Mid layer temp (K)
C      Q  - Mid layer spec hum (g/g)
C      P  - Mid layer pressure (Pa) (linear average of interfacial 
C               pressures in log P)
C      PINT - Interfacial pressure (Pa)
C      LMH - Number of layers above ground
C      LM  - Number of layers
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C  NOTE: VERTICAL ORDER OF ARRAYS MUST BE LAYER 1 = TOP
C                                 ----          .
C                                               .
C                                               .
C                                         LAYER LMH = BOTTOM
C          (JUST LIKE IN THE ETA MODEL)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C    INTERNAL:
C      TWET - Mid layer wet bulb temp (K)
C      TD - Mid layer dew point temp (K)
C
C
C    OUTPUT:
C      IWX - INSTANTANEOUS WEATHER TYPE.
C        ACTS LIKE A 4 BIT BINARY
C          1111 = RAIN/FREEZING RAIN/ICE PELLETS/SNOW
C          WHERE THE ONE'S DIGIT IS FOR SNOW
C                THE TWO'S DIGIT IS FOR ICE PELLETS
C                THE FOUR'S DIGIT IS FOR FREEZING RAIN
C            AND THE EIGHT'S DIGIT IS FOR RAIN
C
C-------------------------------------------------------------
C          IN OTHER WORDS...
C
C          IWX=1 SNOW
C          IWX=2 ICE PELLETS/MIX WITH ICE PELLETS
C          IWX=4 FREEZING RAIN/MIX WITH FREEZING RAIN
C          IWX=8 RAIN
C-------------------------------------------------------------
C
C
C    SUBROUTINES CALLED:
C     WETBLB
C     
C
C     INITIALIZE WEATHER TYPE ARRAY TO ZERO (IE, OFF).
C     WE DO THIS SINCE WE WANT IWX TO REPRESENT THE
C     INSTANTANEOUS WEATHER TYPE ON RETURN.
C     
      REAL T(LM),Q(LM),P(LM),PINT(LM+1),TWET(LM),TD(LM)
      REAL PPT,AREAS8,AREAN8,AREAPI,AREAP4,SURFW,SURFC
      INTEGER LMHK,LMH
C
      IWX = 0
      IF (PPT.LE.PTHRES) RETURN

      AREAS8=D00
      AREAN8=D00
      AREAPI=D00
      AREAP4=D00
      SURFW =D00
      SURFC =D00
C
C   NUMBER OF LEVELS
C
      LMHK=LMH
C
C   COMPUTE DEW POINTS,
C   FIND COLDEST TEMP IN SATURATED LAYER BETWEEN
C   70 MB ABOVE GROUND AND 500 MB,
C   AND FIND THE HIGHEST SAT LAYER, 'TIS THE ICE NUCL LEVEL.
C
C
      PSFCK=PINT(LMHK+1)
      TDCHK=2.0
 1960 TCOLD=T(LMHK)
      TWARM=T(LMHK)
      LICE=LMHK
      DO 1915 L=1,LMHK
       QKL=Q(L)
       QKL=AMAX1(H1M12,QKL)
       TKL=T(L)
       PKL=P(L)
       IF (Q(L).LT.3E-12) THEN
         TD(L)=TKL-40.0
       ELSE
         A=ALOG(QKL*PKL/(610.78*(0.378*QKL+0.622)))
         TDKL=(237.3*A)/(17.269-A)+273.15
         TD(L)=TDKL
         IF (TD(L).GT.TKL) TD(L)=TKL
       ENDIF
C
C   SKIP PAST THIS IF THE LAYER IS NOT BETWEEN 70 MB ABOVE GROUND
C       AND 500 MB
C
       IF (PKL.LT.50000.0.OR.PKL.GT.PSFCK-7000.0) GOTO 1915
       TDPRE=TKL-TDKL
C
C   ALSO FIND THE HIGHEST SAT LAYER-USE FOR AREAPI,AREAP4
C
       IF (TDPRE.LT.TDCHK.AND.P(L).LT.P(LICE)) LICE=L
       IF (TDPRE.LT.TDCHK.AND.TKL.GT.TWARM) TWARM=TKL
       IF (TDPRE.LT.TDCHK.AND.TKL.LT.TCOLD) TCOLD=TKL
 1915 CONTINUE
C
C    IF WE DONT HAVE A LAYER WITH DEW POINT DEP OF TDCHK OR LESS
C      INCREASE TDCHK (UP TO 6 MAX)
C
      IF (TCOLD.EQ.T(LMHK).AND.TDCHK.LT.6.0) THEN
        TDCHK=TDCHK+2.0
        GOTO 1960
      ENDIF
C 
C   GET WET BULB TEMPS
C
      CALL WETBLB(T,TD,P,LMHK,TWET)
C
C    LOWEST LAYER T
C
      TLMHK=T(LMHK)
C
C    TWET AREA VARIABLES
C      FROM GROUND TO 150 MB ABOVE SURFACE
C      FROM GROUND TO TCOLD LAYER
C      FROM GROUND TO 1ST LAYER WHERE T < 0.0
C      FROM GROUND TO TWARM LAYER
C
C     PINTK1 IS THE PRESSURE AT THE BOTTOM OF THE LAYER
C     PINTK2 IS THE PRESSURE AT THE TOP OF THE LAYER
C
C     AREAPI IS THE AREA OF TWET ABOVE FREEZING BELOW TCOLD LYR 
C     AREAP4 IS THE AREA OF TWET ABOVE -4 C     BELOW TCOLD LYR 
C
      PINTK1=PSFCK
      DO 1945 L=LMHK,LICE,-1
       PINTK2=PINT(L)
       DZKL=T(L)*(Q(L)*D608+H1)*ROG*
     1        ALOG(PINTK1/PINTK2)
       AREA1=(TWET(L)-273.15)*DZKL
       AREA2=(TWET(L)-269.15)*DZKL
       IF (TWET(L).GE.273.15) AREAPI=AREAPI+AREA1
       IF (TWET(L).GE.269.15) AREAP4=AREAP4+AREA2
       PINTK1=PINTK2
 1945 CONTINUE
C
C     AREAS8 IS THE NET AREA OF TWET W.R.T. FREEZING IN LOWEST 150MB
C     AREAN8 IS THE NET AREA OF TWET < FREEZING IN LOWEST 150MB
C
      PINTK1=PSFCK
      PM150=PSFCK-15000.
      DO 1955 L=LMHK,1,-1
       PINTK2=PINT(L)
       IF (PINTK1.LT.PM150) GOTO 1950
       DZKL=T(L)*(Q(L)*D608+H1)*ROG*
     1        ALOG(PINTK1/PINTK2)
C
C    SUM PARTIAL LAYER IF IN 150 MB AGL LAYER
C
       IF (PINTK2.LT.PM150)
     &  DZKL=T(L)*(Q(L)*D608+H1)*ROG*
     1         ALOG(PINTK1/PM150)
       AREA1=(TWET(L)-273.15)*DZKL
       AREAS8=AREAS8+AREA1
       IF(AREA1.LT.0.) AREAN8=AREAN8+AREA1
 1950  PINTK1=PINTK2
 1955 CONTINUE
C
C     SURFW IS THE AREA OF TWET ABOVE FREEZING BETWEEN THE GROUND
C       AND THE FIRST LAYER ABOVE GROUND BELOW FREEZING
C     SURFC IS THE AREA OF TWET BELOW FREEZING BETWEEN THE GROUND
C       AND THE TWARM LAYER
C
      PINTK1=PSFCK
      IFRZL=0
      IWRML=0
      DO 2050 L=LMHK,1,-1
       IF (IFRZL.EQ.0.AND.T(L).LE.273.15) IFRZL=1
       IF (IWRML.EQ.0.AND.T(L).GE.TWARM) IWRML=1
        PINTK2=PINT(L)
        DZKL=T(L)*(Q(L)*D608+H1)*ROG*
     1        ALOG(PINTK1/PINTK2)
        AREA1=(TWET(L)-273.15)*DZKL
       IF (IFRZL.EQ.0) THEN
        IF (TWET(L).GE.273.15) SURFW=SURFW+AREA1
       ENDIF
       IF (IWRML.EQ.0) THEN
        IF (TWET(L).LE.273.15) SURFC=SURFC+AREA1
       ENDIF
        PINTK1=PINTK2
 2050 CONTINUE
C
C    DECISION TREE TIME
C
       IF (TCOLD.GT.269.15) THEN
          IF (TLMHK.LE.273.15) THEN
C             TURN ON THE FLAG FOR
C             FREEZING RAIN = 4
              IWX=4
            GOTO 1900
          ELSE
C             TURN ON THE FLAG FOR
C             RAIN = 8
              IWX=8
            GOTO 1900
          ENDIF
       ENDIF
C
       IF (AREAP4.LT.3000.0) THEN
C             TURN ON THE FLAG FOR
C             SNOW = 1
              IWX=1
            GOTO 1900
       ENDIF
C
       IF (SURFC.LE.-3000.0.OR.
     &     (AREAS8.LE.-3000.0.AND.SURFW.LT.50.0)) THEN
C             TURN ON THE FLAG FOR
C             ICE PELLETS = 2
              IWX=2
            GOTO 1900
       ENDIF
      IF (TLMHK.LT.273.15) THEN
C             TURN ON THE FLAG FOR
C             FREEZING RAIN = 4
              IWX=4
      ELSE
C             TURN ON THE FLAG FOR
C             RAIN = 8
              IWX=8
      ENDIF
 1900 CONTINUE
      RETURN
      END
