      SUBROUTINE MDL2P
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    MDL2P       VERT INTRP OF MODEL LVLS TO PRESSURE
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-09-23       
C     
C ABSTRACT:
C     FOR MOST APPLICATIONS THIS ROUTINE IS THE WORKHORSE
C     OF THE POST PROCESSOR.  IN A NUTSHELL IT INTERPOLATES
C     DATA FROM MODEL TO PRESSURE SURFACES.  IT ORIGINATED
C     FROM THE VERTICAL INTERPOLATION CODE IN THE OLD ETA
C     POST PROCESSOR SUBROUTINE OUTMAP AND IS A REVISION
C     OF SUBROUTINE ETA2P.
C   .     
C     
C PROGRAM HISTORY LOG:
C   99-09-23  T BLACK       - REWRITTEN FROM ETA2P
C   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-06-12  MIKE BALDWIN - WRF VERSION
C   02-07-29  H CHUANG - ADD UNDERGROUND FIELDS AND MEMBRANE SLP FOR WRF
C   04-11-24  H CHUANG - ADD FERRIER'S HYDROMETEOR FIELD
C   05-07-07  B ZHOU   - ADD RSM MODEL for SLP  
C   05--8-30  B ZHOU   - ADD AVIATION PRODUCTS: ICING, CAT, LLWS COMPUTATION
C
C USAGE:    CALL MDL2P
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST: 
C     NONE       
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       SCLFLD   - SCALE ARRAY ELEMENTS BY CONSTANT.
C       CALPOT   - COMPUTE POTENTIAL TEMPERATURE.
C       CALRH    - COMPUTE RELATIVE HUMIDITY.
C       CALDWP   - COMPUTE DEWPOINT TEMPERATURE.
C       BOUND    - BOUND ARRAY ELEMENTS BETWEEN LOWER AND UPPER LIMITS.
C       CALMCVG  - COMPUTE MOISTURE CONVERGENCE.
C       CALVOR   - COMPUTE ABSOLUTE VORTICITY.
C       CALSTRM  - COMPUTE GEOSTROPHIC STREAMFUNCTION.
C
C     LIBRARY:
C       COMMON   - CTLBLK
C                  RQSTFLD
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$  
C
C
      use vrbls3d
      use vrbls2d
      use masks
C     
C     INCLUDE MODEL DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
C     GAMMA AND RGAMOG ARE USED IN THE EXTRAPOLATION OF VIRTUAL
C     TEMPERATURES BEYOND THE UPPER OF LOWER LIMITS OF DATA.
C     
!      INCLUDE "parmeta"
!      INCLUDE "parmout"
      INCLUDE "params"
      INCLUDE "CTLBLK.comm"
      INCLUDE "RQSTFLD.comm"

C
!      PARAMETER (IM_JM=IM*JM,LMP1=LM+1)

      PARAMETER (GAMMA=6.5E-3,RGAMOG=RD*GAMMA/G)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESTRT,SIGMA
      LOGICAL IOOMG,IOALL
      REAL FSL(IM,JM),TSL(IM,JM),QSL(IM,JM)
      REAL OSL(IM,JM),USL(IM,JM),VSL(IM,JM)
      REAL Q2SL(IM,JM),WSL(IM,JM)
      REAL EGRID1(IM,JM),EGRID2(IM,JM)
      REAL GRID1(IM,JM),GRID2(IM,JM)
      REAL FSL_OLD(IM,JM),USL_OLD(IM,JM),VSL_OLD(IM,JM)
C
      INTEGER NL1X(IM,JM),NL1XF(IM,JM)
      REAL TPRS(IM,JSTA_2L:JEND_2U,LSM)
     + ,QPRS(IM,JSTA_2L:JEND_2U,LSM),FPRS(IM,JSTA_2L:JEND_2U,LSM)
C
!
!--- Definition of the following 2D (horizontal) dummy variables
!
!  C1D   - total condensate
!  QW1   - cloud water mixing ratio
!  QI1   - cloud ice mixing ratio
!  QR1   - rain mixing ratio
!  QS1   - snow mixing ratio
!  DBZ1  - radar reflectivity
!
      REAL C1D(IM,JM),QW1(IM,JM),QI1(IM,JM),QR1(IM,JM)
     &,    QS1(IM,JM) ,DBZ1(IM,JM),FRIME(IM,JM),RAD(IM,JM)
 
!  SAVE RH, U,V, for Icing, CAT, LLWS computation
      REAL SAVRH(IM,JM)


C
C     INCLUDE COMMON BLOCKS.
C
C      COMMON/JIMA/NL1X(IM,JM),ALPETUX(IM,JM),ALPET2X(IM,JM)
C
C     
C******************************************************************************
C
C     START MDL2P. 
C     
C     SET TOTAL NUMBER OF POINTS ON OUTPUT GRID.
C
C---------------------------------------------------------------
C
C     *** PART I ***
C
C     VERTICAL INTERPOLATION OF EVERYTHING ELSE.  EXECUTE ONLY
C     IF THERE'S SOMETHING WE WANT.
C
      IF((IGET(012).GT.0).OR.(IGET(013).GT.0).OR.
     X   (IGET(014).GT.0).OR.(IGET(015).GT.0).OR.
     X   (IGET(016).GT.0).OR.(IGET(017).GT.0).OR.
     X   (IGET(018).GT.0).OR.(IGET(019).GT.0).OR.
     X   (IGET(020).GT.0).OR.(IGET(030).GT.0).OR.
     X   (IGET(021).GT.0).OR.(IGET(022).GT.0).OR.
     X   (IGET(023).GT.0).OR.(IGET(085).GT.0).OR.
     X   (IGET(086).GT.0).OR.(IGET(284).GT.0).OR.
     X   (IGET(153).GT.0).OR.(IGET(166).GT.0).OR.
     X   (IGET(183).GT.0).OR.(IGET(184).GT.0).OR.
     X   (IGET(198).GT.0).OR.(IGET(251).GT.0).OR. 
     X   (IGET(257).GT.0).OR.(IGET(258).GT.0).OR.
     X   (IGET(294).GT.0))THEN
C
C---------------------------------------------------------------------
C***
C***  BECAUSE SIGMA LAYERS DO NOT GO UNDERGROUND,  DO ALL
C***  INTERPOLATION ABOVE GROUND NOW.
C***
C
        print*,'LSM= ',lsm
        DO 310 LP=1,LSM
C
        DO J=JSTA_2L,JEND_2U
        DO I=1,IM

C
        TSL(I,J)=SPVAL
        QSL(I,J)=SPVAL
        FSL(I,J)=SPVAL
        OSL(I,J)=SPVAL
        USL(I,J)=SPVAL
        VSL(I,J)=SPVAL
        Q2SL(I,J)=SPVAL
        C1D(I,J)=SPVAL      ! Total condensate
        QW1(I,J)=SPVAL      ! Cloud water
        QI1(I,J)=SPVAL      ! Cloud ice
        QR1(I,J)=SPVAL      ! Rain 
        QS1(I,J)=SPVAL      ! Snow (precip ice) 
	DBZ1(I,J)=SPVAL
	FRIME(I,J)=SPVAL
	RAD(I,J)=SPVAL
C
C***  LOCATE VERTICAL INDEX OF MODEL MIDLAYER JUST BELOW
C***  THE PRESSURE LEVEL TO WHICH WE ARE INTERPOLATING.
C
        NL1X(I,J)=LP1
        DO L=2,LM
        IF(NL1X(I,J).EQ.LP1.AND.PMID(I,J,L).GT.SPL(LP))THEN
          NL1X(I,J)=L
        ENDIF
        ENDDO
C
C  IF THE PRESSURE LEVEL IS BELOW THE LOWEST MODEL MIDLAYER
C  BUT STILL ABOVE THE LOWEST MODEL BOTTOM INTERFACE,
C  WE WILL NOT CONSIDER IT UNDERGROUND AND THE INTERPOLATION
C  WILL EXTRAPOLATE TO THAT POINT
C

	IF (I .eq. 1 .and. J .eq. 1) then
	DO L=1,LP1
	write(6,*) 'L, PINT(1,1,L) in MDL2P.f:: ', L, PINT(1,1,L)
	ENDDO
        ENDIF


        IF(NL1X(I,J).EQ.LP1.AND.PINT(I,J,LP1).GT.SPL(LP))THEN
          NL1X(I,J)=LM
        ENDIF

        NL1XF(I,J)=LP1+1
        DO L=2,LP1
        IF(NL1XF(I,J).EQ.(LP1+1).AND.PINT(I,J,L).GT.SPL(LP))THEN
          NL1XF(I,J)=L
        ENDIF
        ENDDO
C
c        if(NL1X(I,J).EQ.LMP1)print*,'Debug: NL1X=LMP1 AT '
c     1 ,i,j,lp
        ENDDO
        ENDDO
C
!mptest        IF(NHOLD.EQ.0)GO TO 310
C
!$omp  parallel do
!$omp& private(nn,i,j,ll,fact,qsat,rhl)
chc        DO 220 NN=1,NHOLD
chc        I=IHOLD(NN)
chc        J=JHOLD(NN)
c        DO 220 J=JSTA,JEND
        DO 220 J=JSTA_2L,JEND_2U
        DO 220 I=1,IM
C---------------------------------------------------------------------
C***  VERTICAL INTERPOLATION OF GEOPOTENTIAL, TEMPERATURE, SPECIFIC
C***  HUMIDITY, CLOUD WATER/ICE, OMEGA, WINDS, AND TKE.
C---------------------------------------------------------------------
C
        LL=NL1X(I,J)
	LLMH = NINT(LMH(I,J))
CHC        IF(NL1X(I,J).LE.LM)THEN        
	IF(SPL(LP) .LT. PINT(I,J,2))THEN ! Above second interface
	  IF(T(I,J,1).LT.SPVAL)
     &	     TSL(I,J)=T(I,J,1)
          IF(Q(I,J,1).LT.SPVAL)
     &       QSL(I,J)=Q(I,J,1)
	  IF(MODELNAME .EQ. 'NCAR'.OR. MODELNAME .EQ. 'RSM')THEN
	    USL(I,J)=UH(I,J,1)
	    VSL(I,J)=VH(I,J,1)
	  END IF
	  IF(WH(I,J,1).LT.SPVAL)
     &       WSL(I,J)=WH(I,J,1)
          IF(OMGA(I,J,1).LT.SPVAL)
     &	     OSL(I,J)=OMGA(I,J,1) 
          IF(Q2(I,J,1).LT.SPVAL)
     &       Q2SL(I,J)=Q2(I,J,1)
	  IF(CWM(I,J,1).LT.SPVAL)
     &       C1D(I,J)=CWM(I,J,1)
          C1D(I,J)=AMAX1(C1D(I,J),H1M12)      ! Total condensate
	  IF(QQW(I,J,1).LT.SPVAL)
     &       QW1(I,J)=QQW(I,J,1)
          QW1(I,J)=AMAX1(QW1(I,J),H1M12)      ! Cloud water
	  IF(QQI(I,J,1).LT.SPVAL)
     &       QI1(I,J)=QQI(I,J,1)
          QI1(I,J)=AMAX1(QI1(I,J),H1M12)      ! Cloud ice
	  IF(QQR(I,J,1).LT.SPVAL)
     &       QR1(I,J)=QQR(I,J,1)
          QR1(I,J)=AMAX1(QR1(I,J),H1M12)      ! Rain 
	  IF(QQS(I,J,1).LT.SPVAL)
     &       QS1(I,J)=QQS(I,J,1)
          QS1(I,J)=AMAX1(QS1(I,J),H1M12)      ! Snow (precip ice) 
	  IF(DBZ(I,J,1).LT.SPVAL)
     &       DBZ1(I,J)=DBZ(I,J,1)
	  DBZ1(I,J)=AMAX1(DBZ1(I,J),DBZmin)
	  IF(F_RimeF(I,J,1).LT.SPVAL)
     &       FRIME(I,J)=F_RimeF(I,J,1)
          FRIME(I,J)=AMAX1(FRIME(I,J),H1)
	  IF(TTND(I,J,1).LT.SPVAL)
     &       RAD(I,J)=TTND(I,J,1)

        ELSE IF(NL1X(I,J).LE.LLMH)THEN
C
C---------------------------------------------------------------------
C          INTERPOLATE LINEARLY IN LOG(P)
C***  EXTRAPOLATE ABOVE THE TOPMOST MIDLAYER OF THE MODEL
C***  INTERPOLATION BETWEEN NORMAL LOWER AND UPPER BOUNDS
C***  EXTRAPOLATE BELOW LOWEST MODEL MIDLAYER (BUT STILL ABOVE GROUND)
C---------------------------------------------------------------------
C
          FACT=(ALSL(LP)-ALOG(PMID(I,J,LL)))/
     &         (ALOG(PMID(I,J,LL))-ALOG(PMID(I,J,LL-1)))
          IF(T(I,J,LL).LT.SPVAL .AND. T(I,J,LL-1).LT.SPVAL)
     &          TSL(I,J)=T(I,J,LL)+(T(I,J,LL)-T(I,J,LL-1))*FACT
          IF(Q(I,J,LL).LT.SPVAL .AND. Q(I,J,LL-1).LT.SPVAL)
     &          QSL(I,J)=Q(I,J,LL)+(Q(I,J,LL)-Q(I,J,LL-1))*FACT
	  IF(MODELNAME .EQ. 'NCAR'.OR. MODELNAME .EQ. 'RSM')THEN
	   IF(UH(I,J,LL).LT.SPVAL .AND. UH(I,J,LL-1).LT.SPVAL)
     &        USL(I,J)=UH(I,J,LL)+(UH(I,J,LL)-UH(I,J,LL-1))*FACT
           IF(VH(I,J,LL).LT.SPVAL .AND. VH(I,J,LL-1).LT.SPVAL)
     &        VSL(I,J)=VH(I,J,LL)+(VH(I,J,LL)-VH(I,J,LL-1))*FACT
	  END IF 
	  IF(WH(I,J,LL).LT.SPVAL .AND. WH(I,J,LL-1).LT.SPVAL)
     &       WSL(I,J)=WH(I,J,LL)+(WH(I,J,LL)-WH(I,J,LL-1))*FACT
          IF(OMGA(I,J,LL).LT.SPVAL .AND. OMGA(I,J,LL-1).LT.SPVAL)
     &       OSL(I,J)=OMGA(I,J,LL)+(OMGA(I,J,LL)-OMGA(I,J,LL-1))*FACT
          IF(Q2(I,J,LL).LT.SPVAL .AND. Q2(I,J,LL-1).LT.SPVAL)
     &       Q2SL(I,J)=Q2(I,J,LL)+(Q2(I,J,LL)-Q2(I,J,LL-1))*FACT
!          IF(ZMID(I,J,LL).LT.SPVAL .AND. ZMID(I,J,LL-1).LT.SPVAL)
!     &       FSL(I,J)=ZMID(I,J,LL)+(ZMID(I,J,LL)-ZMID(I,J,LL-1))*FACT
!          FSL(I,J)=FSL(I,J)*G
          QSAT=PQ0/SPL(LP)
     1      *EXP(A2*(TSL(I,J)-A3)/(TSL(I,J)-A4))
C
          RHL=QSL(I,J)/QSAT
C
          IF(RHL.GT.1.) QSL(I,J)=QSAT
          IF(RHL.LT.RHmin) QSL(I,J)=RHmin*QSAT
          if(tsl(i,j).gt.335. .or. tsl(i,j).lt.100.)then
           print*,
     +    'bad isobaric T Q',i,j,lp,tsl(i,j),qsl(i,j)
     +,T(I,J,LL),T(I,J,LL-1),Q(I,J,LL),Q(I,J,LL-1)
	write(6,*) 'fact= ', fact
	write(6,*) 'PMID(L),PMID(L-1): ', PMID(I,J,LL),PMID(I,J,LL-1)
	STOP
	    endif
          IF(Q2SL(I,J).LT.0.0) Q2SL(I,J)=0.0
C	  
CHC ADD FERRIER'S HYDROMETEOR
          IF(CWM(I,J,LL).LT.SPVAL .AND. CWM(I,J,LL-1).LT.SPVAL)
     &       C1D(I,J)=CWM(I,J,LL)+(CWM(I,J,LL)-CWM(I,J,LL-1))*FACT
          C1D(I,J)=AMAX1(C1D(I,J),H1M12)      ! Total condensate
	  IF(QQW(I,J,LL).LT.SPVAL .AND. QQW(I,J,LL-1).LT.SPVAL)
     &       QW1(I,J)=QQW(I,J,LL)+(QQW(I,J,LL)-QQW(I,J,LL-1))*FACT
          QW1(I,J)=AMAX1(QW1(I,J),H1M12)      ! Cloud water
	  IF(QQI(I,J,LL).LT.SPVAL .AND. QQI(I,J,LL-1).LT.SPVAL)
     &       QI1(I,J)=QQI(I,J,LL)+(QQI(I,J,LL)-QQI(I,J,LL-1))*FACT
          QI1(I,J)=AMAX1(QI1(I,J),H1M12)      ! Cloud ice
	  IF(QQR(I,J,LL).LT.SPVAL .AND. QQR(I,J,LL-1).LT.SPVAL)
     &       QR1(I,J)=QQR(I,J,LL)+(QQR(I,J,LL)-QQR(I,J,LL-1))*FACT
          QR1(I,J)=AMAX1(QR1(I,J),H1M12)      ! Rain 
	  IF(QQS(I,J,LL).LT.SPVAL .AND. QQS(I,J,LL-1).LT.SPVAL)
     &       QS1(I,J)=QQS(I,J,LL)+(QQS(I,J,LL)-QQS(I,J,LL-1))*FACT
          QS1(I,J)=AMAX1(QS1(I,J),H1M12)      ! Snow (precip ice) 
	  IF(DBZ(I,J,LL).LT.SPVAL .AND. DBZ(I,J,LL-1).LT.SPVAL)
     &       DBZ1(I,J)=DBZ(I,J,LL)+(DBZ(I,J,LL)-DBZ(I,J,LL-1))*FACT
	  DBZ1(I,J)=AMAX1(DBZ1(I,J),DBZmin)
	  IF(F_RimeF(I,J,LL).LT.SPVAL .AND. F_RimeF(I,J,LL-1).LT.SPVAL)
     &       FRIME(I,J)=F_RimeF(I,J,LL)+(F_RimeF(I,J,LL)
     +	            -F_RimeF(I,J,LL-1))*FACT
          FRIME(I,J)=AMAX1(FRIME(I,J),H1)
	  IF(TTND(I,J,LL).LT.SPVAL .AND. TTND(I,J,LL-1).LT.SPVAL)
     &       RAD(I,J)=TTND(I,J,LL)+(TTND(I,J,LL)
     +	            -TTND(I,J,LL-1))*FACT
	  
C FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
C ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
C LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
C GOUND
        ELSE
          ii=im/2
          jj=(jsta+jend)/2
          if(i.eq.ii.and.j.eq.jj)print*,'Debug: underg extra at i,j,lp'
     &,   i,j,lp
	  PL=PINT(I,J,LM-1)
          ZL=ZINT(I,J,LM-1)
          TL=0.5*(T(I,J,LM-2)+T(I,J,LM-1))
          QL=0.5*(Q(I,J,LM-2)+Q(I,J,LM-1))
!	  TMT0=TL-A0
!          TMT15=AMIN1(TMT0,-15.)
!          AI=0.008855
!          BI=1.
!          IF(TMT0.LT.-20.)THEN
!            AI=0.007225
!            BI=0.9674
!          ENDIF
          QSAT=PQ0/PL
     1      *EXP(A2*(TL-A3)/(TL-A4))
C
          RHL=QL/QSAT
C
          IF(RHL.GT.1.)THEN
            RHL=1.
            QL =RHL*QSAT
          ENDIF
C
          IF(RHL.LT.RHmin)THEN
            RHL=RHmin
            QL =RHL*QSAT
          ENDIF
C
          TVRL  =TL*(1.+0.608*QL)
          TVRBLO=TVRL*(SPL(LP)/PL)**RGAMOG
          TBLO  =TVRBLO/(1.+0.608*QL)
C     
!          TMT0=TBLO-A3
!          TMT15=AMIN1(TMT0,-15.)
!          AI=0.008855
!          BI=1.
!          IF(TMT0.LT.-20.)THEN
!            AI=0.007225
!            BI=0.9674
!          ENDIF
          QSAT=PQ0/SPL(LP)
     1      *EXP(A2*(TBLO-A3)/(TBLO-A4))
C
          TSL(I,J)=TBLO
	  QBLO =RHL*QSAT
          QSL(I,J)=AMAX1(1.E-12,QBLO)
          if(tsl(i,j).gt.320. .or. tsl(i,j).lt.100.)print*,
     +    'bad isobaric T Q',i,j,lp,tsl(i,j),qsl(i,j),tl,ql,pl
	  IF(MODELNAME .EQ. 'NCAR'.OR. MODELNAME .EQ. 'RSM')THEN
           USL(I,J)=UH(I,J,LLMH)
	   VSL(I,J)=VH(I,J,LLMH)
	  END IF 
	  WSL(I,J)=WH(I,J,LLMH)
	  OSL(I,J)=OMGA(I,J,LLMH)
	  Q2SL(I,J)=0.5*(Q2(I,J,LLMH-1)+Q2(I,J,LLMH))
	  IF(Q2SL(I,J).LT.0.0) Q2SL(I,J)=0.0
	  PNL1=PINT(I,J,NL1X(I,J))
	  FAC=0.
	  AHF=0.0
c          FSL(I,J)=(PNL1-SPL(LP))/(SPL(LP)+PNL1)
c     1       *(TSL(I,J))*(1.+0.608*QSL(I,J))
c     2       *RD*2.+ZINT(I,J,NL1X(I,J))*G

!          FSL(I,J)=FPRS(I,J,LP-1)-RD*(TPRS(I,J,LP-1)
!     1             *(H1+D608*QPRS(I,J,LP-1))
!     2             +TSL(I,J)*(H1+D608*QSL(I,J)))
!     3             *LOG(SPL(LP)/SPL(LP-1))/2.0

          if(abs(SPL(LP)-97500.0).lt.0.01)then
           if(gdlat(i,j).gt.35.0.and.gdlat(i,j).le.37.0 .and.
     1    gdlon(i,j).gt.-100.0 .and. gdlon(i,j).lt.-96.0)print*,
     2    'Debug:I,J,FPRS(LP-1),TPRS(LP-1),TSL,SPL(LP),SPL(LP-1)='
     3,    i,j,FPRS(I,J,LP-1),TPRS(I,J,LP-1),TSL(I,J),SPL(LP)
     4,SPL(LP-1)
c          if(gdlat(i,j).gt.35.0.and.gdlat(i,j).le.37.0 .and.
c     1    gdlon(i,j).gt.-100.0 .and. gdlon(i,j).lt.-96.0)print*,
c     2    'Debug:I,J,PNL1,TSL,NL1X,ZINT,FSL= ',I,J,PNL1,TSL(I,J)
c     3    ,NL1X(I,J),ZINT(I,J,NL1X(I,J)),FSL(I,J)/G
          end if
c          if(lp.eq.lsm)print*,'Debug:undergound T,Q,U,V,FSL='
c     1,TSL(I,J),QSL(I,J),USL(I,J),VSL(I,J),FSL(I,J)
C
!--- Set hydrometeor fields to zero below ground
          C1D(I,J)=0.
          QW1(I,J)=0.
          QI1(I,J)=0.
          QR1(I,J)=0.
          QS1(I,J)=0.
	  DBZ1(I,J)=DBZmin
	  FRIME(I,J)=1.
	  RAD(I,J)=0.
        END IF	
	    	
	LL=NL1XF(I,J)
        IF(NL1XF(I,J).LE.(LLMH+1))THEN
	  FACT=(ALSL(LP)-ALOG(PINT(I,J,LL)))/
     &         (ALOG(PINT(I,J,LL))-ALOG(PINT(I,J,LL-1)))
	  IF(ZINT(I,J,LL).LT.SPVAL .AND. ZINT(I,J,LL-1).LT.SPVAL)
     &       FSL(I,J)=ZINT(I,J,LL)+(ZINT(I,J,LL)-ZINT(I,J,LL-1))*FACT
          FSL(I,J)=FSL(I,J)*G 
	ELSE
	  FSL(I,J)=FPRS(I,J,LP-1)-RD*(TPRS(I,J,LP-1)
     1             *(H1+D608*QPRS(I,J,LP-1))
     2             +TSL(I,J)*(H1+D608*QSL(I,J)))
     3             *LOG(SPL(LP)/SPL(LP-1))/2.0 
        END IF 

  220   CONTINUE
C
C***  FILL THE 3-D-IN-PRESSURE ARRAYS FOR THE MEMBRANE SLP REDUCTION
C
        DO J=JSTA,JEND
        DO I=1,IM
          TPRS(I,J,LP)=TSL(I,J)
          QPRS(I,J,LP)=QSL(I,J)
          FPRS(I,J,LP)=FSL(I,J)
        ENDDO
        ENDDO
C	
C VERTICAL INTERPOLATION FOR WIND FOR E GRID
C
        IF(MODELNAME .EQ. 'NMM')THEN
        DO J=JSTA,JEND
        DO I=2,IM-MOD(J,2)
c       IF(i.eq.im/2 .and. j.eq.(jsta+jend)/2)then
c         do l=1,lm
c          print*,'PMIDV=',PMIDV(i,j,l)
c         end do
c       end if  
C
C***  LOCATE VERTICAL INDEX OF MODEL MIDLAYER FOR V POINT JUST BELOW
C***  THE PRESSURE LEVEL TO WHICH WE ARE INTERPOLATING.
C
        NL1X(I,J)=LP1
        DO L=2,LM
!	 IF(J .EQ. 1 .AND. I .LT. IM)THEN   !SOUTHERN BC
!           PDV=0.5*(PMID(I,J,L)+PMID(I+1,J,L))
!         ELSE IF(J.EQ.JM .AND. I.LT.IM)THEN   !NORTHERN BC
!           PDV=0.5*(PMID(I,J,L)+PMID(I+1,J,L))
!         ELSE IF(I .EQ. 1 .AND. MOD(J,2) .EQ. 0) THEN   !WESTERN EVEN BC
!           PDV=0.5*(PMID(I,J-1,L)+PMID(I,J+1,L))
!	 ELSE IF(I .EQ. IM .AND. MOD(J,2) .EQ. 0) THEN   !EASTERN EVEN BC
!           PDV=0.5*(PMID(I,J-1,L)+PMID(I,J+1,L))  
!         ELSE IF (MOD(J,2) .LT. 1) THEN
!           PDV=0.25*(PMID(I,J,L)+PMID(I-1,J,L)
!     &       +PMID(I,J+1,L)+PMID(I,J-1,L))
!         ELSE
!           PDV=0.25*(PMID(I,J,L)+PMID(I+1,J,L)
!     &       +PMID(I,J+1,L)+PMID(I,J-1,L))
!         END IF
!         JJB=JSTA 
!         IF(MOD(JSTA,2).EQ.0)JJB=JSTA+1
!         JJE=JEND
!         IF(MOD(JEND,2).EQ.0)JJE=JEND-1
!         DO J=JJB,JJE,2 !chc
!          PDV(IM,J)=PDV(IM-1,J)
!         END DO
	  
         IF(NL1X(I,J).EQ.LP1.AND.PMIDV(I,J,L).GT.SPL(LP))THEN
          NL1X(I,J)=L
	  IF(i.eq.im/2 .and. j.eq.jm/2)print*,
     &'Wind Debug:LP,NL1X',LP,NL1X(I,J)
         ENDIF
        ENDDO
C
C  IF THE PRESSURE LEVEL IS BELOW THE LOWEST MODEL MIDLAYER
C  BUT STILL ABOVE THE LOWEST MODEL BOTTOM INTERFACE,
C  WE WILL NOT CONSIDER IT UNDERGROUND AND THE INTERPOLATION
C  WILL EXTRAPOLATE TO THAT POINT
C
!        IF(NL1X(I,J).EQ.LMP1.AND.PINT(I,J,LMP1).GT.SPL(LP))THEN	
	IF(NL1X(I,J).EQ.LP1)THEN
	 IF(J .EQ. 1 .AND. I .LT. IM)THEN   !SOUTHERN BC
           PDV=0.5*(PINT(I,J,LP1)+PINT(I+1,J,LP1))
         ELSE IF(J.EQ.JM .AND. I.LT.IM)THEN   !NORTHERN BC
           PDV=0.5*(PINT(I,J,LP1)+PINT(I+1,J,LP1))
         ELSE IF(I .EQ. 1 .AND. MOD(J,2) .EQ. 0) THEN   !WESTERN EVEN BC
           PDV=0.5*(PINT(I,J-1,LP1)+PINT(I,J+1,LP1))
	 ELSE IF(I .EQ. IM .AND. MOD(J,2) .EQ. 0) THEN   !EASTERN EVEN BC
           PDV=0.5*(PINT(I,J-1,LP1)+PINT(I,J+1,LP1))  
         ELSE IF (MOD(J,2) .LT. 1) THEN
           PDV=0.25*(PINT(I,J,LP1)+PINT(I-1,J,LP1)
     &       +PINT(I,J+1,LP1)+PINT(I,J-1,LP1))
         ELSE
           PDV=0.25*(PINT(I,J,LP1)+PINT(I+1,J,LP1)
     &       +PINT(I,J+1,LP1)+PINT(I,J-1,LP1))
         END IF
	 IF(PDV .GT.SPL(LP))THEN
          NL1X(I,J)=LM
	 END IF 
        ENDIF
C
        ENDDO
        ENDDO
C
        DO 230 J=JSTA,JEND
        DO 230 I=1,IM-MOD(j,2)
        
        LL=NL1X(I,J)
C---------------------------------------------------------------------
C***  VERTICAL INTERPOLATION OF WINDS FOR A-E GRID
C---------------------------------------------------------------------
C         
CHC        IF(NL1X(I,J).LE.LM)THEN
        LLMH = NINT(LMH(I,J))
	
	IF(SPL(LP) .LT. PINT(I,J,2))THEN ! Above second interface
	  IF(UH(I,J,1).LT.SPVAL)
     &	     USL(I,J)=UH(I,J,1)
          IF(VH(I,J,1).LT.SPVAL)
     &	     VSL(I,J)=VH(I,J,1)
     
        ELSE IF(NL1X(I,J).LE.LLMH)THEN
C
C---------------------------------------------------------------------
C          INTERPOLATE LINEARLY IN LOG(P)
C***  EXTRAPOLATE ABOVE THE TOPMOST MIDLAYER OF THE MODEL
C***  INTERPOLATION BETWEEN NORMAL LOWER AND UPPER BOUNDS
C***  EXTRAPOLATE BELOW LOWEST MODEL MIDLAYER (BUT STILL ABOVE GROUND)
C---------------------------------------------------------------------
C
	   
          FACT=(ALSL(LP)-ALOG(PMIDV(I,J,LL)))/
     &         (ALOG(PMIDV(I,J,LL))-ALOG(PMIDV(I,J,LL-1)))
          IF(UH(I,J,LL).LT.SPVAL .AND. UH(I,J,LL-1).LT.SPVAL)
     &        USL(I,J)=UH(I,J,LL)+(UH(I,J,LL)-UH(I,J,LL-1))*FACT
          IF(VH(I,J,LL).LT.SPVAL .AND. VH(I,J,LL-1).LT.SPVAL)
     &        VSL(I,J)=VH(I,J,LL)+(VH(I,J,LL)-VH(I,J,LL-1))*FACT
          IF(i.eq.im/2 .and. j.eq.jm/2)print*,
     &'Wind Debug:LP,NL1X,FACT=',LP,NL1X(I,J),FACT
C
C FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
C ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
C LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
C GOUND
        ELSE
          IF(UH(I,J,LLMH).LT.SPVAL)USL(I,J)=UH(I,J,LLMH)
	  IF(VH(I,J,LLMH).LT.SPVAL)VSL(I,J)=VH(I,J,LLMH)
        END IF
  230   CONTINUE
        JJB=JSTA 
        IF(MOD(JSTA,2).EQ.0)JJB=JSTA+1
        JJE=JEND
        IF(MOD(JEND,2).EQ.0)JJE=JEND-1
        DO J=JJB,JJE,2 !chc
          USL(IM,J)=USL(IM-1,J)
	  VSL(IM,J)=VSL(IM-1,J)
        END DO
        END IF  ! END OF WIND INTERPOLATION FOR NMM


C
C---------------------------------------------------------------------
C        LOAD GEOPOTENTIAL AND TEMPERATURE INTO STANDARD LEVEL 
C        ARRAYS FOR THE NEXT PASS.
C---------------------------------------------------------------------
C     
C***     SAVE 500MB TEMPERATURE FOR LIFTED INDEX.
C     
        IF(NINT(SPL(LP)).EQ.50000)THEN
          DO J=JSTA,JEND
          DO I=1,IM
            T500(I,J)=TSL(I,J)
          ENDDO
          ENDDO
        ENDIF
C     
C---------------------------------------------------------------------
C***  CALCULATE 1000MB GEOPOTENTIALS CONSISTENT WITH SLP OBTAINED 
C***  FROM THE MESINGER OR NWS SHUELL SLP REDUCTION.
C---------------------------------------------------------------------
C     
C***  FROM MESINGER SLP
C
CHC MOVE THIS PART TO THE END OF THIS SUBROUTINE AFTER PSLP IS COMPUTED
CHC        IF(IGET(023).GT.0.AND.NINT(SPL(LP)).EQ.100000)THEN
CHC          ALPTH=ALOG(1.E5)
CHC!$omp  parallel do private(i,j)
CHC          DO J=JSTA,JEND
CHC          DO I=1,IM
CHC           IF(FSL(I,J).LT.SPVAL) THEN
CHC            PSLPIJ=PSLP(I,J)
CHC            ALPSL=ALOG(PSLPIJ)
CHC            PSFC=PINT(I,J,NINT(LMH(I,J))+1)
CHC            IF(ABS(PSLPIJ-PSFC).LT.5.E2) THEN
CHC              FSL(I,J)=R*TSL(I,J)*(ALPSL-ALPTH)
CHC            ELSE
CHC              FSL(I,J)=FIS(I,J)/(ALPSL-ALOG(PSFC))*
CHC     1                              (ALPSL-ALPTH)
CHC            ENDIF
CHC            Z1000(I,J)=FSL(I,J)*GI
CHC           ELSE
CHC            Z1000(I,J)=SPVAL
CHC           ENDIF
CHC          ENDDO
CHC          ENDDO
C     
C***  FROM NWS SHUELL SLP. NGMSLP2 COMPUTES 1000MB GEOPOTENTIAL.
C
CHC        ELSEIF(IGET(023).LE.0.AND.LP.EQ.LSM)THEN
CHC        IF(IGET(023).LE.0.AND.LP.EQ.LSM)THEN
!$omp  parallel do private(i,j)
CHC          DO J=JSTA,JEND
CHC          DO I=1,IM
CHC           IF(Z1000(I,J).LT.SPVAL) THEN
CHC            FSL(I,J)=Z1000(I,J)*G
CHC           ELSE
CHC            FSL(I,J)=SPVAL
CHC           ENDIF
CHC          ENDDO
CHC          ENDDO
CHC        ENDIF
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C        *** PART II ***
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C
C        INTERPOLATE/OUTPUT SELECTED FIELDS.
C
C---------------------------------------------------------------------
C
C***  OUTPUT GEOPOTENTIAL (SCALE BY GI)
C
        IF(IGET(012).GT.0)THEN
          IF(LVLS(LP,IGET(012)).GT.0)THEN
           IF(IGET(023).GT.0.AND.NINT(SPL(LP)).EQ.100000)THEN
            GO TO 222
           ELSE
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              IF(FSL(I,J).LT.SPVAL) THEN
                GRID1(I,J)=FSL(I,J)*GI
              ELSE
                GRID1(I,J)=SPVAL
              ENDIF
            ENDDO
            ENDDO
            ID(1:25)=0
            CALL GRIBIT(IGET(012),LP,GRID1,IM,JM)
           END IF
          ENDIF
        ENDIF
 222    CONTINUE
C     
C***  TEMPERATURE
C
        IF(IGET(013).GT.0) THEN
          IF(LVLS(LP,IGET(013)).GT.0)THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=TSL(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
             CALL GRIBIT(IGET(013),LP,GRID1,IM,JM)
          ENDIF
        ENDIF
C     
C***  POTENTIAL TEMPERATURE.
C
        IF(IGET(014).GT.0)THEN
          IF(LVLS(LP,IGET(014)).GT.0)THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID2(I,J)=SPL(LP)
            ENDDO
            ENDDO
C
            CALL CALPOT(EGRID2,TSL,EGRID1)
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=EGRID1(I,J)
             ENDDO
             ENDDO
            ID(1:25)=0
            CALL GRIBIT(IGET(014),LP,GRID1,IM,JM)
          ENDIF
        ENDIF
C     
C***  RELATIVE HUMIDITY.
C
        IF(IGET(017).GT.0)THEN
          IF(LVLS(LP,IGET(017)).GT.0)THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID2(I,J)=SPL(LP)
            ENDDO
            ENDDO
C
            CALL CALRH(EGRID2,TSL,QSL,EGRID1)
             DO J=JSTA,JEND
             DO I=1,IM
              IF(EGRID1(I,J).LT.SPVAL) THEN
                GRID1(I,J)=EGRID1(I,J)*100.
              ELSE
                GRID1(I,J)=EGRID1(I,J)
              ENDIF
             ENDDO
             ENDDO
            ID(1:25)=0
            CALL GRIBIT(IGET(017),LP,GRID1,IM,JM)
                                                                                                          
            DO J=JSTA,JEND
             DO I=1,IM
              SAVRH(I,J) = GRID1(I,J)
             ENDDO
            ENDDO

          ENDIF
        ENDIF

C     
C***  DEWPOINT TEMPERATURE.
C
        IF(IGET(015).GT.0)THEN
          IF(LVLS(LP,IGET(015)).GT.0)THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID2(I,J)=SPL(LP)
            ENDDO
            ENDDO
C
            CALL CALDWP(EGRID2,QSL,EGRID1,TSL)
             DO J=JSTA,JEND
             DO I=1,IM
              IF(TSL(I,J).LT.SPVAL) THEN
               GRID1(I,J)=EGRID1(I,J)
              ELSE
               GRID1(I,J)=SPVAL
              ENDIF
             ENDDO
             ENDDO
            ID(1:25)=0
            CALL GRIBIT(IGET(015),LP,GRID1,IM,JM)
          ENDIF
        ENDIF
C     
C***  SPECIFIC HUMIDITY.
C
        IF(IGET(016).GT.0)THEN
          IF(LVLS(LP,IGET(016)).GT.0)THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QSL(I,J)
             ENDDO
             ENDDO
            CALL BOUND(GRID1,H1M12,H99999)
            ID(1:25)=0
            CALL GRIBIT(IGET(016),LP,GRID1,IM,JM)
          ENDIF
        ENDIF
C     
C***  OMEGA
C
        IF(IGET(020).GT.0)THEN
          IF(LVLS(LP,IGET(020)).GT.0)THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=OSL(I,J)
             ENDDO
             ENDDO
            ID(1:25)=0
            CALL GRIBIT(IGET(020),LP,GRID1,IM,JM)
          ENDIF
        ENDIF
C     
C***  W
C
        IF(IGET(284).GT.0)THEN
          IF(LVLS(LP,IGET(284)).GT.0)THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=WSL(I,J)
             ENDDO
             ENDDO
            ID(1:25)=0
            CALL GRIBIT(IGET(284),LP,GRID1,IM,JM)
          ENDIF
        ENDIF
C     
C***  MOISTURE CONVERGENCE
C
        IF(IGET(085).GT.0)THEN
          IF(LVLS(LP,IGET(085)).GT.0)THEN
            CALL CALMCVG(QSL,USL,VSL,EGRID1)
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=EGRID1(I,J)
             ENDDO
             ENDDO
CMEB NOT SURE IF I STILL NEED THIS
C     CONVERT TO DIVERGENCE FOR GRIB UNITS
C
C           CALL SCLFLD(GRID1,-1.0,IM,JM)
CMEB NOT SURE IF I STILL NEED THIS
            ID(1:25)=0
            CALL GRIBIT(IGET(085),LP,GRID1,IM,JM)
          ENDIF
        ENDIF
C     
C***  U AND/OR V WIND
C
        IF(IGET(018).GT.0.OR.IGET(019).GT.0)THEN
          IF(LVLS(LP,IGET(018)).GT.0.OR.LVLS(LP,IGET(019)).GT.0)THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=USL(I,J)
               GRID2(I,J)=VSL(I,J)
             ENDDO
             ENDDO
            ID(1:25)=0
            IF(IGET(018).GT.0)
     &        CALL GRIBIT(IGET(018),LP,GRID1,IM,JM)
            ID(1:25)=0
            IF(IGET(019).GT.0) 
     &       CALL GRIBIT(IGET(019),LP,GRID2,IM,JM)
          ENDIF
        ENDIF
C     
C***  ABSOLUTE VORTICITY
C
         IF (IGET(021).GT.0) THEN
          IF (LVLS(LP,IGET(021)).GT.0) THEN
            CALL CALVOR(USL,VSL,EGRID1)
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=EGRID1(I,J)
             ENDDO
             ENDDO
            ID(1:25)=0
            CALL GRIBIT(IGET(021),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C     
C        GEOSTROPHIC STREAMFUNCTION.
         IF (IGET(086).GT.0) THEN
          IF (LVLS(LP,IGET(086)).GT.0) THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID2(I,J)=FSL(I,J)*GI
            ENDDO
            ENDDO
            CALL CALSTRM(EGRID2,EGRID1)
             DO J=JSTA,JEND
             DO I=1,IM
              IF(FSL(I,J).LT.SPVAL) THEN
               GRID1(I,J)=EGRID1(I,J)
              ELSE
               GRID1(I,J)=SPVAL
              ENDIF
             ENDDO
             ENDDO
            ID(1:25)=0
            CALL GRIBIT(IGET(086),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C     
C***  TURBULENT KINETIC ENERGY
C
         IF (IGET(022).GT.0) THEN
          IF (LVLS(LP,IGET(022)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=Q2SL(I,J)
             ENDDO
             ENDDO
            ID(1:25)=0
            CALL GRIBIT(IGET(022),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C     
C***  CLOUD WATER
C
         IF (IGET(153).GT.0) THEN
          IF (LVLS(LP,IGET(153)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QW1(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
             CALL GRIBIT(IGET(153),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C
C***  CLOUD ICE 
C
         IF (IGET(166).GT.0) THEN
          IF (LVLS(LP,IGET(166)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QI1(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
             CALL GRIBIT(IGET(166),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C
C---  RAIN
c        print*,'iget183= ',IGET(183)
         IF (IGET(183).GT.0) THEN
          IF (LVLS(LP,IGET(183)).GT.0) THEN 
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QR1(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
             CALL GRIBIT(IGET(183),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C
C---  SNOW
         IF (IGET(184).GT.0) THEN
          IF (LVLS(LP,IGET(184)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QS1(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
             CALL GRIBIT(IGET(184),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C
C---  TOTAL CONDENSATE
         IF (IGET(198).GT.0) THEN
          IF (LVLS(LP,IGET(198)).GT.0) THEN 
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=C1D(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
             ID(02)=129    ! Parameter Table 129
             CALL GRIBIT(IGET(198),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C
C---  RIME FACTOR
         IF (IGET(263).GT.0) THEN
          IF (LVLS(LP,IGET(263)).GT.0) THEN 
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=FRIME(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
             ID(02)=129    ! Parameter Table 129
             CALL GRIBIT(IGET(263),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C
C---  Temperature tendency by all radiation: Requested by AFWA
         IF (IGET(294).GT.0) THEN
          IF (LVLS(LP,IGET(294)).GT.0) THEN 
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=RAD(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
             CALL GRIBIT(IGET(294),LP,GRID1,IM,JM)
          ENDIF
         ENDIF	 
C
C---  Radar Reflectivity
         IF (IGET(251).GT.0) THEN
          IF (LVLS(LP,IGET(251)).GT.0) THEN
	     DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=DBZ1(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
	     ID(02)=129
             CALL GRIBIT(IGET(251),LP,GRID1,IM,JM)
          ENDIF
         ENDIF
C
C---  IN-FLIGHT ICING CONDITION: ADD BY B. ZHOU
        IF(IGET(257).GT.0)THEN
          IF(LVLS(LP,IGET(257)).GT.0)THEN
            CALL CALICING(TSL,SAVRH,OSL,EGRID1)
                                                                                                          
                                                                                                          
             DO J=JSTA,JEND
             DO I=1,IM
                GRID1(I,J)=EGRID1(I,J)
             ENDDO
             ENDDO
                                                                                                          
                                                                                                          
            ID(1:25)=0
            CALL GRIBIT(IGET(257),LP,GRID1,IM,JM)
                                                                                                          
                                                                                                          
          ENDIF
        ENDIF
 
C---  CLEAR AIR TURBULENCE (CAT): ADD BY B. ZHOU
        IF (LP .GT. 1) THEN
         IF(IGET(258).GT.0)THEN
          IF(LVLS(LP,IGET(258)).GT.0)THEN
             DO J=JSTA,JEND
             DO I=1,IM
                GRID1(I,J)=FSL(I,J)*GI
                EGRID1(I,J)=SPVAL
             ENDDO
             ENDDO
             CALL CALCAT(USL,VSL,GRID1,USL_OLD,VSL_OLD
     +	     ,FSL_OLD,EGRID1)
             DO J=JSTA,JEND
             DO I=1,IM
                GRID1(I,J)=EGRID1(I,J)
!                IF(GRID1(I,J) .GT. 3. .OR. GRID1(I,J).LT.0.)
!     +          print*,'bad CAT',i,j,GRID1(I,J)
             ENDDO
             ENDDO
            ID(1:25)=0
           CALL GRIBIT(IGET(258),LP,GRID1,IM,JM)
	  end if
	 end if
	end if    
C     

        DO J=JSTA_2L,JEND_2U
          DO I=1,IM
            USL_OLD(I,J)=USL(I,J)
	    VSL_OLD(I,J)=VSL(I,J)
	    FSL_OLD(I,J)=FSL(I,J)*GI
          ENDDO
	ENDDO
C***  END OF MAIN VERTICAL LOOP
C     
  310   CONTINUE
C***  ENDIF FOR IF TEST SEEING IF WE WANT ANY OTHER VARIABLES
C
      ENDIF
C
C  CALL MEMBRANE SLP REDUCTION IF REQUESTED IN CONTROL FILE
C
C OUTPUT MEMBRANCE SLP
      IF(IGET(023).GT.0)THEN
        IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM')THEN
         PRINT*,'CALLING MEMSLP'
         CALL MEMSLP(TPRS,QPRS,FPRS)
        ELSE IF (MODELNAME .EQ. 'NMM')THEN
         PRINT*,'CALLING MEMSLP_NMM'
         CALL MEMSLP_NMM(TPRS,QPRS,FPRS)
        ELSE
         PRINT*,'UNKNOW MODEL-> WONT DERIVE MESINGER SLP'
        END IF
	DO J=JSTA,JEND
        DO I=1,IM
          GRID1(I,J)=PSLP(I,J)
        ENDDO
        ENDDO
	ID(1:25)=0
	CALL GRIBIT(IGET(023),LVLS(1,IGET(023)),GRID1,IM,JM)
C ADJUST 1000 MB HEIGHT TO MEMBEANCE SLP
        IF(IGET(012).GT.0)THEN
         DO LP=LSM,1,-1
	  IF(ABS(SPL(LP)-1.0E5).LE.1.0E-5)THEN
           IF(LVLS(LP,IGET(012)).GT.0)THEN
            ALPTH=ALOG(1.E5)
!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
            DO I=1,IM
             PSLPIJ=PSLP(I,J)
             ALPSL=ALOG(PSLPIJ)
             PSFC=PINT(I,J,NINT(LMH(I,J))+1)
             IF(ABS(PSLPIJ-PSFC).LT.5.E2) THEN
              GRID1(I,J)=RD*TPRS(I,J,LP)*(ALPSL-ALPTH)
             ELSE
              GRID1(I,J)=FIS(I,J)/(ALPSL-ALOG(PSFC))*
     1                              (ALPSL-ALPTH)
             ENDIF
             Z1000(I,J)=GRID1(I,J)*GI
             GRID1(I,J)=Z1000(I,J)
            ENDDO
            ENDDO	    
	    ID(1:25)=0
	    CALL GRIBIT(IGET(012),LP,GRID1,IM,JM)
	    GO TO 320
	   END IF
	  END IF 
	 END DO
 320     CONTINUE
 	END IF  
      END IF 	
C
C     
C     END OF ROUTINE.
C
      RETURN
      END
