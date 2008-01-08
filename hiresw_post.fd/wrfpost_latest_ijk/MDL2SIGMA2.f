      SUBROUTINE MDL2SIGMA2
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
C
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
      INCLUDE "RQSTFLD.comm"
C
!      PARAMETER (IM_JM=IM*JM,LMP1=LM+1)
      PARAMETER (GAMMA=6.5E-3,RGAMOG=RD*GAMMA/G)
!      PARAMETER (LSIG=60,PTSIGO=1.0E4)
      PARAMETER (LSIG=5)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESTRT,SIGMA,READTHK
      LOGICAL IOOMG,IOALL
      LOGICAL DONEFSL1,TSLDONE
      REAL FSL(IM,JM),TSL(IM,JM),QSL(IM,JM)
      REAL OSL(IM,JM),USL(IM,JM),VSL(IM,JM)
      REAL Q2SL(IM,JM),FSL1(IM,JM),CFRSIG(IM,JM)
      REAL EGRID1(IM,JM),EGRID2(IM,JM)
      REAL GRID1(IM,JM),GRID2(IM,JM)
      REAL SIGO(LSIG+1),DSIGO(LSIG),ASIGO(LSIG)
C
      INTEGER IHOLD(IM_JM),JHOLD(IM_JM),NL1X(IM,JM),NL1XF(IM,JM)
C
!
!--- Definition of the following 2D (horizontal) dummy variables
!
!  C1D   - total condensate
!  QW1   - cloud water mixing ratio
!  QI1   - cloud ice mixing ratio
!  QR1   - rain mixing ratio
!  QS1   - snow mixing ratio
!
      REAL C1D(IM,JM),QW1(IM,JM),QI1(IM,JM),QR1(IM,JM)
     &,    QS1(IM,JM),QG1(IM,JM),AKH(IM,JM)
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
      IF((IGET(296).GT.0) ) THEN  !!Air Quality (Plee Oct2003)
C
C---------------------------------------------------------------------
!
!---  VERTICAL INTERPOLATION OF GEOPOTENTIAL, SPECIFIC HUMIDITY, TEMPERATURE, 
!     OMEGA, TKE, & CLOUD FIELDS.  START AT THE UPPERMOST TARGET SIGMA LEVEL.
!
        PTSIGO=PT   
        READTHK=.FALSE.
        IF(READTHK)THEN   ! EITHER READ DSG THICKNESS
	 READ(41)DSIGO  !DSIGO FROM TOP TO BOTTOM
!
         SIGO(1)=0.0
	 DO L=2,LSIG+1
          SIGO(L)=SIGO(L-1)+DSIGO(LSIG-L+2)
	 END DO 
         SIGO(LSIG+1)=1.0
         DO L=1,LSIG
          ASIGO(L)=0.5*(SIGO(L)+SIGO(L+1))
         END DO
        ELSE  ! SPECIFY SIGO
         ASIGO( 1)=   0.7000
         ASIGO( 2)=   0.7500
         ASIGO( 3)=   0.8000
         ASIGO( 4)=   0.8500
         ASIGO( 5)=   0.9000
        END IF
C***
C***  BECAUSE SIGMA LAYERS DO NOT GO UNDERGROUND,  DO ALL
C***  INTERPOLATION ABOVE GROUND NOW.
C***
C

        DO 310 LP=1,LSIG
        NHOLD=0
C
        DO J=JSTA_2L,JEND_2U
        DO I=1,IM

C
        TSL(I,J)=SPVAL
C
C***  LOCATE VERTICAL INDEX OF MODEL MIDLAYER JUST BELOW
C***  THE PRESSURE LEVEL TO WHICH WE ARE INTERPOLATING.
C
        NL1X(I,J)=LP1
        DO L=2,LM
        LLMH = NINT(LMH(I,J))
        PSIGO=PTSIGO+ASIGO(LP)*(PINT(I,J,LLMH+1)-PTSIGO)
        IF(NL1X(I,J).EQ.LP1.AND.PMID(I,J,L).GT.PSIGO)THEN
          NL1X(I,J)=L
        ENDIF
        ENDDO
C
C  IF THE PRESSURE LEVEL IS BELOW THE LOWEST MODEL MIDLAYER
C  BUT STILL ABOVE THE LOWEST MODEL BOTTOM INTERFACE,
C  WE WILL NOT CONSIDER IT UNDERGROUND AND THE INTERPOLATION
C  WILL EXTRAPOLATE TO THAT POINT
C
        IF(NL1X(I,J).EQ.LP1.AND.PINT(I,J,LLMH+1).GE.PSIGO)THEN
          NL1X(I,J)=LM
        ENDIF
C
c        if(NL1X(I,J).EQ.LP1)print*,'Debug: NL1X=LP1 AT '
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
        LL=NL1X(I,J)
C---------------------------------------------------------------------
C***  VERTICAL INTERPOLATION OF GEOPOTENTIAL, TEMPERATURE, SPECIFIC
C***  HUMIDITY, CLOUD WATER/ICE, OMEGA, WINDS, AND TKE.
C---------------------------------------------------------------------
C
CHC        IF(NL1X(I,J).LE.LM)THEN
        LLMH = NINT(LMH(I,J))
	PSIGO=PTSIGO+ASIGO(LP)*(PINT(I,J,LLMH+1)-PTSIGO) 
	APSIGO=ALOG(PSIGO)
        IF(NL1X(I,J).LE.LLMH)THEN
C
C---------------------------------------------------------------------
C          INTERPOLATE LINEARLY IN LOG(P)
C***  EXTRAPOLATE ABOVE THE TOPMOST MIDLAYER OF THE MODEL
C***  INTERPOLATION BETWEEN NORMAL LOWER AND UPPER BOUNDS
C***  EXTRAPOLATE BELOW LOWEST MODEL MIDLAYER (BUT STILL ABOVE GROUND)
C---------------------------------------------------------------------
C

          FACT=(APSIGO-ALOG(PMID(I,J,LL)))/
     &         (ALOG(PMID(I,J,LL))-ALOG(PMID(I,J,LL-1)))
          TSL(I,J)=T(I,J,LL)+(T(I,J,LL)-T(I,J,LL-1))*FACT
C FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
C ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
C LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
C GOUND
        ELSE
          ii=91
          jj=13
          if(i.eq.ii.and.j.eq.jj)print*,'Debug: underg extra at i,j,lp'
     &,   i,j,lp
	  PL=PINT(I,J,LM-1)
          ZL=ZINT(I,J,LM-1)
          TL=0.5*(T(I,J,LM-2)+T(I,J,LM-1))
          QL=0.5*(Q(I,J,LM-2)+Q(I,J,LM-1))
          TMT15=AMIN1(TMT0,-15.)
          AI=0.008855
          BI=1.
          IF(TMT0.LT.-20.)THEN
            AI=0.007225
            BI=0.9674
          ENDIF
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
          IF(RHL.LT.0.01)THEN
            RHL=0.01
            QL =RHL*QSAT
          ENDIF
C
          TVRL  =TL*(1.+0.608*QL)
          TVRBLO=TVRL*(PSIGO/PL)**RGAMOG
          TBLO  =TVRBLO/(1.+0.608*QL)
C     
          TMT0=TBLO-A3
          TMT15=AMIN1(TMT0,-15.)
          AI=0.008855
          BI=1.
          IF(TMT0.LT.-20.)THEN
            AI=0.007225
            BI=0.9674
          ENDIF
          QSAT=PQ0/PSIGO
     1      *EXP(A2*(TBLO-A3)/(TBLO-A4))
C
          TSL(I,J)=TBLO
        END IF
  220   CONTINUE

C---------------------------------------------------------------------
C        *** PART II ***
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C
C        OUTPUT SELECTED FIELDS.
C     
C***  TEMPERATURE
C
        IF(IGET(296).GT.0) THEN
          IF(LVLS(LP,IGET(296)).GT.0)THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=TSL(I,J)
             ENDDO
             ENDDO
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(296),LP,GRID1,IM,JM)
          ENDIF
        ENDIF
C     
C***  END OF MAIN VERTICAL LOOP
C     
  310   CONTINUE
C***  ENDIF FOR IF TEST SEEING IF WE WANT ANY OTHER VARIABLES
C
      ENDIF
C
C
C     
C     END OF ROUTINE.
C
      RETURN
      END
