      PROGRAM SNDPST
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: ETA_SNDP
C   PRGMMR: ROGERS           ORG: NP22        DATE: 1999-09-24
C
C ABSTRACT:  THIS ROUTINE POSTS PROFILE DATA AND WRITES
C   OUTPUT IN BUFR FORMAT.  THIS REPLACES CODE THAT USED TO
C   RUN INSIDE OF CHKOUT IN THE ETA MODEL.
C     
C PROGRAM HISTORY LOG:
C   95-07-26  MIKE BALDWIN
C   96-05-07  MIKE BALDWIN - USE SMASK TO SET SOIL VARS TO MISSING
C   96-11-22  MIKE BALDWIN - ADD OPTION OF DOING MONOLITHIC FILE OR
C                            SPLIT OUT FILES OR BOTH
C   97-12-16  MIKE BALDWIN - NEW MULTI-LEVEL PARAMETERS (SUCH
C                            AS SOIL MOISTURE)
C   98-07-23  ERIC ROGERS  - MADE Y2K COMPLIANT
C   98-09-29  MIKE BALDWIN - SET ACC/AVE VARS TO MISSING AT T=0
C   99-04-01  GEOFF MANIKIN - MAJOR CHANGES FEATURING A REMOVAL OF
C                            THE DISTNICTION OF CLASS0 - ALL OUTPUT
C                            IS NOW CLASS1.  ALSO, THE FIELDS OF
C                            VISIBILITY AND CLOUD BASE PRESSURE 
C                            ARE ADDED.
C   99-09-03  JIM TUCCILLO - REDUCED MEMORY REQUIREMENTS BY CHANGING
C                            THE SIZE OF PRODAT AND INTRODUCING LPRO.
C                            ALSO, PRODAT'S STRUCTURE WAS CHANGED TO
C                            PROVIDE STRIDE-1 ACCESS.
C                            NOTE: THIS CODE CAN STILL BE MODIFIED TO
C                            REDUCE MEMORY. THE CHANGES TODAY WILL
C                            NOT AFFECT ITS FUNCTIONALITY BUT WILL
C                            ALLOW IT TO RUN ON A WINTERHAWK NODE
C                            WITHOUT PAGING. THE MEMORY REQUIREMENT
C                            IS NOW AT ABOUT 260 MBs.
C   00-03-10  GEOFF MANIKIN - CODE CHANGED TO BE READY FOR ETA EXTENSION
C                            TO 60 HOURS
C   00--5-15  ERIC ROGERS   - PUT NSOIL AND LM1 IN INCLUDED PARAMETER 
C                             FILE
C   04-11-18  BRAD FERRIER  - Added Cu precip rate; separated cloud water,
C             GEOFF MANIKIN   rain, & ice from CWM array => feed into visibility
C     
C USAGE:   
C   INPUT ARGUMENT LIST:
C     NONE    
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       CALWXT
C       CALHEL
C       BFRIZE
C     LIBRARY:
C       BUFRLIB     
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C***
C***   7/26/95 M. BALDWIN
C*** 
C***     SOUNDING POST PROCESSOR
C***     PROGRAM TO READ THE PROFILE OUTPUT FILE ON THE CRAY
C***     AND PRODUCE DIAGNOSTIC QUANTITIES AND PACK INTO BUFR
C***
C--------------------------------------------------------------------
C
C    PARMS FOR HOURLY PROFILER OUTPUT
C      LM    - MAX NUMBER OF VERTICAL LEVELS
C      NPNT  - MAX NUMBER OF OUTPUT TIMES    
C      
C          TO HOLD ALL VARIABLES FOR ANY CLASS OF OUTPUT
C          (MAX NO MULTI-LAYER VARIABLES*LM + NO OF SINGLE LAYER VARS)
C      LCL1ML1 - NUMBER OF MULTI-LAYER VARIABLES IN CLASS 1 OUTPUT
C      LCL1SL1 - NUMBER OF SINGLE LAYER VARIABLES IN CLASS 1 OUTPUT 
C      LCL1ML - NUMBER OF MULTI-LAYER VARIABLES IN PROFILM 
C      LCL1SL - NUMBER OF SINGLE LAYER VARIABLES IN PROFILM
C      LCL1SOIL - MAX NUMBER OF SOIL LAYER VARIABLES FOR CLASS 0 OR 1
C      NSTAT - NUMBER OF STATIONS
C
C        NOTE: THESE NUMBERS WILL BE LARGER THAN THE NUMBERS
C              COMING OUT OF THE MODEL IN THE BINARY FILE SINCE
C              WE ARE COMPUTING SOME ADDITIONAL VARIABLES TO GO INTO
C              BUFR IN THIS PROGRAM.
C
C--------------------------------------------------------------------
      INCLUDE "parm.nmm"
cZhou      PARAMETER (LM=60,NPNT=85,NSTAT=1400
        PARAMETER (LM=LM1,NPNT=NSTP
     &, SPVAL=-99999.0,SMISS=1.E10
     &, LCL1ML=15,LCL1SL=52,LCL1SOIL=2
     &, LCL1ML1=14 
     &, NWORD=(LCL1ML)*LM+2*LCL1SL+NSOIL*LCL1SOIL  !Binbin: NWORD is the # of data in output for each record (each station)
     &, ROG=287.04/9.8)
      PARAMETER (SNOCON=1.4594E5,RAINCON=1.1787E4)
C
      LOGICAL LVLWSE,SEQFLG(8),NEED
      CHARACTER*16 SEQNM1(8), SBSET
      CHARACTER*80 CLIST1(8),FMTO,ASSIGN
      CHARACTER*8 CISTAT
cwas  DIMENSION FPACK(NWORD),PRODAT(NSTAT,NPNT,NWORD)
      DIMENSION FPACK(NWORD)
      REAL PRODAT(NWORD,NSTAT,NPNT)
C
C     THE PURPOSE OF LPRO IS TO HOLD THE VALUES OF RISTAT UNTIL
C     THEY ARE COPIED TO FRODAT. THIS ADDITION WILL ALLOW PRODAT
C     TO BE A REAL(4) ARRAY ( AND SAVE A CONSIDERABLE AMOUNT OF 
C     MEMORY) . PRODAT CAN BE FURTHER REDUCED WITH SOME MORE EFFORT.
C                    JIM TUCCILLO
C
      REAL(8) LPRO(NSTAT,NPNT)
      REAL(8) RISTAT 
cwas  REAL(8) PRODAT(NSTAT,NPNT,NWORD),RISTAT 
      REAL(8) FRODAT(NWORD),WORKK(NWORD)
      DIMENSION P(LM),T(LM),U(LM),V(LM),Q(LM),PINT(LM+1),ZINT(LM+1)
      REAL CWTR(LM),IMXR(LM),RAIN(LM)
      INTEGER IDATE(3),NP1(8),LLMH(NSTAT),NLVL(2)
      INTEGER NSTAT_TRUE
      EQUIVALENCE (CISTAT,RISTAT)
C--------------------------------------------------------------------     
C
C     SET OUTPUT UNITS FOR CLASS 1 PROFILE FILE.
C      LCLAS1 - OUTPUT UNIT FOR CLASS 1 BINARY FILE
C      LTBCL1 - INPUT UNIT FOR CLASS 1 BUFR TABLE FILE
C      LUNCL1 - OUTPUT UNIT FOR CLASS 1 BUFR FILE
C
C--------------------------------------------------------------------     
                            I N T E G E R
     & LCLAS1,LTBCL1,LUNCL1,STDOUT
C--------------------------------------------------------------------     
                            L O G I C A L
     & MONOL,BRKOUT
C--------------------------------------------------------------------     
       NAMELIST /MODTOP/ ETOP
       NAMELIST /OPTION/ MONOL,BRKOUT
                            D A T A
     & LCLAS1 / 76 /
     &,LTBCL1 / 32 /
     &,LUNCL1 / 78 /
     &,STDOUT / 6 /
     &,SEQNM1 /'HEADR','PROFILE','SURF','FLUX',
     &         'HYDR','D10M','SLYR','XTRA'/
     &,SEQFLG /.FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &   .TRUE.,.FALSE./
     &,LVLWSE /.TRUE./

      PRODAT=0.
      FMTO='("ln -s ${DIRD}",I5.5,".",I4.4,3I2.2,'//
     &     '"  fort.",I2.2)'
C
C   GET MODEL TOP PRESSURE
C
       PTOP=25.0*100.0
       READ(5,MODTOP,END=12321)
12321  CONTINUE
       PTOP=ETOP*100.0
C
C   READ IN SWITCHES TO CONTROL WHETHER TO DO...
C     MONOL=.TRUE.   DO MONOLITHIC FILE
C     BRKOUT=.TRUE.  DO BREAKOUT FILES
C
       MONOL=.TRUE.
       BRKOUT=.FALSE.
       READ(11,OPTION,END=12322)
12322  CONTINUE
C
        IFCSTL=-99
        JHR=0
       write(*,*) 'NFCST=',NFCST
C----------------------------------------------------------------------
C---READ STATION DATA--------------------------------------------------
C----------------------------------------------------------------------
      LUNIT=66
      LRECPR=4*(8+9+LCL1ML*LM1+LCL1SL)         !Binbin: LRECPR is total bytes of one record (one record for one station) 
                                               !(8+9+LCL1ML*LM1+LCL1SL) is  # of one record for each station from input file
      write(*,*)'LRECPR=',LRECPR
      OPEN(UNIT=LUNIT,ACCESS='DIRECT',RECL=LRECPR,IOSTAT=IER)

      NREC=0
 33   CONTINUE
      NREC=NREC+1

!      DO 3000 NST = 1, NSTAT              !Binbin: Station loop, one station on loop     
                                           !Note: the order of fcst time and stations are reversed in WRF and in ETA/RSM 
                                           !In ETA/RSM, reading recode is first one station store all fcst hours, so first read   
                                           !    station, then read fcst time 
                                           !In WRF, all stations saved in one fcst hour (i.e. one files), so first read
                                           !    fcst hour, then read station 
                                           !So use DO 3000/4000 loop here 

!      DO 4000 JHR= 1,NFCST                 !Binbin: Forecast time loop

!      NREC=(JHR-1)*NSTAT + NST

!      READ(LUNIT,REC=NREC,END=999) IHRST,IDATE,IFCST,ISTAT,CISTAT,  
      READ(LUNIT,REC=NREC,ERR=999) IHRST,IDATE,IFCST,ISTAT,CISTAT,  
     &   (FPACK(N),N=1,9),(FPACK(N),N=10,FPACK(7))

       IF (IFCST.GT.IFCSTL) THEN
         INUMS=1
         JHR=JHR+1
         IFCSTL=IFCST
        ELSE
         INUMS=INUMS+1
        ENDIF

!        INUMS=NST

        IYR=IDATE(3)
        IMON=IDATE(1)
        IDAY=IDATE(2)
        RLAT=FPACK(1)*DEG
        RLON=FPACK(2)*DEG
        ELEV=FPACK(3)
        LLMH(INUMS)=NINT(FPACK(4))
        LMH=NINT(FPACK(4))

        write(0,*) 'STATION=',INUMS,' IHRST,IDATE,IFCST,ISTAT,CISTAT=',
     +   IHRST,IDATE,IFCST,ISTAT,CISTAT,' FPACK(1-3)=',(FPACK(K),K=1,3) 
c        do k=1,9
c         write(*,*) k, FPACK(K)
c        end do

        DO 25 L=1,LMH
C   REVERSE ORDER SO THAT P(1) IS THE TOP AND P(LMH) IS THE BOTTOM
         LV=LMH-L+1
         P(LV)=FPACK(L+9)
         T(LV)=FPACK(L+9+LMH)
         U(LV)=FPACK(L+9+LMH*2)
         V(LV)=FPACK(L+9+LMH*3)
         Q(LV)=FPACK(L+9+LMH*4)
C   CWTR, RAIN, AND IMXR NOW IN SEPARATE ARRAYS 
         CWTR(LV)=FPACK(L+9+LMH*6)
         RAIN(LV)=FPACK(L+9+LMH*13)  
         IMXR(LV)=FPACK(L+9+LMH*14) !Binbin note: IMXR is added  
 25     CONTINUE

c        do L=1,LMH
c        write(*,'(I5,f10.2,3f7.2,4f10.5)') L,P(L),T(L),U(L),V(L),
c     +    Q(L),CWTR(L),RAIN(L),IMXR(L)
c        end do

c        do J=10+15*LMH,FPACK(7)
c         write(*,*)J, FPACK(J)
c        end do

C  USE SEA MASK TO SET SOIL/SFC VARIABLES TO MISSING VALUES
C    (IF SEA)
C
         SM  =FPACK(LCL1ML*LMH+54)
         IF (SM.GT.0.5) THEN
C   SMSTAV
          FPACK(LCL1ML*LMH+15)=SMISS
C   SUBSHX
          FPACK(LCL1ML*LMH+21)=SMISS
C   SNOPCX
          FPACK(LCL1ML*LMH+22)=SMISS
C   ACSNOW, SMSTOT, SNO, ACSNOM, SSROFF, BGROFF, SOILTB
          DO LKJ=20,26
            FPACK(LCL1ML*LMH+LKJ+9)=SMISS
          ENDDO
C   SFCEXC, VEGFRC, CMC, SMC(1:4), STC(1:4)
          DO LKJ=34,44
            FPACK(LCL1ML*LMH+LKJ+9)=SMISS
          ENDDO
         ENDIF
C
C  GET PPT FOR CALWXT
C
         PPT  =FPACK(LCL1ML*LMH+16)
C     COMPUTE PINT,ZINT
C
        PINT(1)=PTOP
        DO L=1,LMH
          DP1=P(L)-PINT(L)
          PINT(L+1)=P(L)+DP1
        ENDDO

        ZINT(LMH+1)=FPACK(3)
        DO L=LMH,1,-1
         TV2=T(L)*(1.0+0.608*Q(L))
         ZZ=ROG*TV2*ALOG(PINT(L+1)/PINT(L))
         ZINT(L)=ZINT(L+1)+ZZ
        ENDDO
C
C     CALL PRECIP TYPE SUBROUTINES.
C
      RIME=FPACK(LCL1ML*LMH+61)
      SR=FPACK(LCL1ML*LMH+58)
      TSKIN=FPACK(LCL1ML*LMH+12)
      CALL CALWXT(T,Q,P,PINT,LMH,LM,PPT,IWX1)
      CALL CALWXT_RAMER(T,Q,P,PINT,LMH,LM,PPT,IWX2)
      CALL CALWXT_BOURG(T,Q,PINT,LMH,LM,PPT,ZINT,IWX3)
      CALL CALWXT_REVISED(T,Q,P,PINT,LMH,LM,PPT,IWX4) 

C WARNING: EXPLICIT ALGORITHM.  UNDER 18 NOT ADMITTED
C   WITHOUT PARENT OR GUARDIAN
      CALL CALWXT_EXPLICIT(LMH,TSKIN,PPT,SR,RIME,IWX5)
      CALL CALWXT_DOMINANT(PPT,IWX1,IWX2,IWX3,IWX4,IWX5,
     *        CSNO,CICE,CFZR,CRAI)

C   COMPUTE HELICITY AND STORM MOTION
C
      CALL CALHEL(U,V,P,ZINT,PINT,LMH,LM,HELI,UST,VST)
C
C   COMPUTE VISIBILITY
C   FIRST, EXTRACT SEA LEVEL PRESSURE
      SR=FPACK(LCL1ML*LMH+58)
      SLP=FPACK(LCL1ML*LMH+10)
      CPRATE=FPACK(LCL1ML*LMH+60)   !--- Convective precip rate
!---#####  Need grid-scale contributions to QRAIN and QSNO from the profilm file!!
      QRAIN=RAIN(LMH)
      QSNO=IMXR(LMH)    !-- Nearly all grid-scale ice is snow
      IF (CPRATE .GT. 0.) THEN
         RAINRATE=(1-SR)*CPRATE
         TERM1=(T(LMH)/SLP)**0.4167
         TERM2=(T(LMH)/(P(LMH)))**0.5833
         TERM3=RAINRATE**0.8333
         QRAIN=QRAIN+RAINCON*TERM1*TERM2*TERM3
         IF (SR .GT. 0.) THEN
            SNORATE=SR*CPRATE
            TERM1=(T(LMH)/SLP)**0.47
            TERM2=(T(LMH)/(P(LMH)))**0.53
            TERM3=SNORATE**0.94
            QSNO=QSNO+SNOCON*TERM1*TERM2*TERM3
         ENDIF
      ENDIF
      TT=T(LMH)
      QV=Q(LMH)
      QCD=CWTR(LMH)
!      QICE=IMXR(LMH)   !--- Nearly all of grid-scale ice is snow
      QICE=0.
      PPP=P(LMH)
      CALL CALVIS(QV,QCD,QRAIN,QICE,QSNO,TT,PPP,HOVI)
C   COMPUTE CLOUD BASE PRESSURE
C   FIRST, EXTRACT THE CONVECTIVE CLOUD BASE
       HBOT=FPACK(LCL1ML*LMH+59)
       CLIMIT =1.0E-06
       NEED = .TRUE.
       CDBP = SMISS
       CBOT = 5000
       DO L=LMH,1,-1
C GSM
C START AT THE FIRST LAYER ABOVE GROUND, AND FIND THE
C   FIRST LAYER WITH A VALUE OF CLOUD WATER GREATER THAN
C   THE SIGNIFICANT LIMIT (VALUE DESIGNATED BY Q. ZHAO).
C   THIS LAYER WILL BE THE CLOUD BOTTOM UNLESS THE BOTTOM
C   OF THE CONVECTIVE CLOUD (HBOT) IS FOUND BELOW IN WHICH
C   CASE HBOT BECOMES THE CLOUD BASE LAYER.

        IF ((CWTR(L)+IMXR(L)).GT.CLIMIT.AND.NEED) THEN
            CBOT=L
            IF (HBOT.GT.CBOT) THEN
              CBOT = HBOT
            ENDIF
            NEED=.FALSE.
          ENDIF
        ENDDO

        IF (CBOT.GT.LMH) THEN
          CDBP=SMISS
        ELSE
          CDBP=P(INT(CBOT))
        ENDIF
C
C
C   SET ACC/AVERAGED VARIABLES TO MISSING IF IFCST=0
C
      IF (IFCST.EQ.0) THEN
          DO L=1,LMH
           FPACK(L+9+LMH*7)=SMISS
           FPACK(L+9+LMH*8)=SMISS
          ENDDO
          DO JK=16,29
           FPACK(LCL1ML*LMH+JK)=SMISS
          ENDDO
          DO JK=32,34
           FPACK(LCL1ML*LMH+JK)=SMISS
          ENDDO
      ENDIF
C

C      ADD 9 SINGLE LEVEL VARIABLES TO THE OUTPUT
C      TACK THEM ON TO THE END;  WE DON'T NEED CONVECTIVE
C      CLOUD BASE OR RIME, THOUGH, SO WRITE OVER THOSE RECORDS
C
          NLENF = FPACK (7)
          NLENP = 7 + LCL1ML1*LMH + LCL1SL1
          FPACK(NLENF-2) = CSNO
          FPACK(NLENF-1) = CICE
          FPACK(NLENF) = CFZR
          FPACK(NLENF+1) = CRAI
          FPACK(NLENF+2) = UST
          FPACK(NLENF+3) = VST
          FPACK(NLENF+4) = HELI
          FPACK(NLENF+5) = CDBP
          FPACK(NLENF+6) = HOVI
C
C           PLACE DATA INTO PRODAT IN PROPER LOCATIONS

          PRODAT (1,INUMS,JHR) = FLOAT(IFCST)
          PRODAT (2,INUMS,JHR) = FLOAT(ISTAT)
C         RISTAT is a REAL(8) variable by virtue of the fact that it 
C         is equivalenced to CISTAT. Everything else stored in PRODAT
C         is REAL(4). We have made PRODAT REAL(4) but need a REAL(8)
C         array for storing RISTAT - that is what LPRO is. Farther
C         down in the code, we will pull values out of LPRO and store
C         in FRODAT ( a REAL(8) array ).
cwas      PRODAT (3,INUMS,JHR) = RISTAT 
          LPRO   (  INUMS,JHR) = RISTAT
          PRODAT (4,INUMS,JHR) = FPACK (1)
          PRODAT (5,INUMS,JHR) = FPACK (2)
          PRODAT (6,INUMS,JHR) = FPACK (3)
          PRODAT (7,INUMS,JHR) = 1 

C   FPACK HAS 9 ENTRIES BEFORE THE 1ST VERTICAL PROFILE, WHILE PRODAT HAS
C      ONLY 7.  THIS ACCOUNTS FOR THE IJ-2
C   START BUILDING PRODAT FROM FPACK, BUT STOP WHEN WE GET TO THE RAIN
C      ARRAY WHICH IS NOT WRITTEN TO PRODAT 

          DO IJ = 10, (LCL1ML1-1)*LMH+9
            PRODAT (IJ-2,INUMS,JHR) = FPACK (IJ)
          ENDDO

C    CONTINUE BUILDING PRODAT, BUT SKIP OVER RAIN.  THIS IS ACCOMPLISHED
C       BY ADDING LMH (THE SIZE OF THE RAIN ARRAY) TO THE FPACK INDEXING
          DO IJ = (LCL1ML1-1)*LMH+10,NLENP+2
            MN = IJ+LMH
            PRODAT (IJ-2,INUMS,JHR) = FPACK (MN)
          ENDDO
c        write(*,*) 'NREC=',NREC,' done'
         GOTO 33

c       write(*,*) 'STATION#', NST,' done!' 
3000    CONTINUE 

        write(*,*) 'READ FCST ', JHR, 'done !'
4000    CONTINUE 

 999    CONTINUE

        write(*,*) 'Write all of data into one big Bufr file ...'
C
C  WRITE OUT INDIVIDUAL FILES FOR EACH STATION
C
        IF (BRKOUT) THEN
        DO I=1,NSTAT
         NLVL(1)=LLMH(I)
         NLVL(2)=NSOIL
C
         DO J=1,NFCST
          DO IJ = 1, NWORD
            FRODAT(IJ) = PRODAT (IJ,I,J)
          ENDDO
          FRODAT(3) = LPRO(I,J)
          ISTAT=NINT(FRODAT(2))
  
C
          IF (J.EQ.1) THEN
C
C     INITIALIZE BUFR LISTS SO BFRHDR WILL BE CALLED THE FIRST
C     TIME THROUGH.
C
            WRITE(ASSIGN,FMTO) ISTAT,IYR,IMON,IDAY,IHRST,LCLAS1
            CALL SYSTEM(ASSIGN)
            CLIST1(1)=' '
          ENDIF
C
C           CALL BUFR-IZING ROUTINE
C
           NSEQ = 8
           SBSET = 'ETACLS1'
          CALL BFRIZE(LTBCL1,LCLAS1,SBSET,IYR,IMON,IDAY,IHRST
     1,               SEQNM1,SEQFLG,NSEQ,LVLWSE,FRODAT,NLVL,CLIST1,NP1
     2,               WORKK,IER)
          IF(IER.NE.0)WRITE(6,1080)ISTAT,IER,FRODAT(1)
 1080   FORMAT(' SOME SORT OF ERROR ',2I8,F9.1)
C
C
         ENDDO
C
C   FINISHED, CLOSE UP BUFR FILES
C
        NSEQ = 8
        CALL BFRIZE(0,LCLAS1,SBSET,IYR,IMON,IDAY,IHRST
     1,             SEQNM1,SEQFLG,NSEQ,LVLWSE,FRODAT,NLVL,CLIST1,NP1
     2,             WORKK,IER)
         ENDDO
         ENDIF

         IF (MONOL) THEN
C
C  WRITE OUT ONE FILE FOR ALL STATIONS
C
C     INITIALIZE BUFR LISTS SO BFRHDR WILL BE CALLED THE FIRST
C     TIME THROUGH.
C
       NSTAT_TRUE=(NREC-1)/NFCST

        write(0,*) 'computed NSTAT_TRUE as: ', NSTAT_TRUE

            CLIST1(1)=' '
C        DO I=1,NSTAT                            !loop for all stations
        DO I=1,NSTAT_TRUE                            !loop for all stations
         NLVL(1)=LLMH(I)
         NLVL(2)=NSOIL
C
         DO J=1,NFCST                             !loop for all fcst hours

          DO IJ = 1, NWORD
            FRODAT(IJ) = PRODAT (IJ,I,J)
          ENDDO

c          write(*,*) 'FRODAT =', (FRODAT (IJ),IJ=1,10)

          FRODAT(3) = LPRO(I,J)
          ISTAT=NINT(FRODAT(2))
C
C           CALL BUFR-IZING ROUTINE
C
           NSEQ = 8
           SBSET = 'ETACLS1'
          CALL BFRIZE(LTBCL1,LUNCL1,SBSET,IYR,IMON,IDAY,IHRST
     1,               SEQNM1,SEQFLG,NSEQ,LVLWSE,FRODAT,NLVL,CLIST1,NP1
     2,               WORKK,IER)
          IF(IER.NE.0)WRITE(6,1080)ISTAT,IER,FRODAT(1)
C
C
         ENDDO
         write(*,*) 'BFRIZE STATION ', I ,' done' 
         ENDDO
C
C   FINISHED, CLOSE UP BUFR FILES
C
        NSEQ = 8
        CALL BFRIZE(0,LUNCL1,SBSET,IYR,IMON,IDAY,IHRST
     1,             SEQNM1,SEQFLG,NSEQ,LVLWSE,FRODAT,NLVL,CLIST1,NP1
     2,             WORKK,IER)
         ENDIF
C
        WRITE(STDOUT,*) ' END OF SOUNDING POST '
        STOP
        END
