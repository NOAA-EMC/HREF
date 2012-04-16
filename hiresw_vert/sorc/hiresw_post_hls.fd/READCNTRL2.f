      SUBROUTINE READCNTRL2(IEOF)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    READCNTRL2  READS CONTROL FILE
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-20       
C     
C ABSTRACT:
C     THIS ROUTINE READS THE CONTROL FILE SPECIFYING OUTPUT
C     GRID(S), DATA FORMAT(S), AND FIELD(S) TO POST.  THE
C     ORDER OF OPERATIONS IS 
C        (1) READ HEADER BLOCK OF CONTROL FILE,
C        (2) SET FLAGS, CLOSE OPEN UNITS, SET E-GRID SPECS,
C        (3) READ OR COMPUTE INTERPOLATION WEIGHTS, AND
C        (4) READ BODY OF CONTROL FILE (FIELD SPECIFICATIONS)
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-20  RUSS TREADON
C   93-06-15  RUSS TREADON - ADD PROJECTION CONTROL CARD
C   98-06-01  BLACK - CONVERSION OF POST FROM 1-D TO 2-D
C   98-07-17  MIKE BALDWIN - REMOVED PACK84
C   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C     
C USAGE:    CALL READCNTRL2(IEOF)
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST: 
C     IEOF     - INTEGER FLAG FOR EOF IN CONTROL FILE.
C                IEOF=0 WHEN AN EOF IS READ IN THE
C                CONTROL FILE.  IEOF=1 OTHERWISE.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       TRNSEG   - SET E-GRID SPECIFCATIONS.
C       TRNSGD   - SET OUTPUT GRID SPECIFICATIONS.
C       E2TLL    - COMPUTE TRANSFORMED (LAT,LON) ON E-GRID.
C       FILLH    - FILL "H" POINTS FOR "V" POINT ARRAY.
C       FILLV    - FILL "V" POINTS FOR "H" POINT ARRAY.
C       GENLL    - COMPUTE OUTPUT GRID GEODETIC (LAT,LON).
C       GD2TLL   - MAP GEODETIC (LAT,LON) TO TRANSFORM
C                     ETA GRID REFERENCE FRAME.
C       GD2EG    - MAP OUTPUT GRID (I,J) TO FILLED E-GRID (I,J).
C       GD2EGK   - MAP OUTPUT GRID (I,J) TO E-GRID (K) FOR CETLIH4.
C       GENBIL   - COMPUTE BILINEAR INTERPOLATION WEIGHTS.
C       GENEUV   - LOAD ROTATION ARRAYS FOR WINDS.
C
C     LIBRARY:
C       COMMON   - OUTGRD
C                  RQSTFLD
C                  LLGRDS
C                  IOUNIT
C                  PHYS2
C                  OPTIONS
C                  MAPOT
C                  CTLBLK
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C     
C     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
C
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "parm.tbl"
      PARAMETER (IMJM=IM*JM-JM/2,IMT=2*IM-1,JMT=JM,LP1=LM+1)
      PARAMETER (DTR=1.745329E-2,RTD=1./DTR)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL NORTH
      CHARACTER*2  CHAR2
      CHARACTER*4  CHAR4
      CHARACTER*6  NUFILE,OUTYPE,PROJ,READCO,READLL
      CHARACTER*80 LINE
      REAL EGRID1(IM,JM), EGRID2(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "OUTGRD.comm"
      INCLUDE "RQSTFLD.comm"
      INCLUDE "LLGRDS.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "CTLBLK.comm"
C
C******************************************************************************
C     START READCNTRL HERE.
C     
      IFHR = NTSD/TSPH + 0.5
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)'READCNTRL2:  POSTING FCST HR ',IFHR,' FROM ',
     X       IHRST,'UTC ',IDAT(1),'-',IDAT(2),'-',IDAT(3),' RUN'
      ENDIF
C     
C     INITIALIZE VARIABLES.
C        IEOF IS THE END OF FILE FLAG FOR THE CONTROL FILE.
C        ARRAY IGET IS THE "GET FIELD" FLAG ARRAY.
C
      IEOF=0
      DO 100 IFLD=1,MXFLD
        IGET(IFLD)=-1
 100  CONTINUE
C
C     READ OUTPUT GRID SPECIFICATIONS.
      if(me.eq.0)print*,'start reading control file'
C
      READ(LCNTRL,1000,ERR=990,END=999) KGTYPE
      READ(LCNTRL,1000,ERR=990,END=999) IMDLTY
      READ(LCNTRL,1030,ERR=990,END=999) DATSET
      READ(LCNTRL,1030,ERR=990,END=999) OUTYPE
      READ(LCNTRL,1030,ERR=990,END=999) NUFILE
      READ(LCNTRL,1030,ERR=990,END=999) PROJ
      READ(LCNTRL,1010,ERR=990,END=999) NORTH
      READ(LCNTRL,1000,ERR=990,END=999) IGOUT
      READ(LCNTRL,1000,ERR=990,END=999) JGOUT
      READ(LCNTRL,1020,ERR=990,END=999) POLEI
      READ(LCNTRL,1020,ERR=990,END=999) POLEJ
      READ(LCNTRL,1020,ERR=990,END=999) ALATVT
      READ(LCNTRL,1020,ERR=990,END=999) ALONVT
      READ(LCNTRL,1020,ERR=990,END=999) XMESHL
      READ(LCNTRL,1030,ERR=990,END=999) READLL
      READ(LCNTRL,1030,ERR=990,END=999) READCO
 1000 FORMAT(T28,I5)
 1010 FORMAT(T28,L1)
 1020 FORMAT(T28,F11.6)
 1030 FORMAT(T28,A6)
C     
C     IF THE GRID TYPE (KGTYPE) IS NEITHER 0 NOR 99999 THEN THE
C     ONLY ALLOWED PROJECTIONS FOR THE OUTPUT GRID ARE POLAR 
C     STEREOGRAPHIC (POLA), LATITUDE-LONGITUDE (LOLA), AND LAMBERT
C     (TANGENT) CONFORMAL (LMBC).  IF THE REQUESTED PROJECTION 
C     DOES NOT SATISFY THESE CONDITIONS, TELL THE USER AND ASSUME
C     OUTPUT ON THE STAGGERED E-GRID.
C     
      IF ( (KGTYPE.LT.90).OR.(KGTYPE.GT.196) ) THEN
         IF (INDEX(PROJ,'POLA').NE.0) THEN
            WRITE(STDOUT,*)'READCNTRL2:  POLAR STEREOGRAPHIC ',
     X           'PROJECTION REQUESTED.  PROJ=',PROJ
         ELSEIF (INDEX(PROJ,'LOLA').NE.0) THEN
            WRITE(STDOUT,*)'READCNTRL2:  LATITUDE-LONGITUDE ',
     X           'PROJECTION REQUESTED.  PROJ=',PROJ
         ELSEIF (INDEX(PROJ,'LMBC').NE.0) THEN
            WRITE(STDOUT,*)'READCNTRL2:  LAMBERT (TANGENT) CONFORMAL ',
     X           'PROJECTION REQUESTED.  PROJ=',PROJ
         ELSE
            WRITE(STDOUT,*)
     X           'READCNTRL2:  PROJ=',PROJ,' IS NOT AVAILABLE.'
            WRITE(STDOUT,*)
     X           '             USING STAGGERED E-GRID AS DEFAULT'
C
CMEB  NEED TO KNOW WHICH VERSION OF THE MODEL IS RUNNING AT THIS POINT
C
            KGTYPE = 90
            PROJ   = 'ETA'
         ENDIF
      ENDIF
C     
C     00HR LFM LOOK-ALIKE FIELDS GO ON GRIDS 026 AND 005.
C     THE WAY THE CODE IS STRUCTURED WE MUST PROCESS GRIDS
C     026 AND 005 SEPARATELY.  TO UNIQUELY IDENTIFY THAT
C     THE USER ONLY WANTS THE 00HR LFM FIELDS ON GRID 005
C     DATSET MUST BE SET TO "ETA_AN".  IF THE CURRENT POST
C     TIME IS NOT 00, SKIP OUTPUT ON GRID 005.
C     
      IF ((KGTYPE.EQ.005).AND.(INDEX(DATSET,'ETA_AN').NE.0)) THEN
         IFHR = NTSD/TSPH+0.50
         IF (IFHR.NE.0) GOTO 999
      ENDIF
C     
C     SET FLAG FOR TYPE OF OUTPUT.
C     
      IOUTYP=0
      IF (INDEX(OUTYPE,'NO'    ).NE.0) IOUTYP=1
      IF (INDEX(OUTYPE,'GRIBIT').NE.0) IOUTYP=3
      IF (INDEX(OUTYPE,'GRIBRK').NE.0) IOUTYP=5
      IF (IOUTYP.EQ.0) THEN
         WRITE(STDOUT,*)'OUTPUT TYPE ',
     X        OUTYPE,'NOT SUPPORTED.'
         IOUTYP=1
         WRITE(STDOUT,*)'USING MACHINE BINARY AS DEFAULT.  ',
     X        'IOUTYP=',IOUTYP
      ENDIF
C     
C     IF NEW OUTPUT FILE IS SPECIFIED
C       1.  CLOSE THE PREVIOUS OUTPUT FILE UNIT,
C       2.  SET FLAG TO OPEN NEW OUTPUT FILE
C
      RITEHD = .FALSE.
      RITE2  = .FALSE.
      IF (INDEX(NUFILE,'YES').NE.0) THEN
         CLOSE(LUNOUT)
         CLOSE(LUNOUT+1)
         CLOSE(LUNOUT+2)
      IF(ME.EQ.0)THEN
         WRITE(STDOUT,*)' READCNTRL2:  JUST CLOSED UNITS ',LUNOUT,
     X        LUNOUT+1,LUNOUT+2
      ENDIF
         RITEHD = .TRUE.
         RITE2  = .TRUE.
      IF(ME.EQ.0)THEN
         WRITE(STDOUT,*)
     X        ' READCNTRL2:  NEXT UNIT(S) OPENED WILL BE ',LUNOUT,
     X        LUNOUT+1,LUNOUT+2
      ENDIF
      ENDIF
C     
C     INCREMENT UNIT NUMBERS FOR WEIGHTS AND LATLON GRID.
C     SET FLAG TO WRITE HEADER TO LEAD OFF OUTPUT FILE.
      IF (INDEX(READCO,'NO').EQ.0) THEN
         LUNCO  = LUNCO + 1
         REWIND(LUNCO)
      ENDIF
      IF (INDEX(READLL,'NO').EQ.0) THEN
         LUNLL  = LUNLL + 1
         REWIND(LUNLL)
      ENDIF
C     
C     LOAD E-GRID AND OUTPUT GRID COMMON BLOCKS.
      CALL TRNSEG
      IF ( (KGTYPE.LT.90).OR.(KGTYPE.GT.196) ) CALL TRNSGD
C     
C     IF THE USER WANTS TO READ IN PRECOMPUTED WEIGHTS, DO SO.
C     
      IF(INDEX(READCO,'NO').EQ.0)THEN
        IF(ME.EQ.0)THEN
          WRITE(STDOUT,*)'READ PRECOMPUTED WEIGHTS'
        ENDIF
        READ(LUNCO,ERR=992) ((GDLAT(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((GDLON(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((GDTLAT(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((GDTLON(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((HTLAT(I,J),I=1,IM),J=1,JM)
        READ(LUNCO,ERR=992) ((HTLON(I,J),I=1,IM),J=1,JM)
        READ(LUNCO,ERR=992) ((IEGRDK(I,J),I=1,IM),J=1,JM)
        READ(LUNCO,ERR=992) ((JEGRDK(I,J),I=1,IM),J=1,JM)
        READ(LUNCO,ERR=992) ((VTLAT(I,J),I=1,IM),J=1,JM)
        READ(LUNCO,ERR=992) ((VTLON(I,J),I=1,IM),J=1,JM)
        READ(LUNCO,ERR=992) ((FVTLON(I,J),I=1,IMT),J=1,JMT)
        READ(LUNCO,ERR=992) ((EVLAT(I,J),I=1,IMT),J=1,JMT)
        READ(LUNCO,ERR=992) ((EVLON(I,J),I=1,IMT),J=1,JMT)
        READ(LUNCO,ERR=992) ((EGRDI(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((EGRDJ(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((IEGRD(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((JEGRD(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((IWGT(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((WIJ(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((WIPJ(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((WIJP(I,J),I=1,IGOUT),J=1,JGOUT)
        READ(LUNCO,ERR=992) ((WIPJP(I,J),I=1,IGOUT),J=1,JGOUT)
C     
C     OTHERWISE, WE MUST COMPUTE THE WEIGHTS AS WE RUN THE
C     PROGRAM.  CHECK TO SEE IF A USER DEFINED (LAT,LON)
C     GRID EXITS.  IF SO, READ IT.  IF NOT, USE THE GRID
C     SPECIFICATIONS TO GENERATE (LAT,LON) FOR THE OUTPUT
C     GRID.  IN EITHER CASE, TRANSFORM GEODETIC (LAT,LON)
C     OF THE OUTPUT GRID TO THE ETA REFERENCE FRAME.
C     
      ELSE
C     
C        SET UP E-GRID (LAT,LON) ARRAYS.
         IF(ME.EQ.0)THEN
           WRITE(STDOUT,*)'SET UP E-GRID (LAT,LON) ARRAYS'
         ENDIF
         CALL E2TLL(HTLAT,HTLON,VTLAT,VTLON)
         CALL FILLH(VTLON,FVTLON,IMT,JMT)
C     
C        READ IN USER (LAT,LON) GRID IF ONE EXISTS.
         IF (INDEX(READLL,'NO').EQ.0) THEN
            IF(ME.EQ.0)THEN
              WRITE(STDOUT,*)'READ USER OUTPUT GRID (LAT,LON)'
            ENDIF
            READ(LUNLL,ERR=994) ((GDLAT(I,J),I=1,IGOUT),J=1,JGOUT)
            READ(LUNLL,ERR=994) ((GDLON(I,J),I=1,IGOUT),J=1,JGOUT)
C           READ(LUNLL,1050,ERR=994) (((GDLAT(I,J),GDLON(I,J)),
C    X           I=1,IGOUT),J=1,JGOUT)
C1050       FORMAT(3(2(F11.6,1X),1X))
C     
C        OTHERWISE GENERATE OUTPUT GRID (LAT,LON)
C        WE DON'T NEED THE WEIGHTS IF OUTPUT GRID
C        IS A FILLED E-GRID (KGTYPE=0).  HOWEVER,
C        THE USER MAY REQUEST FILLED E-GRID (LAT,
C        LON), SO COMPUTE THESE FIELDS.
C
         ELSE
            IF ( (KGTYPE.LT.90).OR.(KGTYPE.GT.196) ) THEN
               IF(ME.EQ.0)THEN
                 WRITE(STDOUT,*)'GENERATE OUTPUT GRID (LAT,LON)'
               ENDIF
               CALL GENLL(GDLAT,GDLON)
            ELSE
               IF(ME.EQ.0)THEN
                 WRITE(STDOUT,*)'COMPUTE E-GRID GEO-(LAT,LON)'
               ENDIF
               DO J=1,JM
               DO I=1,IM
                 EGRID1(I,J) = GLAT(I,J)*RTD
                 EGRID2(I,J) = GLON(I,J)*RTD
               ENDDO
               ENDDO
C
C ROGERS  1/26/01: Not needed since we don't output filled e-grids
C
c              IF (MOD(KGTYPE,2).EQ.1.AND.KGTYPE.NE.99) THEN
c                 IGOUT = IMT
c                 JGOUT = JMT
c                 CALL FILLV(EGRID1,GDLAT,IFLAG,IMT,JMT)
c                 CALL FILLV(EGRID2,GDLON,IFLAG,IMT,JMT)
c              ELSE
                  DO J=1,JM
                  DO I=1,IM
                    GDLAT(I,J)=EGRID1(I,J)
                    GDLON(I,J)=EGRID2(I,J)
                  ENDDO
                  ENDDO
c              ENDIF
            ENDIF
         ENDIF
C     
C        COMPUTE INTERPOLATION WEIGHTS.  WE DON'T NEED TO
C        DO THIS FOR KGTYPE=0.  HOWEVER, WE DO NEED GEODETIC
C        (LAT,LON) FOR FILLED E-EGRID IF KGTYPE=0
C
         IF ( (KGTYPE.LT.90).OR.(KGTYPE.GT.196) ) THEN
            CALL GD2TLL(GDLAT,GDLON,GDTLAT,GDTLON,IGOUT,JGOUT)
            CALL GD2EG(IGOUT,JGOUT)
            CALL GD2EGK(IGOUT,JGOUT)
            CALL GENBIL(FVTLON,IGOUT,JGOUT)
            CALL GENEUV(EVLAT,EVLON)
         ENDIF
      ENDIF
C     
C     ECHO GRID SPECIFICATIONS TO STDOUT.
C
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)'READCNTRL:  OUTPUT GRID SPECIFICATIONS'
     1,                ' AND SETUP'
        WRITE(STDOUT,*)' KGTYPE       :  ',KGTYPE
        WRITE(STDOUT,*)' IGOUT,JGOUT  :  ',IGOUT,JGOUT
        WRITE(STDOUT,*)' DATSET,OUTYPE:  ',DATSET,' ',OUTYPE
        WRITE(STDOUT,*)' NUFILE,RITEHD:  ',NUFILE,RITEHD,' ',RITE2
        WRITE(STDOUT,*)' READLL,READCO:  ',READLL,' ',READCO
        WRITE(STDOUT,*)' IOUTYP       :  ',IOUTYP
        WRITE(STDOUT,*)' LUNCO,LL,OUT :  ',LUNCO,LUNLL,LUNOUT
      ENDIF
C     
C     ALL THE GRID STUFF IS DONE.  NOW READ WHICH FIELDS ON 
C     WHICH LEVELS TO INTERPOLATE TO THE OUTPUT GRID.  THE
C     CHARACTER STRING "DONE" MARKS THE END OF THE OUTPUT
C     FIELD SPECIFICATIONS.
C
      IFLD = 0
 10   CONTINUE
         READ(LCNTRL,1060,ERR=996) LINE
         IF (INDEX(LINE,'DONE').NE.0) GOTO 40
         IF (INDEX(LINE,'SCAL=').EQ.0)   GOTO 10
         IFLD        = IFLD+1
         FIELD(IFLD) = LINE(3:22)
         CHAR2       = LINE(64:65)
         CALL CHR2INT(CHAR2,2,ISMSTG(IFLD))
         CHAR2       = LINE(67:68)
         CALL CHR2INT(CHAR2,2,ISMFUL(IFLD))
         CHAR2       = LINE(70:71)
         CALL CHR2INT(CHAR2,2,ISMOUT(IFLD))
         READ(LINE,1061) DEC(IFLD)
         READ(LCNTRL,1090,ERR=996) (LVLS(L,IFLD),L=1,MXLVL)
 1060    FORMAT(A80)
 1061    FORMAT(50X,F4.1)
 1070    FORMAT(A4)
 1080    FORMAT(A2)
 1090    FORMAT(T5,12(5I1,1X))
C     
C        SEE IF WE WANT THIS FIELD.  THE SUM OF THE LEVELS
C        INDICATORS MUST BE GREATER THAN ZERO IF WE WANT 
C        THIS FIELD.
C     
         ISUM = 0
         DO 15 L = 1,MXLVL
            ISUM = ISUM + LVLS(L,IFLD)
 15      CONTINUE
         IF (ISUM.LT.1) THEN
            IFLD = IFLD - 1
            GOTO 10
         ENDIF
C     
C        SEE IF REQUESTED FIELD IS AVAILABLE.  IF NOT, 
C        WRITE MESSAGE TO STDOUT AND DECREMENT FIELD 
C        COUNTER BY ONE.  THEN READ NEXT REQUESTED FIELD.
C     
         DO 20 IAVBL = 1,MXFLD
            IF (INDEX(FIELD(IFLD),AVBL(IAVBL)).NE.0)GO TO 30
 20      CONTINUE
         IF(ME.EQ.0)THEN
           WRITE(STDOUT,*)'FIELD ',FIELD(IFLD),' NOT AVAILABLE'
         ENDIF
         IFLD = IFLD-1
         GOTO 10
C     
C        IF FIELD IS AVAILABLE, TURN THE GET SWITCH ON.
C     
 30      CONTINUE
         IGET(IAVBL) = IFLD
         IDENT(IFLD) = IAVBL
         GOTO 10
C     
C     ALL DONE READING REQUESTED FIELDS FOR CURRENT OUTPUT GRID.
C     SET NFLD TO TOTAL NUMBER OF REQUESTED OUTPUT FIELDS THAT 
C     ARE AVAILABLE.
C
 40   CONTINUE
      NFLD = IFLD
C     
C     ECHO OUTPUT FIELDS/LEVELS TO STDOUT.
C
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)'BELOW ARE FIELD/LEVEL/SMOOTHING ',
     X       'SPECIFICATIONS.'
      ENDIF
      DO 50 IFLD = 1,NFLD
        IF(ME.EQ.0)THEN
         WRITE(STDOUT,2060) FIELD(IFLD),IQ(IDENT(IFLD)),
     X        IS(IDENT(IFLD)),ISMSTG(IFLD),ISMFUL(IFLD),ISMOUT(IFLD)
         WRITE(STDOUT,2070) (LVLS(L,IFLD),L=1,MXLVL)
 2060    FORMAT('(',A20,') Q=(',I4,'), S=(',I4,
     X        '), SMTH=(',I2,1X,I2,1X,I2,')')
 2070    FORMAT('L=(',12(5I1,1X),')')
        ENDIF
 50   CONTINUE
C     
C     WE HAVE AN OUTPUT GRID AND THE FIELDS TO GENERATE ON IT.
C     SKIP OVER THE FOLLOWING EOF MESSAGE TO EXIT THIS ROUTINE.
C     
      GOTO 60
C     
C     WE REACH THIS BLOCK ONLY IF THERE IS AN ERROR WHILE READING
C     IN THE CONTROL FILE.  PRINT AN ERROR MESSAGE TO STANDARD
C     OUT AND CARRY ON.
C     
 990  CONTINUE
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)' READCNTRL2:  ERROR READING CNTRL GRID INFO'
        WRITE(STDOUT,*)' BELOW IS CNTRL GRID INFO'
        WRITE(STDOUT,*)'  KGTYPE,DATSET:  ',KGTYPE,' ',DATSET
        WRITE(STDOUT,*)'  OUTYPE,NUFILE:  ',OUTYPE,' ',NUFILE
        WRITE(STDOUT,*)'  PROJ         :  ',PROJ
        WRITE(STDOUT,*)'  NORTH        :  ',NORTH
        WRITE(STDOUT,*)'  IGOUT,JGOUT  :  ',IGOUT,JGOUT
        WRITE(STDOUT,*)'  POLEI,POLEJ  :  ',POLEI,POLEJ
        WRITE(STDOUT,*)'  ALATVT,ALONVT:  ',ALATVT,ALONVT
        WRITE(STDOUT,*)'  XMESHL       :  ',XMESHL
        WRITE(STDOUT,*)'  READLL,READCO:  ',READLL,READCO
      ENDIF
      GOTO 999
 992  CONTINUE
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)' READCNTRL2:  ERROR READING INTERP WEIGHTS'
      ENDIF
      GOTO 999
 994  CONTINUE
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)' READCNTRL2:  ERROR READING LATLON GRID'
      ENDIF
      GOTO 999
 996  CONTINUE
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)' READCNTRL2:  ERROR READING CNTRL FLD/LVL INFO'
      ENDIF
C     
C     WE REACH THIS BLOCK ONLY WHEN AN EOF HAS BEEN READ FROM 
C     THE CONTROL FILE.  THAT MEANS WE'VE PROCESSED ALL GRIDS
C     AND ALL FIELDS.  WE'RE DONE.  SET THE EOF FLAG TO ANY
C     NONZERO INTEGER, SAY ONE.  CLOSE THE UNIT CONNECTED TO
C     THE LAST OUTPUT FILE AND EXIT THE ROUTINE.
C     
 999  CONTINUE
      IEOF=1
      CLOSE(LUNOUT)
      CLOSE(LUNOUT+1)
      CLOSE(LUNOUT+2)
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)' READCNTRL2:  ALL GRIDS PROCESSED.  ',
     X       'CLOSED ',LUNOUT
      ENDIF
C     
C     END OF ROUTINE.
C     
 60   CONTINUE
      RETURN
      END
