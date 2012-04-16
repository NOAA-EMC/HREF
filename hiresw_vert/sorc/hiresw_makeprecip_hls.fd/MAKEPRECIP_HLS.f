       PROGRAM MAKEPRECIP150
C                .      .    .                                       .
C SUBPROGRAM:    MAKEPRECIP150 
C   PRGMMR: MANIKIN        ORG: W/NP22     DATE:  01-10-00
C
C ABSTRACT: PRODUCES 3-HOUR TOTAL AND CONVECTIVE PRECIPITATION BUCKETS
C             ON THE ETA 150 OUTPUT GRID FOR AWIPS 
C
C PROGRAM HISTORY LOG:
C   01-10-00  GEOFF MANIKIN 
C   05-07-28  ROGERS ADDED ACCUMULATED EVAPORATION
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-90
C   MACHINE:  CRAY C-90
C$$$
      INCLUDE "parmg"
      PARAMETER(ITOT=ILIM*JLIM)
      DIMENSION GRID(ITOT),DIFF(5)
      DIMENSION INCDAT(8),JNCDAT(8)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER LEVS(MAXLEV),IVAR(5)
      INTEGER NBITSS(5), NBITSS2(5), IBSS(5)
      LOGICAL*1 MASK(ITOT), MASK2(ITOT)
C
      PARAMETER(MBUF=2000000,JF=1000000)
      CHARACTER CBUF(MBUF)
      CHARACTER CBUF2(MBUF)
      CHARACTER*11 ENVVAR
      CHARACTER*80 FNAME
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MSK1=32000,MSK2=4000)
      DIMENSION APCP(ITOT), APCP2(ITOT),APCP3HR(ITOT),
     &     CAPCP(ITOT),CAPCP2(ITOT),CAPCP3HR(ITOT)
C
      READ (5,*) FHR2, FHR1
C      READ (12,*) ILIM, JLIM, MXLEV
      NUMLEV=MAXLEV
C
      LUGB=13
      LUGI=14
      LUGB2=15
      LUGI2=16
      LUGB3=50
      LUGB4=51
      LUGB5=52
      LUGB6=53
      LUGB7=54
C
      JJ1 = 1
      JJINC = 1
C
      ISTAT = 0
C
C  READ INDEX FILE TO GET GRID SPECS 
C
      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGB
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGB,FNAME,IRETGB)
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGI
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGI,FNAME,IRETGI)
      CALL GETGI(LUGI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
      write(6,*)' IRET FROM GETGI ',IRGI
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        RETURN
      ENDIF 
c      REWIND LUGI

C
      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        write(6,*)' IRET FROM GETGB1S ',IRGS
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          RETURN
        ENDIF 
C
      ENDDO

C    GET GRID NUMBER FROM PDS
C
      IGDNUM = KPDS(3)
C
C   PROCESS THE GRIB FILE
C
      IMAX = KGDS(2)
      JMAX = KGDS(3)
      NUMVAL = IMAX*JMAX
      KMAX = MAXLEV
      WRITE(6,280) IMAX,JMAX,NUMLEV,KMAX
  280 FORMAT(' IMAX,JMAX,NUMLEV,KMAX ',5I4)
  285 FORMAT(' IV, IVAR, L, IRET:  ',4I5)

C -== GET SURFACE FIELDS ==-
      L = 0
      IV= 0

C   PRECIP 

c   to start each new file with its index, set J=-1 for sfc pressure
C
      J = 0 
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 061
      JPDS(6) = 001
      JPDS(13) = 1
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,NBITSS(1),IBSS(1),IRET)
      IF(IRET.EQ.0) THEN
        II = 1
        JJ = JJ1
        DO KK = 1, ITOT
          APCP(KK) = GRID(KK) 
c          if (APCP(KK) .GT. 0.1) THEN
c            print *, 'PRECIP ', KK, APCP(KK)
c          endif 
        ENDDO
      ELSE
        WRITE(6,285)IV,JPDS(5),L,IRET
        WRITE(6,*)' COULD NOT UNPACK GRID 150 FILE ' 
         ISTAT = IRET
        RETURN
      ENDIF

C   PRECIP

c   to start each new file with its index, set J=-1 for sfc pressure
C
C  CONVECTIVE PRECIP
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 063
      JPDS(6) = 001
      JPDS(13) = 1
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,NBITSS(2),IBSS(2),IRET)
      IF(IRET.EQ.0) THEN
        II = 1
        JJ = JJ1
        DO KK = 1, ITOT
          CAPCP(KK) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,285)IV,JPDS(5),L,IRET
        WRITE(6,*)' COULD NOT UNPACK GRID 150 FILE '
         ISTAT = IRET
        RETURN
      ENDIF

C  PRINT VALUES AT POINT IN MIDDLE OF GRID
      write(6,1234)apcp(5500),capcp(5500)
1234  format(2(1x,e12.5))
      write(6,1235)nbitss(1),nbitss(2)
      write(6,1235)ibss(1),ibss(2)
1235  format(2i4)

C BEGIN WORK ON 2ND FILE
      JJ1 = 1
      JJINC = 1

C  READ INDEX FILE TO GET GRID SPECS
C
      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGB2
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGB2,FNAME,IRETGB2)
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGI2
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGI2,FNAME,IRETGI2)
      CALL GETGI(LUGI2,KSKIP,MBUF,CBUF2,NLEN,NNUM,IRGI)
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        RETURN
      ENDIF
c      REWIND LUGI2

      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1 
        JGDS = -1
        CALL GETGB1S(CBUF2,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        write(6,*)' IRET FROM GETGB1S ',IRGS
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          RETURN
        ENDIF
C
      ENDDO

C    GET GRID NUMBER FROM PDS
C
      IGDNUM = KPDS(3)
C
C   PROCESS THE GRIB FILE
C
      IMAX = KGDS(2)
      JMAX = KGDS(3)
      NUMVAL = IMAX*JMAX
      KMAX = MAXLEV
      WRITE(6,280) IMAX,JMAX,NUMLEV,KMAX

C -== GET SURFACE FIELDS ==-
      L = 0
      IV= 0
C   ACCUMULATED PRECIP 

c   to start each new file with its index, set J=-1 for sfc pressure
C
      J = -1
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 061
      JPDS(6) = 001
      JPDS(13) = 1
      CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,NBITSS2(1),IBSS(1),IRET)
      IF(IRET.EQ.0) THEN
        II = 1
        JJ = JJ1
        DO KK = 1, ITOT
          APCP2(KK) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,285)IV,JPDS(5),L,IRET
        WRITE(6,*)' COULD NOT UNPACK 2ND GRID 150 FILE '
         ISTAT = IRET
        RETURN
      ENDIF

C   ACCUMULATED CONVECTIVE PRECIP

c   to start each new file with its index, set J=-1 for sfc pressure
C
      J = -1
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 063
      JPDS(6) = 001
      JPDS(13) = 1
      CALL GETGB(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK2,GRID,NBITSS2(2),IBSS(2),IRET)
      IF(IRET.EQ.0) THEN
        II = 1
        JJ = JJ1
        DO KK = 1, ITOT
          CAPCP2(KK) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,285)IV,JPDS(5),L,IRET
        WRITE(6,*)' COULD NOT UNPACK 2ND GRID 150 FILE '
         ISTAT = IRET
        RETURN
      ENDIF

C  PRINT VALUES AT POINT IN MIDDLE OF GRID
      write(6,1234)apcp2(5500),capcp2(5500)
      write(6,1235)nbitss2(1),nbitss2(2)
      write(6,1235)ibss(1),ibss(2)

       DO K = 1, ITOT
          APCP3HR(K)=APCP2(K)-APCP(K)
          CAPCP3HR(K)=CAPCP2(K)-CAPCP(K)
       ENDDO

      KPDS(5)=61
      KPDS(14)=FHR1
      KPDS(15)=FHR2
      IBS=IBSS(1)
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGB3
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGB3,FNAME,IRET)
      print *,'IRET from BAOPEN on LUGB3 = ', IRET
      CALL PUTGBN(LUGB3,ITOT,KPDS,KGDS,IBS,NBITSS(1),MASK2,
     &   APCP3HR,IRET)
      print *,'IRET from PUTGB on LUGB3 = ', IRET
      CALL BACLOSE(LUGB3,IRET)

      KPDS(5)=63
      KPDS(14)=FHR1
      KPDS(15)=FHR2
      IBS=IBSS(2)
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') LUGB4
      CALL GETENV(ENVVAR,FNAME)
      CALL BAOPEN(LUGB4,FNAME,IRET)
      print *,'IRET from BAOPEN on LUGB4 = ', IRET
      CALL PUTGBN(LUGB4,ITOT,KPDS,KGDS,IBS,NBITSS(2),MASK2,
     &   CAPCP3HR,IRET)
      print *,'IRET from PUTGB on LUGB4 = ', IRET
      CALL BACLOSE(LUGB4,IRET)

      STOP
      END
