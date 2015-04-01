C-----------------------------------------------------------------------
      SUBROUTINE IPOLATEV(IP,IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
     &                    NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  IPOLATEV   IREDELL'S POLATE FOR VECTOR FIELDS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM INTERPOLATES VECTOR FIELDS
C           FROM ANY GRID TO ANY GRID (JOE IRWIN'S DREAM).
C           ONLY HORIZONTAL INTERPOLATION IS PERFORMED.
C           THE FOLLOWING INTERPOLATION METHODS ARE POSSIBLE:
C             (IP=0) BILINEAR
C             (IP=1) BICUBIC
C             (IP=2) NEIGHBOR
C             (IP=3) BUDGET
C             (IP=4) SPECTRAL
C             (IP=6) NEIGHBOR-BUDGET
C           SOME OF THESE METHODS HAVE INTERPOLATION OPTIONS AND/OR
C           RESTRICTIONS ON THE INPUT OR OUTPUT GRIDS, BOTH OF WHICH
C           ARE DOCUMENTED MORE FULLY IN THEIR RESPECTIVE SUBPROGRAMS.
C           THE GRIDS ARE DEFINED BY THEIR GRID DESCRIPTION SECTIONS
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63).
C           THE CURRENT CODE RECOGNIZES THE FOLLOWING PROJECTIONS:
C             (KGDS(1)=000) EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=001) MERCATOR CYLINDRICAL
C             (KGDS(1)=003) LAMBERT CONFORMAL CONICAL
C             (KGDS(1)=004) GAUSSIAN CYLINDRICAL
C             (KGDS(1)=005) POLAR STEREOGRAPHIC AZIMUTHAL
C             (KGDS(1)=201) ROTATED EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=202) ROTATED EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=203) ROTATED EQUIDISTANT CYLINDRICAL
C           WHERE KGDS COULD BE EITHER INPUT KGDSI OR OUTPUT KGDSO.
C           THE INPUT AND OUTPUT VECTORS ARE ROTATED SO THAT THEY ARE
C           EITHER RESOLVED RELATIVE TO THE DEFINED GRID
C           IN THE DIRECTION OF INCREASING X AND Y COORDINATES
C           OR RESOLVED RELATIVE TO EASTERLY AND NORTHERLY DIRECTIONS,
C           AS DESIGNATED BY THEIR RESPECTIVE GRID DESCRIPTION SECTIONS.
C           AS AN ADDED BONUS THE NUMBER OF OUTPUT GRID POINTS
C           AND THEIR LATITUDES AND LONGITUDES ARE ALSO RETURNED
C           ALONG WITH THEIR VECTOR ROTATION PARAMETERS.
C           ON THE OTHER HAND, THE OUTPUT CAN BE A SET OF STATION POINTS
C           IF KGDSO(1)<0, IN WHICH CASE THE NUMBER OF POINTS
C           AND THEIR LATITUDES AND LONGITUDES MUST BE INPUT 
C           ALONG WITH THEIR VECTOR ROTATION PARAMETERS.
C           NOTE: FOR THE BUDGET APPROACH, A SUBSECTION OF THE GRID MAY
C           BE OUTPUT BY SUBTRACTING KGDSO(1) FROM 255 AND PASSING
C           IN THE LATITUDES AND LONGITUDES OF THE POINTS.
C           INPUT BITMAPS WILL BE INTERPOLATED TO OUTPUT BITMAPS.
C           OUTPUT BITMAPS WILL ALSO BE CREATED WHEN THE OUTPUT GRID
C           EXTENDS OUTSIDE OF THE DOMAIN OF THE INPUT GRID.
C           THE OUTPUT FIELD IS SET TO 0 WHERE THE OUTPUT BITMAP IS OFF.
C        
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C 2003-06-23  IREDELL  STAGGERING FOR GRID TYPE 203
C
C USAGE:    CALL IPOLATEV(IP,IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
C    &                    NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C
C   INPUT ARGUMENT LIST:
C     IP       - INTEGER INTERPOLATION METHOD
C                (IP=0 FOR BILINEAR;
C                 IP=1 FOR BICUBIC;
C                 IP=2 FOR NEIGHBOR;
C                 IP=3 FOR BUDGET;
C                 IP=4 FOR SPECTRAL;
C                 IP=6 FOR NEIGHBOR-BUDGET)
C     IPOPT    - INTEGER (20) INTERPOLATION OPTIONS
C                (IP=0: (NO OPTIONS)
C                 IP=1: CONSTRAINT OPTION
C                 IP=2: (NO OPTIONS)
C                 IP=3: NUMBER IN RADIUS, RADIUS WEIGHTS ...
C                 IP=4: SPECTRAL SHAPE, SPECTRAL TRUNCATION
C                 IP=6: NUMBER IN RADIUS, RADIUS WEIGHTS ...)
C     KGDSI    - INTEGER (200) INPUT GDS PARAMETERS AS DECODED BY W3FI63
C                NOTE: IF KGDSI(1)=201 OR KGDSI(1)=203,
C                      THEN THE 9TH BIT OF KGDSI(11)
C                      IS TEMPORARILY SET TO 1 TO ALERT THE GDS WIZARD
C                      THAT THESE FIELDS ARE STAGGERED ETA WINDS.
C     KGDSO    - INTEGER (200) OUTPUT GDS PARAMETERS
C                NOTE: IF KGDSO(1)=201 OR KGDSO(1)=203,
C                      THEN THE 9TH BIT OF KGDSO(11)
C                      IS TEMPORARILY SET TO 1 TO ALERT THE GDS WIZARD
C                      THAT THESE FIELDS ARE STAGGERED ETA WINDS.
C     MI       - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF INPUT GRID FIELDS IF KM=1
C     MO       - INTEGER SKIP NUMBER BETWEEN OUTPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF OUTPUT GRID FIELDS IF KM=1
C     KM       - INTEGER NUMBER OF FIELDS TO INTERPOLATE
C     IBI      - INTEGER (KM) INPUT BITMAP FLAGS
C     LI       - LOGICAL*1 (MI,KM) INPUT BITMAPS (IF RESPECTIVE IBI(K)=1)
C     UI       - REAL (MI,KM) INPUT U-COMPONENT FIELDS TO INTERPOLATE
C     VI       - REAL (MI,KM) INPUT V-COMPONENT FIELDS TO INTERPOLATE
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (IF KGDSO(1)<0)
C     RLAT     - REAL (NO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)<0)
C     RLON     - REAL (NO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)<0)
C     CROT     - REAL (NO) VECTOR ROTATION COSINES (IF KGDSO(1)<0)
C     SROT     - REAL (NO) VECTOR ROTATION SINES (IF KGDSO(1)<0)
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C
C   OUTPUT ARGUMENT LIST:
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)>=0)
C     RLAT     - REAL (MO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)>=0)
C     RLON     - REAL (MO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)>=0)
C     CROT     - REAL (MO) VECTOR ROTATION COSINES (IF KGDSO(1)>=0)
C     SROT     - REAL (MO) VECTOR ROTATION SINES (IF KGDSO(1)>=0)
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C     IBO      - INTEGER (KM) OUTPUT BITMAP FLAGS
C     LO       - LOGICAL*1 (MO,KM) OUTPUT BITMAPS (ALWAYS OUTPUT)
C     UO       - REAL (MO,KM) OUTPUT U-COMPONENT FIELDS INTERPOLATED
C     VO       - REAL (MO,KM) OUTPUT V-COMPONENT FIELDS INTERPOLATED
C     IRET     - INTEGER RETURN CODE
C                0    SUCCESSFUL INTERPOLATION
C                1    UNRECOGNIZED INTERPOLATION METHOD
C                2    UNRECOGNIZED INPUT GRID OR NO GRID OVERLAP
C                3    UNRECOGNIZED OUTPUT GRID
C                1X   INVALID BICUBIC METHOD PARAMETERS
C                3X   INVALID BUDGET METHOD PARAMETERS
C                4X   INVALID SPECTRAL METHOD PARAMETERS
C
C SUBPROGRAMS CALLED:
C   POLATEV0     INTERPOLATE VECTOR FIELDS (BILINEAR)
C   POLATEV1     INTERPOLATE VECTOR FIELDS (BICUBIC)
C   POLATEV2     INTERPOLATE VECTOR FIELDS (NEIGHBOR)
C   POLATEV3     INTERPOLATE VECTOR FIELDS (BUDGET)
C   POLATEV4     INTERPOLATE VECTOR FIELDS (SPECTRAL)
C   POLATEV6     INTERPOLATE VECTOR FIELDS (NEIGHBOR-BUDGET)
C
C REMARKS: EXAMPLES DEMONSTRATING RELATIVE CPU COSTS.
C   THIS EXAMPLE IS INTERPOLATING 12 LEVELS OF WINDS
C   FROM THE 360 X 181 GLOBAL GRID (NCEP GRID 3)
C   TO THE 93 X 68 HAWAIIAN MERCATOR GRID (NCEP GRID 204).
C   THE EXAMPLE TIMES ARE FOR THE C90.  AS A REFERENCE, THE CP TIME
C   FOR UNPACKING THE GLOBAL 12 PAIRS OF WIND FIELDS IS 0.07 SECONDS.
C
C   BILINEAR    0                   0.05
C   BICUBIC     1   0               0.16
C   BICUBIC     1   1               0.17
C   NEIGHBOR    2                   0.02
C   BUDGET      3   -1,-1           0.94
C   SPECTRAL    4   0,40            0.31
C   SPECTRAL    4   1,40            0.33
C   SPECTRAL    4   0,-1            0.59
C   N-BUDGET    6   0,-1            0.31
C
C   THE SPECTRAL INTERPOLATION IS FAST FOR THE MERCATOR GRID.
C   HOWEVER, FOR SOME GRIDS THE SPECTRAL INTERPOLATION IS SLOW.
C   THE FOLLOWING EXAMPLE IS INTERPOLATING 12 LEVELS OF WINDS
C   FROM THE 360 X 181 GLOBAL GRID (NCEP GRID 3)
C   TO THE 93 X 65 CONUS LAMBERT CONFORMAL GRID (NCEP GRID 211).
C
C   METHOD      IP  IPOPT          CP SECONDS
C   --------    --  -------------  ----------
C   BILINEAR    0                   0.05
C   BICUBIC     1   0               0.15
C   BICUBIC     1   1               0.16
C   NEIGHBOR    2                   0.02
C   BUDGET      3   -1,-1           0.92
C   SPECTRAL    4   0,40            4.51
C   SPECTRAL    4   1,40            5.77
C   SPECTRAL    4   0,-1           12.60
C   N-BUDGET    6   0,-1            0.33
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      INTEGER IPOPT(20)
      INTEGER KGDSI(200),KGDSO(200)
      INTEGER IBI(KM),IBO(KM)
      LOGICAL*1 LI(MI,KM),LO(MO,KM)
      REAL UI(MI,KM),VI(MI,KM),UO(MO,KM),VO(MO,KM)
      REAL RLAT(MO),RLON(MO),CROT(MO),SROT(MO)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(KGDSI(1).EQ.201.OR.KGDSI(1).EQ.203) THEN
        KGDSI11=KGDSI(11)
        KGDSI(11)=IOR(KGDSI(11),256)
      ENDIF
      IF(KGDSO(1).EQ.201.OR.KGDSO(1).EQ.203) THEN
        KGDSO11=KGDSO(11)
        KGDSO(11)=IOR(KGDSO(11),256)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  BILINEAR INTERPOLATION
      IF(IP.EQ.0) THEN
        CALL POLATEV0(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
     &                NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  BICUBIC INTERPOLATION
      ELSEIF(IP.EQ.1) THEN
        CALL POLATEV1(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
     &                NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  NEIGHBOR INTERPOLATION
      ELSEIF(IP.EQ.2) THEN
        CALL POLATEV2(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
     &                NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  BUDGET INTERPOLATION
      ELSEIF(IP.EQ.3) THEN
        CALL POLATEV3(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
     &                NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SPECTRAL INTERPOLATION
c      ELSEIF(IP.EQ.4) THEN
c        CALL POLATEV4(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
c     &                NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  NEIGHBOR-BUDGET INTERPOLATION
C      ELSEIF(IP.EQ.6) THEN
C        CALL POLATEV6(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,UI,VI,
C     &                NO,RLAT,RLON,CROT,SROT,IBO,LO,UO,VO,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  UNRECOGNIZED INTERPOLATION METHOD
      ELSE
        IF(KGDSO(1).GE.0) NO=0
        DO K=1,KM
          IBO(K)=1
          DO N=1,NO
            LO(N,K)=.FALSE.
            UO(N,K)=0.
            VO(N,K)=0.
          ENDDO
        ENDDO
        IRET=1
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(KGDSI(1).EQ.201.OR.KGDSI(1).EQ.203) THEN
        KGDSI(11)=KGDSI11
      ENDIF
      IF(KGDSO(1).EQ.201.OR.KGDSO(1).EQ.203) THEN
        KGDSO(11)=KGDSO11
      ENDIF
      END
