C-----------------------------------------------------------------------
      SUBROUTINE SPTRUNS(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,NPS,
     &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
     &                   NISKIP,NJSKIP,JCPU,TRUE,XMESH,ORIENT,
     &                   GRIDI,GN,GS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRUNS    SPECTRALLY INTERPOLATE SCALARS TO POLAR STEREO
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM SPECTRALLY TRUNCATES SCALAR FIELDS
C           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C           TO SPECIFIC PAIRS OF POLAR STEREOGRAPHIC SCALAR FIELDS.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE GRID FIELDS MAY HAVE GENERAL INDEXING.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRUNS(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,NPS,
C    &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
C    &                   NISKIP,NJSKIP,JCPU,TRUE,XMESH,ORIENT,
C    &                   GRIDI,GN,GS)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRTI    - INTEGER INPUT GRID IDENTIFIER
C                (IDRTI=4 FOR GAUSSIAN GRID,
C                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C     JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NPS      - INTEGER ODD ORDER OF THE POLAR STEREOGRAPHIC GRIDS
C     IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C     ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPI=0)
C     JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C     KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO NPS*NPS IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO NPS IF NJSKIP=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C     TRUE     - REAL LATITUDE AT WHICH PS GRID IS TRUE (USUALLY 60.)
C     XMESH    - REAL GRID LENGTH AT TRUE LATITUDE (M)
C     ORIENT   - REAL LONGITUDE AT BOTTOM OF NORTHERN PS GRID
C                (SOUTHERN PS GRID WILL HAVE OPPOSITE ORIENTATION.)
C     GRIDI    - REAL (*) INPUT GRID FIELDS
C   OUTPUT ARGUMENTS:
C     GN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC FIELDS
C     GS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC FIELDS
C
C SUBPROGRAMS CALLED:
C   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C   SPTGPS       TRANSFORM SPECTRAL SCALAR TO POLAR STEREO.
C   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      REAL GRIDI(*),GN(*),GS(*)
      REAL W((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM INPUT GRID TO WAVE
      JC=JCPU
      IF(JC.EQ.0) JC=NCPUS()
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MDIM=2*MX+1
      JN=-JSKIPI
      IF(JN.EQ.0) JN=IMAXI
      JS=-JN
      INP=(JMAXI-1)*MAX(0,-JN)+1
      ISP=(JMAXI-1)*MAX(0,-JS)+1
      CALL SPTRAN(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,
     &            IPRIME,ISKIPI,JN,JS,MDIM,KSKIPI,0,0,JC,
     &            W,GRIDI(INP),GRIDI(ISP),-1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM WAVE TO OUTPUT
      CALL SPTGPS(IROMB,MAXWV,KMAX,NPS,MDIM,KGSKIP,NISKIP,NJSKIP,
     &            TRUE,XMESH,ORIENT,W,GN,GS)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
