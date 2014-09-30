       PROGRAM SMARTPRECP
!                .      .    .                                       .
! SUBPROGRAM:    SMARTPECIP
!   PRGMMR: MANIKIN        ORG: W/NP22     DATE:  07-03-07

! ABSTRACT: PRODUCES 3,6 or 12-HOUR TOTAL AND CONVECTIVE PRECIPITATION BUCKETS
!              AS WELL AS SNOWFALL ON THE ETA NATIVE GRID FOR SMARTINIT 

! PROGRAM HISTORY LOG:
!   07-03-07  GEOFF MANIKIN 
!   10-25-12  JEFF MCQUEEN
! REMARKS:
!   10-25-12 JTM UNIFIED make and add precip for different accum hours
!                addprecip6, addprecip12 and makeprecip all combined in
!                smartprecip
!                To call, must set all 4 fhrs
!                for 3 or 6 hour buckets, set fhr3,fh4 to -99
!                For 12 hour buckets: 
!                    smartprecip  fhr fhr-3 fhr-6 fhr-9 
! ATTRIBUTES:
!   LANGUAGE: FORTRAN-90
!   MACHINE:  WCOSS     
!======================================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER FHR0,FHR1, FHR2, FHR3, FHR4, IARW
      CHARACTER*80 FNAME
      LOGICAL*1 LSUB

      REAL,     ALLOCATABLE :: GRID(:)
      REAL,     ALLOCATABLE :: APCP1(:),APCP2(:),APCP3(:),APCP4(:)
      REAL,     ALLOCATABLE :: CAPCP1(:),CAPCP2(:),CAPCP3(:),CAPCP4(:)
      REAL,     ALLOCATABLE :: SNOW1(:),SNOW2(:),SNOW3(:),SNOW4(:)
      REAL,     ALLOCATABLE :: APCPOUT(:),CAPCPOUT(:),SNOWOUT(:)
      LOGICAL,  ALLOCATABLE :: MASK(:)
!--------------------------------------------------------------------------
      INTERFACE
      SUBROUTINE RDHDRS(LUB,LUI,JPDS,JGDS,IGDN,IMAX,JMAX,KMAX,NUMV)
      INTEGER,  INTENT(IN)     :: lub,lui                  ! unit numbers
      INTEGER,  INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,  INTENT(OUT)    :: igdn,imax,jmax,kmax,numv ! grid size
      END SUBROUTINE rdhdrs
!--------------------------------------------------------------------------
      SUBROUTINE SETVAR(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
      INTEGER,  INTENT(IN)     :: lub,lui,numv             ! unit numbers
      INTEGER,  INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,  INTENT(OUT)    :: k,kf,kpds(200),kgds(200) ! grid info
      INTEGER,  INTENT(OUT)    :: iret,istat               ! grid info
      LOGICAL,  INTENT(INOUT)  :: MASK(:)                ! L/S mask 
      REAL,     INTENT(INOUT)  :: GRID(:)                  ! grib data
      REAL,     INTENT(OUT)    :: VARB(:)                  ! output varb
      END SUBROUTINE setvar
      END INTERFACE
!--------------------------------------------------------------------------

      FNAME='fort.  '
!====================================================================
!     FHR3 = -99 signals a 6 hour summation requested
!     FHR4 GT 00 signals a 12 hour summation requested
!     FHR1 GT FHR2 signals do a 3 hour subtraction of files
      READ (5,*) FHR1, FHR2,FHR3,FHR4, IARW
!====================================================================

!==>  Make 3 hour buckets by subtracting fhr3 - fhr files
      LSUB=.FALSE.

      IF (FHR1.GT.FHR2) THEN
       FHR0=FHR2
       FHR2=FHR1
       FHR1=FHR0
       LSUB=.TRUE.
        write(0,*) 'reset so FHR0, FHR1, FHR2 are: ', FHR0, FHR1, FHR2
       IF (MOD(FHR2,12).EQ. 0.) THEN
         FHR3=FHR2-12
        write(0,*) 'FHR3 defined(a): ', FHR3
        ELSE
         FHR3=FHR2-MOD(FHR2,12)
        write(0,*) 'FHR3 defined(b): ', FHR3
       ENDIF
      ELSE

!==>  sum up precip files
        if (fhr3 .lt. 0) FHR3=FHR1-3

!==>    make 12 hr precip for NMMB
        if (fhr4 .ge. 0) then 
                FHR0=FHR1-3
        write(0,*) 'making 12 h precip'
        endif
      ENDIF

!       print *, 'fhr0,fhr1 fhr2 fhr3 fhr4 ',FHR0, FHR1, FHR2, FHR3,FHR4

      LUGB=13;LUGI=14; LUGB2=15;LUGI2=16
      LUGB3=17;LUGI3=18;LUGB4=19;LUGI4=20
      LUGB5=50; LUGB6=51; LUGB7=52

      ISTAT = 0

!=======================================================
!  READ INDEX FILE TO GET GRID SPECS 
!=======================================================
      CALL RDHDRS(LUGB,LUGI,JPDS,JGDS,IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

! -== GET SURFACE FIELDS ==-

      ALLOCATE (MASK(NUMVAL),GRID(NUMVAL),STAT=kret)
      ALLOCATE (APCP1(NUMVAL),CAPCP1(NUMVAL),SNOW1(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!   PRECIP 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
        write(0,*) 'read from LUGB: ', LUGB
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,      &
                K,KPDS,KGDS,MASK,GRID,APCP1,IRET,ISTAT)

!  CONVECTIVE PRECIP
!      J = 0;JPDS = -1;JPDS(3) = IGDNUM
!      JPDS(5) = 063;JPDS(6) = 001
!      JPDS(13) = 1
!      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,       &
!                 K,KPDS,KGDS,MASK,GRID,CAPCP1,IRET,ISTAT)
!
!  SNOWFALL 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001

        if (IARW .eq. 0) then

      JPDS(14) = FHR3
      JPDS(15) = FHR1
        write(0,*) 'FHR0: ', FHR0
      if (fhr4.gt.0) JPDS(14)=FHR0
        write(0,*) 'JPDS(14) for snow now: ', JPDS(14)

        endif
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,      &
                 K,KPDS,KGDS,MASK,GRID,SNOW1,IRET,ISTAT)

!=======================================================
!  READ INDEX FILE TO GET GRID SPECS for 2nd file
!=======================================================
      CALL RDHDRS(LUGB2,LUGI2,JPDS,JGDS,IGDNUM,IMAX,JMAX,KMAX,NUMVAL)
      JGDS=-1

      ALLOCATE (APCP2(NUMVAL),CAPCP2(NUMVAL),SNOW2(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!     ACCUMULATED PRECIP 
      J = -1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1 
        write(0,*) 'read from LUGB2: ', LUGB2
      CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,     &
                 K,KPDS,KGDS,MASK,GRID,APCP2,IRET,ISTAT)

!     ACCUMULATED CONVECTIVE PRECIP
!      J = 0;JPDS = -1;JPDS(3) = IGDNUM
!      JPDS(5) = 063;JPDS(6) = 001
!      JPDS(13) = 1
!      CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,     &
!                 K,KPDS,KGDS,MASK,GRID,CAPCP2,IRET,ISTAT)
!
!     SNOWFALL
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
        if (IARW .eq. 0) then
      JPDS(14) = FHR1
      JPDS(15) = FHR2
      IF (LSUB) JPDS(14)=FHR3
        endif
      CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,     &
                 K,KPDS,KGDS,MASK,GRID,SNOW2,IRET,ISTAT)

      IF (FHR4.GT.0 ) THEN

!=======================================================
!  READ INDEX FILE TO GET GRID SPECS for 3rd file
!=======================================================
      CALL RDHDRS(LUGB3,LUGI3,JPDS,JGDS,              &
                  IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

      ALLOCATE (APCP3(NUMVAL),CAPCP3(NUMVAL),SNOW3(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF
!     ACCUMULATED PRECIP 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,      &
                 K,KPDS,KGDS,MASK,GRID,APCP3,IRET,ISTAT)

!     ACCUMULATED CONVECTIVE PRECIP
!      J = 0;JPDS = -1;JPDS(3) = IGDNUM
!      JPDS(5) = 063;JPDS(6) = 001
!      JPDS(13) = 1
!      CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,      &
!                 K,KPDS,KGDS,MASK,GRID,CAPCP3,IRET,ISTAT)

!     SNOWFALL
      J = 0 ;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
        if (IARW .eq. 0) then
        JPDS(14) = FHR2
        JPDS(15) = FHR3
        endif
      CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,      &
                 K,KPDS,KGDS,MASK,GRID,SNOW3,IRET,ISTAT)

!=======================================================
!  READ INDEX FILE TO GET GRID SPECS for 4th file
!=======================================================
      CALL RDHDRS(LUGB4,LUGI4,JPDS,JGDS,                  &
                  IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

      ALLOCATE (APCP4(NUMVAL),CAPCP4(NUMVAL),SNOW4(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF
!     ACCUMULATED PRECIP 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,     &
                 K,KPDS,KGDS,MASK,GRID,APCP4,IRET,ISTAT)

!     ACCUMULATED CONVECTIVE PRECIP
!      J = 0;JPDS = -1;JPDS(3) = IGDNUM
!      JPDS(5) = 063;JPDS(6) = 001
!      JPDS(13) = 1
!      CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,       &
!                 K,KPDS,KGDS,MASK,GRID,CAPCP4,IRET,ISTAT)
!
!     SNOWFALL
      J = 0 ;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
        if (IARW .eq. 0) then
        JPDS(14) = FHR3
        JPDS(15) = FHR4
        endif
      CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,      &
                 K,KPDS,KGDS,MASK,GRID,SNOW4,IRET,ISTAT)

      ENDIF 

!=======================================================
!      OUTPUT 3, 6 or 12 hr PRECIP BUCKETS
!=======================================================
      ALLOCATE (APCPOUT(NUMVAL),CAPCPOUT(NUMVAL),SNOWOUT(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

      IF (LSUB) THEN
       APCPOUT=APCP2-APCP1
!       CAPCPOUT=CAPCP2-CAPCP1
       SNOWOUT=SNOW2-SNOW1
      ELSE
       APCPOUT=APCP2+APCP1
!       CAPCPOUT=CAPCP2+CAPCP1
       SNOWOUT=SNOW2+SNOW1
 
       IF (FHR4 .GT.0 )THEN
          APCPOUT=APCPOUT+APCP3+APCP4
!          CAPCPOUT=CAPCPOUT+CAPCP3+CAPCP4
          SNOWOUT=SNOWOUT+SNOW3+SNOW4
       ENDIF
      ENDIF

      KPDS(14)=FHR3
      KPDS(15)=FHR2
      IF (LSUB) KPDS(14)=FHR1
      IF (FHR4.GT.0)THEN
        KPDS(14)=FHR0
        KPDS(15)=FHR4
      ENDIF

      KPDS(5)=61
      print *, 'writing precip', KPDS(5),KPDS(14),KPDS(15),LUGB5,MAXVAL(APCPOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB5
      CALL BAOPEN(LUGB5,FNAME,IRETGB)
      CALL PUTGB(LUGB5,NUMVAL,KPDS,KGDS,MASK,APCPOUT,IRET)
      CALL BACLOSE(LUGB5,IRET)

!      KPDS(5)=63
!      print *, 'writing CAPCP', KPDS(5),KPDS(14),KPDS(15),LUGB6 , MAXVAL(CAPCPOUT)
!      WRITE(FNAME(6:7),FMT='(I2)')LUGB6
!      CALL BAOPEN(LUGB6,FNAME,IRET)
!      CALL PUTGB(LUGB6,NUMVAL,KPDS,KGDS,MASK,CAPCPOUT,IRET)
!      CALL BACLOSE(LUGB6,IRET)

      KPDS(5)=65
      print *, 'writing SNOW', KPDS(5),KPDS(14),KPDS(15),LUGB7, MAXVAL(SNOWOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB7
      CALL BAOPEN(LUGB7,FNAME,IRET)
      CALL PUTGB(LUGB7,NUMVAL,KPDS,KGDS,MASK,SNOWOUT,IRET)
      CALL BACLOSE(LUGB7,IRET)

      STOP
      END

      SUBROUTINE SETVAR(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
!============================================================================
!     This Routine reads in a grib field and initializes a 2-D variable
!     Requested from w3lib GETGRB routine
!     10-2012   Jeff McQueen
!     NOTE: ONLY WORKS for REAL Type Variables
!============================================================================
      INTEGER,  INTENT(IN)     :: lub,lui,numv             ! unit numbers
      INTEGER,  INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,  INTENT(OUT)    :: k,kf,kpds(200),kgds(200) ! grid info
      INTEGER,  INTENT(OUT)    :: iret,istat               ! grid info
      LOGICAL,  INTENT(INOUT) :: MASK(:)                  ! L/S mask 
      REAL,     INTENT(INOUT)  :: GRID(:)                  ! grib data
      REAL,     INTENT(OUT)    :: VARB(:)                  ! output varb

!     Get GRIB Variable
      CALL GETGB(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, NUMV
          VARB(KK) = GRID(KK)
        ENDDO
        WRITE(6,100) JPDS(5),JPDS(6),JPDS(7),J,MAXVAL(VARB),KF,K
 100    FORMAT('VARB UNPACKED ', 4I7,G12.4,I8, ' RECORD',I5)
      ELSE
        WRITE(0,*)'===================================================='
        WRITE(0,*)'SETVAR : COULD NOT UNPACK VARB',JPDS(5),JPDS(6)
        WRITE(0,*)' J ',J,' GRID ',JPDS(3),'GETGB RETURN CODE',IRET
        WRITE(0,*)'UNIT', LUB,LUI,'NUMVAL ', NUMV,KF,' RECORD',K
        WRITE(0,*)'===================================================='
        WRITE(0,*) 'JPDS ', JPDS(1:25)
!        print *, 'JGDS', JGDS(1:25)
        ISTAT = IRET
        ERR=IRET
        STOP 'UNIFPRECIP ABORT:   VARB NOT UNPACKED'
      ENDIF

      RETURN
      END

      SUBROUTINE RDHDRS(LUB,LUI,JPDS,JGDS,IGDN,IMAX,JMAX,KMAX,NUMV)
!=============================================================================
!     This Routine Reads GRIB index file and returns its contents
!     (GETGI)
!     Also reads GRIB index and grib file headers to
!     find a GRIB message and unpack pds/gds parameters (GETGB1S)
!
!     10-2012  Jeff McQueen
!==============================================================================
      INTEGER,    INTENT(IN)     :: lub,lui                  ! unit numbers
      INTEGER,    INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,    INTENT(OUT)    :: igdn,imax,jmax,kmax,numv ! grid size

      INTEGER,PARAMETER :: MBUF = 2000000 !Character length of bufr varb
      CHARACTER*80 FNAME
      CHARACTER CBUF(MBUF)
      INTEGER KPDS(200),KGDS(200)
      INTEGER JENS(200),KENS(200)

!jtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0

      WRITE(FNAME(6:7),FMT='(I2)')LUB
      CALL BAOPEN(LUB,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUI
      CALL BAOPEN(LUI,FNAME,IRETGI)

      CALL GETGI(LUI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)

      write(6,*)' IRET FROM GETGI ',IRGI,' UNIT ',LUB,LUI,' NLEN',NLEN
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT',IRGI
        ISTAT = IRGI
        STOP 'RDHDRS ABORT GETGI'
      ENDIF

       DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,    &
                     KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT',IRGS
          ISTAT = IRGS
          STOP 'RDHDRS ABORT: GETBS1S'
        ENDIF
        IGDN = KPDS(3)
        IMAX = KGDS(2)
        JMAX = KGDS(3)
        NUMV = IMAX*JMAX
        KMAX = 0    ! HARDWIRED FOR PRECIP
      ENDDO

      WRITE(6,280) IGDN,JPDS(4),JPDS(5),IMAX,JMAX,KMAX
  280 FORMAT(' IGDN, IMAX,JMAX,KMAX ',6I5)
      RETURN
      END
