  MODULE rdgrib
  use grddef
        USE GRIB_MOD
        USE pdstemplates

!=======================================================================
!  routines to  read/write grib data 
!=======================================================================
     
   REAL,      ALLOCATABLE  :: GRID(:)
   LOGICAL*1, ALLOCATABLE  :: MASK(:)

contains
     SUBROUTINE SETVAR(LUB,LUI,NUMV,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
!============================================================================
!     This Routine reads in a grib field and initializes a 2-D variable
!     Requested from w3lib GETGRB routine
!     10-2012   Jeff McQueen
!     NOTE: ONLY WORKS for REAL Type Variables
!============================================================================

   REAL,      INTENT(INOUT)  :: GRID(:),VARB(:,:)
   LOGICAL*1, INTENT(INOUT)  :: MASK(:)
!-----------------------------------------------------------------------------------------
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)

!     Get GRIB Variable

      CALL GETGB(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
!        write(0,*) 'IRET from GETGB call ', IRET
!        write(0,*) 'jpds(5:7) for found variable: ' , JPDS(5:7)

      IMAX=KGDS(2)
      IF(IRET.EQ.0) THEN
        DO KK = 1, NUMV
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          VARB(M,N) = GRID(KK)
        ENDDO
       IF(JPDS(6).ne.109 .or. JPDS(6).eq.109.and.J.le.40) &
        WRITE(0,100) JPDS(5),JPDS(6),JPDS(7),J,MINVAL(VARB),MAXVAL(VARB)
 100   FORMAT('VARB UNPACKED ', 4I7,2G12.4)
      ELSE
       WRITE(6,*)'====================================================='
       WRITE(6,*)'COULD NOT UNPACK VARB(setvar)',K,JPDS(3),JPDS(5),JPDS(6),IRET
       WRITE(6,*)'USING J: ', J
       WRITE(6,*)'UNIT', LUB,LUI,NUMV,KF
       WRITE(6,*)'====================================================='
       print *,'JPDS',jpds(1:25)
       ISTAT = IRET
! 01-29-13 JTM : past hour 60 nam output onli to level 35
       if (JPDS(6).ne.109) then
        write(0,*) 'jpds(5:7): ' , JPDS(5:7)
                STOP 'ABORT: GRIB VARB READ ERROR'
       endif
      ENDIF

      RETURN
      END SUBROUTINE setvar

     SUBROUTINE SETVAR_g2(LUB,LUI,NUMV,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF, &
                                       K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
!============================================================================
!     This Routine reads in a grib field and initializes a 2-D variable
!     Requested from w3lib GETGRB routine
!     10-2012   Jeff McQueen
!     NOTE: ONLY WORKS for REAL Type Variables
!============================================================================

   REAL,      INTENT(INOUT)  :: GRID(:),VARB(:,:)
   LOGICAL*1, INTENT(INOUT)  :: MASK(:)
!-----------------------------------------------------------------------------------------
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)

! C grib2
      INTEGER :: LUB,LUI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET
      TYPE(GRIBFIELD) :: GFLD
! C grib2



!     Get GRIB Variable

!       CALL GETGB(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)

        JDISC=0

        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUB,LUI,J,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
          UNPACK,K,GFLD,IRET)

!      SUBROUTINE GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
!     &                  UNPACK,K,GFLD,IRET)

        write(0,*) 'IRET from GETGB2 call ', IRET
!        write(0,*) 'jpds(5:7) for found variable: ' , JPDS(5:7)

!      IMAX=KGDS(2)


      IF(IRET.EQ.0) THEN
        write(0,*) 'found it'

        write(0,*) 'probably not knowing NUMV: ', NUMV
        write(0,*) 'probably not knowing IMAX: ', IMAX
        DO KK = 1, NUMV
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          VARB(M,N) = GRID(KK)
        ENDDO
       IF(JPDS(6).ne.109 .or. JPDS(6).eq.109.and.J.le.40) &
        WRITE(0,100) JPDS(5),JPDS(6),JPDS(7),J,MINVAL(VARB),MAXVAL(VARB)
 100   FORMAT('VARB UNPACKED ', 4I7,2G12.4)
      ELSE
       WRITE(6,*)'====================================================='
       WRITE(6,*)'COULD NOT UNPACK VARB(setvar)',K,JPDS(3),JPDS(5),JPDS(6),IRET
       WRITE(6,*)'USING J: ', J
       WRITE(6,*)'UNIT', LUB,LUI,NUMV,KF
       WRITE(6,*)'====================================================='
       print *,'JPDS',jpds(1:25)
       ISTAT = IRET
! 01-29-13 JTM : past hour 60 nam output onli to level 35
       if (JPDS(6).ne.109) then
        write(0,*) 'jpds(5:7): ' , JPDS(5:7)
                STOP 'ABORT: GRIB VARB READ ERROR'
       endif
      ENDIF

      RETURN
      END SUBROUTINE setvar_g2



      SUBROUTINE RDHDRS(LUB,LUI,IGDN,GDIN,NUMV)
      use grddef
!=============================================================
!     This Routine Reads GRIB index file and returns its contents
!     (GETGI)
!     Also reads GRIB index and grib file headers to
!     find a GRIB message and unpack pds/gds parameters (GETGB1S)
!
!     10-2012  Jeff McQueen
!=============================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      PARAMETER(MBUF=2000000)
      CHARACTER CBUF(MBUF)
      CHARACTER*80 FNAME
      INTEGER JENS(200),KENS(200)
      TYPE (GINFO)  ::  GDIN

!jtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

      IRGI = 1
      IRGS = 1
!TEST 1/27/13      KMAX = 0
      JR=0
      KSKIP = 0

      WRITE(FNAME(6:7),FMT='(I2)')LUB
      CALL BAOPEN(LUB,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUI
      CALL BAOPEN(LUI,FNAME,IRETGI)
      CALL GETGI(LUI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)

      write(6,*)' IRET FROM GETGI ',IRGI,LUB,LUI,NLEN,NNUM
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        STOP 'ABORT RDHDRS: GRIB INDEX FILE READ ERROR '
      ENDIF


      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)

!JTM    write(6,*)' IRET FROM GETGB1S ',IRGS,JR
        IF(IRGS .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          WRITE(6,280) IGDN,JPDS(4),JPDS(5)
          ISTAT = IRGS
          STOP 'ABORT RDHDRS: GRIB HDR READ ERROR '
        ENDIF
        IGDN = KPDS(3)
        GDIN%IMAX = KGDS(2)
        GDIN%JMAX = KGDS(3)
        NUMV = GDIN%IMAX*GDIN%JMAX
      ENDDO

  280 FORMAT(' IGDN, IMAX,JMAX, ',4I5)
      RETURN
      END SUBROUTINE rdhdrs


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE RDHDRS_g2(LUB,LUI,IGDN,GDIN,NUMV)
      use grddef
!=============================================================
!     This Routine Reads GRIB index file and returns its contents
!     (GETGI)
!     Also reads GRIB index and grib file headers to
!     find a GRIB message and unpack pds/gds parameters (GETGB1S)
!
!     10-2012  Jeff McQueen
!     10-2014  M. Pyle - GRIB2 version
!=============================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)

      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF


!C  DECLARE INTERFACES (REQUIRED FOR CBUF POINTER)

      INTERFACE
         SUBROUTINE GETIDX(LUGB,LUGI,CBUF,NLEN,NNUM,IRGI)
            CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
            INTEGER,INTENT(IN) :: LUGB,LUGI
            INTEGER,INTENT(OUT) :: NLEN,NNUM,IRGI
         END SUBROUTINE GETIDX
      END INTERFACE


      CHARACTER*80 FNAME
      INTEGER JENS(200),KENS(200)

! C grib2
      INTEGER :: LUB,LUI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET,LPOS
      TYPE(GRIBFIELD) :: GFLD
! C grib2

      TYPE (GINFO)  ::  GDIN

!jtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

      IRGI = 1
      IRGS = 1
!TEST 1/27/13      KMAX = 0
      JR=0
      KSKIP = 0

      WRITE(FNAME(6:7),FMT='(I2)')LUB
      CALL BAOPEN(LUB,FNAME,IRETGB)
        write(0,*) 'IRETGB on baopen: ', IRETGB
      WRITE(FNAME(6:7),FMT='(I2)')LUI
      CALL BAOPEN(LUI,FNAME,IRETGI)
        write(0,*) 'IRETGI on baopen: ', IRETGi


        write(0,*) 'to getidx call'

        CALL GETIDX(LUB,LUI,CBUF,NLEN,NNUM,IRGI)
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        STOP 'ABORT RDHDRS: GRIB INDEX FILE READ ERROR '
      ENDIF

!!! why retrieve for all records???
        NNUM=1

      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
!        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)


        JDISC=-1
        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999

        call GETGB2S(CBUF,NLEN,NNUM,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                        JGDT,K,GFLD,LPOS,IRGS)
        IF(IRGS .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
        ENDIF

!        if (K .eq. 1) then
!        do N=1,30
!        write(0,*) 'N, gfld%igdtmpl(N): ', N, gfld%igdtmpl(N)
!        enddo
!        endif

        write(0,*) 'gfld%igdtnum: ', gfld%igdtnum

          selectcase( gfld%igdtnum )     !  Template number
           case (0:3)   ! Lat/Lon
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (10)   ! Mercator
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (20)   ! Polar Stereographic
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (30)   ! Lambert Conformal
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (40:43)   ! Gaussian
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (90)   ! Space View/Orthographic
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (110)   ! Equatorial Azimuthal
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case default
              GDIN%IMAX=0
              GDIN%JMAX=0
         end select

        write(0,*) 'past select'

!JTM    write(6,*)' IRET FROM GETGB1S ',IRGS,JR
        IF(IRGS .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          WRITE(6,280) IGDN,JPDS(4),JPDS(5)
          ISTAT = IRGS
          STOP 'ABORT RDHDRS: GRIB HDR READ ERROR '
        ENDIF
!        IGDN = KPDS(3)
!        GDIN%IMAX = KGDS(2)
!        GDIN%JMAX = KGDS(3)
        NUMV = GDIN%IMAX*GDIN%JMAX
      ENDDO

        write(0,*) 'to diag writes'

        write(0,*) 'gfld%igdtnum: ', gfld%igdtnum
        write(0,*) 'GDIN%IMAX, GDIN%JMAX: ', GDIN%IMAX, GDIN%JMAX

  280 FORMAT(' IGDN, IMAX,JMAX, ',4I5)
      RETURN
      END SUBROUTINE rdhdrs_g2

  END MODULE rdgrib
