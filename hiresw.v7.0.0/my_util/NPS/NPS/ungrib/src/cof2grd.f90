      subroutine COF2GRD(LUN1,IMAX,JMAX,KMAX,TGRID,ZGRID, &
                         UGRID,VGRID,QGRID,CWMGRID,PRESGRID,PINTGRID)
!
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!C                .      .    .                                       .
! SUBPROGRAM:    COF2GRD     CONVERT ONE RECORD OF SIGMA COEFF FILE
!C                            TO LAT/LON GRID
!   PRGMMR: ROGERS           ORG: W/NP22     DATE: 99-01-28
!
! ABSTRACT: CONVERT SIGMA COEFFICIENT RECORD TO GRID SPACE USING
!           SPLIB ROUTINES. THESE ROUTINES WILL RETURN A GLOBAL
!           LAT/LON GRID WHOSE RESOLUTION IS DETERMINED BY THE 
!           NUMBER OF GRID POINTS. THEN, THE RELEVENT SUBSET FOR
!           WHICH WE HAVE HIGH-RES OROGRAPHY IS EXTRACTED (DIMENSION
!           OF BOTH THE EXTRACTED GRID AND GLOBAL GRID SET IN 
!           parmanl FILE)
!
! PROGRAM HISTORY LOG:
!   99-01-28  ROGERS
!
! USAGE:    CALL COF2GRD(LUN1,JROMB,JCAP,KMAX)
!               JCAP)
!
!   INPUT ARGUMENT LIST:
!     LUN1     - FORTRAN UNIT FOR SIGMA FILE
!     JROMB    - SPECTRAL DOMAIN SHAPE (0 FOR TRIANGULAR, 
!                1 FOR RHOMBOIDAL)
!     JCAP     - SPECTRAL TRUNCATION
!
!   OUTPUT FILES:
!     KMAX     - NUMBER OF SIGMA LEVELS IN GLOBAL MODEL
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN-90
!   MACHINE: CRAY C-90
!
!$$$

      USE SIGIO_MODULE

      IMPLICIT NONE

      integer,parameter::real_32=selected_real_kind(6,30)

      INTEGER, INTENT(IN) :: LUN1
      INTEGER, INTENT(IN) :: IMAX,JMAX, KMAX

      REAL(REAL_32), DIMENSION(IMAX,JMAX,KMAX), INTENT(OUT) :: TGRID, UGRID, VGRID
      REAL(REAL_32), DIMENSION(IMAX,JMAX,KMAX), INTENT(OUT) :: QGRID, CWMGRID, PRESGRID
      REAL(REAL_32), DIMENSION(IMAX,JMAX,KMAX+1), INTENT(OUT) :: PINTGRID, ZGRID

      TYPE(sigio_head), SAVE:: HEAD
      TYPE(sigio_data), SAVE:: DATA

      INTEGER :: I,J,IRET,L,IER,K
      INTEGER :: II,JJ,LL

      real, parameter:: G=9.81
      real, parameter:: R=287.04
      real, parameter:: P00=1.0E5
      real, parameter:: CP=1004.6
      real, parameter:: CPOG=CP/G
      real, parameter:: CAPA=R/CP

      CHARACTER FILENAME*80

      REAL(REAL_32) :: EXNT
      REAL(REAL_32),DIMENSION(IMAX,JMAX) :: EXNL
      REAL(REAL_32),DIMENSION(IMAX,JMAX,KMAX) :: PD
      REAL(REAL_32),DIMENSION(IMAX,JMAX,3) :: PGRID

      write(filename,633) LUN1
  633 format('fort.',I2.2)

      CLOSE(LUN1)

      CALL sigio_srohdc(LUN1,trim(filename),head,data,iret)
      if (iret /= 0) then
         write(0,*) 'iret from sigio_srohdc: ', iret, LUN1, trim(filename)
         call mpi_abort(iret)
      end if
      if (KMAX /= head%levs) then
         write(0,*) ' KMAX /= head%levs : KMAX=',KMAX,' head%levs-',head%levs 
         call mpi_abort(iret)
      end if

!     hs (surface topo)

      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX, &
             0,0,0,0,1,data%hs,PGRID(1,JMAX,2),PGRID(1,1,2),1)

!     ps and midlayer P values

      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX, &
             0,0,0,0,1,data%ps,PGRID(1,JMAX,1),PGRID(1,1,1),1)

      DO J=1,JMAX 
      DO I=1,IMAX
         PGRID(I,J,1)=1000.*EXP(PGRID(I,J,1))
      ENDDO
      ENDDO

      CALL sigio_modpr(IMAX*JMAX,IMAX*JMAX,head%levs,    &
                      head%nvcoord,head%idvc,head%idsl,    &       
                      head%vcoord,iret,ps=PGRID(1,1,1),pm=PRESGRID, &
                      pd=PD)

!       temperatures

      DO J=1,JMAX
      DO I=1,IMAX
          PINTGRID(I,J,1)=PGRID(I,J,1)
      ENDDO
      ENDDO

      DO L=1,head%levs
      DO JJ=1,JMAX
      DO II=1,IMAX
         PINTGRID(II,JJ,L+1)=PINTGRID(II,JJ,L)-PD(II,JJ,L)
         IF (L .eq. head%levs .and. PINTGRID(II,JJ,L+1) .lt. 20.) then
            PINTGRID(II,JJ,L+1)=20.
         ENDIF
      ENDDO
      ENDDO
      ENDDO

      DO LL=1,KMAX
      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0, -IMAX,IMAX, &
             0,0,0,0,1,data%t(:,LL),TGRID(1,JMAX,LL),TGRID(1,1,LL),1)
      END DO

!
!   READ DIVERGENCE AND VORTICITY COEFFICIENTS
!

      DO L=1,KMAX
        CALL SPTRANV(0,head%jcap,0,IMAX,JMAX,1,0,0,-IMAX,IMAX, &
       0,0,0,0,1,data%d(:,L),data%z(:,L),UGRID(1,JMAX,L),UGRID(1,1,L), &
       VGRID(1,JMAX,L),VGRID(1,1,L),1)
      ENDDO

!
!   READ SPECIFIC HUMIDITY COEFFICIENTS
!
      DO L=1,KMAX
        CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX, &
             0,0,0,0,1,data%q(:,L,1),QGRID(1,JMAX,L),QGRID(1,1,L),1)
      ENDDO

      DO K = 1, KMAX
       DO J = 1, JMAX
        DO I = 1, IMAX
          QGRID(I,J,K) = AMAX1(QGRID(I,J,K),1.0E-8)
        ENDDO
       ENDDO
      ENDDO

      CWMGRID=-9999.
      DO L=1,KMAX
        CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX, &
          0,0,0,0,1,data%q(:,L,3),CWMGRID(1,JMAX,L),CWMGRID(1,1,L),1)
      ENDDO

!!! compute ZGRID following the approach in SIG2HYB.f of the mkbnd code

      CALL FLIP(PRESGRID,IMAX,JMAX,KMAX)
      CALL FLIP(TGRID,IMAX,JMAX,KMAX)
      CALL FLIP(UGRID,IMAX,JMAX,KMAX)
      CALL FLIP(VGRID,IMAX,JMAX,KMAX)
      CALL FLIP(QGRID,IMAX,JMAX,KMAX)
      CALL FLIP(PINTGRID,IMAX,JMAX,KMAX+1)

!! has zsfc

      DO 250 LL=1,KMAX
         L=KMAX+1-LL

         DO 250 J=1,JMAX
         DO 250 I=1,IMAX

         IF(L.EQ.KMAX) then
          EXNL(I,J)=(PGRID(I,J,1)/P00)**CAPA
          ZGRID(I,J,L+1)=PGRID(I,J,2)
         ENDIF

         EXNT=(PINTGRID(I,J,L)/P00)**CAPA

         ZGRID(I,J,L)=ZGRID(I,J,L+1)-CPOG*TGRID(I,J,L)*(EXNT-EXNL(I,J)) &
                                    *(P00/PRESGRID(I,J,L))**CAPA

         EXNL(I,J)=EXNT

  250 CONTINUE


!!!! end ZGRID compute

!  Devirtualize GFS temperature
      DO K = 1, KMAX
       DO J = 1, JMAX
        DO I = 1, IMAX
         TGRID(I,J,K)=TGRID(I,J,K)/(1.0+0.602*QGRID(I,J,K))
        ENDDO
       ENDDO
      ENDDO

      END subroutine COF2GRD

      SUBROUTINE FLIP (AE,IMAX,JMAX,KMAX)

      DIMENSION AE(IMAX,JMAX,KMAX),TEMP(IMAX,JMAX,KMAX)
!************************************************************************
      
      DO 100 KN=1,KMAX
      K=KMAX-KN+1
      DO 50 J=1,JMAX
      DO 50 I=1,IMAX
      TEMP(I,J,K)=AE(I,J,KN)
   50 CONTINUE
  100 CONTINUE

      DO KN=1,KMAX
      DO J=1,JMAX
      DO I=1,IMAX
      AE(I,J,KN)=TEMP(I,J,KN)
      ENDDO
      ENDDO
      ENDDO

      RETURN
      END SUBROUTINE FLIP
