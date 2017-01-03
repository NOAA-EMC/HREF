       subroutine COF2GRD_NPS(LUN1,IMAX, JMAX,KMAX, TGRID, ZGRID, &
                            UGRID, VGRID, QGRID, CWMGRID, PRESGRID, PINTGRID)
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
      include "mpif.h"
!      IMPLICIT NONE
      TYPE(sigio_head), SAVE:: HEAD
      TYPE(sigio_data), SAVE:: DATA

!      real*8 :: timef
!      real :: btim,btimx


      INTEGER :: I,J,IRET,L,IER,K,LLL,KMAX,LUN1
      INTEGER :: LUN1HOLD, MYTHREAD, MAX_THREADS

      INTEGER, allocatable:: L_S(:), L_E(:), icnt(:)
      INTEGER, allocatable:: idsp(:)

!!!      INCLUDE "parmlbc"
      integer,parameter::real_32=selected_real_kind(6,30)
!
      real, parameter:: G=9.81
      real, parameter:: R=287.04
      real, parameter:: P00=1.0E5
      real, parameter:: CP=1004.6
      real, parameter:: CPOG=CP/G
      real, parameter:: CAPA=R/CP

      CHARACTER HOLDFIL*80,FILENAME*80
!
      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:)::GRD
      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:)::MYGRD,EXNL
      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:,:)::MYGRD2,PD

      REAL(REAL_32), DIMENSION(IMAX,JMAX,64) :: TGRID, UGRID, VGRID
      REAL(REAL_32), DIMENSION(IMAX,JMAX,64) :: QGRID, CWMGRID, PRESGRID
      REAL(REAL_32),ALLOCATABLE,SAVE :: GRID_TMP(:,:,:)
      REAL(REAL_32), DIMENSION(IMAX,JMAX,65) :: PINTGRID, ZGRID

!      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:,:)::TGRID
!      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:,:)::UGRID
!      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:,:)::VGRID
!      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:,:)::QGRID
!      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:,:)::CWMGRID
!      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:,:)::PRESGRID
!      REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:,:,:)::PINTGRID

!       REAL(REAL_32),SAVE,ALLOCATABLE,DIMENSION(:)::DWORK,ZWORK
!
	REAL,ALLOCATABLE,SAVE:: PGRID(:,:,:)

	KMAX=64

!	btim=timef()
!	btimx=timef()
	write(6,*) 'start play_COF2GRD'
	call summary()

!      call mpi_init(ierr)
      call mpi_comm_rank(MPI_COMM_WORLD,mype,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,npes,ierr)

      print *,'entering cof2grd',imax,jmax,kmax
!
        write(filename,633) LUN1
  633	format('fort.',I2.2)

	CLOSE(LUN1)

      CALL sigio_srohdc(LUN1,trim(filename),head,data,iret)
      write(0,*) 'iret from sigio_srohdc: ', iret
      write(0,*) 'head%levs: ', head%levs
      write(0,*) 'head%jcap: ', head%jcap
!	write(0,*) 'head%ntrac: ', head%ntrac
!	write(6,*) 'start COF2GRD'
!	call summary()

	write(6,*) 'past sigio_srohdc calls...to allocs'

	if (.not. ALLOCATED(MYGRD)) then
	write(6,*) 'allocating MYGRD'
      ALLOCATE(MYGRD(IMAX,JMAX))
      ALLOCATE(MYGRD2(IMAX,JMAX,head%levs))
      ALLOCATE(PD(IMAX,JMAX,head%levs))
      ALLOCATE(GRD(IMAX,JMAX))
      ALLOCATE(PGRID(IMAX,JMAX,3))
      ALLOCATE(GRID_TMP(IMAX,JMAX,64))
	endif


!       hs (surface topo)

    MYPE1:	if (MYPE .eq. 0) then

      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0, &
             -IMAX,IMAX, &
             0,0,0,0,1,data%hs,MYGRD(1,JMAX),MYGRD(1,1),1)


	K=0
       DO J=1,JMAX 
       DO I=1,IMAX
         K=K+1
!         PGRID(I,J,2)=MYGRD(K)
         PGRID(I,J,2)=MYGRD(I,J)
       ENDDO
       ENDDO
!	write(0,*) 'surface topo, J=1 to J=JMAX'

	do J=1,JMAX,JMAX/30
	write(6,617) (PGRID(I,J,2),I=1,IMAX,IMAX/20)
	enddo

  617	format(30(f5.0,1x))

!       ps and midlayer P values

      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0, &
             -IMAX,IMAX, &
             0,0,0,0,1,data%ps,MYGRD(1,JMAX),MYGRD(1,1),1)

	K=0
       DO J=1,JMAX 
       DO I=1,IMAX
         K=K+1
         PGRID(I,J,1)=1000.*EXP(MYGRD(I,J))
         MYGRD(I,J)=PGRID(I,J,1)
       ENDDO
       ENDDO

!	write(0,*) 'past first few calls;'
!	write(0,*) 1.e-3*(timef()-btim)
!	do J=1,JMAX,JMAX/40
!	write(6,617) (PGRID(I,J,1)/100.,I=1,IMAX,IMAX/20)
!	enddo
!	btim=timef()


      CALL sigio_modpr(IMAX*JMAX,IMAX*JMAX,head%levs,    &
                      head%nvcoord,head%idvc,head%idsl,    &       
                      head%vcoord,iret,ps=MYGRD,pm=MYGRD2, &
                      pd=PD)

!       temperatures

        DO J=1,JMAX
        DO I=1,IMAX
          PINTGRID(I,J,1)=MYGRD(I,J)
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

!
!   READ SFC PRESSURE COEFFICIENTS
!
      DO L=1,head%levs
      DO J = 1, JMAX
        DO I = 1, IMAX
         PRESGRID(I,J,L) =  MYGRD2(I,J,L)
        ENDDO
      ENDDO
      ENDDO

	write(6,*) 'finish PE=0 wrap part'
        endif MYPE1 ! PE=0 wrap

	call para_range(1,head%levs,npes,mype,L_START, L_END)

	if (.not. allocated(L_S)) then
	allocate(L_S(0:npes-1), L_E(0:npes-1), icnt(0:npes-1))
        allocate(idsp(0:npes-1))
	endif

	do L=0,npes-1
	call para_range(1,head%levs,npes,L,L_S(L), L_E(L))
	icnt(L)=IMAX*JMAX*(L_E(L)-L_S(L)+1)
        idsp(L)=IMAX*JMAX*(L_S(L)-1)
!	write(0,*) 'L, icnt(L),idsp(L): ',L, icnt(L),idsp(L)
	enddo

	write(0,*) 'MYPE, L_START, L_END: ', MYPE, L_START, L_END

        DO LL=L_START,L_END

      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0, &
             -IMAX,IMAX, &
             0,0,0,0,1,data%t(:,LL),TGRID(1,JMAX,LL),TGRID(1,1,LL),1)

!	write(0,*) 'LL, mythread past SPTRAN: ',LL, mythread
        END DO

!	write(0,*) 'past T'
!	write(6,*) 'past T'
!	call summary()
!	write(0,*) 1.e-3*(timef()-btim)
!	btim=timef()


!      print *,'ok after terrain coeffs'

!      DEALLOCATE(GRD)
!
!   READ DIVERGENCE AND VORTICITY COEFFICIENTS
!

      DO L=L_START,L_END
        CALL SPTRANV(0,head%jcap,0,IMAX,JMAX,1,0,0,-IMAX,IMAX, &
       0,0,0,0,1,data%d(:,L),data%z(:,L),UGRID(1,JMAX,L),UGRID(1,1,L), &
       VGRID(1,JMAX,L),VGRID(1,1,L),1)
      ENDDO

!	write(0,*) 1.e-3*(timef()-btim)
!	btim=timef()
!	call summary()

!
!   READ SPECIFIC HUMIDITY COEFFICIENTS
!
      DO L=L_START,L_END

        CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX, &
             0,0,0,0,1,data%q(:,L,1),QGRID(1,JMAX,L),QGRID(1,1,L),1)
      ENDDO
!      print *,'ok after q coeffs'

      DO K = L_START, L_END
       DO J = 1, JMAX
        DO I = 1, IMAX
          QGRID(I,J,K) = AMAX1(QGRID(I,J,K),1.0E-8)
        ENDDO
       ENDDO
      ENDDO



!
!	write(0,*) 'past Q'
!	write(6,*) 'past Q'
!	call summary()
!	write(0,*) 1.e-3*(timef()-btim)
!	btim=timef()

	CWMGRID=-9999.
      DO L=L_START,L_END
        CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX, &
          0,0,0,0,1,data%q(:,L,3),CWMGRID(1,JMAX,L),CWMGRID(1,1,L),1)
      ENDDO

!	btim=timef()

!!! NEED TO EXCHANGE INFORMATION TO GET ALL LEVELS on TASK0 to write

	maxl=0
	do L=0,npes-1
	maxl=max(maxl, 1+( L_E(L)-L_S(L) ) )
	enddo

        GRID_TMP=0.

	CALL MPI_GATHERV(TGRID(:,:,L_START), &
            icnt(mype), MPI_REAL,  &
            GRID_TMP, icnt, idsp, &
            MPI_REAL, 0, MPI_COMM_WORLD, ierr) 

	if (MYPE .eq. 0) then
         do L=1,64
          do J=1,JMAX
           do I=1,IMAX
             TGRID(I,J,L)=GRID_TMP(I,J,L)
           enddo
          enddo
         enddo
	endif


	CALL MPI_GATHERV(UGRID(:,:,L_START), &
            icnt(mype), MPI_REAL,  &
            GRID_TMP, icnt, idsp, &
            MPI_REAL, 0, MPI_COMM_WORLD, ierr) 

	if (MYPE .eq. 0) then
         do L=1,64
          do J=1,JMAX
           do I=1,IMAX
             UGRID(I,J,L)=GRID_TMP(I,J,L)
           enddo
          enddo
         enddo
	endif


	CALL MPI_GATHERV(VGRID(:,:,L_START), &
            icnt(mype), MPI_REAL,  &
            GRID_TMP, icnt, idsp, &
            MPI_REAL, 0, MPI_COMM_WORLD, ierr) 

	if (MYPE .eq. 0) then
         do L=1,64
          do J=1,JMAX
           do I=1,IMAX
             VGRID(I,J,L)=GRID_TMP(I,J,L)
           enddo
          enddo
         enddo
	endif


	CALL MPI_GATHERV(QGRID(:,:,L_START), &
            icnt(mype), MPI_REAL,  &
            GRID_TMP, icnt, idsp, &
            MPI_REAL, 0, MPI_COMM_WORLD, ierr) 

	if (MYPE .eq. 0) then
         do L=1,64
          do J=1,JMAX
           do I=1,IMAX
             QGRID(I,J,L)=GRID_TMP(I,J,L)
           enddo
          enddo
         enddo
	endif


	CALL MPI_GATHERV(CWMGRID(:,:,L_START), &
            icnt(mype),  MPI_REAL,  &
            GRID_TMP, icnt, idsp, &
            MPI_REAL, 0, MPI_COMM_WORLD, ierr) 

	if (MYPE .eq. 0) then
         do L=1,64
          do J=1,JMAX
           do I=1,IMAX
             CWMGRID(I,J,L)=GRID_TMP(I,J,L)
           enddo
          enddo
         enddo
	endif

!       DEALLOCATE(GRID_TMP)


	write(0,*) 'mype, ierr ', mype, ierr

!	write(0,*) 'gather times:', 1.e-3*(timef()-btim)
!	btim=timef()

	if (MYPE .eq. 0) then
	do L=1,head%levs,3
	write(0,*) 'TGRID(200,200,L),UGRID, QGRID, PRESGRID: ',  &
                   L, TGRID(200,200,L), &
                   UGRID(200,200,L),QGRID(200,200,L),PRESGRID(200,200,L)
	enddo
	endif


!!! compute ZGRID following the approach in SIG2HYB.f of the mkbnd code

	if (.not. allocated(EXNL)) then
       ALLOCATE(EXNL(IMAX,JMAX))
	endif

!      ALLOCATE(EXNL_V(KB_V))
!

    MYPE2:  if (MYPE .eq. 0) then

	write(0,*) 'to flip calls'
	write(0,*) 'size(PRESGRID):: ', size(PRESGRID,dim=1), size(PRESGRID,dim=2), size(PRESGRID,dim=3)
        CALL FLIP(PRESGRID,IMAX,JMAX,KMAX)
	write(0,*) 'past first flip call'
        CALL FLIP(TGRID,IMAX,JMAX,KMAX)
        CALL FLIP(UGRID,IMAX,JMAX,KMAX)
        CALL FLIP(VGRID,IMAX,JMAX,KMAX)
        CALL FLIP(QGRID,IMAX,JMAX,KMAX)
        CALL FLIP(PINTGRID,IMAX,JMAX,KMAX+1)
	write(0,*) 'past flip calls'

!! has zsfc      PGRID(I,J,2)=MYGRD(I,J)

!mp      DO 250 LL=1,KMAX-1
      DO 250 LL=1,KMAX
         L=KMAX+1-LL

         DO 250 J=1,JMAX
         DO 250 I=1,IMAX

         IF(L.EQ.KMAX) then
          EXNL(I,J)=(PGRID(I,J,1)/P00)**CAPA
          ZGRID(I,J,L+1)=PGRID(I,J,2)

        if (I .eq. 180 .and. J .eq. 180.) then
!	write(0,*) 'PGRID(I,J,1): ', PGRID(I,J,1)
!       write(0,*) 'EXNL(I,J): ', EXNL(I,J)
	write(0,*) 'L+1, ZGRID(I,J,L+1): ', L+1,ZGRID(I,J,L+1)
        endif

         ENDIF

         EXNT=(PINTGRID(I,J,L)/P00)**CAPA


        ZGRID(I,J,L)=ZGRID(I,J,L+1)-CPOG*TGRID(I,J,L)*(EXNT-EXNL(I,J)) &
            *(P00/PRESGRID(I,J,L))**CAPA
!        if (I .eq. 180 .and. J .eq. 180.) then
!       write(0,*) 'EXNT, EXNL(I,J), TGRID(I,J,L), PRESGRID(I,J,L):: ', EXNT, EXNL(I,J), TGRID(I,J,L), PRESGRID(I,J,L)
!	write(0,*) 'L, ZGRID(L), ZGRID(L+1): ', ZGRID(I,J,L),ZGRID(I,J,L+1)
!	endif


        if (I .eq. 180 .and. J .eq. 180.) then
        write(0,*) 'L, ZGRID(I,J,L): ', L, ZGRID(I,J,L)
        endif

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


!!!   gather TGRID, UGRID, VGRID, QGRID, CWMGRID, PRESGRID(nlev), PINTGRID (nlev+1), PGRID(3)
!      LUN1HOLD=LUN1+100
!      WRITE(HOLDFIL,1000)LUN1
1000  FORMAT('holdsig',I3.3)
!      OPEN(UNIT=LUN1HOLD,FILE=HOLDFIL,FORM='UNFORMATTED',IOSTAT=IER)
!
!      WRITE(LUN1HOLD)TGRID
!      WRITE(LUN1HOLD)UGRID
!      WRITE(LUN1HOLD)VGRID
!      WRITE(LUN1HOLD)QGRID
!      WRITE(LUN1HOLD)CWMGRID
!      WRITE(LUN1HOLD)PRESGRID   ! midlayer P
!      WRITE(LUN1HOLD)PINTGRID   ! interface P
!      WRITE(LUN1HOLD)PGRID      ! SFC P/Z
!      CLOSE(LUN1HOLD)

	write(0,*) 'all written'

	endif MYPE2

	KMAX=head%levs

	write(6,*) 'end COF2GRD'
!	write(0,*) 'TOTAL TIME: ', 1.e-3*(timef()-btimx)
!	call summary()
!	call mpi_finalize(ierr)
!
      END subroutine COF2GRD_NPS

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE PARA_RANGE (N1,N2,NPROCS,IRANK,ISTA,IEND)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    PARA_RANGE  SET UP DECOMPOSITION VALUES
!   PRGRMMR: TUCCILLO        ORG: IBM
!
! ABSTRACT:
!     SETS UP DECOMOSITION VALUES
!   .
!
! PROGRAM HISTORY LOG:
!   00-01-06  TUCCILLO - ORIGINAL
!
! USAGE:    CALL COLLECT(A)
!   INPUT ARGUMENT LIST:
!     N1 - FIRST INTERATE VALUE
!     N2 - LAST INTERATE VALUE
!     NPROCS - NUMBER OF MPI TASKS
!     IRANK - MY TASK ID
!
!   OUTPUT ARGUMENT LIST:
!     ISTA - FIRST LOOP VALUE
!     IEND - LAST LOOP VALUE
!
!   OUTPUT FILES:
!    STDOUT  - RUN TIME STANDARD OUT.
!
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM RS/6000 SP
!$$$
      implicit none
      integer n1,n2,nprocs,irank,ista,iend
      integer iwork1, iwork2
      iwork1 = ( n2 - n1 + 1 ) / nprocs
      iwork2 = mod ( n2 - n1 + 1, nprocs )
      ista = irank * iwork1 + n1 + min ( irank, iwork2 )
      iend = ista + iwork1 - 1
      if ( iwork2 .gt. irank ) iend = iend + 1
      end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE FLIP (AE,IMAX,JMAX,KMAX)

      DIMENSION AE(IMAX,JMAX,KMAX),TEMP(IMAX,JMAX,KMAX)
!************************************************************************
	
	write(0,*) 'size(AE): ', size(AE,dim=1),size(ae,dim=2),size(ae,dim=3)

      DO 100 KN=1,KMAX
      K=KMAX-KN+1
!	write(0,*) 'put KN: ', KN, 'into K: ', K
      DO 50 J=1,JMAX
      DO 50 I=1,IMAX
      TEMP(I,J,K)=AE(I,J,KN)
   50 CONTINUE
  100 CONTINUE

!	write(0,*) 'past first do blocks'
      DO KN=1,KMAX
      DO J=1,JMAX
      DO I=1,IMAX
      AE(I,J,KN)=TEMP(I,J,KN)
      ENDDO
      ENDDO
      ENDDO

      RETURN
      END SUBROUTINE FLIP

