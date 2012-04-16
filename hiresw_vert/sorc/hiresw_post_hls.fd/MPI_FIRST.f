      SUBROUTINE MPI_FIRST
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    MPI_FIRST   SET UP MESSGAE PASSING INFO
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     SETS UP MESSAGE PASSING INFO
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C
C USAGE:    CALL MPI_FIRST
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C       PARA_RANGE
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - CTLBLK.comm
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$
c
      use vrbls
      use omgaot
      use extra
      use cldwtr
      use pvrbls
      use masks
c
      include "parmeta"
      include "CTLBLK.comm"
      include "params"
      include 'mpif.h'
c

c
      PARAMETER (IMJM=IM*JM-JM/2)
c
      integer ierr
c
      if ( me .eq. 0 ) then
c        print *, ' NUM_PROCS = ',num_procs
      end if

      if ( num_procs .gt. 1024 ) then
         print *, ' too many MPI tasks, max is 1024, stopping'
         call mpi_abort(MPI_COMM_WORLD,1,ierr)
         stop
      end if
c
c     error check
c
      if ( num_procs .gt. JM/2 ) then
         print *, ' too many MPI tasks, max is ',jm/2,' stopping'
         call mpi_abort(MPI_COMM_WORLD,1,ierr)
         stop
      end if
c
c     global loop ranges
c
      call para_range(1,jm,num_procs,me,jsta,jend)
      jsta_m  = jsta
      jsta_m2 = jsta
      jend_m  = jend
      jend_m2 = jend
      if ( me .eq. 0 ) then
         jsta_m  = 2
         jsta_m2 = 3
      end if
      if ( me .eq. num_procs - 1 ) then
         jend_m  = jm - 1
         jend_m2 = jm - 2
      end if
c
c     neighbors
c
      iup = me + 1
      idn = me - 1
      if ( me .eq. 0 ) then
         idn = MPI_PROC_NULL
      end if
      if ( me .eq. num_procs - 1 ) then
         iup = MPI_PROC_NULL
      end if
C
c     print *, ' ME, NUM_PROCS = ',me,num_procs
c     print *, ' ME, JSTA, JSTA_M, JSTA_M2 = ',me,jsta,jsta_m,jsta_m2
c     print *, ' ME, JEND, JEND_M, JEND_M2 = ',me,jend,jend_m,jend_m2
c     print *, ' ME, IUP, IDN = ',me,iup,idn
c
c     counts, disps for gatherv and scatterv
c
      do i = 0, num_procs - 1
         call para_range(1,jm,num_procs,i,jsx,jex) 
         icnt(i) = (jex-jsx+1)*im
         idsp(i) = (jsx-1)*im
         if ( me .eq. 0 ) then
c           print *, ' i, icnt(i),idsp(i) = ',i,icnt(i),idsp(i)
         end if
      end do
c
c     extraction limits -- set to two rows    
c
      jsta_2l = max(jsta - 2,  1 )
      jend_2u = min(jend + 2, jm )
c     print *, ' me, jsta_2l, jend_2u = ',me,jsta_2l, jend_2u
c
c     allocate arrays
c
C
C     FROM VRBLS
C
      allocate(pd(im,jsta_2l:jend_2u))
      allocate(res(im,jsta_2l:jend_2u))
      allocate(fis(im,jsta_2l:jend_2u))
      allocate(u(im,jsta_2l:jend_2u,lm))
      allocate(v(im,jsta_2l:jend_2u,lm))
      allocate(t(im,jsta_2l:jend_2u,lm))
      allocate(q(im,jsta_2l:jend_2u,lm))
      allocate(w(im,jsta_2l:jend_2u,lp1))
      allocate(dwdt(im,jsta_2l:jend_2u,lm))
C
C     FROM OMGAOT
C
      allocate(omga(im,jsta_2l:jend_2u,lm))
C
C     FROM EXTRA
C
      allocate(pint(im,jsta_2l:jend_2u,lp1))
      allocate(alpint(im,jsta_2l:jend_2u,lp1))
      allocate(zint(im,jsta_2l:jend_2u,lp1))
      allocate(kmntm(lm))
      allocate(kmnt(imjm,lm))
      allocate(slp(im,jm))
      allocate(t500(im,jm))
      allocate(pdsl(im,jm))
      allocate(pdvp1(im,jm))
      allocate(pslp(im,jm))
      allocate(ptsl(im,jm))
      allocate(pfsl(im,jm))
      allocate(tsl(im,jm))
      allocate(ttv(im,jm))
      allocate(fsl(im,jm))
      allocate(qsl(im,jm))
      allocate(z1000(im,jm))

C
C     FROM CLDWTR
C
      allocate(cwm(im,jsta_2l:jend_2u,lm))
      allocate(u00(im,jsta_2l:jend_2u))
      allocate(ul(2*lm))
      allocate(lc(im,jsta_2l:jend_2u))
      allocate(sr(im,jsta_2l:jend_2u))
C
C     FROM PVRBLS
C
      allocate(z0(im,jm))
      allocate(akms(im,jm))
      allocate(akhs(im,jm))
      allocate(ths(im,jm))
      allocate(qs(im,jm))
      allocate(uz0(im,jm))
      allocate(vz0(im,jm))
      allocate(thz0(im,jm))
      allocate(qz0(im,jm))
      allocate(rf(im,jm))
      allocate(twbs(im,jm))
      allocate(qwbs(im,jm))
      allocate(sno(im,jm))
      allocate(si(im,jm))
      allocate(cldefi(im,jm))
      allocate(prec(im,jm))
      allocate(acprec(im,jm))
      allocate(accliq(im,jm))
      allocate(cuprec(im,jm))
      allocate(q2(im,jsta_2l:jend_2u,lm))
C
C     FROM MASKS
C
      allocate(hbm2(im,jm))
      allocate(vbm2(im,jm))
      allocate(vbm3(im,jm))
      allocate(sm(im,jm))
      allocate(sice(im,jm))
      allocate(htm(im,jsta_2l:jend_2u,lm))
      allocate(vtm(im,jsta_2l:jend_2u,lm))
C
      end
