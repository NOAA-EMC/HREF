      SUBROUTINE COLLECT_LOC ( A, B ) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    COLLECT     GATHERS FROM ALL MPI TASKS
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     GATHER "A" FROM ALL MPI TASKS ONTO TASK 0
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C
C USAGE:    CALL COLLECT(A)
C   INPUT ARGUMENT LIST:
C     A        - ARRAY BEING GATHERED
C
C   OUTPUT ARGUMENT LIST:
C     A        - GATHERED ARRAY - ONLY VALID ON TASK 0
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C       MPI_GATHERV
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK.comm
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$

!      include "parmeta"
      include "CTLBLK.comm"
      include 'mpif.h'
      real a ( im, jsta_2l:jend_2u ) 
      real b ( im, jm ) 
      integer i, j
      integer ierr
c
      if ( num_procs .le. 1 ) then
         b = a
      else
         call mpi_gatherv(a(1,jsta),icnt(me),MPI_REAL,
     &    b,icnt,idsp,MPI_REAL,0,MPI_COMM_COMP, ierr )
      end if
      end               
