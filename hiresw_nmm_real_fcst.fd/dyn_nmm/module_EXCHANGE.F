!-----------------------------------------------------------------------
!
      MODULE MODULE_EXCHANGE
!
!-----------------------------------------------------------------------
!
!***  MODULE_EXCHANGE contains the halo exchange routines.  There is a
!***  unique routine for every combination of 2-D, 3-D, and 4-D real array
!***  exchanges being done at one time.  Each subroutine name begins with
!***  "exch" which is then followed by a string of integers.  Each "2"
!***  in the string indicates exchange being done for a 2-D array.
!***  Similarly each "3" in the string indicates exchange being done for
!***  a 3-D array.
!
!***  A generic interface exists so that all of the routines
!***  may be called with the name "HALO_EXCH".  If new routines
!***  are added because new combinations are needed then also
!***  add the routine's name to the interface block.
!
!***  Buffer arrays are used during the exchange process.  Set the size
!***  below in the parameter ibufexch.  If an error occurs where the
!***  MPI library indicates that the receive buffer is too small then
!***  increase the size of ibufexch.
!
!***  The 4-element IHANDLE array is used for the nonblocking requests
!***  for all the ISENDS/IRECVS and their MPI_WAITS.  Here is the key
!***  to their use:
!***
!***  IRECV/store from north --> IHANDLE(1)
!***  IRECV/store from south --> IHANDLE(2)
!***  ISEND to north --> IHANDLE(3)
!***  ISEND to south --> IHANDLE(4)
!***
!***  IRECV/store from west --> IHANDLE(1)
!***  IRECV/store from east --> IHANDLE(2)
!***  ISEND to east --> IHANDLE(3)
!***  ISEND to west --> IHANDLE(4)
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!
      INCLUDE "mpif.h"
!
!-----------------------------------------------------------------------
!
      INTEGER,PARAMETER :: IBUFEXCH=2500000
!
      INTEGER :: IERR,ISEND,NEBPE
!
      INTEGER,DIMENSION(8) :: MY_NEB
!
      REAL,DIMENSION(IBUFEXCH) :: BUF0,BUF1,BUF2,BUF3
!
!-----------------------------------------------------------------------
!
      INTERFACE HALO_EXCH
        MODULE PROCEDURE EXCH2 
        MODULE PROCEDURE EXCH22 
        MODULE PROCEDURE EXCH222
        MODULE PROCEDURE EXCH223
        MODULE PROCEDURE EXCH23 
        MODULE PROCEDURE EXCH233 
        MODULE PROCEDURE EXCH23333 
        MODULE PROCEDURE EXCH222333
        MODULE PROCEDURE EXCH2223333
        MODULE PROCEDURE EXCH233333333
        MODULE PROCEDURE EXCH3
        MODULE PROCEDURE EXCH33
        MODULE PROCEDURE EXCH333
        MODULE PROCEDURE EXCH3333
        MODULE PROCEDURE EXCH33333
        MODULE PROCEDURE EXCH333333
        MODULE PROCEDURE EXCH4
        MODULE PROCEDURE IEXCH2 
      END INTERFACE
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH2(ARR1,LL1,IHALO,JHALO                             &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,MPI_COMM_COMP,MYPE          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(MIN(ITE+IHALO,IME),IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                   &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
      END SUBROUTINE EXCH2
!--------------------------------------------------------------------
!********************************************************************
!--------------------------------------------------------------------
!
      SUBROUTINE EXCH3(ARR1,LL1,IHALO,JHALO                             &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,MPI_COMM_COMP,MYPE          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LL1),INTENT(INOUT) :: ARR1
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME) 
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0 
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH3
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH23(ARR1,LL1,ARR2,LL2,IHALO,JHALO                   &
     &                 ,MYPE,MPI_COMM_COMP                              &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,MPI_COMM_COMP,MYPE      &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1 
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH23
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH223(ARR1,LL1,ARR2,LL2,ARR3,LL3,IHALO,JHALO         &
     &                  ,MYPE,MPI_COMM_COMP                             &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,MPI_COMM_COMP,MYPE  &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME,LL3),INTENT(INOUT) :: ARR3
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J)
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J)
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1 
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH223
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH22(ARR1,LL1,ARR2,LL2,IHALO,JHALO                   &
     &                 ,MYPE,MPI_COMM_COMP                              &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,MPI_COMM_COMP,MYPE      &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR2
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH22
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH222(ARR1,LL1,ARR2,LL2,ARR3,LL3,IHALO,JHALO         &
     &                  ,MYPE,MPI_COMM_COMP                             &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,MPI_COMM_COMP,MYPE  &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR3
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  & 
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH222
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH33(ARR1,LL1,ARR2,LL2,IHALO,JHALO                   &
     &                 ,MYPE,MPI_COMM_COMP                              &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,MPI_COMM_COMP,MYPE      &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LL1),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1 
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH33
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------

      SUBROUTINE EXCH333(ARR1,LL1,ARR2,LL2,ARR3,LL3,IHALO,JHALO         &
     &                  ,MYPE,MPI_COMM_COMP                             &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,MPI_COMM_COMP,MYPE  &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LL1),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME,LL3),INTENT(INOUT) :: ARR3
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3) &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        NEBPE=MY_NEB(4)
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH333
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------

      SUBROUTINE EXCH3333(ARR1,LL1,ARR2,LL2,ARR3,LL3                    &
     &                   ,ARR4,LL4,IHALO,JHALO                          &
     &                   ,MYPE,MPI_COMM_COMP                            &
     &                   ,IDS,IDE,JDS,JDE,KDS,KDE                       &
     &                   ,IMS,IME,JMS,JME,KMS,KME                       &
     &                   ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,LL4,MPI_COMM_COMP,MYPE &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LL1),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME,LL3),INTENT(INOUT) :: ARR3
      REAL,DIMENSION(IMS:IME,JMS:JME,LL4),INTENT(INOUT) :: ARR4
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        NEBPE=MY_NEB(2)
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH3333
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------

      SUBROUTINE EXCH33333(ARR1,LL1,ARR2,LL2,ARR3,LL3                   &
     &                    ,ARR4,LL4,ARR5,LL5,IHALO,JHALO                &
     &                    ,MYPE,MPI_COMM_COMP                           &
     &                    ,IDS,IDE,JDS,JDE,KDS,KDE                      &
     &                    ,IMS,IME,JMS,JME,KMS,KME                      &
     &                    ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,LL4,LL5             &
     &                     ,MPI_COMM_COMP,MYPE                          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LL1),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME,LL3),INTENT(INOUT) :: ARR3
      REAL,DIMENSION(IMS:IME,JMS:JME,LL4),INTENT(INOUT) :: ARR4
      REAL,DIMENSION(IMS:IME,JMS:JME,LL5),INTENT(INOUT) :: ARR5
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH33333
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------

      SUBROUTINE EXCH333333(ARR1,LL1,ARR2,LL2,ARR3,LL3,ARR4,LL4         &
     &                     ,ARR5,LL5,ARR6,LL6,IHALO,JHALO               &
     &                     ,MYPE,MPI_COMM_COMP                          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,LL4,LL5,LL6         &
     &                     ,MPI_COMM_COMP,MYPE                          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LL1),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME,LL3),INTENT(INOUT) :: ARR3
      REAL,DIMENSION(IMS:IME,JMS:JME,LL4),INTENT(INOUT) :: ARR4
      REAL,DIMENSION(IMS:IME,JMS:JME,LL5),INTENT(INOUT) :: ARR5
      REAL,DIMENSION(IMS:IME,JMS:JME,LL6),INTENT(INOUT) :: ARR6
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH333333
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH233(ARR1,LL1,ARR2,LL2,ARR3,LL3,IHALO,JHALO         &
     &                  ,MYPE,MPI_COMM_COMP                             &
     &                  ,IDS,IDE,JDS,JDE,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3                     &
     &                     ,MPI_COMM_COMP,MYPE                          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME,LL3),INTENT(INOUT) :: ARR3
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH233
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH23333(ARR1,LL1,ARR2,LL2,ARR3,LL3,ARR4,LL4          &
     &                    ,ARR5,LL5,IHALO,JHALO                         &
     &                    ,MYPE,MPI_COMM_COMP                           &
     &                    ,IDS,IDE,JDS,JDE,KDS,KDE                      &
     &                    ,IMS,IME,JMS,JME,KMS,KME                      &
     &                    ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,LL4,LL5             &
     &                     ,MPI_COMM_COMP,MYPE                          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME,LL3),INTENT(INOUT) :: ARR3
      REAL,DIMENSION(IMS:IME,JMS:JME,LL4),INTENT(INOUT) :: ARR4
      REAL,DIMENSION(IMS:IME,JMS:JME,LL5),INTENT(INOUT) :: ARR5
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------

      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH23333
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH222333(ARR1,LL1,ARR2,LL2,ARR3,LL3                  &
     &                     ,ARR4,LL4,ARR5,LL5                           &
     &                     ,ARR6,LL6,IHALO,JHALO                        &
     &                     ,MYPE,MPI_COMM_COMP                          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,LL4,LL5,LL6         &
     &                     ,MPI_COMM_COMP,MYPE                          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR3
      REAL,DIMENSION(IMS:IME,JMS:JME,LL4),INTENT(INOUT) :: ARR4
      REAL,DIMENSION(IMS:IME,JMS:JME,LL5),INTENT(INOUT) :: ARR5
      REAL,DIMENSION(IMS:IME,JMS:JME,LL6),INTENT(INOUT) :: ARR6
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH222333
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH2223333(ARR1,LL1,ARR2,LL2,ARR3,LL3                 &
     &                      ,ARR4,LL4,ARR5,LL5                          &
     &                      ,ARR6,LL6,ARR7,LL7,IHALO,JHALO              &
     &                      ,MYPE,MPI_COMM_COMP                         &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,LL4,LL5,LL6,LL7     &
     &                     ,MPI_COMM_COMP,MYPE                          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR3
      REAL,DIMENSION(IMS:IME,JMS:JME,LL4),INTENT(INOUT) :: ARR4
      REAL,DIMENSION(IMS:IME,JMS:JME,LL5),INTENT(INOUT) :: ARR5
      REAL,DIMENSION(IMS:IME,JMS:JME,LL6),INTENT(INOUT) :: ARR6
      REAL,DIMENSION(IMS:IME,JMS:JME,LL7),INTENT(INOUT) :: ARR7
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR7(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J)
        ENDDO
        ENDDO
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR7(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH2223333
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE EXCH233333333(ARR1,LL1,ARR2,LL2,ARR3,LL3               &
     &                        ,ARR4,LL4,ARR5,LL5,ARR6,LL6               &
     &                        ,ARR7,LL7,ARR8,LL8,ARR9,LL9,IHALO,JHALO   &
     &                        ,MYPE,MPI_COMM_COMP                       &
     &                        ,IDS,IDE,JDS,JDE,KDS,KDE                  &
     &                        ,IMS,IME,JMS,JME,KMS,KME                  &
     &                        ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,LL4,LL5,LL6,LL7     &
     &                     ,LL8,LL9,MPI_COMM_COMP,MYPE                  &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME,LL3),INTENT(INOUT) :: ARR3
      REAL,DIMENSION(IMS:IME,JMS:JME,LL4),INTENT(INOUT) :: ARR4
      REAL,DIMENSION(IMS:IME,JMS:JME,LL5),INTENT(INOUT) :: ARR5
      REAL,DIMENSION(IMS:IME,JMS:JME,LL6),INTENT(INOUT) :: ARR6
      REAL,DIMENSION(IMS:IME,JMS:JME,LL7),INTENT(INOUT) :: ARR7
      REAL,DIMENSION(IMS:IME,JMS:JME,LL8),INTENT(INOUT) :: ARR8
      REAL,DIMENSION(IMS:IME,JMS:JME,LL9),INTENT(INOUT) :: ARR9
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR7(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL8
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR8(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL9
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR9(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR7(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL8
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR8(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL9
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR9(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL8
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR8(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL9
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR9(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL8
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR8(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL9
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR9(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL8
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR8(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL9
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR9(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL8
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR8(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL9
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR9(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL8
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR8(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL9
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR9(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL8
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR8(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL9
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR9(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH233333333
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH3333333(ARR1,LL1,ARR2,LL2,ARR3,LL3                 &
     &                      ,ARR4,LL4,ARR5,LL5                          &
     &                      ,ARR6,LL6,ARR7,LL7                          &
     &                      ,IHALO,JHALO                                &
     &                      ,MYPE,MPI_COMM_COMP                         &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,LL2,LL3,LL4,LL5,LL6,LL7     &
     &                     ,MPI_COMM_COMP,MYPE                          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LL1),INTENT(INOUT) :: ARR1
      REAL,DIMENSION(IMS:IME,JMS:JME,LL2),INTENT(INOUT) :: ARR2
      REAL,DIMENSION(IMS:IME,JMS:JME,LL3),INTENT(INOUT) :: ARR3
      REAL,DIMENSION(IMS:IME,JMS:JME,LL4),INTENT(INOUT) :: ARR4
      REAL,DIMENSION(IMS:IME,JMS:JME,LL5),INTENT(INOUT) :: ARR5
      REAL,DIMENSION(IMS:IME,JMS:JME,LL6),INTENT(INOUT) :: ARR6
      REAL,DIMENSION(IMS:IME,JMS:JME,LL7),INTENT(INOUT) :: ARR7
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR7(I,JTE-J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR7(I,JTS+J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL2
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR2(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL3
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR3(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL4
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR4(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL5
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR5(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL6
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR6(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
        DO K=1,LL7
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR7(I,J,K)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!-----------------------------------------------------------------------
      END SUBROUTINE EXCH3333333
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      SUBROUTINE EXCH4(ARR1,LL1,NL1,IHALO,JHALO                         &
     &                ,MYPE,MPI_COMM_COMP                               &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,NL1,MPI_COMM_COMP,MYPE      &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LL1,NL1),INTENT(INOUT) :: ARR1
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND,K,N
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO N=1,NL1
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,JTE-J,K,N)
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME) 
        IC=0
!
        DO N=1,NL1
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,JTS+J,K,N)
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO N=1,NL1
        DO K=1,LL1
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K,N)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO N=1,NL1
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K,N)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO N=1,NL1
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=ARR1(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO N=1,NL1
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=ARR1(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0 
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO N=1,NL1
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K,N)=BUF0(IC)
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO N=1,NL1
        DO K=1,LL1
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          ARR1(I,J,K,N)=BUF1(IC)
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
      END SUBROUTINE EXCH4
!--------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!-----------------------------------------------------------------------
!
      SUBROUTINE IEXCH2(IARR1,LL1,IHALO,JHALO                           &
     &                 ,MYPE,MPI_COMM_COMP                              &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IHALO,JHALO,LL1,MPI_COMM_COMP,MYPE          &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: IARR1
!
!-----------------------------------------------------------------------
!
      INTEGER :: I,IBEG,IC,IEND,IRECV,ITYPE,J,JBEG,JEND
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: ISTAT
      INTEGER,DIMENSION(4) :: IHANDLE
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ITYPE=MPI_REAL
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  NORTH/SOUTH
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(1),MY_NEB(1)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(3),MY_NEB(3)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO NORTH
!--------------------------------------------------------------------
!     
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(MIN(ITE+IHALO,IME),IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=IARR1(I,JTE-J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(1),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO SOUTH
!--------------------------------------------------------------------
!    
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        IC=0
!
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=IARR1(I,JTS+J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(3),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!    
!--------------------------------------------------------------------
!     STORE RESULTS FROM SOUTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(3)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTS-1
        JEND=MAX(JTS-JHALO,JMS)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND,-1
        DO I=IBEG,IEND
          IC=IC+1
          IARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM NORTH
!--------------------------------------------------------------------
!
      IF(MY_NEB(1)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=MIN(ITE+IHALO,IME)
        JBEG=JTE+1
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          IARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(1)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(3)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!***
!***  EAST/WEST
!***
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!
!--------------------------------------------------------------------
!     RECEIVE FROM WEST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_IRECV(BUF0,IBUFEXCH,ITYPE,MY_NEB(4),MY_NEB(4)       &
     &                ,MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     RECEIVE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_IRECV(BUF1,IBUFEXCH,ITYPE,MY_NEB(2),MY_NEB(2)       &
     &                ,MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO EAST
!--------------------------------------------------------------------
!      
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE-IHALO+1
        IEND=ITE
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF2(IC)=IARR1(I,J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF2,IC,ITYPE,MY_NEB(2),MYPE                  &
     &                ,MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     SEND TO WEST
!--------------------------------------------------------------------
!       
      IF(MY_NEB(4)>=0)THEN
        IBEG=ITS
        IEND=ITS+IHALO-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          BUF3(IC)=IARR1(I,J)
        ENDDO
        ENDDO
!
        CALL MPI_ISEND(BUF3,IC,ITYPE,MY_NEB(4),MYPE                   &
     &                ,MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
!
!--------------------------------------------------------------------
!     STORE FROM WEST
!--------------------------------------------------------------------
!
      IF(MY_NEB(4)>=0)THEN
        IBEG=MAX(ITS-IHALO,IMS)
        IEND=ITS-1
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          IARR1(I,J)=BUF0(IC)
        ENDDO
        ENDDO
!
      ENDIF
!   
!--------------------------------------------------------------------
!     STORE FROM EAST
!--------------------------------------------------------------------
!    
      IF(MY_NEB(2)>=0)THEN
        IBEG=ITE+1
        IEND=MIN(ITE+IHALO,IME)
        JBEG=MAX(JTS-JHALO,JMS)
        JEND=MIN(JTE+JHALO,JME)
        IC=0
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!
        DO J=JBEG,JEND
        DO I=IBEG,IEND
          IC=IC+1
          IARR1(I,J)=BUF1(IC)
        ENDDO
        ENDDO
!
      ENDIF
!
!***  BE SURE SENDS ARE COMPLETE.
!
      IF(MY_NEB(4)>=0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
      ENDIF
!
      IF(MY_NEB(2)>=0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
      ENDIF
!
!--------------------------------------------------------------------
      END SUBROUTINE IEXCH2
!--------------------------------------------------------------------
!********************************************************************
!--------------------------------------------------------------------
!
      END MODULE MODULE_EXCHANGE
!
!--------------------------------------------------------------------
