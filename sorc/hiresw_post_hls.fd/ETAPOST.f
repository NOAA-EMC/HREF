      PROGRAM ETAPOST
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C MAIN PROGRAM: ETA_ETAPOST
C   PRGMMR: MANIKIN          ORG: NP22        DATE: 2000-02-01
C     
C ABSTRACT:  
C     THIS PROGRAM DRIVES THE EXTERNAL ETA MODEL POST PROCESSOR.
C     
C PROGRAM HISTORY LOG:
C   92-12-24  RUSS TREADON - CODED ETAPOST AS STAND ALONE CODE
C   98-03-06  BALDWIN/BLACK/ROGERS - MODIFIED TO POST MULTIPLE
C             FORECAST HOURS IN ONE EXECUTION; NUMBER AND 
C             FREQUENCY DETERMINED BY UNIT 5 INPUT CARD
C   98-05-29  BLACK - CONVERSION OF POST CODE FROM 1-D TO 2-D
C   00-02-04  JIM TUCCILLO - PARALLEL VERSION VIA MPI
C   01-02-15  JIM TUCCILLO - MANY COMMON BLOCKS REPLACED WITH MODULES
C             TO SUPPORT FORTRAN "ALLOCATE"s FOR THE EXACT SIZE OF THE 
C             ARRAYS NEEDED BASED ON THE NUMBER OF MPI TASKS.
C             THIS WAS DONE TO REDUCE THE ADDRESS SPACE THAT THE LOADER SEES.
C             THESE CHANGES WERE NECESSARY FOR RUNNING LARGER DOMAINS SUCH AS
C             12 KMS
C   01-06-15  JIM TUCCILLO - ADDED ASYNCRONOUS I/O CAPABILITY. IF THERE ARE MORE
C             THAN ONE MPI TASK, THE IO WILL BE DONE AYNCHRONOUSLY BY THE LAST
C             MPI TASK.
C     
C USAGE:    ETAPOST
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - IOUNIT
C                RQSTFLD
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM RS/6000 SP
C$$$  
C
C
C============================================================================================================
C
C     This is an MPI code. All array indexing is with respect to the global indices. Loop indices 
C     look as follows for N MPI tasks.
C
C
C
C  Original                                            New
C  Index                                               Index
C
C   JM ----------------------------------------------- JEND
C JM-1 -                                             - JEND_M
C JM-2 -               MPI TASK N-1                  - JEND_M2
C      -                                             -
C      -                                             -
C      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
C      ----------------------------------------------- JEND, JEND_M, JEND_M2
C      -                                             -
C      -               MPI TASK N-2                  -
C      -                                             -
C      -                                             -
C      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
C
C                           .
C                           .
C                           .
C
C      ----------------------------------------------- JEND, JEND_M, JEND_M2
C      -                                             -
C      -               MPI TASK 1                    -
C      -                                             -
C      -                                             -
C      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
C      ----------------------------------------------- JEND, JEND_M, JEND_M2
C      -                                             - 
C      -               MPI TASK 0                    - 
C    3 -                                             - JSTA_M2
C    2 -                                             - JSTA_M
C    1 ----------------------------------------------- JSTA
C
C     1                                              IM               
C
C
C     Jim Tuccillo
C     Jan 2000
C
C     README - Jim Tuccillo Feb 2001
C 
C     Many common blocks have been replaced by modules to support Fortran
C     "allocate" commands. Many of the 3-D arrays are now allocated to be the
C     exact size required based on the number of MPI tasks. The dimensioning will be 
C        x ( im,jsta_2l:jend_2u,lm)
C     Most 2-D arrays continue to be dimensioned (im,jm). This is fine but please be aware 
C     that the EXCH routine for arrays dimensioned (im,jm) is different than arrays dimensioned
C     (im,jsta_2l:jend_2u). Also, be careful about passing any arrays dimensioned
C     (im,jst_2l:jend_2u,lm). See examples in the code as to the correct calling sequence and
C     EXCH routine to use.
C
C
C     ASYNCHRONOUS I/O HAS BEEN ADDED. THE LAST MPI TASK DOES THE I/O. IF THERE IS
C     ONLY ONE MPI TASK THN TASK ) DOES THE I/O.
C     THE CODE HAS GOTTEN A LITTLE KLUDGY. BASICLY, IM, IMX and IMOUT MUST BE EQUAL
C     AND REPRESENT THE VALUE USED IN THE MODEL. THE SAME HOLDS FOR JM, JMX and JMOUT.
C
C     Jim Tuccillo June 2001
C
C
C===========================================================================================
C
C     INCLUDE ARRAY DIMENSIONS.
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "mpif.h"
C
C     DECLARE VARIABLES.
C     
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "OUTFIL.comm"
      INCLUDE "IOUNIT.comm"
      INCLUDE "CTLBLK.comm"
C     
C     SET HEADER WRITER FLAGS TO TRUE.
c
      common/tim_info/ETAFLD2_tim,ETA2P_tim,SURFCE2_tim, CLDRAD_tim,
     *                MISCLN_tim,FIXED_tim
C
      common/jjt/time_output, time_e2out
      real(8) time_output, time_e2out, time_initpost, rtc, ist
C
C     START HERE
C
      call start()
C
C     INITIALIZE MPI
      
      CALL SETUP_SERVERS(ME,
     *                   NUM_PROCS,
     *                   NUM_SERVERS,
     *                   MPI_COMM_COMP,
     *                   MPI_COMM_INTER)
C
C     ME IS THE RANK
C     NUM_PROCS IS THE NUMBER OF TASKS DOING POSTING
C     NUM_SERVERS IS ONE IF THERE ARE MORE THAN ONE TOTAL MPI TASKS, OTHERWISE ZERO
C     MPI_COMM_COMP IS THE INTRACOMMUNICATOR
C     MPI_COMM_INTER IS THE INTERCOMMUNICATOR FOR COMMUNCATION BETWEEN TASK 0 OF THE
C        TASKS DOING THE POSTING AND THE I/O SERVER
C
C
C     IF WE HAVE MORE THAN 1 MPI TASK THEN WE WILL FIRE UP THE IO SERVER
C     THE LAST TASK ( IN THE CONTEXT OF MPI_COMM_WORLD ) IS THE I/O SERVER
C
      if ( me .ge. num_procs ) then
C
         call server
C
      else
C
      time_output = 0.
      time_e2out = 0.
      time_initpost = 0.
      CALL MPI_FIRST
C
      ETAFLD2_tim = 0.0
      ETA2P_tim = 0.0
      SURFCE2_tim = 0.0
      CLDRAD_tim = 0.0
      MISCLN_tim =0.0
      FIXED_tim =  0.0
      bbtim = timef()
c
C     
C
c     IF(ME.EQ.0)THEN
c       CALL W3TAGB('ETA_ETAPOST',2000,0032,0094,'NP22')
c     ENDIF
C
C
C**************************************************************************
C
C     START PROGRAM ETAPOST.
C
      IF(ME.EQ.0)THEN
        READ(5,*)ITAG,NRSTRT,NPINCR
      ENDIF
C
      CALL MPI_BCAST(ITAG  ,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(NRSTRT,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
      CALL MPI_BCAST(NPINCR,1,MPI_INTEGER,0,MPI_COMM_COMP,IRTN)
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C
C     LOOP OVER THE RESTRT FILES
C
      DO 1000 NR=1,NRSTRT
      LUNCO=19
      LUNLL=29
C
C     REWIND ALL INPUT FILE UNIT NUMBERS (FOR CONTROL FILE 
C     AND INTERPOLATION WEIGHTS)
C
      REWIND LCNTRL
      DO KER=19,39
        REWIND KER
      ENDDO
C
C     INITIALIZE POST COMMON BLOCKS 
C
      ist = rtc()
      CALL INITPOST
      time_initpost = time_initpost + rtc() - ist
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)'ETAPOST:  INITIALIZED POST COMMON BLOCKS'
      ENDIF
C
C     LOOP OVER THE OUTPUT GRID(S).  FIELD(S) AND 
C     OUTPUT GRID(S) ARE SPECIFIED IN THE CONTROL 
C     FILE.  WE PROCESS ONE GRID AND ITS FIELDS 
C     AT A TIME.  THAT'S WHAT THIS LOOP DOES.
C     
 10   CONTINUE
C     
C        READ CONTROL FILE DIRECTING WHICH FIELDS ON WHICH
C        LEVELS AND TO WHICH GRID TO INTERPOLATE DATA TO.
C        VARIABLE IEOF.NE.0 WHEN THERE ARE NO MORE GRIDS
C        TO PROCESS.
C     
         CALL READCNTRL2(IEOF)
         IF(ME.EQ.0)THEN
           WRITE(STDOUT,*)'ETAPOST:  RETURN FROM READCNTRL.  ',
     1          'IEOF=',IEOF
         ENDIF
         IF (IEOF.NE.0) GOTO 20
C     
C        PROCESS SELECTED FIELDS.  FOR EACH SELECTED FIELD/LEVEL
C        WE GO THROUGH THE FOLLOWING STEPS:
C           (1) COMPUTE FIELD IF NEED BE
C           (2) INTERPOLATE FROM E-GRID TO OUTPUT GRID.
C           (3) WRITE FIELD TO OUTPUT FILE IN REQUESTED FORMAT.
C
         CALL PROCESS
         IF(ME.EQ.0)THEN
           WRITE(STDOUT,*)' '
           WRITE(STDOUT,*)'ETAPOST:  PREPARE TO PROCESS NEXT GRID'
         ENDIF
C     
C     PROCESS NEXT GRID.
C     
      GO TO 10
C     
C     ALL GRIDS PROCESSED.
C     
 20   CONTINUE
      IF(ME.EQ.0)THEN
        WRITE(STDOUT,*)' '
        WRITE(STDOUT,*)'ALL GRIDS PROCESSED.'
        WRITE(STDOUT,*)' '
      ENDIF
C
      ITAG=ITAG+NPINCR
 1000 CONTINUE
C
      print*, 'ETAFLD2_tim = ',  ETAFLD2_tim*1.0e-3
      print*, 'ETA2P_tim =  ',ETA2P_tim *1.0e-3
      print*, 'SURFCE2_tim =  ',SURFCE2_tim*1.0e-3
      print*, 'CLDRAD_tim =  ',CLDRAD_tim *1.0e-3
      print*, 'MISCLN_tim = ',MISCLN_tim*1.0e-3
      print*, 'FIXED_tim = ',FIXED_tim*1.0e-3
      print*, 'Total time = ',(timef() - bbtim) * 1.0e-3
      print*, 'Time for OUTPUT = ',time_output
      print*, 'Time for E2OUT = ',time_e2out
      print*, 'Time for INITPOST = ',time_initpost

C     
C     END OF PROGRAM.
C
C
c     IF(ME.EQ.0)THEN
c       CALL W3TAGE('ETA_ETAPOST')
c     ENDIF
C
C     MPI_LAST WILL SHUTDOWN THE IO SERVER, IF IT EXISTS
C
      CALL MPI_LAST
C
C
      end if
C
C
C
      call summary()
      CALL MPI_FINALIZE(MPI_COMM_WORLD,IERR)
      STOP0
      END

