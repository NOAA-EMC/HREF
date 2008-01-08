      PROGRAM WRFPOST
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C MAIN PROGRAM: WRFPOST
C   PRGMMR: BALDWIN          ORG: NSSL/SPC    DATE: 2002-06-18
C     
C ABSTRACT:  
C     THIS PROGRAM DRIVES THE EXTERNAL WRF POST PROCESSOR.
C     
C PROGRAM HISTORY LOG:
C   92-12-24  RUSS TREADON - CODED ETAPOST AS STAND ALONE CODE
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
C   02-06-17  MIKE BALDWIN - CONVERT ETAPOST TO WRFPOST.  INCLUDE WRF I/O API
C             FOR INPUT OF MODEL DATA.  MODIFY CODE TO DEAL WITH C-GRID
C             DATA.  STREAMLINE OUTPUT TO A CALL OF ONE SUBROUTINE INSTEAD OF THREE.
C             REPLACE COMMON BLOCKS WITH A LIMITED NUMBER OF MODULES.
C   04-01-01  H CHUANG - ADDED NMM IO MODULE AND BINARY OPTIONS
C   05-07-08  Binbin Zhou: Aadded RSM model
C   05-12-05  H CHUANG - ADDED CAPABILITY TO OUTPUT OFF-HOUR FORECAST WHICH HAS
c               NO IMPACTS ON ON-HOUR FORECAST
C   06-02-20  CHUANG, BLACK, AND ROGERS - FINALIZED COMPLETE LIST OF NAM
C             OPERATIONAL PRODUCTS FROM WRF
C   06-02-27  H CHUANG - MODIFIED TO POST MULTIPLE
C             FORECAST HOURS IN ONE EXECUTION
C   06-03-03  H CHUANG - ADDED PARRISH'S MPI BINARY IO TO READ BINARY
C             WRF FILE AS RANDOM ASSCESS SO THAT VARIABLES IN WRF OUTPUT
C             DON'T HAVE TO BE READ IN IN SPECIFIC ORDER 
C  
C USAGE:    WRFPOST
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - CTLBLK
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
!      INCLUDE "parmeta"
!      INCLUDE "parmout"
      INCLUDE "mpif.h"
C
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
C
C     DECLARE VARIABLES.
C     
C     SET HEADER WRITER FLAGS TO TRUE.
c
      common/tim_info/ETAFLD2_tim,ETA2P_tim,SURFCE2_tim, CLDRAD_tim,
     *                MISCLN_tim,FIXED_tim,MDL2SIGMA_tim
C
      common/jjt/time_output, time_e2out
!      real(8) time_output, time_e2out, time_initpost, rtc, ist
      real rinc(5)
chc      integer jdate(8),idate(8)
      integer iii
      integer :: Status
      character startdate*19,SysDepInfo*80,IOWRFNAME*3
      character(len=20) :: IOFORM
!      character(len=4) :: MODELNAME 
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
      print*,'ME,NUM_PROCS,NUM_SERVERS=',ME,NUM_PROCS,NUM_SERVERS
      if ( me .ge. num_procs ) then
C
         call server
C
      else
C
      time_output = 0.
      time_e2out = 0.
      time_initpost = 0.
C
      INITPOST_tim = 0.
      ETAFLD2_tim = 0.0
      ETA2P_tim = 0.0
      MDL2SIGMA_tim = 0.0
      SURFCE2_tim = 0.0
      CLDRAD_tim = 0.0
      MISCLN_tim =0.0
      FIXED_tim =  0.0
!      bbtim = timef()
C**************************************************************************
C
C     START PROGRAM WRFPOST.
C
 98      read(5,111,end=1000) fileName
      read(5,113) IOFORM
      print*,'IOFORM= ',IOFORM
      read(5,112) DateStr
! 98   read(5,114,end=1000) MODELNAME
      read(5,114) MODELNAME 
! assume for now that the first date in the stdin file is the start date
      read(DateStr,300) iyear,imn,iday,ihrst,imin
      write(*,*) 'in WRFPOST iyear,imn,iday,ihrst,imin'
     +      ,iyear,imn,iday,ihrst,imin
 300  format(i4,1x,i2,1x,i2,1x,i2,1x,i2)
         IDAT(1)=imn
         IDAT(2)=iday
         IDAT(3)=iyear
         IDAT(4)=ihrst
	 IDAT(5)=imin
 111  format(a256)
 112  format(a19)
 113  format(a20)
 114  format(a4)

!Chuang, Jun and Binbin: If model is RSM, read in precip accumulation frequency (sec) from unit5
      if(MODELNAME .EQ. 'RSM')then
       read(5,115)PRNTSEC
       TPREC=PRNTSEC/3600.0
       print*,'TPREC in RSM= ',TPREC
      end if
 115  format(f7.1)

!Chuang: add dynamical allocation
      if(TRIM(IOFORM) .EQ. 'netcdf')THEN
       call ext_ncd_ioinit(SysDepInfo,Status)
       print*,'called ioinit', Status
       call ext_ncd_open_for_read( trim(fileName), 0, 0, " ", 
     &   DataHandle, Status)
       print*,'called open for read', Status
       if ( Status /= 0 ) then
         print*,'error opening ',fileName, ' Status = ', Status ; stop
       endif
       call ext_ncd_get_dom_ti_integer(DataHandle
     1 ,'WEST-EAST_GRID_DIMENSION',iim,1,ioutcount, status )
       im=iim-1
       call ext_ncd_get_dom_ti_integer(DataHandle
     1 ,'SOUTH-NORTH_GRID_DIMENSION',jjm,1,ioutcount, status )
       jm=jjm-1
       call ext_ncd_get_dom_ti_integer(DataHandle
     1 ,'BOTTOM-TOP_GRID_DIMENSION',llm,1,ioutcount, status )
       lm=llm-1
       LP1=LM+1
       LM1=LM-1
       IM_JM=IM*JM
       
       print*,'im jm lm from wrfout= ',im,jm,lm
       

       call ext_ncd_get_dom_ti_integer(DataHandle
     1    ,'SF_SURFACE_PHYSICS',itmp,1,ioutcount, status )
! set NSOIL to 4 as default for NOAH but change if using other
! SFC scheme
       NSOIL=4
       IF(itmp.eq.1)then !thermal diffusion scheme
         NSOIL=5
       ELSE IF(itmp.eq.3)then ! RUC LSM
         NSOIL=6
       END IF
       print*,'NSOIL from wrfout= ',NSOIL

       call ext_ncd_ioclose ( DataHandle, Status )

       
      else if(TRIM(IOFORM) .EQ. 'binary' .OR. 
     &   TRIM(IOFORM) .EQ. 'binarympiio'    )THEN
      
!       call ext_int_ioinit(SysDepInfo,Status)
       call ext_int_ioinit(SysDepInfo,Status)
       print*,'called ioinit', Status
       call ext_int_open_for_read( trim(fileName), 0, 0, " ", 
     &   DataHandle, Status)
       print*,'called open for read', Status
       if ( Status /= 0 ) then
         print*,'error opening ',fileName, ' Status = ', Status ; stop
       endif

       call ext_int_get_dom_ti_integer(DataHandle
     1 ,'WEST-EAST_GRID_DIMENSION',iim,1,ioutcount, status )
       if ( Status /= 0 ) then
         print*,'error getting grid dim '; stop
       endif
       im=iim-1
       call ext_int_get_dom_ti_integer(DataHandle
     1 ,'SOUTH-NORTH_GRID_DIMENSION',jjm,1,ioutcount, status )
       jm=jjm-1
       call ext_int_get_dom_ti_integer(DataHandle
     1 ,'BOTTOM-TOP_GRID_DIMENSION',llm,1,ioutcount, status )
       lm=llm-1
       LP1=LM+1
       LM1=LM-1
       IM_JM=IM*JM
       print*,'im jm lm from wrfout= ',im,jm,lm
       
       IF(MODELNAME .EQ. 'RSM') THEN
         NSOIL=2
       ELSE	   
        call ext_int_get_dom_ti_integer(DataHandle
     1    ,'SF_SURFACE_PHYSICS',itmp,1,ioutcount, status )
! set NSOIL to 4 as default for NOAH but change if using other 
! SFC scheme
        NSOIL=4
        IF(itmp.eq.1)then !thermal diffusion scheme
         NSOIL=5
        ELSE IF(itmp.eq.3)then ! RUC LSM
         NSOIL=6
        END IF
       END IF	
       print*,'NSOIL from wrfout= ',NSOIL
       call ext_int_ioclose ( DataHandle, Status )
      ELSE
       PRINT*,'UNKNOWN WRF OUTPUT FORMAT, STOPPING'
       STOP 9999
      END IF  

      CALL MPI_FIRST
      print*,'jsta,jend,jsta_m,jend_m,jsta_2l,jend_2u=',jsta,jend
     1,jsta_m,jend_m,jsta_2l,jend_2u
     
c 30   read(5,112,end=1000) DateStr
c      read(DateStr,300) iyear,imn,iday,ihr
c         jdate=0
c         idate=0
c         jdate(1)=iyear
c         jdate(2)=imn
c         jdate(3)=iday
c         jdate(5)=ihr
c         idate(1)=idat(3)
c         idate(2)=idat(1)
c         idate(3)=idat(2)
c         idate(5)=ihrst
c         CALL W3DIFDAT(JDATE,IDATE,2,RINC)
c         ifhr=nint(rinc(2))
c         write(*,*) ' in WRFPOST ifhr ',ifhr
C
C     INITIALIZE POST COMMON BLOCKS 
C
      LCNTRL=14
      REWIND(LCNTRL)
!
!--- Initialize a few constants for new cloud fields (Ferrier, Feb '02)
!
      CALL MICROINIT
!
C EXP. initialize netcdf here instead
cexp      call ext_ncd_ioinit(Status)
cexp      call ext_ncd_open_for_read( trim(fileName), 0, 0, " ",
cexp     &  DataHandle, Status)
cexp       if ( Status /= 0 ) then
cexp         print*,'error opening ',fileName, ' Status = ', Status ; stop
cexp       endif
C Exp
!      ist = rtc()
c      IOFORM='netcdf'
      btim = timef()
      IF(TRIM(IOFORM) .EQ. 'netcdf')THEN
       IF(MODELNAME .EQ. 'NCAR')THEN
        print*,'CALLING INITPOST TO PROCESS NCAR NETCDF OUTPUT'
        CALL INITPOST
       ELSE IF(MODELNAME .EQ. 'NMM')THEN
        print*,'CALLING INITPOST_NMM TO PROCESS NMM NETCDF OUTPUT'
        CALL INITPOST_NMM
       ELSE
        PRINT*,'UNKNOW WRF MODEL NAME, STOPPING'
        STOP 9998
       END IF
      ELSE IF(TRIM(IOFORM) .EQ. 'binary')THEN
       IF(MODELNAME .EQ. 'NCAR')THEN
        print*,'CALLING INITPOST_BIN TO PROCESS NCAR BINARY OUTPUT'
        CALL INITPOST_BIN
       ELSE IF (MODELNAME .EQ. 'NMM')THEN
        print*,'CALLING INITPOST_NMM_BIN TO PROCESS NMM BINARY OUTPUT'
        CALL INITPOST_NMM_BIN

       ELSE IF(MODELNAME .EQ. 'RSM') THEN                            
          print*,'CALLING INITPOST_RSM TO PROCESS BINARY OUTPUT'
          CALL INITPOST_RSM

       ELSE
        PRINT*,'UNKNOW WRF MODEL NAME, STOPPING'
        STOP 9998
       END IF
      ELSE IF(TRIM(IOFORM) .EQ. 'binarympiio')THEN 
       IF(MODELNAME .EQ. 'NCAR')THEN
        print*,'MPI BINARY IO IS NOT YET INSTALLED FOR ARW, STOPPING'
	STOP 9997
       ELSE IF (MODELNAME .EQ. 'NMM')THEN
        print*,'CALLING INITPOST_NMM_BIN_MPIIO TO 
     +  PROCESS NMM BINARY OUTPUT'
        CALL INITPOST_NMM_BIN_MPIIO
       ELSE IF(MODELNAME .EQ. 'RSM') THEN                            
          print*,'MPI BINARY IO IS NOT YET INSTALLED FOR RSM, STOPPING'
          STOP 9997
       ELSE
        PRINT*,'UNKNOW WRF MODEL NAME, STOPPING'
        STOP 9998
       END IF
      ELSE
       PRINT*,'UNKNOWN WRF OUTPUT FORMAT, STOPPING'
       STOP 9999
      END IF 
      INITPOST_tim = INITPOST_tim +(timef() - btim)
      time_initpost = time_initpost + rtc() - ist
      IF(ME.EQ.0)THEN
        WRITE(6,*)'WRFPOST:  INITIALIZED POST COMMON BLOCKS'
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
         IEOF=1
         CALL READCNTRL(IEOF)
         IF(ME.EQ.0)THEN
           WRITE(6,*)'WRFPOST:  RETURN FROM READCNTRL.  ',
     1          'IEOF=',IEOF
         ENDIF
         IF (IEOF.NE.0) GOTO 20
C     
C        PROCESS SELECTED FIELDS.  FOR EACH SELECTED FIELD/LEVEL
C        WE GO THROUGH THE FOLLOWING STEPS:
C           (1) COMPUTE FIELD IF NEED BE
C           (2) WRITE FIELD TO OUTPUT FILE IN GRIB.
C
         CALL PROCESS
         IF(ME.EQ.0)THEN
           WRITE(6,*)' '
           WRITE(6,*)'WRFPOST:  PREPARE TO PROCESS NEXT GRID'
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
        WRITE(6,*)' '
        WRITE(6,*)'ALL GRIDS PROCESSED.'
        WRITE(6,*)' '
      ENDIF
C
      call DE_ALLOCATE
!      if(IOFORM .EQ. 'netcdf')THEN
!       call ext_ncd_ioclose ( DataHandle, Status )
!      else
!       call ext_int_ioclose ( DataHandle, Status )
!      end if  

      GO TO 98
 1000 CONTINUE
cexp      call ext_ncd_ioclose ( DataHandle, Status )
C
      print*, 'INITPOST_tim = ',  INITPOST_tim*1.0e-3
      print*, 'MDLFLD_tim = ',  ETAFLD2_tim*1.0e-3
      print*, 'MDL2P_tim =  ',ETA2P_tim *1.0e-3
      print*, 'MDL2SIGMA_tim =  ',MDL2SIGMA_tim *1.0e-3
      print*, 'SURFCE_tim =  ',SURFCE2_tim*1.0e-3
      print*, 'CLDRAD_tim =  ',CLDRAD_tim *1.0e-3
      print*, 'MISCLN_tim = ',MISCLN_tim*1.0e-3
      print*, 'FIXED_tim = ',FIXED_tim*1.0e-3
      print*, 'Total time = ',(timef() - bbtim) * 1.0e-3
      print*, 'Time for OUTPUT = ',time_output
      print*, 'Time for INITPOST = ',time_initpost

C     
C     END OF PROGRAM.
C
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

