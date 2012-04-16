      SUBROUTINE OUTPUT(IOUTYP,IFLD,ILVL,GRID,IMOUT,JMOUT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    OUTPUT      DRIVER FOR PACKING/POSTING ROUTINES.
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-24       
C     
C ABSTRACT:
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-02-26  RUSS TREADON
C   93-06-13  RUSS TREADON - ADDED COMBINED ON84 AND GRIB PACKING OPTION.
C   98-07-17  MIKE BALDWIN - REMOVED PACK84
C   00-01-04  JIM TUCCILLO - MPI VERSION
C     
C USAGE:    CALL OUTPUT(IOUTYP,IFLD,ILVL,GRID,IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IOUTYP   - INTEGER FLAG FOR TYPE OF OUTPUT.
C     IFLD     - LOCATION OF FIELD INFORMATION IN ARRAYS.
C     ILVL     - INDEX OF LEVEL (P,ETA,SFC) OF GRID.
C     GRID     - OUTPUT GRID.
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NOPACK   - WRITE DATA AS UNFORMATTED BINARY CRAY WORDS.
C       GRIBIT   - PACK/WRITE DATA IN GRIB FORMAT.
C     LIBRARY:
C       NONE
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "CTLBLK.comm"
      INCLUDE "BITMAP.comm"
C     
C     DECLARE VARIABLES.
      DIMENSION GRID(IMOUT,JMOUT), GRIDO(IM,JM)
      save grido
C
      common/jjt/time_output, time_e2out
      real(8) ist, rtc, time_output, time_e2out
      logical once
      data once /.true./
C***********************************************************
C     START OUPUT HERE.
C     
C     BRANCH TO APPROPRIATE OUTPUT ROUTINE.
C    
      ist = rtc()
      CALL COLLECT(GRID,GRIDO)
c     CALL COLLECT(IBMAP)
      IF ( ME .EQ. 0 ) THEN
         if ( once ) then
             ibmap = 1
             once = .false.
         end if
c     call stat(grido,imout,jmout)
      IF (IOUTYP.EQ.1) CALL NOPACK(IFLD,ILVL,GRIDO,IMOUT,JMOUT)
      IF (IOUTYP.EQ.3) CALL GRIBIT(IFLD,ILVL,GRIDO,IMOUT,JMOUT)
      IF (IOUTYP.EQ.5) CALL GRIBIT(IFLD,ILVL,GRIDO,IMOUT,JMOUT)
      END IF
      time_output = time_output + rtc() - ist
C
C     END OF ROUTINE.
C
      RETURN
      END
