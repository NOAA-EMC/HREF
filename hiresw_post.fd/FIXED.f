      SUBROUTINE FIXED
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    FIXED       POSTS FIXED FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-08-30
C     
C ABSTRACT:  THIS ROUTINE POSTS FIXED (IE, TIME INDEPENDENT)
C  ETA MODEL FIELDS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-08-30  RUSS TREADON
C   96-04-05  MIKE BALDWIN - CHANGED ALBEDO CALC
C   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-07-17  MIKE BALDWIN - REMOVED LABL84
C   00-01-05  JIM TUCCILLO - MPI VERSION
C   02-06-19  MIKE BALDWIN - WRF VERSION
C     
C USAGE:    CALL FIXED
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST: 
C     NONE 
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - LOOPS
C                  MASKS
C                  LLGRDS
C                  RQSTFLD
C                  PHYS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
      use vrbls2d
      use vrbls3d
      use masks
C     
C     INCLUDE/DECLARE PARAMETERS.
C     
!      INCLUDE "parmeta"
!      INCLUDE "parmout"
      INCLUDE "parm.tbl"
      INCLUDE "params"
!      INCLUDE "parmsoil"
C
      PARAMETER (SNOALB=0.55)
C     INCLUDE COMMON BLOCKS.
C
      INCLUDE "RQSTFLD.comm"
      INCLUDE "CTLBLK.comm"
C     
C     DECLARE VARIABLES
      REAL GRID1(IM,JM),GRID2(IM,JM)
C     
C     
C********************************************************************
C
C     START FIXED HERE.
C
C     LATITUDE (OUTPUT GRID).
      IF (IGET(048).GT.0) THEN
         DO J = JSTA,JEND
            DO I = 1,IM
               GRID1(I,J) = GDLAT(I,J)
            END DO
         END DO
         ID(1:25) = 0
         CALL GRIBIT(IGET(048),LVLS(1,IGET(048)),GRID1,IM,JM)
      ENDIF
C     
C     LONGITUDE (OUTPUT GRID). CONVERT TO EAST
      IF (IGET(049).GT.0) THEN
         DO J = JSTA,JEND
            DO I = 1,IM
             IF (GDLON(I,J) .LT. 0.)THEN            
               GRID1(I,J) = 360. + GDLON(I,J)
             ELSE
               GRID1(I,J) = GDLON(I,J)
             END IF
             IF (GRID1(I,J).GT.360.)print*,'LARGE GDLON ',
     +       i,j,GDLON(I,J)
            END DO
         END DO
         ID(1:25) = 0
         CALL GRIBIT(IGET(049),LVLS(1,IGET(049)),GRID1,IM,JM)
      ENDIF
C     
C     LAND/SEA MASK.
      IF (IGET(050).GT.0) THEN
         DO J = JSTA,JEND
         DO I = 1,IM
            GRID1(I,J) = 1. - SM(I,J)
	    IF(SICE(I,J).GT.0.1)GRID1(I,J)=0.
c           if(j.eq.jm/2)print*,'i,mask= ',i,grid1(i,j)
         ENDDO
         ENDDO
         ID(1:25) = 0
         CALL GRIBIT(IGET(050),LVLS(1,IGET(050)),GRID1,IM,JM)
      ENDIF
C     
C     SEA ICE MASK.
      IF (IGET(051).GT.0) THEN
         DO J = JSTA,JEND
         DO I = 1,IM
            GRID1(I,J) = SICE(I,J)
         ENDDO
         ENDDO
         ID(1:25) = 0
         CALL GRIBIT(IGET(051),LVLS(1,IGET(051)),GRID1,IM,JM)
      ENDIF
C     
C     MASS POINT ETA SURFACE MASK.
      IF (IGET(052).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           GRID1(I,J) = LMH(I,J)
         ENDDO
         ENDDO
         ID(1:25) = 0
         CALL GRIBIT(IGET(052),LVLS(1,IGET(052)),GRID1,IM,JM)
      ENDIF
C     
C     VELOCITY POINT ETA SURFACE MASK.
      IF (IGET(053).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           GRID1(I,J) = LMV(I,J)
         ENDDO
         ENDDO
         ID(1:25) = 0
         CALL GRIBIT(IGET(053),LVLS(1,IGET(053)),GRID1,IM,JM)
      ENDIF
C
C     SURFACE ALBEDO.
C       NO LONGER A FIXED FIELD, THIS VARIES WITH SNOW COVER
!MEB since this is not a fixed field, move this to SURFCE
C
      IF (IGET(150).GT.0) THEN
       DO J=JSTA,JEND
        DO I=1,IM
c           SNOK = AMAX1(SNO(I,J),0.0)
c           SNOFAC = AMIN1(SNOK*50.0,1.0)
c           EGRID1(I,J)=ALB(I,J)+(1.-VEGFRC(I,J))*SNOFAC
c     1                *(SNOALB-ALB(I,J))
         GRID1(I,J)=ALBEDO(I,J)
        ENDDO
       ENDDO
c       CALL E2OUT(150,000,GRID1,GRID2,GRID1,GRID2,IM,JM)
       ID(1:25) = 0
       CALL SCLFLD(GRID1,100.,IM,JM)
       CALL GRIBIT(IGET(150),LVLS(1,IGET(150)),
     X        GRID1,IM,JM)
      ENDIF
C
      IF (IGET(226).GT.0) THEN
       DO J=JSTA,JEND
        DO I=1,IM
         GRID1(I,J)=ALBASE(I,J)*100.
        ENDDO
       ENDDO
       ID(1:25) = 0
       ID(02) = 130
       CALL GRIBIT(IGET(226),LVLS(1,IGET(226)),
     X        GRID1,IM,JM)
      ENDIF
C  Max snow albedo
      IF (IGET(227).GT.0) THEN
       DO J=JSTA,JEND
       DO I=1,IM
c sea point, albedo=0.06 same as snow free albedo
        IF( (abs(SM(I,J)-1.) .lt. 1.0E-5) ) THEN
         MXSNAL(I,J)=0.06
c sea-ice point, albedo=0.60, same as snow free albedo
        ELSEIF( (abs(SM(I,J)-0.)   .lt. 1.0E-5) .AND.
     &          (abs(SICE(I,J)-1.) .lt. 1.0E-5) ) THEN
         MXSNAL(I,J)=0.60
        ENDIF
       ENDDO
       ENDDO
       
       DO J=JSTA,JEND
        DO I=1,IM
	 IF(ABS(MXSNAL(I,J)-SPVAL).GT.SMALL) 
     &    GRID1(I,J)=MXSNAL(I,J)*100.
        ENDDO
       ENDDO
       ID(1:25) = 0
       ID(02) = 130
       CALL GRIBIT(IGET(227),LVLS(1,IGET(227)),
     X        GRID1,IM,JM)
      ENDIF
C
C     SEA SURFACE TEMPERAURE.
      IF (IGET(151).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
	   IF( (abs(SM(I,J)-1.) .lt. 1.0E-5) ) THEN
             GRID1(I,J) = SST(I,J)
	   ELSE
	     GRID1(I,J) = THS(I,J)*(PINT(I,J,LM+1)/P1000)**CAPA
	   END IF  
         ENDDO
         ENDDO
         ID(1:25) = 0
         CALL GRIBIT(IGET(151),LVLS(1,IGET(151)),GRID1,IM,JM)
      ENDIF
C
C     END OF ROUTINE.
C     
      RETURN
      END

