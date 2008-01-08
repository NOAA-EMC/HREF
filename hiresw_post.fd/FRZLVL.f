      SUBROUTINE FRZLVL(ZFRZ,RHFRZ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    FRZLVL      COMPUTES FRZING LVL Z AND RH
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES THE FREEZING LEVEL HEIGHT AND RELATIVE
C     HUMIDITY AT THIS LEVEL FOR EACH MASS POINT ON THE ETA GRID.
C     THE COMPUTED FREEZING LEVEL HEIGHT IS THE MEAN SEA LEVEL
C     HEIGHT.  AT EACH MASS POINT WE MOVE UP FROM THE SURFACE TO 
C     FIND THE FIRST ETA LAYER WHERE THE TEMPERATURE IS LESS THAN
C     273.16K.  VERTICAL INTERPOLATION IN TEMPERATURE TO THE FREEZING
C     TEMPERATURE GIVES THE FREEZING LEVEL HEIGHT.  PRESSURE AND 
C     SPECIFIC HUMIDITY ARE INTERPOLATED TO THIS LEVEL AND ALONG WITH
C     THE TEMPERATURE PROVIDE THE FREEZING LEVEL RELATIVE HUMIDITY.
C     IF THE SURFACE (SKIN) TEMPERATURE IS BELOW FREEZING, THE ROUTINE
C     USES SURFACE BASED FIELDS TO COMPUTE THE RELATIVE HUMIDITY.
C     
C     NOTE THAT IN POSTING FREEZING LEVEL DATA THE LFM LOOK-ALIKE FILE
C     (IE, GRID 26), WE PACK 273.15K AS THE FREEZING TEMPERATURE.  ALL
C     OTHER OUTPUT GRIDS USE 273.16K
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-22  RUSS TREADON
C   93-06-10  RUSS TREADON - CORRECTED FREEZING LEVEL HEIGHTS TO BE
C                            WITH REPSECT TO MEAN SEA LEVEL, NOT  
C                            ABOVE GROUND LEVEL.
C   98-06-15  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-08-17  MIKE BALDWIN - COMPUTE RH OVER ICE IF NECESSARY
C   98-12-22  MIKE BALDWIN - BACK OUT RH OVER ICE
C   00-01-04  JIM TUCCILLO - MPI VERSION
C   01-10-25  H CHUANG     - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-01-15  MIKE BALDWIN - WRF VERSION
C     
C USAGE:    CALL FRZLVL(ZFRZ,RHFRZ)
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     ZFRZ     - ABOVE GROUND LEVEL FREEZING HEIGHT.
C     RHFRZ    - RELATIVE HUMIDITY AT FREEZING LEVEL.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - 
C                  LOOPS
C                  PVRBLS
C                  MASKS
C                  MAPOT
C                  POSTVAR
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     
      use vrbls3d
      use vrbls2d
      use masks
C
C     INCLUDE/SET PARAMETERS.
!      INCLUDE "parmeta"
      INCLUDE "params"
C
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
C
C     DECLARE VARIABLES.
C     
      REAL RHFRZ(IM,JM),ZFRZ(IM,JM)
C     
C*********************************************************************
C     START FRZLVL.
C
C
C     
C     LOOP OVER HORIZONTAL GRID.
C     
!$omp  parallel do
!$omp& private(alpfrz,alph,alpl,delalp,delq,delt,delz,
!$omp&         delzp,dzabv,dzfr,htsfc,l,llmh,psfc,qfrz,
!$omp&         qsat,qsfc,qsfrz,rhsfc,rhz,tsfc,
!$omp&         zl,zu)
      DO 20 J=JSTA,JEND
      DO 20 I=1,IM
         HTSFC    = FIS(I,J)*GI
         LLMH     = NINT(LMH(I,J))
         RHFRZ(I,J) = D00
         ZFRZ(I,J)  = HTSFC
         PSFC    = PINT(I,J,LLMH+1)
C     
C        CHECK IF FREEZING LEVEL IS AT THE GROUND.
C     
         TSFC = (SM(I,J)*THZ0(I,J)+(1.-SM(I,J))*THS(I,J))
     1	 *(PINT(I,J,NINT(LMH(I,J))+1)/P1000)**CAPA
         IF (TSFC.LE.TFRZ) THEN
            ZFRZ(I,J) = HTSFC+(TSFC-TFRZ)/D0065
            QSFC    = SM(I,J)*QZ0(I,J)+(1.-SM(I,J))*QS(I,J)
C
            QSAT=PQ0/PSFC
     1          *EXP(A2*(TSFC-A3)/(TSFC-A4))
C
            RHSFC   = QSFC/QSAT
            RHSFC   = AMAX1(0.01,RHSFC)
            RHSFC   = AMIN1(RHSFC,1.0)
            RHFRZ(I,J)= RHSFC
            GOTO 20
         ENDIF
C     
C        OTHERWISE, LOCATE THE FREEZING LEVEL ALOFT.
C
         DO 10 L = LLMH,1,-1
            IF (T(I,J,L).LE.TFRZ) THEN
               IF (L.LT.LLMH-1) THEN
                  DELZ = D50*(ZINT(I,J,L)-ZINT(I,J,L+2))
                  ZL   = D50*(ZINT(I,J,L+1)+ZINT(I,J,L+2))
                  DELT = T(I,J,L)-T(I,J,L+1)
                  ZFRZ(I,J) = ZL + (TFRZ-T(I,J,L+1))/DELT*DELZ
C     
                  DZABV = ZFRZ(I,J)-ZL
                  DELQ  = Q(I,J,L)-Q(I,J,L+1)
                  QFRZ  = Q(I,J,L+1) + DELQ/DELZ*DZABV
                  QFRZ  = AMAX1(0.0,QFRZ)
C     
C
                  ALPL   = ALPINT(I,J,L+2)
                  ALPH   = ALPINT(I,J,L)
                  DELALP = ALPH - ALPL
                  DELZP  = ZINT(I,J,L)-ZINT(I,J,L+2)
                  DZFR   = ZFRZ(I,J) - ZINT(I,J,L+2)
                  ALPFRZ = ALPL + DELALP/DELZP*DZFR
                  PFRZ   = EXP(ALPFRZ)
                  QSFRZ  = PQ0/PFRZ
C     
                  RHZ      = QFRZ/QSFRZ
                  RHZ      = AMAX1(0.01,RHZ)
                  RHZ      = AMIN1(RHZ,1.0)
                  RHFRZ(I,J) = RHZ
C     
               ELSE
                  ZU      = ZMID(I,J,L)
                  ZL      = HTSFC
                  DELZ    = ZU-ZL
                  TSFC    = (SM(I,J)*THZ0(I,J)+(1.-SM(I,J))*THS(I,J))
     1           *(PINT(I,J,NINT(LMH(I,J))+1)/P1000)**CAPA
                  DELT    = T(I,J,L)-TSFC
                  ZFRZ(I,J) = ZL + (TFRZ-TSFC)/DELT*DELZ
C     
                  DZABV   = ZFRZ(I,J)-ZL
                  QSFC    = SM(I,J)*QZ0(I,J)+(1.-SM(I,J))*QS(I,J)
                  DELQ    = Q(I,J,L)-QSFC
                  QFRZ    = QSFC + DELQ/DELZ*DZABV
                  QFRZ    = AMAX1(0.0,QFRZ)
C     
                  ALPH    = ALPINT(I,J,L)
                  ALPL    = ALOG(PSFC)
                  DELALP  = ALPH-ALPL
                  ALPFRZ  = ALPL + DELALP/DELZ*DZABV
                  PFRZ    = EXP(ALPFRZ)
                  QSFRZ   = PQ0/PFRZ
C
                  RHZ     = QFRZ/QSFRZ
                  RHZ     = AMAX1(0.01,RHZ)
                  RHZ     = AMIN1(RHZ,1.0)
                  RHFRZ(I,J)= RHZ
               ENDIF
C     
C              BOUND FREEZING LEVEL RH.  FREEZING LEVEL HEIGHT IS
C              MEASURED WITH RESPECT TO MEAN SEA LEVEL.
C
               RHFRZ(I,J) = AMAX1(0.01,RHFRZ(I,J))
               RHFRZ(I,J) = AMIN1(RHFRZ(I,J),1.00)
               ZFRZ(I,J)  = AMAX1(0.0,ZFRZ(I,J))
               GOTO 20
            ENDIF
 10      CONTINUE
 20   CONTINUE
C     
C     END OF ROUTINE.
C     
      RETURN
      END
