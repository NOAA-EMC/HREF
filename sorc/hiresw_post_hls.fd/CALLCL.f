      SUBROUTINE CALLCL(P1D,T1D,Q1D,PLCL,ZLCL)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALLCL      COMPUTES LCL HEIGHTS AND PRESSURE
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-03-15
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES THE LIFTING CONDENSATION LEVEL 
C     PRESSURE AND HEIGHT IN EACH COLUMN AT MASS POINTS.
C     THE HEIGHT IS ABOVE GROUND LEVEL.  THE EQUATION USED
C     TO FIND THE LCL PRESSURE IS FROM BOLTAN (1980,MWR) 
C     AND IS THE SAME AS THAT USED IN SUBROUTINE CALCAPE.
C     
C     THIS ROUTINE IS A TEST VERSION.  STILL TO BE RESOLVED
C     IS THE "BEST" PARCEL TO LIFT.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-03-15  RUSS TREADON
C   98-06-16  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION            
C     
C USAGE:    CALL CALLCL(P1D,T1D,Q1D,PLCL,ZLCL)
C   INPUT ARGUMENT LIST:
C     P1D      - ARRAY OF PARCEL PRESSURES (PA)
C     T1D      - ARRAY OF PARCEL TEMPERATURES (K)
C     Q1D      - ARRAY OF PARCEL SPECIFIC HUMIDITIES (KG/KG)
C
C   OUTPUT ARGUMENT LIST: 
C     PLCL     - PARCEL PRESSURE AT LCL (PA)
C     ZLCL     - PARCEL AGL HEIGHT AT LCL (M)
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - LOOPS
C                  OPTIONS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C     
C     
      use vrbls 
      use extra 
C     
C     INCLUDE/SET PARAMETERS.
      INCLUDE "parmeta"
      INCLUDE "params"
C
      PARAMETER (D35=3.5,D4805=4.805,H2840=2840.,H55=55.)
      PARAMETER (D2845=0.2845,D28=0.28)
C
C     DECLARE VARIABLES.
C     
      REAL P1D(IM,JM),T1D(IM,JM),Q1D(IM,JM)
      REAL PLCL(IM,JM),TLCL(IM,JM),ZLCL(IM,JM)
CX      REAL PFAL(IM,JM),TFAL(IM,JM),QFAL(IM,JM)
CX      REAL PSAL(IM,JM),TSAL(IM,JM),QSAL(IM,JM)
CX      REAL PBAR(IM,JM),TBAR(IM,JM),QBAR(IM,JM)
C     
C     INCLUDE COMMON BLOCKS.
      INCLUDE "LOOPS.comm"
      INCLUDE "OPTIONS.comm"
      INCLUDE "CTLBLK.comm"
C     
C     
C**********************************************************************
C     START CALLCL HERE.
C     
C     LOAD OUTPUT ARRAYS WITH SPECIAL VALUE.
C     
      DO J=JSTA,JEND
      DO I=1,IM
        PLCL(I,J)=SPVAL
        TLCL(I,J)=SPVAL
        ZLCL(I,J)=SPVAL
      ENDDO
      ENDDO

CX      DO J=JSTA,JEND
CX      DO I=1,IM
CX         LLMH    = LMH(I,J)
CX         ALPFAL  = D50*(ALPINT(I,J,LLMH)+ALPINT(I,J,LLMH+1))
CX         ALPSAL  = D50*(ALPINT(I,J,LLMH-1)+ALPINT(I,J,LLMH))
CX         PFAL(I,J) = EXP(ALPFAL)
CX         PSAL(I,J) = EXP(ALPSAL)
CX         TFAL(I,J) = T(I,J,LLMH)
CX         TSAL(I,J) = T(I,J,LLMH-1)
CX         QFAL(I,J) = Q(I,J,LLMH)
CX         QSAL(I,J) = Q(I,J,LLMH-1)
CX         PBAR(I,J) = D50*(PFAL(I,J)+PSAL(I,J))
CX         TBAR(I,J) = D50*(TFAL(I,J)+TSAL(I,J))
CX         QBAR(I,J) = D50*(QFAL(I,J)+QSAL(I,J))
CX      ENDDO
CX      ENDDO

C     
C     COMPUTE PRESSURE, TEMPERATURE AND AGL HEIGHT AT LCL.
C
      DO 30 J=JSTA_M,JEND_M
      DO 30 I=2,IM-1
      EVP      =P1D(I,J)*Q1D(I,J)/(EPS+ONEPS*Q1D(I,J))
      RMX      =EPS*EVP/(P1D(I,J)-EVP)
      CKAPA    =D2845*(1.-D28*RMX)
      RKAPA    =1./CKAPA
      ARG      =EVP*D01
      ARG      =AMAX1(H1M12,ARG)
      DENOM    =D35*ALOG(T1D(I,J))-ALOG(ARG)-D4805
      TLCL(I,J)=H2840/DENOM+H55
      PLCL(I,J)=P1D(I,J)*(TLCL(I,J)/T1D(I,J))**RKAPA
      ALPLCL   =ALOG(PLCL(I,J))
      LLMH     =LMH(I,J)
C
      DO 20 L=LLMH,1,-1
      IF(ALPINT(I,J,L).LT.ALPLCL)THEN
        DLPLCL   =ALPLCL-ALPINT(I,J,L+1)
        DALP     =ALPINT(I,J,L)-ALPINT(I,J,L+1)
        DZ       =ZINT(I,J,L)-ZINT(I,J,L+1)
        ZLCL(I,J)=ZINT(I,J,L+1)+DZ*DLPLCL/DALP
        ZSFC     =FIS(I,J)*GI
        ZLCL(I,J)=ZLCL(I,J)-ZSFC
        ZLCL(I,J)=AMAX1(D00,ZLCL(I,J))
CX               PSFC=PD(I,J)+50.E2
CX               WRITE(81,1234)I,J,LLMH,L,PSFC*D01,T1D(I,J),ZSFC
CX               WRITE(81,1234)I,J,LLMH,L,PLCL(I,J)*D01,TLCL(I,J),ZLCL(I,J)
CX               WRITE(81,*)' '
CX 1234          FORMAT(I3,1X,I3,1X,2(I2,1X),4(G12.6,1X))
               
        GOTO 30
      ENDIF
 20   CONTINUE
 30   CONTINUE
C     
C     END OF ROUTINE.
C     
      RETURN
      END
