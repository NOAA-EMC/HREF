      SUBROUTINE WETFRZLVL(TWET,ZWET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    WETFRZLVL      COMPUTES LEVEL OF 0 WET BULB	 
C PRGRMMR: MANIKIN         ORG: W/NP2      DATE: 03-11-14     
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES THE LOWEST HEIGHT WITH A WET BULB
C     TEMPERATURE OF FREEZING FOR EACH MASS POINT ON THE ETA GRID.  
C     THE COMPUTED WET BULB ZERO HEIGHT IS THE MEAN SEA LEVEL
C     HEIGHT.  AT EACH MASS POINT WE MOVE UP FROM THE SURFACE TO 
C     FIND THE FIRST ETA LAYER WHERE THE TW IS LESS THAN
C     273.16K.  VERTICAL INTERPOLATION IN TEMPERATURE TO THE FREEZING
C     TEMPERATURE GIVES THE FREEZING LEVEL HEIGHT.  PRESSURE AND 
C     SPECIFIC HUMIDITY ARE INTERPOLATED TO THIS LEVEL AND ALONG WITH
C     THE TEMPERATURE PROVIDE THE FREEZING LEVEL RELATIVE HUMIDITY.
C     IF THE SURFACE (SKIN) TEMPERATURE IS BELOW FREEZING, THE ROUTINE
C     USES SURFACE BASED FIELDS TO COMPUTE THE RELATIVE HUMIDITY.
C     
C PROGRAM HISTORY LOG:
C   03-11-14 GEOFF MANIKIN - NEW PROGRAM 
C   04-12-06  G MANIKIN    - CORRECTED COMPUTATION OF SFC TEMPERATURE
C   05-03-11  H CHUANG     - WRF VERSION
C     
C USAGE:   CALL WETFRZLVL(TWET,ZWET) 
C   INPUT ARGUMENT LIST:
C     TWET     - WET BULB TEMPERATURES 
C
C   OUTPUT ARGUMENT LIST: 
C     ZWET     - ABOVE GROUND LEVEL HEIGHT OF LEVEL WITH 0 WET BULB.
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       REL_HUM
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
      REAL TWET(IM,JSTA_2L:JEND_2U,LM),ZWET(IM,JM)
C     
C*********************************************************************
C     START FRZLVL.
C
C     LOOP OVER HORIZONTAL GRID.
C     
!$omp  parallel do
!$omp& private(delt,delz,htsfc,l,llmh
!$omp&         tsfc,zl,zu)
      DO 20 J=JSTA,JEND
      DO 20 I=1,IM
         HTSFC    = FIS(I,J)*GI
         LLMH     = NINT(LMH(I,J))
         ZWET(I,J)  = HTSFC
C     
C        CHECK IF FREEZING LEVEL IS AT THE GROUND.
C        IF YES, ESTIMATE UNDERGROUND FREEZING LEVEL USING 6.5C/KM LAPSE RATE
C        AND ASSUME RH TO BE EQUAL TO RH AT SFC
C     
         THSFC = (SM(I,J)*THZ0(I,J)+(1.-SM(I,J))*THS(I,J))
         PSFC=PINT(I,J,LLMH+1)
         TSFC=THSFC*(PSFC/P1000)**CAPA

         IF (TSFC.LE.TFRZ) THEN
c            ZWET(I,J) = HTSFC
            ZWET(I,J) = HTSFC+(TSFC-TFRZ)/D0065
            GOTO 20
         ENDIF
C     
C        OTHERWISE, LOCATE THE FREEZING LEVEL ALOFT.
C
         DO 10 L = LLMH,1,-1
            IF (TWET(I,J,L).LE.TFRZ) THEN
               IF (L.LT.LLMH-1) THEN
                  DELZ = D50*(ZINT(I,J,L)-ZINT(I,J,L+2))
                  ZL   = D50*(ZINT(I,J,L+1)+ZINT(I,J,L+2))
                  DELT = TWET(I,J,L)-TWET(I,J,L+1)
                  ZWET(I,J) = ZL + (TFRZ-TWET(I,J,L+1))/DELT*DELZ
               ELSE
                  ZU      = D50*(ZINT(I,J,L)+ZINT(I,J,L+1))
                  ZL      = HTSFC
                  DELZ    = ZU-ZL
                  TSFC    = SM(I,J)*THZ0(I,J)+(1.-SM(I,J))*THS(I,J)
     1             *(PINT(I,J,NINT(LMH(I,J))+1)/P1000)**CAPA
                  DELT    = T(I,J,L)-TSFC
		  IF(DELT .NE. 0.)THEN  
                   ZWET(I,J) = ZL + (TFRZ-TSFC)/DELT*DELZ
		  ELSE
		   ZWET(I,J) = HTSFC+(TSFC-TWET(I,J,L))/D0065
		  END IF  
                  IF (ZWET(I,J) .GT. ZU) THEN
                    ZWET(I,J)=ZU
                  ENDIF
                   IF ((-1*ZWET(I,J)) .GT. ZU) THEN
                    ZWET(I,J)=ZU
                  endif
               ENDIF
               GOTO 20
            ENDIF
 10      CONTINUE
 20   CONTINUE
C     
C     END OF ROUTINE.
C     
      RETURN
      END
