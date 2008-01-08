      SUBROUTINE MISCLN
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    MISCLN      POSTS MISCELLANEOUS FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-20
C     
C ABSTRACT:
C     THIS ROUTINE HAS BECOME THE CATCH-ALL FOR MISCELLANEOUS
C     OUTPUT FIELDS POSTED BY THE ETA POST PROCESSOR.  
C     CURRENTLY THIS ROUTINE POSTS THE FOLLOWING FIELDS:
C        (1) TROPOPAUSE LEVEL Z,P, T, U, V, AND VERTICAL WIND SHEAR,
C        (2) MAX WIND LEVEL Z, P, U, AND V,
C        (3) FD LEVEL T, U, AND V,
C        (4) FREEZING LEVEL Z AND RH,
C        (5) CONSTANT MASS (BOUNDARY) FIELDS,
C        (6) LFM LOOK-ALIKE FIELDS, AND
C        (7) NGM LOOK-ALIKE FIELDS.
C
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-20  RUSS TREADON
C   93-06-19  RUSS TREADON - ADDED TYPE 2 CAPE POSTING.
C   94-11-07  MIKE BALDWIN - ADDED HELICITY POSTING.
C   96-03-26  MIKE BALDWIN - CHANGE ETA BOUNDARY LAYER LABELS FOR GRIB
C   96-11-19  MIKE BALDWIN - BACK OUT PREVIOUS CHANGE 
C   97-04-25  MIKE BALDWIN - CHANGE ETA BOUNDARY LAYER LABELS FOR GRIB
C   97-04-29  GEOFF MANIKIN - ADDED TROPOPAUSE HEIGHT AND
C                             MAX WIND LEVEL FIELDS
C   98-06-15  T BLACK       - CONVERSION FROM 1-D TO 2-D
C   98-07-17  MIKE BALDWIN - REMOVED LABL84
C   00-01-04  JIM TUCCILLO - MPI VERSION
C   02-04-23  MIKE BALDWIN - WRF VERSION
C     
C USAGE:    CALL MISCLN
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       TRPAUS  - COMPUTE TROPOPAUSE LEVEL FIELDS.
C       CALMXW  - COMPUTE MAX WIND LEVEL FIELDS.
C       SCLFLD  - SCALE ARRAY ELEMENTS BY CONSTANT.
C       GRIBIT  - OUTPUT FIELD TO GRIB FILE.
C       CALPOT  - CALCULATE POTENTIAL TEMPERATURE.
C       FDLVL   - COMPUTE FD LEVEL DATA (AGL OR MSL).
C       FRZLVL  - COMPUTE FREEZING LEVEL DATA.
C       BOUND   - BOUND ARRAY ELEMENTS BETWEEN MINIMUM AND MAXIMUM VALUES.
C       BNDLYR  - COMPUTE BOUNDARY LAYER FIELDS.
C       CALDWP  - CALCULATE DEWPOINT TEMPERATURE.
C       OTLFT   - COMPUTE LIFTED INDEX AT 500MB.
C       CALLCL  - COMPUTE LCL DATA.
C       LFMFLD  - COMPUTE LFM LOOK-ALIKE FIELDS.
C       NGMFLD  - COMPUTE NGM LOOK-ALIKE FIELDS.
C       CALTHTE - COMPUTE THETA-E.
C       CALHEL  - COMPUTE HELICITY AND STORM MOTION.
C
C     LIBRARY:
C       COMMON - RQSTFLD
C                CTLBLK
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
      use vrbls3d
      use vrbls2d
      use masks
C
C     INCLUDE GRID DIMENSIONS.  DERIVE DEPENDENT PARAMETERS.
C
!      INCLUDE "parmeta"
!      INCLUDE "parmout"
      INCLUDE "params"
C
C     INCLUDE COMMON BLOCKS.
      INCLUDE "RQSTFLD.comm"
      INCLUDE "CTLBLK.comm"
C     
C     SET LOCAL PARAMETERS.  MAKE SURE NFD AND NBND AGREE
C     WITH THE VALUES SET IN SUBROUTINES FDLVL AND BNDLYR,
C     RESPECTIVELY.
      PARAMETER (NFD=11,NBND=6)
      PARAMETER (C2K=273.15)
C     
C     DECLARE VARIABLES.
C     
      LOGICAL NORTH
      LOGICAL RUN,FIRST,RESTRT,SIGMA
      LOGICAL FIELD1,FIELD2
      LOGICAL DONE(IM,JSTA:JEND),DONE1(IM,JSTA:JEND)
      INTEGER LVLBND(IM,JM,NBND),LB2(IM,JM),LPBL(IM,JM)
      REAL P1D(IM,JM),T1D(IM,JM),Q1D(IM,JM),U1D(IM,JM),V1D(IM,JM)
      REAL SHR1D(IM,JM),Z1D(IM,JM),RH1D(IM,JM)
      REAL OMGBND(IM,JM,NBND),PWTBND(IM,JM,NBND)
      REAL QCNVBND(IM,JM,NBND)
      REAL PBND(IM,JM,NBND),TBND(IM,JM,NBND),QBND(IM,JM,NBND)
      REAL UBND(IM,JM,NBND),VBND(IM,JM,NBND),RHBND(IM,JM,NBND)
      REAL WBND(IM,JM,NBND)
      REAL T78483(IM,JM),T89671(IM,JM),P78483(IM,JM),P89671(IM,JM)
      REAL PKL1,PKU1
      REAL QM8510(IM,JM),RH4710(IM,JM),RH8498(IM,JM)
      REAL RH4796(IM,JM),RH1847(IM,JM),UST(IM,JM),VST(IM,JM)
      REAL RH3310(IM,JM),RH6610(IM,JM),RH3366(IM,JM),PW3310(IM,JM)
      REAL HTFD(NFD),T7D(IM,JM,NFD),U7D(IM,JM,NFD),V6D(IM,JM,NFD)
      REAL PETABND(NBND),SIGBND(NBND),HELI(IM,JM)
      REAL EGRID1(IM,JM),EGRID2(IM,JM),EGRID3(IM,JM)
      REAL GRID1(IM,JM),GRID2(IM,JM)
      REAL MAXWP(IM,JM),MAXWZ(IM,JM),MAXWU(IM,JM), MAXWV(IM,JM)
      REAL GUST(IM,JM) 
C     
C
C     EQUIVALENCES FOR SUBROUTINE MISCLN.
!      EQUIVALENCE (TBND(1,1,1),T7D(1,1,1))
!      EQUIVALENCE (TBND(1,1,2),T7D(1,1,2))
!      EQUIVALENCE (TBND(1,1,3),T7D(1,1,3))
!      EQUIVALENCE (TBND(1,1,4),T7D(1,1,4))
!      EQUIVALENCE (TBND(1,1,5),T7D(1,1,5))
!      EQUIVALENCE (TBND(1,1,6),T7D(1,1,6))
!      EQUIVALENCE (UBND(1,1,1),U7D(1,1,1))
!      EQUIVALENCE (UBND(1,1,2),U7D(1,1,2))
!      EQUIVALENCE (UBND(1,1,3),U7D(1,1,3))
!      EQUIVALENCE (UBND(1,1,4),U7D(1,1,4))
!      EQUIVALENCE (UBND(1,1,5),U7D(1,1,5))
!      EQUIVALENCE (UBND(1,1,6),U7D(1,1,6))
!      EQUIVALENCE (VBND(1,1,1),V6D(1,1,1))
!      EQUIVALENCE (VBND(1,1,2),V6D(1,1,2))
!      EQUIVALENCE (VBND(1,1,3),V6D(1,1,3))
!      EQUIVALENCE (VBND(1,1,4),V6D(1,1,4))
!      EQUIVALENCE (VBND(1,1,5),V6D(1,1,5))
!      EQUIVALENCE (VBND(1,1,6),V6D(1,1,6))
C     
C     SET FD LEVEL HEIGHTS IN GEOPOTENTAL METERS.
      DATA HTFD  / 305.E0,457.E0,610.E0,914.E0,1524.E0,1829.E0,
     X     2134.E0,2743.E0,3658.E0,4572.E0,6000.E0/
C     
C     SET MIDPOINT "SIGMA" VALUES FOR ETA BOUNDARY LAYERS.
      DATA SIGBND / 0.985,0.955,0.925,0.895,0.865,0.835 /
      DATA PETABND / 15.,45.,75.,105.,135.,165. /
C     
C****************************************************************************
C     START MISCLN HERE.
C     
C        HELICITY AND STORM MOTION.
       IF (IGET(162).GT.0.OR.IGET(163).GT.0.OR.IGET(164).GT.0) THEN
        IF (IGET(162).GT.0) THEN
          IF(LVLS(1,IGET(162)).GT.0)DEPTH=3000.0
          CALL CALHEL(DEPTH,UST,VST,HELI)
          IF (IGET(162).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=HELI(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            ID(10)   = 30
            ID(11)   = 0
            CALL GRIBIT(IGET(162),LVLS(1,IGET(162)),GRID1,IM,JM)
          ENDIF

          IF(LVLS(2,IGET(162)).GT.0)DEPTH=1000.0
          CALL CALHEL(DEPTH,UST,VST,HELI)
          IF (IGET(162).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=HELI(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            ID(10)   = 10
            ID(11)   = 0
            CALL GRIBIT(IGET(162),LVLS(1,IGET(162)),GRID1,IM,JM)
          ENDIF
         ENDIF



         IF (IGET(163).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=UST(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            ID(10)   = 60
            ID(11)   = 0 
            CALL GRIBIT(IGET(163),LVLS(1,IGET(163)),GRID1,IM,JM)
         ENDIF
         IF (IGET(164).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=VST(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            ID(10)   = 60
            ID(11)   = 0 
            CALL GRIBIT(IGET(164),LVLS(1,IGET(164)),GRID1,IM,JM)
         ENDIF
       ENDIF
C     
C
C
C     ***BLOCK 1:  TROPOPAUSE P, Z, T, U, V, AND WIND SHEAR.
C    
      IF ( (IGET(054).GT.0).OR.(IGET(055).GT.0).OR.
     X     (IGET(056).GT.0).OR.(IGET(057).GT.0).OR.
     X     (IGET(177).GT.0).OR.
     X     (IGET(058).GT.0).OR.(IGET(108).GT.0) ) THEN
         CALL TRPAUS(P1D,T1D,Z1D,U1D,V1D,SHR1D) 
C
C        TROPOPAUSE PRESSURE.
         IF (IGET(054).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=P1D(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(054),LVLS(1,IGET(054)),
     X           GRID1,IM,JM)
         ENDIF

C        TROPOPAUSE HEIGHT.
         IF (IGET(177).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=Z1D(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(177),LVLS(1,IGET(177)),
     X           GRID1,IM,JM)
         ENDIF
C
C        TROPOPAUSE TEMPERATURE.
         IF (IGET(055).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=T1D(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(055),LVLS(1,IGET(055)),
     X           GRID1,IM,JM)
         ENDIF
C
C        TROPOPAUSE POTENTIAL TEMPERATURE.
         IF (IGET(108).GT.0) THEN
            CALL CALPOT(P1D,T1D,EGRID1)
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID1(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(108),LVLS(1,IGET(108)),
     X           GRID1,IM,JM)
         ENDIF
C     
C        TROPOPAUSE U WIND AND/OR V WIND.
         IF ((IGET(056).GT.0).OR.(IGET(057).GT.0)) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=U1D(I,J)
                 GRID2(I,J)=V1D(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            IF (IGET(056).GT.0) CALL GRIBIT(IGET(056),
     X           LVLS(1,IGET(056)),GRID1,IM,JM)
            ID(1:25) = 0
            IF (IGET(057).GT.0) CALL GRIBIT(IGET(057),
     X           LVLS(1,IGET(057)),GRID2,IM,JM)
         ENDIF
C
C        TROPOPAUSE WIND SHEAR.
         IF (IGET(058).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=SHR1D(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(058),LVLS(1,IGET(058)),
     X           GRID1,IM,JM)
         ENDIF
      ENDIF
C
C
C
C     ***BLOCK 2:  MAX WIND LEVEL  P, Z, U, AND V
C
C        MAX WIND LEVEL CALCULATIONS
         IF ((IGET(173).GT.0) .OR. (IGET(174).GT.0) .OR.
     X      (IGET(175).GT.0) .OR. (IGET(176).GT.0)) THEN
            CALL CALMXW(MAXWP,MAXWZ,MAXWU,MAXWV)
         ENDIF
C        PRESSURE OF MAX WIND LEVEL
         IF (IGET(173).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=MAXWP(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(173),LVLS(1,IGET(173)),
     X           GRID1,IM,JM)
          ENDIF
C        HEIGHT OF MAX WIND LEVEL
         IF (IGET(174).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=MAXWZ(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(174),LVLS(1,IGET(174)),
     X           GRID1,IM,JM)
          ENDIF

C        MAX WIND LEVEL U WIND AND/OR V WIND.
         IF ((IGET(175).GT.0).OR.(IGET(176).GT.0)) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=MAXWU(I,J)
                 GRID2(I,J)=MAXWV(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            IF (IGET(175).GT.0) CALL GRIBIT(IGET(175),
     X           LVLS(1,IGET(175)),GRID1,IM,JM)
            ID(1:25) = 0
            IF (IGET(176).GT.0) CALL GRIBIT(IGET(176),
     X           LVLS(1,IGET(176)),GRID2,IM,JM)
         ENDIF
C
C
C
C     ***BLOCK 3:  FD LEVEL T, U, AND V.
C     
      IF ( (IGET(059).GT.0).OR.(IGET(060).GT.0).OR.
     X     (IGET(061).GT.0) ) THEN
C
C     DETERMINE WHETHER TO DO MSL OR AGL FD LEVELS
C
         ITYPE=1
         DO IFD = 1,NFD
           IF (IGET(059).GT.0) THEN
            IF (LVLS(IFD,IGET(059)).GT.1) ITYPE=2
           ENDIF
           IF (IGET(060).GT.0) THEN
            IF (LVLS(IFD,IGET(060)).GT.1) ITYPE=2
           ENDIF
           IF (IGET(061).GT.0) THEN
            IF (LVLS(IFD,IGET(061)).GT.1) ITYPE=2
           ENDIF
         ENDDO
	write(6,*) 'call FDLVL with ITYPE: ', ITYPE
         CALL FDLVL(ITYPE,T7D,U7D,V6D)
C     
         DO 10 IFD = 1,NFD
            ID(1:25) = 0
            ISVALUE = NINT(HTFD(IFD))
            ID(11) = ISVALUE
C
C           FD LEVEL TEMPERATURE.
            IF (IGET(059).GT.0) THEN
              IF (LVLS(IFD,IGET(059)).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=T7D(I,J,IFD)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(059),
     X              LVLS(IFD,IGET(059)),GRID1,IM,JM)
              ENDIF
            ENDIF
C
C           FD LEVEL U WIND AND/OR V WIND.
            IF ((IGET(060).GT.0).OR.(IGET(061).GT.0)) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=U7D(I,J,IFD)
                 GRID2(I,J)=V6D(I,J,IFD)
               ENDDO
               ENDDO
               IF (IGET(060).GT.0) THEN
                 IF (LVLS(IFD,IGET(060)).GT.0) CALL GRIBIT(
     X              IGET(060),LVLS(IFD,IGET(060)),GRID1,IM,JM)
               ENDIF
               IF (IGET(061).GT.0) THEN
                 IF (LVLS(IFD,IGET(061)).GT.0) CALL GRIBIT(
     X              IGET(061),LVLS(IFD,IGET(061)),GRID2,IM,JM)
               ENDIF
            ENDIF
 10      CONTINUE
      ENDIF
C     
C
C
C     ***BLOCK 4:  FREEZING LEVEL Z AND RH.
C     
      IF ( (IGET(062).GT.0).OR.(IGET(063).GT.0) ) THEN
         CALL FRZLVL(Z1D,RH1D)
C
C        FREEZING LEVEL HEIGHT.
         IF (IGET(062).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=Z1D(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL BOUND (GRID1,D00,H99999)
            CALL GRIBIT(IGET(062),LVLS(1,IGET(062)),
     X           GRID1,IM,JM)
         ENDIF
C
C        FREEZING LEVEL RELATIVE HUMIDITY.
         IF (IGET(063).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RH1D(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL SCLFLD(GRID1,H100,IM,JM)
            CALL BOUND(GRID1,H1,H100)
            CALL GRIBIT(IGET(063),LVLS(1,IGET(063)),
     X           GRID1,IM,JM)
         ENDIF
      ENDIF
      IF (IGET(165).GT.0) THEN
         CALL FRZLVL2(Z1D,RH1D)
C
C        HIGHEST FREEZING LEVEL HEIGHT.
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=Z1D(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL BOUND (GRID1,D00,H99999)
            CALL GRIBIT(IGET(165),LVLS(1,IGET(165)),
     X           GRID1,IM,JM)
      ENDIF
C     
C
C
C     ***BLOCK 5:  BOUNDARY LAYER FIELDS.
C     
      IF ( (IGET(067).GT.0).OR.(IGET(068).GT.0).OR.
     X     (IGET(069).GT.0).OR.(IGET(070).GT.0).OR.
     X     (IGET(071).GT.0).OR.(IGET(072).GT.0).OR.
     X     (IGET(073).GT.0).OR.(IGET(074).GT.0).OR.
     X     (IGET(088).GT.0).OR.(IGET(089).GT.0).OR.
     X     (IGET(090).GT.0).OR.(IGET(075).GT.0).OR.
     X     (IGET(109).GT.0).OR.(IGET(110).GT.0).OR.
     X     (IGET(031).GT.0).OR.(IGET(032).GT.0).OR.
     X     (IGET(107).GT.0).OR.(IGET(091).GT.0).OR.
     X     (IGET(092).GT.0).OR.(IGET(093).GT.0).OR.
     X     (IGET(094).GT.0).OR.(IGET(095).GT.0).OR.
     X     (IGET(096).GT.0).OR.(IGET(097).GT.0).OR.
     X     (IGET(098).GT.0).OR.(IGET(221).GT.0) ) THEN
C
C        COMPUTE ETA BOUNDARY LAYER FIELDS.
         CALL BNDLYR(PBND,TBND,QBND,RHBND,UBND,VBND,
     X        WBND,OMGBND,PWTBND,QCNVBND,LVLBND)

C     
C        LOOP OVER NBND BOUNDARY LAYERS.
         DO 20 LBND = 1,NBND
            ID(1:25) = 0
            ID(10)   = NINT(PETABND(LBND)+15.)
            ID(11)   = NINT(PETABND(LBND)-15.)
C     
C           BOUNDARY LAYER PRESSURE.
            IF (IGET(067).GT.0) THEN
              IF (LVLS(LBND,IGET(067)).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=PBND(I,J,LBND)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(067),
     X              LVLS(LBND,IGET(067)),GRID1,IM,JM)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER TEMPERATURE.
            IF (IGET(068).GT.0) THEN
              IF (LVLS(LBND,IGET(068)).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=TBND(I,J,LBND)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(068),LVLS(LBND,IGET(068)),
     X              GRID1,IM,JM)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER POTENTIAL TEMPERATURE.
            IF (IGET(069).GT.0) THEN
              IF (LVLS(LBND,IGET(069)).GT.0) THEN
               CALL CALPOT(PBND(1,1,LBND),TBND(1,1,LBND),EGRID1)
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID1(I,J)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(069),LVLS(LBND,IGET(069)),
     X              GRID1,IM,JM)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER RELATIVE HUMIDITY.
            IF (IGET(072).GT.0) THEN
              IF (LVLS(LBND,IGET(072)).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RHBND(I,J,LBND)
               ENDDO
               ENDDO
               CALL SCLFLD(GRID1,H100,IM,JM)
               CALL BOUND(GRID1,H1,H100)
               CALL GRIBIT(IGET(072),LVLS(LBND,IGET(072)),
     X              GRID1,IM,JM)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER DEWPOINT TEMPERATURE.
            IF (IGET(070).GT.0) THEN
              IF (LVLS(LBND,IGET(070)).GT.0) THEN
               CALL CALDWP(PBND(1,1,LBND),QBND(1,1,LBND),EGRID1,
     X              TBND(1,1,LBND))
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID1(I,J)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(070),LVLS(LBND,IGET(070)),
     X              GRID1,IM,JM)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER SPECIFIC HUMIDITY.
            IF (IGET(071).GT.0) THEN
              IF (LVLS(LBND,IGET(071)).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=QBND(I,J,LBND)
               ENDDO
               ENDDO
               CALL BOUND(GRID1,H1M12,H99999)
               CALL GRIBIT(IGET(071),LVLS(LBND,IGET(071)),
     X              GRID1,IM,JM)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER MOISTURE CONVERGENCE.
            IF (IGET(088).GT.0) THEN
              IF (LVLS(LBND,IGET(088)).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=QCNVBND(I,J,LBND)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(088),LVLS(LBND,IGET(088)),
     X              GRID1,IM,JM)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER U WIND AND/OR V WIND.
C
            FIELD1=.FALSE.
            FIELD2=.FALSE.
C
            IF(IGET(073).GT.0)THEN
              IF(LVLS(LBND,IGET(073)).GT.0)FIELD1=.TRUE.
            ENDIF
            IF(IGET(074).GT.0)THEN
              IF(LVLS(LBND,IGET(074)).GT.0)FIELD2=.TRUE.
            ENDIF
C
            IF(FIELD1.OR.FIELD2)THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=UBND(I,J,LBND)
                 GRID2(I,J)=VBND(I,J,LBND)
               ENDDO
               ENDDO
C
               IF (IGET(073).GT.0) THEN
                 IF (LVLS(LBND,IGET(073)).GT.0)
     X              CALL GRIBIT(IGET(073),
     X              LVLS(LBND,IGET(073)),GRID1,IM,JM)
               ENDIF
               IF (IGET(074).GT.0) THEN
                 IF (LVLS(LBND,IGET(074)).GT.0) 
     X              CALL GRIBIT(IGET(074),
     X              LVLS(LBND,IGET(074)),GRID2,IM,JM)
               ENDIF
            ENDIF
C     
C           BOUNDARY LAYER OMEGA.
            IF (IGET(090).GT.0) THEN
              IF (LVLS(LBND,IGET(090)).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=OMGBND(I,J,LBND)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(090),LVLS(LBND,IGET(090)),
     X              GRID1,IM,JM)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER PRECIPITBLE WATER.
            IF (IGET(089).GT.0) THEN
              IF (LVLS(LBND,IGET(089)).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=PWTBND(I,J,LBND)
               ENDDO
               ENDDO
               CALL BOUND(GRID1,D00,H99999)
               CALL GRIBIT(IGET(089),LVLS(LBND,IGET(089)),
     X              GRID1,IM,JM)
              ENDIF
            ENDIF
C     
C           BOUNDARY LAYER LIFTED INDEX.
            IF (IGET(075).GT.0) THEN
              IF (LVLS(LBND,IGET(075)).GT.0) THEN
               CALL OTLFT(PBND(1,1,LBND),TBND(1,1,LBND),
     X              QBND(1,1,LBND),EGRID1)
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID1(I,J)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(075),LVLS(LBND,IGET(075)),GRID1,IM,JM)
              ENDIF
            ENDIF
C
C        END OF ETA BOUNDARY LAYER LOOP.
 20      CONTINUE
C     
C        BEST LIFTED INDEX FROM BOUNDARY LAYER FIELDS.
C     
         IF (IGET(031).GT.0) THEN
            DO J=JSTA,JEND
            DO I=1,IM
              EGRID1(I,J) = H99999
              EGRID2(I,J) = H99999
            ENDDO
            ENDDO
C
            DO 50 LBND = 1,NBND
               CALL OTLFT(PBND(1,1,LBND),TBND(1,1,LBND),
     X              QBND(1,1,LBND),EGRID2)
               DO J=JSTA,JEND
               DO I=1,IM
                 EGRID1(I,J)=AMIN1(EGRID1(I,J),EGRID2(I,J))
               ENDDO
               ENDDO
 50         CONTINUE
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID1(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            ID(10)   = PETABND(NBND)+15.
            ID(11)   = PETABND(1)-15.
            CALL GRIBIT(IGET(031),LVLS(1,IGET(031)),
     X           GRID1,IM,JM)
         ENDIF
C     
C        BEST BOUNDARY LAYER CAPE AND CINS.
C     
         FIELD1=.FALSE.
         FIELD2=.FALSE.
C
         IF(IGET(032).GT.0)THEN
           IF(LVLS(2,IGET(032)).GT.0)FIELD1=.TRUE.
         ENDIF
         IF(IGET(107).GT.0)THEN
           IF(LVLS(2,IGET(107)).GT.0)FIELD2=.TRUE.
         ENDIF
C
         IF(FIELD1.OR.FIELD2)THEN
           ITYPE = 2
C
           DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = -H99999
             EGRID2(I,J) = -H99999
           ENDDO
           ENDDO
C
           DO 80 LBND = 1,NBND
           CALL CALTHTE(PBND(1,1,LBND),TBND(1,1,LBND),
     X                  QBND(1,1,LBND),EGRID1)
           DO J=JSTA,JEND
           DO I=1,IM
             IF (EGRID1(I,J).GT.EGRID2(I,J)) THEN
               EGRID2(I,J) = EGRID1(I,J)
	       LB2(I,J)  = LVLBND(I,J,LBND)
               P1D(I,J)  = PBND(I,J,LBND)
               T1D(I,J)  = TBND(I,J,LBND)
               Q1D(I,J)  = QBND(I,J,LBND)
             ENDIF
           ENDDO
           ENDDO
 80        CONTINUE
C
           DPBND=0.
           CALL CALCAPE(ITYPE,DPBND,P1D,T1D,Q1D,LB2,EGRID1,
     X	           EGRID2,EGRID3) 
C
           IF (IGET(032).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID1(I,J)
               ENDDO
               ENDDO
             CALL BOUND(GRID1,D00,H99999)
             ID(1:25) = 0
             ID(09)   = 116
             ID(10)   = PETABND(NBND)+15.
             ID(11)   = PETABND(1)-15.
             CALL GRIBIT(IGET(032),LVLS(1,IGET(032)),
     X                   GRID1,IM,JM)
           ENDIF
C
           IF (IGET(107).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID2(I,J)
               ENDDO
               ENDDO
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = -1.*GRID1(I,J)
             ENDDO
             ENDDO
C
             CALL BOUND(GRID1,D00,H99999)
C
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J) = -1.*GRID1(I,J)
             ENDDO
             ENDDO
C
             ID(1:25) = 0
             ID(09)   = 116
             ID(10)   = PETABND(NBND)+15.
             ID(11)   = PETABND(1)-15.
             CALL GRIBIT(IGET(107),LVLS(1,IGET(107)),
     X                   GRID1,IM,JM)
           ENDIF
         ENDIF
C

C    PBL HEIGHT 
         IF(IGET(221).GT.0) THEN
	   DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J)=PBLH(I,J)
           ENDDO
           ENDDO
	   ID(1:25) = 0
	   CALL GRIBIT(IGET(221),LVLS(1,IGET(221)),
     X                   GRID1,IM,JM)
         END IF
C        BOUNDARY LAYER LIFTING CONDENSATION PRESSURE AND HEIGHT.
C        EGRID1 IS LCL PRESSURE.  EGRID2 IS LCL HEIGHT.
C
         IF ( (IGET(109).GT.0).OR.(IGET(110).GT.0) ) THEN
            CALL CALLCL(PBND(1,1,1),TBND(1,1,1),
     X           QBND(1,1,1),EGRID1,EGRID2)
            IF (IGET(109).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID2(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(109),ILVL,
     X              GRID1,IM,JM)
            ENDIF
            IF (IGET(110).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID1(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(110),ILVL,
     X              GRID1,IM,JM)
            ENDIF
         ENDIF
C     
C        NGM BOUNDARY LAYER FIELDS.
C     
         IF ( (IGET(091).GT.0).OR.(IGET(092).GT.0).OR.
     X        (IGET(093).GT.0).OR.(IGET(094).GT.0).OR.
     X        (IGET(095).GT.0).OR.(IGET(095).GT.0).OR.
     X        (IGET(096).GT.0).OR.(IGET(097).GT.0).OR.
     X        (IGET(098).GT.0) ) THEN
C
C  COMPUTE SIGMA 0.89671 AND 0.78483 TEMPERATURES
C    INTERPOLATE LINEAR IN LOG P
            IF (IGET(097).GT.0.OR.IGET(098).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 P78483(I,J)=ALOG(PINT(I,J,NINT(LMH(I,J)))*0.78483)
                 P89671(I,J)=ALOG(PINT(I,J,NINT(LMH(I,J)))*0.89671)
               ENDDO
               ENDDO
!$omp  parallel do
!$omp& private(fac1,fac2,pkl1,pku1,t78483,t89671)
               DONE =.FALSE.
	       DONE1=.FALSE.
               DO L=2,LM
                DO J=JSTA,JEND
                DO I=1,IM                  
                  PKL1=0.5*(ALPINT(I,J,L)+ALPINT(I,J,L+1))
                  PKU1=0.5*(ALPINT(I,J,L)+ALPINT(I,J,L-1))
		  IF(I.EQ.1 .AND. J.EQ.1)PRINT*,'L,P89671,PKL1,PKU1= ',
     +              L,P89671(I,J), PKL1, PKU1   		  
                  IF(P78483(I,J).LT.PKL1.AND.P78483(I,J).GT.PKU1)THEN
                    FAC1=(PKL1-P78483(I,J))/(PKL1-PKU1)
                    FAC2=(P78483(I,J)-PKU1)/(PKL1-PKU1)
                    T78483(I,J)=T(I,J,L)*FAC2+T(I,J,L-1)*FAC1
		    DONE1(I,J)=.TRUE.
                  ENDIF
                  IF(P89671(I,J).LT.PKL1.AND.P89671(I,J).GT.PKU1)THEN
                    FAC1=(PKL1-P89671(I,J))/(PKL1-PKU1)
                    FAC2=(P89671(I,J)-PKU1)/(PKL1-PKU1)
                    T89671(I,J)=T(I,J,L)*FAC2+T(I,J,L-1)*FAC1
                    DONE(I,J)=.TRUE.
		    IF(I.EQ.1 .AND. J.EQ.1)PRINT*,'done(1,1)= ',done(1,1)
                  ENDIF
                ENDDO
                ENDDO
               ENDDO
!	       print*,'done(1,1)= ',done(1,1)
             DO J=JSTA,JEND
               DO I=1,IM
                 IF(.NOT. DONE(I,J))THEN
		   PL=PINT(I,J,LM-1)
                   TL=0.5*(T(I,J,LM-2)+T(I,J,LM-1))
                   QL=0.5*(Q(I,J,LM-2)+Q(I,J,LM-1))
                   QSAT=PQ0/PL
     1               *EXP(A2*(TL-A3)/(TL-A4))
C
                   RHL=QL/QSAT
C
                   IF(RHL.GT.1.)THEN
                    RHL=1.
                    QL =RHL*QSAT
                   ENDIF
C
                   IF(RHL.LT.RHmin)THEN
                    RHL=RHmin
                    QL =RHL*QSAT
                   ENDIF
C
                   TVRL  =TL*(1.+0.608*QL)
                   TVRBLO=TVRL*(P89671(I,J)/PL)**RGAMOG
                   T89671(I,J)  =TVRBLO/(1.+0.608*QL)
C     
		   
!                   PKL1=0.5*(ALPINT(I,J,LM)+ALPINT(I,J,LM+1))
!                   PKU1=0.5*(ALPINT(I,J,LM-1)+ALPINT(I,J,LM))
!                   T89671(I,J)=T(I,J,LM)+(T(I,J,LM)-T(I,J,LM-1))*
!     +               (P89671(I,J)-PKL1)/(PKL1-PKU1)

!                   print*,'Debug T89671= ',i,j
!     +		     ,P89671(I,J), PKL1, PKU1  
!     +               ,T89671(I,J),T(I,J,LM-1),T(I,J,LM)
                 END IF
		 
		 IF(.NOT. DONE1(I,J))THEN
		   PL=PINT(I,J,LM-1)
                   TL=0.5*(T(I,J,LM-2)+T(I,J,LM-1))
                   QL=0.5*(Q(I,J,LM-2)+Q(I,J,LM-1))
                   QSAT=PQ0/PL
     1               *EXP(A2*(TL-A3)/(TL-A4))
C
                   RHL=QL/QSAT
C
                   IF(RHL.GT.1.)THEN
                    RHL=1.
                    QL =RHL*QSAT
                   ENDIF
C
                   IF(RHL.LT.RHmin)THEN
                    RHL=RHmin
                    QL =RHL*QSAT
                   ENDIF
C
                   TVRL  =TL*(1.+0.608*QL)
                   TVRBLO=TVRL*(P78483(I,J)/PL)**RGAMOG
                   T78483(I,J)  =TVRBLO/(1.+0.608*QL)
C     
                 END IF
		 
               END DO
             END DO
C     
C           SIGMA 0.89671 TEMPERATURE
             IF (IGET(097).GT.0) THEN
               ID(1:25) = 0
               ISVALUE = 8967
               ID(11) = ISVALUE
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=T89671(I,J)
                 IF(T89671(I,J).GT.350.)PRINT*,'LARGE T89671 ',
     +             I,J,T89671(I,J)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(097),LVLS(1,IGET(097)),
     X              GRID1,IM,JM)
             ENDIF
C     
C           SIGMA 0.78483 TEMPERATURE
             IF (IGET(098).GT.0) THEN
               ID(1:25) = 0
               ISVALUE = 7848
               ID(11) = ISVALUE
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=T78483(I,J)
               ENDDO
               ENDDO
               CALL GRIBIT(IGET(098),LVLS(1,IGET(098)),
     X              GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           NGM SIGMA LAYER 0.98230 FIELDS.  THESE FIELDS ARE 
C           THE FIRST ETA LAYER BOUNDARY LAYER FIELDS. 
C     
C     
            IF ( (IGET(091).GT.0).OR.(IGET(092).GT.0).OR.
     X           (IGET(093).GT.0).OR.(IGET(094).GT.0).OR.
     X           (IGET(095).GT.0).OR.(IGET(095).GT.0).OR.
     X           (IGET(096).GT.0) ) THEN
C     
               ID(1:25) = 0
               ISVALUE = 9823
               ID(11) = ISVALUE
C     
C              PRESSURE.
               IF (IGET(091).GT.0) THEN
                 DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J)=PBND(I,J,1)
                 ENDDO
                 ENDDO
                  CALL GRIBIT(IGET(091),LVLS(1,IGET(091)),
     X                 GRID1,IM,JM)
               ENDIF
C     
C              TEMPERATURE.
               IF (IGET(092).GT.0) THEN
                 DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J)=TBND(I,J,1)
                 ENDDO
                 ENDDO
                  CALL GRIBIT(IGET(092),LVLS(1,IGET(092)),
     X                 GRID1,IM,JM)
               ENDIF
C     
C              SPECIFIC HUMIDITY.
               IF (IGET(093).GT.0) THEN
                 DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J)=QBND(I,J,1)
                 ENDDO
                 ENDDO
                  CALL BOUND(GRID1,H1M12,H99999)
                  CALL GRIBIT(IGET(093),LVLS(1,IGET(093)),
     X                 GRID1,IM,JM)
               ENDIF
C     
C              RELATIVE HUMIDITY.
               IF (IGET(094).GT.0) THEN
                 DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J)=RHBND(I,J,1)
                 ENDDO
                 ENDDO
                  CALL SCLFLD(GRID1,H100,IM,JM)
                  CALL BOUND(GRID1,H1,H100)
                  CALL GRIBIT(IGET(094),LVLS(1,IGET(094)),
     X                 GRID1,IM,JM)
               ENDIF
C     
C              U AND/OR V WIND.
               IF ((IGET(095).GT.0).OR.(IGET(096).GT.0)) THEN
                 DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J)=UBND(I,J,1)
                   GRID2(I,J)=VBND(I,J,1)
                 ENDDO
                 ENDDO
                  IF (IGET(095).GT.0) CALL GRIBIT(IGET(095),
     X                 LVLS(1,IGET(095)),GRID1,IM,JM)
                  IF (IGET(096).GT.0) CALL GRIBIT(IGET(096),
     X                 LVLS(1,IGET(096)),GRID2,IM,JM)
               ENDIF
            ENDIF
         ENDIF
C     
C     ENDIF FOR BOUNDARY LAYER BLOCK.
C
      ENDIF
C     
C
C
C     ***BLOCK 6:  MISCELLANEOUS LAYER MEAN LFM AND NGM FIELDS.
C     
      IF ( (IGET(066).GT.0).OR.(IGET(081).GT.0).OR.
     X     (IGET(082).GT.0).OR.(IGET(104).GT.0).OR.
     X     (IGET(099).GT.0).OR.(IGET(100).GT.0).OR.
     X     (IGET(101).GT.0).OR.(IGET(102).GT.0).OR.
     X     (IGET(103).GT.0) ) THEN
C     
C        LFM "MEAN" RELATIVE HUMIDITIES AND PRECIPITABLE WATER.
C     
         IF ( (IGET(066).GT.0).OR.(IGET(081).GT.0).OR.
     X        (IGET(082).GT.0).OR.(IGET(104).GT.0) ) THEN
            CALL LFMFLD(RH3310,RH6610,RH3366,PW3310)
            ID(1:25) = 0
C     
C           SIGMA 0.33-1.00 MEAN RELATIVE HUMIIDITY.
            IF (IGET(066).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RH3310(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10) = 33
               ID(11) = 100
               CALL SCLFLD(GRID1,H100,IM,JM)
               CALL BOUND(GRID1,H1,H100)
               CALL GRIBIT(IGET(066),LVLS(1,IGET(066)),
     X              GRID1,IM,JM)
            ENDIF
C     
C           SIGMA 0.66-1.00 MEAN RELATIVE HUMIIDITY.
            IF (IGET(081).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RH6610(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10) = 67
               ID(11) = 100
               CALL SCLFLD(GRID1,H100,IM,JM)
               CALL BOUND(GRID1,H1,H100)
               CALL GRIBIT(IGET(081),LVLS(1,IGET(081)),
     X              GRID1,IM,JM)
            ENDIF
C     
C           SIGMA 0.33-0.66 MEAN RELATIVE HUMIIDITY.
            IF (IGET(082).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RH3366(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10) = 33
               ID(11) = 67
               CALL SCLFLD(GRID1,H100,IM,JM)
               CALL BOUND(GRID1,H1,H100)
               CALL GRIBIT(IGET(082),LVLS(1,IGET(082)),
     X              GRID1,IM,JM)
            ENDIF
C     
C           SIGMA 0.33-1.00 PRECIPITABLE WATER.
            IF (IGET(104).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=PW3310(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10) = 33
               ID(11) = 100
               CALL BOUND(GRID1,D00,H99999)
               CALL GRIBIT(IGET(104),LVLS(1,IGET(104)),
     X              GRID1,IM,JM)
            ENDIF
         ENDIF
C     
C        VARIOUS LAYER MEAN NGM SIGMA FIELDS.
C     
         IF ( (IGET(099).GT.0).OR.(IGET(100).GT.0).OR.
     X        (IGET(101).GT.0).OR.(IGET(102).GT.0).OR.
     X        (IGET(103).GT.0) ) THEN
            CALL NGMFLD(RH4710,RH4796,RH1847,RH8498,QM8510)
C     
C           SIGMA 0.47191-1.00000 RELATIVE HUMIDITY.
            IF (IGET(099).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RH4710(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10)   = 47
               ID(11)   = 100
               CALL SCLFLD(GRID1,H100,IM,JM)
               CALL BOUND(GRID1,H1,H100)
               CALL GRIBIT(IGET(099),LVLS(1,IGET(099)),
     X              GRID1,IM,JM)
            ENDIF
C     
C           SIGMA 0.47191-0.96470 RELATIVE HUMIDITY.
            IF (IGET(100).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RH4796(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10)   = 47
               ID(11)   = 96
               CALL SCLFLD(GRID1,H100,IM,JM)
               CALL BOUND(GRID1,H1,H100)
               CALL GRIBIT(IGET(100),LVLS(1,IGET(100)),
     X              GRID1,IM,JM)
            ENDIF
C     
C           SIGMA 0.18019-0.47191 RELATIVE HUMIDITY.
            IF (IGET(101).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RH1847(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10)   = 18
               ID(11)   = 47
               CALL SCLFLD(GRID1,H100,IM,JM)
               CALL BOUND(GRID1,H1,H100)
               CALL GRIBIT(IGET(101),LVLS(1,IGET(101)),
     X              GRID1,IM,JM)
            ENDIF
C     
C           SIGMA 0.84368-0.98230 RELATIVE HUMIDITY.
            IF (IGET(102).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RH8498(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10)   = 84
               ID(11)   = 98
               CALL SCLFLD(GRID1,H100,IM,JM)
               CALL BOUND(GRID1,H1,H100)
               CALL GRIBIT(IGET(102),LVLS(1,IGET(102)),
     X              GRID1,IM,JM)
            ENDIF
C     
C           SIGMA 0.85000-1.00000 MOISTURE CONVERGENCE.
            IF (IGET(103).GT.0) THEN
C           CONVERT TO DIVERGENCE FOR GRIB
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=-1.0*QM8510(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               ID(10)   = 85
               ID(11)   = 100
               CALL GRIBIT(IGET(103),LVLS(1,IGET(103)),
     X              GRID1,IM,JM)
            ENDIF
         ENDIF
      ENDIF

C       MIXED LAYER CAPE AND CINS
C
         FIELD1=.FALSE.
         FIELD2=.FALSE.
C
         IF(IGET(032).GT.0)THEN
           IF(LVLS(3,IGET(032)).GT.0)FIELD1=.TRUE.
         ENDIF
         IF(IGET(107).GT.0)THEN
           IF(LVLS(3,IGET(107)).GT.0)FIELD2=.TRUE.
         ENDIF
C
         IF(FIELD1.OR.FIELD2)THEN
           ITYPE = 2
C
           DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = -H99999
             EGRID2(I,J) = -H99999
           ENDDO
           ENDDO
            
            
           DO J=JSTA,JEND
           DO I=1,IM
               LB2(I,J)  = (LVLBND(I,J,1) + LVLBND(I,J,2) +
     X                      LVLBND(I,J,3))/3
               P1D(I,J)  = (PBND(I,J,1) + PBND(I,J,2) + PBND(I,J,3))/3
               T1D(I,J)  = (TBND(I,J,1) + TBND(I,J,2) + TBND(I,J,3))/3
               Q1D(I,J)  = (QBND(I,J,1) + QBND(I,J,2) + QBND(I,J,3))/3
           ENDDO
           ENDDO
C
           DPBND=0.
           CALL CALCAPE(ITYPE,DPBND,P1D,T1D,Q1D,LB2,EGRID1,
     X          EGRID2,EGRID3)
 
           IF (IGET(032).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID1(I,J)
               ENDDO
               ENDDO

               CALL BOUND(GRID1,D00,H99999)
               ID(1:25) = 0
               ID(09)   = 116
               ID(10)   = PETABND(3)+15.
               ID(11)   = PETABND(1)-15.
	       CALL GRIBIT(IGET(32),LVLS(3,IGET(32)),
     X              GRID1,IM,JM)
           ENDIF
                                                                                               
           IF (IGET(107).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=-1.*EGRID2(I,J)
               ENDDO
               ENDDO	    
C
               CALL BOUND(GRID1,D00,H99999)
C
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = -1.*GRID1(I,J)
               ENDDO
               ENDDO
C
               ID(1:25) = 0
               ID(09)   = 116
               ID(10)   = PETABND(3)+15.
               ID(11)   = PETABND(1)-15.
	       CALL GRIBIT(IGET(107),LVLS(3,IGET(107)),
     X              GRID1,IM,JM)
           ENDIF
         ENDIF
              
C        MIXED LAYER LIFTING CONDENSATION PRESSURE AND HEIGHT.
C        EGRID1 IS LCL PRESSURE.  EGRID2 IS LCL HEIGHT.
C
!         IF ( (IGET(109).GT.0).OR.(IGET(110).GT.0) ) THEN
!            CALL CALLCL(P1D,T1D,Q1D,EGRID1,EGRID2)
!            IF (IGET(109).GT.0) THEN
!	       DO J=JSTA,JEND
!               DO I=1,IM
!                 GRID1(I,J)=EGRID2(I,J)
!               ENDDO
!               ENDDO
!           
!               ID(1:25) = 0
!	       
!	       CALL GRIBIT(IGET(109),1,
!     X              GRID1,IM,JM)
!            ENDIF
!	    
!            IF (IGET(110).GT.0) THEN
!	       DO J=JSTA,JEND
!               DO I=1,IM
!                 GRID1(I,J)=EGRID1(I,J)
!               ENDDO
!               ENDDO
!	       
!               ID(1:25) = 0
!	       
!	       CALL GRIBIT(IGET(110),1,
!     X              GRID1,IM,JM)
!            ENDIF
!         ENDIF
C
C       MOST UNSTABLE CAPE-LOWEST 300 MB
C
         FIELD1=.FALSE.
         FIELD2=.FALSE.
C
         IF(IGET(032).GT.0)THEN
           IF(LVLS(4,IGET(032)).GT.0)FIELD1=.TRUE.
         ENDIF
              
         IF(IGET(107).GT.0)THEN
           IF(LVLS(4,IGET(107)).GT.0)FIELD2=.TRUE.
         ENDIF
C
         IF(FIELD1.OR.FIELD2)THEN
           ITYPE = 1
C
           DO J=JSTA,JEND
           DO I=1,IM
             EGRID1(I,J) = -H99999
             EGRID2(I,J) = -H99999
           ENDDO
           ENDDO
              
           DPBND=300.E2
           CALL CALCAPE(ITYPE,DPBND,P1D,T1D,Q1D,LB2,EGRID1,
     X             EGRID2,EGRID3)
C
           IF (IGET(032).GT.0) THEN
	       DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID1(I,J)
               ENDDO
               ENDDO
               CALL BOUND(GRID1,D00,H99999)
               ID(1:25) = 0
               ID(09)   = 116
               ID(10) = 255
               ID(11) = 0
	       CALL GRIBIT(IGET(32),4,
     X              GRID1,IM,JM)
           ENDIF
                
           IF (IGET(107).GT.0) THEN
	       DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=-1.0*EGRID2(I,J)
               ENDDO
               ENDDO

               CALL BOUND(GRID1,D00,H99999)
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = -1.*GRID1(I,J)
               ENDDO
               ENDDO
               ID(1:25)=0
               ID(09)   = 116
               ID(10) = 255
               ID(11) = 0
	       CALL GRIBIT(IGET(107),4,
     X              GRID1,IM,JM)
            ENDIF
              
C      PRESSURE OF LEVEL FROM WHICH 300 MB MOST UNSTABLE CAPE
C             PARCEL WAS LIFTED
           IF (IGET(246).GT.0) THEN
	       DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID3(I,J)
               ENDDO
               ENDDO
               CALL BOUND(GRID1,D00,H99999)
               ID(1:25) = 0
               ID(02) = 129
               ID(09) = 116
               ID(10) = 255
               ID(11) = 0
	       CALL GRIBIT(IGET(246),1,
     X              GRID1,IM,JM)
           ENDIF
         ENDIF  

C CALCULATE LPBL
      IF (IGET(245).GT.0) THEN
       DO 101 J=JSTA,JEND
        DO 101 I=1,IM
         ZSF=ZINT(I,J,NINT(LMH(I,J))+1)
         DO L=NINT(LMH(I,J)),1,-1
          IF(ZINT(I,J,L) .GT. PBLH(I,J)+ZSF)THEN
           LPBL(I,J)=L+1
           IF(LPBL(I,J).GE.LP1) LPBL(I,J) = LM
           GO TO 101
          END IF
         END DO
 101   CONTINUE
       CALL CALGUST(LPBL,PBLH,GUST)
       DO J=JSTA,JEND
       DO I=1,IM
         if(GUST(I,J) .gt. 200. .and. gust(i,j).lt.spval)
     &	 print*,'big gust at ',i,j
         GRID1(I,J)=GUST(I,J)
       ENDDO
       ENDDO      
       ID(1:25) = 0
       CALL GRIBIT(IGET(245),1,
     X              GRID1,IM,JM)      
      END IF
C    
 
C     END OF ROUTINE.
C     
      RETURN
      END
