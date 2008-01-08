      SUBROUTINE MDLFLD
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    MDLFLD      SLP AND NATIVE LEVEL POSTING
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-21       
C     
C ABSTRACT:
C     THIS ROUTINE DOES SEVERAL THINGS.  IT IS THE FIRST 
C     ROUTINE CALLED BY POST PROCESSOR SUBROUTINE PROCESS 
C     WHICH SETS THE ORDER IN WHICH FIELDS ARE POSTED.  THE
C     NEGATIVE SPECIFIC HUMIDITY IS CLIPPED.
C     COMPUTE THE STANDARD NMC SEA LEVEL PRESSURE IF THIS OPTION
C     IS ACTIVATED.  FINALLY WE COMPUTE/POST REQUESTED FIELDS ON
C     MODEL LAYERS.
C
C   .     
C     
C PROGRAM HISTORY LOG:
C   92-12-21  RUSS TREADON
C   93-09-01  RUSS TREADON - ADDED ADDITIONAL OUTPUT FIELDS.
C   96-03-20  MIKE BALDWIN - ADDED CLOUD TOP TEMPS, CHANGE CLOUD WATER
C                            TO CONTAIN WATER ONLY
C   97-04-29  GEOFF MANIKIN - MOVED CLOUD TOP TEMPS TO CLDRAD
C   98-06-01  T BLACK - CONVERSION FROM 1-D TO 2-D
C   98-07-20  MIKE BALDWIN - REMOVED LABL84
C   98-08-18  T BLACK - REMOVED EXCESS SPACE IN EXTRA.com
C   00-01-04  JIM TUCCILLO - MPI VERSION
C   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-01-15  MIKE BALDWIN - WRF VERSION
C   04-11-17  H CHUANG, B FERRIER, AND Y JIN - ADD HYDROMETEORS, 
C					VISIBILITY & RADAR REFLECTIVITY
C   05-07-07  B ZHOU ADD RSM MODEL A GRID     
C   05-08-18  B ZHOU ADD /VISB/ COMMON BLOCK TO PASS VISIBILITY TO
C                        AVIATION SUBROUTINE TO CALCULATE FLIGHT
C                        CONDITION RESTRICTION
C
C USAGE:    CALL MDLFLD
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
C       BOUND    - BOUND ARRAY ELEMENTS BETWEEN LOWER AND UPPER LIMITS.
C       SCLFLD   - SCALE ARRAY ELEMENTS BY SCALAR CONSTANT.
C       NGMSLP   - COMPUTE SLP USING STANDARD NMC REDUCTION METHOD.
C       CALPOT   - COMPUTE POTENTIAL TEMPERATURE.
C       CALRH    - COMPUTE RELATIVE HUMIDITY.
C       CALDWP   - COMPUTE DEWPOINT TEMPERATURE.
C       CALMCVG  - COMPUTE MOISTURE CONVERGENCE.
C       CALVOR   - COMPUTE ABSOLUTE VORTICITY.
C       CALSTRM  - COMPUTE GEOSTROPHIC STREAMFUNCTION.
C       CALMICT  - COMPUTES NEW CLOUD FIELDS AND RADAR REFLECTIVITY FACTOR
C     LIBRARY:
C       COMMON   - 
C                  RQSTFLD
C                  CTLBLK
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
C     INCLUDE ETA MODEL DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
C     
!      INCLUDE "parmeta"
!      INCLUDE "parmout"
      INCLUDE "params"
C     INCLUDE REQUIRED COMMONS.
      INCLUDE "RQSTFLD.comm"
      INCLUDE "CTLBLK.comm"
C
C      PARAMETER (RAINCON=1.1787E4)
C      PARAMETER (SNOCON=1.4594E5)
C      PARAMETER (VCON1=1.66476,VCON2=0.55683)
      REAL, PARAMETER :: CURATE=24.*1000., CTIM1=0., CTIM2=24.*3600.
     &, PTHRESH=0.000004, RAINCON=0.8333*1.1787E4, SNOCON=0.94*1.4594E5
!
!--- 88D reflectivity algorithm, Z = 300.*R**1.4 , R is rain rate in mm/h
!
     &, DBZmax=80., ZR_A=300., ZR_B=1.4
!
!--- Modification of Slingo (1987) to enhance convective cloudiness
!
      REAL CC(10), PPT(10)
      DATA CC / 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0 /
      DATA PPT/  0., .14, .31, .70, 1.6, 3.4, 7.7, 17., 38., 85. /
      INTEGER ICBOT(IM,JM),ICTOP(IM,JM) 

C     
C     DECLARE VARIABLES.
C     
      LOGICAL RUN,FIRST,RESTRT,SIGMA
      LOGICAL NORTH,NEED(IM,JM)
      REAL EGRID1(IM,JM),EGRID2(IM,JM),EGRID3(IM,JM),EGRID4(IM,JM)
     &,     EL0(IM,JM)
     &,     P1D(IM,JM),T1D(IM,JM),Q1D(IM,JM),EGRID5(IM,JM)
     &,     C1D(IM,JM),FI1D(IM,JM),FR1D(IM,JM),FS1D(IM,JM)
     &,     QW1(IM,JM),QI1(IM,JM),QR1(IM,JM),QS1(IM,JM)
     &,     GRID1(IM,JM), GRID2(IM,JM)
     &,     CUREFL_S(IM,JM), CUREFL(IM,JM), CUREFL_I(IM,JM)
     &,     Zfrz(IM,JM), DBZ1(IM,JM),DBZR1(IM,JM),DBZI1(IM,JM)
     &,     DBZC1(IM,JM)
C
      REAL, ALLOCATABLE :: EL(:,:,:),RICHNO(:,:,:),PBLRI(:,:)
C
      REAL QI(IM,JM),QINT(IM,JM)
      REAL TT(IM,JM),PPP(IM,JM),QV(IM,JM),QCD(IM,JM),QICE1(IM,JM)
      REAL QRAIN1(IM,JM),QSNO1(IM,JM)
!      REAL VIS(IM,JM)
C     

C ADD by B Zhou
!      COMMON /VISB/VIS
C
C     
C*****************************************************************************
C     START SUBROUTINE MDLFLD.
C
C     ALLOCATE LOCAL ARRAYS
C
      ALLOCATE(EL     (IM,JSTA_2L:JEND_2U,LM))     
      ALLOCATE(RICHNO (IM,JSTA_2L:JEND_2U,LM))     
      ALLOCATE(PBLRI  (IM,JSTA_2L:JEND_2U))     
CHC COMMENT OUT THE CALL BECAUSE MEMBRANE SLP REDUCTION IS
CHC NOW DONE IN MDL2P
C
C     OUTPUT SEA LEVEL PRESSURE IF REQUESTED.
C     FIRST, MESINGER'S SEA LEVEL PRESSURE.
CHC      IF (IGET(023).GT.0) THEN
CHC         DO J=JSTA,JEND
CHC         DO I=1,IM
CHC           GRID1(I,J)=PSLP(I,J)
CHC         ENDDO
CHC         ENDDO
CHC         ID(1:25) = 0
CHC         CALL GRIBIT(IGET(023),LVLS(1,IGET(023)),
CHC     X        GRID1,IM,JM)
CHC      ENDIF
C     
C     SECOND, STANDARD NGM SEA LEVEL PRESSURE.
      IF (IGET(105).GT.0) THEN
         CALL NGMSLP
         DO J=JSTA,JEND
         DO I=1,IM
           GRID1(I,J)=SLP(I,J)
         ENDDO
         ENDDO
         ID(1:25) = 0
         CALL GRIBIT(IGET(105),LVLS(1,IGET(105)),
     X        GRID1,IM,JM)
      ENDIF
!
!--- Calculate convective cloud fractions following RADTN in Eta,
!    convective radar reflectivity at the surface (CUREFL_S), and
!    the decrease in reflectivity above the 0C level (CUREFL_I)
!
      IF(imp_physics.eq.5)THEN
       print*,'DTQ2 in MDLFLD= ',DTQ2
       RDTPHS=3.6E6/DTQ2
       DO J=JSTA,JEND
        DO I=1,IM
          CUPRATE=RDTPHS*CPRATE(I,J)            !--- Cu precip rate, R (mm/h)
!          CUPRATE=CUPPT(I,J)/(TRDLW*3600.)*3.6E6
          Zfrz(I,J)=ZMID(I,J,NINT(LMH(I,J)))  !-- Initialize to lowest model level
          DO L=1,NINT(LMH(I,J))               !-- Start from the top, work down
             IF (T(I,J,L) .GE. TFRZ) THEN
                Zfrz(I,J)=ZMID(I,J,L)         !-- Find highest level where T>0C
                EXIT
             ENDIF
          ENDDO
          IF (CUPRATE .LE. 0. .OR. CUPPT(I,J).LE.0.) THEN
             CUREFL_S(I,J)=0.
             CUREFL_I(I,J)=0.
          ELSE
             CUREFL_S(I,J)=ZR_A*CUPRATE**ZR_B   !--- Use Z=A*R**B
             Lctop=NINT(HTOP(I,J))              !--- Cu cld top level
  !
  !--- Assume convective reflectivity (Z, not dBZ) above 0C level decreases
  !    with height by two orders of magnitude (20 dBZ) from the 0C level up
  !    to cloud top.  If cloud top temperature is above 0C, assume 20 dBZ
  !    decrease occurs in the first 1 km above the 0C level.
  !
             CUREFL_I(I,J)=-2./MAX( 1000., ZMID(I,J,Lctop)-Zfrz(I,J) )
          ENDIF                                                                  
          IF ((HBOT(I,J)-HTOP(I,J)) .LE. 1.0) THEN
            ICBOT(I,J)=0
            ICTOP(I,J)=0
            CNVCFR(I,J)=0.
          ELSE
            ICBOT(I,J)=NINT(HBOT(I,J))
            ICTOP(I,J)=NINT(HTOP(I,J))
            CFRdum=CC(1)
!            PMOD=CUPPT(I,J)*CURATE
            PMOD=RDTPHS*CPRATE(I,J)*24  ! mm/day
            IF (PMOD .GT. PPT(1)) THEN
              DO NC=1,10
                IF(PMOD.GT.PPT(NC)) NMOD=NC
              ENDDO
              IF (NMOD .GE. 10) THEN
                CFRdum=CC(10)
              ELSE
                CC1=CC(NMOD)
                CC2=CC(NMOD+1)
                P1=PPT(NMOD)
                P2=PPT(NMOD+1)
                CFRdum=CC1+(CC2-CC1)*(PMOD-P1)/(P2-P1)
              ENDIF   !--- End IF (NMOD .GE. 10) ...
              CFRdum=MIN(H1, CFRdum)
            ENDIF     !--- End IF (PMOD .GT. PPT(1)) ...
            CNVCFR(I,J)=100.*CFRdum
          ENDIF       !--- End IF (HBOT(I,J)-HTOP(I,J) .LE. 1.0) ...
        ENDDO         !--- End DO I
       ENDDO    

C
!--- Calculate each hydrometeor category & GRID-SCALE cloud fraction
!    (Jin, Aug-Oct '01; Ferrier, Feb '02)
!
       DO L=1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          P1D(I,J)=PMID(I,J,L)
          T1D(I,J)=T(I,J,L)
          Q1D(I,J)=Q(I,J,L)
          C1D(I,J)=CWM(I,J,L)
          FI1D(I,J)=F_ice(I,J,L)
          FR1D(I,J)=F_rain(I,J,L)
          FS1D(I,J)=MIN(H1, F_RimeF(I,J,L))
    !
    !--- Estimate radar reflectivity factor at level L
    !
          CUREFL(I,J)=0.
          IF (CUREFL_S(I,J) .GT. 0.) THEN
             FCTR=0.
             LLMH = NINT(LMH(I,J)) 
             Lctop=NINT(HTOP(I,J))              !--- Cu cld top level
             IF (L.GE.Lctop .AND. L.LE.LLMH) THEN
                DELZ=ZMID(I,J,L)-Zfrz(I,J)
                IF (DELZ .LE. 0.) THEN
                   FCTR=1.        !-- Below the highest freezing level
                ELSE
       !
       !--- Reduce convective radar reflectivity above freezing level
       !
                   FCTR=10.**(CUREFL_I(I,J)*DELZ)
                ENDIF             !-- End IF (DELZ .LE. 0.)
             ENDIF                !-- End IF (L.GE.HTOP(I,J) .OR. L.LE.LLMH)
             CUREFL(I,J)=FCTR*CUREFL_S(I,J)
          ENDIF                   !-- End IF (CUREFL_S(I,J) .GT. 0.)

        ENDDO         !-- End DO I loop
        ENDDO         !-- End DO J loop 
  !
  !--- Determine composition of condensate in terms of cloud water,
  !    rain, and ice (cloud ice & precipitation ice) following
  !    GSMDRIVE in the model; composition of cloud ice & precipitation
  !    ice (snow) follows algorithm in GSMCOLUMN; radar reflectivity
  !    is derived to be consistent with microphysical assumptions 
  !
!      CALL CALMICT(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,QW1,QI1,QR1,QS1)
        CALL CALMICT(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL
     &               ,QW1,QI1,QR1,QS1,DBZ1,DBZR1,DBZI1,DBZC1)
        DO J=JSTA,JEND
        DO I=1,IM
          LLMH = NINT(LMH(I,J))
          IF (L .GT. LLMH) THEN
            QQW(I,J,L)=D00
            QQI(I,J,L)=D00
            QQR(I,J,L)=D00
            QQS(I,J,L)=D00
            CFR(I,J,L)=D00
            DBZ(I,J,L)=DBZmin
            DBZR(I,J,L)=DBZmin
            DBZI(I,J,L)=DBZmin
            DBZC(I,J,L)=DBZmin
          ELSE
            QQW(I,J,L)=MAX(D00, QW1(I,J))
            QQI(I,J,L)=MAX(D00, QI1(I,J))
            QQR(I,J,L)=MAX(D00, QR1(I,J))
            QQS(I,J,L)=MAX(D00, QS1(I,J))
            DBZ(I,J,L)=MAX(DBZmin, DBZ1(I,J))
            DBZR(I,J,L)=MAX(DBZmin, DBZR1(I,J))
            DBZI(I,J,L)=MAX(DBZmin, DBZI1(I,J))
            DBZC(I,J,L)=MAX(DBZmin, DBZC1(I,J))
          ENDIF       !-- End IF (L .GT. LMH(I,J)) ...
        ENDDO         !-- End DO I loop
        ENDDO         !-- End DO J loop
                                        
       ENDDO           !-- End DO L loop        

      ELSE
! compute radar reflectivity for non-ferrier's scheme      
        print*,'calculating radar ref for non-Ferrier scheme' 
! Determine IICE FLAG
        IF(IMP_PHYSICS.EQ.1 .OR. IMP_PHYSICS.EQ.3)THEN
          IICE=0
        ELSE
          IICE=1
        END IF
        PRINT*,'IICE= ',IICE
        DO L=1,LM
         DO J=JSTA,JEND
          DO I=1,IM
            IF(T(I,J,L) .LT. 1.0E-3)print*,'ZERO T'    
            IF(T(I,J,L) .gt. 1.0E-3)   
     &       DENS=PMID(I,J,L)/
     &         (RD*T(I,J,L)*(Q(I,J,L)*D608+1.0))      ! DENSITY
! PATCH to set QQR, QQS, AND QQG to zeros if they are negative so that post won't abort
            IF(QQR(I,J,L).LT. 0.0)QQR(I,J,L)=0.0
            IF (IICE.EQ.0) THEN
               IF (T(I,J,L) .GE. TFRZ) THEN
                  DBZ(I,J,L)=((QQR(I,J,L)*DENS)**1.75)*
     &               3.630803E-9 * 1.E18                  ! Z FOR RAIN
                  DBZR(I,J,L)=DBZ(I,J,L)
               ELSE
                  DBZ(I,J,L)=((QQR(I,J,L)*DENS)**1.75)*
     &               2.18500E-10 * 1.E18                  ! Z FOR SNOW
                  DBZI(I,J,L)=DBZ(I,J,L)
               ENDIF
            ELSEIF (IICE.EQ.1) THEN
	       IF(QQS(I,J,L).LT. 0.0)QQS(I,J,L)=0.0
	       IF(QQG(I,J,L).LT. 0.0)QQG(I,J,L)=0.0
               DBZR(I,J,L)=((QQR(I,J,L)*DENS)**1.75)*
     &               3.630803E-9 * 1.E18                  ! Z FOR RAIN
               DBZI(I,J,L)= DBZI(I,J,L)+
     &            ((QQS(I,J,L)*DENS)**1.75)*
     &               2.18500E-10 * 1.E18                  ! Z FOR SNOW
               DBZI(I,J,L)= DBZI(I,J,L)+
     &            ((QQG(I,J,L)*DENS)**1.75)*
     &               1.033267E-9 * 1.E18                  ! Z FOR GRAUP
               DBZ(I,J,L)=DBZR(I,J,L)+DBZI(I,J,L)
!               IF(L.EQ.27.and.QQR(I,J,L).gt.1.e-4)print*,
!     &'sample QQR DEN,DBZ= ',QQR(I,J,L),DENS,DBZ(I,J,L)
            ENDIF
            IF (DBZ(I,J,L).GT.0.)
     &         DBZ(I,J,L)=10.0*LOG10(DBZ(I,J,L)) ! DBZ
            IF (DBZR(I,J,L).GT.0.)
     &         DBZR(I,J,L)=10.0*LOG10(DBZR(I,J,L)) ! DBZ
            IF (DBZI(I,J,L).GT.0.)
     &         DBZI(I,J,L)=10.0*LOG10(DBZI(I,J,L)) ! DBZ
            LLMH = NINT(LMH(I,J))
            IF(L.GT.LLMH)THEN
             DBZ(I,J,L)=DBZmin
             DBZR(I,J,L)=DBZmin
             DBZI(I,J,L)=DBZmin
            ELSE
             DBZ(I,J,L)=MAX(DBZmin, DBZ(I,J,L))
             DBZR(I,J,L)=MAX(DBZmin, DBZR(I,J,L))
             DBZI(I,J,L)=MAX(DBZmin, DBZI(I,J,L))
            END IF 
           ENDDO
          ENDDO
         ENDDO
      END IF
C     
C     OUTPUT/CALCULATE PRESSURE, OMEGA, POTENTIAL TEMPERATURE,
C     DEWPOINT TEMPERATURE, RELATIVE HUMIDITY, AND 
C     ABSOLUTE VORTICITY ON MDL SURFACES.
C     
C
      IF ( (IGET(001).GT.0).OR.(IGET(077).GT.0).OR.
     X     (IGET(002).GT.0).OR.(IGET(003).GT.0).OR.
     X     (IGET(004).GT.0).OR.(IGET(005).GT.0).OR.
     X     (IGET(006).GT.0).OR.(IGET(083).GT.0).OR.
     X     (IGET(007).GT.0).OR.(IGET(008).GT.0).OR.
     X     (IGET(009).GT.0).OR.(IGET(010).GT.0).OR.
     X     (IGET(084).GT.0).OR.(IGET(011).GT.0).OR.
     X     (IGET(041).GT.0).OR.(IGET(124).GT.0).OR.
     X     (IGET(078).GT.0).OR.(IGET(079).GT.0).OR.
     X     (IGET(125).GT.0).OR.(IGET(145).GT.0).OR.
     X     (IGET(140).GT.0).OR.(IGET(040).GT.0).OR.
C     X     (IGET(180).GT.0) ) THEN
     X     (IGET(181).GT.0).OR.(IGET(182).GT.0).OR.
     X     (IGET(199).GT.0).OR.(IGET(185).GT.0).OR.
     X     (IGET(186).GT.0).OR.(IGET(187).GT.0).OR.
     X     (IGET(250).GT.0).OR.(IGET(252).GT.0).OR.
     X     (IGET(276).GT.0).OR.(IGET(277).GT.0).OR.
     X     (IGET(278).GT.0).OR.(IGET(264).GT.0) )  THEN

      DO 190 L=1,LM

C           PRESSURE ON MDL SURFACES.
            IF (IGET(001).GT.0) THEN
             IF (LVLS(L,IGET(001)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=PMID(I,J,LL)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(001),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
!
!---  CLOUD WATER on MDL SURFACE  (Jin, '01; Ferrier, Feb '02)
!
          IF (IGET(124) .GT. 0) THEN
            IF (LVLS(L,IGET(124)) .GT. 0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=QQW(I,J,LL)
               ENDDO
               ENDDO	    
               ID(1:25) = 0
	       CALL GRIBIT(IGET(124),L,GRID1,IM,JM)
            ENDIF
          ENDIF 
!
!---  CLOUD ICE ON MDL SURFACE  (Jin, '01; Ferrier, Feb '02)
!
          IF (IGET(125) .GT. 0) THEN
            IF (LVLS(L,IGET(125)) .GT. 0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=QQI(I,J,LL)
               ENDDO
               ENDDO	   	    
               ID(1:25) = 0
	       CALL GRIBIT(IGET(125),L,GRID1,IM,JM)
            ENDIF
          ENDIF
!
!---  RAIN ON MDL SURFACE  (Jin, '01; Ferrier, Feb '02)
!
          IF (IGET(181) .GT. 0) THEN
            IF (LVLS(L,IGET(181)) .GT. 0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=QQR(I,J,LL)
               ENDDO
               ENDDO
               ID(1:25) = 0
	       CALL GRIBIT(IGET(181),L,GRID1,IM,JM)
            ENDIF
          ENDIF
!
!---  SNOW ON MDL SURFACE  (Jin, '01; Ferrier, Feb '02)
!
          IF (IGET(182) .GT. 0) THEN
            IF (LVLS(L,IGET(182)) .GT. 0)THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=QQS(I,J,LL)
               ENDDO
               ENDDO	    
               ID(1:25) = 0
	       CALL GRIBIT(IGET(182),L,GRID1,IM,JM)
            ENDIF
          ENDIF
!
!---  Total cloud fraction on MDL surfaces.  (Ferrier, Nov '04)
!
          IF (IGET(145) .GT. 0) THEN
            IF (LVLS(L,IGET(145)) .GT. 0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=CFR(I,J,LL)*H100
               ENDDO
               ENDDO
               CALL BOUND(GRID1,D00,H100)
               ID(1:25) = 0
               CALL GRIBIT(IGET(145),L,GRID1,IM,JM)
            ENDIF
          ENDIF
!
!---  Equivalent radar reflectivity factor.  
!
          IF (IGET(250) .GT. 0) THEN
            IF (LVLS(L,IGET(250)) .GT. 0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=DBZ(I,J,LL)
               ENDDO
               ENDDO
               CALL BOUND(GRID1,DBZmin,DBZmax)
               ID(1:25) = 0
	       ID(02)=129
               CALL GRIBIT(IGET(250),L,GRID1,IM,JM) 
            ENDIF
          ENDIF

!
!--- TOTAL CONDENSATE ON MDL SURFACE (CWM array; Ferrier, Feb '02)
!
          IF (IGET(199).GT.0) THEN
            IF (LVLS(L,IGET(199)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=CWM(I,J,LL)
               ENDDO
               ENDDO	     
               ID(1:25) = 0
               ID(02)=129      !--- Parameter Table 129, PDS Octet 4 = 129)
	       CALL GRIBIT(IGET(199),L,GRID1,IM,JM)
            ENDIF
          ENDIF
!
!---  F_rain ON MDL SURFACE  (Jin, '01; Ferrier, Feb '02)
!
          IF (IGET(185).GT.0) THEN
            IF (LVLS(L,IGET(185)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=F_rain(I,J,LL)
               ENDDO
               ENDDO	    
               ID(1:25) = 0
               ID(02)=129      !--- Parameter Table 129, PDS Octet 4 = 129)
	       CALL GRIBIT(IGET(185),L,GRID1,IM,JM)
            ENDIF
          ENDIF
!
!---  F_ice ON MDL SURFACE  (Jin, '01; Ferrier, Feb '02)
!
          IF (IGET(186).GT.0) THEN
            IF (LVLS(L,IGET(186)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=F_ice(I,J,LL)
               ENDDO
               ENDDO	    
               ID(1:25) = 0
               ID(02)=129      !--- Parameter Table 129, PDS Octet 4 = 129)
	       CALL GRIBIT(IGET(186),L,GRID1,IM,JM)
            ENDIF
          ENDIF
!
!---  F_RimeF ON MDL SURFACE  (Jin, '01; Ferrier, Feb '02)
!
          IF (IGET(187).GT.0) THEN
            IF (LVLS(L,IGET(187)).GT.0) THEN
!--- Filter "rime factor" for non-zero precip rates and % frozen precip
              LL=LM-L+1
              DO J=JSTA,JEND
                DO I=1,IM
                 GRID1(I,J)=F_RimeF(I,J,LL)		 
                ENDDO
              ENDDO
              ID(1:25) = 0
              ID(02)=129      !--- Parameter Table 129, PDS Octet 4 = 129)
	      CALL GRIBIT(IGET(187),L,GRID1,IM,JM)
            ENDIF
          ENDIF
C	  
C           HEIGHTS ON MDL SURFACES.
            IF (IGET(077).GT.0) THEN
             IF (LVLS(L,IGET(077)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=ZMID(I,J,LL)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(077),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           TEMPERATURE ON MDL SURFACES.
            IF (IGET(002).GT.0) THEN
             IF (LVLS(L,IGET(002)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=T(I,J,LL)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(002),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           POTENTIAL TEMPERATURE ON MDL SURFACES.
            IF (IGET(003).GT.0) THEN
             IF (LVLS(L,IGET(003)).GT.0) THEN
              LL=LM-L+1
              IF(MODELNAME .EQ. 'NCAR')THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=TH(I,J,LL)
               ENDDO
               ENDDO
              ELSE
               DO J=JSTA,JEND
               DO I=1,IM
                 P1D(I,J)=PMID(I,J,LL)
                 T1D(I,J)=T(I,J,LL)
               ENDDO
               ENDDO
               CALL CALPOT(P1D,T1D,EGRID3)
                                                                                
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID3(I,J)
               ENDDO
               ENDDO
              END IF
              ID(1:25) = 0
              CALL GRIBIT(IGET(003),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           RELATIVE HUMIDITY ON MDL SURFACES.
            IF (IGET(006).GT.0) THEN
             IF (LVLS(L,IGET(006)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 P1D(I,J)=PMID(I,J,LL)
                 T1D(I,J)=T(I,J,LL)
                 Q1D(I,J)=Q(I,J,LL)
               ENDDO
               ENDDO
               CALL CALRH(P1D,T1D,Q1D,EGRID4)
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID4(I,J)*100.
               ENDDO
               ENDDO
!               CALL BOUND(GRID1,H1,H100)
               ID(1:25) = 0
               CALL GRIBIT(IGET(006),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           DEWPOINT ON MDL SURFACES.
            IF (IGET(004).GT.0) THEN
             IF (LVLS(L,IGET(004)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 P1D(I,J)=PMID(I,J,LL)
                 T1D(I,J)=T(I,J,LL)
                 Q1D(I,J)=Q(I,J,LL)
               ENDDO
               ENDDO
               CALL CALDWP(P1D,Q1D,EGRID3,T1D)
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID3(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(004),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           SPECIFIC HUMIDITY ON MDL SURFACES.
            IF (IGET(005).GT.0) THEN
             IF (LVLS(L,IGET(005)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=Q(I,J,LL)
               ENDDO
               ENDDO
               CALL BOUND(GRID1,H1M12,H99999)
               ID(1:25) = 0
               CALL GRIBIT(IGET(005),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           MOISTURE CONVERGENCE ON MDL SURFACES.
            IF (IGET(083).GT.0 .OR. IGET(295).GT.0) THEN
             IF (LVLS(L,IGET(083)).GT.0 .OR. IGET(295).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA_2L,JEND_2U
               DO I=1,IM
                 Q1D(I,J)=Q(I,J,LL)
                 EGRID1(I,J)=UH(I,J,LL)
                 EGRID2(I,J)=VH(I,J,LL)
               ENDDO
               ENDDO
               CALL CALMCVG(Q1D,EGRID1,EGRID2,EGRID3)
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID3(I,J)
		 MCVG(I,J,LL)=EGRID3(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(083),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           U AND/OR V WIND ON MDL SURFACES.
CMEB needs to be modified to do u at u-points and v at v-points
            IF (IGET(007).GT.0.OR.IGET(008).GT.0) THEN
             IF (LVLS(L,IGET(007)).GT.0.OR.LVLS(L,IGET(008)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=UH(I,J,LL)
                 GRID2(I,J)=VH(I,J,LL)
               ENDDO
               ENDDO
               ID(1:25) = 0
               IF (IGET(007).GT.0)
     X              CALL GRIBIT(IGET(007),L,GRID1,IM,JM)
               ID(1:25) = 0
               IF (IGET(008).GT.0)
     X              CALL GRIBIT(IGET(008),L,GRID2,IM,JM)
             ENDIF
            ENDIF
C     
C           OMEGA ON MDL SURFACES.
            IF (IGET(009).GT.0) THEN
             IF (LVLS(L,IGET(009)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=OMGA(I,J,LL)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(009),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           W ON MDL SURFACES.
            IF (IGET(264).GT.0) THEN
             IF (LVLS(L,IGET(264)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=WH(I,J,LL)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(264),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           ABSOLUTE VORTICITY ON MDL SURFACES.
            IF (IGET(010).GT.0) THEN
             IF (LVLS(L,IGET(010)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA_2L,JEND_2U
               DO I=1,IM
                 EGRID1(I,J)=UH(I,J,LL)
                 EGRID2(I,J)=VH(I,J,LL)
               ENDDO
               ENDDO
               CALL CALVOR(EGRID1,EGRID2,EGRID3)
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID3(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(010),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           GEOSTROPHIC STREAMFUNCTION ON MDL SURFACES.
            IF (IGET(084).GT.0) THEN
             IF (LVLS(L,IGET(084)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 EGRID1(I,J)=ZMID(I,J,LL)
               ENDDO
               ENDDO
               CALL CALSTRM(EGRID1,EGRID2)
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EGRID2(I,J)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(084),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C           TURBULENT KINETIC ENERGY ON MDL SURFACES.
            IF (IGET(011).GT.0) THEN
             IF (LVLS(L,IGET(011)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=Q2(I,J,LL)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(011),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C    
C           CLOUD WATER CONTENT
CHC            IF (IGET(124).GT.0) THEN
CHC             IF (LVLS(L,IGET(124)).GT.0) THEN
CHC              DO J=JSTA,JEND
CHC              DO I=1,IM
CHC                IF(CWM(I,J,L).LT.0..AND.CWM(I,J,L).GT.-1.E-10)
CHC     1            CWM(I,J,L)=0.
CHC                 GRID1(I,J)=CWM(I,J,L)
CHC              ENDDO
CHC              ENDDO
CHC              ID(1:25) = 0
CHC              CALL GRIBIT(IGET(124),L,GRID1,IM,JM)
CHC             ENDIF
CHC            ENDIF
C     
C           CLOUD ICE CONTENT.
!commented out until QICE is brought into post
!           IF (IGET(125).GT.0) THEN
!            IF (LVLS(L,IGET(125)).GT.0) THEN
!              DO J=JSTA,JEND
!              DO I=1,IM
!                GRID1(I,J)=QICE(I,J,L)
!              ENDDO
!              ENDDO
!              ID(1:25) = 0
!              CALL GRIBIT(IGET(125),L,GRID1,IM,JM)
!            ENDIF
!           ENDIF
C     
C           CLOUD FRACTION
C     
!commented out until CFRC is brought into post
!           IF (IGET(145).GT.0) THEN
!            IF (LVLS(L,IGET(145)).GT.0) THEN
!              DO J=JSTA,JEND
!              DO I=1,IM
!                GRID1(I,J)=CFRC(I,J,L)
!              ENDDO
!              ENDDO
!              ID(1:25) = 0
!              CALL GRIBIT(IGET(145),L,GRID1,IM,JM)
!            ENDIF
!           ENDIF
C     
C           TEMPERATURE TENDENCY DUE TO RADIATIVE FLUX CONVERGENCE
!commented out until TTND is brought into post
           IF (IGET(140).GT.0) THEN
            IF (LVLS(L,IGET(140)).GT.0) THEN
	      LL=LM-L+1
              DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J)=TTND(I,J,LL)
              ENDDO
              ENDDO
                 ID(1:25) = 0
                 CALL GRIBIT(IGET(140),L,GRID1,IM,JM)
            ENDIF
           ENDIF
C     
C           TEMPERATURE TENDENCY DUE TO SHORT WAVE RADIATION.
!commented out until RSWTT is brought into post
           IF (IGET(040).GT.0) THEN
            IF (LVLS(L,IGET(040)).GT.0) THEN
	      LL=LM-L+1
              DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J)=RSWTT(I,J,LL)
              ENDDO
              ENDDO
                 ID(1:25) = 0
                 CALL GRIBIT(IGET(040),L,GRID1,IM,JM)
            ENDIF
           ENDIF
C     
C           TEMPERATURE TENDENCY DUE TO LONG WAVE RADIATION.
!commented out until RLWTT is brought into post
           IF (IGET(041).GT.0) THEN
            IF (LVLS(L,IGET(041)).GT.0) THEN
	      LL=LM-L+1
              DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J)=RLWTT(I,J,LL)
              ENDDO
              ENDDO
                 ID(1:25) = 0
                 CALL GRIBIT(IGET(041),L,GRID1,IM,JM)
            ENDIF
           ENDIF
C
C     
C        PROCESS NEXT MDL LEVEL.
C
C           LATENT HEATING FROM GRID SCALE RAIN/EVAP. (TIME AVE)
           IF (IGET(078).GT.0) THEN
            IF (LVLS(L,IGET(078)).GT.0) THEN
	       LL=LM-L+1 
               IF(AVRAIN.GT.0.)THEN 
                 RRNUM=1./AVRAIN
               ELSE
                 RRNUM=0.
               ENDIF
!$omp  parallel do
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=TRAIN(I,J,LL)*RRNUM
               ENDDO
               ENDDO
               ID(1:25) = 0
               ITHEAT     = INT(THEAT)
	       IF (ITHEAT .NE. 0) THEN
                IFINCR     = MOD(IFHR,ITHEAT)
	       ELSE
	        IFINCR=0
	       END IF		
               ID(19) = IFHR
	       IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
               ID(20) = 3
               IF (IFINCR.EQ.0) THEN
                  ID(18) = IFHR-ITHEAT
               ELSE
                  ID(18) = IFHR-IFINCR
               ENDIF
	       IF(IFMIN .GE. 1)ID(18)=ID(18)*60
               IF (ID(18).LT.0) ID(18) = 0
               CALL GRIBIT(IGET(078),L,GRID1,IM,JM)
            END IF
           ENDIF
C
C           LATENT HEATING FROM CONVECTION. (TIME AVE)
           IF (IGET(079).GT.0) THEN
            IF (LVLS(L,IGET(079)).GT.0) THEN
	       LL=LM-L+1 
               IF(AVCNVC.GT.0.)THEN
                 RRNUM=1./AVCNVC
               ELSE
                 RRNUM=0.
               ENDIF
!$omp  parallel do
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = TCUCN(I,J,LL)*RRNUM
               ENDDO
               ENDDO
               ID(1:25) = 0
               ITHEAT     = INT(THEAT)
	       IF (ITHEAT .NE. 0) THEN
                IFINCR     = MOD(IFHR,ITHEAT)
	       ELSE
	        IFINCR=0
	       END IF	
               ID(19) = IFHR
	       IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
               ID(20) = 3
               IF (IFINCR.EQ.0) THEN
                  ID(18) = IFHR-ITHEAT
               ELSE
                  ID(18) = IFHR-IFINCR
               ENDIF
	       IF(IFMIN .GE. 1)ID(18)=ID(18)*60
               IF (ID(18).LT.0) ID(18) = 0
               CALL GRIBIT(IGET(079),L,GRID1,IM,JM)
            END IF
           ENDIF

 190     CONTINUE
C
C     END OF MDL SURFACE OUTPUT BLOCK.
C
      ENDIF
C   VISIBILITY
!     IF (IGET(180).GT.0) THEN
!comment out until we get QICE, QSNOW brought into post
!MEB   RDTPHS= 1./(NPHS*DT)
!MEB modifying this Eta-specific code, assuming WRF physics will
!MEB explicitly predict vapor/water/ice/rain/snow
!MEB comments starting with MEB are lines associated with this
!MEB Eta-specific code
C            NEED TO CALCULATE RAIN WATER AND SNOW MIXING RATIOS
!      DO J=JSTA,JEND
!      DO I=1,IM
!MEB     IF (PREC(I,J).EQ.0) THEN
!MEB       QSNO(I,J)=0.
!MEB       QRAIN(I,J)=0.
!MEB     ELSE
!MEB       LLMH=LMH(I,J)
!MEB       SNORATE=SR(I,J)*PREC(I,J)*RDTPHS
!MEB       RAINRATE=(1-SR(I,J))*PREC(I,J)*RDTPHS
!MEB       TERM1=(T(I,J,LM)/PSLP(I,J))**0.4167
!MEB       TERM2=(T(I,J,LLMH)/PMID(I,J,LMH(I,J)))**0.5833
!MEB       TERM3=RAINRATE**0.8333
!MEB       QRAIN(I,J)=RAINCON*TERM1*TERM2*TERM3
!MEB       TERM4=(T(I,J,LM)/PSLP(I,J))**0.47
!MEB       TERM5=(T(I,J,LLMH)/PMID(I,J,LMH(I,J)))**0.53
!MEB       TERM6=SNORATE**0.94
!MEB       QSNO(I,J)=SNOCON*TERM4*TERM5*TERM6
!MEB     ENDIF
!        LLMH=NINT(LMH(I,J))
!        QRAIN1(I,J)=QRAIN(I,J,LLMH)
!        QSNO1(I,J)=QSNOW(I,J,LLMH)
!        TT(I,J)=T(I,J,LLMH)
!        QV(I,J)=Q(I,J,LLMH)
!        QCD(I,J)=CWM(I,J,LLMH)
!        QICE1(I,J)=QICE(I,J,LLMH)
!        PPP(I,J)=PMID(I,J,LLMH)
!      ENDDO
!      ENDDO
!      CALL CALVIS(QV,QCD,QRAIN1,QICE1,QSNO1,TT,PPP,VIS)
!              DO J=JSTA,JEND
!              DO I=1,IM
!                GRID1(I,J)=VIS(I,J)
!              ENDDO
!              ENDDO
!      ID(1:25) = 0
!      CALL GRIBIT(IGET(180),LVLS(1,IGET(180)),
!    X           GRID1,IM,JM)
!      ENDIF
C
C     INSTANTANEOUS CONVECTIVE PRECIPITATION RATE.
C
!      IF (IGET(249).GT.0) THEN
!         RDTPHS=1000./DTQ2
!         DO J=JSTA,JEND
!         DO I=1,IM
!           GRID1(I,J)=CPRATE(I,J)*RDTPHS
!           GRID1(I,J)=SPVAL
!         ENDDO
!         ENDDO
!         ID(1:25) = 0
!	 CALL GRIBIT(IGET(249),LM,GRID1,IM,JM)
!      ENDIF
C
C     COMPOSITE RADAR REFLECTIVITY (maximum dBZ in each column)
C
      IF (IGET(252).GT.0) THEN
         DO J=JSTA,JEND
            DO I=1,IM
               GRID1(I,J)=DBZmin
               DO L=1,NINT(LMH(I,J))
                  GRID1(I,J)=MAX( GRID1(I,J), DBZ(I,J,L) )
               ENDDO
            ENDDO
         ENDDO
         ID(1:25) = 0
	 ID(02)=129
         CALL GRIBIT(IGET(252),LM,GRID1,IM,JM)
      ENDIF
!
!--   COMPOSITE RADAR REFLECTIVITY FROM RAIN (maximum dBZ in each column due to rain)
!
      IF (IGET(276).GT.0) THEN
         DO J=JSTA,JEND
            DO I=1,IM
               GRID1(I,J)=DBZmin
               DO L=1,NINT(LMH(I,J))
                  GRID1(I,J)=MAX( GRID1(I,J), DBZR(I,J,L) )
               ENDDO
            ENDDO
         ENDDO
         ID(1:25) = 0
         ID(02)=129
         CALL GRIBIT(IGET(276),LM,GRID1,IM,JM)
      ENDIF
!
!--   COMPOSITE RADAR REFLECTIVITY FROM ICE
!     (maximum dBZ in each column due to all ice habits; snow + graupel + etc.)
!
      IF (IGET(277).GT.0) THEN
         DO J=JSTA,JEND
            DO I=1,IM
               GRID1(I,J)=DBZmin
               DO L=1,NINT(LMH(I,J))
                  GRID1(I,J)=MAX( GRID1(I,J), DBZI(I,J,L) )
               ENDDO
            ENDDO
         ENDDO
         ID(1:25) = 0
         ID(02)=129
         CALL GRIBIT(IGET(277),LM,GRID1,IM,JM)
      ENDIF
!
!--   COMPOSITE RADAR REFLECTIVITY FROM PARAMETERIZED CONVECTION
!     (maximum dBZ in each column due to parameterized convection, as bogused into
!      post assuming a constant reflectivity from the surface to the 0C level, 
!      and decreasing with height at higher levels)
!
      IF (IGET(278).GT.0) THEN
         DO J=JSTA,JEND
            DO I=1,IM
               GRID1(I,J)=DBZmin
               DO L=1,NINT(LMH(I,J))
                  GRID1(I,J)=MAX( GRID1(I,J), DBZC(I,J,L) )
               ENDDO
            ENDDO
         ENDDO
         ID(1:25) = 0
         ID(02)=129
         CALL GRIBIT(IGET(278),LM,GRID1,IM,JM)
      ENDIF
C
C---   VISIBILITY
C
      IF (IGET(180).GT.0) THEN
        RDTPHS=1./DTQ2
  !
  !--- Needed values at 1st level above ground  (Jin, '01; Ferrier, Feb '02)
  !
        DO J=JSTA,JEND
          DO I=1,IM
            LLMH=NINT(LMH(I,J))
            Q1D(I,J)=Q(I,J,LLMH)
            QW1(I,J)=QQW(I,J,LLMH)
            QR1(I,J)=QQR(I,J,LLMH)
            QI1(I,J)=QQI(I,J,LLMH)
            QS1(I,J)=QQS(I,J,LLMH)
            T1D(I,J)=T(I,J,LLMH)
            P1D(I,J)=PMID(I,J,LLMH)
CHC Because instantanous convective precip rate is not yet available as wrf output,
CHC cuppt is used as a replacement for now  
CHC Only adding convective precipitation rate when using Ferrier's scheme
           IF(imp_physics.eq.5)THEN
            IF (CPRATE(I,J) .GT. 0.) THEN
!            IF (CUPPT(I,J) .GT. 0.) THEN
               RAINRATE=(1-SR(I,J))*CPRATE(I,J)*RDTPHS
!               RAINRATE=(1-SR(I,J))*CUPPT(I,J)/(TRDLW*3600.)
               TERM1=(T(I,J,LM)/PMID(I,J,LM))**0.4167
               TERM2=(T1D(I,J)/P1D(I,J))**0.5833
               TERM3=RAINRATE**0.8333
	       QROLD=1.2*QR1(I,J)
               QR1(I,J)=QR1(I,J)+RAINCON*TERM1*TERM2*TERM3
               IF (SR(I,J) .GT. 0.) THEN
                  SNORATE=SR(I,J)*CPRATE(I,J)*RDTPHS
!                  SNORATE=SR(I,J)*CUPPT(I,J)/(TRDLW*3600.)
                  TERM1=(T(I,J,LM)/PMID(I,J,LM))**0.47
                  TERM2=(T1D(I,J)/P1D(I,J))**0.53
                  TERM3=SNORATE**0.94
                  QS1(I,J)=QS1(I,J)+SNOCON*TERM1*TERM2*TERM3
               ENDIF
            ENDIF
	   END IF 
          ENDDO
        ENDDO
  !
  !-- Visibility using Warner-Stoelinga algorithm  (Jin, '01)
  !
        ii=im/2
        jj=(jsta+jend)/2
!        print*,'Debug: Visbility ',Q1D(ii,jj),QW1(ii,jj),QR1(ii,jj)
!     +,QI1(ii,jj) ,QS1(ii,jj),T1D(ii,jj),P1D(ii,jj)

        CALL CALVIS(Q1D,QW1,QR1,QI1,QS1,T1D,P1D,VIS)
        print*,'Debug: Visbility ',VIS(ii,jj)

!        print*,'Debug: Visbility ',Q1D(ii,jj),QW1(ii,jj),QR1(ii,jj),QI1(ii,jj)
!     +,QS1(ii,jj),T1D(ii,jj),P1D(ii,jj)
	DO J=JSTA,JEND
	DO I=1,IM
	  IF(abs(vis(i,j)).gt.24135.1)print*,'bad visbility'
     + ,i,j,Q1D(i,j),QW1(i,j),QR1(i,j),QI1(i,j)
     +,QS1(i,j),T1D(i,j),P1D(i,j),vis(i,j)	  
	  GRID1(I,J)=VIS(I,J)
	END DO
	END DO  
        ID(1:25) = 0
	CALL GRIBIT(IGET(180),LM,GRID1,IM,JM)
       ENDIF
C
C     
C     ASYMPTOTIC AND FREE ATMOSPHERE MASTER LENGTH SCALE (EL), PLUS
C     GRADIENT RICHARDSON NUMBER.
C
      IF ( (IGET(111).GT.0) .OR. (IGET(146).GT.0) .OR. 
     X     (IGET(147).GT.0) ) THEN
C     
C        COMPUTE ASYMPTOTIC MASTER LENGTH SCALE.
         CALL CLMAX(EL0,EGRID2,EGRID3,EGRID4,EGRID5)
C     
C        IF REQUESTED, POST ASYMPTOTIC MASTER LENGTH SCALE.
         IF (IGET(147).GT.0) THEN
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EL0(I,J)
               ENDDO
               ENDDO
            ID(1:25) = 0
            CALL GRIBIT(IGET(147),LM,GRID1,IM,JM)
         ENDIF
C     
C        IF REQUESTED, POST FREE ATMOSPHERE MASTER LENGTH SCALE
C        AND/OR THE GRADIENT RICHARDSON NUMBER.    
C
         IF ( (IGET(111).GT.0) .OR. (IGET(146).GT.0) ) THEN
C     
C           COMPUTE FREE ATMOSPHERE MASTER LENGTH SCALE.
!$omp  parallel do
            DO L=1,LM
               DO J=JSTA,JEND
               DO I=1,IM
                 EL(I,J,L)=D00
               ENDDO
               ENDDO
            ENDDO

            IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM')THEN
             CALL MIXLEN(EL0,EL)
            ELSE IF(MODELNAME .EQ. 'NMM')THEN
              DO L=1,LM
               DO J=JSTA,JEND
               DO I=1,IM
                 EL(I,J,L)=EL_MYJ(I,J,L)  !NOW EL COMES OUT OF WRF NMM
               ENDDO
               ENDDO
              ENDDO
            END IF
C     
C           COMPUTE GRADIENT RICHARDSON NUMBER IF REQUESTED.
C     
            IF ( (IGET(111).GT.0) ) CALL CALRCH(EL,RICHNO)
C
C           LOOP OVER MDL LAYERS.
            DO 200 L = 1,LM
C     
C              POST MIXING LENGTH.
C
            IF (IGET(146).GT.0) THEN
             IF (LVLS(L,IGET(146)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=EL(I,J,LL)
               ENDDO
               ENDDO
                  ID(1:25) = 0
                  CALL GRIBIT(IGET(146),L,GRID1,IM,JM)
             ENDIF
            ENDIF
C     
C              POST GRADIENT RICHARDSON NUMBER.
C
            IF(L .LT. LM)THEN
             IF (IGET(111).GT.0) THEN
              IF (LVLS(L,IGET(111)).GT.0) THEN
	       LL=LM-L+1
               DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=RICHNO(I,J,LL)
               ENDDO
               ENDDO
               ID(1:25) = 0
               CALL GRIBIT(IGET(111),L,GRID1,IM,JM)
              ENDIF
             ENDIF
            END IF
 200        CONTINUE
C
C
         ENDIF
      ENDIF
C     
C           COMPUTE PBL HEIGHT BASED ON RICHARDSON NUMBER
C     
            IF ( (IGET(289).GT.0) ) CALL CALPBL(PBLRI)

            IF (IGET(289).GT.0) THEN
                DO J=JSTA,JEND
                DO I=1,IM
                     GRID1(I,J)=PBLRI(I,J)
c                     PBLH(I,J)=PBLRI(I,J)
                ENDDO
                ENDDO
                ID(1:25) = 0
!		ID(02)=129
                CALL GRIBIT(IGET(289),LM,GRID1,IM,JM)
            ENDIF

C     
      DEALLOCATE(EL)
      DEALLOCATE(RICHNO)
      DEALLOCATE(PBLRI)
C     
C     END OF ROUTINE.
C     
      RETURN
      END
