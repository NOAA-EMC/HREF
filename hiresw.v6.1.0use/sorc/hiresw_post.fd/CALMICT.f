      SUBROUTINE CALMICT(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL,     &
                        QW1,QI1,QR1,QS1,DBZ1,DBZR1,DBZI1,DBZC1,NLICE1)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALMIC      COMPUTES HYDROMETEORS 
!   PRGRMMR: JIN         ORG: W/NP2      DATE: 01-08-14       
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES THE MIXING RATIOS OF CLOUD WATER,
!     CLOUD ICE, RAIN, AND SNOW.  THE CODE IS BASED ON SUBROUTINES
!     GSMDRIVE & GSMCOLUMN IN THE NMM MODEL. 
!     
! PROGRAM HISTORY LOG:
!   01-08-14  YI JIN 
!   02-02-11  Brad Ferrier - Minor changes for consistency w/ NMM model
!   04-11-10  Brad Ferrier - Removed cloud fraction algorithm
!   04-11-17  H CHUANG - WRF VERSION     
! USAGE:    CALL CALMICT(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL
!     &,                QW1,QI1,QR1,QS1,DBZ1,DBZR1,DBZI1,DBZC1)
!   INPUT ARGUMENT LIST:
!     P1D     - PRESSURE (PA)
!     T1D     - TEMPERATURE (K)
!     Q1D     - SPECIFIC HUMIDITY (KG/KG)
!     C1D     - TOTAL CONDENSATE (CWM, KG/KG)
!     FI1D    - F_ice (fraction of condensate in form of ice)
!     FR1D    - F_rain (fraction of liquid water in form of rain)
!     FS1D    - F_RimeF ("Rime Factor", ratio of total ice growth 
!                       to deposition growth)
!     CUREFL  - Radar reflectivity contribution from convection (mm**6/m**3)
!
!   OUTPUT ARGUMENT LIST:
!     QW1   - CLOUD WATER MIXING RATIO (KG/KG)
!     QI1   - CLOUD ICE MIXING RATIO (KG/KG)
!     QR1   - RAIN MIXING RATIO (KG/KG)
!     QS1   - "SNOW" (precipitation ice) MIXING RATIO (KG/KG)
!     DBZ1  - Equivalent radar reflectivity factor in dBZ; i.e., 10*LOG10(Z)
!     DBZR  - Equivalent radar reflectivity factor from rain in dBZ
!     DBZI  - Equivalent radar reflectivity factor from ice (all forms) in dBZ
!     DBZC  - Equivalent radar reflectivity factor from parameterized convection in dBZ
!
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     FUNCTIONS:
!        FPVS
!     UTILITIES:
!     LIBRARY:
!       NONE
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM SP
!$$$  
!
      use params_mod
      use ctlblk_mod
      use rhgrd_mod
      use cmassi_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      INTEGER INDEXS, INDEXR
      REAL, PARAMETER :: Cice=1.634e13, Cwet=1./.189, Cboth=Cice/.224,   &
     &  NSImax=1000.E3, NLI_min=1.0E3

      real,dimension(IM,JM),intent(in) :: P1D,T1D,Q1D,C1D,FI1D,FR1D,     &
           FS1D,CUREFL
      real,dimension(IM,JM),intent(inout) ::  QW1,QI1,QR1,QS1,DBZ1,DBZR1,&
           DBZI1,DBZC1,NLICE1
      
      REAL N0r,Ztot,Zrain,Zice,Zconv,Zmin,Zmix,NLICE,NSmICE,QSmICE,      &
           NRAIN,Zsmice
      integer I,J
      real TC, Frain,Fice,Flimass,FLARGE,          &
           Fsmall,RimeF,Xsimass,Qice,Qsat,ESAT,WV,RHO,RRHO,RQR,          &
           DRmm,Qsigrd,WVQW,Dum,XLi,Qlice,WC,DLI,xlimass,NLImax
      real,external :: fpvs
!************************************************************************
!--- Determine composition of condensate in the form of cloud water, 
!    total ice (cloud ice & snow), & rain following GSMDRIVE in NMM model
!
      Zmin=10.**(0.1*DBZmin)
      DO J=JSTA,JEND
        DO I=1,IM
          QW1(I,J)=0.
          QI1(I,J)=0.
          QR1(I,J)=0.
          QS1(I,J)=0.
          NLICE1(I,J)=0.
          DBZ1(I,J)=DBZmin
          DBZR1(I,J)=DBZmin
          DBZI1(I,J)=DBZmin
          DBZC1(I,J)=DBZmin
        ENDDO
      ENDDO
      DO J=JSTA,JEND
        DO I=1,IM
          Ztot=0.             !--- Total radar reflectivity
          Zrain=0.            !--- Radar reflectivity from rain
          Zice=0.             !--- Radar reflectivity from ice
          Zsmice=0.           !--- Radar reflectivity from small ice
          Zconv=CUREFL(I,J)   !--- Radar reflectivity from convection
          IF (C1D(I,J) .LE. EPSQ) THEN
!
!--- Skip rest of calculatiions if no condensate is present
!
            GO TO 10
          ELSE
            WC=C1D(I,J)
          ENDIF
!
!--- Code below is from GSMDRIVE for determining:
!    QI1 - total ice (cloud ice & snow) mixing ratio
!    QW1 - cloud water mixing ratio
!    QR1 - rain mixing ratio
!
          TC=T1D(I,J)-TFRZ
          Fice=FI1D(I,J)
          Frain=FR1D(I,J)
          IF (TC.LE.T_ICE .OR. Fice.GE.1.) THEN
!          IF (Fice.GE.1.) THEN  
            QI1(I,J)=WC
          ELSE IF (Fice .LE. 0.) THEN
            QW1(I,J)=WC
          ELSE
            QI1(I,J)=Fice*WC
            QW1(I,J)=WC-QI1(I,J)
          ENDIF   
          IF (QW1(I,J).GT.0. .AND. Frain.GT.0.) THEN
            IF (Frain .GE. 1.) THEN 
              QR1(I,J)=QW1(I,J)
              QW1(I,J)=0.
            ELSE
              QR1(I,J)=Frain*QW1(I,J)
              QW1(I,J)=QW1(I,J)-QR1(I,J)
            ENDIF 
          ENDIF
          WV=Q1D(I,J)/(1.-Q1D(I,J))
!
!--- Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
!
          ESAT=1000.*FPVS(T1D(I,J))
          QSAT=EPS*ESAT/(P1D(I,J)-ESAT)
	  RHO=P1D(I,J)/(RD*T1D(I,J)*(1.+D608*Q1D(I,J)))
          RRHO=1./RHO
  !
  !--- Based on code from GSMCOLUMN in model to determine reflectivity from rain
  !
          IF (QR1(I,J) .GT. EPSQ) THEN
            RQR=RHO*QR1(I,J)
            IF (RQR .LE. RQR_DRmin) THEN
              N0r=MAX(N0rmin, CN0r_DMRmin*RQR)
              INDEXR=MDRmin
            ELSE IF (RQR .GE. RQR_DRmax) THEN
              N0r=CN0r_DMRmax*RQR
              INDEXR=MDRmax
            ELSE
              N0r=N0r0
              INDEXR=MAX( XMRmin, MIN(CN0r0*RQR**.25, XMRmax) )
            ENDIF
  !
  !--- INDEXR is the mean drop size in microns; convert to mm
  !
            DRmm=1.e-3*REAL(INDEXR)
  !
  !--- Number concentration of rain drops (convert INDEXR to m)
  !
            NRAIN=N0r*1.E-6*REAL(INDEXR)
            Zrain=0.72*N0r*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm
          ENDIF        !--- End IF (QR1(I,J) .GT. EPSQ) block
!
!--- Based on code from GSMCOLUMN in model to determine partition of 
!    total ice into cloud ice & snow (precipitation ice)
!
          IF (QI1(I,J) .GT. EPSQ) THEN
            QICE=QI1(I,J)
            RHO=P1D(I,J)/(RD*T1D(I,J)*(1.+ONEPS*Q1D(I,J)))
            RRHO=1./RHO
!
! * FLARGE  - ratio of number of large ice to total (large & small) ice
! * FSMALL  - ratio of number of small ice crystals to large ice particles
!  ->  Small ice particles are assumed to have a mean diameter of 50 microns.
!  * XSIMASS - used for calculating small ice mixing ratio
!  * XLIMASS - used for calculating large ice mixing ratio
!  * INDEXS  - mean size of snow to the nearest micron (units of microns)
!  * RimeF   - Rime Factor, which is the mass ratio of total (unrimed &
!              rimed) ice mass to the unrimed ice mass (>=1)
!  * FLIMASS - mass fraction of large ice
!  * QTICE   - time-averaged mixing ratio of total ice
!  * QLICE   - time-averaged mixing ratio of large ice
!  * NLICE1   - time-averaged number concentration of large ice
!
            IF (TC>=0.) THEN
   !
   !--- Eliminate small ice particle contributions for melting & sublimation
   !
              NSmICE=0.
              QSmICE=0.
            ELSE
   !
   !--- Maximum possible number concentration of small ice crystals
   !
              NSmICE=MIN(0.01*EXP(-0.6*TC), NSImax)       !- Fletcher (1962)
              DUM=RRHO*MASSI(MDImin)
              NSmICE=MIN(NSmICE, QICE/DUM)
              QSmICE=NSmICE*DUM
            ENDIF            ! End IF (TC>=0.) THEN
            QLICE=MAX(0., QICE-QSmICE)
            QS1(I,J)=QLICE
            QI1(I,J)=QSmICE
            DUM=RHO*QLICE
            IF (DUM <= QLImax1) THEN
               NLImax=NLImax1
            ELSE IF (DUM >= QLImax2) THEN
               NLImax=NLImax2
            ELSE
               NLImax=NLImax1*EXP(CLImax*(DUM-QLImax1))
            ENDIF
            DUM=XMImax*EXP(.0536*TC)
            INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
            RimeF=AMAX1(1., FS1D(I,J) )
            NLICE=RHO*QLICE/(RimeF*MASSI(INDEXS))
            DUM=RRHO*NLI_min*MASSI(MDImin)     !-- Minimum large ice mixing ratio
            IF (QLICE<=DUM) THEN
              NLICE=QLICE/DUM
            ELSE IF (NLICE<NLI_min .OR. NLICE>NLImax) THEN
!
!--- Force NLICE to be between NLI_min and NLImax
!
              DUM=RHO*QLICE
              NLICE=MAX(NLI_min, MIN(NLImax, NLICE) )
              XLI=DUM/(NLICE*RimeF)
              IF (XLI .LE. MASSI(MDImin) ) THEN
                INDEXS=MDImin
              ELSE IF (XLI .LE. MASSI(450) ) THEN
                DLI=9.5885E5*XLI**.42066         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE IF (XLI .LE. MASSI(MDImax) ) THEN
                DLI=3.9751E6*XLI**.49870         ! DLI in microns
                INDEXS=MIN(MDImax, MAX(MDImin, INT(DLI) ) )
              ELSE 
                INDEXS=MDImax
!
!--- 8/22/01: Increase density of large ice if maximum limits
!    are reached for number concentration (NLImax) and mean size
!    (MDImax).  Done to increase fall out of ice.
!
                IF (NLICE>=NLImax) RimeF=DUM/(NLImax*MASSI(INDEXS))
              ENDIF             ! End IF (XLI .LE. MASSI(MDImin) )
              NLICE=DUM/(RimeF*MASSI(INDEXS))
            ENDIF               ! End IF (QLICE<=DUM) THEN
            NLICE1(I,J)=NLICE
   !
   !--- Equation (C.8) in Ferrier (1994, JAS, p. 272), which when
   !    converted from cgs units to mks units results in the same
   !    value for Cice, which is equal to the {} term below:
   !
   !    Zi={.224*720*(10**18)/[(PI*RHOL)**2]}*(RHO*QLICE)**2/NLICE1(I,J),
   !    where RHOL=1000 kg/m**3 is the density of liquid water
   !
   !--- Valid only for exponential ice distributions
   !
            IF (NSmICE > 0.) THEN
               Zsmice=Cice*RHO*RHO*QSmICE*QSmICE/NSmICE
            ENDIF
            Zice=Cice*RHO*RHO*QLICE*QLICE/NLICE1(I,J) 
            IF (TC>=0.) Zice=Cwet*Zice      ! increased for wet ice
          ENDIF                 ! End IF (QI1(I,J) .GT. 0.) THEN
   !
   !--- Experimental:  Zmix is the simple estimate of the radar return 
   !    at >0C by combining contributions from rain and melting ice. 
   !    If it is larger than the individual contributions, then classify
   !    the extra "boosted" radar return into the rain category.  
   !
!aligo          IF (TC>0. .AND. Zrain>0. .AND. Zice>0.) THEN
!aligo          IF (Zrain>0. .AND. Zice>0.) THEN
            IF (QR1(I,J)>0.1E-3 .AND. QLICE>0.1E-3) THEN
              NRAIN=MAX(NRAIN,NLICE)
              DUM=RHO*(QR1(I,J)+QLICE)
              Zmix=Cboth*DUM*DUM/NRAIN
              IF (Zmix > Zrain+Zice) THEN
                 Zrain=Zmix-Zice
              ENDIF
            ENDIF
!aligo          ENDIF
!
!---  Calculate total (convective + grid-scale) radar reflectivity
10        Zice=Zice+Zsmice
          Ztot=Zrain+Zice+Zconv
          IF (Ztot .GT. Zmin)  DBZ1(I,J)= 10.*ALOG10(Ztot)
          IF (Zrain .GT. Zmin) DBZR1(I,J)=10.*ALOG10(Zrain)
          IF (Zice .GT. Zmin)  DBZI1(I,J)=10.*ALOG10(Zice)
          IF (Zconv .GT. Zmin) DBZC1(I,J)=10.*ALOG10(Zsmice)
!          IF (Zconv .GT. Zmin) DBZC1(I,J)=10.*ALOG10(Zconv)
        ENDDO
      ENDDO
!
      RETURN
      END
