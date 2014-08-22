!   (1) NLImax=20.e3 rather than 50.e3
!   (2) Improve algorithm for defining NLICE, INDEXS, and RimeF1
!   (3) Added code blocks for enhanced warning diagnostics at "!-- Debug 20120111"
!-----------------------------------------------------------------------------
!
     MODULE MODULE_MP_FER_HIRES
!
!-----------------------------------------------------------------------------
     USE MODULE_INCLUDE
!-----------------------------------------------------------------------------
      PUBLIC :: FERRIER_INIT_HR, GPVS_HR,FPVS,FPVS0,NX
!-----------------------------------------------------------------------------
      REAL,PRIVATE,SAVE ::  ABFR, CBFR, CIACW, CIACR, C_N0r0,            &
     &  CRACW, ARAUT, BRAUT, ESW0, RFmax,                                &
     &  RR_DRmin, RR_DR1, RR_DR2, RR_DR3, RR_DR4, RR_DR5, RR_DRmax
!
      REAL,PUBLIC,SAVE ::  CN0r0, CN0r_DMRmin, CN0r_DMRmax,             &
                           RQR_DRmax, RQR_DRmin
!
      INTEGER, PRIVATE,PARAMETER :: MY_T1=1, MY_T2=35
      REAL,PRIVATE,DIMENSION(MY_T1:MY_T2),SAVE :: MY_GROWTH_NMM
!
      REAL, PRIVATE,PARAMETER :: DMImin=.05e-3, DMImax=1.e-3,            &
     &      DelDMI=1.e-6,XMImin=1.e6*DMImin
      INTEGER, PUBLIC,PARAMETER :: XMImax=1.e6*DMImax, XMIexp=.0536,     &
     &                             MDImin=XMImin, MDImax=XMImax
      REAL, PRIVATE,DIMENSION(MDImin:MDImax) ::                          &
     &      ACCRI,VSNOWI,VENTI1,VENTI2
      REAL, PUBLIC,DIMENSION(MDImin:MDImax) :: SDENS    !-- For RRTM
!
      REAL, PRIVATE,PARAMETER :: DMRmin=.05e-3, DMRmax=1.0e-3,           &
     &      DelDMR=1.e-6,XMRmin=1.e6*DMRmin, XMRmax=1.e6*DMRmax
      INTEGER, PRIVATE,PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax
      REAL, PRIVATE,DIMENSION(MDRmin:MDRmax)::                           &
     &      ACCRR,MASSR,RRATE,VRAIN,VENTR1,VENTR2
!
      INTEGER, PRIVATE,PARAMETER :: Nrime=40
      REAL, DIMENSION(2:9,0:Nrime),PRIVATE,SAVE :: VEL_RF
!
      INTEGER,PARAMETER :: NX=7501
      REAL, PARAMETER :: XMIN=180.0,XMAX=330.0
      REAL, DIMENSION(NX),PRIVATE,SAVE :: TBPVS,TBPVS0
      REAL, PRIVATE,SAVE :: C1XPVS0,C2XPVS0,C1XPVS,C2XPVS
!
      REAL, PRIVATE,PARAMETER ::                                        &
!--- Physical constants follow:
     &   CP=1004.6, EPSQ=1.E-12, GRAV=9.806, RHOL=1000., RD=287.04      &
     &  ,RV=461.5, T0C=273.15, XLS=2.834E6                              &
!--- Derived physical constants follow:
     &  ,EPS=RD/RV, EPS1=RV/RD-1., EPSQ1=1.001*EPSQ                     &
     &  ,RCP=1./CP, RCPRV=RCP/RV, RGRAV=1./GRAV, RRHOL=1./RHOL          &
     &  ,XLS1=XLS*RCP, XLS2=XLS*XLS*RCPRV, XLS3=XLS*XLS/RV              &
!--- Constants specific to the parameterization follow:
!--- CLIMIT/CLIMIT1 are lower limits for treating accumulated precipitation
     &  ,CLIMIT=10.*EPSQ, CLIMIT1=-CLIMIT                               &
     &  ,C1=1./3.                                                       &
     &  ,DMR1=.1E-3, DMR2=.2E-3, DMR3=.32E-3, DMR4=0.45E-3              &
     &  ,DMR5=0.67E-3                                                   &
     &  ,XMR1=1.e6*DMR1, XMR2=1.e6*DMR2, XMR3=1.e6*DMR3                 &
     &  ,XMR4=1.e6*DMR4, XMR5=1.e6*DMR5
      INTEGER, PARAMETER :: MDR1=XMR1, MDR2=XMR2, MDR3=XMR3, MDR4=XMR4  &
     &  , MDR5=XMR5

!-- Debug 20120111
LOGICAL, SAVE :: WARN1=.TRUE.,WARN2=.TRUE.,WARN3=.TRUE.,WARN5=.TRUE.
REAL, SAVE :: Pwarn=75.E2, QTwarn=1.E-3
INTEGER, PARAMETER :: MAX_ITERATIONS=10

!
! ======================================================================
!--- Important tunable parameters that are exported to other modules
!  * RHgrd - threshold relative humidity for onset of condensation
!  * T_ICE - temperature (C) threshold at which all remaining liquid water
!            is glaciated to ice
!  * T_ICE_init - maximum temperature (C) at which ice nucleation occurs
!  * NLImax - maximum number concentrations (m**-3) of large ice (snow/graupel/sleet)
!  * NLImin - minimum number concentrations (m**-3) of large ice (snow/graupel/sleet)
!  * N0r0 - assumed intercept (m**-4) of rain drops if drop diameters are between 0.2 and 1.0 mm
!  * N0rmin - minimum intercept (m**-4) for rain drops
!  * NCW - number concentrations of cloud droplets (m**-3)
!  * FLARGE1, FLARGE2 - number fraction of large ice to total (large+snow) ice
!          at T>0C and in presence of sublimation (FLARGE1), otherwise in
!          presence of ice saturated/supersaturated conditions
! ======================================================================
      REAL, PUBLIC,PARAMETER ::                                         &
     &  RHgrd=1.00                                                      &
     & ,T_ICE=-40.                                                      &
     & ,T_ICEK=T0C+T_ICE                                                &
     & ,T_ICE_init=0.                                                   &
     & ,NLImax=10.E3                                                    &
     & ,NLImin=1.E3                                                     &
     & ,N0r0=8.E6                                                       &
     & ,N0rmin=1.E4                                                     &
     & ,NCW=200.E6                           & !- previously 500.e6, originally 100.e6
     & ,FLARGE1=1.                                                      &
     & ,FLARGE2=0.07  ! from Dr. Nakagawa's sensitivity tests
!--- Other public variables passed to other routines:
      REAL,PUBLIC,SAVE ::  QAUT0
      REAL, PUBLIC,DIMENSION(MDImin:MDImax) :: MASSI
!

     CONTAINS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
      SUBROUTINE FER_HIRES (itimestep,DT,DX,DY,                         &
     &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,qv,qt,     &
     &                      LOWLYR,SR,                                  &
     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           &
     &                      QC,QR,QS,                                   &
     &                      mp_restart_state,tbpvs_state,tbpvs0_state,  &
     &                      RAINNC,RAINNCV,                             &
     &                      ids,ide, jds,jde, kds,kde,		        &
     &                      ims,ime, jms,jme, kms,kme,		        &
                            its,ite, jts,jte, kts,kte,d_ss,mprates)
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER, PARAMETER :: ITLO=-60, ITHI=40

      INTEGER,INTENT(IN) :: D_SS,IDS,IDE,JDS,JDE,KDS,KDE                &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,ITIMESTEP
      REAL, INTENT(IN) 	    :: DT,DX,DY
      REAL, INTENT(IN),     DIMENSION(ims:ime, jms:jme, kms:kme)::      &
     &                      dz8w,p_phy,pi_phy,rho_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, jms:jme, kms:kme)::      &
     &                      th_phy,qv,qt
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme, kms:kme ) ::    &
     &                      qc,qr,qs
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, jms:jme,kms:kme) ::    &
     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme)           ::     &
     &                                                   RAINNC,RAINNCV
      REAL,               DIMENSION(ims:ime, jms:jme,kms:kme,d_ss) ::  &
     &                     mprates 
      REAL, INTENT(OUT),    DIMENSION(ims:ime,jms:jme):: SR
!
      REAL,DIMENSION(*),INTENT(INOUT) :: MP_RESTART_STATE
!
      REAL,DIMENSION(nx),INTENT(INOUT) :: TBPVS_STATE,TBPVS0_STATE
!
      INTEGER, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) :: LOWLYR

!-----------------------------------------------------------------------
!     LOCAL VARS
!-----------------------------------------------------------------------

!     NSTATS,QMAX,QTOT are diagnostic vars

      INTEGER,DIMENSION(ITLO:ITHI,4) :: NSTATS
      REAL,   DIMENSION(ITLO:ITHI,5) :: QMAX
      REAL,   DIMENSION(ITLO:ITHI,22):: QTOT

!     SOME VARS WILL BE USED FOR DATA ASSIMILATION (DON'T NEED THEM NOW). 
!     THEY ARE TREATED AS LOCAL VARS, BUT WILL BECOME STATE VARS IN THE 
!     FUTURE. SO, WE DECLARED THEM AS MEMORY SIZES FOR THE FUTURE USE

!     TLATGS_PHY,TRAIN_PHY,APREC,PREC,ACPREC,SR are not directly related 
!     the microphysics scheme. Instead, they will be used by Eta precip 
!     assimilation.

      REAL,  DIMENSION( ims:ime, jms:jme,kms:kme ) ::                  &
     &       TLATGS_PHY,TRAIN_PHY
      REAL,  DIMENSION(ims:ime,jms:jme):: APREC,PREC,ACPREC
      REAL,  DIMENSION(its:ite, jts:jte, kts:kte):: t_phy

      INTEGER :: I,J,K,KK
      REAL :: wc
!------------------------------------------------------------------------
! For ECGP01
!-----------------------------------------------------------------------
      LOGICAL, PARAMETER :: PRINT_diag=.FALSE.
      INTEGER :: LSFC,I_index,J_index,L
      INTEGER,DIMENSION(its:ite,jts:jte) :: LMH
      REAL :: TC,QI,QRdum,QW,Fice,Frain,DUM,ASNOW,ARAIN
      REAL,DIMENSION(kts:kte) :: P_col,Q_col,T_col,QV_col,WC_col,       &
         RimeF_col,QI_col,QR_col,QW_col, THICK_col,DPCOL,pcond1d,       &
         pidep1d,piacw1d,piacwi1d,piacwr1d,piacr1d,picnd1d,pievp1d,     &
         pimlt1d,praut1d,pracw1d,prevp1d,pisub1d,pevap1d
      REAL,DIMENSION(2) :: PRECtot,PRECmax
!
!-----------------------------------------------------------------------
!**********************************************************************
!-----------------------------------------------------------------------
!
      MY_GROWTH_NMM(MY_T1:MY_T2)=MP_RESTART_STATE(MY_T1:MY_T2)
!
      C1XPVS0=MP_RESTART_STATE(MY_T2+1)
      C2XPVS0=MP_RESTART_STATE(MY_T2+2)
      C1XPVS =MP_RESTART_STATE(MY_T2+3)
      C2XPVS =MP_RESTART_STATE(MY_T2+4)
      CIACW  =MP_RESTART_STATE(MY_T2+5)
      CIACR  =MP_RESTART_STATE(MY_T2+6)
      CRACW  =MP_RESTART_STATE(MY_T2+7)
      BRAUT  =MP_RESTART_STATE(MY_T2+8)
!
      TBPVS(1:NX) =TBPVS_STATE(1:NX)
      TBPVS0(1:NX)=TBPVS0_STATE(1:NX)
!
!     write(0,*)' in ETAMP_NEW its=',its,' ite=',ite,' jts=',jts,' jte=',jte,' kts=',kts,' kte=',kte
!     write(0,*)' ims=',ims,' ime=',ime,' jms=',jms,' jme=',jme,' kms=',kms,' kme=',kme
!.......................................................................
!$omp parallel do private(j,k,i)
!.......................................................................
      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
        t_phy(i,j,k) = th_phy(i,j,k)*pi_phy(i,j,k)
        qv(i,j,k)=qv(i,j,k)/(1.+qv(i,j,k)) !Convert to specific humidity
!     if(i==101.and.j==163)then
!       write(0,*)' enter etamp_new k=',k,' t=',t_phy(i,k,j),' th=',th_phy(i,k,j),' pii=',pi_phy(i,k,j)
!     endif
      ENDDO
      ENDDO
      ENDDO
!.......................................................................
!$omp end parallel do
!.......................................................................

!     initial diagnostic variables and data assimilation vars
!     (will need to delete this part in the future)

      DO k = 1,4
      DO i = ITLO,ITHI
         NSTATS(i,k)=0. 
      ENDDO
      ENDDO

      DO k = 1,5
      DO i = ITLO,ITHI
         QMAX(i,k)=0.
      ENDDO
      ENDDO

      DO k = 1,22
      DO i = ITLO,ITHI
         QTOT(i,k)=0.
      ENDDO
      ENDDO

! initial data assimilation vars (will need to delete this part in the future)

!.......................................................................
!$omp parallel do private(j,k,i)
!.......................................................................
      DO j = jts,jte
       DO i = its,ite
         ACPREC(i,j)=0.
         APREC (i,j)=0.
         PREC  (i,j)=0.
         SR    (i,j)=0.
       ENDDO
       DO k = kts,kte
       DO i = its,ite
	 TLATGS_PHY (i,j,k)=0.
	 TRAIN_PHY  (i,j,k)=0.
       ENDDO
       ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!-- Start of original driver for EGCP01COLUMN_hr
!-----------------------------------------------------------------------
!
!.......................................................................
!$omp end parallel do
!.......................................................................
!$omp parallel do                                                       &
!$omp private (j,i,k,lsfc,dpcol,l,p_col,thick_col,t_col,tc,qv_col,      &
!$omp          wc_col,wc,qi,QRdum,qw,fice,frain,rimef_col,qi_col,qr_col,   &
!$omp          qw_col,i_index,j_index,arain,asnow,dum,prectot,precmax,  &
!$omp          qmax,qtot,nstats),SCHEDULE(dynamic)  
!.......................................................................
       DO J=JTS,JTE    
        DO I=ITS,ITE  
          LSFC=KTE-LOWLYR(I,J)+1                      ! "L" of surface
          DO K=KTS,KTE
            DPCOL(K)=RHO_PHY(I,J,K)*GRAV*dz8w(I,J,K)
          ENDDO
!
!--- Initialize column data (1D arrays)
!
        L=1
!-- qt = CWM, total condensate
        IF (qt(I,J,L) .LE. EPSQ) qt(I,J,L)=EPSQ
          F_ice_phy(I,J,L)=1.
          F_rain_phy(I,J,L)=0.
          F_RimeF_phy(I,J,L)=1.
          DO L=1,LSFC
!
!--- Pressure (Pa) = (Psfc-Ptop)*(ETA/ETA_sfc)+Ptop
!
            P_col(L)=P_phy(I,J,L)
!
!--- Layer thickness = RHO*DZ = -DP/G = (Psfc-Ptop)*D_ETA/(G*ETA_sfc)
!
            THICK_col(L)=DPCOL(L)*RGRAV
            T_col(L)=T_phy(I,J,L)
            TC=T_col(L)-T0C
            QV_col(L)=max(EPSQ, qv(I,J,L))
            IF (qt(I,J,L) .LE. EPSQ1) THEN
              WC_col(L)=0.
              IF (TC .LT. T_ICE) THEN
                F_ice_phy(I,J,L)=1.
              ELSE
                F_ice_phy(I,J,L)=0.
              ENDIF
              F_rain_phy(I,J,L)=0.
              F_RimeF_phy(I,J,L)=1.
            ELSE
              WC_col(L)=qt(I,J,L)

!-- Debug 20120111
!   TC==TC will fail if NaN, preventing unnecessary error messages
IF (WC_col(L)>QTwarn .AND. P_col(L)<Pwarn .AND. TC==TC) THEN
   WRITE(0,*) 'WARN4: >1 g/kg condensate in stratosphere; I,J,L,TC,P,QT=',   &
              I,J,L,TC,.01*P_col(L),1000.*WC_col(L)
   QTwarn=MAX(WC_col(L),10.*QTwarn)
   Pwarn=MIN(P_col(L),0.5*Pwarn)
ENDIF
!-- TC/=TC will pass if TC is NaN
IF (WARN5 .AND. TC/=TC) THEN
   WRITE(0,*) 'WARN5: NaN temperature; I,J,L,P=',I,J,L,.01*P_col(L)
   WARN5=.FALSE.
ENDIF

            ENDIF
!     !
!     !--- Determine composition of condensate in terms of 
!     !      cloud water, ice, & rain
!     !
            WC=WC_col(L)
            QI=0.
            QRdum=0.
            QW=0.
            Fice=F_ice_phy(I,J,L)
            Frain=F_rain_phy(I,J,L)
!
            IF (Fice .GE. 1.) THEN
              QI=WC
            ELSE IF (Fice .LE. 0.) THEN
              QW=WC
            ELSE
              QI=Fice*WC
              QW=WC-QI
            ENDIF
!
            IF (QW.GT.0. .AND. Frain.GT.0.) THEN
              IF (Frain .GE. 1.) THEN
                QRdum=QW
                QW=0.
              ELSE
                QRdum=Frain*QW
                QW=QW-QRdum
              ENDIF
            ENDIF
            IF (QI .LE. 0.) F_RimeF_phy(I,J,L)=1.
            RimeF_col(L)=F_RimeF_phy(I,J,L)               ! (real)
            QI_col(L)=QI
            QR_col(L)=QRdum
            QW_col(L)=QW
          ENDDO
!
!#######################################################################
!
!--- Perform the microphysical calculations in this column
!
          I_index=I
          J_index=J
       CALL EGCP01COLUMN_hr ( ARAIN, ASNOW, DT, I_index, J_index, LSFC, &
     & P_col, QI_col, QR_col, QV_col, QW_col, RimeF_col, T_col,         &
     & THICK_col, WC_col,KTS,KTE,NSTATS,QMAX,QTOT,pcond1d,pidep1d,      &
     & piacw1d,piacwi1d,piacwr1d,piacr1d,picnd1d,pievp1d,pimlt1d,       &
     & praut1d,pracw1d,prevp1d,pisub1d,pevap1d)
!#######################################################################
!
!--- Update storage arrays
!
          DO L=1,LSFC
            TRAIN_phy(I,J,L)=(T_col(L)-T_phy(I,J,L))/DT
            TLATGS_phy(I,J,L)=T_col(L)-T_phy(I,J,L)
            T_phy(I,J,L)=T_col(L)
            qv(I,J,L)=QV_col(L)
            qt(I,J,L)=WC_col(L)
!---convert 1D source/sink terms to one 4D array
!---d_ss is the total number of source/sink terms in the 4D mprates array
!---if d_ss=1, only 1 source/sink term is used
!
       IF(D_SS.EQ.1)THEN
           mprates(I,J,L,1)=0.
       ELSE
           mprates(I,J,L,1)=mprates(I,J,L,1)+pcond1d(L)
           mprates(I,J,L,2)=mprates(I,J,L,2)+pidep1d(L)
           mprates(I,J,L,3)=mprates(I,J,L,3)+piacw1d(L)
           mprates(I,J,L,4)=mprates(I,J,L,4)+piacwi1d(L)
           mprates(I,J,L,5)=mprates(I,J,L,5)+piacwr1d(L)
           mprates(I,J,L,6)=mprates(I,J,L,6)+piacr1d(L)
           mprates(I,J,L,7)=mprates(I,J,L,7)+picnd1d(L)
           mprates(I,J,L,8)=mprates(I,J,L,8)+pievp1d(L)
           mprates(I,J,L,9)=mprates(I,J,L,9)+pimlt1d(L)
           mprates(I,J,L,10)=mprates(I,J,L,10)+praut1d(L)
           mprates(I,J,L,11)=mprates(I,J,L,11)+pracw1d(L)
           mprates(I,J,L,12)=mprates(I,J,L,12)+prevp1d(L)
           mprates(I,J,L,13)=mprates(I,J,L,13)+pisub1d(L)
           mprates(I,J,L,14)=mprates(I,J,L,14)+pevap1d(L)
        ENDIF
!
!--- REAL*4 array storage
!
            IF (QI_col(L) .LE. EPSQ) THEN
              F_ice_phy(I,J,L)=0.
              IF (T_col(L) .LT. T_ICEK) F_ice_phy(I,J,L)=1.
              F_RimeF_phy(I,J,L)=1.
            ELSE
              F_ice_phy(I,J,L)=MAX( 0., MIN(1., QI_col(L)/WC_col(L)) )
              F_RimeF_phy(I,J,L)=MAX(1., RimeF_col(L))
            ENDIF
            IF (QR_col(L) .LE. EPSQ) THEN
              DUM=0
            ELSE
              DUM=QR_col(L)/(QR_col(L)+QW_col(L))
            ENDIF
            F_rain_phy(I,J,L)=DUM
          ENDDO
!
!--- Update accumulated precipitation statistics
!
!--- Surface precipitation statistics; SR is fraction of surface 
!    precipitation (if >0) associated with snow
!
        APREC(I,J)=(ARAIN+ASNOW)*RRHOL       ! Accumulated surface precip (depth in m)  !<--- Ying
        PREC(I,J)=PREC(I,J)+APREC(I,J)
        ACPREC(I,J)=ACPREC(I,J)+APREC(I,J)
        IF(APREC(I,J) .LT. 1.E-8) THEN
          SR(I,J)=0.
        ELSE
          SR(I,J)=RRHOL*ASNOW/APREC(I,J)
        ENDIF
!
!--- Debug statistics 
!
        IF (PRINT_diag) THEN
          PRECtot(1)=PRECtot(1)+ARAIN
          PRECtot(2)=PRECtot(2)+ASNOW
          PRECmax(1)=MAX(PRECmax(1), ARAIN)
          PRECmax(2)=MAX(PRECmax(2), ASNOW)
        ENDIF
!
!#######################################################################
!#######################################################################
!
    enddo                          ! End "I" loop
    enddo                          ! End "J" loop
!.......................................................................
!$omp end parallel do
!.......................................................................
!
!-----------------------------------------------------------------------
!-- End of original driver for EGCP01COLUMN_hr
!-----------------------------------------------------------------------
!
!.......................................................................
!$omp parallel do private(j,k,i,wc)
!.......................................................................
     DO j = jts,jte
        DO k = kts,kte
	DO i = its,ite
           th_phy(i,j,k) = t_phy(i,j,k)/pi_phy(i,j,k)
           qv(i,j,k)=qv(i,j,k)/(1.-qv(i,j,k))  !Convert to mixing ratio
           WC=qt(I,J,K)
           QS(I,J,K)=0.
           QR(I,J,K)=0.
           QC(I,J,K)=0.
!
           IF(F_ICE_PHY(I,J,K)>=1.)THEN
             QS(I,J,K)=WC
           ELSEIF(F_ICE_PHY(I,J,K)<=0.)THEN
             QC(I,J,K)=WC
           ELSE
             QS(I,J,K)=F_ICE_PHY(I,J,K)*WC
             QC(I,J,K)=WC-QS(I,J,K)
           ENDIF
!
           IF(QC(I,J,K)>0..AND.F_RAIN_PHY(I,J,K)>0.)THEN
             IF(F_RAIN_PHY(I,J,K).GE.1.)THEN
               QR(I,J,K)=QC(I,J,K)
               QC(I,J,K)=0.
             ELSE
               QR(I,J,K)=F_RAIN_PHY(I,J,K)*QC(I,J,K)
               QC(I,J,K)=QC(I,J,K)-QR(I,J,K)
             ENDIF
           ENDIF
!
          ENDDO   !- i
        ENDDO     !- k
     ENDDO        !- j
!.......................................................................
!$omp end parallel do
!.......................................................................
! 
!- Update rain (convert from m to kg/m**2, which is also equivalent to mm depth)
! 
       DO j=jts,jte
       DO i=its,ite
          RAINNC(i,j)=APREC(i,j)*1000.+RAINNC(i,j)
          RAINNCV(i,j)=APREC(i,j)*1000.
       ENDDO
       ENDDO
!
     MP_RESTART_STATE(MY_T1:MY_T2)=MY_GROWTH_NMM(MY_T1:MY_T2)
     MP_RESTART_STATE(MY_T2+1)=C1XPVS0
     MP_RESTART_STATE(MY_T2+2)=C2XPVS0
     MP_RESTART_STATE(MY_T2+3)=C1XPVS
     MP_RESTART_STATE(MY_T2+4)=C2XPVS
     MP_RESTART_STATE(MY_T2+5)=CIACW
     MP_RESTART_STATE(MY_T2+6)=CIACR
     MP_RESTART_STATE(MY_T2+7)=CRACW
     MP_RESTART_STATE(MY_T2+8)=BRAUT
!
     TBPVS_STATE(1:NX) =TBPVS(1:NX)
     TBPVS0_STATE(1:NX)=TBPVS0(1:NX)
!
!-----------------------------------------------------------------------
!
  END SUBROUTINE FER_HIRES
!
!-----------------------------------------------------------------------
!
!###############################################################################
! ***** VERSION OF MICROPHYSICS DESIGNED FOR HIGHER RESOLUTION MESO ETA MODEL
!       (1) Represents sedimentation by preserving a portion of the precipitation
!           through top-down integration from cloud-top.  Modified procedure to
!           Zhao and Carr (1997).
!       (2) Microphysical equations are modified to be less sensitive to time
!           steps by use of Clausius-Clapeyron equation to account for changes in
!           saturation mixing ratios in response to latent heating/cooling.  
!       (3) Prevent spurious temperature oscillations across 0C due to 
!           microphysics.
!       (4) Uses lookup tables for: calculating two different ventilation 
!           coefficients in condensation and deposition processes; accretion of
!           cloud water by precipitation; precipitation mass; precipitation rate
!           (and mass-weighted precipitation fall speeds).
!       (5) Assumes temperature-dependent variation in mean diameter of large ice
!           (Houze et al., 1979; Ryan et al., 1996).
!        -> 8/22/01: This relationship has been extended to colder temperatures
!           to parameterize smaller large-ice particles down to mean sizes of MDImin,
!           which is 50 microns reached at -55.9C.
!       (6) Attempts to differentiate growth of large and small ice, mainly for
!           improved transition from thin cirrus to thick, precipitating ice
!           anvils.
!       (7) Top-down integration also attempts to treat mixed-phase processes,
!           allowing a mixture of ice and water.  Based on numerous observational
!           studies, ice growth is based on nucleation at cloud top &
!           subsequent growth by vapor deposition and riming as the ice particles 
!           fall through the cloud.  There are two modes of ice nucleation
!           following Meyers et al. (JAM, 1992):
!            a) Deposition & condensation freezing nucleation - eq. (2.4) when
!               air is supersaturated w/r/t ice
!            b) Contact freezing nucleation - eq. (2.6) in presence of cloud water
!       (8) Depositional growth of newly nucleated ice is calculated for large time
!           steps using Fig. 8 of Miller and Young (JAS, 1979), at 1 deg intervals
!           using their ice crystal masses calculated after 600 s of growth in water
!           saturated conditions.  The growth rates are normalized by time step
!           assuming 3D growth with time**1.5 following eq. (6.3) in Young (1993).
!       (9) Ice precipitation rates can increase due to increase in response to
!           cloud water riming due to (a) increased density & mass of the rimed
!           ice, and (b) increased fall speeds of rimed ice.
!###############################################################################
!###############################################################################
!
      SUBROUTINE EGCP01COLUMN_hr ( ARAIN, ASNOW, DTPH, I_index, J_index, &
     & LSFC, P_col, QI_col, QR_col, QV_col, QW_col, RimeF_col, T_col,    &
     & THICK_col, WC_col ,KTS,KTE,NSTATS,QMAX,QTOT,pcond1d,pidep1d,      &
     & piacw1d,piacwi1d,piacwr1d,piacr1d,picnd1d,pievp1d,pimlt1d,        &
     & praut1d,pracw1d,prevp1d,pisub1d,pevap1d)                          
!
!###############################################################################
!###############################################################################
!
!-------------------------------------------------------------------------------
!----- NOTE:  Code is currently set up w/o threading!  
!-------------------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:  Grid-scale microphysical processes - condensation & precipitation
!   PRGRMMR: Ferrier         ORG: W/NP22     DATE: 08-2001
!   PRGRMMR: Jin  (Modification for WRF structure)
!-------------------------------------------------------------------------------
! ABSTRACT:
!   * Merges original GSCOND & PRECPD subroutines.   
!   * Code has been substantially streamlined and restructured.
!   * Exchange between water vapor & small cloud condensate is calculated using
!     the original Asai (1965, J. Japan) algorithm.  See also references to
!     Yau and Austin (1979, JAS), Rutledge and Hobbs (1983, JAS), and Tao et al.
!     (1989, MWR).  This algorithm replaces the Sundqvist et al. (1989, MWR)
!     parameterization.  
!-------------------------------------------------------------------------------
!     
! USAGE: 
!   * CALL EGCP01COLUMN_hr FROM SUBROUTINE EGCP01DRV
!
! INPUT ARGUMENT LIST:
!   DTPH       - physics time step (s)
!   I_index    - I index
!   J_index    - J index
!   LSFC       - Eta level of level above surface, ground
!   P_col      - vertical column of model pressure (Pa)
!   QI_col     - vertical column of model ice mixing ratio (kg/kg)
!   QR_col     - vertical column of model rain ratio (kg/kg)
!   QV_col     - vertical column of model water vapor specific humidity (kg/kg)
!   QW_col     - vertical column of model cloud water mixing ratio (kg/kg)
!   RimeF_col  - vertical column of rime factor for ice in model (ratio, defined below)
!   T_col      - vertical column of model temperature (deg K)
!   THICK_col  - vertical column of model mass thickness (density*height increment)
!   WC_col     - vertical column of model mixing ratio of total condensate (kg/kg)
!   
!
! OUTPUT ARGUMENT LIST: 
!   ARAIN      - accumulated rainfall at the surface (kg)
!   ASNOW      - accumulated snowfall at the surface (kg)
!   QV_col     - vertical column of model water vapor specific humidity (kg/kg)
!   WC_col     - vertical column of model mixing ratio of total condensate (kg/kg)
!   QW_col     - vertical column of model cloud water mixing ratio (kg/kg)
!   QI_col     - vertical column of model ice mixing ratio (kg/kg)
!   QR_col     - vertical column of model rain ratio (kg/kg)
!   RimeF_col  - vertical column of rime factor for ice in model (ratio, defined below)
!   T_col      - vertical column of model temperature (deg K)
!     
! OUTPUT FILES:
!     NONE
!     
! Subprograms & Functions called:
!   * Real Function CONDENSE  - cloud water condensation
!   * Real Function DEPOSIT   - ice deposition (not sublimation)
!   * Integer Function GET_INDEXR  - estimate the mean size of raindrops (microns)
!
! UNIQUE: NONE
!  
! LIBRARY: NONE
!  
! COMMON BLOCKS:  
!     CMICRO_CONS  - key constants initialized in GSMCONST
!     CMICRO_STATS - accumulated and maximum statistics
!     CMY_GROWTH   - lookup table for growth of ice crystals in 
!                    water saturated conditions (Miller & Young, 1979)
!     IVENT_TABLES - lookup tables for ventilation effects of ice
!     IACCR_TABLES - lookup tables for accretion rates of ice
!     IMASS_TABLES - lookup tables for mass content of ice
!     IRATE_TABLES - lookup tables for precipitation rates of ice
!     IRIME_TABLES - lookup tables for increase in fall speed of rimed ice
!     RVENT_TABLES - lookup tables for ventilation effects of rain
!     RACCR_TABLES - lookup tables for accretion rates of rain
!     RMASS_TABLES - lookup tables for mass content of rain
!     RVELR_TABLES - lookup tables for fall speeds of rain
!     RRATE_TABLES - lookup tables for precipitation rates of rain
!   
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!
!------------------------------------------------------------------------- 
!--------------- Arrays & constants in argument list --------------------- 
!------------------------------------------------------------------------- 
!
      IMPLICIT NONE
!    
      INTEGER,INTENT(IN) :: KTS,KTE,I_index, J_index, LSFC
      REAL,INTENT(INOUT) ::  ARAIN, ASNOW
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) ::  P_col, QI_col,QR_col    &
     & ,QV_col ,QW_col, RimeF_col, T_col, THICK_col,WC_col,pcond1d      &
     & ,pidep1d,piacw1d,piacwi1d,piacwr1d,piacr1d,picnd1d,pievp1d       &
     & ,pimlt1d,praut1d,pracw1d,prevp1d,pisub1d,pevap1d
!
!------------------------------------------------------------------------- 
!-------------- Common blocks for microphysical statistics ---------------
!------------------------------------------------------------------------- 
!
!------------------------------------------------------------------------- 
!--------- Common blocks for constants initialized in GSMCONST ----------
!
      INTEGER, PARAMETER :: ITLO=-60, ITHI=40
      INTEGER,INTENT(INOUT) :: NSTATS(ITLO:ITHI,4)
      REAL,INTENT(INOUT) :: QMAX(ITLO:ITHI,5),QTOT(ITLO:ITHI,22) 
!
!------------------------------------------------------------------------- 
!--------------- Common blocks for various lookup tables -----------------
!
!--- Discretized growth rates of small ice crystals after their nucleation 
!     at 1 C intervals from -1 C to -35 C, based on calculations by Miller 
!     and Young (1979, JAS) after 600 s of growth.  Resultant growth rates
!     are multiplied by physics time step in GSMCONST.
!
!------------------------------------------------------------------------- 
!
!--- Mean ice-particle diameters varying from 50 microns to 1000 microns
!      (1 mm), assuming an exponential size distribution.  
!
!---- Meaning of the following arrays: 
!        - mdiam - mean diameter (m)
!        - VENTI1 - integrated quantity associated w/ ventilation effects 
!                   (capacitance only) for calculating vapor deposition onto ice
!        - VENTI2 - integrated quantity associated w/ ventilation effects 
!                   (with fall speed) for calculating vapor deposition onto ice
!        - ACCRI  - integrated quantity associated w/ cloud water collection by ice
!        - MASSI  - integrated quantity associated w/ ice mass 
!        - VSNOWI - mass-weighted fall speed of snow (large ice), used to calculate 
!                   precipitation rates
!
!
!------------------------------------------------------------------------- 
!
!--- VEL_RF - velocity increase of rimed particles as functions of crude
!      particle size categories (at 0.1 mm intervals of mean ice particle
!      sizes) and rime factor (different values of Rime Factor of 1.1**N, 
!      where N=0 to Nrime).
!
!------------------------------------------------------------------------- 
!
!--- Mean rain drop diameters varying from 50 microns (0.05 mm) to 1000 microns 
!      (1.0 mm) assuming an exponential size distribution.  
!
!------------------------------------------------------------------------- 
!------- Key parameters, local variables, & important comments ---------
!-----------------------------------------------------------------------
!
!--- TOLER => Tolerance or precision for accumulated precipitation 
!
      REAL, PARAMETER :: TOLER=5.E-7, C2=1./6., RHO0=1.194, Xratio=.025                                           
!
!--- If BLEND=1:
!      precipitation (large) ice amounts are estimated at each level as a 
!      blend of ice falling from the grid point above and the precip ice
!      present at the start of the time step (see TOT_ICE below).
!--- If BLEND=0:
!      precipitation (large) ice amounts are estimated to be the precip
!      ice present at the start of the time step.
!
!--- Extended to include sedimentation of rain on 2/5/01 
!
      REAL, PARAMETER :: BLEND=1.
!
!--- This variable is for debugging purposes (if .true.)
!
      LOGICAL, PARAMETER :: PRINT_diag=.FALSE.
!
!-----------------------------------------------------------------------
!--- Local variables
!-----------------------------------------------------------------------
!
      REAL EMAIRI, N0r, NLICE, NSmICE
      LOGICAL CLEAR, ICE_logical, DBG_logical, RAIN_logical
      INTEGER :: IDR,INDEX_MY,INDEXR,INDEXR1,INDEXS,IPASS,ITDX,IXRF,    &
     &           IXS,LBEF,L
!
      REAL :: ABI,ABW,AIEVP,ARAINnew,ASNOWnew,BLDTRH,BUDGET,            &
     &        CREVP,DELI,DELR,DELT,DELV,DELW,DENOMF,                    &
     &        DENOMI,DENOMW,DENOMWI,DIDEP,                              &
     &        DIEVP,DIFFUS,DLI,DTPH,DTRHO,DUM,DUM1,                     &
     &        DUM2,DWV0,DWVI,DWVR,DYNVIS,ESI,ESW,FIR,FLARGE,FLIMASS,    &
     &        FSMALL,FWR,FWS,GAMMAR,GAMMAS,                             &
     &        PCOND,PIACR,PIACW,PIACWI,PIACWR,PICND,PIDEP,PIDEP_max,    &
     &        PIEVP,PILOSS,PIMLT,PINT,PP,PRACW,PRAUT,PREVP,PRLOSS,      &
     &        QI,QInew,QLICE,QR,QRnew,QSI,QSIgrd,QSInew,QSW,QSW0,       &
     &        QSWgrd,QSWnew,QT,QTICE,QTnew,QTRAIN,QV,QW,QWnew,          &
     &        RFACTOR,RHO,RIMEF,RIMEF1,RQR,RR,RRHO,SFACTOR,             &
     &        TC,TCC,TFACTOR,THERM_COND,THICK,TK,TK2,TNEW,              &
     &        TOT_ICE,TOT_ICEnew,TOT_RAIN,TOT_RAINnew,                  &
     &        VEL_INC,VENTR,VENTIL,VENTIS,VRAIN1,VRAIN2,VRIMEF,VSNOW,   &
     &        WC,WCnew,WSgrd,WS,WSnew,WV,WVnew,WVQW,                    &
     &        XLF,XLF1,XLI,XLV,XLV1,XLV2,XLIMASS,XRF,XSIMASS,           &
     &        QRdum,VCI                                     !-- new variables
!
!#######################################################################
!########################## Begin Execution ############################
!#######################################################################
!
!
      ARAIN=0.                ! Accumulated rainfall into grid box from above (kg/m**2)
      ASNOW=0.                ! Accumulated snowfall into grid box from above (kg/m**2)
!
!-----------------------------------------------------------------------
!------------ Loop from top (L=1) to surface (L=LSFC) ------------------
!-----------------------------------------------------------------------
!
      DO 10 L=1,LSFC
      pcond1d(L)=0.
      pidep1d(L)=0.
      piacw1d(L)=0.
      piacwi1d(L)=0.
      piacwr1d(L)=0.
      piacr1d(L)=0.
      picnd1d(L)=0.
      pievp1d(L)=0.
      pimlt1d(L)=0.
      praut1d(L)=0.
      pracw1d(L)=0.
      prevp1d(L)=0.
      pisub1d(L)=0.
      pevap1d(L)=0.
!--- Skip this level and go to the next lower level if no condensate 
!      and very low specific humidities
!
        IF (QV_col(L).LE.EPSQ .AND. WC_col(L).LE.EPSQ) GO TO 10
!
!-----------------------------------------------------------------------
!------------ Proceed with cloud microphysics calculations -------------
!-----------------------------------------------------------------------
!
          TK=T_col(L)         ! Temperature (deg K)
          TC=TK-T0C           ! Temperature (deg C)
          PP=P_col(L)         ! Pressure (Pa)
          QV=QV_col(L)        ! Specific humidity of water vapor (kg/kg)
          WV=QV/(1.-QV)       ! Water vapor mixing ratio (kg/kg)
          WC=WC_col(L)        ! Grid-scale mixing ratio of total condensate (water or ice; kg/kg)
!
!-----------------------------------------------------------------------
!--- Moisture variables below are mixing ratios & not specifc humidities
!-----------------------------------------------------------------------
!
          CLEAR=.TRUE.
!    
!--- This check is to determine grid-scale saturation when no condensate is present
!    
          ESW=MIN(1000.*FPVS0(TK),0.99*PP) ! Saturation vapor pressure w/r/t water
          QSW=EPS*ESW/(PP-ESW)             ! Saturation mixing ratio w/r/t water
          WS=QSW                           ! General saturation mixing ratio (water/ice)
          IF (TC .LT. 0.) THEN
            ESI=MIN(1000.*FPVS(TK),0.99*PP)  ! Saturation vapor pressure w/r/t ice
            QSI=EPS*ESI/(PP-ESI)           ! Saturation mixing ratio w/r/t water
            WS=QSI                         ! General saturation mixing ratio (water/ice)
          ENDIF
!
!--- Effective grid-scale Saturation mixing ratios
!
          QSWgrd=RHgrd*QSW
          QSIgrd=RHgrd*QSI
          WSgrd=RHgrd*WS
!
!--- Check if air is subsaturated and w/o condensate
!
          IF (WV.GT.WSgrd .OR. WC.GT.EPSQ) CLEAR=.FALSE.
!
!--- Check if any rain is falling into layer from above
!
          IF (ARAIN .GT. CLIMIT) THEN
            CLEAR=.FALSE.
          ELSE
            ARAIN=0.
          ENDIF
!
!--- Check if any ice is falling into layer from above
!
!--- NOTE that "SNOW" in variable names is synonomous with 
!    large, precipitation ice particles
!
          IF (ASNOW .GT. CLIMIT) THEN
            CLEAR=.FALSE.
          ELSE
            ASNOW=0.
          ENDIF
!
!-----------------------------------------------------------------------
!-- Loop to the end if in clear, subsaturated air free of condensate ---
!-----------------------------------------------------------------------
!
          IF (CLEAR) GO TO 10
!
!-----------------------------------------------------------------------
!--------- Initialize RHO, THICK & microphysical processes -------------
!-----------------------------------------------------------------------
!
!
!--- Virtual temperature, TV=T*(1./EPS-1)*Q, Q is specific humidity;
!    (see pp. 63-65 in Fleagle & Businger, 1963)
!
          RHO=PP/(RD*TK*(1.+EPS1*QV))   ! Air density (kg/m**3)
          RRHO=1./RHO                ! Reciprocal of air density
          DTRHO=DTPH*RHO             ! Time step * air density
          BLDTRH=BLEND*DTRHO         ! Blend parameter * time step * air density
          THICK=THICK_col(L)         ! Layer thickness = RHO*DZ = -DP/G = (Psfc-Ptop)*D_ETA/(G*ETA_sfc)
!
          ARAINnew=0.                ! Updated accumulated rainfall
          ASNOWnew=0.                ! Updated accumulated snowfall
          QI=QI_col(L)               ! Ice mixing ratio
          QInew=0.                   ! Updated ice mixing ratio
          QR=QR_col(L)               ! Rain mixing ratio
          QRnew=0.                   ! Updated rain ratio
          QW=QW_col(L)               ! Cloud water mixing ratio
          QWnew=0.                   ! Updated cloud water ratio
!
          PCOND=0.                   ! Condensation (>0) or evaporation (<0) of cloud water (kg/kg)
          PIDEP=0.                   ! Deposition (>0) or sublimation (<0) of ice crystals (kg/kg)
          PIACW=0.                   ! Cloud water collection (riming) by precipitation ice (kg/kg; >0)
          PIACWI=0.                  ! Growth of precip ice by riming (kg/kg; >0)
          PIACWR=0.                  ! Shedding of accreted cloud water to form rain (kg/kg; >0)
          PIACR=0.                   ! Freezing of rain onto large ice at supercooled temps (kg/kg; >0)
          PICND=0.                   ! Condensation (>0) onto wet, melting ice (kg/kg)
          PIEVP=0.                   ! Evaporation (<0) from wet, melting ice (kg/kg)
          PIMLT=0.                   ! Melting ice (kg/kg; >0)
          PRAUT=0.                   ! Cloud water autoconversion to rain (kg/kg; >0)
          PRACW=0.                   ! Cloud water collection (accretion) by rain (kg/kg; >0)
          PREVP=0.                   ! Rain evaporation (kg/kg; <0)
!
!--- Double check input hydrometeor mixing ratios
!
!          DUM=WC-(QI+QW+QR)
!          DUM1=ABS(DUM)
!          DUM2=TOLER*MIN(WC, QI+QW+QR)
!          IF (DUM1 .GT. DUM2) THEN
!            WRITE(6,"(/2(a,i4),a,i2)") '{@ i=',I_index,' j=',J_index,
!     &                                 ' L=',L
!            WRITE(6,"(4(a12,g11.4,1x))") 
!     & '{@ TCold=',TC,'P=',.01*PP,'DIFF=',DUM,'WCold=',WC,
!     & '{@ QIold=',QI,'QWold=',QW,'QRold=',QR
!          ENDIF
!
!***********************************************************************
!*********** MAIN MICROPHYSICS CALCULATIONS NOW FOLLOW! ****************
!***********************************************************************
!
!--- Calculate a few variables, which are used more than once below
!
!--- Latent heat of vaporization as a function of temperature from
!      Bolton (1980, JAS)
!
          XLV=3.148E6-2370*TK        ! Latent heat of vaporization (Lv)
          XLF=XLS-XLV                ! Latent heat of fusion (Lf)
          XLV1=XLV*RCP               ! Lv/Cp
          XLF1=XLF*RCP               ! Lf/Cp
          TK2=1./(TK*TK)             ! 1./TK**2
          XLV2=XLV*XLV*QSW*TK2/RV    ! Lv**2*Qsw/(Rv*TK**2)
          DENOMW=1.+XLV2*RCP         ! Denominator term, Clausius-Clapeyron correction
!
!--- Basic thermodynamic quantities
!      * DYNVIS - dynamic viscosity  [ kg/(m*s) ]
!      * THERM_COND - thermal conductivity  [ J/(m*s*K) ]
!      * DIFFUS - diffusivity of water vapor  [ m**2/s ]
!
          TFACTOR=SQRT(TK*TK*TK)/(TK+120.)
          DYNVIS=1.496E-6*TFACTOR
          THERM_COND=2.116E-3*TFACTOR
          DIFFUS=8.794E-5*TK**1.81/PP
!
!--- Air resistance term for the fall speed of ice following the
!      basic research by Heymsfield, Kajikawa, others 
!
          GAMMAS=(1.E5/PP)**C1
!
!--- Air resistance for rain fall speed (Beard, 1985, JAS, p.470)
!
          GAMMAR=(RHO0/RHO)**.4
!
!----------------------------------------------------------------------
!-------------  IMPORTANT MICROPHYSICS DECISION TREE  -----------------
!----------------------------------------------------------------------
!
!--- Determine if conditions supporting ice are present
!
          IF (TC.LT.0. .OR. QI.GT. EPSQ .OR. ASNOW.GT.CLIMIT) THEN
            ICE_logical=.TRUE.
          ELSE
            ICE_logical=.FALSE.
            QLICE=0.
            QTICE=0.
          ENDIF
!
!--- Determine if rain is present
!
          RAIN_logical=.FALSE.
          IF (ARAIN.GT.CLIMIT .OR. QR.GT.EPSQ) RAIN_logical=.TRUE.
!
          IF (ICE_logical) THEN
!
!--- IMPORTANT:  Estimate time-averaged properties.
!
!---
!  * FLARGE  - ratio of number of large ice to total (large & small) ice
!  * FSMALL  - ratio of number of small ice crystals to large ice particles
!  ->  Small ice particles are assumed to have a mean diameter of 50 microns.
!  * XSIMASS - used for calculating small ice mixing ratio
!---
!  * TOT_ICE - total mass (small & large) ice before microphysics,
!              which is the sum of the total mass of large ice in the 
!              current layer and the input flux of ice from above
!  * PILOSS  - greatest loss (<0) of total (small & large) ice by
!              sublimation, removing all of the ice falling from above
!              and the ice within the layer
!  * RimeF1  - Rime Factor, which is the mass ratio of total (unrimed & rimed) 
!              ice mass to the unrimed ice mass (>=1)
!  * VrimeF  - the velocity increase due to rime factor or melting (ratio, >=1)
!  * VSNOW   - Fall speed of rimed snow w/ air resistance correction
!  * VCI     - Fall speed of 50-micron ice crystals w/ air resistance correction
!  * EMAIRI  - equivalent mass of air associated layer and with fall of snow into layer
!  * XLIMASS - used for calculating large ice mixing ratio
!  * FLIMASS - mass fraction of large ice
!  * QTICE   - time-averaged mixing ratio of total ice
!  * QLICE   - time-averaged mixing ratio of large ice
!  * NLICE   - time-averaged number concentration of large ice
!  * NSmICE  - number concentration of small ice crystals at current level
!---
!--- Assumed number fraction of large ice particles to total (large & small) 
!    ice particles, which is based on a general impression of the literature.
!
            WVQW=WV+QW                ! Water vapor & cloud water
!


            IF (TC>=0.) THEN
   !
   !--- Eliminate small ice particle contributions for melting & sublimation
   !
              FLARGE=FLARGE1
            ELSE
   !
   !--- Enhanced number of small ice particles during depositional growth
   !    (effective only when 0C > T >= T_ice [-40C] )
   !
              FLARGE=FLARGE2
   !
   !--- Larger number of small ice particles due to rime splintering
   !
              IF (TC.GE.-8. .AND. TC.LE.-3.) FLARGE=.5*FLARGE
!
            ENDIF            ! End IF (TC.GE.0. .OR. WVQW.LT.QSIgrd)
            FSMALL=(1.-FLARGE)/FLARGE
            XSIMASS=RRHO*MASSI(MDImin)*FSMALL
            IF (QI.LE.EPSQ .AND. ASNOW.LE.CLIMIT) THEN
              INDEXS=MDImin
              TOT_ICE=0.
              PILOSS=0.
              RimeF1=1.
              VrimeF=1.
              VEL_INC=GAMMAS
              VSNOW=0.
              VCI=0.
              EMAIRI=THICK
              XLIMASS=RRHO*RimeF1*MASSI(INDEXS)
              FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
              QLICE=0.
              QTICE=0.
              NLICE=0.
              NSmICE=0.
            ELSE
   !
   !--- For T<0C mean particle size follows Houze et al. (JAS, 1979, p. 160), 
   !    converted from Fig. 5 plot of LAMDAs.  Similar set of relationships 
   !    also shown in Fig. 8 of Ryan (BAMS, 1996, p. 66).
   !
              DUM=XMImax*EXP(.0536*TC)
              INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
              TOT_ICE=THICK*QI+BLEND*ASNOW
              PILOSS=-TOT_ICE/THICK
              LBEF=MAX(1,L-1)
              DUM1=RimeF_col(LBEF)
              DUM2=RimeF_col(L)
              RimeF1=(DUM2*THICK*QI+DUM1*BLEND*ASNOW)/TOT_ICE
              RimeF1=MIN(RimeF1, RFmax)
              VCI=GAMMAS*VSNOWI(MDImin)
              DO IPASS=0,1
                IF (RimeF1 .LE. 1.) THEN
                  RimeF1=1.
                  VrimeF=1.
                ELSE
                  IXS=MAX(2, MIN(INDEXS/100, 9))
                  XRF=10.492*ALOG(RimeF1)
                  IXRF=MAX(0, MIN(INT(XRF), Nrime))
                  IF (IXRF .GE. Nrime) THEN
                    VrimeF=VEL_RF(IXS,Nrime)
                  ELSE
                    VrimeF=VEL_RF(IXS,IXRF)+(XRF-FLOAT(IXRF))*          &
     &                    (VEL_RF(IXS,IXRF+1)-VEL_RF(IXS,IXRF))
                  ENDIF
                ENDIF            ! End IF (RimeF1 .LE. 1.)
                VEL_INC=GAMMAS*VrimeF*VrimeF   !-- Faster rimed ice fall speeds
                VSNOW=VEL_INC*VSNOWI(INDEXS)
                EMAIRI=THICK+BLDTRH*VSNOW
                XLIMASS=RRHO*RimeF1*MASSI(INDEXS)
                FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
                QTICE=TOT_ICE/EMAIRI
                QLICE=FLIMASS*QTICE
                NLICE=QLICE/XLIMASS
                NSmICE=Fsmall*NLICE
   !
                IF ( (NLICE.GE.NLImin .AND. NLICE.LE.NLImax)            &
     &                .OR. IPASS.EQ.1) THEN
                  EXIT
                ELSE
!-- Enforce NLImin at all temperatures                  IF (TC < 0) THEN
                    DUM=MAX(NLImin, MIN(NLImax, NLICE) )
                    XLI=RHO*QLICE/(DUM*RimeF1)
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
!-- Enforce NLImin at all temperatures                  ENDIF               ! End IF (TC < 0)
        !
        !--- Reduce excessive accumulation of ice at upper levels
        !    associated with strong grid-resolved ascent
        !
        !--- Force NLICE to be between NLImin and NLImax
        !
        !
        !--- 8/22/01: Increase density of large ice if maximum limits 
        !    are reached for number concentration (NLImax) and mean size 
        !    (MDImax).  Done to increase fall out of ice.
        !
                      IF (DUM>=NLImax)                                  &
     &                   RimeF1=RHO*QLICE/(NLImax*MASSI(INDEXS))
                    ENDIF             ! End IF (XLI .LE. MASSI(MDImin) )

!            WRITE(6,"(4(a12,g11.4,1x))") 
!     & '{$ TC=',TC,'P=',.01*PP,'NLICE=',NLICE,'DUM=',DUM,
!     & '{$ XLI=',XLI,'INDEXS=',FLOAT(INDEXS),'RHO=',RHO,'QTICE=',QTICE,
!     & '{$ XSIMASS=',XSIMASS,'RimeF1=',RimeF1
                ENDIF                  ! End IF ( (NLICE.GE.NLImin .AND. NLICE.LE.NLImax) ...
              ENDDO                    ! End DO IPASS=0,1
            ENDIF                      ! End IF (QI.LE.EPSQ .AND. ASNOW.LE.CLIMIT)
          ENDIF                        ! End IF (ICE_logical)
!
!----------------------------------------------------------------------
!--------------- Calculate individual processes -----------------------
!----------------------------------------------------------------------
!
!--- Cloud water autoconversion to rain (PRAUT) and collection of cloud 
!    water by precipitation ice (PIACW)
!    
          IF (QW.GT.EPSQ .AND. TC.GE.T_ICE) THEN
!
!-- July 2010 version follows Liu & Daum (JAS, 2004) and Liu et al. (JAS, 2006)
!
            DUM=BRAUT*RHO*RHO*QW*QW*QW
            DUM1=ARAUT*RHO*RHO*QW*QW
            PRAUT=MIN(QW, DUM*(1.-EXP(-DUM1*DUM1)) )
            IF (QLICE .GT. EPSQ) THEN
      !
      !--- Collection of cloud water by large ice particles ("snow")
      !    PIACWI=PIACW for riming, PIACWI=0 for shedding
      !
              FWS=MIN(1., CIACW*VEL_INC*NLICE*ACCRI(INDEXS)/PP**C1)
              PIACW=FWS*QW
              IF (TC .LT. 0.) PIACWI=PIACW    ! Large ice riming
            ENDIF           ! End IF (QLICE .GT. EPSQ)
          ENDIF             ! End IF (QW.GT.EPSQ .AND. TC.GE.T_ICE)
!
!----------------------------------------------------------------------
!--- Loop around some of the ice-phase processes if no ice should be present
!----------------------------------------------------------------------
!
          IF (ICE_logical .EQV. .FALSE.) GO TO 20
!
!--- Now the pretzel logic of calculating ice deposition
!
          IF (TC.LT.T_ICE .AND. (WV.GT.QSWgrd .OR. QW.GT.EPSQ)) THEN
   !
   !--- Adjust to ice saturation at T<T_ICE (-40C) if saturated w/r/t water
   !    or if cloud water is present (homogeneous glaciation).
   !    
            PCOND=-QW
            DUM1=TK+XLV1*PCOND                 ! Updated (dummy) temperature (deg K)
            DUM2=WV+QW                         ! Updated (dummy) water vapor mixing ratio
            DUM=MIN(1000.*FPVS(DUM1),0.99*PP)  ! Updated (dummy) saturation vapor pressure w/r/t ice
            DUM=RHgrd*EPS*DUM/(PP-DUM)         ! Updated (dummy) saturation mixing ratio w/r/t ice

!-- Debug 20120111
IF (WARN1 .AND. DUM1<XMIN) THEN
   WRITE(0,*) 'WARN1: Water saturation T<180K; I,J,L,TC,P=',   &
              I_index,J_index,L,DUM1-T0C,.01*PP
   WARN1=.FALSE.
ENDIF
            IF (DUM2>DUM) PIDEP=DEPOSIT(PP,DUM1,DUM2,I_index,J_index,L)

            DWVi=0.    ! Used only for debugging
   !
          ELSE IF (TC .LT. 0.) THEN
   !
   !--- These quantities are handy for ice deposition/sublimation
   !    PIDEP_max - max deposition or minimum sublimation to ice saturation
   !
            DENOMI=1.+XLS2*QSI*TK2
            DWVi=MIN(WVQW,QSW)-QSI
            PIDEP_max=MAX(PILOSS, DWVi/DENOMI)
            DIDEP=0.     !-- Vapor deposition/sublimation onto existing ice
            PINT=0.      !-- Ice initiation (part of PIDEP calculation, kg/kg)
            IF (QTICE .GT. 0.) THEN
      !
      !--- Calculate ice deposition/sublimation
      !      * SFACTOR - [VEL_INC**.5]*[Schmidt**(1./3.)]*[(RHO/DYNVIS)**.5],
      !        where Schmidt (Schmidt Number) =DYNVIS/(RHO*DIFFUS)
      !      * Units: SFACTOR - s**.5/m ;  ABI - m**2/s ;  NLICE - m**-3 ;
      !               VENTIL, VENTIS - m**-2 ;  VENTI1 - m ;  
      !               VENTI2 - m**2/s**.5 ; DIDEP - unitless
      !
              SFACTOR=SQRT(VEL_INC)*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2
              ABI=1./(RHO*XLS3*QSI*TK2/THERM_COND+1./DIFFUS)
      !
      !--- VENTIL - Number concentration * ventilation factors for large ice
      !--- VENTIS - Number concentration * ventilation factors for small ice
      !
      !--- Variation in the number concentration of ice with time is not
      !      accounted for in these calculations (could be in the future).
      !
              VENTIL=(VENTI1(INDEXS)+SFACTOR*VENTI2(INDEXS))*NLICE
              VENTIS=(VENTI1(MDImin)+SFACTOR*VENTI2(MDImin))*NSmICE
              DIDEP=ABI*(VENTIL+VENTIS)*DTPH
              IF (DIDEP>=Xratio) DIDEP=(1.-EXP(-DIDEP*DENOMI))/DENOMI
            ENDIF   !-- IF (QTICE .GT. 0.) THEN
      !
      !--- Two modes of ice nucleation from Meyers et al. (JAM, 1992):
      !      (1) Deposition & condensation freezing nucleation - eq. (2.4),
      !          requires ice supersaturation
      !      (2) Contact freezing nucleation - eq. (2.6), requires presence
      !          of cloud water
      !
      !--- Ice crystal growth during the physics time step is calculated using
      !    Miller & Young (1979, JAS) and is represented by MY_GROWTH(INDEX_MY),
      !    where INDEX_MY is tabulated air temperatures between -1 and -35 C.
      !    The original Miller & Young (MY) calculations only went down to -30C,
      !    so a fixed value is assumed at temperatures colder than -30C.
      !
            IF (DWVi>0. .AND. TC<=T_ICE_init) THEN
              DUM1=DWVi/QSI                                     !- Ice supersaturation ratio
              DUM2=1.E3*EXP(12.96*DUM1-.639)                    !- (2.4) from Meyers
              IF (QW > EPSQ) DUM2=DUM2+1.E3*EXP(-0.262*TC-2.8)  !- (2.6) from Meyers
              INDEX_MY=MAX(MY_T1, MIN( INT(.5-TC), MY_T2 ) )
      !-- Only initiate ice in excess of what is present (QTICE)
              PINT=MAX(0., DUM2*MY_GROWTH_NMM(INDEX_MY)*RRHO-QTICE)
            ENDIF
      !
      !--- Calculate PIDEP, but also account for limited water vapor supply
      !
            IF (DWVi>0.) THEN
              PIDEP=MIN(DWVI*DIDEP+PINT, PIDEP_max)
            ELSE IF (DWVi<0.) THEN
              PIDEP=MAX(DWVi*DIDEP, PIDEP_max)
            ENDIF
   !
          ENDIF         ! End IF (TC.LT.T_ICE .AND. (WV.GT.QSWgrd .OR. QW.GT.EPSQ))
!
!------------------------------------------------------------------------
!
20      CONTINUE     ! Jump here if conditions for ice are not present


!
!------------------------------------------------------------------------
!
!--- Cloud water condensation
!
          IF (TC.GE.T_ICE .AND. (QW.GT.EPSQ .OR. WV.GT.QSWgrd)) THEN
            IF (PIACWI.EQ.0. .AND. PIDEP.EQ.0.) THEN
              PCOND=CONDENSE(PP,QW,TK,WV,I_index,J_index,L)    !-- Debug 20120111
            ELSE
   !
   !--- Modify cloud condensation in response to ice processes
   !
              DUM=XLV*QSWgrd*RCPRV*TK2
              DENOMWI=1.+XLS*DUM
              DENOMF=XLF*DUM
              DUM=MAX(0., PIDEP)
              PCOND=(WV-QSWgrd-DENOMWI*DUM-DENOMF*PIACWI)/DENOMW
              DUM1=-QW
              DUM2=PCOND-PIACW
              IF (DUM2 .LT. DUM1) THEN
      !
      !--- Limit cloud water sinks
      !
                DUM=DUM1/DUM2
                PCOND=DUM*PCOND
                PIACW=DUM*PIACW
                PIACWI=DUM*PIACWI
              ENDIF        ! End IF (DUM2 .LT. DUM1)
            ENDIF          ! End IF (PIACWI.EQ.0. .AND. PIDEP.EQ.0.)
          ENDIF            ! End IF (TC.GE.T_ICE .AND. (QW.GT.EPSQ .OR. WV.GT.QSWgrd))
!
!--- Limit freezing of accreted rime to prevent temperature oscillations,
!    a crude Schumann-Ludlam limit (p. 209 of Young, 1993). 
!
          TCC=TC+XLV1*PCOND+XLS1*PIDEP+XLF1*PIACWI
          IF (TCC .GT. 0.) THEN
            PIACWI=0.
            TCC=TC+XLV1*PCOND+XLS1*PIDEP
          ENDIF
          IF (TC.GT.0. .AND. TCC.GT.0. .AND. ICE_logical) THEN
   !
   !--- Calculate melting and evaporation/condensation
   !      * Units: SFACTOR - s**.5/m ;  ABI - m**2/s ;  NLICE - m**-3 ;
   !               VENTIL - m**-2 ;  VENTI1 - m ;  
   !               VENTI2 - m**2/s**.5 ; CIEVP - /s
   !
            SFACTOR=SQRT(VEL_INC)*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2
            VENTIL=NLICE*(VENTI1(INDEXS)+SFACTOR*VENTI2(INDEXS))
            AIEVP=VENTIL*DIFFUS*DTPH
            IF (AIEVP .LT. Xratio) THEN
              DIEVP=AIEVP
            ELSE
              DIEVP=1.-EXP(-AIEVP)
            ENDIF
            QSW0=EPS*ESW0/(PP-ESW0)
            DWV0=MIN(WV,QSW)-QSW0
            DUM=QW+PCOND
            IF (WV.LT.QSW .AND. DUM.LE.EPSQ) THEN
   !
   !--- Evaporation from melting snow (sink of snow) or shedding
   !    of water condensed onto melting snow (source of rain)
   !
              DUM=DWV0*DIEVP
              PIEVP=MAX( MIN(0., DUM), PILOSS)
              PICND=MAX(0., DUM)
            ENDIF            ! End IF (WV.LT.QSW .AND. DUM.LE.EPSQ)
            PIMLT=THERM_COND*TCC*VENTIL*RRHO*DTPH/XLF
   !
   !--- Limit melting to prevent temperature oscillations across 0C
   !
            DUM1=MAX( 0., (TCC+XLV1*PIEVP)/XLF1 )
            PIMLT=MIN(PIMLT, DUM1)
   !
   !--- Limit loss of snow by melting (>0) and evaporation
   !
            DUM=PIEVP-PIMLT
            IF (DUM .LT. PILOSS) THEN
              DUM1=PILOSS/DUM
              PIMLT=PIMLT*DUM1
              PIEVP=PIEVP*DUM1
            ENDIF           ! End IF (DUM .GT. QTICE)
          ENDIF             ! End IF (TC.GT.0. .AND. TCC.GT.0. .AND. ICE_logical) 
!
!--- IMPORTANT:  Estimate time-averaged properties.
!
!  * TOT_RAIN - total mass of rain before microphysics, which is the sum of
!               the total mass of rain in the current layer and the input 
!               flux of rain from above
!  * VRAIN1   - fall speed of rain into grid from above (with air resistance correction)
!  * QTRAIN   - time-averaged mixing ratio of rain (kg/kg)
!  * PRLOSS   - greatest loss (<0) of rain, removing all rain falling from
!               above and the rain within the layer
!  * RQR      - rain content (kg/m**3)
!  * INDEXR   - mean size of rain drops to the nearest 1 micron in size
!  * N0r      - intercept of rain size distribution (typically 10**6 m**-4)
!
          TOT_RAIN=0.
          VRAIN1=0.
          QTRAIN=0.
          PRLOSS=0.
          RQR=0.
          N0r=0.
          INDEXR=MDRmin
          INDEXR1=INDEXR    !-- For debugging only
          IF (RAIN_logical) THEN
            IF (ARAIN .LE. 0.) THEN
              INDEXR=MDRmin
              VRAIN1=0.
            ELSE
   !
   !--- INDEXR (related to mean diameter) & N0r could be modified 
   !      by land/sea properties, presence of convection, etc.
   !
   !--- Rain rate normalized to a density of 1.194 kg/m**3
   !
              RR=ARAIN/(DTPH*GAMMAR)
   !--- Integer function to estimate the mean size of the raindrops in microns
              INDEXR=GET_INDEXR(RR)
              VRAIN1=GAMMAR*VRAIN(INDEXR)
            ENDIF              ! End IF (ARAIN .LE. 0.)
            INDEXR1=INDEXR     ! For debugging only
            TOT_RAIN=THICK*QR+BLEND*ARAIN
            QTRAIN=TOT_RAIN/(THICK+BLDTRH*VRAIN1)
            PRLOSS=-TOT_RAIN/THICK
            RQR=RHO*QTRAIN
   !
   !--- RQR - time-averaged rain content (kg/m**3)
   !
            IF (RQR .LE. RQR_DRmin) THEN
              N0r=MAX(N0rmin, CN0r_DMRmin*RQR)
              INDEXR=MDRmin
            ELSE IF (RQR .GE. RQR_DRmax) THEN
              N0r=CN0r_DMRmax*RQR
              INDEXR=MDRmax
            ELSE
              N0r=N0r0
              DUM=SQRT(SQRT(RQR))
              INDEXR=MAX( XMRmin, MIN(CN0r0*DUM, XMRmax) )
            ENDIF
   !
            IF (TC .LT. T_ICE) THEN
              PIACR=-PRLOSS
            ELSE
              DWVr=WV-PCOND-QSW
              DUM=QW+PCOND
              IF (DWVr.LT.0. .AND. DUM.LE.EPSQ) THEN
      !
      !--- Rain evaporation
      !
      !    * RFACTOR - [GAMMAR**.5]*[Schmidt**(1./3.)]*[(RHO/DYNVIS)**.5],
      !        where Schmidt (Schmidt Number) =DYNVIS/(RHO*DIFFUS)
      !
      !    * Units: RFACTOR - s**.5/m ;  ABW - m**2/s ;  VENTR - m**-2 ;  
      !             N0r - m**-4 ;  VENTR1 - m**2 ;  VENTR2 - m**3/s**.5 ;
      !             CREVP - unitless
      !
                RFACTOR=SQRT(GAMMAR)*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2
                ABW=1./(RHO*XLV2/THERM_COND+1./DIFFUS)
      !
      !--- Note that VENTR1, VENTR2 lookup tables do not include the 
      !      1/Davg multiplier as in the ice tables
      !
                VENTR=N0r*(VENTR1(INDEXR)+RFACTOR*VENTR2(INDEXR))
                CREVP=ABW*VENTR*DTPH
                IF (CREVP .LT. Xratio) THEN
                  DUM=DWVr*CREVP
                ELSE
                  DUM=DWVr*(1.-EXP(-CREVP*DENOMW))/DENOMW
                ENDIF
                PREVP=MAX(DUM, PRLOSS)
              ELSE IF (QW .GT. EPSQ) THEN
                FWR=CRACW*GAMMAR*N0r*ACCRR(INDEXR)
                PRACW=MIN(1.,FWR)*QW
              ENDIF           ! End IF (DWVr.LT.0. .AND. DUM.LE.EPSQ)
      !
              IF (TC.LT.0. .AND. TCC.LT.0.) THEN
         !
         !--- Biggs (1953) heteorogeneous freezing (e.g., Lin et al., 1983)
         !   - Rescaled mean drop diameter from microns (INDEXR) to mm (DUM) to prevent underflow
         !
                DUM=.001*FLOAT(INDEXR)
                DUM=(EXP(ABFR*TC)-1.)*DUM*DUM*DUM*DUM*DUM*DUM*DUM
                PIACR=MIN(CBFR*N0r*RRHO*DUM, QTRAIN)
                IF (QLICE .GT. EPSQ) THEN
            !
            !--- Freezing of rain by collisions w/ large ice
            !
                  DUM=GAMMAR*VRAIN(INDEXR)
                  DUM1=DUM-VSNOW
            !
            !--- DUM2 - Difference in spectral fall speeds of rain and
            !      large ice, parameterized following eq. (48) on p. 112 of 
            !      Murakami (J. Meteor. Soc. Japan, 1990)
            !
                  DUM2=SQRT(DUM1*DUM1+.04*DUM*VSNOW)
                  DUM1=5.E-12*INDEXR*INDEXR+2.E-12*INDEXR*INDEXS        &
     &                 +.5E-12*INDEXS*INDEXS
                  FIR=MIN(1., CIACR*NLICE*DUM1*DUM2)
            !
            !--- Future?  Should COLLECTION BY SMALL ICE SHOULD BE INCLUDED???
            !
                  PIACR=MIN(PIACR+FIR*QTRAIN, QTRAIN)
                ENDIF        ! End IF (QLICE .GT. EPSQ)
                DUM=PREVP-PIACR
                If (DUM .LT. PRLOSS) THEN
                  DUM1=PRLOSS/DUM
                  PREVP=DUM1*PREVP
                  PIACR=DUM1*PIACR
                ENDIF        ! End If (DUM .LT. PRLOSS)
              ENDIF          ! End IF (TC.LT.0. .AND. TCC.LT.0.)
            ENDIF            ! End IF (TC .LT. T_ICE)
          ENDIF              ! End IF (RAIN_logical) 
!
!----------------------------------------------------------------------
!---------------------- Main Budget Equations -------------------------
!----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!--- Update fields, determine characteristics for next lower layer ----
!-----------------------------------------------------------------------
!
!--- Carefully limit sinks of cloud water
!
          DUM1=PIACW+PRAUT+PRACW-MIN(0.,PCOND)
          IF (DUM1 .GT. QW) THEN
            DUM=QW/DUM1
            PIACW=DUM*PIACW
            PIACWI=DUM*PIACWI
            PRAUT=DUM*PRAUT
            PRACW=DUM*PRACW
            IF (PCOND .LT. 0.) PCOND=DUM*PCOND
          ENDIF
          PIACWR=PIACW-PIACWI          ! TC >= 0C
!
!--- QWnew - updated cloud water mixing ratio
!
          DELW=PCOND-PIACW-PRAUT-PRACW
          QWnew=QW+DELW
          IF (QWnew .LE. EPSQ) QWnew=0.
          IF (QW.GT.0. .AND. QWnew.NE.0.) THEN
            DUM=QWnew/QW
            IF (DUM .LT. TOLER) QWnew=0.
          ENDIF
!
!--- Update temperature and water vapor mixing ratios
!
          DELT= XLV1*(PCOND+PIEVP+PICND+PREVP)                          &
     &         +XLS1*PIDEP+XLF1*(PIACWI+PIACR-PIMLT)
          Tnew=TK+DELT
!
          DELV=-PCOND-PIDEP-PIEVP-PICND-PREVP
          WVnew=WV+DELV
!
!--- Update ice mixing ratios
!
!---
!  * TOT_ICEnew - total mass (small & large) ice after microphysics,
!                 which is the sum of the total mass of ice in the 
!                 layer and the flux of ice out of the grid box below
!  * RimeF      - Rime Factor, which is the mass ratio of total (unrimed & 
!                 rimed) ice mass to the unrimed ice mass (>=1)
!  * QInew      - updated mixing ratio of total (large & small) ice in layer
!  * QLICEnew=FLIMASS*QInew, an estimate of the updated large ice mixing ratio
!
!  -> TOT_ICEnew=QInew*THICK+QLICEnew*BLDTRH*VSNOW+(QInew-QLICEnew)*BLDTRH*VCI
!               =QInew*THICK+QInew*FLIMASS*BLDTRH*VSNOW+QInew*(1.-FLIMASS)*BLDTRH*VCI
!               =QInew*THICK+QInew*BLDTRH*(FLIMASS*VSNOW+(1.-FLIMASS)*VCI)
!               =QInew*(THICK+BLDTRH*(FLIMASS*VSNOW+(1.-FLIMASS)*VCI))
!  -> Rearranging this equation gives:
!     QInew=TOT_ICEnew/(THICK+BLDTRH*(FLIMASS*VSNOW+(1.-FLIMASS)*VCI))
!
!  * ASNOWnew   - updated accumulation of snow and cloud ice at bottom of grid cell
!      -> ASNOWnew=QInew*BLDTRH*(FLIMASS+VSNOW+(1.-FLIMASS)*VCI)
!---
!
          DELI=0.
          RimeF=1.
          IF (ICE_logical) THEN
            DELI=PIDEP+PIEVP+PIACWI+PIACR-PIMLT
            TOT_ICEnew=TOT_ICE+THICK*DELI
            IF (TOT_ICE.GT.0. .AND. TOT_ICEnew.NE.0.) THEN
              DUM=TOT_ICEnew/TOT_ICE
              IF (DUM .LT. TOLER) TOT_ICEnew=0.
            ENDIF
            IF (TOT_ICEnew .LE. CLIMIT) THEN
              TOT_ICEnew=0.
              RimeF=1.
              QInew=0.
              ASNOWnew=0.
            ELSE
      !
      !--- Update rime factor if appropriate
      !
              DUM=PIACWI+PIACR
              IF (DUM.LE.EPSQ .AND. PIDEP.LE.EPSQ) THEN
                RimeF=RimeF1
              ELSE
         !
         !--- Rime Factor, RimeF = (Total ice mass)/(Total unrimed ice mass)
         !      DUM1 - Total ice mass, rimed & unrimed
         !      DUM2 - Estimated mass of *unrimed* ice
         !
                DUM1=TOT_ICE+THICK*(PIDEP+DUM)
                DUM2=TOT_ICE/RimeF1+THICK*PIDEP
                IF (DUM2 .LE. 0.) THEN
                  RimeF=RFmax
                ELSE
                  RimeF=MIN(RFmax, MAX(1., DUM1/DUM2) )
                ENDIF
              ENDIF       ! End IF (DUM.LE.EPSQ .AND. PIDEP.LE.EPSQ)
              QInew=TOT_ICEnew/(THICK+BLDTRH*(FLIMASS*VSNOW             &
     &                          +(1.-FLIMASS)*VCI) )
              IF (QInew .LE. EPSQ) QInew=0.
              IF (QI.GT.0. .AND. QInew.NE.0.) THEN
                DUM=QInew/QI
                IF (DUM .LT. TOLER) QInew=0.
              ENDIF
              ASNOWnew=QInew*BLDTRH*(FLIMASS*VSNOW+(1.-FLIMASS)*VCI)
              IF (ASNOW.GT.0. .AND. ASNOWnew.NE.0.) THEN
                DUM=ASNOWnew/ASNOW
                IF (DUM .LT. TOLER) ASNOWnew=0.
              ENDIF
            ENDIF         ! End IF (TOT_ICEnew .LE. CLIMIT)
          ENDIF           ! End IF (ICE_logical)

!
!--- Update rain mixing ratios
!
!---
! * TOT_RAINnew - total mass of rain after microphysics
!                 current layer and the input flux of ice from above
! * VRAIN2      - time-averaged fall speed of rain in grid and below 
!                 (with air resistance correction)
! * QRdum       - first-guess estimate (dummy) rain mixing ratio in layer
!                 (uses old rain fall speed estimate, VRAIN1)
! * QRnew       - updated rain mixing ratio in layer
!      -> TOT_RAINnew=QRnew*(THICK+BLDTRH*VRAIN2)
!  * ARAINnew  - updated accumulation of rain at bottom of grid cell
!---
!
          DELR=PRAUT+PRACW+PIACWR-PIACR+PIMLT+PREVP+PICND
          TOT_RAINnew=TOT_RAIN+THICK*DELR
          IF (TOT_RAIN.GT.0. .AND. TOT_RAINnew.NE.0.) THEN
            DUM=TOT_RAINnew/TOT_RAIN
            IF (DUM .LT. TOLER) TOT_RAINnew=0.
          ENDIF
          IF (TOT_RAINnew .LE. CLIMIT) THEN
            TOT_RAINnew=0.
            VRAIN2=0.
            QRnew=0.
            ARAINnew=0.
          ELSE
   !
   !--- 1st guess, time-averaged rain mixing ratio and rain rate
   !    normalized to a density of 1.194 kg/m**3
   !
            QRdum=TOT_RAINnew/(THICK+BLDTRH*VRAIN1)
   !
   !--- 1st-guess estimate of rainfall through the bottom of the box
   !
            DUM=BLDTRH*VRAIN1*QRdum
   !
   !--- 1st-guess estimate of normalized rain rate
   !
            RR=DUM/(DTPH*GAMMAR)
   !
   !--- Use same algorithm as above for calculating mean drop diameter
   !      (IDR, in microns), which is used to estimate the time-averaged
   !      fall speed of rain drops at the bottom of the grid layer.
   !--- Integer function to estimate the mean size of the raindrops in microns
   !
            IDR=GET_INDEXR(RR)
            VRAIN2=GAMMAR*VRAIN(IDR)
!!            VRAIN2=.5*(VRAIN1+GAMMAR*VRAIN(IDR))   !-- time-averaged estimate
            QRnew=TOT_RAINnew/(THICK+BLDTRH*VRAIN2)
            IF (QRnew .LE. EPSQ) QRnew=0.
            IF (QR.GT.0. .AND. QRnew.NE.0.) THEN
              DUM=QRnew/QR
              IF (DUM .LT. TOLER) QRnew=0.
            ENDIF
            ARAINnew=BLDTRH*VRAIN2*QRnew
            IF (ARAIN.GT.0. .AND. ARAINnew.NE.0.) THEN
              DUM=ARAINnew/ARAIN
              IF (DUM .LT. TOLER) ARAINnew=0.
            ENDIF
          ENDIF
!
          WCnew=QWnew+QRnew+QInew
!
!----------------------------------------------------------------------
!-------------- Begin debugging & verification ------------------------
!----------------------------------------------------------------------
!
!--- QT, QTnew - total water (vapor & condensate) before & after microphysics, resp.
!


          QT=THICK*(WV+WC)+ARAIN+ASNOW
          QTnew=THICK*(WVnew+WCnew)+ARAINnew+ASNOWnew
          BUDGET=QT-QTnew
!
!--- Additional check on budget preservation, accounting for truncation effects
!
          DBG_logical=.FALSE.
!          DUM=ABS(BUDGET)
!          IF (DUM .GT. TOLER) THEN
!            DUM=DUM/MIN(QT, QTnew)
!            IF (DUM .GT. TOLER) DBG_logical=.TRUE.
!          ENDIF
!!
!          DUM=(RHgrd+.001)*QSInew
!          IF ( (QWnew.GT.EPSQ) .OR. QRnew.GT.EPSQ .OR. WVnew.GT.DUM)
!     &        .AND. TC.LT.T_ICE )  DBG_logical=.TRUE.
!
!          IF (TC.GT.5. .AND. QInew.GT.EPSQ) DBG_logical=.TRUE.
!
          IF ((WVnew.LT.EPSQ .OR. DBG_logical) .AND. PRINT_diag) THEN
   !
            WRITE(6,"(/2(a,i4),2(a,i2))") '{} i=',I_index,' j=',J_index,&
     &                                    ' L=',L,' LSFC=',LSFC
   !
            ESW=MIN(1000.*FPVS0(Tnew),0.99*PP)
            QSWnew=EPS*ESW/(PP-ESW)
            IF (TC.LT.0. .OR. Tnew .LT. 0.) THEN
              ESI=MIN(1000.*FPVS(Tnew),0.99*PP)
              QSInew=EPS*ESI/(PP-ESI)
            ELSE
              QSI=QSW
              QSInew=QSWnew
            ENDIF
            WSnew=QSInew
            WRITE(6,"(4(a12,g11.4,1x))")                                   &
     & '{} TCold=',TC,'TCnew=',Tnew-T0C,'P=',.01*PP,'RHO=',RHO,            &
     & '{} THICK=',THICK,'RHold=',WV/WS,'RHnew=',WVnew/WSnew,              &
     &   'RHgrd=',RHgrd,                                                   &
     & '{} RHWold=',WV/QSW,'RHWnew=',WVnew/QSWnew,'RHIold=',WV/QSI,        &
     &   'RHInew=',WVnew/QSInew,                                           &
     & '{} QSWold=',QSW,'QSWnew=',QSWnew,'QSIold=',QSI,'QSInew=',QSInew,   &
     & '{} WSold=',WS,'WSnew=',WSnew,'WVold=',WV,'WVnew=',WVnew,           &
     & '{} WCold=',WC,'WCnew=',WCnew,'QWold=',QW,'QWnew=',QWnew,           &
     & '{} QIold=',QI,'QInew=',QInew,'QRold=',QR,'QRnew=',QRnew,           &
     & '{} ARAINold=',ARAIN,'ARAINnew=',ARAINnew,'ASNOWold=',ASNOW,        &
     &   'ASNOWnew=',ASNOWnew,                                             &
     & '{} TOT_RAIN=',TOT_RAIN,'TOT_RAINnew=',TOT_RAINnew,                 &
     &   'TOT_ICE=',TOT_ICE,'TOT_ICEnew=',TOT_ICEnew,                      &
     & '{} BUDGET=',BUDGET,'QTold=',QT,'QTnew=',QTnew                       
   !
            WRITE(6,"(4(a12,g11.4,1x))")                                   &
     & '{} DELT=',DELT,'DELV=',DELV,'DELW=',DELW,'DELI=',DELI,             &
     & '{} DELR=',DELR,'PCOND=',PCOND,'PIDEP=',PIDEP,'PIEVP=',PIEVP,       &
     & '{} PICND=',PICND,'PREVP=',PREVP,'PRAUT=',PRAUT,'PRACW=',PRACW,     &
     & '{} PIACW=',PIACW,'PIACWI=',PIACWI,'PIACWR=',PIACWR,'PIMLT=',       &
     &    PIMLT,                                                           &
     & '{} PIACR=',PIACR                                                    
   !
            IF (ICE_logical) WRITE(6,"(4(a12,g11.4,1x))")                  &
     & '{} RimeF1=',RimeF1,'GAMMAS=',GAMMAS,'VrimeF=',VrimeF,              &
     &   'VSNOW=',VSNOW,                                                   &
     & '{} INDEXS=',FLOAT(INDEXS),'FLARGE=',FLARGE,'FSMALL=',FSMALL,       &
     &   'FLIMASS=',FLIMASS,                                               &
     & '{} XSIMASS=',XSIMASS,'XLIMASS=',XLIMASS,'QLICE=',QLICE,            &
     &   'QTICE=',QTICE,                                                   &
     & '{} NLICE=',NLICE,'NSmICE=',NSmICE,'PILOSS=',PILOSS,                &
     &   'EMAIRI=',EMAIRI,                                                 &
     & '{} RimeF=',RimeF,'VCI=',VCI
   !
            IF (TOT_RAIN.GT.0. .OR. TOT_RAINnew.GT.0.)                     &
     &        WRITE(6,"(4(a12,g11.4,1x))")                                 &
     & '{} INDEXR1=',FLOAT(INDEXR1),'INDEXR=',FLOAT(INDEXR),               &
     &   'GAMMAR=',GAMMAR,'N0r=',N0r,                                      &
     & '{} VRAIN1=',VRAIN1,'VRAIN2=',VRAIN2,'QTRAIN=',QTRAIN,'RQR=',RQR,   &
     & '{} PRLOSS=',PRLOSS,'VOLR1=',THICK+BLDTRH*VRAIN1,                   &
     &   'VOLR2=',THICK+BLDTRH*VRAIN2
   !
            IF (PRACW .GT. 0.) WRITE(6,"(a12,g11.4,1x)") '{} FWR=',FWR
   !
            IF (PIACR .GT. 0.) WRITE(6,"(a12,g11.4,1x)") '{} FIR=',FIR
   !
            DUM=PIMLT+PICND-PREVP-PIEVP
            IF (DUM.GT.0. .or. DWVi.NE.0.)                                 &
     &        WRITE(6,"(4(a12,g11.4,1x))")                                 &
     & '{} TFACTOR=',TFACTOR,'DYNVIS=',DYNVIS,                             &
     &   'THERM_CON=',THERM_COND,'DIFFUS=',DIFFUS
   !
            IF (PREVP .LT. 0.) WRITE(6,"(4(a12,g11.4,1x))")                &
     & '{} RFACTOR=',RFACTOR,'ABW=',ABW,'VENTR=',VENTR,'CREVP=',CREVP,     &
     & '{} DWVr=',DWVr,'DENOMW=',DENOMW
   !
            IF (PIDEP.NE.0. .AND. DWVi.NE.0.)                              &
     &        WRITE(6,"(4(a12,g11.4,1x))")                                 &
     & '{} DWVi=',DWVi,'DENOMI=',DENOMI,'PIDEP_max=',PIDEP_max,            &
     &   'SFACTOR=',SFACTOR,                                               &
     & '{} ABI=',ABI,'VENTIL=',VENTIL,'VENTIL1=',VENTI1(INDEXS),           &
     &   'VENTIL2=',SFACTOR*VENTI2(INDEXS),                                &
     & '{} VENTIS=',VENTIS,'DIDEP=',DIDEP
   !
            IF (PIDEP.GT.0. .AND. PCOND.NE.0.)                             &
     &        WRITE(6,"(4(a12,g11.4,1x))")                                 &
     & '{} DENOMW=',DENOMW,'DENOMWI=',DENOMWI,'DENOMF=',DENOMF,            &
     &    'DUM2=',PCOND-PIACW
   !
            IF (FWS .GT. 0.) WRITE(6,"(4(a12,g11.4,1x))")                  &
     & '{} FWS=',FWS                     
   !
            DUM=PIMLT+PICND-PIEVP
            IF (DUM.GT. 0.) WRITE(6,"(4(a12,g11.4,1x))")                   &
     & '{} SFACTOR=',SFACTOR,'VENTIL=',VENTIL,'VENTIL1=',VENTI1(INDEXS),   &
     &   'VENTIL2=',SFACTOR*VENTI2(INDEXS),                                &
     & '{} AIEVP=',AIEVP,'DIEVP=',DIEVP,'QSW0=',QSW0,'DWV0=',DWV0       
   !
          ENDIF


!
!-----------------------------------------------------------------------
!--------------- Water budget statistics & maximum values --------------
!-----------------------------------------------------------------------
!
          IF (PRINT_diag) THEN
            ITdx=MAX( ITLO, MIN( INT(Tnew-T0C), ITHI ) )
            IF (QInew .GT. EPSQ) NSTATS(ITdx,1)=NSTATS(ITdx,1)+1
            IF (QInew.GT.EPSQ  .AND.  QRnew+QWnew.GT.EPSQ)              &
     &        NSTATS(ITdx,2)=NSTATS(ITdx,2)+1
            IF (QWnew .GT. EPSQ) NSTATS(ITdx,3)=NSTATS(ITdx,3)+1 
            IF (QRnew .GT. EPSQ) NSTATS(ITdx,4)=NSTATS(ITdx,4)+1
  !
            QMAX(ITdx,1)=MAX(QMAX(ITdx,1), QInew)
            QMAX(ITdx,2)=MAX(QMAX(ITdx,2), QWnew)
            QMAX(ITdx,3)=MAX(QMAX(ITdx,3), QRnew)
            QMAX(ITdx,4)=MAX(QMAX(ITdx,4), ASNOWnew)
            QMAX(ITdx,5)=MAX(QMAX(ITdx,5), ARAINnew)
            QTOT(ITdx,1)=QTOT(ITdx,1)+QInew*THICK
            QTOT(ITdx,2)=QTOT(ITdx,2)+QWnew*THICK
            QTOT(ITdx,3)=QTOT(ITdx,3)+QRnew*THICK
  !
            QTOT(ITdx,4)=QTOT(ITdx,4)+PCOND*THICK
            QTOT(ITdx,5)=QTOT(ITdx,5)+PICND*THICK
            QTOT(ITdx,6)=QTOT(ITdx,6)+PIEVP*THICK
            QTOT(ITdx,7)=QTOT(ITdx,7)+PIDEP*THICK
            QTOT(ITdx,8)=QTOT(ITdx,8)+PREVP*THICK
            QTOT(ITdx,9)=QTOT(ITdx,9)+PRAUT*THICK
            QTOT(ITdx,10)=QTOT(ITdx,10)+PRACW*THICK
            QTOT(ITdx,11)=QTOT(ITdx,11)+PIMLT*THICK
            QTOT(ITdx,12)=QTOT(ITdx,12)+PIACW*THICK
            QTOT(ITdx,13)=QTOT(ITdx,13)+PIACWI*THICK
            QTOT(ITdx,14)=QTOT(ITdx,14)+PIACWR*THICK
            QTOT(ITdx,15)=QTOT(ITdx,15)+PIACR*THICK
  !
            QTOT(ITdx,16)=QTOT(ITdx,16)+(WVnew-WV)*THICK
            QTOT(ITdx,17)=QTOT(ITdx,17)+(QWnew-QW)*THICK
            QTOT(ITdx,18)=QTOT(ITdx,18)+(QInew-QI)*THICK
            QTOT(ITdx,19)=QTOT(ITdx,19)+(QRnew-QR)*THICK
            QTOT(ITdx,20)=QTOT(ITdx,20)+(ARAINnew-ARAIN)
            QTOT(ITdx,21)=QTOT(ITdx,21)+(ASNOWnew-ASNOW)
            IF (QInew .GT. 0.)                                          &
     &        QTOT(ITdx,22)=QTOT(ITdx,22)+QInew*THICK/RimeF
  !
          ENDIF
!
!----------------------------------------------------------------------
!------------------------- Update arrays ------------------------------
!----------------------------------------------------------------------
!


          T_col(L)=Tnew                           ! Updated temperature
!
          QV_col(L)=max(EPSQ, WVnew/(1.+WVnew))   ! Updated specific humidity
          WC_col(L)=max(EPSQ, WCnew)              ! Updated total condensate mixing ratio
          QI_col(L)=max(EPSQ, QInew)              ! Updated ice mixing ratio
          QR_col(L)=max(EPSQ, QRnew)              ! Updated rain mixing ratio
          QW_col(L)=max(EPSQ, QWnew)              ! Updated cloud water mixing ratio
          RimeF_col(L)=RimeF                      ! Updated rime factor
          ASNOW=ASNOWnew                          ! Updated accumulated snow
          ARAIN=ARAINnew                          ! Updated accumulated rain
! assign microphysical processes and fall speeds to 1D array

      if(pcond.gt.0)then
        pcond1d(L)=pcond
      elseif(pcond.lt.0)then
        pevap1d(L)=pcond
      endif
      if(pidep.gt.0)then
        pidep1d(L)=pidep
      elseif(pidep.lt.0)then
        pisub1d(L)=pidep
      endif
      piacw1d(L)=piacw
      piacwi1d(L)=piacwi
      piacwr1d(L)=piacwr
      piacr1d(L)=piacr
      picnd1d(L)=picnd
      pievp1d(L)=pievp
      pimlt1d(L)=pimlt
      praut1d(L)=praut
      pracw1d(L)=pracw
      prevp1d(L)=prevp
!
!#######################################################################
!
10      CONTINUE         ! ##### End "L" loop through model levels #####


!
!#######################################################################
!
!-----------------------------------------------------------------------
!--------------------------- Return to GSMDRIVE -----------------------
!-----------------------------------------------------------------------
!
        CONTAINS
!#######################################################################
!--------- Produces accurate calculation of cloud condensation ---------
!#######################################################################
!
      REAL FUNCTION CONDENSE (PP,QW,TK,WV,I,J,L)    !-- Debug 20120111
!
!---------------------------------------------------------------------------------
!------   The Asai (1965) algorithm takes into consideration the release of ------
!------   latent heat in increasing the temperature & in increasing the     ------
!------   saturation mixing ratio (following the Clausius-Clapeyron eqn.).  ------
!---------------------------------------------------------------------------------
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: HIGH_PRES=Selected_Real_Kind(15)
      REAL (KIND=HIGH_PRES), PARAMETER ::                               &
     & RHLIMIT=.001, RHLIMIT1=-RHLIMIT
      REAL (KIND=HIGH_PRES) :: COND, SSAT, WCdum
!
      REAL,INTENT(IN) :: QW,PP,WV,TK
      REAL WVdum,Tdum,XLV2,DWV,WS,ESW,XLV1,XLV

integer,INTENT(IN) :: I,J,L     !-- Debug 20120111
integer :: niter
real :: DWVinp,SSATinp

!
!-----------------------------------------------------------------------
!
!--- LV (T) is from Bolton (JAS, 1980)
!
      XLV=3.148E6-2370.*TK
      XLV1=XLV*RCP
      XLV2=XLV*XLV*RCPRV
      Tdum=TK
      WVdum=WV
      WCdum=QW
      ESW=MIN(1000.*FPVS0(Tdum),0.99*PP)        ! Saturation vapor press w/r/t water
      WS=RHgrd*EPS*ESW/(PP-ESW)                 ! Saturation mixing ratio w/r/t water
      DWV=WVdum-WS                              ! Deficit grid-scale water vapor mixing ratio
      SSAT=DWV/WS                               ! Supersaturation ratio
      CONDENSE=0.

DWVinp=DWV     !-- Debug 20120111
SSATinp=SSAT

      DO NITER=1,MAX_ITERATIONS
        COND=DWV/(1.+XLV2*WS/(Tdum*Tdum))       ! Asai (1965, J. Japan)
        COND=MAX(COND, -WCdum)                  ! Limit cloud water evaporation
        Tdum=Tdum+XLV1*COND                     ! Updated temperature
        WVdum=WVdum-COND                        ! Updated water vapor mixing ratio
        WCdum=WCdum+COND                        ! Updated cloud water mixing ratio
        CONDENSE=CONDENSE+COND                  ! Total cloud water condensation
        ESW=MIN(1000.*FPVS0(Tdum),0.99*PP)      ! Updated saturation vapor press w/r/t water
        WS=RHgrd*EPS*ESW/(PP-ESW)               ! Updated saturation mixing ratio w/r/t water
        DWV=WVdum-WS                            ! Deficit grid-scale water vapor mixing ratio
        SSAT=DWV/WS                             ! Grid-scale supersaturation ratio
        IF (SSAT>=RHLIMIT1 .AND. SSAT<=RHLIMIT) EXIT   !-- Exit if SSAT is near 0
        IF (SSAT<RHLIMIT1 .AND. WCdum<=EPSQ) EXIT      !-- Exit if SSAT<0 & no cloud water
      ENDDO

!-- Debug 20120111
IF (NITER>MAX_ITERATIONS) THEN
!-- Too many iterations - indicates possible numerical instability
   IF (WARN3) THEN
      write(0,*) 'WARN3: Too many iterations in function CONDENSE; ', &
         ' I,J,L,TC,SSAT,QW,DWV=',I,J,L,TK-T0C,SSATinp,1000.*QW,DWVinp
      WARN3=.FALSE.
   ENDIF
   SSAT=0.
   CONDENSE=DWVinp
ENDIF

!
      END FUNCTION CONDENSE
!
!#######################################################################
!---------------- Calculate ice deposition at T<T_ICE ------------------
!#######################################################################
!
      REAL FUNCTION DEPOSIT (PP,Tdum,WVdum,I,J,L)   !-- Debug 20120111
!
!--- Also uses the Asai (1965) algorithm, but uses a different target
!      vapor pressure for the adjustment
!
      IMPLICIT NONE      
!
      INTEGER, PARAMETER :: HIGH_PRES=Selected_Real_Kind(15)
      REAL (KIND=HIGH_PRES), PARAMETER :: RHLIMIT=.001,                 &
     & RHLIMIT1=-RHLIMIT
      REAL (KIND=HIGH_PRES) :: DEP, SSAT
!    
      real,INTENT(IN) ::  PP
      real,INTENT(INOUT) ::  WVdum,Tdum
      real ESI,WS,DWV

integer,INTENT(IN) :: I,J,L   !-- Debug 20120111
integer :: niter
real :: Tinp,DWVinp,SSATinp

!
!-----------------------------------------------------------------------
!
      ESI=MIN(1000.*FPVS(Tdum),0.99*PP)         ! Saturation vapor press w/r/t ice
      WS=RHgrd*EPS*ESI/(PP-ESI)                 ! Saturation mixing ratio
      DWV=WVdum-WS                              ! Deficit grid-scale water vapor mixing ratio
      SSAT=DWV/WS                               ! Supersaturation ratio
      DEPOSIT=0.

Tinp=Tdum     !-- Debug 20120111
DWVinp=DWV
SSATinp=SSAT

      DO NITER=1,MAX_ITERATIONS
   !
   !--- Note that XLVS2=LS*LV/(CP*RV)=LV*WS/(RV*T*T)*(LS/CP*DEP1), 
   !     where WS is the saturation mixing ratio following Clausius-
   !     Clapeyron (see Asai,1965; Young,1993,p.405) 
   !
        DEP=DWV/(1.+XLS2*WS/(Tdum*Tdum))        ! Asai (1965, J. Japan)
        Tdum=Tdum+XLS1*DEP                      ! Updated temperature
        WVdum=WVdum-DEP                         ! Updated ice mixing ratio
        DEPOSIT=DEPOSIT+DEP                     ! Total ice deposition
        ESI=MIN(1000.*FPVS(Tdum),0.99*PP)       ! Updated saturation vapor press w/r/t ice
        WS=RHgrd*EPS*ESI/(PP-ESI)               ! Updated saturation mixing ratio w/r/t ice
        DWV=WVdum-WS                            ! Deficit grid-scale water vapor mixing ratio
        SSAT=DWV/WS                             ! Grid-scale supersaturation ratio
        IF (SSAT>=RHLIMIT1 .AND. SSAT<=RHLIMIT) EXIT   !-- Exit if SSAT is near 0
      ENDDO

!-- Debug 20120111
IF (NITER>MAX_ITERATIONS) THEN
!-- Too many iterations - indicates possible numerical instability
   IF (WARN2) THEN
      write(0,*) 'WARN2: Too many iterations in function DEPOSIT; ', &
         ' I,J,L,TC,SSAT,DWV=',I,J,L,Tinp-T0C,SSATinp,DWVinp
      WARN2=.FALSE.
   ENDIF
   SSAT=0.
   DEPOSIT=DWVinp
ENDIF

!
      END FUNCTION DEPOSIT
!
!#######################################################################
!--- Used to calculate the mean size of rain drops (INDEXR) in microns
!#######################################################################
!
      INTEGER FUNCTION GET_INDEXR(RR)
      IMPLICIT NONE
      REAL, INTENT(IN) :: RR
      IF (RR .LE. RR_DRmin) THEN
!
!--- Assume fixed mean diameter of rain (0.2 mm) for low rain rates, 
!      instead vary N0r with rain rate
!
        GET_INDEXR=MDRmin
      ELSE IF (RR .LE. RR_DR1) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
!      for mean diameters (Dr) between 0.05 and 0.10 mm:
!      V(Dr)=5.6023e4*Dr**1.136, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*5.6023e4*Dr**(4+1.136) = 1.408e15*Dr**5.136,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.123e-3*RR**.1947 -> Dr (microns) = 1.123e3*RR**.1947
!
        GET_INDEXR=INT( 1.123E3*RR**.1947 + .5 )
        GET_INDEXR=MAX( MDRmin, MIN(GET_INDEXR, MDR1) )
      ELSE IF (RR .LE. RR_DR2) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
!      for mean diameters (Dr) between 0.10 and 0.20 mm:
!      V(Dr)=1.0867e4*Dr**.958, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*1.0867e4*Dr**(4+.958) = 2.731e14*Dr**4.958,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.225e-3*RR**.2017 -> Dr (microns) = 1.225e3*RR**.2017
!
        GET_INDEXR=INT( 1.225E3*RR**.2017 + .5 )
        GET_INDEXR=MAX( MDR1, MIN(GET_INDEXR, MDR2) )
      ELSE IF (RR .LE. RR_DR3) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
!      for mean diameters (Dr) between 0.20 and 0.32 mm:
!      V(Dr)=2831.*Dr**.80, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*2831.*Dr**(4+.80) = 7.115e13*Dr**4.80, 
!        RR in kg/(m**2*s)
!      Dr (m) = 1.3006e-3*RR**.2083 -> Dr (microns) = 1.3006e3*RR**.2083
!
        GET_INDEXR=INT( 1.3006E3*RR**.2083 + .5 )
        GET_INDEXR=MAX( MDR2, MIN(GET_INDEXR, MDR3) )
      ELSE IF (RR .LE. RR_DR4) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables
!      for mean diameters (Dr) between 0.32 and 0.45 mm:
!      V(Dr)=963.0*Dr**.666, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*963.0*Dr**(4+.666) = 2.4205e13*Dr**4.666,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.354e-3*RR**.2143 -> Dr (microns) = 1.354e3*RR**.2143
!
        GET_INDEXR=INT( 1.354E3*RR**.2143 + .5 )
        GET_INDEXR=MAX( MDR3, MIN(GET_INDEXR, MDR4) )
      ELSE IF (RR .LE. RR_DR5) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables
!      for mean diameters (Dr) between 0.45 and 0.675 mm:
!      V(Dr)=309.0*Dr**.5185, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*309.0*Dr**(4+.5185) = 7.766e12*Dr**4.5185,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.404e-3*RR**.2213 -> Dr (microns) = 1.404e3*RR**.2213
!
        GET_INDEXR=INT( 1.404E3*RR**.2213 + .5 )
        GET_INDEXR=MAX( MDR4, MIN(GET_INDEXR, MDR5) )
      ELSE IF (RR .LE. RR_DRmax) THEN
!
!--- Best fit to mass-weighted fall speeds (V) from rain lookup tables
!      for mean diameters (Dr) between 0.675 and 1.0 mm:
!      V(Dr)=85.81Dr**.343, V in m/s and Dr in m
!      RR = PI*1000.*N0r0*85.81*Dr**(4+.343) = 2.1566e12*Dr**4.343,
!        RR in kg/(m**2*s)
!      Dr (m) = 1.4457e-3*RR**.2303 -> Dr (microns) = 1.4457e3*RR**.2303
!
        GET_INDEXR=INT( 1.4457E3*RR**.2303 + .5 )
        GET_INDEXR=MAX( MDR5, MIN(GET_INDEXR, MDRmax) )
      ELSE 
!
!--- Assume fixed mean diameter of rain (1.0 mm) for high rain rates, 
!      instead vary N0r with rain rate
!
        GET_INDEXR=MDRmax
      ENDIF              ! End IF (RR .LE. RR_DRmin) etc. 
!
      END FUNCTION GET_INDEXR
!
      END SUBROUTINE EGCP01COLUMN_hr 
!#######################################################################
!------- Initialize constants & lookup tables for microphysics ---------
!#######################################################################
!

! SH 0211/2002

!-----------------------------------------------------------------------
      SUBROUTINE FERRIER_INIT_hr (GSMDT,DT,DELX,DELY,restart,           &
     &   F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,                              &
     &   MP_RESTART_STATE,TBPVS_STATE,TBPVS0_STATE,                     &
     &   ALLOWED_TO_READ,                                               &
     &   IDS,IDE,JDS,JDE,KDS,KDE,                                       &
     &   IMS,IME,JMS,JME,KMS,KME,                                       &
     &   ITS,ITE,JTS,JTE,KTS,KTE,                                       &
     &   MPI_COMM_COMP,MYPE,MASSRout,MASSIout)
!-----------------------------------------------------------------------
!-------------------------------------------------------------------------------
!---  SUBPROGRAM DOCUMENTATION BLOCK
!   PRGRMMR: Ferrier         ORG: W/NP22     DATE: February 2001
!            Jin             ORG: W/NP22     DATE: January 2002 
!        (Modification for WRF structure)
!                                               
!-------------------------------------------------------------------------------
! ABSTRACT:
!   * Reads various microphysical lookup tables used in COLUMN_MICRO
!   * Lookup tables were created "offline" and are read in during execution
!   * Creates lookup tables for saturation vapor pressure w/r/t water & ice
!-------------------------------------------------------------------------------
!     
! USAGE: CALL FERRIER_INIT_hr FROM SUBROUTINE PHYSICS_INITIALIZE
!
!   INPUT ARGUMENT LIST:
!       DTPH - physics time step (s)
!  
!   OUTPUT ARGUMENT LIST: 
!     NONE
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBROUTINES:
!     MY_GROWTH_RATES_NMM_hr - lookup table for growth of nucleated ice
!     GPVS_hr            - lookup tables for saturation vapor pressure (water, ice)
!
!   UNIQUE: NONE
!  
!   LIBRARY: NONE
!  
!   COMMON BLOCKS:
!     CMICRO_CONS - constants used in GSMCOLUMN
!     CMY600       - lookup table for growth of ice crystals in 
!                    water saturated conditions (Miller & Young, 1979)
!     IVENT_TABLES - lookup tables for ventilation effects of ice
!     IACCR_TABLES - lookup tables for accretion rates of ice
!     IMASS_TABLES - lookup tables for mass content of ice
!     IRATE_TABLES - lookup tables for precipitation rates of ice
!     IRIME_TABLES - lookup tables for increase in fall speed of rimed ice
!     MAPOT        - Need lat/lon grid resolution
!     RVENT_TABLES - lookup tables for ventilation effects of rain
!     RACCR_TABLES - lookup tables for accretion rates of rain
!     RMASS_TABLES - lookup tables for mass content of rain
!     RVELR_TABLES - lookup tables for fall speeds of rain
!     RRATE_TABLES - lookup tables for precipitation rates of rain
!   
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!------------------------------------------------------------------------- 
!-------------- Parameters & arrays for lookup tables -------------------- 
!------------------------------------------------------------------------- 
!
!--- Common block of constants used in column microphysics
!
!WRF
!     real DLMD,DPHD
!WRF
!
!-----------------------------------------------------------------------
!--- Parameters & data statement for local calculations
!-----------------------------------------------------------------------
!
      INTEGER, PARAMETER :: MDR1=XMR1, MDR2=XMR2, MDR3=XMR3
!
!     VARIABLES PASSED IN
      integer,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     & 
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE       
!WRF
!!!!   INTEGER, DIMENSION(ims:ime,jms:jme),INTENT(INOUT) :: LOWLYR
!
      real, INTENT(IN) ::  DELX,DELY
      real,DIMENSION(*), INTENT(INOUT) :: MP_RESTART_STATE
      real,DIMENSION(NX), INTENT(INOUT) :: TBPVS_STATE,TBPVS0_STATE
      real,DIMENSION(ims:ime, jms:jme, kms:kme),INTENT(OUT) ::          &
     &  F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
      real,DIMENSION(MDRmin:MDRmax) :: MASSRout 
      real,DIMENSION(MDImin:MDImax) :: MASSIout 
      INTEGER, PARAMETER :: ITLO=-60, ITHI=40
!     integer,DIMENSION(ITLO:ITHI,4),INTENT(INOUT) :: NSTATS
!     real,DIMENSION(ITLO:ITHI,5),INTENT(INOUT) :: QMAX
!     real,DIMENSION(ITLO:ITHI,22),INTENT(INOUT) :: QTOT
!     real,INTENT(INOUT) :: PRECtot(2),PRECmax(2)
      real,INTENT(IN) :: DT,GSMDT
      LOGICAL,INTENT(IN) :: allowed_to_read,restart
      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE
!
!-----------------------------------------------------------------------
!     LOCAL VARIABLES
!-----------------------------------------------------------------------
      REAL :: BBFR,DTPH,PI,DX,Thour_print,RDIS,BETA6
      INTEGER :: I,IM,J,L,K,JTF,KTF,ITF
      INTEGER :: etampnew_unit1
      LOGICAL, PARAMETER :: PRINT_diag=.FALSE.
      LOGICAL :: opened
      INTEGER :: IRTN
      CHARACTER*80 errmess
!
!-----------------------------------------------------------------------
!
      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)
!
!     DO J=JTS,JTF
!     DO I=ITS,ITF
!       LOWLYR(I,J)=1
!     ENDDO
!     ENDDO
!    
      IF(.NOT.RESTART)THEN
        DO K = kts,kte
        DO J = jts,jte
        DO I= its,ite
          F_ICE_PHY(i,j,k)=0.
          F_RAIN_PHY(i,j,k)=0.
          F_RIMEF_PHY(i,j,k)=1.
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!    
!-----------------------------------------------------------------------
      IF(ALLOWED_TO_READ)THEN
!-----------------------------------------------------------------------
!
        DX=SQRT(DELX*DELX+DELY*DELY)/1000.    ! Model resolution at equator (km)
        DX=MIN(100., MAX(5., DX) )
!
!-- Relative humidity threshold for the onset of grid-scale condensation
!!-- 9/1/01:  Assume the following functional dependence for 5 - 100 km resolution:
!!       RHgrd=0.90 for dx=100 km, 0.98 for dx=5 km, where
!        RHgrd=0.90+.08*SQRT((100.-DX)/95.)
!
!-- Old WRF and Eta code when input physics time step (GSMDT) was in minutes:
!wrf        DTPH=MAX(GSMDT*60.,DT)
!wrf        DTPH=NINT(DTPH/DT)*DT
        DTPH=GSMDT     !-- Time step in s
!
!--- Create lookup tables for saturation vapor pressure w/r/t water & ice
!
        CALL GPVS_hr
!
!--- Read in various lookup tables
!
        IF(MYPE==0)THEN
          DO i = 31,99
            INQUIRE ( i , OPENED = opened )
            IF ( .NOT. opened ) THEN
              etampnew_unit1 = i
              GOTO 2061
            ENDIF
          ENDDO
          etampnew_unit1 = -1
 2061     CONTINUE
        ENDIF
!
        CALL MPI_BCAST(ETAMPNEW_UNIT1,1,MPI_INTEGER,0  &
                      ,MPI_COMM_COMP,IRTN)
!
        IF ( etampnew_unit1 < 0 ) THEN
          WRITE(0,*)'ferrier_init_hr: Can not find unused fortran unit to read in lookup table.'
          STOP
        ENDIF
!
        IF(MYPE==0)THEN
!!was     OPEN (UNIT=1,FILE="eta_micro_lookup.dat",FORM="UNFORMATTED")
          OPEN(UNIT=etampnew_unit1,FILE="ETAMPNEW_DATA.expanded_rain",  &
     &        FORM="UNFORMATTED",STATUS="OLD",ERR=9061)
!
          READ(etampnew_unit1) VENTR1
          READ(etampnew_unit1) VENTR2
          READ(etampnew_unit1) ACCRR
          READ(etampnew_unit1) MASSR
          READ(etampnew_unit1) VRAIN
          READ(etampnew_unit1) RRATE
          READ(etampnew_unit1) VENTI1
          READ(etampnew_unit1) VENTI2
          READ(etampnew_unit1) ACCRI
          READ(etampnew_unit1) MASSI
          READ(etampnew_unit1) VSNOWI
          READ(etampnew_unit1) VEL_RF
!        read(etampnew_unit1) my_growth    ! Applicable only for DTPH=180 s
          CLOSE (etampnew_unit1)
        ENDIF
          MASSRout=MASSR
          MASSIout=MASSI
!
        CALL MPI_BCAST(VENTR1,SIZE(VENTR1),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VENTR2,SIZE(VENTR2),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(ACCRR,SIZE(ACCRR),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(MASSR,SIZE(MASSR),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VRAIN,SIZE(VRAIN),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(RRATE,SIZE(RRATE),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VENTI1,SIZE(VENTI1),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VENTI2,SIZE(VENTI2),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(ACCRI,SIZE(ACCRI),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(MASSI,SIZE(MASSI),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VSNOWI,SIZE(VSNOWI),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
        CALL MPI_BCAST(VEL_RF,SIZE(VEL_RF),MPI_REAL,0  &
                      ,MPI_COMM_COMP,IRTN)
!
!--- Calculates coefficients for growth rates of ice nucleated in water
!    saturated conditions, scaled by physics time step (lookup table)
!
        CALL MY_GROWTH_RATES_NMM_hr (DTPH)
!
        PI=ACOS(-1.)
!
!--- Constants associated with Biggs (1953) freezing of rain, as parameterized
!    following Lin et al. (JCAM, 1983) & Reisner et al. (1998, QJRMS).
!
        ABFR=-0.66
        BBFR=100.
        CBFR=20.*PI*PI*BBFR*RHOL*1.E-21
!
!--- CIACW is used in calculating riming rates
!      The assumed effective collection efficiency of cloud water rimed onto
!      ice is =0.5 below:
!
        CIACW=DTPH*0.25*PI*0.5*(1.E5)**C1
!
!--- CIACR is used in calculating freezing of rain colliding with large ice
!      The assumed collection efficiency is 1.0
!
        CIACR=PI*DTPH
!
!--- Based on rain lookup tables for mean diameters from 0.05 to 1.0 mm
!    * Four different functional relationships of mean drop diameter as 
!      a function of rain rate (RR), derived based on simple fits to 
!      mass-weighted fall speeds of rain as functions of mean diameter
!      from the lookup tables.  
!
        RR_DRmin=N0r0*RRATE(MDRmin)     ! RR for mean drop diameter of .05 mm
        RR_DR1=N0r0*RRATE(MDR1)         ! RR for mean drop diameter of .10 mm
        RR_DR2=N0r0*RRATE(MDR2)         ! RR for mean drop diameter of .20 mm
        RR_DR3=N0r0*RRATE(MDR3)         ! RR for mean drop diameter of .32 mm
        RR_DR4=N0r0*RRATE(MDR4)         ! RR for mean drop diameter of .45 mm
        RR_DR5=N0r0*RRATE(MDR5)         ! RR for mean drop diameter of .675 mm
        RR_DRmax=N0r0*RRATE(MDRmax)     ! RR for mean drop diameter of 1.0 mm
!
        RQR_DRmin=N0r0*MASSR(MDRmin)    ! Rain content for mean drop diameter of .05 mm
        RQR_DRmax=N0r0*MASSR(MDRmax)    ! Rain content for mean drop diameter of 1.0 mm
        C_N0r0=PI*RHOL*N0r0
        CN0r0=1.E6/SQRT(SQRT(C_N0r0))
        CN0r_DMRmin=1./(PI*RHOL*DMRmin*DMRmin*DMRmin*DMRmin)
        CN0r_DMRmax=1./(PI*RHOL*DMRmax*DMRmax*DMRmax*DMRmax)
!
!--- CRACW is used in calculating collection of cloud water by rain (an
!      assumed collection efficiency of 1.0)
!
        CRACW=DTPH*0.25*PI*1.0
!
        ESW0=1000.*FPVS0(T0C)     ! Saturation vapor pressure at 0C
        RFmax=1.1**Nrime          ! Maximum rime factor allowed
!
!------------------------------------------------------------------------
!--------------- Constants passed through argument list -----------------
!------------------------------------------------------------------------
!
!--- Important parameters for self collection (autoconversion) of 
!    cloud water to rain. 
!
!-- Relative dispersion == standard deviation of droplet spectrum / mean radius
!   (see pp 1542-1543, Liu & Daum, JAS, 2004)
        RDIS=0.5  !-- relative dispersion of droplet spectrum
        BETA6=( (1.+3.*RDIS*RDIS)*(1.+4.*RDIS*RDIS)*(1.+5.*RDIS*RDIS)/  &
     &         ((1.+RDIS*RDIS)*(1.+2.*RDIS*RDIS) ) )
!-- Kappa=1.1e10 g^-2 cm^3 s^-1 after eq. (8b) on p.1105 of Liu et al. (JAS, 2006)
!   => More extensive units conversion than can be described here to go from
!      eq. (13) in Liu et al. (JAS, 2006) to what's programmed below.  Note that
!      the units used throughout the paper are in cgs units!
!
        ARAUT=1.03e19/(NCW*SQRT(NCW))
        BRAUT=DTPH*1.1E10*BETA6/NCW
!
!--- QAUT0 is the *OLD* threshold cloud content for autoconversion to rain 
!      needed for droplets to reach a diameter of 20 microns (following
!      Manton and Cotton, 1977; Banta and Hanson, 1987, JCAM).  It's no longer
!      used in this version, but the value is passed into radiation in case
!      a ball park estimate is needed.
!--- QAUT0=1.2567, 0.8378, or 0.4189 g/m**3 for droplet number concentrations
!      of 300, 200, and 100 cm**-3, respectively
!
        QAUT0=PI*RHOL*NCW*(20.E-6)**3/6.     !-- legacy
!
!--- For calculating snow optical depths by considering bulk density of
!      snow based on emails from Q. Fu (6/27-28/01), where optical 
!      depth (T) = 1.5*SWP/(Reff*DENS), SWP is snow water path, Reff 
!      is effective radius, and DENS is the bulk density of snow.
!
!    SWP (kg/m**2)=(1.E-3 kg/g)*SWPrad, SWPrad in g/m**2 used in radiation
!    T = 1.5*1.E3*SWPrad/(Reff*DENS)
!  
!    See derivation for MASSI(INDEXS), note equal to RHO*QSNOW/NSNOW
!
!      SDENS=1.5e3/DENS, DENS=MASSI(INDEXS)/[PI*(1.E-6*INDEXS)**3]
!
        DO I=MDImin,MDImax
          SDENS(I)=PI*1.5E-15*FLOAT(I*I*I)/MASSI(I)
        ENDDO
!
        Thour_print=-DTPH/3600.

! SH 0211/2002
!       IF (PRINT_diag) THEN
       
      !-------- Total and maximum quantities
      !
!         NSTATS=0      ! Microphysical statistics dealing w/ grid-point counts
!         QMAX=0.       ! Microphysical statistics dealing w/ hydrometeor mass
!         QTOT=0.       ! Microphysical statistics dealing w/ hydrometeor mass
!         PRECmax=0.    ! Maximum precip rates (rain, snow) at surface (mm/h)
!         PRECtot=0.    ! Total precipitation (rain, snow) accumulation at surface
!       ENDIF

!wrf
        IF(.NOT.RESTART)THEN
          MP_RESTART_STATE(MY_T1:MY_T2)=MY_GROWTH_NMM(MY_T1:MY_T2)
          MP_RESTART_STATE(MY_T2+1)=C1XPVS0
          MP_RESTART_STATE(MY_T2+2)=C2XPVS0
          MP_RESTART_STATE(MY_T2+3)=C1XPVS
          MP_RESTART_STATE(MY_T2+4)=C2XPVS
          MP_RESTART_STATE(MY_T2+5)=CIACW
          MP_RESTART_STATE(MY_T2+6)=CIACR
          MP_RESTART_STATE(MY_T2+7)=CRACW
          MP_RESTART_STATE(MY_T2+8)=BRAUT
          TBPVS_STATE(1:NX) =TBPVS(1:NX)
          TBPVS0_STATE(1:NX)=TBPVS0(1:NX)
        ENDIF

      ENDIF  ! Allowed_to_read

      RETURN
!
!-----------------------------------------------------------------------
!
9061 CONTINUE
      WRITE(0,*)' module_mp_etanew: error opening ETAMPNEW_DATA.expanded_rain on unit ',etampnew_unit1
      STOP
!
!-----------------------------------------------------------------------
      END SUBROUTINE FERRIER_INIT_hr
!
      SUBROUTINE MY_GROWTH_RATES_NMM_hr (DTPH)
!
!--- Below are tabulated values for the predicted mass of ice crystals
!    after 600 s of growth in water saturated conditions, based on 
!    calculations from Miller and Young (JAS, 1979).  These values are
!    crudely estimated from tabulated curves at 600 s from Fig. 6.9 of
!    Young (1993).  Values at temperatures colder than -27C were 
!    assumed to be invariant with temperature.  
!
!--- Used to normalize Miller & Young (1979) calculations of ice growth
!    over large time steps using their tabulated values at 600 s.
!    Assumes 3D growth with time**1.5 following eq. (6.3) in Young (1993).
!
      IMPLICIT NONE
!
      REAL,INTENT(IN) :: DTPH
!
      REAL  DT_ICE
      REAL,DIMENSION(35) :: MY_600
!WRF
!
!-----------------------------------------------------------------------
!-- 20090714: These values are in g and need to be converted to kg below
      DATA MY_600 /                                                     &
     & 5.5e-8, 1.4E-7, 2.8E-7, 6.E-7, 3.3E-6,                           & 
     & 2.E-6, 9.E-7, 8.8E-7, 8.2E-7, 9.4e-7,                            & 
     & 1.2E-6, 1.85E-6, 5.5E-6, 1.5E-5, 1.7E-5,                         & 
     & 1.5E-5, 1.E-5, 3.4E-6, 1.85E-6, 1.35E-6,                         & 
     & 1.05E-6, 1.E-6, 9.5E-7, 9.0E-7, 9.5E-7,                          & 
     & 9.5E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7,                              & 
     & 9.E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7 /        ! -31 to -35 deg C
!
!-----------------------------------------------------------------------
!
      DT_ICE=(DTPH/600.)**1.5
      MY_GROWTH_NMM=DT_ICE*MY_600*1.E-3    !-- 20090714: Convert from g to kg
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE MY_GROWTH_RATES_NMM_hr
!
!-----------------------------------------------------------------------
!---------  Old GFS saturation vapor pressure lookup tables  -----------
!-----------------------------------------------------------------------
!
      SUBROUTINE GPVS_hr
!     ******************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GPVS_hr     COMPUTE SATURATION VAPOR PRESSURE TABLE
!   AUTHOR: N PHILLIPS       W/NP2      DATE: 30 DEC 82
!
! ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE TABLE AS A FUNCTION OF
!   TEMPERATURE FOR THE TABLE LOOKUP FUNCTION FPVS.
!   EXACT SATURATION VAPOR PRESSURES ARE CALCULATED IN SUBPROGRAM FPVSX.
!   THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A LENGTH
!   OF 7501 FOR TEMPERATURES RANGING FROM 180.0 TO 330.0 KELVIN.
!
! PROGRAM HISTORY LOG:
!   91-05-07  IREDELL
!   94-12-30  IREDELL             EXPAND TABLE
!   96-02-19  HONG                ICE EFFECT
!   01-11-29  JIN                 MODIFIED FOR WRF
!
! USAGE:  CALL GPVS_hr
!
! SUBPROGRAMS CALLED:
!   (FPVSX)  - INLINABLE FUNCTION TO COMPUTE SATURATION VAPOR PRESSURE
!
! COMMON BLOCKS:
!   COMPVS   - SCALING PARAMETERS AND TABLE FOR FUNCTION FPVS.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      IMPLICIT NONE
      real :: X,XINC,T
      integer :: JX
!----------------------------------------------------------------------
      XINC=(XMAX-XMIN)/(NX-1)
      C1XPVS=1.-XMIN/XINC
      C2XPVS=1./XINC
      C1XPVS0=1.-XMIN/XINC
      C2XPVS0=1./XINC
!
      DO JX=1,NX
        X=XMIN+(JX-1)*XINC
        T=X
        TBPVS(JX)=FPVSX(T)
        TBPVS0(JX)=FPVSX0(T)
      ENDDO
! 
      END SUBROUTINE GPVS_hr
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
                     REAL   FUNCTION FPVS(T)
!-----------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    FPVS        COMPUTE SATURATION VAPOR PRESSURE
!   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
!
! ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE FROM THE TEMPERATURE.
!   A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE
!   COMPUTED IN GPVS. SEE DOCUMENTATION FOR FPVSX FOR DETAILS.
!   INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.
!   THE INTERPOLATION ACCURACY IS ALMOST 6 DECIMAL PLACES.
!   ON THE CRAY, FPVS IS ABOUT 4 TIMES FASTER THAN EXACT CALCULATION.
!   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
!
! PROGRAM HISTORY LOG:
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
!   94-12-30  IREDELL             EXPAND TABLE
!   96-02-19  HONG                ICE EFFECT
!   01-11-29  JIN                 MODIFIED FOR WRF
!
! USAGE:   PVS=FPVS(T)
!
!   INPUT ARGUMENT LIST:
!     T        - REAL TEMPERATURE IN KELVIN
!
!   OUTPUT ARGUMENT LIST:
!     FPVS     - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!$$$
      IMPLICIT NONE
      real,INTENT(IN) :: T
      real XJ
      integer :: JX
!-----------------------------------------------------------------------
      XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),FLOAT(NX))
      JX=MIN(XJ,NX-1.)
      FPVS=TBPVS(JX)+(XJ-JX)*(TBPVS(JX+1)-TBPVS(JX))
!
      END FUNCTION FPVS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                     REAL FUNCTION FPVS0(T)
!-----------------------------------------------------------------------
      IMPLICIT NONE
      real,INTENT(IN) :: T
      real :: XJ1
      integer :: JX1
!-----------------------------------------------------------------------
      XJ1=MIN(MAX(C1XPVS0+C2XPVS0*T,1.),FLOAT(NX))
      JX1=MIN(XJ1,NX-1.)
      FPVS0=TBPVS0(JX1)+(XJ1-JX1)*(TBPVS0(JX1+1)-TBPVS0(JX1))
!
      END FUNCTION FPVS0
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
                    REAL FUNCTION FPVSX(T)
!-----------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    FPVSX       COMPUTE SATURATION VAPOR PRESSURE
!   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
!
! ABSTRACT: EXACTLY COMPUTE SATURATION VAPOR PRESSURE FROM TEMPERATURE.
!   THE WATER MODEL ASSUMES A PERFECT GAS, CONSTANT SPECIFIC HEATS
!   FOR GAS AND LIQUID, AND NEGLECTS THE VOLUME OF THE LIQUID.
!   THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT
!   OF CONDENSATION WITH TEMPERATURE.  THE ICE OPTION IS NOT INCLUDED.
!   THE CLAUSIUS-CLAPEYRON EQUATION IS INTEGRATED FROM THE TRIPLE POINT
!   TO GET THE FORMULA
!       PVS=PSATK*(TR**XA)*EXP(XB*(1.-TR))
!   WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS
!   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
!
! PROGRAM HISTORY LOG:
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
!   94-12-30  IREDELL             EXACT COMPUTATION
!   96-02-19  HONG                ICE EFFECT 
!   01-11-29  JIN                 MODIFIED FOR WRF
!
! USAGE:   PVS=FPVSX(T)
! REFERENCE:   EMANUEL(1994),116-117
!
!   INPUT ARGUMENT LIST:
!     T        - REAL TEMPERATURE IN KELVIN
!
!   OUTPUT ARGUMENT LIST:
!     FPVSX    - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!$$$
      IMPLICIT NONE
!-----------------------------------------------------------------------
       real, parameter :: TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2   &
      ,         CLIQ=4.1855E+3,CVAP= 1.8460E+3                          &
      ,         CICE=2.1060E+3,HSUB=2.8340E+6
!
      real, parameter :: PSATK=PSAT*1.E-3
      real, parameter :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
      real, parameter :: DLDTI=CVAP-CICE                                &
      ,                  XAI=-DLDTI/RV,XBI=XAI+HSUB/(RV*TTP)
      real T,TR
!-----------------------------------------------------------------------
      TR=TTP/T
!
      IF(T.GE.TTP)THEN
        FPVSX=PSATK*(TR**XA)*EXP(XB*(1.-TR))
      ELSE
        FPVSX=PSATK*(TR**XAI)*EXP(XBI*(1.-TR))
      ENDIF
! 
      END FUNCTION FPVSX
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                 REAL   FUNCTION FPVSX0(T)
!-----------------------------------------------------------------------
      IMPLICIT NONE
      real,parameter :: TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2     &
      ,         CLIQ=4.1855E+3,CVAP=1.8460E+3                           &
      ,         CICE=2.1060E+3,HSUB=2.8340E+6
      real,PARAMETER :: PSATK=PSAT*1.E-3
      real,PARAMETER :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
      real,PARAMETER :: DLDTI=CVAP-CICE                                 &
      ,                 XAI=-DLDT/RV,XBI=XA+HSUB/(RV*TTP)
      real :: T,TR
!-----------------------------------------------------------------------
      TR=TTP/T
      FPVSX0=PSATK*(TR**XA)*EXP(XB*(1.-TR))
!
      END FUNCTION FPVSX0

!
      END MODULE module_mp_fer_hires
