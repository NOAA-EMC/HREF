!WRF:MODEL_MP:PHYSICS
!
MODULE module_mp_etanew
!
!-----------------------------------------------------------------------
      REAL,PRIVATE,SAVE ::  ABFR, CBFR, CIACW, CIACR, C_N0r0,           &
     &  CN0r0, CN0r_DMRmin, CN0r_DMRmax, CRACW, CRAUT, ESW0,            &
     &  QAUT0, RFmax, RQR_DR1, RQR_DR2, RQR_DR3, RQR_DRmin,             &
     &  RQR_DRmax, RR_DRmin, RR_DR1, RR_DR2, RR_DR3, RR_DRmax
!
      INTEGER, PRIVATE,PARAMETER :: MY_T1=1, MY_T2=35
      REAL,PRIVATE,DIMENSION(MY_T1:MY_T2),SAVE :: MY_GROWTH
!
      REAL, PRIVATE,PARAMETER :: DMImin=.05e-3, DMImax=1.e-3,           &
     &      DelDMI=1.e-6,XMImin=1.e6*DMImin, XMImax=1.e6*DMImax    
      INTEGER, PRIVATE,PARAMETER :: MDImin=XMImin, MDImax=XMImax
      REAL, PRIVATE,DIMENSION(MDImin:MDImax) ::                         &
     &      ACCRI,MASSI,SDENS,VSNOWI,VENTI1,VENTI2
!
      REAL, PRIVATE,PARAMETER :: DMRmin=.05e-3, DMRmax=.45e-3,          &
     &      DelDMR=1.e-6,XMRmin=1.e6*DMRmin, XMRmax=1.e6*DMRmax
      INTEGER, PRIVATE,PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax                   
      REAL, PRIVATE,DIMENSION(MDRmin:MDRmax)::                          &
     &      ACCRR,MASSR,RRATE,VRAIN,VENTR1,VENTR2
!
      INTEGER, PRIVATE,PARAMETER :: Nrime=40
      REAL, DIMENSION(2:9,0:Nrime),PRIVATE,SAVE :: VEL_RF
!
      INTEGER,PARAMETER :: NX=7501
      REAL, PARAMETER :: XMIN=180.0,XMAX=330.0
      REAL, DIMENSION(NX),PRIVATE,SAVE :: TBPVS,TBPVS0
      REAL, PRIVATE,SAVE :: C1XPVS0,C2XPVS0,C1XPVS,C2XPVS
      REAL, PUBLIC,SAVE :: RHgrd
!
      CONTAINS

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE ETAMP_NEW (itimestep,DT,DX,DY,RAINNC,RAINNCV,          &
     &                      dz8w,rho_phy,p_phy,pi_phy,th_phy,qv,qc,     &
     &                      LOWLYR,SR,                                  &
     &                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           &
     &                      ids,ide, jds,jde, kds,kde,		        &
     &                      ims,ime, jms,jme, kms,kme,		        &
     &                      its,ite, jts,jte, kts,kte		     )
!---------------------------------------------------------------------
      IMPLICIT NONE
!---------------------------------------------------------------------
      INTEGER, PARAMETER :: ITLO=-60, ITHI=40

      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE  		     &
                           ,IMS,IME,JMS,JME,KMS,KME		     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE		     &
                           ,ITIMESTEP

      REAL, INTENT(IN) 	    :: DT,DX,DY
      REAL, INTENT(IN),     DIMENSION(ims:ime, kms:kme, jms:jme)::   &
                            dz8w,p_phy,pi_phy,rho_phy
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme)::   &
                            th_phy,qv,qc
      REAL, INTENT(INOUT),  DIMENSION(ims:ime,jms:jme):: RAINNC,RAINNCV
      REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) :: &
 	                    F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
      REAL, INTENT(OUT),    DIMENSION(ims:ime,jms:jme):: SR
      INTEGER, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) :: LOWLYR


!     LOCAL VARS

!     NSTATS,QMAX,QTOT are diagnostic vars

      INTEGER,DIMENSION(ITLO:ITHI,4) :: NSTATS
      REAL,   DIMENSION(ITLO:ITHI,5) :: QMAX
      REAL,   DIMENSION(ITLO:ITHI,22):: QTOT

!     SOME VARS WILL BE USED FOR DATA ASSIMILATION (DONT NEED THEM NOW). 
!     THEY ARE TREATED AS LOCAL VARS, BUT WILL BECOME STATE VARS IN THE 
!     FUTURE. SO, WE DECLARED THEM AS MEMORY SIZES FOR THE FUTURE USE

!     TLATGS_PHY,TRAIN_PHY,APREC,PREC,ACPREC,SR are not directly related 
!     the microphysics scheme. Instead, they will be used by Eta precip 
!     assimilation.

      REAL,  DIMENSION( ims:ime, kms:kme, jms:jme ) ::                &
	     TLATGS_PHY,TRAIN_PHY
      REAL,  DIMENSION(ims:ime,jms:jme):: APREC,PREC,ACPREC
      REAL,  DIMENSION(its:ite, kts:kte, jts:jte):: t_phy

      INTEGER :: I,J,K,KFLIP

      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
        t_phy(i,k,j) = th_phy(i,k,j)*pi_phy(i,k,j)
        qv(i,k,j)=qv(i,k,j)/(1.+qv(i,k,j)) !Convert to specific humidity
      ENDDO
      ENDDO
      ENDDO

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

      DO j = jts,jte
      DO k = kts,kte
      DO i = its,ite
	 TLATGS_PHY (i,k,j)=0.
	 TRAIN_PHY  (i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,jte
      DO i = its,ite
         ACPREC(i,j)=0.
         APREC (i,j)=0.
         PREC  (i,j)=0.
         SR    (i,j)=0.
      ENDDO
      ENDDO

!     if (itimestep .eq. 0)                                       &
!   	 CALL etanewinit (DT,DX,DY,LOWLYR,               	  &
!                         F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
!                         ids, ide, jds, jde, kds, kde,	          &
!                         ims, ime, jms, jme, kms, kme,	          &
!                         its, ite, jts, jte, kts, kte)


      CALL EGCP01DRV(DT,LOWLYR,                     &
                     APREC,PREC,ACPREC,SR,NSTATS,QMAX,QTOT,	  &
                     dz8w,rho_phy,qc,t_phy,qv,F_ICE_PHY,P_PHY,    &
                     F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY,TRAIN_PHY, &
                     ids,ide, jds,jde, kds,kde,		          &
                     ims,ime, jms,jme, kms,kme,		          &
                     its,ite, jts,jte, kts,kte		          )


     DO j = jts,jte
        DO k = kts,kte
	DO i = its,ite
  	   th_phy(i,k,j) = t_phy(i,k,j)/pi_phy(i,k,j)
           qv(i,k,j)=qv(i,k,j)/(1.-qv(i,k,j))  !Convert to mixing ratio
	ENDDO
        ENDDO
     ENDDO
! 
! update rain (from m to mm)

     DO j = jts,jte
     DO i = its,ite
        RAINNC (i,j)=APREC(i,j)*1000. + RAINNC(i,j)
        RAINNCV(i,j)=APREC(i,j)*1000.
      ENDDO
      ENDDO


  END SUBROUTINE ETAMP_NEW

!--------------------------------------------------------------------------
       SUBROUTINE EGCP01DRV(                                              &
        DTPH,LOWLYR,APREC,PREC,ACPREC,SR,                                 &
        NSTATS,QMAX,QTOT,                                                 &
        dz8w,RHO_PHY,CWM_PHY,T_PHY,Q_PHY,F_ICE_PHY,P_PHY,                 &
        F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY,TRAIN_PHY,                      &
        ids,ide, jds,jde, kds,kde,                                        &
        ims,ime, jms,jme, kms,kme,                                        &
        its,ite, jts,jte, kts,kte)
!--------------------------------------------------------------------------
! DTPH           Physics time step (s)
! CWM_PHY (qc)   Mixing ratio of the total condensate. kg/kg
! Q_PHY          Mixing ratio of water vapor. kg/kg
! F_RAIN_PHY     Fraction of rain. 
! F_ICE_PHY      Fraction of ice.
! F_RIMEF_PHY    Mass ratio of rimed ice (rime factor).
!
!TLATGS_PHY,TRAIN_PHY,APREC,PREC,ACPREC,SR are not directly related the
!micrphysics sechme. Instead, they will be used by Eta precip assimilation.
!
!NSTATS,QMAX,QTOT are used for diagnosis purposes.
!
!----------------------------------------------------------------------
!--- Variables APREC,PREC,ACPREC,SR are calculated for precip assimilation
!    and/or ZHAOs scheme in Eta and are not required by this microphysics 
!    scheme itself.  
!--- NSTATS,QMAX,QTOT are used for diagnosis purposes only.  They will be 
!    printed out when PRINT_diag is true.
!
      IMPLICIT NONE
!
      REAL, PARAMETER    :: EPSQ=1.E-20, GRAV=9.81, RHOL=1000., T0C=273.15,&
                            T_ICE=-10., T_ICEK=T0C+T_ICE, RRHOL=1./RHOL,   &
                            CLIMIT=1.E-12 
      INTEGER, PARAMETER :: ITLO=-60, ITHI=40
      LOGICAL, PARAMETER :: PRINT_diag=.FALSE.

      
!     VARIABLES PASSED IN/OUT
      INTEGER,INTENT(IN ) :: ids,ide, jds,jde, kds,kde                     &
                            ,ims,ime, jms,jme, kms,kme                     &
                             ,its,ite, jts,jte, kts,kte
      REAL,INTENT(IN) :: DTPH
      INTEGER, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) :: LOWLYR
      INTEGER,DIMENSION(ITLO:ITHI,4),INTENT(INOUT) :: NSTATS
      REAL,DIMENSION(ITLO:ITHI,5),INTENT(INOUT) :: QMAX
      REAL,DIMENSION(ITLO:ITHI,22),INTENT(INOUT) :: QTOT
      REAL,DIMENSION(ims:ime,jms:jme),INTENT(INOUT) ::                     &
                               APREC,PREC,ACPREC,SR
      REAL,DIMENSION( its:ite, kts:kte, jts:jte ),INTENT(INOUT) :: t_phy
      REAL,DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN) ::            &
                                                   dz8w,P_PHY,RHO_PHY
      REAL,DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(INOUT) ::         &
         CWM_PHY, F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,TLATGS_PHY                       &
         ,Q_PHY,TRAIN_PHY
!
!LOCAL VARIABLES
      INTEGER :: LSFC,I,J,I_index,J_index,L,K,KFLIP
      REAL,DIMENSION(its:ite,jts:jte,kts:kte) ::                           &
         CWM,T,Q,TRAIN,TLATGS,P                                               
      REAL,DIMENSION(kts:kte,its:ite,jts:jte) :: F_ice,F_rain,F_RimeF       
      INTEGER,DIMENSION(its:ite,jts:jte) :: LMH
      REAL :: TC,WC,QI,QR,QW,Fice,Frain,DUM,ASNOW,ARAIN
      REAL,DIMENSION(kts:kte) :: P_col,Q_col,T_col,QV_col,WC_col,          &
         RimeF_col,QI_col,QR_col,QW_col, THICK_col,DPCOL 
      REAL,DIMENSION(2) :: PRECtot,PRECmax
!    
!    
!
        DO J=JTS,JTE    
        DO I=ITS,ITE  
           LMH(I,J) = KTE-LOWLYR(I,J)+1
        ENDDO
        ENDDO

        DO 98  J=JTS,JTE    
        DO 98  I=ITS,ITE  
           DO L=KTS,KTE
             KFLIP=KTE+1-L
             CWM(I,J,L)=CWM_PHY(I,KFLIP,J)
             T(I,J,L)=T_PHY(I,KFLIP,J)
             Q(I,J,L)=Q_PHY(I,KFLIP,J)
             P(I,J,L)=P_PHY(I,KFLIP,J)
             TLATGS(I,J,L)=TLATGS_PHY(I,KFLIP,J)
             TRAIN(I,J,L)=TRAIN_PHY(I,KFLIP,J)
             F_ice(L,I,J)=F_ice_PHY(I,KFLIP,J)
             F_rain(L,I,J)=F_rain_PHY(I,KFLIP,J)
             F_RimeF(L,I,J)=F_RimeF_PHY(I,KFLIP,J)
           ENDDO
98      CONTINUE
     
       DO 100 J=JTS,JTE    
        DO 100 I=ITS,ITE  
          LSFC=LMH(I,J)                      ! "L" of surface
!
          DO K=KTS,KTE
            KFLIP=KTE+1-K
            DPCOL(K)=RHO_PHY(I,KFLIP,J)*GRAV*dz8w(I,KFLIP,J)
          ENDDO
!   
   !
   !--- Initialize column data (1D arrays)
   !
        IF (CWM(I,J,1) .LE. CLIMIT) CWM(I,J,1)=EPSQ
          F_ice(1,I,J)=1.
          F_rain(1,I,J)=0.
          F_RimeF(1,I,J)=1.
          DO L=1,LSFC
      !
      !--- Pressure (Pa) = (Psfc-Ptop)*(ETA/ETA_sfc)+Ptop
      !
            P_col(L)=P(I,J,L)
      !
      !--- Layer thickness = RHO*DZ = -DP/G = (Psfc-Ptop)*D_ETA/(G*ETA_sfc)
      !
            THICK_col(L)=DPCOL(L)/GRAV
            T_col(L)=T(I,J,L)
            TC=T_col(L)-T0C
            QV_col(L)=max(EPSQ, Q(I,J,L))
            IF (CWM(I,J,L) .LE. CLIMIT) THEN
              WC_col(L)=0.
              IF (TC .LT. T_ICE) THEN
                F_ice(L,I,J)=1.
              ELSE
                F_ice(L,I,J)=0.
              ENDIF
              F_rain(L,I,J)=0.
              F_RimeF(L,I,J)=1.
            ELSE
              WC_col(L)=CWM(I,J,L)
            ENDIF
      !
      !--- Determine composition of condensate in terms of 
      !      cloud water, ice, & rain
      !
            WC=WC_col(L)
            QI=0.
            QR=0.
            QW=0.
            Fice=F_ice(L,I,J)
            Frain=F_rain(L,I,J)


      !
      !--- REAL*4 array storage
      !
            IF (Fice .GE. 1.) THEN
              QI=WC
            ELSE IF (Fice .LE. 0.) THEN
              QW=WC
            ELSE
              QI=Fice*WC
              QW=WC-QI
            ENDIF
            IF (QW.GT.0. .AND. Frain.GT.0.) THEN
              IF (Frain .GE. 1.) THEN
                QR=QW
                QW=0.
              ELSE
                QR=Frain*QW
                QW=QW-QR
              ENDIF
            ENDIF
            RimeF_col(L)=F_RimeF(L,I,J)               ! (real)
        !
        !--- INTEGER*2 array storage
!!      !
!!            IF (TC.LE.T_ICE .OR. Fice.GE.IFMAX) THEN
!!              QI=WC
!!            ELSE IF (Fice .LE. IFMIN) THEN
!!              QW=WC
!!            ELSE
!!              QI=RFSCALE*Fice*WC
!!              QW=WC-QI
!!            ENDIF
!!            IF (QW.GT.0. .AND. Frain.GT.0) THEN
!!              IF (Frain .EQ. IFMAX) THEN
!!                QR=QW
!!                QW=0.
!!              ELSE
!!                QR=RFSCALE*Frain*QW
!!                QW=QW-QR
!!              ENDIF
!!            ENDIF
!!            RimeF_col(L)=F_RimeF(L,I,J)/RIMESCALE
      !
            QI_col(L)=QI
            QR_col(L)=QR
            QW_col(L)=QW
          ENDDO
!
!#######################################################################
   !
   !--- Perform the microphysical calculations in this column
   !
          I_index=I
          J_index=J
       CALL EGCP01COLUMN ( ARAIN, ASNOW, DTPH, I_index, J_index, LSFC,     &
       P_col, QI_col, QR_col, QV_col, QW_col, RimeF_col, T_col,            &
       THICK_col, WC_col,KTS,KTE,NSTATS,QMAX,QTOT )


   !
!#######################################################################
!
   !
   !--- Update storage arrays
   !
          DO L=1,LSFC
            TRAIN(I,J,L)=(T_col(L)-T(I,J,L))/DTPH
            TLATGS(I,J,L)=T_col(L)-T(I,J,L)
            T(I,J,L)=T_col(L)
            Q(I,J,L)=QV_col(L)
            CWM(I,J,L)=WC_col(L)
      !
      !--- REAL*4 array storage
      !
            F_RimeF(L,I,J)=MAX(1., RimeF_col(L))
            IF (QI_col(L) .LE. CLIMIT) THEN
              F_ice(L,I,J)=0.
              IF (T_col(L) .LT. T_ICEK) F_ice(L,I,J)=1.
            ELSE
              F_ice(L,I,J)=MAX( 0., MIN(1., QI_col(L)/WC_col(L)) )
            ENDIF
            IF (QR_col(L) .LE. CLIMIT) THEN
              DUM=0
            ELSE
              DUM=QR_col(L)/(QR_col(L)+QW_col(L))
            ENDIF
            F_rain(L,I,J)=DUM
!!      !
!!      !--- INTEGER*2 array storage
!!      !
!!            IF (RimeF_col(L) .LE. 1.) THEN
!!              I2DUM=IRimeF
!!              I2DUM=IRimeF
!!            ELSE
!!              I2DUM=(RimeF_col(L)+RIMEROUND)*RIMESCALE
!!            ENDIF
!!            F_RimeF(L,I,J)=I2DUM
!!            IF (QI_col(L) .LE. CLIMIT) THEN
!!              I2DUM=0
!!            ELSE
!!              I2DUM=FSCALE*QI_col(L)/WC_col(L)
!!              I2DUM=MAX( IFMIN, MIN(IFMAX, I2DUM) )
!!            ENDIF
!!            F_ice(L,I,J)=I2DUM
!!            IF (QR_col(L) .LE. CLIMIT) THEN
!!              I2DUM=0
!!            ELSE
!!              I2DUM=FSCALE*QR_col(L)/(QR_col(L)+QW_col(L))
!!              I2DUM=MAX( IFMIN, MIN(IFMAX, I2DUM) )
!!            ENDIF
!!            F_rain(L,I,J)=I2DUM
      !
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


!#######################################################################
!#######################################################################
!
100   CONTINUE                          ! End "I" & "J" loops
        DO 101 J=JTS,JTE    
        DO 101 I=ITS,ITE  
           DO L=KTS,KTE
              KFLIP=KTE+1-L
             CWM_PHY(I,KFLIP,J)=CWM(I,J,L)
             T_PHY(I,KFLIP,J)=T(I,J,L)
             Q_PHY(I,KFLIP,J)=Q(I,J,L)
             TLATGS_PHY(I,KFLIP,J)=TLATGS(I,J,L)
             TRAIN_PHY(I,KFLIP,J)=TRAIN(I,J,L)
             F_ice_PHY(I,KFLIP,J)=F_ice(L,I,J)
             F_rain_PHY(I,KFLIP,J)=F_rain(L,I,J)
             F_RimeF_PHY(I,KFLIP,J)=F_RimeF(L,I,J)
           ENDDO
101     CONTINUE
      END SUBROUTINE EGCP01DRV
!
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
!        -> 8/22/01: This feature has been diminished by effectively adjusting to
!           ice saturation during depositional growth at temperatures colder than
!           -10C.  Ice sublimation is calculated more explicitly.  The logic is
!           that sources of are either poorly understood (e.g., nucleation for NWP) 
!           or are not represented in the Eta model (e.g., detrainment of ice from 
!           convection).  Otherwise the model is too wet compared to the radiosonde
!           observations based on 1 Feb - 18 March 2001 retrospective runs.  
!       (7) Top-down integration also attempts to treat mixed-phase processes,
!           allowing a mixture of ice and water.  Based on numerous observational
!           studies, ice growth is based on nucleation at cloud top &
!           subsequent growth by vapor deposition and riming as the ice particles 
!           fall through the cloud.  Effective nucleation rates are a function
!           of ice supersaturation following Meyers et al. (JAM, 1992).  
!        -> 8/22/01: The simulated relative humidities were far too moist compared 
!           to the rawinsonde observations.  This feature has been substantially 
!           diminished, limited to a much narrower temperature range of 0 to -10C.  
!       (8) Depositional growth of newly nucleated ice is calculated for large time
!           steps using Fig. 8 of Miller and Young (JAS, 1979), at 1 deg intervals
!           using their ice crystal masses calculated after 600 s of growth in water
!           saturated conditions.  The growth rates are normalized by time step
!           assuming 3D growth with time**1.5 following eq. (6.3) in Young (1993).
!        -> 8/22/01: This feature has been effectively limited to 0 to -10C.  
!       (9) Ice precipitation rates can increase due to increase in response to
!           cloud water riming due to (a) increased density & mass of the rimed
!           ice, and (b) increased fall speeds of rimed ice.
!        -> 8/22/01: This feature has been effectively limited to 0 to -10C.  
!###############################################################################
!###############################################################################
!
      SUBROUTINE EGCP01COLUMN ( ARAIN, ASNOW, DTPH, I_index, J_index, LSFC, &
       P_col, QI_col, QR_col, QV_col, QW_col, RimeF_col, T_col,             &
       THICK_col, WC_col ,KTS,KTE,NSTATS,QMAX,QTOT)                          
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
!   * CALL EGCP01COLUMN FROM SUBROUTINE EGCP01DRV
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
!FEB  use module_GPVS
      IMPLICIT NONE
!    
      INTEGER,INTENT(IN) :: KTS,KTE,I_index, J_index, LSFC
      REAL,INTENT(INOUT) ::  ARAIN, ASNOW
      REAL,DIMENSION(KTS:KTE),INTENT(INOUT) ::  P_col, QI_col,QR_col, QV_col    &
       ,QW_col, RimeF_col, T_col, THICK_col,WC_col
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
!--- Mean rain drop diameters varying from 50 microns (0.05 mm) to 450 microns 
!      (0.45 mm), assuming an exponential size distribution.  
!
!------------------------------------------------------------------------- 
!------- Key parameters, local variables, & important comments ---------
!-----------------------------------------------------------------------
!
!--- KEY Parameters:
!
!---- Comments on 2 August 2000
!    * EPSQ=1.E-20 is the lower limit for specific humidity and cloud 
!      condensate.  The value of EPSQ will need to be changed in the other 
!      subroutines in order to make it consistent throughout the Eta code.  
!- NLImax - maximum number concentration of large ice crystals (20,000 /m**3, 20 per liter)
!- NLImin - minimum number concentration of large ice crystals (100 /m**3, 0.1 per liter)
!
      REAL, PARAMETER :: CP=1004.6, EPSQ=1.E-20, EPSW=1.E-12, RD=287.04,    &
       RHOL=1000., RV=461.5, T0C=273.15, XLS=2.834E6, EPS=RD/RV,            &
       NLImax=20.E3, NLImin=100., T_ICE=-10., T_ICE_init=-5.,               &
       TOLER=5.E-7,                                                         &
       CLIMIT=10.*EPSQ, CLIMIT1=-CLIMIT, EPS1=RV/RD-1., RCP=1./CP,          &
       RCPRV=RCP/RV, RRHOL=1./RHOL, XLS1=XLS*RCP, XLS2=XLS*XLS*RCPRV,       &
       XLS3=XLS*XLS/RV,                                                     &
       C1=1./3., C2=1./6., C3=3.31/6.,                                      & 
       DMR1=.1E-3, DMR2=.2E-3, DMR3=.32E-3, N0r0=8.E6, N0rmin=1.e4,         &
       N0s0=4.E6, RHO0=1.194, XMR1=1.e6*DMR1, XMR2=1.e6*DMR2,               &
       XMR3=1.e6*DMR3, Xratio=.025                                           
      INTEGER, PARAMETER :: MDR1=XMR1, MDR2=XMR2, MDR3=XMR3
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
!--- Local variables
!
      REAL EMAIRI, N0r, NLICE, NSmICE
      LOGICAL CLEAR, ICE_logical, DBG_logical, DIAG_print, RAIN_logical
      INTEGER :: IDR,INDEX_MY,INDEXR,INDEXR1,INDEXS,IPASS,ITDX,IXRF,       &
                 IXS,LBEF,L
!
      REAL :: ABI,ABW,AIEVP,ARAINNEW,ASNOWNEW,BLDTRH,BUDGET,               &
              CREVP,DELI,DELR,DELT,DELV,DELW,DENOMF,                       &
              DENOMI,DENOMW,DENOMWI,DIDEP,                                 &
              DIEVP,DIFFUS,DLI,DTPH,DTRHO,DUM,DUM1,                        &
              DUM2,DWV0,DWVI,DWVR,DYNVIS,ESI,ESW,FIR,FLARGE,FLIMASS,       &
              FSMALL,FWR,FWS,GAMMAR,GAMMAS,                                &
              PCOND,PIACR,PIACW,PIACWI,PIACWR,PICND,PIDEP,PIDEP_max,       &
              PIEVP,PILOSS,PIMLT,PP,PRACW,PRAUT,PREVP,PRLOSS,              &
              QI,QINEW,QLICE,QR,QRNEW,QSI,QSIGRD,QSINEW,QSW,QSW0,          &
              QSWGRD,QSWNEW,QT,QTICE,QTNEW,QTRAIN,QV,QW,QW0,QWNEW,         &
              RFACTOR,RHO,RIMEF,RIMEF1,RQR,RR,RRHO,SFACTOR,                &
              TC,TCC,TFACTOR,THERM_COND,THICK,TK,TK2,TNEW,                 &
              TOT_ICE,TOT_ICENEW,TOT_RAIN,TOT_RAINNEW,                     &
              VEL_INC,VENTR,VENTIL,VENTIS,VRAIN1,VRAIN2,VRIMEF,VSNOW,      &
              WC,WCNEW,WSGRD,WS,WSNEW,WV,WVNEW,WVQW,                       &
              XLF,XLF1,XLI,XLV,XLV1,XLV2,XLIMASS,XRF,XSIMASS          
!
!#######################################################################
!########################## Begin Execution ############################
!#######################################################################
!
!
      ARAIN=0.                ! Accumulated rainfall into grid box from above (kg/m**2)
      ASNOW=0.                ! Accumulated snowfall into grid box from above (kg/m**2)
      DIAG_print=.false.
!
!-----------------------------------------------------------------------
!------------ Loop from top (L=1) to surface (L=LSFC) ------------------
!-----------------------------------------------------------------------
!

      DO 10 L=1,LSFC

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
          ESW=1000.*FPVS0(TK)              ! Saturation vapor pressure w/r/t water
          QSW=EPS*ESW/(PP-ESW)             ! Saturation mixing ratio w/r/t water
          WS=QSW                           ! General saturation mixing ratio (water/ice)
          IF (TC .LT. 0.) THEN
            ESI=1000.*FPVS(TK)             ! Saturation vapor pressure w/r/t ice
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
          IF (WV.GT.WSgrd .OR. WC.GT.EPSW) CLEAR=.FALSE.
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
!            WRITE(6,"(/2(a,i4),a,i2)") {@ i=,I_index, j=,J_index,
!     &                                  L=,L
!            WRITE(6,"(4(a12,g11.4,1x))") 
!     & {@ TCold=,TC,P=,.01*PP,DIFF=,DUM,WCold=,WC,
!     & {@ QIold=,QI,QWold=,QW,QRold=,QR
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
          TFACTOR=TK**1.5/(TK+120.)
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
          IF (TC.LT.0. .OR. QI.GT. CLIMIT .OR. ASNOW.GT.CLIMIT) THEN
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
          IF (ARAIN.GT.CLIMIT .OR. QR.GT.CLIMIT) RAIN_logical=.TRUE.
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


            IF (TC.GE.0. .OR. WVQW.LT.QSIgrd) THEN
   !
   !--- Eliminate small ice particle contributions for melting & sublimation
   !
              FLARGE=1.
            ELSE
   !
   !--- Enhanced number of small ice particles during depositional growth
   !    (effective only when 0C > T >= T_ice [-10C] )
   !
              FLARGE=.2
   !
   !--- Larger number of small ice particles due to rime splintering
   !
              IF (TC.GE.-8. .AND. TC.LE.-3.) FLARGE=.5*FLARGE
!
            ENDIF            ! End IF (TC.GE.0. .OR. WVQW.LT.QSIgrd)
            FSMALL=(1.-FLARGE)/FLARGE
            XSIMASS=RRHO*MASSI(MDImin)*FSMALL
            IF (QI.LE.CLIMIT .AND. ASNOW.LE.CLIMIT) THEN
              INDEXS=MDImin
              TOT_ICE=0.
              PILOSS=0.
              RimeF1=1.
              VrimeF=1.
              VEL_INC=GAMMAS
              VSNOW=0.
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
                    VrimeF=VEL_RF(IXS,IXRF)+(XRF-FLOAT(IXRF))*             &
                          (VEL_RF(IXS,IXRF+1)-VEL_RF(IXS,IXRF))
                  ENDIF
                ENDIF            ! End IF (RimeF1 .LE. 1.)
                VEL_INC=GAMMAS*VrimeF
                VSNOW=VEL_INC*VSNOWI(INDEXS)
                EMAIRI=THICK+BLDTRH*VSNOW
                XLIMASS=RRHO*RimeF1*MASSI(INDEXS)
                FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
                QTICE=TOT_ICE/EMAIRI
                QLICE=FLIMASS*QTICE
                NLICE=QLICE/XLIMASS
                NSmICE=Fsmall*NLICE
   !
                IF ( (NLICE.GE.NLImin .AND. NLICE.LE.NLImax)              &
                      .OR. IPASS.EQ.1) THEN
                  EXIT
                ELSE
        !
        !--- Reduce excessive accumulation of ice at upper levels
        !    associated with strong grid-resolved ascent
        !
        !--- Force NLICE to be between NLImin and NLImax
        !
                  DUM=MAX(NLImin, MIN(NLImax, NLICE) )
                  XLI=RHO*(QTICE/DUM-XSIMASS)/RimeF1
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
                    IF (DUM .GE. NLImax)                                   &
                      RimeF1=RHO*(QTICE/NLImax-XSIMASS)/MASSI(INDEXS)
                  ENDIF             ! End IF (XLI .LE. MASSI(MDImin) ) 
!            WRITE(6,"(4(a12,g11.4,1x))") 
!     & {$ TC=,TC,P=,.01*PP,NLICE=,NLICE,DUM=,DUM,
!     & {$ XLI=,XLI,INDEXS=,FLOAT(INDEXS),RHO=,RHO,QTICE=,QTICE,
!     & {$ XSIMASS=,XSIMASS,RimeF1=,RimeF1
                ENDIF                  ! End IF ( (NLICE.GE.NLImin .AND. NLICE.LE.NLImax) ...
              ENDDO                    ! End DO IPASS=0,1
            ENDIF                      ! End IF (QI.LE.CLIMIT .AND. ASNOW.LE.CLIMIT)
          ENDIF                        ! End IF (ICE_logical)
!
!----------------------------------------------------------------------
!--------------- Calculate individual processes -----------------------
!----------------------------------------------------------------------
!
!--- Cloud water autoconversion to rain and collection by rain
!
          IF (QW.GT.CLIMIT .AND. TC.GE.T_ICE) THEN
   !
   !--- QW0 could be modified based on land/sea properties, 
   !      presence of convection, etc.  This is why QAUT0 and CRAUT
   !      are passed into the subroutine as externally determined
   !      parameters.  Can be changed in the future if desired.
   !
            QW0=QAUT0*RRHO
            PRAUT=MAX(0., QW-QW0)*CRAUT
            IF (QLICE .GT. CLIMIT) THEN
      !
      !--- Collection of cloud water by large ice particles ("snow")
      !    PIACWI=PIACW for riming, PIACWI=0 for shedding
      !
              FWS=MIN(1., CIACW*VEL_INC*NLICE*ACCRI(INDEXS)/PP**C1)
              PIACW=FWS*QW
              IF (TC .LT. 0.) PIACWI=PIACW    ! Large ice riming
            ENDIF           ! End IF (QLICE .GT. CLIMIT)
          ENDIF             ! End IF (QW.GT.CLIMIT .AND. TC.GE.T_ICE)
!
!----------------------------------------------------------------------
!--- Loop around some of the ice-phase processes if no ice should be present
!----------------------------------------------------------------------
!
          IF (ICE_logical .EQV. .FALSE.) GO TO 20
!
!--- Now the pretzel logic of calculating ice deposition
!
          IF (TC.LT.T_ICE .AND. (WV.GT.QSIgrd .OR. QW.GT.CLIMIT)) THEN
   !
   !--- Adjust to ice saturation at T<T_ICE (-10C) if supersaturated.
   !    Sources of ice due to nucleation and convective detrainment are
   !    either poorly understood, poorly resolved at typical NWP 
   !    resolutions, or are not represented (e.g., no detrained 
   !    condensate in BMJ Cu scheme).
   !    
            PCOND=-QW
            DUM1=TK+XLV1*PCOND                 ! Updated (dummy) temperature (deg K)
            DUM2=WV+QW                         ! Updated (dummy) water vapor mixing ratio
            DUM=1000.*FPVS(DUM1)               ! Updated (dummy) saturation vapor pressure w/r/t ice
            DUM=RHgrd*EPS*DUM/(PP-DUM)         ! Updated (dummy) saturation mixing ratio w/r/t ice
            IF (DUM2 .GT. DUM) PIDEP=DEPOSIT (PP, RHgrd, DUM1, DUM2)
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
            IF (QTICE .GT. 0.) THEN
      !
      !--- Calculate ice deposition/sublimation
      !      * SFACTOR - [VEL_INC**.5]*[Schmidt**(1./3.)]*[(RHO/DYNVIS)**.5],
      !        where Schmidt (Schmidt Number) =DYNVIS/(RHO*DIFFUS)
      !      * Units: SFACTOR - s**.5/m ;  ABI - m**2/s ;  NLICE - m**-3 ;
      !               VENTIL, VENTIS - m**-2 ;  VENTI1 - m ;  
      !               VENTI2 - m**2/s**.5 ; DIDEP - unitless
      !
              SFACTOR=VEL_INC**.5*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2
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
      !
      !--- Account for change in water vapor supply w/ time
      !
              IF (DIDEP .GE. Xratio)then
                DIDEP=(1.-EXP(-DIDEP*DENOMI))/DENOMI
              endif
              IF (DWVi .GT. 0.) THEN
                PIDEP=MIN(DWVi*DIDEP, PIDEP_max)
              ELSE IF (DWVi .LT. 0.) THEN
                PIDEP=MAX(DWVi*DIDEP, PIDEP_max)
              ENDIF
      !
            ELSE IF (WVQW.GT.QSI .AND. TC.LE.T_ICE_init) THEN
      !
      !--- Ice nucleation in near water-saturated conditions.  Ice crystal
      !    growth during time step calculated using Miller & Young (1979, JAS).
      !--- These deposition rates could drive conditions below water saturation,
      !    which is the basis of these calculations.  Intended to approximate
      !    more complex & computationally intensive calculations.
      !
              INDEX_MY=MAX(MY_T1, MIN( INT(.5-TC), MY_T2 ) )
      !
      !--- DUM1 is the supersaturation w/r/t ice at water-saturated conditions
      !
      !--- DUM2 is the number of ice crystals nucleated at water-saturated 
      !    conditions based on Meyers et al. (JAM, 1992).
      !
      !--- Prevent unrealistically large ice initiation (limited by PIDEP_max)
      !      if DUM2 values are increased in future experiments
      !
              DUM1=QSW/QSI-1.      
              DUM2=1.E3*EXP(12.96*DUM1-.639)
              PIDEP=MIN(PIDEP_max, DUM2*MY_GROWTH(INDEX_MY)*RRHO)
      !
            ENDIF       ! End IF (QTICE .GT. 0.)
   !
          ENDIF         ! End IF (TC.LT.T_ICE .AND. (WV.GT.QSIgrd .OR. QW.GT.CLIMIT))
!
!------------------------------------------------------------------------
!
20      CONTINUE     ! Jump here if conditions for ice are not present


!
!------------------------------------------------------------------------
!
!--- Cloud water condensation
!
          IF (TC.GE.T_ICE .AND. (QW.GT.CLIMIT .OR. WV.GT.QSWgrd)) THEN
            IF (PIACWI.EQ.0. .AND. PIDEP.EQ.0.) THEN
              PCOND=CONDENSE (PP, QW, RHgrd, TK, WV)
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
          ENDIF            ! End IF (TC.GE.T_ICE .AND. (QW.GT.CLIMIT .OR. WV.GT.QSWgrd))
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
            SFACTOR=VEL_INC**.5*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2
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
            IF (WV.LT.QSW .AND. DUM.LE.CLIMIT) THEN
   !
   !--- Evaporation from melting snow (sink of snow) or shedding
   !    of water condensed onto melting snow (source of rain)
   !
              DUM=DWV0*DIEVP
              PIEVP=MAX( MIN(0., DUM), PILOSS)
              PICND=MAX(0., DUM)
            ENDIF            ! End IF (WV.LT.QSW .AND. DUM.LE.CLIMIT)
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
          INDEXR1=INDEXR    ! For debugging only
          INDEXR=MDRmin
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
   !
              IF (RR .LE. RR_DRmin) THEN
        !
        !--- Assume fixed mean diameter of rain (0.2 mm) for low rain rates, 
        !      instead vary N0r with rain rate
        !
                INDEXR=MDRmin
              ELSE IF (RR .LE. RR_DR1) THEN
        !
        !--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
        !      for mean diameters (Dr) between 0.05 and 0.10 mm:
        !      V(Dr)=5.6023e4*Dr**1.136, V in m/s and Dr in m
        !      RR = PI*1000.*N0r0*5.6023e4*Dr**(4+1.136) = 1.408e15*Dr**5.136,
        !        RR in kg/(m**2*s)
        !      Dr (m) = 1.123e-3*RR**.1947 -> Dr (microns) = 1.123e3*RR**.1947
        !
                INDEXR=INT( 1.123E3*RR**.1947 + .5 )
                INDEXR=MAX( MDRmin, MIN(INDEXR, MDR1) )
              ELSE IF (RR .LE. RR_DR2) THEN
        !
        !--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
        !      for mean diameters (Dr) between 0.10 and 0.20 mm:
        !      V(Dr)=1.0867e4*Dr**.958, V in m/s and Dr in m
        !      RR = PI*1000.*N0r0*1.0867e4*Dr**(4+.958) = 2.731e14*Dr**4.958,
        !        RR in kg/(m**2*s)
        !      Dr (m) = 1.225e-3*RR**.2017 -> Dr (microns) = 1.225e3*RR**.2017
        !
                INDEXR=INT( 1.225E3*RR**.2017 + .5 )
                INDEXR=MAX( MDR1, MIN(INDEXR, MDR2) )
              ELSE IF (RR .LE. RR_DR3) THEN
        !
        !--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
        !      for mean diameters (Dr) between 0.20 and 0.32 mm:
        !      V(Dr)=2831.*Dr**.80, V in m/s and Dr in m
        !      RR = PI*1000.*N0r0*2831.*Dr**(4+.80) = 7.115e13*Dr**4.80, 
        !        RR in kg/(m**2*s)
        !      Dr (m) = 1.3006e-3*RR**.2083 -> Dr (microns) = 1.3006e3*RR**.2083
        !
                INDEXR=INT( 1.3006E3*RR**.2083 + .5 )
                INDEXR=MAX( MDR2, MIN(INDEXR, MDR3) )
              ELSE IF (RR .LE. RR_DRmax) THEN
        !
        !--- Best fit to mass-weighted fall speeds (V) from rain lookup tables 
        !      for mean diameters (Dr) between 0.32 and 0.45 mm:
        !      V(Dr)=944.8*Dr**.6636, V in m/s and Dr in m
        !      RR = PI*1000.*N0r0*944.8*Dr**(4+.6636) = 2.3745e13*Dr**4.6636,
        !        RR in kg/(m**2*s)
        !      Dr (m) = 1.355e-3*RR**.2144 -> Dr (microns) = 1.355e3*RR**.2144
        !
                INDEXR=INT( 1.355E3*RR**.2144 + .5 )
                INDEXR=MAX( MDR3, MIN(INDEXR, MDRmax) )
              ELSE 
        !
        !--- Assume fixed mean diameter of rain (0.45 mm) for high rain rates, 
        !      instead vary N0r with rain rate
        !
                INDEXR=MDRmax
              ENDIF              ! End IF (RR .LE. RR_DRmin) etc. 
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
              INDEXR=MAX( XMRmin, MIN(CN0r0*RQR**.25, XMRmax) )
            ENDIF
   !
            IF (TC .LT. T_ICE) THEN
              PIACR=-PRLOSS
            ELSE
              DWVr=WV-PCOND-QSW
              DUM=QW+PCOND
              IF (DWVr.LT.0. .AND. DUM.LE.CLIMIT) THEN
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
                RFACTOR=GAMMAR**.5*(RHO/(DIFFUS*DIFFUS*DYNVIS))**C2
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
              ELSE IF (QW .GT. CLIMIT) THEN
                FWR=CRACW*GAMMAR*N0r*ACCRR(INDEXR)
                PRACW=MIN(1.,FWR)*QW
              ENDIF           ! End IF (DWVr.LT.0. .AND. DUM.LE.CLIMIT)
      !
              IF (TC.LT.0. .AND. TCC.LT.0.) THEN
         !
         !--- Biggs (1953) heteorogeneous freezing (e.g., Lin et al., 1983)
         !
                PIACR=CBFR*N0r*RRHO*(EXP(ABFR*TC)-1.)*(FLOAT(INDEXR))**7
                IF (QLICE .GT. CLIMIT) THEN
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
                  DUM2=(DUM1*DUM1+.04*DUM*VSNOW)**.5
                  DUM1=5.E-12*INDEXR*INDEXR+2.E-12*INDEXR*INDEXS           &
                       +.5E-12*INDEXS*INDEXS
                  FIR=MIN(1., CIACR*NLICE*DUM1*DUM2)
            !
            !--- Future?  Should COLLECTION BY SMALL ICE SHOULD BE INCLUDED???
            !
                  PIACR=MIN(PIACR+FIR*QTRAIN, QTRAIN)
                ENDIF        ! End IF (QLICE .GT. CLIMIT)
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
          IF (QWnew .LE. CLIMIT) QWnew=0.
          IF (QW.GT.0. .AND. QWnew.NE.0.) THEN
            DUM=QWnew/QW
            IF (DUM .LT. TOLER) QWnew=0.
          ENDIF
!
!--- Update temperature and water vapor mixing ratios
!
          DELT= XLV1*(PCOND+PIEVP+PICND+PREVP)                             &
               +XLS1*PIDEP+XLF1*(PIACWI+PIACR-PIMLT)
          Tnew=TK+DELT
!
          DELV=-PCOND-PIDEP-PIEVP-PICND-PREVP
          WVnew=WV+DELV
!
!--- Update ice mixing ratios
!
!---
!  * TOT_ICEnew - total mass (small & large) ice after microphysics,
!                 which is the sum of the total mass of large ice in the 
!                 current layer and the flux of ice out of the grid box below
!  * RimeF      - Rime Factor, which is the mass ratio of total (unrimed & 
!                 rimed) ice mass to the unrimed ice mass (>=1)
!  * QInew      - updated mixing ratio of total (large & small) ice in layer
!      -> TOT_ICEnew=QInew*THICK+BLDTRH*QLICEnew*VSNOW
!        -> But QLICEnew=QInew*FLIMASS, so
!      -> TOT_ICEnew=QInew*(THICK+BLDTRH*FLIMASS*VSNOW)
!  * ASNOWnew   - updated accumulation of snow at bottom of grid cell
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
              IF (DUM.LE.CLIMIT .AND. PIDEP.LE.CLIMIT) THEN
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
              ENDIF       ! End IF (DUM.LE.CLIMIT .AND. PIDEP.LE.CLIMIT)
              QInew=TOT_ICEnew/(THICK+BLDTRH*FLIMASS*VSNOW)
              IF (QInew .LE. CLIMIT) QInew=0.
              IF (QI.GT.0. .AND. QInew.NE.0.) THEN
                DUM=QInew/QI
                IF (DUM .LT. TOLER) QInew=0.
              ENDIF
              ASNOWnew=BLDTRH*FLIMASS*VSNOW*QInew
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
   !--- 1st guess time-averaged rain rate at bottom of grid box
   !
            RR=TOT_RAINnew/(DTPH*GAMMAR)
   !
   !--- Use same algorithm as above for calculating mean drop diameter
   !      (IDR, in microns), which is used to estimate the time-averaged
   !      fall speed of rain drops at the bottom of the grid layer.  This
   !      isnt perfect, but the alternative is solving a transcendental 
   !      equation that is numerically inefficient and nasty to program
   !      (coded in earlier versions of GSMCOLUMN prior to 8-22-01).
   !
            IF (RR .LE. RR_DRmin) THEN
              IDR=MDRmin
            ELSE IF (RR .LE. RR_DR1) THEN
              IDR=INT( 1.123E3*RR**.1947 + .5 )
              IDR=MAX( MDRmin, MIN(IDR, MDR1) )
            ELSE IF (RR .LE. RR_DR2) THEN
              IDR=INT( 1.225E3*RR**.2017 + .5 )
              IDR=MAX( MDR1, MIN(IDR, MDR2) )
            ELSE IF (RR .LE. RR_DR3) THEN
              IDR=INT( 1.3006E3*RR**.2083 + .5 )
              IDR=MAX( MDR2, MIN(IDR, MDR3) )
            ELSE IF (RR .LE. RR_DRmax) THEN
              IDR=INT( 1.355E3*RR**.2144 + .5 )
              IDR=MAX( MDR3, MIN(IDR, MDRmax) )
            ELSE 
              IDR=MDRmax
            ENDIF              ! End IF (RR .LE. RR_DRmin)
            VRAIN2=GAMMAR*VRAIN(IDR)
            QRnew=TOT_RAINnew/(THICK+BLDTRH*VRAIN2)
            IF (QRnew .LE. CLIMIT) QRnew=0.
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
!          IF ( (QWnew.GT.CLIMIT .OR. QRnew.GT.CLIMIT .OR. WVnew.GT.DUM)
!     &        .AND. TC.LT.T_ICE )  DBG_logical=.TRUE.
!
!          IF (TC.GT.5. .AND. QInew.GT.CLIMIT) DBG_logical=.TRUE.
!
          IF ((WVnew.LT.CLIMIT .OR. DBG_logical) .AND. PRINT_diag) THEN
   !
            WRITE(6,"(/2(a,i4),2(a,i2))") '{} i=',I_index,' j=',J_index,   &
                                          ' L=',L,' LSFC=',LSFC
   !
            ESW=1000.*FPVS0(Tnew)
            QSWnew=EPS*ESW/(PP-ESW)
            IF (TC.LT.0. .OR. Tnew .LT. 0.) THEN
              ESI=1000.*FPVS(Tnew)
              QSInew=EPS*ESI/(PP-ESI)
            ELSE
              QSI=QSW
              QSInew=QSWnew
            ENDIF
            WSnew=QSInew
            WRITE(6,"(4(a12,g11.4,1x))")                                   &
       '{} TCold=',TC,'TCnew=',Tnew-T0C,'P=',.01*PP,'RHO=',RHO,            &
       '{} THICK=',THICK,'RHold=',WV/WS,'RHnew=',WVnew/WSnew,              &
         'RHgrd=',RHgrd,                                                   &
       '{} RHWold=',WV/QSW,'RHWnew=',WVnew/QSWnew,'RHIold=',WV/QSI,        &
         'RHInew=',WVnew/QSInew,                                           &
       '{} QSWold=',QSW,'QSWnew=',QSWnew,'QSIold=',QSI,'QSInew=',QSInew,   &
       '{} WSold=',WS,'WSnew=',WSnew,'WVold=',WV,'WVnew=',WVnew,           &
       '{} WCold=',WC,'WCnew=',WCnew,'QWold=',QW,'QWnew=',QWnew,           &
       '{} QIold=',QI,'QInew=',QInew,'QRold=',QR,'QRnew=',QRnew,           &
       '{} ARAINold=',ARAIN,'ARAINnew=',ARAINnew,'ASNOWold=',ASNOW,        &
         'ASNOWnew=',ASNOWnew,                                             &
       '{} TOT_RAIN=',TOT_RAIN,'TOT_RAINnew=',TOT_RAINnew,                 &
         'TOT_ICE=',TOT_ICE,'TOT_ICEnew=',TOT_ICEnew,                      &
       '{} BUDGET=',BUDGET,'QTold=',QT,'QTnew=',QTnew                       
   !
            WRITE(6,"(4(a12,g11.4,1x))")                                   &
       '{} DELT=',DELT,'DELV=',DELV,'DELW=',DELW,'DELI=',DELI,             &
       '{} DELR=',DELR,'PCOND=',PCOND,'PIDEP=',PIDEP,'PIEVP=',PIEVP,       &
       '{} PICND=',PICND,'PREVP=',PREVP,'PRAUT=',PRAUT,'PRACW=',PRACW,     &
       '{} PIACW=',PIACW,'PIACWI=',PIACWI,'PIACWR=',PIACWR,'PIMLT=',       &
          PIMLT,                                                           &
       '{} PIACR=',PIACR                                                    
   !
            IF (ICE_logical) WRITE(6,"(4(a12,g11.4,1x))")                  &
       '{} RimeF1=',RimeF1,'GAMMAS=',GAMMAS,'VrimeF=',VrimeF,              &
         'VSNOW=',VSNOW,                                                   &
       '{} INDEXS=',FLOAT(INDEXS),'FLARGE=',FLARGE,'FSMALL=',FSMALL,       &
         'FLIMASS=',FLIMASS,                                               &
       '{} XSIMASS=',XSIMASS,'XLIMASS=',XLIMASS,'QLICE=',QLICE,            &
         'QTICE=',QTICE,                                                   &
       '{} NLICE=',NLICE,'NSmICE=',NSmICE,'PILOSS=',PILOSS,                &
         'EMAIRI=',EMAIRI,                                                 &
       '{} RimeF=',RimeF                                                    
   !
            IF (TOT_RAIN.GT.0. .OR. TOT_RAINnew.GT.0.)                     &
              WRITE(6,"(4(a12,g11.4,1x))")                                 &
       '{} INDEXR1=',FLOAT(INDEXR1),'INDEXR=',FLOAT(INDEXR),               &
         'GAMMAR=',GAMMAR,'N0r=',N0r,                                      &
       '{} VRAIN1=',VRAIN1,'VRAIN2=',VRAIN2,'QTRAIN=',QTRAIN,'RQR=',RQR,   &
       '{} PRLOSS=',PRLOSS,'VOLR1=',THICK+BLDTRH*VRAIN1,                   &
         'VOLR2=',THICK+BLDTRH*VRAIN2
   !
            IF (PRAUT .GT. 0.) WRITE(6,"(a12,g11.4,1x)") '{} QW0=',QW0
   !
            IF (PRACW .GT. 0.) WRITE(6,"(a12,g11.4,1x)") '{} FWR=',FWR
   !
            IF (PIACR .GT. 0.) WRITE(6,"(a12,g11.4,1x)") '{} FIR=',FIR
   !
            DUM=PIMLT+PICND-PREVP-PIEVP
            IF (DUM.GT.0. .or. DWVi.NE.0.)                                 &
              WRITE(6,"(4(a12,g11.4,1x))")                                 &
       '{} TFACTOR=',TFACTOR,'DYNVIS=',DYNVIS,                             &
         'THERM_CON=',THERM_COND,'DIFFUS=',DIFFUS
   !
            IF (PREVP .LT. 0.) WRITE(6,"(4(a12,g11.4,1x))")                &
       '{} RFACTOR=',RFACTOR,'ABW=',ABW,'VENTR=',VENTR,'CREVP=',CREVP,     &
       '{} DWVr=',DWVr,'DENOMW=',DENOMW
   !
            IF (PIDEP.NE.0. .AND. DWVi.NE.0.)                              &
              WRITE(6,"(4(a12,g11.4,1x))")                                 &
       '{} DWVi=',DWVi,'DENOMI=',DENOMI,'PIDEP_max=',PIDEP_max,            &
         'SFACTOR=',SFACTOR,                                               &
       '{} ABI=',ABI,'VENTIL=',VENTIL,'VENTIL1=',VENTI1(INDEXS),           &
         'VENTIL2=',SFACTOR*VENTI2(INDEXS),                                &
       '{} VENTIS=',VENTIS,'DIDEP=',DIDEP
   !
            IF (PIDEP.GT.0. .AND. PCOND.NE.0.)                             &
              WRITE(6,"(4(a12,g11.4,1x))")                                 &
       '{} DENOMW=',DENOMW,'DENOMWI=',DENOMWI,'DENOMF=',DENOMF,            &
          'DUM2=',PCOND-PIACW
   !
            IF (FWS .GT. 0.) WRITE(6,"(4(a12,g11.4,1x))")                  &
       '{} FWS=',FWS                     
   !
            DUM=PIMLT+PICND-PIEVP
            IF (DUM.GT. 0.) WRITE(6,"(4(a12,g11.4,1x))")                   &
       '{} SFACTOR=',SFACTOR,'VENTIL=',VENTIL,'VENTIL1=',VENTI1(INDEXS),   &
         'VENTIL2=',SFACTOR*VENTI2(INDEXS),                                &
       '{} AIEVP=',AIEVP,'DIEVP=',DIEVP,'QSW0=',QSW0,'DWV0=',DWV0       
   !
          ENDIF


!
!----------------------------------------------------------------------
!-------------- Water budget statistics & maximum values --------------
!----------------------------------------------------------------------
!
          IF (PRINT_diag) THEN
            ITdx=MAX( ITLO, MIN( INT(Tnew-T0C), ITHI ) )
            IF (QInew .GT. CLIMIT) NSTATS(ITdx,1)=NSTATS(ITdx,1)+1
            IF (QInew.GT.CLIMIT .AND.  QRnew+QWnew.GT.CLIMIT)              &
              NSTATS(ITdx,2)=NSTATS(ITdx,2)+1
            IF (QWnew .GT. CLIMIT) NSTATS(ITdx,3)=NSTATS(ITdx,3)+1 
            IF (QRnew .GT. CLIMIT) NSTATS(ITdx,4)=NSTATS(ITdx,4)+1
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
            IF (QInew .GT. 0.)                                             &
              QTOT(ITdx,22)=QTOT(ITdx,22)+QInew*THICK/RimeF
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
      REAL FUNCTION CONDENSE (PP, QW, RHgrd, TK, WV)
!
!---------------------------------------------------------------------------------
!------   The Asai (1965) algorithm takes into consideration the release of ------
!------   latent heat in increasing the temperature & in increasing the     ------
!------   saturation mixing ratio (following the Clausius-Clapeyron eqn.).  ------
!---------------------------------------------------------------------------------
!
!FEB  use module_GPVS
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: HIGH_PRES=Selected_Real_Kind(15)
      REAL (KIND=HIGH_PRES), PARAMETER :: CLIMIT=1.E-20,                   &
       RHLIMIT=.001, RHLIMIT1=-RHLIMIT
      REAL, PARAMETER :: CP=1004.6, RD=287.04, RV=461.5, EPS=RD/RV,        &
       RCP=1./CP, RCPRV=RCP/RV
      REAL (KIND=HIGH_PRES) :: COND, SSAT, WCdum
!
      REAL,INTENT(IN) :: RHgrd,QW,PP,WV,TK
      REAL WVdum,Tdum,XLV2,DWV,WS,ESW,XLV1,XLV
integer nsteps
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
      ESW=1000.*FPVS0(Tdum)                     ! Saturation vapor press w/r/t water
      WS=RHgrd*EPS*ESW/(PP-ESW)                 ! Saturation mixing ratio w/r/t water
      DWV=WVdum-WS                              ! Deficit grid-scale water vapor mixing ratio
      SSAT=DWV/WS                               ! Supersaturation ratio
      CONDENSE=0.
nsteps = 0
      DO WHILE ((SSAT.LT.RHLIMIT1 .AND. WCdum.GT.CLIMIT)                   &
                 .OR. SSAT.GT.RHLIMIT)
        nsteps = nsteps + 1
        COND=DWV/(1.+XLV2*WS/(Tdum*Tdum))       ! Asai (1965, J. Japan)
        COND=MAX(COND, -WCdum)                  ! Limit cloud water evaporation
        Tdum=Tdum+XLV1*COND                     ! Updated temperature
        WVdum=WVdum-COND                        ! Updated water vapor mixing ratio
        WCdum=WCdum+COND                        ! Updated cloud water mixing ratio
        CONDENSE=CONDENSE+COND                  ! Total cloud water condensation
        ESW=1000.*FPVS0(Tdum)                   ! Updated saturation vapor press w/r/t water
        WS=RHgrd*EPS*ESW/(PP-ESW)               ! Updated saturation mixing ratio w/r/t water
        DWV=WVdum-WS                            ! Deficit grid-scale water vapor mixing ratio
        SSAT=DWV/WS                             ! Grid-scale supersaturation ratio
      ENDDO
!
      END FUNCTION CONDENSE
!
!#######################################################################
!---------------- Calculate ice deposition at T<T_ICE ------------------
!#######################################################################
!
      REAL FUNCTION DEPOSIT (PP, RHgrd, Tdum, WVdum)
!
!--- Also uses the Asai (1965) algorithm, but uses a different target
!      vapor pressure for the adjustment
!
!FEB  use module_GPVS
      IMPLICIT NONE      
!
      INTEGER, PARAMETER :: HIGH_PRES=Selected_Real_Kind(15)
      REAL (KIND=HIGH_PRES), PARAMETER :: RHLIMIT=.001,                    &
       RHLIMIT1=-RHLIMIT
      REAL, PARAMETER :: CP=1004.6, RD=287.04, RV=461.5, XLS=2.834E6,      &
       EPS=RD/RV, RCP=1./CP, RCPRV=RCP/RV, XLS1=XLS*RCP,                   & 
       XLS2=XLS*XLS*RCPRV
      REAL (KIND=HIGH_PRES) :: DEP, SSAT
!    
      real,INTENT(IN) ::  PP,RHgrd
      real,INTENT(INOUT) ::  WVdum,Tdum
      real ESI,WS,DWV
!
!-----------------------------------------------------------------------
!
      ESI=1000.*FPVS(Tdum)                      ! Saturation vapor press w/r/t ice
      WS=RHgrd*EPS*ESI/(PP-ESI)                 ! Saturation mixing ratio
      DWV=WVdum-WS                              ! Deficit grid-scale water vapor mixing ratio
      SSAT=DWV/WS                               ! Supersaturation ratio
      DEPOSIT=0.
      DO WHILE (SSAT.GT.RHLIMIT .OR. SSAT.LT.RHLIMIT1)
   !
   !--- Note that XLVS2=LS*LV/(CP*RV)=LV*WS/(RV*T*T)*(LS/CP*DEP1), 
   !     where WS is the saturation mixing ratio following Clausius-
   !     Clapeyron (see Asai,1965; Young,1993,p.405) 
   !
        DEP=DWV/(1.+XLS2*WS/(Tdum*Tdum))        ! Asai (1965, J. Japan)
        Tdum=Tdum+XLS1*DEP                      ! Updated temperature
        WVdum=WVdum-DEP                         ! Updated ice mixing ratio
        DEPOSIT=DEPOSIT+DEP                     ! Total ice deposition
        ESI=1000.*FPVS(Tdum)                    ! Updated saturation vapor press w/r/t ice
        WS=RHgrd*EPS*ESI/(PP-ESI)               ! Updated saturation mixing ratio w/r/t ice
        DWV=WVdum-WS                            ! Deficit grid-scale water vapor mixing ratio
        SSAT=DWV/WS                             ! Grid-scale supersaturation ratio
      ENDDO
!
      END FUNCTION DEPOSIT
!
      END SUBROUTINE EGCP01COLUMN 

!#######################################################################
!------- Initialize constants & lookup tables for microphysics ---------
!#######################################################################
!

! SH 0211/2002

      SUBROUTINE etanewinit (GSMDT,DT,DELX,DELY,LOWLYR,restart,            &
         F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,                                 &
         IDS,IDE,JDS,JDE,KDS,KDE,                                          &
         IMS,IME,JMS,JME,KMS,KME,                                          &
         ITS,ITE,JTS,JTE,KTS,KTE                                           )

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
! USAGE: CALL etanewinit FROM SUBROUTINE GSMDRIVE AT MODEL START TIME
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
!     MY_GROWTH_RATES - lookup table for growth of nucleated ice
!     GPVS            - lookup tables for saturation vapor pressure (water, ice)
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
!--- INCLUDE COMMON BLOCKS
!
!FEB  use module_GPVS
      IMPLICIT NONE
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
!
!--- Parameters & data statement for local calculations
!
      REAL, PARAMETER :: C1=1./3., DMR1=.1E-3, DMR2=.2E-3, DMR3=.32E-3,    &
       N0r0=8.E6, N0s0=4.E6, RHOL=1000., RHOS=100., T0C=273.15,            &
       XMR1=1.e6*DMR1, XMR2=1.e6*DMR2, XMR3=1.e6*DMR3                      &
      ,CLIMIT=1.E-12,T_ICE=-10., T_ICEK=T0C+T_ICE
!
      INTEGER, PARAMETER :: MDR1=XMR1, MDR2=XMR2, MDR3=XMR3
!
!     VARIABLES PASSED IN
      integer,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                        &
                           ,IMS,IME,JMS,JME,KMS,KME                        & 
                           ,ITS,ITE,JTS,JTE,KTS,KTE       
!WRF
       INTEGER, DIMENSION(ims:ime,jms:jme),INTENT(INOUT) :: LOWLYR
!
      real, INTENT(IN) ::  DELX,DELY
      real,DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(OUT) ::             &
        F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
      INTEGER, PARAMETER :: ITLO=-60, ITHI=40
!     integer,DIMENSION(ITLO:ITHI,4),INTENT(INOUT) :: NSTATS
!     real,DIMENSION(ITLO:ITHI,5),INTENT(INOUT) :: QMAX
!     real,DIMENSION(ITLO:ITHI,22),INTENT(INOUT) :: QTOT
!     real,INTENT(INOUT) :: PRECtot(2),PRECmax(2)
      real,INTENT(IN) :: DT,GSMDT
      LOGICAL,INTENT(IN) :: restart
!
!     LOCAL VARIABLES
      REAL :: BBFR,DTPH,PI,XNCW,DX,Thour_print
      INTEGER :: I,IM,J,L,K,JTF,KTF,ITF
      INTEGER :: etampnew_unit1
      LOGICAL :: AETA_01
      LOGICAL, PARAMETER :: PRINT_diag=.FALSE.
      LOGICAL :: opened
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      CHARACTER*80 errmess
!
!------------------------------------------------------------------------
!--------- Constants passed through /CMICRO_CONS/ common block ----------
!------------------------------------------------------------------------
!
!--- 9/1/01:  Assume the following functional dependence for 5 - 100 km resolution:
!       RHgrd=0.90 for dx=100 km, 0.98 for dx=5 km, where
!       RHgrd=0.90+0.08*[(100.-dX)/95.]**.5
!
      JTF=MIN0(JTE,JDE-1)
      KTF=MIN0(KTE,KDE-1)
      ITF=MIN0(ITE,IDE-1)
!
      DO J=JTS,JTF
      DO I=ITS,ITF
        LOWLYR(I,J)=1
      ENDDO
      ENDDO
!    
      DO J = jts,jte
      DO K = kts,kte
      DO I= its,ite
         F_ICE_PHY(i,k,j)=0.
         F_RAIN_PHY(i,k,j)=0.
         F_RIMEF_PHY(i,k,j)=1.
      ENDDO
      ENDDO
      ENDDO

!    
!     DX=111.*(DPHD**2+DLMD**2)**.5         ! Model resolution at equator (km)
      DX=((DELX)**2+(DELY)**2)**.5/1000.    ! Model resolution at equator (km)
      DX=MIN(100., MAX(5., DX) )
!     RHgrd=0.90+.08*((100.-DX)/95.)**.5
      RHgrd=1.0
!
      DTPH=GSMDT*60.
!
!--- Create lookup tables for saturation vapor pressure w/r/t water & ice
!
      CALL GPVS
!
!--- Read in various lookup tables
!
      IF ( wrf_dm_on_monitor() ) THEN
        DO i = 31,99
          INQUIRE ( i , OPENED = opened )
          IF ( .NOT. opened ) THEN
            etampnew_unit1 = i
            GOTO 2061
          ENDIF
        ENDDO
        etampnew_unit1 = -1
 2061   CONTINUE
      ENDIF
!
      CALL wrf_dm_bcast_bytes ( etampnew_unit1 , 4 )
!
      IF ( etampnew_unit1 < 0 ) THEN
        CALL wrf_error_fatal ( 'module_mp_etanew: etanewinit: Can not find unused fortran unit to read in lookup table.' )
      ENDIF
!
      IF ( wrf_dm_on_monitor() ) THEN
!!was      OPEN (UNIT=1,FILE="eta_micro_lookup.dat",FORM="UNFORMATTED")
        OPEN(UNIT=etampnew_unit1,FILE="ETAMPNEW_DATA",             &
             FORM="UNFORMATTED",STATUS="OLD",ERR=9061)
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
!      read(etampnew_unit1) my_growth    ! Applicable only for DTPH=180 s
        CLOSE (etampnew_unit1)
      ENDIF
!
      CALL wrf_dm_bcast_bytes ( VENTR1 , size ( VENTR1 ) * 4 )
      CALL wrf_dm_bcast_bytes ( VENTR2 , size ( VENTR2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( ACCRR , size ( ACCRR ) * 4 )
      CALL wrf_dm_bcast_bytes ( MASSR , size ( MASSR ) * 4 )
      CALL wrf_dm_bcast_bytes ( VRAIN , size ( VRAIN ) * 4 )
      CALL wrf_dm_bcast_bytes ( RRATE , size ( RRATE ) * 4 )
      CALL wrf_dm_bcast_bytes ( VENTI1 , size ( VENTI1 ) * 4 )
      CALL wrf_dm_bcast_bytes ( VENTI2 , size ( VENTI2 ) * 4 )
      CALL wrf_dm_bcast_bytes ( ACCRI , size ( ACCRI ) * 4 )
      CALL wrf_dm_bcast_bytes ( MASSI , size ( MASSI ) * 4 )
      CALL wrf_dm_bcast_bytes ( VSNOWI , size ( VSNOWI ) * 4 )
      CALL wrf_dm_bcast_bytes ( VEL_RF , size ( VEL_RF ) * 4 )
!
!--- Calculates coefficients for growth rates of ice nucleated in water
!    saturated conditions, scaled by physics time step (lookup table)
!
      CALL MY_GROWTH_RATES (DTPH)
!     CALL MY_GROWTH_RATES (DTPH,MY_GROWTH)
!
      PI=ACOS(-1.)
!
!--- Constants associated with Biggs (1953) freezing of rain, as parameterized
!    following Lin et al. (JCAM, 1983) & Reisner et al. (1998, QJRMS).
!
      ABFR=-0.66
      BBFR=100.
!     CBFR=20.*PI*PI*BBFR*RHOL*1.E-42
      CBFR=0.
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
!--- Based on rain lookup tables for mean diameters from 0.05 to 0.45 mm
!    * Four different functional relationships of mean drop diameter as 
!      a function of rain rate (RR), derived based on simple fits to 
!      mass-weighted fall speeds of rain as functions of mean diameter
!      from the lookup tables.  
!
      RR_DRmin=N0r0*RRATE(MDRmin)     ! RR for mean drop diameter of .05 mm
      RR_DR1=N0r0*RRATE(MDR1)         ! RR for mean drop diameter of .10 mm
      RR_DR2=N0r0*RRATE(MDR2)         ! RR for mean drop diameter of .20 mm
      RR_DR3=N0r0*RRATE(MDR3)         ! RR for mean drop diameter of .32 mm
      RR_DRmax=N0r0*RRATE(MDRmax)     ! RR for mean drop diameter of .45 mm
!
      RQR_DRmin=N0r0*MASSR(MDRmin)    ! Rain content for mean drop diameter of .05 mm
      RQR_DR1=N0r0*MASSR(MDR1)        ! Rain content for mean drop diameter of .10 mm
      RQR_DR2=N0r0*MASSR(MDR2)        ! Rain content for mean drop diameter of .20 mm
      RQR_DR3=N0r0*MASSR(MDR3)        ! Rain content for mean drop diameter of .32 mm
      RQR_DRmax=N0r0*MASSR(MDRmax)    ! Rain content for mean drop diameter of .45 mm
      C_N0r0=PI*RHOL*N0r0
      CN0r0=1.E6/C_N0r0**.25
      CN0r_DMRmin=1./(PI*RHOL*DMRmin**4)
      CN0r_DMRmax=1./(PI*RHOL*DMRmax**4)
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
!--- CRAUT is proportional to the rate that cloud water is converted by
!      self collection to rain (autoconversion rate)
!
      CRAUT=1.-EXP(-1.E-3*DTPH)
!
!--- QAUT0 is the threshold cloud content for autoconversion to rain 
!      needed for droplets to reach a diameter of 20 microns (following
!      Manton and Cotton, 1977; Banta and Hanson, 1987, JCAM)
!--- QAUT0=1.2567, 0.8378, or 0.4189 g/m**3 for droplet number concentrations
!          of 300, 200, and 100 cm**-3, respectively
!
      XNCW=200.E6                 ! 300 cm**-3 droplet concentration
      QAUT0=PI*RHOL*XNCW*(20.E-6)**3/6.
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
!    SDENS=1.5e3/DENS, DENS=MASSI(INDEXS)/[PI*(1.E-6*INDEXS)**3]
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
     RETURN
!
!-----------------------------------------------------------------------
!
9061 CONTINUE
     WRITE( errmess , '(A37,I4)' ) 'module_mp_etanew: error reading unit ',etampnew_unit1
     CALL wrf_error_fatal(errmess)
!
!-----------------------------------------------------------------------
      END SUBROUTINE etanewinit
!
      SUBROUTINE MY_GROWTH_RATES (DTPH)
!     SUBROUTINE MY_GROWTH_RATES (DTPH,MY_GROWTH)
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
      DATA MY_600 /                                                        &
       5.5e-8, 1.4E-7, 2.8E-7, 6.E-7, 3.3E-6,                              & 
       2.E-6, 9.E-7, 8.8E-7, 8.2E-7, 9.4e-7,                               & 
       1.2E-6, 1.85E-6, 5.5E-6, 1.5E-5, 1.7E-5,                            & 
       1.5E-5, 1.E-5, 3.4E-6, 1.85E-6, 1.35E-6,                            & 
       1.05E-6, 1.E-6, 9.5E-7, 9.0E-7, 9.5E-7,                             & 
       9.5E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7,                                 & 
       9.E-7, 9.E-7, 9.E-7, 9.E-7, 9.E-7 /        ! -31 to -35 deg C
!
!-----------------------------------------------------------------------
!
      DT_ICE=(DTPH/600.)**1.5
      MY_GROWTH=DT_ICE*MY_600
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE MY_GROWTH_RATES
!
      SUBROUTINE GPVS
!     ******************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GPVS        COMPUTE SATURATION VAPOR PRESSURE TABLE
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
! USAGE:  CALL GPVS
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
!     XMIN=180.0
!     XMAX=330.0
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
      END SUBROUTINE GPVS
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
       real, parameter :: CP=1.0046E+3,RD=287.04,RV=4.6150E+2                     &
      ,         TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2                       &
      ,         CLIQ=4.1855E+3,CVAP= 1.8460E+3                                    &
      ,         CICE=2.1060E+3,HSUB=2.8340E+6
!
      real, parameter :: PSATK=PSAT*1.E-3
      real, parameter :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
      real, parameter :: DLDTI=CVAP-CICE                                          &
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
      real,parameter :: CP=1.0046E+3,RD=287.04,RV=4.6150E+2                      &
      ,         TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2                      &
      ,         CLIQ=4.1855E+3,CVAP=1.8460E+3                                    &
      ,         CICE=2.1060E+3,HSUB=2.8340E+6
      real,PARAMETER :: PSATK=PSAT*1.E-3
      real,PARAMETER :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
      real,PARAMETER :: DLDTI=CVAP-CICE                                          &
      ,                 XAI=-DLDT/RV,XBI=XA+HSUB/(RV*TTP)
      real :: T,TR
!-----------------------------------------------------------------------
      TR=TTP/T
      FPVSX0=PSATK*(TR**XA)*EXP(XB*(1.-TR))
!
      END FUNCTION FPVSX0
!
      END MODULE module_mp_etanew
