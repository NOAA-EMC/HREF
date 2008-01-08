!-----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: PHYSICS
!
!-----------------------------------------------------------------------
! these define the various loop range variables
! that were defined in module_MPP. Defined as macros
! here to allow thread-safety/tile callability


! these define the various loop range variables
! that were defined in module_MPP. Defined as macros
! here to allow thread-safety/tile callability





!-----------------------------------------------------------------------
!
      MODULE MODULE_PHYSICS_CALLS
!
!-----------------------------------------------------------------------
      USE MODULE_DOMAIN
      USE MODULE_DM
      USE MODULE_CONFIGURE
      USE MODULE_TILES
      USE MODULE_STATE_DESCRIPTION,ONLY : P_QV,P_QC,P_QR,P_QI,P_QS,P_QG,P_QNI
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_RA_GFDLETA,ONLY : CAL_MON_DAY,ZENITH
      USE MODULE_RADIATION_DRIVER
      USE MODULE_SF_MYJSFC
      USE MODULE_SURFACE_DRIVER
      USE MODULE_PBL_DRIVER
      USE MODULE_CU_BMJ
      USE MODULE_CUMULUS_DRIVER
      USE MODULE_MP_ETANEW
      USE MODULE_MICROPHYSICS_DRIVER
      USE MODULE_MICROPHYSICS_ZERO_OUT
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE RADIATION(NTSD,DT,JULDAY,JULYR,XTIME,JULIAN            &
     &                    ,IHRST,NPHS,GLAT,GLON                         &
     &                    ,NRADS,NRADL                                  &
     &                    ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT   &
     &                    ,PD,RES,PINT,T,Q,MOIST,THS,ALBEDO,EPSR        &
     &                    ,F_ICE,F_RAIN                                 &
     &                    ,SM,HBM2,CLDFRA,N_MOIST,RESTRT                &
     &                    ,RLWTT,RSWTT,RLWIN,RSWIN,RSWINC,RSWOUT        &
     &                    ,RLWTOA,RSWTOA,CZMEAN                         &
     &                    ,CFRACL,CFRACM,CFRACH,SIGT4                   &
     &                    ,ACFRST,NCFRST,ACFRCV,NCFRCV                  &
     &                    ,CUPPT,VEGFRC,SNOW,HTOP,HBOT                  &
     &                    ,Z,SICE,NUM_AEROSOLC,NUM_OZMIXM               &
     &                    ,GRID,CONFIG_FLAGS                            &
     &                    ,RTHRATEN                                     &
     &                    ,IDS,IDE,JDS,JDE,KDS,KDE                      &
     &                    ,IMS,IME,JMS,JME,KMS,KME                      &
     &                    ,ITS,ITE,JTS,JTE,KTS,KTE)
!***  NOTE ***
! RLWIN  - downward longwave at the surface (=TOTLWDN, now a local array)
! RSWIN  - downward shortwave at the surface (=TOTSWDN, now a local array)
! RSWINC - CLEAR-SKY downward shortwave at the surface (=TOTSWDNC, new for AQ)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    RADIATION   RADIATION OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 2002-06-04       
!     
! ABSTRACT:
!     RADIATION SERVES AS THE INTERFACE BETWEEN THE NCEP NONHYDROSTATIC
!     MESOSCALE MODEL AND THE WRF RADIATION DRIVER.
!     
! PROGRAM HISTORY LOG:
!   02-06-04  BLACK      - ORIGINATOR
!   02-09-09  WOLFE      - CONVERTING TO GLOBAL INDEXING
!   04-11-18  BLACK      - THREADED
!   05-12-15  BLACK      - CONVERTED FROM IKJ TO IJK
!     
! USAGE: CALL RADIATION FROM SOLVE_NMM      
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM 
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,IHRST,JULDAY,JULYR                          &
     &                     ,N_MOIST,NPHS,NRADL,NRADS,NTSD               &
     &                     ,NUM_AEROSOLC,NUM_OZMIXM
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: NCFRCV,NCFRST
!
      REAL,INTENT(IN) :: DT,PDTOP,PT,XTIME,JULIAN
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ALBEDO              &
     &                                             ,EPSR,GLAT,GLON      &
     &                                             ,HBM2                &
     &                                             ,PD,RES,SICE,SM      &
     &                                             ,SNOW,THS,VEGFRC
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CUPPT            

!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: Q,T,Z
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: F_ICE       &   !<--- Used only with physics (IKJ)
     &                                                     ,F_RAIN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: RTHRATEN     !<--- Used only with physics (IKJ)
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST)                   &
                                                 ,INTENT(INOUT) :: MOIST
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACFRCV,ACFRST    &
     &                                                ,HBOT,HTOP        &
     &                                                ,RLWIN,RLWTOA     &
     &                                                ,RSWIN,RSWOUT     &
     &                                                ,RSWINC,RSWTOA
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: PINT     &
     &                                                        ,RLWTT    &
     &                                                        ,RSWTT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CFRACH,CFRACL    &
     &                                                ,CFRACM,CZMEAN    &
     &                                                ,SIGT4
!
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CLDFRA
!
      LOGICAL,INTENT(IN) :: RESTRT
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: I,ICLOUD,IENDX,II,ISTAT,J,JDAY,JMONTH,K,KMNTH,N,NRAD
!
      INTEGER,DIMENSION(3) :: IDAT
      INTEGER,DIMENSION(12) :: MONTH=(/31,28,31,30,31,30,31,31          &
     &                                ,30,31,30,31/)
!
      REAL :: CAPA,DAYI,DPL,FICE,FRAIN,GMT,HOUR,PLYR,PSFC               &
     &       ,QI,QR,QW,RADT,TIMES,WC,TDUM
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUPPTR,CZEN,HBOTR,HTOPR        &
     &                                  ,PDSL,REXNSFC,SWNETDN           &
     &                                  ,TOT,TOTLWDN,TOTSWDN,TOTSWDNC   &
     &                                  ,TSFC,XLAND,XLAT,XLON
!
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: CLFR,DZ                &   !<--- Used only with physics (IKJ)
     &                                          ,P8W,P_PHY,PI_PHY       &
     &                                          ,RR,T8W                 &
     &                                          ,THRATENLW,THRATENSW    &
     &                                          ,TH_PHY,T_PHY,Z_PHY
!
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: MOIST_TRANS
!
      LOGICAL :: WARM_RAIN
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!*****
!***** NOTE: THIS IS HARDWIRED FOR CALLS TO LONGWAVE AND SHORTWAVE
!*****       AT EQUAL INTERVALS
!*****
!-----------------------------------------------------------------------
!
      NRAD=NRADS
      RADT=DT*NRADS/60.
!
!-----------------------------------------------------------------------
!
      ALLOCATE(MOIST_TRANS(IMS:IME,KMS:KME,JMS:JME,N_MOIST),STAT=ISTAT)
!
!-----------------------------------------------------------------------
!
      CAPA=R_D/CP
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
!
        PDSL(I,J)=PD(I,J)*RES(I,J)
        P8W(I,KTE+1,J)=PT
        XLAT(I,J)=GLAT(I,J)/DEGRAD
        XLON(I,J)=GLON(I,J)/DEGRAD
        XLAND(I,J)=SM(I,J)+1.
        PSFC=PD(I,J)+PDTOP+PT
        REXNSFC(I,J)=(PSFC*1.E-5)**CAPA
        TSFC(I,J)=THS(I,J)*REXNSFC(I,J)
        T8W(I,KTS,J)=TSFC(I,J)
        P8W(I,KTS,J)=ETA1(KTS)*PDTOP+ETA2(KTS)*PDSL(I,J)+PT
        Z_PHY(I,KTS,J)=Z(I,J,KTS)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  FILL THE SINGLE-COLUMN INPUT
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dpl,i,j,k,plyr,ql,qr,tl)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
        DO K=KTS,KTE
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          QL(K)=MAX(Q(I,J,K),EPSQ)
          PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL(I,J)+PT
          TL(K)=T(I,J,K)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(1.+P608*QL(K)))
          T_PHY(I,K,J)=TL(K)
          TH_PHY(I,K,J)=TL(K)*(1.E5/PLYR)**CAPA
          P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL(I,J)+PT
          P_PHY(I,K,J)=PLYR
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          DZ(I,K,J)=TL(K)*(P608*QL(K)+1.)*R_D                           &
     &                 *(P8W(I,K,J)-P8W(I,K+1,J))                       &
     &                 /(P_PHY(I,K,J)*G)
!
          RTHRATEN(I,K,J)=0.
          THRATENLW(I,K,J)=0.
          THRATENSW(I,K,J)=0.
!         PM2_5_DRY(I,K,J)=0.
!         PM2_5_WATER(I,K,J)=0.

        ENDDO
!
        DO K=KTS+1,KTE
          T8W(I,K,J)=0.5*(TL(K-1)+TL(K))
        ENDDO
        T8W(I,KTE+1,J)=-1.E20
!
      ENDDO
      ENDDO
!
      ICLOUD=999
!
      GMT=REAL(IHRST)
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          CLDFRA(I,J,K)=0.
        ENDDO
        ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=JMS,JME
        DO I=IMS,IME
          CFRACH(I,J)=0.
          CFRACL(I,J)=0.
          CFRACM(I,J)=0.
          CZMEAN(I,J)=0.
          SIGT4(I,J)=0.
          TOTSWDN(I,J)=0.   ! TOTAL (clear+cloudy sky) shortwave down at the surface
          TOTSWDNC(I,J)=0.  ! CLEAR SKY shortwave down at the surface
          SWNETDN(I,J)=0.   ! Net (down - up) total (clear+cloudy sky) shortwave at the surface
          TOTLWDN(I,J)=0.   ! Total longwave down at the surface
          CUPPTR(I,J)=CUPPT(I,J)   ! Temporary array set to zero in radiation
!
!-- NOTE:  HBOTR, HTOPR are passed into radiation and set equal to HBOT, HTOP.  HBOT, HTOP are
!          reset to clear sky values to be used by the ARW.  At the bottom of this subroutine, 
!          HBOT, HTOP are re-defined again to values stored in HBOTR, HTOPR.  HBOT, HTOP are 
!          reset to clear sky values after the call to radiation and after the top of the hour
!          in subroutine CUCNVC below.
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST ARRAY (IJK) FOR THE PHYSICS (IKJ).
!***  REMEMBER THAT MOIST AND MOIST_TRANS ARE ONLY USED WITH
!***  THE PHYSICS AND THUS THE P_QV SLOT (=2) IS MIXING RATIO,
!***  NOT SPECIFIC HUMIDITY.
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          MOIST_TRANS(I,K,J,N)=MOIST(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!***  CALL THE INNER DRIVER.
!
!-----------------------------------------------------------------------
!
      CALL SET_TILES(GRID,IDS+1,IDE-1,JDS+2,JDE-2,ITS,ITE,JTS,JTE)
!
      CALL RADIATION_DRIVER(                                            &
     &                  IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &                 ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &                 ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &                 ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &                 ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
     &                 ,ITIMESTEP=NTSD,DT=DT                            &
     &                 ,RTHRATENLW=THRATENLW,RTHRATENSW=THRATENSW       &
     &                 ,RTHRATEN=RTHRATEN                               &
     &                 ,GLW=TOTLWDN,GSW=SWNETDN,SWDOWN=TOTSWDN          &
     &                 ,XLAT=XLAT,XLONG=XLON,ALBEDO=ALBEDO,EMISS=EPSR   &
     &                 ,XICE=SICE,XLAND=XLAND,Z=Z,TSK=TSFC              &
     &                 ,N_AEROSOLC=NUM_AEROSOLC,PAERLEV=GRID%PAERLEV    &
     &                 ,CAM_ABS_DIM1=GRID%CAM_ABS_DIM1                  &
     &                 ,CAM_ABS_DIM2=GRID%CAM_ABS_DIM2                  &
     &                 ,CAM_ABS_FREQ_S=GRID%CAM_ABS_FREQ_S              &
     &                 ,LEVSIZ=GRID%LEVSIZ,N_OZMIXM=NUM_OZMIXM          &
     &                 ,HTOP=HTOP,HBOT=HBOT,CUPPT=CUPPTR                &
     &                 ,HTOPR=HTOPR,HBOTR=HBOTR                         &
     &                 ,VEGFRA=VEGFRC,SNOW=SNOW                         &
     &                 ,RHO=RR,P8W=P8W,P=P_PHY,PI=PI_PHY                &
     &                 ,DZ8W=DZ,T=T_PHY,T8W=T8W,GMT=GMT                 &
     &                 ,JULDAY=JULDAY,JULYR=JULYR,NPHS=NPHS             &
     &                 ,JULIAN=JULIAN,XTIME=XTIME                       &
     &                 ,LW_PHYSICS=CONFIG_FLAGS%RA_LW_PHYSICS           &
     &                 ,SW_PHYSICS=CONFIG_FLAGS%RA_SW_PHYSICS           &
     &                 ,RADT=RADT,RA_CALL_OFFSET=GRID%RA_CALL_OFFSET    &
     &                 ,STEPRA=NRAD,ICLOUD=ICLOUD                       &
     &                 ,WARM_RAIN=WARM_RAIN                             & 
     &                 ,SWDOWNC=TOTSWDNC,CLDFRA=CLFR                    &
     &                 ,RSWTOA=RSWTOA,RLWTOA=RLWTOA                     &
     &                 ,CZMEAN=CZMEAN,CFRACL=CFRACL                     &
     &                 ,CFRACM=CFRACM,CFRACH=CFRACH                     &
     &                 ,ACFRST=ACFRST,NCFRST=NCFRST                     &
     &                 ,ACFRCV=ACFRCV,NCFRCV=NCFRCV                     &
     &                 ,F_ICE_PHY=F_ICE,F_RAIN_PHY=F_RAIN               &
     &                 ,QV=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV      &
     &                 ,QC=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC      &
     &                 ,QR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR      &
     &                 ,QI=MOIST_TRANS(IMS,KMS,JMS,P_QI),F_QI=F_QI      &
     &                 ,QS=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS      &
     &                 ,QG=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG     )

!
!-----------------------------------------------------------------------
!
!***  UPDATE FLUXES AND TEMPERATURE TENDENCIES.
!
!-----------------------------------------------------------------------
!***  SHORTWAVE
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
      nrads_block: IF(MOD(NTSD,NRADS)==0)THEN
!-----------------------------------------------------------------------
!
        IF(CONFIG_FLAGS%RA_SW_PHYSICS/=GFDLSWSCHEME)THEN
!
!-----------------------------------------------------------------------
!***  COMPUTE CZMEAN FOR NON-GFDL SHORTWAVE
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
          DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
          DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
            CZMEAN(I,J)=0.
            TOT(I,J)=0.
          ENDDO
          ENDDO
!
          CALL CAL_MON_DAY(JULDAY,JULYR,JMONTH,JDAY)
          IDAT(1)=JMONTH
          IDAT(2)=JDAY
          IDAT(3)=JULYR
!
          DO II=0,NRADS,NPHS
            TIMES=NTSD*DT+II*DT
            CALL ZENITH(TIMES,DAYI,HOUR,IDAT,IHRST,GLON,GLAT,CZEN       &
     &                 ,max(ids+(0),its-(0)),min(ide-(0),ite+(0)),max(jds+(0),jts-(0)),min(jde-(0),jte+(0))                             &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!$omp parallel do                                                       &
!$omp& private(i,j)
            DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
            DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
              IF(CZEN(I,J)>0.)THEN
                CZMEAN(I,J)=CZMEAN(I,J)+CZEN(I,J)
                TOT(I,J)=TOT(I,J)+1.
              ENDIF
            ENDDO
            ENDDO
!
          ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j)
          DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
          DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
            IF(TOT(I,J)>0.)CZMEAN(I,J)=CZMEAN(I,J)/TOT(I,J)
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
!***  COMPUTE TOTAL SFC SHORTWAVE DOWN FOR NON-GFDL SCHEMES
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
          DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
!
            IF(HBM2(I,J)>0.5)THEN
              TOTSWDN(I,J)=SWNETDN(I,J)/(1.-ALBEDO(I,J))  
!
!--- No value currently available for clear-sky solar fluxes from
!    non GFDL schemes, though its needed for air quality forecasts.
!    For the time being, set to the total downward solar fluxes.
!
              TOTSWDNC(I,J)=TOTSWDN(I,J)
            ENDIF
!
          ENDDO
          ENDDO
!
        ENDIF   !End non-GFDL block
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j)
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          IENDX=min(ide-(1),ite+(0))
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=max(ids+(1),its-(0)),IENDX
!
            RSWIN(I,J)=TOTSWDN(I,J)
            RSWINC(I,J)=TOTSWDNC(I,J)
            RSWOUT(I,J)=TOTSWDN(I,J)-SWNETDN(I,J)
!
          ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k)
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          IENDX=min(ide-(1),ite+(0))
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=max(ids+(1),its-(0)),IENDX
            DO K=KTS,KTE
              RSWTT(I,J,K)=THRATENSW(I,K,J)*PI_PHY(I,K,J)
            ENDDO
!
          ENDDO
        ENDDO
!
      ENDIF nrads_block
!
!-----------------------------------------------------------------------
!***  LONGWAVE
!-----------------------------------------------------------------------
!
      nradl_block: IF(MOD(NTSD,NRADL)==0)THEN
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j)
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          IENDX=min(ide-(1),ite+(0))
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=max(ids+(1),its-(0)),IENDX
!
            IF(HBM2(I,J)>0.5)THEN
              TDUM=T(I,J,KTS)
              SIGT4(I,J)=STBOLT*TDUM*TDUM*TDUM*TDUM
              RLWIN(I,J)=TOTLWDN(I,J)
            ENDIF
!
          ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k)
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          IENDX=min(ide-(1),ite+(0))
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
!
          DO K=KTS,KTE
          DO I=max(ids+(1),its-(0)),IENDX
            IF(HBM2(I,J)>0.5)THEN
                RLWTT(I,J,K)=THRATENLW(I,K,J)*PI_PHY(I,K,J)
            ENDIF
          ENDDO
          ENDDO
!
        ENDDO
!
      ENDIF nradl_block
!
!-----------------------------------------------------------------------
!***  STORE 3D CLOUD FRACTIONS.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k)
      DO K=KTS,KTE
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          IENDX=min(ide-(1),ite+(0))
          IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
          DO I=max(ids+(1),its-(0)),IENDX
            CLDFRA(I,J,K)=CLFR(I,K,J)
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  RESET THE DIAGNOSTIC CONVECTIVE CLOUD TOPS/BOTTOMS AFTER
!***  EACH RADIATION CALL.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        IENDX=min(ide-(1),ite+(0))
        IF(MOD(J,2)==0.AND.ITE==IDE)IENDX=IENDX-1
        DO I=max(ids+(1),its-(0)),IENDX
          HBOT(I,J)=HBOTR(I,J)
          HTOP(I,J)=HTOPR(I,J)
          CUPPT(I,J)=CUPPTR(I,J)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  ZERO OUT BOUNDARY ROWS.
!-----------------------------------------------------------------------
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        IF(HBM2(I,J)<0.5)THEN
          ACFRST(I,J)=0.
          ACFRCV(I,J)=0.
          CFRACL(I,J)=0.
          CFRACM(I,J)=0.
          CFRACH(I,J)=0.
          RSWTOA(I,J)=0.
          RLWTOA(I,J)=0.
        ENDIF
      ENDDO
      ENDDO
!
!
!-----------------------------------------------------------------------
!***  UPDATE THE PROGNOSTIC MOIST ARRAY.
!-----------------------------------------------------------------------
!
      DO N=2,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          MOIST(I,J,K,N)=MOIST_TRANS(I,K,J,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
      DEALLOCATE(MOIST_TRANS,STAT=ISTAT)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE RADIATION
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE TURBL(NTSD,DT,NPHS,RESTRT                              &
     &                ,N_MOIST,NSOIL,SLDPTH,DZSOIL                      &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT       &
     &                ,SM,HBM2,VBM2,DX_ARRAY,DFRLG                      &
     &                ,CZEN,CZMEAN,SIGT4,RLWIN,RSWIN,RADOT              &
!- RLWIN/RSWIN - downward longwave/shortwave at the surface (also TOTLWDN/TOTSWDN in RADIATION)
     &                ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,SR              &
     &                ,Q2,U,V,THS,TSFC,SST,PREC,SNO                     &
     &                ,FIS,Z0,Z0BASE,USTAR,PBLH,LPBL,EL_MYJ             &
     &                ,MOIST,RMOL                                       &
     &                ,EXCH_H,AKHS,AKMS,AKHS_OUT,AKMS_OUT               &
     &                ,THZ0,QZ0,UZ0,VZ0,QS,MAVAIL                       &
     &                ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF          &
     &                ,IVGTYP,ISLTYP,VEGFRC,SHDMIN,SHDMAX,GRNFLX        &
     &                ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB       &
     &                ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR                &
     &                ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR            &
     &                ,T2,QSG,QVG,QCG,SOILT1,TSNAV,SMFR3D,KEEPFR3DFLAG  &
     &                ,TWBS,QWBS,SFCSHX,SFCLHX,SFCEVP                   &
     &                ,POTEVP,POTFLX,SUBSHX                             &
     &                ,APHTIM,ARDSW,ARDLW,ASRFC                         &
     &                ,RSWOUT,RSWTOA,RLWTOA                             &
     &                ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA          &
     &                ,UZ0H,VZ0H,DUDT,DVDT                              & 
     &                ,RTHBLTEN,RQVBLTEN                                & 
     &                ,PCPFLG,DDATA                                     & ! PRECIP ASSIM
     &                ,GRID,CONFIG_FLAGS                                &
     &                ,IHE,IHW,IVE,IVW                                  &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    TURBL       TURBULENCE OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-04-19       
!     
! ABSTRACT:
!     TURBL DRIVES THE TURBULENCE SCHEMES
!     
! PROGRAM HISTORY LOG (with changes to called routines) :
!   95-03-15  JANJIC     - ORIGINATOR OF THE SUBROUTINES CALLED
!   BLACK & JANJIC       - ORIGINATORS OF THE DRIVER
!   95-03-28  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   96-03-29  BLACK      - ADDED EXTERNAL EDGE; REMOVED SCRCH COMMON
!   96-07-19  MESINGER   - ADDED Z0 EFFECTIVE
!   98-~??  TUCCILLO   - MODIFIED FOR CLASS VIII PARALLELISM
!   98-10-27  BLACK      - PARALLEL CHANGES INTO MOST RECENT CODE
!   02-01-10  JANJIC     - MOIST TURBULENCE (DRIVER, MIXLEN, VDIFH)
!   02-01-10  JANJIC     - VERT. DIF OF Q2 INCREASED (Grenier & Bretherton)
!   02-02-02  JANJIC     - NEW SFCDIF
!   02-04-19  BLACK      - ORIGINATOR OF THIS OUTER DRIVER FOR WRF
!   02-05-03  JANJIC     - REMOVAL OF SUPERSATURATION AT 2m AND 10m
!   04-11-18  BLACK      - THREADED
!   05-12-15  BLACK      - CONVERTED FROM IKJ TO IJK
!     
! USAGE: CALL TURBL FROM SOLVE_NMM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,NPHS,NSOIL,NTSD
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ISLTYP,IVGTYP
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: LPBL
!
      REAL,INTENT(IN) :: DT,PDTOP,PT
!
      REAL,INTENT(INOUT) :: APHTIM,ARDSW,ARDLW,ASRFC
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: DFRLG,ETA1,ETA2
!
      REAL,DIMENSION(NSOIL),INTENT(IN) :: DZSOIL,SLDPTH
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CZEN,CZMEAN         &
     &                                             ,DX_ARRAY            &
     &                                             ,FIS,HBM2            &
     &                                             ,PD,RES              &
     &                                             ,RLWIN,RLWTOA        &
     &                                             ,RSWIN,RSWOUT,RSWTOA &
     &                                             ,SHDMIN,SHDMAX       &
!    &                                             ,SICE,SIGT4,SM,SR    & !Bandaid
     &                                             ,SICE,SIGT4          &
     &                                             ,SST,TG,VBM2,VEGFRC
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: SM,EPSR,SR         !Bandaid
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ALBASE,MXSNAL
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACSNOM,ACSNOW    &
     &                                                ,AKHS,AKMS        &
     &                                                ,ALBEDO           &
     &                                                ,BGROFF,CMC       &
     &                                                ,MAVAIL           &
     &                                                ,PBLH,POTEVP      &
     &                                                ,POTFLX,PREC      &
     &                                                ,QCG,QS,QSG       &
     &                                                ,QVG,QZ0          &
     &                                                ,SFCEVP           &
     &                                                ,SFCLHX,SFCSHX    &
     &                                                ,SI,SMSTOT        &
     &                                                ,SNO,SNOPCX       &
     &                                                ,SOILT1           &
     &                                                ,SSROFF,SUBSHX    &
     &                                                ,T2,THS,THZ0      &
     &                                                ,TSFC,TSNAV       &
     &                                                ,USTAR,UZ0,UZ0H   &
     &                                                ,VZ0,VZ0H         &
     &                                                ,Z0,Z0BASE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: AKHS_OUT,AKMS_OUT  &
     &                                              ,ALWIN,ALWOUT       &
     &                                              ,ALWTOA,ASWIN       &
     &                                              ,ASWOUT,ASWTOA      &
     &                                              ,PSHLTR,Q10,QSHLTR  &
     &                                              ,TH10,TSHLTR        &
     &                                              ,U10,V10
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: GRNFLX,QWBS,RADOT  &
                                                    ,SFCEXC,SMSTAV      &
                                                    ,SOILTB,TWBS
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM      &
     &                                                        ,DUDT     &
     &                                                        ,DVDT     &
     &                                                        ,Q,Q2     &
     &                                                        ,T,U,V
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: F_ICE    &   !<--- Used only in physics (IKJ)
     &                                                        ,F_RAIN   &
     &                                                        ,RQVBLTEN &
     &                                                        ,RTHBLTEN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: EL_MYJ     &   !<--- Used only in physics (IKJ)
     &                                                      ,EXCH_H
!
      REAL,DIMENSION(IMS:IME,NSOIL,JMS:JME),INTENT(INOUT) :: KEEPFR3DFLAG & !<--- Used only in physics (IKJ)
     &                                                      ,SH2O,SMC     &
     &                                                      ,SMFR3D,STC
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST)                   &
     &                                           ,INTENT(INOUT) :: MOIST
!
      LOGICAL,INTENT(IN) :: RESTRT
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!  For precip assimilation:
      LOGICAL,INTENT(IN) :: PCPFLG
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DDATA
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
      INTEGER :: I,I_M,IDUMMY,IEND,ISFFLX,ISTAT,ISTR,J,K,KOUNT_ALL      &
     &          ,LENGTH_ROW,LLIJ,LLYR,N,SST_UPDATE,UCMCALL
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: KPBL,LOWLYR
!
      REAL :: TRESH=0.95
!
      REAL :: ALTITUDE,CWML,DQDT,DTDT,DTPHS,DX,DZHALF,FACTR,FACTRL      &
     &       ,G_INV,PLYR,PSFC,QI,QL,QOLD,QR,QW,RATIOMX,RDTPHS      &
     &       ,ROG,RWMSK,SDEPTH,SNO_FACTR,TL,TLMH,TLMH4,TNEW,TSFC2       &
     &       ,U_FRAME,V_FRAME,XLVRW
!
      REAL :: APES,CKLQ,EXNER,FACTOR,FFS,PQ0X,Q2SAT,QFC1,QLOWX,RLIVWV   &
     &       ,THBOT
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: BR,CHKLOWQ,CT,CWMLOW,ELFLX     &
     &                                  ,EXNSFC,FACTRS,FLHC,FLQC,GZ1OZ0 &
     &                                  ,ONE,PDSL,PLM,PSFC_OUT,PSIH     &
     &                                  ,PSIM,Q2X,QLOW,RAIN,RAINBL      &
     &                                  ,RLW_DN_SFC,RMOL,RSW_NET_SFC    &
     &                                  ,RSW_DN_SFC                     &
     &                                  ,SFCEVPX,SFCZ,SNOW,SNOWC,SNOWH  &
     &                                  ,TH2X,THLOW,TLOW,VGFRCK         &
     &                                  ,WSPD,XLAND
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DUDT_PHY,DVDT_PHY,DZ   &
     &                                          ,P_PHY,P8W,PI_PHY       &
     &                                          ,RQCBLTEN,RQIBLTEN      &
     &                                          ,RR                     &
     &                                          ,T_PHY,TH_PHY,TKE       &
     &                                          ,U_PHY,V_PHY,Z
!
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: MOIST_TRANS
!
      REAL,DIMENSION(IMS:IME,NSOIL,JMS:JME) :: ZERO_SOIL
!
      LOGICAL :: E_BDY,WARM_RAIN
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ALLOCATE(MOIST_TRANS(IMS:IME,KMS:KME,JMS:JME,N_MOIST),STAT=ISTAT)
!
      UCMCALL=CONFIG_FLAGS%UCMCALL
!
      DTPHS=NPHS*DT
      RDTPHS=1./DTPHS
      G_INV=1./G
      ROG=R_D*G_INV
      FACTOR=-XLV*RHOWATER/DTPHS
!
      U_FRAME=0.
      V_FRAME=0.
!
      IDUMMY=0
      ISFFLX=1
      DX=0.
      SST_UPDATE=0
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=JMS,JME
      DO I=IMS,IME
        UZ0H(I,J)=0.
        VZ0H(I,J)=0.
        ONE(I,J)=1.
        RMOL(I,J)=0.     !Reciprocal of Monin-Obukhov length
        SFCEVPX(I,J)=0.  !Dummy for accumulated latent energy, not flux
      ENDDO
      ENDDO
!
      IF(MODEL_CONFIG_REC%SF_SURFACE_PHYSICS(GRID%ID)==99)THEN
        SNO_FACTR=1.
      ELSE
        SNO_FACTR=0.001
      ENDIF
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
      DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
        LOWLYR(I,J)=1
        VGFRCK(I,J)=100.*VEGFRC(I,J)
        SNOW(I,J)=SNO(I,J)
        SNOWH(I,J)=SI(I,J)*SNO_FACTR
        XLAND(I,J)=SM(I,J)+1.
        T2(I,J)=TSFC(I,J)
      ENDDO
      ENDDO
!
      IF(NTSD==0)THEN
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
        DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
          Z0BASE(I,J)=Z0(I,J)
          IF(SM(I,J)>0.5.AND.SICE(I,J)>0.5)THEN  !Bandaid
            SM(I,J)=0.        
          ENDIF              
        ENDDO
        ENDDO
      ENDIF
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
      DO K=KTS,KTE+1
      DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
        Z(I,K,J)=0.
        DZ(I,K,J)=0.
        EXCH_H(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!***  PREPARE NEEDED ARRAYS FOR CALLING THE INNER DRIVER.
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(factrl,i,j,llij,tlmh)
      DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
      DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
!
        PDSL(I,J)=PD(I,J)*RES(I,J)
!!!     PSFC=PD(I,J)+PDTOP+PT
!!!     P8W(I,KTS,J)=PSFC
        P8W(I,KTS,J)=PINT(I,J,KTS)
        PSFC=PINT(I,J,KTS)
        LOWLYR(I,J)=KTS     !<----  The lowest model layer counted from the bottom.
        EXNSFC(I,J)=(1.E5/PSFC)**CAPA
        THS(I,J)=(SST(I,J)*EXNSFC(I,J))*SM(I,J)+THS(I,J)*(1.-SM(I,J))
        TSFC(I,J)=THS(I,J)/EXNSFC(I,J)
        SFCZ(I,J)=FIS(I,J)*G_INV
!YL     RAIN(I,J)=PREC(I,J)*RHOWATER
        IF (PCPFLG.AND.DDATA(I,J)<100.)THEN
          RAIN(I,J)=DDATA(I,J)*RHOWATER
        ELSE
          RAIN(I,J)=PREC(I,J)*RHOWATER
        ENDIF
!YL
        RAINBL(I,J)=0.
        IF(SNO(I,J)>0.)SNOWC(I,J)=1.
        LLIJ=LOWLYR(I,J)
        PLM(I,J)=(PINT(I,J,LLIJ)+PINT(I,J,LLIJ+1))*0.5
        TH2X(I,J)=T(I,J,LLIJ)*(1.E5/PLM(I,J))**CAPA
        Q2X(I,J)=Q(I,J,LLIJ)
!
!-----------------------------------------------------------------------
!*** LONG AND SHORTWAVE FLUX AT GROUND SURFACE
!-----------------------------------------------------------------------
!
        IF(CZMEAN(I,J)>0.)THEN
          FACTRS(I,J)=CZEN(I,J)/CZMEAN(I,J)
        ELSE
          FACTRS(I,J)=0.
        ENDIF
!
        IF(SIGT4(I,J)>0.)THEN
          TLMH=T(I,J,LLIJ)
          FACTRL=STBOLT*TLMH*TLMH*TLMH*TLMH/SIGT4(I,J)
        ELSE
          FACTRL=0.
        ENDIF
!     
!- RLWIN/RSWIN - downward longwave/shortwave at the surface
!
        RLW_DN_SFC(I,J)=RLWIN(I,J)*HBM2(I,J)*FACTRL
        RSW_NET_SFC(I,J)=(RSWIN(I,J)-RSWOUT(I,J))*HBM2(I,J)*FACTRS(I,J)
!
!- Instant downward solar for nmm_lsm
!
        RSW_DN_SFC(I,J)=RSWIN(I,J)*HBM2(I,J)*FACTRS(I,J)
!
        Z(I,KTS,J)=SFCZ(I,J)
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  FILL THE ARRAYS FOR CALLING THE INNER DRIVER.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(cwml,i,j,k,plyr,qi,ql,qr,qw,tl)
      DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
        DO K=KTS,KTE
        DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
          Q2(I,J,K)=MAX(Q2(I,J,K)*HBM2(I,J),EPSQ2)
          QL=MAX(Q(I,J,K),EPSQ)
          PLYR=(PINT(I,J,K)+PINT(I,J,K+1))*0.5
!!!       PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL(I,J)+PT
          TL=T(I,J,K)
          CWML=CWM(I,J,K)
!
          RR(I,K,J)=PLYR/(R_D*TL)
          T_PHY(I,K,J)=TL
!
          EXNER=(1.E5/PLYR)**CAPA
          PI_PHY(I,K,J)=1./EXNER
          TH_PHY(I,K,J)=TL*EXNER
          P8W(I,K+1,J)=PINT(I,J,K+1)
!!!       P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL(I,J)+PT
          P_PHY(I,K,J)=PLYR
          TKE(I,K,J)=0.5*Q2(I,J,K)
!
          RTHBLTEN(I,K,J)=0.
          RQVBLTEN(I,K,J)=0.
          RQCBLTEN(I,K,J)=0.
          RQIBLTEN(I,K,J)=0.
!
          Z(I,K+1,J)=Z(I,K,J)+TL/PLYR                                   &
     &              *(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))*ROG            &
                    *(Q(I,J,K)*P608-CWML+1.)
          DZ(I,K,J)=Z(I,K+1,J)-Z(I,K,J)
        ENDDO
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,llyr,qlowx)
      DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
      DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
        TWBS(I,J)=0.
        QWBS(I,J)=0.
        LLYR=LOWLYR(I,J)
        THLOW(I,J)=TH_PHY(I,LLYR,J)
        TLOW(I,J)=T_PHY(I,LLYR,J)
        QLOW(I,J)=MAX(Q(I,J,LLYR),EPSQ)
        QLOWX=QLOW(I,J)/(1.-QLOW(I,J))
        QLOW(I,J)=QLOWX/(1.+QLOWX)
        CWMLOW(I,J)=CWM(I,J,LLYR)
        PBLH(I,J)=MAX(PBLH(I,J),0.)
        PBLH(I,J)=MIN(PBLH(I,J),Z(I,KTE,J))
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  COMPUTE VELOCITY COMPONENTS AT MASS POINTS
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO K=KTS,KTE
        DO J=max(jds+(1),jts-(1)),min(jde-(1),jte+(1))
          DO I=max(ids+(0),its-(1)),min(ide-(0),ite+(1))
            U_PHY(I,K,J)=(U(I+IHE(J),J,K)+U(I+IHW(J),J,K)               &
     &                   +U(I,J+1,K)+U(I,J-1,K))                        &
     &                   *0.25
            V_PHY(I,K,J)=(V(I+IHE(J),J,K)+V(I+IHW(J),J,K)               &
     &                   +V(I,J+1,K)+V(I,J-1,K))                        &
     &                   *0.25
          ENDDO
        ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,iend,istr,j)
      DO J=max(jds+(1),jts-(1)),min(jde-(1),jte+(1))
        IF(MOD(J,2)==0)THEN
          ISTR=max(ids+(0),its-(1))
          IEND=MIN(min(ide-(0),ite+(1)),IDE-1)
        ELSE
          ISTR=MAX(max(ids+(0),its-(1)),IDS+1)
          IEND=MIN(min(ide-(0),ite+(1)),IDE-1)
        ENDIF
!     
        DO I=ISTR,IEND
          UZ0H(I,J)=(UZ0(I+IHE(J),J)+UZ0(I+IHW(J),J)                    &
     &              +UZ0(I,J+1)+UZ0(I,J-1))*0.25
!!!  &              +UZ0(I,J+1)+UZ0(I,J-1))*HBM2(I,J)*0.25
          VZ0H(I,J)=(VZ0(I+IHE(J),J)+VZ0(I+IHW(J),J)                    &
     &              +VZ0(I,J+1)+VZ0(I,J-1))*0.25
!!!  &              +VZ0(I,J+1)+VZ0(I,J-1))*HBM2(I,J)*0.25
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  SET MAVAIL EQUAL TO 1. ONLY FOR NMM LSM
!-----------------------------------------------------------------------
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        IF(MODEL_CONFIG_REC%SF_SURFACE_PHYSICS(GRID%ID)==99)THEN
          ONE(I,J)=1.
        ELSE
!***  MAVAIL should not be equal to 1. for other LSMs
          ONE(I,J)=MAVAIL(I,J)
        ENDIF 
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST ARRAY (IJK) FOR THE PHYSICS (IKJ).
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          MOIST_TRANS(I,K,J,N)=MOIST(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!***  CALL SURFACE LAYER AND LAND SURFACE PHYSICS
!
!-----------------------------------------------------------------------
!
      CALL SET_TILES(GRID,IDS,IDE-1,JDS+1,JDE-1,ITS,ITE,JTS,JTE)
!
      CALL SURFACE_DRIVER(                                              &
     &           ACSNOM=ACSNOM,ACSNOW=ACSNOW,AKHS=AKHS,AKMS=AKMS        &
     &          ,ALBEDO=ALBEDO,BR=BR,CANWAT=CMC,CHKLOWQ=CHKLOWQ         &
     &          ,DT=DT,DX=DX,DZ8W=DZ,DZS=DZSOIL,GLW=RLW_DN_SFC          &
     &          ,GRDFLX=GRNFLX,GSW=RSW_NET_SFC,SWDOWN=RSW_DN_SFC        &
     &          ,GZ1OZ0=GZ1OZ0,HFX=TWBS                                 &
     &          ,HT=SFCZ,IFSNOW=IDUMMY,ISFFLX=ISFFLX,ISLTYP=ISLTYP      &
     &          ,ITIMESTEP=NTSD,IVGTYP=IVGTYP,LOWLYR=LOWLYR             &
     &          ,MAVAIL=ONE,RMOL=RMOL,NUM_SOIL_LAYERS=NSOIL,P8W=P8W &
     &          ,PBLH=PBLH,PI_PHY=PI_PHY,PSHLTR=PSHLTR,PSIH=PSIH        &
     &          ,PSIM=PSIM,P_PHY=P_PHY,Q10=Q10,Q2=Q2X,QFX=QWBS,QSFC=QS  &
     &          ,QSHLTR=QSHLTR,QZ0=QZ0,RAINCV=RAIN                      &
     &          ,RHO=RR,SFCEVP=SFCEVPX,SFCEXC=SFCEXC,SFCRUNOFF=SSROFF   &
     &          ,SMOIS=SMC,SMSTAV=SMSTAV,SMSTOT=SMSTOT,SNOALB=MXSNAL    &
     &          ,SNOW=SNOW,SNOWC=SNOWC,SNOWH=SNOWH,STEPBL=NPHS          &
     &          ,SST=SST,SST_UPDATE=SST_UPDATE                          &
     &          ,TH10=TH10,TH2=TH2X,T2=T2,THZ0=THZ0,TH_PHY=TH_PHY       &
     &          ,TMN=TG,TSHLTR=TSHLTR,TSK=TSFC,TSLB=STC,T_PHY=T_PHY     &
     &          ,U10=U10,UDRUNOFF=BGROFF,UST=USTAR,UZ0=UZ0H             &
     &          ,U_FRAME=U_FRAME,U_PHY=U_PHY,V10=V10,VEGFRA=VGFRCK      &
     &          ,VZ0=VZ0H,V_FRAME=V_FRAME,V_PHY=V_PHY                   &
     &          ,WARM_RAIN=WARM_RAIN,WSPD=WSPD,XICE=SICE                &
     &          ,XLAND=XLAND,Z=Z,ZNT=Z0,ZS=SLDPTH,CT=CT,TKE_MYJ=TKE     &
     &          ,ALBBCK=ALBASE,LH=ELFLX,SH2O=SH2O,SHDMAX=SHDMAX         &
     &          ,SHDMIN=SHDMIN,Z0=Z0BASE,FLQC=FLQC,FLHC=FLHC            &
     &          ,PSFC=PSFC_OUT,EMISS=EPSR                               &
     &          ,SF_SFCLAY_PHYSICS=CONFIG_FLAGS%SF_SFCLAY_PHYSICS       &
     &          ,SF_SURFACE_PHYSICS=CONFIG_FLAGS%SF_SURFACE_PHYSICS     &
     &          ,RA_LW_PHYSICS=CONFIG_FLAGS%RA_LW_PHYSICS               &
     &          ,UCMCALL=UCMCALL                                        &
     &          ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE        &
     &          ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME        &
     &          ,I_START=GRID%I_START,I_END=GRID%I_END                  &
     &          ,J_START=GRID%J_START,J_END=GRID%J_END                  &
     &          ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES               &
           ! Optional args
     &          ,QV_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV        &
     &          ,QC_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC        &
     &          ,QR_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR        &
     &          ,QI_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QI),F_QI=F_QI        &
     &          ,QS_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS        & 
     &          ,QG_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG        &
     &          ,RAINBL=RAINBL                                          &
! for RUCLSM
     &          ,QSG=QSG,QVG=QVG,QCG=QCG,SOILT1=SOILT1                  &
     &          ,TSNAV=TSNAV,SMFR3D=SMFR3D,KEEPFR3DFLAG=KEEPFR3DFLAG    &
     &          ,POTEVP=POTEVP,SNOPCX=SNOPCX,SOILTB=SOILTB,SR=SR)
!
!-----------------------------------------------------------------------
!
!***  CALL FREE ATMOSPHERE TURBULENCE
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        DUDT_PHY(I,K,J)=0.
        DVDT_PHY(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
!***  THE SURFACE EXCHANGE COEFFICIENTS AKHS AND AKMS ARE ACTUALLY
!***  MULTIPLIED BY HALF THE DEPTH OF THE LOWEST LAYER.  WE MUST RETAIN
!***  THOSE VALUES FOR THE NEXT TIMESTEP SO USE AUXILLIARY ARRAYS FOR
!***  THE OUTPUT.
!
!$omp parallel do                                                       &
!$omp& private(dzhalf,i,j)
      DO J=JTS,JTE
      DO I=ITS,ITE
        DZHALF=0.5*DZ(I,KTS,J)
        AKHS_OUT(I,J)=AKHS(I,J)*DZHALF
        AKMS_OUT(I,J)=AKMS(I,J)*DZHALF
      ENDDO
      ENDDO
!
      CALL PBL_DRIVER(                                                &
     &                ITIMESTEP=NTSD,DT=DT                            &
     &               ,U_FRAME=U_FRAME,V_FRAME=V_FRAME                 &
     &               ,RUBLTEN=DUDT_PHY,RVBLTEN=DVDT_PHY               &
     &               ,RTHBLTEN=RTHBLTEN                               &
     &               ,RQVBLTEN=RQVBLTEN,RQCBLTEN=RQCBLTEN             &
     &               ,RQIBLTEN=RQIBLTEN                               &
     &               ,TSK=TSFC,XLAND=XLAND,ZNT=Z0,HT=SFCZ             &
     &               ,UST=USTAR,PBLH=PBLH                             &
     &               ,HFX=TWBS,QFX=QWBS,GRDFLX=GRNFLX                 &
     &               ,U_PHY=U_PHY,V_PHY=V_PHY,TH_PHY=TH_PHY,RHO=RR    &
     &               ,P_PHY=P_PHY,PI_PHY=PI_PHY,P8W=P8W,T_PHY=T_PHY   &
     &               ,DZ8W=DZ,Z=Z,TKE_MYJ=TKE,EL_MYJ=EL_MYJ           &
     &               ,EXCH_H=EXCH_H,AKHS=AKHS,AKMS=AKMS               &
     &               ,THZ0=THZ0,QZ0=QZ0,UZ0=UZ0H,VZ0=VZ0H             &
     &               ,QSFC=QS,LOWLYR=LOWLYR                           &
     &               ,PSIM=PSIM,PSIH=PSIH,GZ1OZ0=GZ1OZ0               &
     &               ,WSPD=WSPD,BR=BR,CHKLOWQ=CHKLOWQ                 &
     &               ,DX=DX,STEPBL=NPHS,WARM_RAIN=WARM_RAIN           &
     &               ,KPBL=KPBL,CT=CT,LH=ELFLX,SNOW=SNOW,XICE=SICE    &
     &               ,BL_PBL_PHYSICS=config_flags%bl_pbl_physics      &
     &               ,RA_LW_PHYSICS=config_flags%ra_lw_physics        &
     &               ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &               ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &               ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &               ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &               ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
                ! Optional args
     &               ,QV_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV &
     &               ,QC_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC &
     &               ,QR_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR &
     &               ,QI_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QI),F_QI=F_QI &
     &               ,QS_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS &
     &               ,QG_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG)
!
!***  NOTE THAT THE EXCHANGE COEFFICIENTS FOR HEAT EXCH_H COMING OUT OF
!***  PBL_DRIVER ARE DEFINED AT THE TOPS OF THE LAYERS KTS TO KTE-1
!***  IF MODULE_BL_MYJPBL WAS INVOKED.
!
!-----------------------------------------------------------------------
! UNCOMPUTED LOCATIONS MUST BE FILLED IN FOR THE POST-PROCESSOR
!-----------------------------------------------------------------------
!
!***  EASTERN GLOBAL BOUNDARY
!
      IF(min(ide-(0),ite+(0))==IDE)THEN
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=JDS,JDE
        IF (J>=max(jds+(0),jts-(0)).AND.J<=min(jde-(0),jte+(0)))THEN
          TH10(min(ide-(0),ite+(0)),J)=TH10(min(ide-(0),ite+(0))-1,J)
          Q10(min(ide-(0),ite+(0)),J)=Q10(min(ide-(0),ite+(0))-1,J)
          U10(min(ide-(0),ite+(0)),J)=U10(min(ide-(0),ite+(0))-1,J)
          V10(min(ide-(0),ite+(0)),J)=V10(min(ide-(0),ite+(0))-1,J)
          TSHLTR(min(ide-(0),ite+(0)),J)=TSHLTR(min(ide-(0),ite+(0))-1,J)
          QSHLTR(min(ide-(0),ite+(0)),J)=QSHLTR(min(ide-(0),ite+(0))-1,J)
        ENDIF
        ENDDO
      ENDIF
!
!***  SOUTHERN GLOBAL BOUNDARY
!

      IF(max(jds+(0),jts-(0))==JDS)THEN
        DO J=JDS,JDS+1
        DO I=IDS,IDE
          IF (I>=max(ids+(0),its-(0)).AND.I<=min(ide-(0),ite+(0))) THEN
            TH10(I,J)=TH10(I,max(jds+(0),jts-(0))+2)
            Q10(I,J)=Q10(I,max(jds+(0),jts-(0))+2)
            U10(I,J)=U10(I,max(jds+(0),jts-(0))+2)
            V10(I,J)=V10(I,max(jds+(0),jts-(0))+2)
            TSHLTR(I,J)=TSHLTR(I,max(jds+(0),jts-(0))+2)
            QSHLTR(I,J)=QSHLTR(I,max(jds+(0),jts-(0))+2)
          ENDIF
        ENDDO
        ENDDO
      ENDIF
!
!***  NORTHERN GLOBAL BOUNDARY
!
      IF(min(jde-(0),jte+(0))==JDE)THEN
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=min(jde-(0),jte+(0))-1,min(jde-(0),jte+(0))
        DO I=IDS,IDE
          IF (I>=max(ids+(0),its-(0)).AND.I<=min(ide-(0),ite+(0))) THEN
            TH10(I,J)=TH10(I,min(jde-(0),jte+(0))-2)
            Q10(I,J)=Q10(I,min(jde-(0),jte+(0))-2)
            U10(I,J)=U10(I,min(jde-(0),jte+(0))-2)
            V10(I,J)=V10(I,min(jde-(0),jte+(0))-2)
            TSHLTR(I,J)=TSHLTR(I,min(jde-(0),jte+(0))-2)
            QSHLTR(I,J)=QSHLTR(I,min(jde-(0),jte+(0))-2)
          ENDIF
        ENDDO
        ENDDO
      ENDIF
!
      IF(CONFIG_FLAGS%SF_SFCLAY_PHYSICS==1)THEN ! non-NMM package
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=max(jds+(1),jts-(0)),min(jde-(1),jte+(0))
        DO I=max(ids+(0),its-(0)),min(ide-(1),ite+(0))
!         TSHLTR(I,J)=TSHLTR(I,J)*(1.E5/PSHLTR(I,J))**RCP
          IF(TSHLTR(I,J)<200..OR.TSHLTR(I,J)>350.)THEN
            WRITE(0,*)'Troublesome TSHLTR...I,J,TSHLTR,PSHLTR: '        &
                      ,I,J,TSHLTR(I,J),PSHLTR(I,J)
          ENDIF
	ENDDO
	ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!***  COMPUTE MODEL LAYER CONTAINING THE TOP OF THE BOUNDARY LAYER
!-----------------------------------------------------------------------
!
      IF(CONFIG_FLAGS%BL_PBL_PHYSICS/=MYJPBLSCHEME)THEN
        LENGTH_ROW=min(ide-(1),ite+(0))-max(ids+(1),its-(0))+1
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
          KPBL(I,J)=-1000
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(altitude,i,j,k,kount_all)
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          KOUNT_ALL=0
          find_kpbl : DO K=KTS,KTE
          DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
            ALTITUDE=Z(I,K+1,J)-SFCZ(I,J)
            IF(PBLH(I,J)<=ALTITUDE.AND.KPBL(I,J)<0)THEN
              KPBL(I,J)=K
              KOUNT_ALL=KOUNT_ALL+1
            ENDIF
            IF(KOUNT_ALL==LENGTH_ROW)EXIT find_kpbl
          ENDDO
          ENDDO find_kpbl
        ENDDO
      ENDIF
!
      IF(MODEL_CONFIG_REC%SF_SURFACE_PHYSICS(GRID%ID)==99)THEN
        SNO_FACTR=1.
      ELSE
        SNO_FACTR=1000.
      ENDIF
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
        SNO(I,J)=SNOW(I,J)
        SI(I,J)=SNOWH(I,J)*SNO_FACTR
        LPBL(I,J)=KTE-KPBL(I,J)+1
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  DIAGNOSTIC RADIATION ACCUMULATION
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,tsfc2)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
        ASWIN (I,J)=ASWIN (I,J)+RSWIN(I,J)*HBM2(I,J)*FACTRS(I,J)
        ASWOUT(I,J)=ASWOUT(I,J)-RSWOUT(I,J)*HBM2(I,J)*FACTRS(I,J)
        ASWTOA(I,J)=ASWTOA(I,J)+RSWTOA(I,J)*HBM2(I,J)*FACTRS(I,J)
        ALWIN (I,J)=ALWIN (I,J)+RLW_DN_SFC(I,J)
        ALWOUT(I,J)=ALWOUT(I,J)-RADOT (I,J)*HBM2(I,J)
        ALWTOA(I,J)=ALWTOA(I,J)+RLWTOA(I,J)*HBM2(I,J)
!
        TSFC2=TSFC(I,J)*TSFC(I,J)
        RADOT(I,J)=HBM2(I,J)*EPSR(I,J)*STBOLT*TSFC2*TSFC2
        THS(I,J)=TSFC(I,J)*EXNSFC(I,J)
        PREC(I,J)=0.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  TRANSFER THE WIND TENDENCIES.
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO J=JTS,JTE
      DO I=ITS,ITE
        DUDT(I,J,K)=DUDT_PHY(I,K,J)
        DVDT(I,J,K)=DVDT_PHY(I,K,J)
      ENDDO
      ENDDO
      ENDDO
!  
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST_TRANS ARRAY BACK TO THE PROGNOSTIC MOIST ARRAY.
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          MOIST(I,J,K,N)=MOIST_TRANS(I,K,J,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
      DEALLOCATE(MOIST_TRANS,STAT=ISTAT)
!
!-----------------------------------------------------------------------
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, CLOUD, AND TKE.
!-----------------------------------------------------------------------
!
      E_BDY=(ITE>=IDE)
!
!$omp parallel do                                                       &
!$omp& private(dqdt,dtdt,i,iend,j,k,qi,qold,qr,qw,ratiomx,i_m)
      DO K=KTS,KTE
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        IEND=min(ide-(1),ite+(0))
        IF(E_BDY.AND.MOD(J,2)==0)IEND=IEND-1
!
        DO I=max(ids+(1),its-(0)),IEND
          DTDT=RTHBLTEN(I,K,J)*PI_PHY(I,K,J)
          DQDT=RQVBLTEN(I,K,J)         !Mixing ratio tendency
          T(I,J,K)=T(I,J,K)+DTDT*DTPHS
          QOLD=Q(I,J,K)
          RATIOMX=QOLD/(1.-QOLD)+DQDT*DTPHS
          Q(I,J,K)=RATIOMX/(1.+RATIOMX)
!         Q(I,J,K)=MAX(Q(I,J,K),EPSQ)
          QW=MAX(0.,MOIST(I,J,K,P_QC)+RQCBLTEN(I,K,J)*DTPHS )
!
          IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
            QI=MAX(0.,MOIST(I,J,K,P_QS)+RQIBLTEN(I,K,J)*DTPHS )
          ELSE
            QI=MAX(0.,MOIST(I,J,K,P_QI)+RQIBLTEN(I,K,J)*DTPHS )
          ENDIF
!
          QR=MAX(0.,MOIST(I,J,K,P_QR) )
!         CWM(I,J,K)=QW+QI+QR
          CWM(I,J,K)=0. 
!
          DO I_M=1,N_MOIST
            IF(I_M/=P_QV)THEN
              CWM(I,J,K)=CWM(I,J,K)+MOIST(I,J,K,I_M)
            ENDIF
            IF(I_M==P_QV)THEN
              MOIST(I,J,K,P_QV)=MAX(EPSQ,(MOIST(I,J,K,P_QV)+RQVBLTEN(I,K,J)*DTPHS) )
            ELSEIF (I_M==P_QC)THEN
              CWM(I,J,K)=MAX(0.,(CWM(I,J,K)+RQCBLTEN(I,K,J)*DTPHS))
            ELSEIF(I_M==P_QI)THEN
              CWM(I,J,K)=MAX(0.,(CWM(I,J,K)+RQIBLTEN(I,K,J)*DTPHS))
            ENDIF
          ENDDO
!
          MOIST(I,J,K,P_QC)=QW
          MOIST(I,J,K,P_QR)=QR
!
          IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
            MOIST(I,J,K,P_QS)=QI
!
            IF(QI<=EPSQ)THEN  
              F_ICE(I,K,J)=0.
            ELSE
              F_ICE(I,K,J)=MAX(0.,MIN(1.,QI/CWM(I,J,K)))
            ENDIF
!
            IF(QR<=EPSQ)THEN
              F_RAIN(I,K,J)=0.
            ELSE
              F_RAIN(I,K,J)=QR/(QW+QR)
            ENDIF
!
          ELSE
            MOIST(I,J,K,P_QI)=QI
          ENDIF
!
          Q2(I,J,K)=2.*TKE(I,K,J)
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!***
!***  SAVE SURFACE-RELATED FIELDS.
!***
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,llij,xlvrw)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
        LLIJ=LOWLYR(I,J)
!
!-----------------------------------------------------------------------
!***  INSTANTANEOUS SENSIBLE AND LATENT HEAT FLUX
!-----------------------------------------------------------------------
!
        TWBS(I,J)=-TWBS(I,J)
        QWBS(I,J)=-QWBS(I,J)*XLV*CHKLOWQ(I,J)
!
!-----------------------------------------------------------------------
!***  ACCUMULATED QUANTITIES.
!***  IN OPNL LSM, SFCEVP APPEARS TO BE IN UNITS OF
!***  METERS OF LIQUID WATER.  IT IS COMING FROM
!***  WRF MODULE AS KG/M**2.
!-----------------------------------------------------------------------
!
        SFCSHX(I,J)=SFCSHX(I,J)+TWBS(I,J)
        SFCLHX(I,J)=SFCLHX(I,J)+QWBS(I,J)
        XLVRW=DTPHS/(XLV*RHOWATER)
        SFCEVP(I,J)=SFCEVP(I,J)-QWBS(I,J)*XLVRW
        POTEVP(I,J)=POTEVP(I,J)-QWBS(I,J)*SM(I,J)*XLVRW
        POTFLX(I,J)=POTEVP(I,J)*FACTOR
        SUBSHX(I,J)=SUBSHX(I,J)+GRNFLX(I,J)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  COUNTERS
!-----------------------------------------------------------------------
!
      APHTIM=APHTIM+1.
      ARDSW =ARDSW +1.
      ARDLW =ARDLW +1.
      ASRFC =ASRFC +1.
!-----------------------------------------------------------------------
!
      END SUBROUTINE TURBL
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE UV_H_TO_V(NTSD,DT,NPHS,UZ0H,VZ0H,UZ0,VZ0               &
     &                    ,DUDT,DVDT,U,V,HBM2,IVE,IVW                   & 
     &                    ,IDS,IDE,JDS,JDE,KDS,KDE                      &
     &                    ,IMS,IME,JMS,JME,KMS,KME                      &
     &                    ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    UV_H_TO_V   INTERPOLATE WINDS FROM H TO V POINTS
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 05-02-22       
!     
! ABSTRACT:
!     INTERPOLATE WINDS BACK TO V POINTS AFTER TURBULENCE
!     
! PROGRAM HISTORY LOG :
!   05-02-22  BLACK      - ORIGINATOR
!   05-12-12  BLACK      - CONVERTED FROM IKJ TO IJK
!     
! USAGE: CALL TURBL FROM SOLVE_NMM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,NPHS,NTSD
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IVE,IVW
!
      REAL,INTENT(IN) :: DT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HBM2,UZ0H,VZ0H
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: DUDT,DVDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: UZ0,VZ0
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: U,V
!
!-----------------------------------------------------------------------
!***
!***  LOCAL VARIABLES
!***
!-----------------------------------------------------------------------
!
      INTEGER :: I,IEND,J,K
!
      REAL :: DTPHS
!
      LOGICAL :: E_BDY
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      DTPHS=NPHS*DT
      E_BDY=(ITE>=IDE)
!
!-----------------------------------------------------------------------
!***  RECONSTRUCT UZ0 AND VZ0 ON VELOCITY POINTS.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
        UZ0(I,J)=(UZ0H(I+IVE(J),J)*HBM2(I+IVE(J),J)                     &
     &           +UZ0H(I+IVW(J),J)*HBM2(I+IVW(J),J)                     &
     &           +UZ0H(I,J+1)*HBM2(I,J+1)+UZ0H(I,J-1)*HBM2(I,J-1))*0.25
        VZ0(I,J)=(VZ0H(I+IVE(J),J)*HBM2(I+IVE(J),J)                     &
     &           +VZ0H(I+IVW(J),J)*HBM2(I+IVW(J),J)                     &
     &           +VZ0H(I,J+1)*HBM2(I,J+1)+VZ0H(I,J-1)*HBM2(I,J-1))*0.25
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  INTERPOLATE WIND TENDENCIES TO VELOCITY POINTS AND UPDATE WINDS.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iend,j,k)
      DO K=KTS,KTE
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          IEND=min(ide-(1),ite+(0))
          IF(E_BDY.AND.MOD(J,2)==1)IEND=IEND-1
!
          DO I=max(ids+(1),its-(0)),IEND
            U(I,J,K)=(DUDT(I+IVE(J),J,K)+DUDT(I+IVW(J),J,K)             &
     &               +DUDT(I,J+1,K)+DUDT(I,J-1,K))*0.25*DTPHS           &
     &               +U(I,J,K)
            V(I,J,K)=(DVDT(I+IVE(J),J,K)+DVDT(I+IVW(J),J,K)             &
     &               +DVDT(I,J+1,K)+DVDT(I,J-1,K))*0.25*DTPHS           &
     &               +V(I,J,K)
          ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      END SUBROUTINE UV_H_TO_V
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE CUCNVC(NTSD,DT,NCNVC,NRADS,NRADL                       &
     &                 ,GPS,RESTRT,HYDRO                                &
     &                 ,CLDEFI,N_MOIST,ENSDIM                           &
     &                 ,MOIST                                           &
     &                 ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2               &
     &                 ,F_ICE,F_RAIN                                    &
!***  Changes for other cu-schemes, most for gd scheme
     &                 ,APR_GR,APR_W,APR_MC,TTEN,QTEN                   &
     &                 ,APR_ST,APR_AS,APR_CAPMA                         &
     &                 ,APR_CAPME          ,APR_CAPMI                   &
     &                 ,MASS_FLUX         ,XF_ENS                       &
     &                 ,PR_ENS,GSW                                      &
!
     &                 ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TCUCN              &
     &                 ,OMGALF,U,V,W,Z,FIS,W0AVG                        &
     &                 ,PREC,ACPREC,CUPREC,CUPPT,CPRATE                 &
     &                 ,SM,HBM2,LPBL,CNVBOT,CNVTOP                      &
     &                 ,HTOP,HBOT,HTOPD,HBOTD,HTOPS,HBOTS               &
     &                 ,RTHBLTEN,RQVBLTEN,RTHRATEN                      & 
     &                 ,AVCNVC,ACUTIM,IHE,IHW                           &
     &                 ,GRID,CONFIG_FLAGS                               &
     &                 ,IDS,IDE,JDS,JDE,KDS,KDE                         &
     &                 ,IMS,IME,JMS,JME,KMS,KME                         &
     &                 ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CUCNVC      CONVECTIVE PRECIPITATION OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-03-21       
!     
! ABSTRACT:
!     CUCVNC DRIVES THE WRF CONVECTION SCHEMES
!     
! PROGRAM HISTORY LOG:
!   02-03-21  BLACK      - ORIGINATOR
!   04-11-18  BLACK      - THREADED
!   05-12-15  BLACK      - CONVERTED FROM IKJ TO IJK
!     
! USAGE: CALL CUCNVC FROM SOLVE_NMM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM 
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: ENSDIM                                      &
     &                     ,IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,NCNVC,NTSD,NRADS,NRADL
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LPBL
!
      REAL,INTENT(IN) :: DT,GPS,PDTOP,PT
!
      REAL,INTENT(INOUT) :: ACUTIM,AVCNVC
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
      REAL,DIMENSION(KMS:KME  ),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2,PD,RES,SM
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACPREC,CLDEFI    &
     &                                                ,CNVBOT,CNVTOP    &
     &                                                ,CUPPT,CUPREC     &
     &                                                ,HBOT,HTOP        &
     &                                                ,HBOTD,HTOPD      &
     &                                                ,HBOTS,HTOPS      &
     &                                                ,PREC,CPRATE      &
     &                 ,APR_GR,APR_W,APR_MC                             &
     &                 ,APR_ST,APR_AS,APR_CAPMA                         &
     &                 ,APR_CAPME,APR_CAPMI                             &
     &                 ,GSW,MASS_FLUX
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: F_ICE       &
     &                                                     ,F_RAIN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: QTEN     &
     &                                                        ,RQVBLTEN &
     &                                                        ,RTHBLTEN &
     &                                                        ,RTHRATEN &
     &                                                        ,TTEN
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM      &
     &                                                        ,OMGALF   &
     &                                                        ,Q,T      &
     &                                                        ,TCUCN    &
     &                                                        ,U,V      &
     &                                                        ,W,Z
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT  
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: W0AVG
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:ENSDIM),INTENT(INOUT) :: PR_ENS  &
     &                                                         ,XF_ENS
!    
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST)                   &
     &                                           ,INTENT(INOUT) :: MOIST
!
!
      LOGICAL,INTENT(IN) :: HYDRO,RESTRT
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,ICLDCK,IENDX,ISTAT,J,K,MNTO,N,N_TIMSTPS_OUTPUT       &
     &          ,NCUBOT,NCUTOP,NSTEP_CNV
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: KPBL,LBOT,LOWLYR,LTOP
!
      REAL :: CAPA,CF_HI,DPL,DQDT,DTCNVC,DTDT,FICE,FRAIN,G_INV          &
     &       ,PCPCOL,PLYR,QI,QL_K,QR,QW,RDTCNVC,TL_K,WC,WMID
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,NCA,PDSL           &
     &                                  ,RAINC,SFCZ,XLAND
      REAL,DIMENSION(IMS:IME,JMS:JME) :: RAINCV
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE) :: WMID_L
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RQCCUTEN,RQRCUTEN      &
     &                                          ,RQICUTEN,RQSCUTEN      &
     &                                          ,RQVCUTEN,RR,RTHCUTEN   &
     &                                          ,T_PHY,TH_PHY           &
     &                                          ,U_PHY,V_PHY,WINT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,ENSDIM) :: ZERO_GD
!
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: MOIST_TRANS
!
      LOGICAL :: RESTART,WARM_RAIN
      LOGICAL,DIMENSION(IMS:IME,JMS:JME) :: CU_ACT_FLAG
!
!-----------------------------------------------------------------------
!***  FOR TEMPERATURE CHANGE CHECK ONLY.
!-----------------------------------------------------------------------
      INTEGER :: DTEMP_CHECK=1.0
      REAL :: TCHANGE
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  RESET THE HBOT/HTOP CONVECTIVE CLOUD BOTTOM (BASE) AND TOP ARRAYS
!***  USED IN RADIATION.  THEY STORE THE MAXIMUM VERTICAL LIMITS OF 
!***  CONVECTIVE CLOUD BETWEEN RADIATION CALLS.  CUPPT IS THE ACCUMULATED
!***  CONVECTIVE PRECIPITATION BETWEEN RADIATION CALLS.
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,NRADS)==0.OR.MOD(NTSD,NRADL)==0)THEN
         DO J=JMS,JME
         DO I=IMS,IME
           HTOP(I,J)=0.
           HBOT(I,J)=REAL(KTE+1)
           CUPPT(I,J)=0.
         ENDDO
         ENDDO
      ENDIF
!-----------------------------------------------------------------------
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==BMJSCHEME)RETURN
      IF(MOD(NTSD,NCNVC)/=0.AND.                                      &
     &   CONFIG_FLAGS%CU_PHYSICS==SASSCHEME)RETURN
!-----------------------------------------------------------------------
      NSTEP_CNV=NCNVC
!
      RESTART=RESTRT
!-----------------------------------------------------------------------
      IF(CONFIG_FLAGS%CU_PHYSICS==KFETASCHEME)THEN
!
        IF(.NOT.RESTART.AND.NTSD==0)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO J=JTS,JTE
          DO K=KTS,KTE
          DO I=ITS,ITE
            W0AVG(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  GENERAL PREPARATION 
!-----------------------------------------------------------------------
!
      AVCNVC=AVCNVC+1.
      ACUTIM=ACUTIM+1.
!
      DTCNVC=NCNVC*DT
      RDTCNVC=1./DTCNVC
      CAPA=R_D/CP
      G_INV=1./G
!
!$omp parallel do                                                       &
!$omp& private(I,J)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
!
        PDSL(I,J)=PD(I,J)*RES(I,J)
        RAINCV(I,J)=0.
        RAINC(I,J)=0.
        P8W(I,KTS,J)=PD(I,J)+PDTOP+PT
        LOWLYR(I,J)=KTS        !<----  The lowest model layer counted from the bottom.
        XLAND(I,J)=SM(I,J)+1.
        NCA(I,J)=0.
        SFCZ(I,J)=FIS(I,J)*G_INV
!
        CUTOP(I,J)=HTOP(I,J)
        CUBOT(I,J)=HBOT(I,J)
!
!***  LPBL IS THE MODEL LAYER CONTAINING THE PBL TOP
!***  COUNTING DOWNWARD FROM THE TOP OF THE DOMAIN
!***  SO KPBL IS THE SAME LAYER COUNTING UPWARD FROM 
!***  THE GROUND.
!
        KPBL(I,J)=KTE-LPBL(I,J)+1
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(dpl,fice,frain,i,j,k,plyr,qi,ql,qr,qw,wc)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        DO K=KTS,KTE
        DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          QL(K)=MAX(Q(I,J,K),EPSQ)
          PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL(I,J)+PT
          TL(K)=T(I,J,K)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(P608*QL(K)+1.))
          T_PHY(I,K,J)=TL(K)

          TH_PHY(I,K,J)=TL(K)*(1.E5/PLYR)**CAPA
!!!       P8W(I,KFLIP,J)=PINT(I,J,K+1)
          P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL(I,J)+PT
          P_PHY(I,K,J)=PLYR
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
!
          RTHCUTEN(I,K,J)=0.
          RQVCUTEN(I,K,J)=0.
          RQCCUTEN(I,K,J)=0.
          RQRCUTEN(I,K,J)=0.
          RQICUTEN(I,K,J)=0.
          RQSCUTEN(I,K,J)=0.
        ENDDO
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!

      IF(.NOT.HYDRO)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KTS,KTE
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
          DZ(I,K,J)=Z(I,J,K+1)-Z(I,J,K)
        ENDDO
        ENDDO
        ENDDO
!
        IF(NTSD==0)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          DO K=KTS,KTE
          DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
            WINT(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO
        ENDIF
      ELSE
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
          WINT(I,KTS,J)=0.
          WINT(I,KTE+1,J)=0.
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,plyr)
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
            WMID_L(I,J)=-OMGALF(I,J,KTS)*CP/(G*DT)
            PDSL=PD(I,J)*RES(I,J)
            PLYR=AETA1(KTS)*PDTOP+AETA2(KTS)*PDSL(I,J)+PT
            DZ(I,KTS,J)=T(I,J,KTS)*(P608*Q(I,J,KTS)+1.)*R_D             &
     &                 *(P8W(I,KTS,J)-P8W(I,KTS+1,J))                   &
     &                 /(PLYR*G)
          ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,ql_k,tl_k,wmid)
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          DO K=KTS+1,KTE
          DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
            TL_K=T_PHY(I,K,J)
            QL_K=MAX(Q(I,J,K),EPSQ)
            WMID=-OMGALF(I,J,K)*CP/(G*DT)
            WINT(I,K,J)=0.5*(WMID_L(I,J)+WMID)
            WMID_L(I,J)=WMID
            DZ(I,K,J)=TL_K*(P608*QL_K+1.)*R_D                           &
     &               *(P8W(I,K,J)-P8W(I,K+1,J))                         &
     &               /(P_PHY(I,K,J)*G)
          ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  COMPUTE VELOCITY COMPONENTS AT MASS POINTS
!-----------------------------------------------------------------------
!
      IF(CONFIG_FLAGS%CU_PHYSICS/=BMJSCHEME)THEN
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KTS,KTE
!
          DO J=max(jds+(1),jts-(1)),min(jde-(1),jte+(1))
          DO I=max(ids+(0),its-(1)),min(ide-(0),ite+(1))
            U_PHY(I,K,J)=(U(I+IHE(J),J,K)+U(I+IHW(J),J,K)               &
     &                   +U(I,J+1,K)+U(I,K,J-1))                        &
     &                   *0.25
            V_PHY(I,K,J)=(V(I+IHE(J),J,K)+V(I+IHW(J),J,K)               &
     &                   +V(I,J+1,K)+V(I,K,J-1))                        &
     &                   *0.25
          ENDDO
          ENDDO
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST ARRAY (IJK) FOR THE PHYSICS (IKJ).
!-----------------------------------------------------------------------
!
      IF(.NOT.ALLOCATED(MOIST_TRANS))THEN
        ALLOCATE(MOIST_TRANS(IMS:IME,KMS:KME,JMS:JME,N_MOIST),STAT=ISTAT)
      ENDIF
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          MOIST_TRANS(I,K,J,N)=MOIST(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!***  SINGLE-COLUMN CONVECTION
!
!-----------------------------------------------------------------------
!
      CALL SET_TILES(GRID,IDS+1,IDE-1,JDS+2,JDE-2,ITS,ITE,JTS,JTE)
!
      CALL CUMULUS_DRIVER(                                              &
     &                  IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &                 ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &                 ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &                 ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &                 ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
                  ! Prognostic
     &                 ,U=U_PHY,V=V_PHY,TH=TH_PHY,T=T_PHY,W=WINT        &
     &                 ,P=P_PHY,PI=PI_PHY,RHO=RR,W0AVG=W0AVG            &
                  ! Others
     &                 ,ITIMESTEP=NTSD,DT=DT,DX=GPS                     &
     &                 ,RAINC=RAINC,RAINCV=RAINCV,NCA=NCA               &
     &                 ,DZ8W=DZ,P8W=P8W,FORCET=TTEN,FORCEQ=QTEN         &
     &                 ,CLDEFI=CLDEFI,LOWLYR=LOWLYR,XLAND=XLAND         &
     &                 ,CU_ACT_FLAG=CU_ACT_FLAG,WARM_RAIN=WARM_RAIN     &
     &                 ,STEPCU=NSTEP_CNV,GSW=GSW                        &
     &                 ,HTOP=CUTOP,HBOT=CUBOT,KPBL=KPBL,HT=SFCZ         &   
     &                 ,APR_GR=APR_GR,APR_W=APR_W,APR_MC=APR_MC         &
     &                 ,APR_ST=APR_ST,APR_AS=APR_AS,APR_CAPMA=APR_CAPMA &
     &                 ,APR_CAPME=APR_CAPME,APR_CAPMI=APR_CAPMI         &
     &                 ,MASS_FLUX=MASS_FLUX,XF_ENS=XF_ENS               &
     &                 ,PR_ENS=PR_ENS                                   &

     &                 ,ENSDIM=ENSDIM,MAXIENS=1,MAXENS=3                &
     &                 ,MAXENS2=3,MAXENS3=16                            &
     &                 ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
     &                 ,RQCCUTEN=RQCCUTEN,RQRCUTEN=RQRCUTEN             &
     &                 ,RQICUTEN=RQICUTEN,RQSCUTEN=RQSCUTEN             &
     &                 ,RTHBLTEN=RTHBLTEN,RQVBLTEN=RQVBLTEN             & 
     &                 ,RTHRATEN=RTHRATEN                               & 
                  ! Selection argument
     &                 ,CU_PHYSICS=CONFIG_FLAGS%CU_PHYSICS              &
                  ! Moisture tracer arguments
     &                 ,QV_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV &
     &                 ,QC_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC &
     &                 ,QR_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR &
     &                 ,QI_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QI),F_QI=F_QI &
     &                 ,QS_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS &
     &                 ,QG_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG)
!
!-----------------------------------------------------------------------
!
!***  CNVTOP/CNVBOT HOLD THE MAXIMUM VERTICAL LIMITS OF CONVECTIVE CLOUD 
!***  BETWEEN HISTORY OUTPUT TIMES.  HBOTS/HTOPS STORE SIMILIAR INFORMATION
!***  FOR SHALLOW (NONPRECIPITATING) CONVECTION, AND HBOTD/HTOPD ARE FOR
!***  DEEP (PRECIPITATING) CONVECTION.  
!
      CF_HI=CONFIG_FLAGS%HISTORY_INTERVAL
      N_TIMSTPS_OUTPUT=NINT(60.*CF_HI/DT)
      MNTO=MOD(NTSD,N_TIMSTPS_OUTPUT)
!
      IF(MNTO>0.AND.MNTO<=NCNVC)THEN
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        IENDX=min(ide-(1),ite+(0))
        IF(MOD(J,2)==0.AND.ITE==IDE-1)IENDX=IENDX-1
        DO I=max(ids+(1),its-(0)),IENDX
          CNVBOT(I,J)=REAL(KTE+1.)
          CNVTOP(I,J)=0.
          HBOTD(I,J)=REAL(KTE+1.)
          HTOPD(I,J)=0.
          HBOTS(I,J)=REAL(KTE+1.)
          HTOPS(I,J)=0.
        ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,ncubot,ncutop,pcpcol)
      pcp_cloud: DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        IENDX=min(ide-(1),ite+(0))
        IF(MOD(J,2)==0.AND.ITE==IDE-1)IENDX=IENDX-1
        DO I=max(ids+(1),its-(0)),IENDX
!
!***  UPDATE PRECIPITATION
!
          PCPCOL=RAINCV(I,J)*1.E-3*NSTEP_CNV
          PREC(I,J)=PREC(I,J)+PCPCOL
          ACPREC(I,J)=ACPREC(I,J)+PCPCOL
          CUPREC(I,J)=CUPREC(I,J)+PCPCOL
          CUPPT(I,J)=CUPPT(I,J)+PCPCOL
          CPRATE(I,J)=PCPCOL
!
!***  SAVE CLOUD TOP AND BOTTOM FOR RADIATION (HTOP/HBOT) AND
!***  FOR OUTPUT (CNVTOP/CNVBOT, HTOPS/HBOTS, HTOPD/HBOTD) ARRAYS.
!***  THEY MUST BE TREATED SEPARATELY FROM EACH OTHER.
!
          NCUTOP=NINT(CUTOP(I,J))
          NCUBOT=NINT(CUBOT(I,J))
!
          IF(NCUTOP>1.AND.NCUTOP<KDE)THEN
            HTOP(I,J)=MAX(CUTOP(I,J),HTOP(I,J))
            CNVTOP(I,J)=MAX(CUTOP(I,J),CNVTOP(I,J))
            IF(PCPCOL>0.)THEN
              HTOPD(I,J)=MAX(CUTOP(I,J),HTOPD(I,J))
            ELSE
              HTOPS(I,J)=MAX(CUTOP(I,J),HTOPS(I,J))
            ENDIF
          ENDIF
!
          IF(NCUBOT>0.AND.NCUBOT<KDE)THEN
            HBOT(I,J)=MIN(CUBOT(I,J),HBOT(I,J))
            CNVBOT(I,J)=MIN(CUBOT(I,J),CNVBOT(I,J))
            IF(PCPCOL>0.)THEN
              HBOTD(I,J)=MIN(CUBOT(I,J),HBOTD(I,J))
            ELSE
              HBOTS(I,J)=MIN(CUBOT(I,J),HBOTS(I,J))
            ENDIF
          ENDIF
!
        ENDDO
      ENDDO pcp_cloud
!
!-----------------------------------------------------------------------
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, AND HEATING.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dqdt,dtdt,i,iendx,j,k,tchange)
      DO K=KTS,KTE
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        IENDX=min(ide-(1),ite+(0))
        IF(MOD(J,2)==0.AND.ITE==IDE-1)IENDX=IENDX-1
        DO I=max(ids+(1),its-(0)),IENDX
!
!***  RQVCUTEN IN BMJDRV IS THE MIXING RATIO TENDENCY,
!***  SO RETRIEVE DQDT BY CONVERTING TO SPECIFIC HUMIDITY.
!
          DQDT=RQVCUTEN(I,K,J)/(1.+MOIST_TRANS(I,K,J,P_QV))**2
!
!***  RTHCUTEN IN BMJDRV IS DTDT OVER PI.
!
          DTDT=RTHCUTEN(I,K,J)*PI_PHY(I,K,J)
          T(I,J,K)=T(I,J,K)+DTDT*DTCNVC
          Q(I,J,K)=Q(I,J,K)+DQDT*DTCNVC
          TCUCN(I,J,K)=TCUCN(I,J,K)+DTDT
          MOIST_TRANS(I,K,J,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))       !Convert to mixing ratio
!
          cps_select: SELECT CASE(config_flags%cu_physics)
!
          CASE (KFSCHEME,KFETASCHEME,GDSCHEME,SASSCHEME)
            IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
              MOIST_TRANS(I,K,J,P_QS)=MAX(0.,MOIST_TRANS(I,K,J,P_QS)+RQICUTEN(I,K,J)*DTCNVC+RQSCUTEN(I,K,J)*DTCNVC)
            ELSE
              MOIST_TRANS(I,K,J,P_QI)=MAX(0.,MOIST_TRANS(I,K,J,P_QI)+RQICUTEN(I,K,J)*DTCNVC)
              MOIST_TRANS(I,K,J,P_QS)=MAX(0.,MOIST_TRANS(I,K,J,P_QS)+RQSCUTEN(I,K,J)*DTCNVC)
            ENDIF
            MOIST_TRANS(I,K,J,P_QR)=MAX(0.,MOIST_TRANS(I,K,J,P_QR)+RQRCUTEN(I,K,J)*DTCNVC)
            MOIST_TRANS(I,K,J,P_QC)=MAX(0.,MOIST_TRANS(I,K,J,P_QC)+RQCCUTEN(I,K,J)*DTCNVC)
          END SELECT cps_select
!
          TCHANGE=DTDT*DTCNVC
	  IF(ABS(TCHANGE)>DTEMP_CHECK)THEN
            WRITE(0,*)'BIG T CHANGE BY CONVECTION=',TCHANGE             &
                     ,' AT (',I,',',J,',',K,') FOR NTSD=',NTSD
	  ENDIF
!
        ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!***  REFILL THE MOIST ARRAY.
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          MOIST(I,J,K,N)=MOIST_TRANS(I,K,J,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(MOIST_TRANS,STAT=ISTAT)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CUCNVC
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE GSMDRIVE(NTSD,DT,NPHS,N_MOIST                          &
     &                   ,DX,DY,SM,HBM2,FIS                             &
     &                   ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2             &
     &                   ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN            &
     &                   ,MOIST,SCALAR,N_SCALAR                         &
     &                   ,F_ICE,F_RAIN,F_RIMEF,SR                       &
     &                   ,PREC,ACPREC,AVRAIN                            &
     &                   ,MP_RESTART_STATE                              &
     &                   ,TBPVS_STATE                                   &
     &                   ,TBPVS0_STATE                                  &
     &                   ,GRID,CONFIG_FLAGS                             &
     &                   ,IDS,IDE,JDS,JDE,KDS,KDE                       &
     &                   ,IMS,IME,JMS,JME,KMS,KME                       &
     &                   ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    GSMDRIVE    MICROPHYSICS OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-03-26       
!     
! ABSTRACT:
!     GSMDRIVE DRIVES THE MICROPHYSICS SCHEMES
!     
! PROGRAM HISTORY LOG:
!   02-03-26  BLACK      - ORIGINATOR
!   04-11-18  BLACK      - THREADED
!   05-12-19  BLACK      - CONVERTED FROM IKJ TO IJK
!     
! USAGE: CALL GSMDRIVE FROM SOLVE_NMM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM
!$$$  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,N_SCALAR,NPHS,NTSD
!
      REAL,INTENT(IN) :: DT,DX,DY,PDTOP,PT
!
      REAL,INTENT(INOUT) :: AVRAIN
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2,PD,RES,SM
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACPREC,PREC
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM,Q    &
     &                                                        ,T,TRAIN
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: F_ICE    &   !<--- Used only with physics (IKJ)
     &                                                        ,F_RAIN   &
     &                                                        ,F_RIMEF

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST)                   &
     &                                           ,INTENT(INOUT) :: MOIST
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_SCALAR)                  &
     &                                          ,INTENT(INOUT) :: SCALAR
!
!***  State var for etampnew microphysics (JM, 2005 05 02)
!
      REAL,DIMENSION(:),INTENT(INOUT) :: MP_RESTART_STATE               &
     &                                  ,TBPVS_STATE,TBPVS0_STATE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: SR
!
      TYPE(DOMAIN),TARGET :: GRID
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,IENDX,IJ,ISTAT,J,K,N
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: LOWLYR
!
      REAL :: CAPA,DPL,DTPHS,PCPCOL,PDSL,PLYR,RDTPHS,RG,TNEW
!
      REAL,DIMENSION(KMS:KME-1) :: QL,TL
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CUBOT,CUTOP,PDSL               &
     &                                  ,RAINNC,RAINNCV,XLAND
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: CWM_PHY,DZ             &
     &                                          ,P8W,P_PHY,PI_PHY       &
     &                                          ,RR,T_PHY,TH_PHY
!
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: MOIST_TRANS
      REAL,DIMENSION(:,:,:,:),ALLOCATABLE :: SCALAR_TRANS
!
      LOGICAL :: E_BDY,F_QT,QT_PRESENT,WARM_RAIN
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ALLOCATE(MOIST_TRANS(IMS:IME,KMS:KME,JMS:JME,N_MOIST),STAT=ISTAT)
      ALLOCATE(SCALAR_TRANS(IMS:IME,KMS:KME,JMS:JME,N_SCALAR),STAT=ISTAT)
!
!-----------------------------------------------------------------------
!***  TRANSPOSE THE MOIST ARRAY (IJK) FOR THE PHYSICS (IKJ).
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          MOIST_TRANS(I,K,J,N)=MOIST(I,J,K,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
        DO N=1,N_SCALAR
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO K=KMS,KME
          DO J=JMS,JME
          DO I=IMS,IME
            SCALAR_TRANS(I,K,J,N)=SCALAR(I,J,K,N)
          ENDDO
          ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
        QT_PRESENT=.TRUE.
      ELSE
        QT_PRESENT=.FALSE.
      ENDIF
!
      DTPHS=NPHS*DT
      RDTPHS=1./DTPHS
      CAPA=R_D/CP
      RG=1./G
      AVRAIN=AVRAIN+1.
!
!-----------------------------------------------------------------------
!
!***  PREPARE NEEDED ARRAYS
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
      DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
!
        PDSL(I,J)=PD(I,J)*RES(I,J)
        P8W(I,KTE+1,J)=PT
        LOWLYR(I,J)=KTS        !<----  The lowest model layer counted from the bottom.
        XLAND(I,J)=SM(I,J)+1.
!-----------------------------------------------------------------------
!***  FILL RAINNC WITH ZERO (NORMALLY CONTAINS THE NONCONVECTIVE
!***  ACCUMULATED RAIN BUT NOT YET USED BY NMM).
!***  CAN BE OBTAINED FROM ACPREC AND CUPREC (ACPREC-CUPREC).
!-----------------------------------------------------------------------
        RAINNC(I,J)=0.
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  FILL THE SINGLE-COLUMN INPUT
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dpl,i,j,k,plyr,ql,tl)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        DO K=KTS,KTE
        DO I=max(ids+(1),its-(0)),min(ide-(1),ite+(0))
          DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          QL(K)=MAX(Q(I,J,K),EPSQ)
!!!       PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL(I,J)+PT
          PLYR=(PINT(I,J,K)+PINT(I,J,K+1))*0.5
          TL(K)=T(I,J,K)
!
          RR(I,K,J)=PLYR/(R_D*TL(K)*(P608*QL(K)+1.))
          T_PHY(I,K,J)=TL(K)
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          TH_PHY(I,K,J)=TL(K)/PI_PHY(I,K,J)
!!!       P8W(I,KFLIP,J)=PINT(I,J,K+1)
          P8W(I,K,J)=ETA1(K)*PDTOP+ETA2(K)*PDSL(I,J)+PT
          P_PHY(I,K,J)=PLYR
          DZ(I,K,J)=DPL*RG/RR(I,K,J)
          CWM_PHY(I,K,J)=CWM(I,J,K)
        ENDDO
!
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  CALL MICROPHYSICS
!
!-----------------------------------------------------------------------
!
      CALL SET_TILES(GRID,IDS+1,IDE-1,JDS+2,JDE-2,ITS,ITE,JTS,JTE)
!
      CALL MICROPHYSICS_DRIVER(                                         &
     &                  TH=TH_PHY,RHO=RR,PI_PHY=PI_PHY,P=P_PHY          &
     &                 ,RAINNC=RAINNC,RAINNCV=RAINNCV                   &
     &                 ,DZ8W=DZ,P8W=P8W,DT=DTPHS,DX=DX,DY=DY            &
     &                 ,MP_PHYSICS=CONFIG_FLAGS%MP_PHYSICS              &
     &                 ,SPECIFIED=CONFIG_FLAGS%SPECIFIED                &
     &                        .OR.CONFIG_FLAGS%NESTED                   &
     &                 ,SPEC_ZONE=0,WARM_RAIN=WARM_RAIN                 &
     &                 ,XLAND=XLAND,ITIMESTEP=NTSD-1                    &
     &                 ,F_ICE_PHY=F_ICE,F_RAIN_PHY=F_RAIN               &
     &                 ,F_RIMEF_PHY=F_RIMEF                             &
     &                 ,LOWLYR=LOWLYR,SR=SR                             &
     &                 ,QV_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QV),F_QV=F_QV &
     &                 ,QC_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QC),F_QC=F_QC &
     &                 ,QR_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QR),F_QR=F_QR &
     &                 ,QI_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QI),F_QI=F_QI &
     &                 ,QS_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QS),F_QS=F_QS &
     &                 ,QG_CURR=MOIST_TRANS(IMS,KMS,JMS,P_QG),F_QG=F_QG &
     &                 ,QNI_CURR=SCALAR_TRANS(IMS,KMS,JMS,P_QNI),F_QNI=F_QNI  &
     &                 ,QT_CURR=CWM_PHY,F_QT=QT_PRESENT                 &
     &                 ,MP_RESTART_STATE=MP_RESTART_STATE               &
     &                 ,TBPVS_STATE=TBPVS_STATE                         &
     &                 ,TBPVS0_STATE=TBPVS0_STATE                       &
     &                 ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE &
     &                 ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME &
     &                 ,I_START=GRID%I_START,I_END=GRID%I_END           &
     &                 ,J_START=GRID%J_START,J_END=GRID%J_END           &
     &                 ,KTS=KTS,KTE=KTE,NUM_TILES=GRID%NUM_TILES        &
                                                                        )

!$omp parallel do                                                       &
!$omp& private(ij)
      DO IJ=1,GRID%NUM_TILES
        CALL MICROPHYSICS_ZERO_OUT(                                     &
                     MOIST_TRANS,N_MOIST,CONFIG_FLAGS                   &
                    ,IDS,IDE,JDS,JDE,KDS,KDE                            &
                    ,IMS,IME,JMS,JME,KMS,KME                            &
                    ,GRID%I_START(IJ),GRID%I_END(IJ)                    &
                    ,GRID%J_START(IJ),GRID%J_END(IJ)                    &
                    ,KTS,KTE                                       )
      ENDDO



!
!-----------------------------------------------------------------------
!
      E_BDY=(ITE>=IDE)
!
!-----------------------------------------------------------------------
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, CLOUD WATER, AND HEATING.
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,k,tnew)
      DO K=KTS,KTE
        DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
          IENDX=min(ide-(1),ite+(0))
          IF(E_BDY.AND.MOD(J,2)==0)IENDX=IENDX-1
          DO I=max(ids+(1),its-(0)),IENDX
            TNEW=TH_PHY(I,K,J)*PI_PHY(I,K,J)
            TRAIN(I,J,K)=TRAIN(I,J,K)+(TNEW-T(I,J,K))*RDTPHS
            T(I,J,K)=TNEW
            Q(I,J,K)=MOIST_TRANS(I,K,J,P_QV)/(1.+MOIST_TRANS(I,K,J,P_QV))
            CWM(I,J,K)=CWM_PHY(I,K,J)
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  UPDATE PRECIPITATION.
!***  NOTE: RAINNC IS ACCUMULATED INSIDE MICROPHYSICS BUT NMM ZEROES IT
!***  OUT ABOVE SINCE IT IS ONLY A LOCAL ARRAY FOR NOW.
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,iendx,j,pcpcol)
      DO J=max(jds+(2),jts-(0)),min(jde-(2),jte+(0))
        IENDX=min(ide-(1),ite+(0))
        IF(E_BDY.AND.MOD(J,2)==0)IENDX=IENDX-1
        DO I=max(ids+(1),its-(0)),IENDX
          PCPCOL=RAINNCV(I,J)*1.E-3
          PREC(I,J)=PREC(I,J)+PCPCOL
          ACPREC(I,J)=ACPREC(I,J)+PCPCOL
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  REFILL THE MOIST ARRAY.
!-----------------------------------------------------------------------
!
      DO N=1,N_MOIST
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          MOIST(I,J,K,N)=MOIST_TRANS(I,K,J,N)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
        DO N=1,N_SCALAR
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO J=JMS,JME
          DO K=KMS,KME
          DO I=IMS,IME
            SCALAR(I,J,K,N)=SCALAR_TRANS(I,K,J,N)
          ENDDO
          ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(MOIST_TRANS,STAT=ISTAT)
      DEALLOCATE(SCALAR_TRANS,STAT=ISTAT)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE GSMDRIVE
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST          &
     &                       ,IDS,IDE,JDS,JDE,KDS,KDE                   &
     &                       ,IMS,IME,JMS,JME,KMS,KME                   &
     &                       ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: CWM,Q
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: F_ICE       &   !<--- Used only with physics (IKJ)
     &                                                     ,F_RAIN
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,N_MOIST),INTENT(OUT) :: MOIST
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,K
!
      REAL :: FICE,FRAIN,QI,QR,QW,WC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO J=max(jds+(0),jts-(0)),min(jde-(0),jte+(0))
      DO I=max(ids+(0),its-(0)),min(ide-(0),ite+(0))
        MOIST(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))
        WC=CWM(I,J,K)
        QI=0.
        QR=0.
        QW=0.
        FICE=F_ICE(I,K,J)
        FRAIN=F_RAIN(I,K,J)
!
        IF(FICE>=1.)THEN
          QI=WC
        ELSEIF(FICE<=0.)THEN
          QW=WC
        ELSE
          QI=FICE*WC
          QW=WC-QI
        ENDIF
!
        IF(QW>0..AND.FRAIN>0.)THEN
          IF(FRAIN>=1.)THEN
            QR=QW
            QW=0.
          ELSE
            QR=FRAIN*QW
            QW=QW-QR
          ENDIF
        ENDIF
!
        MOIST(I,J,K,P_QC)=QW
        MOIST(I,J,K,P_QR)=QR
        MOIST(I,J,K,P_QI)=0.
        MOIST(I,J,K,P_QS)=QI
        MOIST(I,J,K,P_QG)=0.
      ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE UPDATE_MOIST
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      END MODULE MODULE_PHYSICS_CALLS
!
!-------------------------------------------------------------------
