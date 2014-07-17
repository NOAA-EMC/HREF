MODULE module_sf_noahmpdrv


  USE module_sf_noahmplsm
  USE module_sf_urban
  USE module_sf_noahdrv, ONLY : SOIL_VEG_GEN_PARM
  USE module_sf_noah_seaice
  USE module_sf_noahmp_glacier
  USE MODULE_RA_GFDLETA, ONLY: CAL_MON_DAY






CONTAINS

  SUBROUTINE noahmplsm(ITIMESTEP, YR, JULIAN, COSZIN, XLATIN, &
                  DZ8W, DT, DZS, NSOIL, DX, &
         IVGTYP, ISLTYP, VEGFRA, VEGMAX, TMN, &
   XLAND, XICE,XICE_THRES, ISICE, ISURBAN, &
                 IDVEG, IOPT_CRS, IOPT_BTR, IOPT_RUN, IOPT_SFC, IOPT_FRZ, &
              IOPT_INF, IOPT_RAD, IOPT_ALB, IOPT_SNF,IOPT_TBOT, IOPT_STC, &
               IZ0TLND, &
                   T3D, QV3D, U_PHY, V_PHY, SWDOWN, GLW, &
                 P8W3D, RAINBL, &
                   TSK, HFX, QFX, LH, GRDFLX, SMSTAV, &
                SMSTOT,SFCRUNOFF, UDRUNOFF, ALBEDO, SNOWC, SMOIS, &
    SH2O, TSLB, SNOW, SNOWH, CANWAT, ACSNOM, &
  ACSNOW, EMISS, QSFC, &
               ISNOWXY, TVXY, TGXY, CANICEXY, CANLIQXY, EAHXY, &
          TAHXY, CMXY, CHXY, FWETXY, SNEQVOXY, ALBOLDXY, &
               QSNOWXY, WSLAKEXY, ZWTXY, WAXY, WTXY, TSNOXY, &
        ZSNSOXY, SNICEXY, SNLIQXY, LFMASSXY, RTMASSXY, STMASSXY, &
         WOODXY, STBLCPXY, FASTCPXY, XLAIXY, XSAIXY, TAUSSXY, &
         T2MVXY, T2MBXY, Q2MVXY, Q2MBXY, &
         TRADXY, NEEXY, GPPXY, NPPXY, FVEGXY, RUNSFXY, &
        RUNSBXY, ECANXY, EDIRXY, ETRANXY, FSAXY, FIRAXY, &
         APARXY, PSNXY, SAVXY, SAGXY, RSSUNXY, RSSHAXY, &
  BGAPXY, WGAPXY, TGVXY, TGBXY, CHVXY, CHBXY, &
   SHGXY, SHCXY, SHBXY, EVGXY, EVBXY, GHVXY, &
   GHBXY, IRGXY, IRCXY, IRBXY, TRXY, EVCXY, &
              CHLEAFXY, CHUCXY, CHV2XY, CHB2XY, &
               ids,ide, jds,jde, kds,kde, &
               ims,ime, jms,jme, kms,kme, &
               its,ite, jts,jte, kts,kte )

    IMPLICIT NONE




    INTEGER, INTENT(IN ) :: ITIMESTEP
    INTEGER, INTENT(IN ) :: YR
    REAL, INTENT(IN ) :: JULIAN
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: COSZIN
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: XLATIN
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN ) :: DZ8W
    REAL, INTENT(IN ) :: DT
    REAL, DIMENSION(1:nsoil), INTENT(IN ) :: DZS
    INTEGER, INTENT(IN ) :: NSOIL
    REAL, INTENT(IN ) :: DX
    INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: IVGTYP
    INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: ISLTYP
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: VEGFRA
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN ) :: VEGMAX
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: TMN
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: XLAND
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: XICE
    REAL, INTENT(IN ) :: XICE_THRES
    INTEGER, INTENT(IN ) :: ISICE
    INTEGER, INTENT(IN ) :: ISURBAN
    INTEGER, INTENT(IN ) :: IDVEG
    INTEGER, INTENT(IN ) :: IOPT_CRS
    INTEGER, INTENT(IN ) :: IOPT_BTR
    INTEGER, INTENT(IN ) :: IOPT_RUN
    INTEGER, INTENT(IN ) :: IOPT_SFC
    INTEGER, INTENT(IN ) :: IOPT_FRZ
    INTEGER, INTENT(IN ) :: IOPT_INF
    INTEGER, INTENT(IN ) :: IOPT_RAD
    INTEGER, INTENT(IN ) :: IOPT_ALB
    INTEGER, INTENT(IN ) :: IOPT_SNF
    INTEGER, INTENT(IN ) :: IOPT_TBOT
    INTEGER, INTENT(IN ) :: IOPT_STC
    INTEGER, INTENT(IN ) :: IZ0TLND
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN ) :: T3D
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN ) :: QV3D
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN ) :: U_PHY
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN ) :: V_PHY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: SWDOWN
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: GLW
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN ) :: P8W3D
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: RAINBL



    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TSK
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: HFX
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QFX
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: GRDFLX
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SMSTAV
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SMSTOT
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SFCRUNOFF
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UDRUNOFF
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ALBEDO
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SNOWC
    REAL, DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) :: SMOIS
    REAL, DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) :: SH2O
    REAL, DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) :: TSLB
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SNOW
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SNOWH
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CANWAT
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ACSNOM
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ACSNOW
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: EMISS
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QSFC



    INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ISNOWXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TVXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TGXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CANICEXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CANLIQXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: EAHXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TAHXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FWETXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SNEQVOXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ALBOLDXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QSNOWXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: WSLAKEXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ZWTXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: WAXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: WTXY
    REAL, DIMENSION( ims:ime,-2:0, jms:jme ), INTENT(INOUT) :: TSNOXY
    REAL, DIMENSION( ims:ime,-2:NSOIL, jms:jme ), INTENT(INOUT) :: ZSNSOXY
    REAL, DIMENSION( ims:ime,-2:0, jms:jme ), INTENT(INOUT) :: SNICEXY
    REAL, DIMENSION( ims:ime,-2:0, jms:jme ), INTENT(INOUT) :: SNLIQXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LFMASSXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RTMASSXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: STMASSXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: WOODXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: STBLCPXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FASTCPXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XLAIXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XSAIXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TAUSSXY



    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: T2MVXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: T2MBXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: Q2MVXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: Q2MBXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: TRADXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: NEEXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: GPPXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: NPPXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: FVEGXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: RUNSFXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: RUNSBXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: ECANXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: EDIRXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: ETRANXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: FSAXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: FIRAXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: APARXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: PSNXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: SAVXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: SAGXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: RSSUNXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: RSSHAXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: BGAPXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: WGAPXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: TGVXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: TGBXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: CHVXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: CHBXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: SHGXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: SHCXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: SHBXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: EVGXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: EVBXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: GHVXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: GHBXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: IRGXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: IRCXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: IRBXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: TRXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: EVCXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: CHLEAFXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: CHUCXY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: CHV2XY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT ) :: CHB2XY
    INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde, &
         & ims,ime, jms,jme, kms,kme, &
         & its,ite, jts,jte, kts,kte






    REAL :: COSZ
    REAL :: LAT
    REAL :: Z_ML
    INTEGER :: VEGTYP
    INTEGER :: SOILTYP
    REAL :: FVEG
    REAL :: FVGMAX
    REAL :: TBOT
    REAL :: T_ML
    REAL :: Q_ML
    REAL :: U_ML
    REAL :: V_ML
    REAL :: SWDN
    REAL :: LWDN
    REAL :: P_ML
    REAL :: PSFC
    REAL :: PRCP



    REAL :: FSH
    REAL :: SSOIL
    REAL :: SALB
    REAL :: FSNO
    REAL, DIMENSION( 1:NSOIL) :: SMC
    REAL, DIMENSION( 1:NSOIL) :: SMH2O
    REAL, DIMENSION(-2:NSOIL) :: STC
    REAL :: SWE
    REAL :: SNDPTH
    REAL :: EMISSI
    REAL :: QSFC1D



    INTEGER :: ISNOW
    REAL :: TV
    REAL :: TG
    REAL :: CANICE
    REAL :: CANLIQ
    REAL :: EAH
    REAL :: TAH
    REAL :: CM
    REAL :: CH
    REAL :: FWET
    REAL :: SNEQVO
    REAL :: ALBOLD
    REAL :: QSNOW
    REAL :: WSLAKE
    REAL :: ZWT
    REAL :: WA
    REAL :: WT
    REAL, DIMENSION(-2:NSOIL) :: ZSNSO
    REAL, DIMENSION(-2: 0) :: SNICE
    REAL, DIMENSION(-2: 0) :: SNLIQ
    REAL :: LFMASS
    REAL :: RTMASS
    REAL :: STMASS
    REAL :: WOOD
    REAL :: STBLCP
    REAL :: FASTCP
    REAL :: PLAI
    REAL :: PSAI
    REAL :: TAUSS



    REAL :: T2MV
    REAL :: T2MB
    REAL :: Q2MV
    REAL :: Q2MB
    REAL :: TRAD
    REAL :: NEE
    REAL :: GPP
    REAL :: NPP
    REAL :: FVEGMP
    REAL :: RUNSF
    REAL :: RUNSB
    REAL :: ECAN
    REAL :: ETRAN
    REAL :: ESOIL
    REAL :: FSA
    REAL :: FIRA
    REAL :: APAR
    REAL :: PSN
    REAL :: SAV
    REAL :: SAG
    REAL :: RSSUN
    REAL :: RSSHA
    REAL :: BGAP
    REAL :: WGAP
    REAL :: TGV
    REAL :: TGB
    REAL :: CHV
    REAL :: CHB
    REAL :: IRC
    REAL :: IRG
    REAL :: SHC
    REAL :: SHG
    REAL :: EVG
    REAL :: GHV
    REAL :: IRB
    REAL :: SHB
    REAL :: EVB
    REAL :: GHB
    REAL :: TR
    REAL :: EVC
    REAL :: CHLEAF
    REAL :: CHUC
    REAL :: CHV2
    REAL :: CHB2



    REAL :: FPICE
    REAL :: FCEV
    REAL :: FGEV
    REAL :: FCTR
    REAL :: QSNBOT
    REAL :: PONDING
    REAL :: PONDING1
    REAL :: PONDING2



    REAL :: FSR
    REAL, DIMENSION(-2:0) :: FICEOLD
    REAL :: CO2PP
    REAL :: O2PP
    REAL, DIMENSION(1:NSOIL) :: ZSOIL
    REAL :: FOLN

    REAL :: QC
    REAL :: PBLH
    REAL :: DZ8W1D

    INTEGER :: I
    INTEGER :: J
    INTEGER :: K
    INTEGER :: ICE
    INTEGER :: SLOPETYP
    LOGICAL :: IPRINT

    INTEGER :: ISC
    INTEGER :: IST
    INTEGER :: YEARLEN

    INTEGER, PARAMETER :: NSNOW = 3
    REAL, PARAMETER :: CO2 = 395.e-06
    REAL, PARAMETER :: O2 = 0.209
    REAL, PARAMETER :: undefined_value = -1.E36





    CALL NOAHMP_OPTIONS(IDVEG ,IOPT_CRS ,IOPT_BTR ,IOPT_RUN ,IOPT_SFC ,IOPT_FRZ , &
                     IOPT_INF ,IOPT_RAD ,IOPT_ALB ,IOPT_SNF ,IOPT_TBOT, IOPT_STC )

    IPRINT = .false.

    YEARLEN = 365
    if (mod(YR,4) == 0) then
       YEARLEN = 366
       if (mod(YR,100) == 0) then
          YEARLEN = 365
          if (mod(YR,400) == 0) then
             YEARLEN = 366
          endif
       endif
    endif

    ZSOIL(1) = -DZS(1)
    DO K = 2, NSOIL
       ZSOIL(K) = -DZS(K) + ZSOIL(K-1)
    END DO

    JLOOP : DO J=jts,jte

       IF(ITIMESTEP == 1)THEN
          DO I=its,ite
             IF((XLAND(I,J)-1.5) >= 0.) THEN
                IF(XICE(I,J) == 1. .AND. IPRINT) PRINT *,' sea-ice at water point, I=',I,'J=',J
                SMSTAV(I,J) = 1.0
                SMSTOT(I,J) = 1.0
                DO K = 1, NSOIL
                   SMOIS(I,K,J) = 1.0
                    TSLB(I,K,J) = 273.16
                ENDDO
             ELSE
                IF(XICE(I,J) == 1.) THEN
                   SMSTAV(I,J) = 1.0
                   SMSTOT(I,J) = 1.0
                   DO K = 1, NSOIL
                      SMOIS(I,K,J) = 1.0
                   ENDDO
                ENDIF
             ENDIF
          ENDDO
       ENDIF



   ILOOP : DO I = its, ite

    IF (XICE(I,J) >= XICE_THRES) THEN
       ICE = 1
    ELSE IF ( IVGTYP(I,J) == ISICE ) THEN
       ICE = -1
    ELSE
       ICE=0
    ENDIF

    IF((XLAND(I,J)-1.5) >= 0.) CYCLE ILOOP

    IF ( ICE == 1) THEN

       SH2O (i,1:NSOIL,j) = 1.0
       XLAIXY(i,j) = 0.01

       CYCLE ILOOP

    ELSE





       COSZ = COSZIN (I,J)
       LAT = XLATIN (I,J)
       Z_ML = 0.5*DZ8W(I,1,J)
       VEGTYP = IVGTYP(I,J)
       SOILTYP= ISLTYP(I,J)
       FVEG = VEGFRA(I,J)/100.
       FVGMAX = VEGMAX (I,J)/100.
       TBOT = TMN(I,J)
       T_ML = T3D(I,1,J)
       Q_ML = QV3D(I,1,J)/(1.0+QV3D(I,1,J))
       U_ML = U_PHY(I,1,J)
       V_ML = V_PHY(I,1,J)
       SWDN = SWDOWN(I,J)
       LWDN = GLW(I,J)
       P_ML =(P8W3D(I,KTS+1,J)+P8W3D(I,KTS,J))*0.5

       PSFC = P8W3D(I,1,J)
       PRCP = RAINBL(I,J)/DT



       ISNOW = ISNOWXY (I,J)
       SMC ( 1:NSOIL) = SMOIS (I, 1:NSOIL,J)
       SMH2O( 1:NSOIL) = SH2O (I, 1:NSOIL,J)
       STC (-NSNOW+1: 0) = TSNOXY (I,-NSNOW+1: 0,J)
       STC ( 1:NSOIL) = TSLB (I, 1:NSOIL,J)
       SWE = SNOW (I,J)
       SNDPTH = SNOWH (I,J)
       QSFC1D = QSFC (I,J)



       TV = TVXY (I,J)
       TG = TGXY (I,J)
       CANLIQ = CANLIQXY(I,J)
       CANICE = CANICEXY(I,J)
       EAH = EAHXY (I,J)
       TAH = TAHXY (I,J)
       CM = CMXY (I,J)
       CH = CHXY (I,J)
       FWET = FWETXY (I,J)
       SNEQVO = SNEQVOXY(I,J)
       ALBOLD = ALBOLDXY(I,J)
       QSNOW = QSNOWXY (I,J)
       WSLAKE = WSLAKEXY(I,J)
       ZWT = ZWTXY (I,J)
       WA = WAXY (I,J)
       WT = WTXY (I,J)
       ZSNSO(-NSNOW+1:NSOIL) = ZSNSOXY (I,-NSNOW+1:NSOIL,J)
       SNICE(-NSNOW+1: 0) = SNICEXY (I,-NSNOW+1: 0,J)
       SNLIQ(-NSNOW+1: 0) = SNLIQXY (I,-NSNOW+1: 0,J)
       LFMASS = LFMASSXY(I,J)
       RTMASS = RTMASSXY(I,J)
       STMASS = STMASSXY(I,J)
       WOOD = WOODXY (I,J)
       STBLCP = STBLCPXY(I,J)
       FASTCP = FASTCPXY(I,J)
       PLAI = XLAIXY (I,J)
       PSAI = XSAIXY (I,J)
       TAUSS = TAUSSXY (I,J)



       FICEOLD = 0.0
       FICEOLD(ISNOW+1:0) = SNICEXY(I,ISNOW+1:0,J) &
           /(SNICEXY(I,ISNOW+1:0,J)+SNLIQXY(I,ISNOW+1:0,J))
       CO2PP = CO2 * P_ML
       O2PP = O2 * P_ML
       FOLN = 1.0
       QC = undefined_value
       PBLH = undefined_value
       DZ8W1D = DZ8W (I,1,J)
       SLOPETYP = 1
       IST = 1
       ISC = 4


       IF(SOILTYP == 14 .AND. XICE(I,J) == 0.) THEN
          IF(IPRINT) PRINT *, ' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
          IF(IPRINT) PRINT *, i,j,'RESET SOIL in surfce.F'
          SOILTYP = 7
       ENDIF

       IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == 31 .or. &
            IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33) THEN
          VEGTYP = ISURBAN
       ENDIF
       IF(VEGTYP == 25) FVEG = 0.0
       IF(VEGTYP == 25) PLAI = 0.0
       IF(VEGTYP == 26) FVEG = 0.0
       IF(VEGTYP == 26) PLAI = 0.0
       IF(VEGTYP == 27) FVEG = 0.0
       IF(VEGTYP == 27) PLAI = 0.0

       CALL REDPRM (VEGTYP,SOILTYP,SLOPETYP,ZSOIL,NSOIL,ISURBAN)

    IF ( ICE == -1 ) THEN


       CALL NOAHMP_GLACIER( I, J, COSZ, NSNOW, NSOIL, DT, &
                               T_ML, P_ML, U_ML, V_ML, Q_ML, SWDN, &
                               PRCP, LWDN, TBOT, Z_ML, FICEOLD, ZSOIL, &
                              QSNOW, SNEQVO, ALBOLD, CM, CH, ISNOW, &
                                SWE, SMC, ZSNSO, SNDPTH, SNICE, SNLIQ, &
                                TGB, STC, SMH2O, TAUSS, QSFC1D, &
                                FSA, FSR, FIRA, FSH, FGEV, SSOIL, &
                               TRAD, ESOIL, RUNSF, RUNSB, SAG, SALB, &
                              QSNBOT,PONDING,PONDING1,PONDING2, T2MB, Q2MB, &
         EMISSI, FPICE, CHB2 )

       FSNO = 1.0
       TV = undefined_value
       TG = TGB
       CANICE = undefined_value
       CANLIQ = undefined_value
       EAH = undefined_value
       TAH = undefined_value
       FWET = undefined_value
       WSLAKE = undefined_value
       ZWT = undefined_value
       WA = undefined_value
       WT = undefined_value
       LFMASS = undefined_value
       RTMASS = undefined_value
       STMASS = undefined_value
       WOOD = undefined_value
       STBLCP = undefined_value
       FASTCP = undefined_value
       PLAI = undefined_value
       PSAI = undefined_value
       T2MV = undefined_value
       Q2MV = undefined_value
       NEE = undefined_value
       GPP = undefined_value
       NPP = undefined_value
       FVEGMP = 0.0
       ECAN = undefined_value
       ETRAN = undefined_value
       APAR = undefined_value
       PSN = undefined_value
       SAV = undefined_value
       RSSUN = undefined_value
       RSSHA = undefined_value
       BGAP = undefined_value
       WGAP = undefined_value
       TGV = undefined_value
       CHV = undefined_value
       CHB = CH
       IRC = undefined_value
       IRG = undefined_value
       SHC = undefined_value
       SHG = undefined_value
       EVG = undefined_value
       GHV = undefined_value
       IRB = FIRA
       SHB = FSH
       EVB = FGEV
       GHB = SSOIL
       TR = undefined_value
       EVC = undefined_value
       CHLEAF = undefined_value
       CHUC = undefined_value
       CHV2 = undefined_value
       FCEV = undefined_value
       FCTR = undefined_value

       QFX(I,J) = ESOIL


    ELSE
    goto 1000
if(i==1.and.j==8) then
    print*,I , J , LAT , YEARLEN , JULIAN , COSZ
    print*,'DT'
    print*,DT , DX , DZ8W1D , NSOIL , ZSOIL , 3
    print*,'FVEG'
    print*,FVEG , FVGMAX , VEGTYP , ISURBAN , ICE , IST
    print*,ISC
    print*,IZ0TLND
    print*,'T_ML'
    print*,T_ML , P_ML , PSFC , U_ML , V_ML , Q_ML
    print*,'QC'
    print*,QC , SWDN , LWDN , PRCP , TBOT , CO2PP
    print*,'O2PP'
    print*,O2PP , FOLN , FICEOLD , PBLH , Z_ML
    print*,'ALBOLD'
    print*,ALBOLD , SNEQVO
    print*,'STC'
    print*,STC , SMH2O , SMC , TAH , EAH , FWET
    print*,'CANLIQ'
    print*,CANLIQ , CANICE , TV , TG , QSFC1D , QSNOW
    print*,'ISNOW'
    print*,ISNOW , ZSNSO , SNDPTH , SWE , SNICE , SNLIQ
    print*,'ZWT'
    print*,ZWT , WA , WT , WSLAKE , LFMASS , RTMASS
    print*,'STMASS'
    print*,STMASS , WOOD , STBLCP , FASTCP , PLAI , PSAI
    print*,'CM'
    print*,CM , CH , TAUSS
    print*,'FSA'
    print*,FSA , FSR , FIRA , FSH , SSOIL , FCEV
    print*,'FGEV'
    print*,FGEV , FCTR , ECAN , ETRAN , ESOIL , TRAD
    print*,'TGB'
    print*, TGB , TGV , T2MV , T2MB
    print*,'Q2MV'
    print*, Q2MV , Q2MB , RUNSF , RUNSB , APAR
    print*,'PSN'
    print*,PSN , SAV , SAG , FSNO , NEE , GPP
    print*,'NPP'
    print*,NPP , FVEGMP , SALB , QSNBOT , PONDING , PONDING1
    print*,'PONDING2'
    print*,PONDING2, RSSUN , RSSHA , BGAP , WGAP
    print*,'CHV'
    print*, CHV , CHB , EMISSI
end if

1000 continue

       CALL NOAHMP_SFLX (&
            I , J , LAT , YEARLEN , JULIAN , COSZ , &
            DT , DX , DZ8W1D , NSOIL , ZSOIL , NSNOW , &
            FVEG , FVGMAX , VEGTYP , ISURBAN , ICE , IST , &
            ISC , &
            IZ0TLND , &
            T_ML , P_ML , PSFC , U_ML , V_ML , Q_ML , &
            QC , SWDN , LWDN , PRCP , TBOT , CO2PP , &
            O2PP , FOLN , FICEOLD , PBLH , Z_ML , &
            ALBOLD , SNEQVO , &
            STC , SMH2O , SMC , TAH , EAH , FWET , &
            CANLIQ , CANICE , TV , TG , QSFC1D , QSNOW , &
            ISNOW , ZSNSO , SNDPTH , SWE , SNICE , SNLIQ , &
            ZWT , WA , WT , WSLAKE , LFMASS , RTMASS , &
            STMASS , WOOD , STBLCP , FASTCP , PLAI , PSAI , &
            CM , CH , TAUSS , &
            FSA , FSR , FIRA , FSH , SSOIL , FCEV , &
            FGEV , FCTR , ECAN , ETRAN , ESOIL , TRAD , &
            TGB , TGV , T2MV , T2MB , Q2MV , Q2MB , &
            RUNSF , RUNSB , APAR , PSN , SAV , SAG , &
            FSNO , NEE , GPP , NPP , FVEGMP , SALB , &
            QSNBOT , PONDING , PONDING1, PONDING2, RSSUN , RSSHA , &
            BGAP , WGAP , CHV , CHB , EMISSI , &
            SHG , SHC , SHB , EVG , EVB , GHV , &
     GHB , IRG , IRC , IRB , TR , EVC , &
     CHLEAF , CHUC , CHV2 , CHB2 , FPICE )

            QFX(I,J) = ECAN + ESOIL + ETRAN

   ENDIF



             TSK (I,J) = TRAD
             HFX (I,J) = FSH
             LH (I,J) = FCEV + FGEV + FCTR
             GRDFLX (I,J) = SSOIL
      SMSTAV (I,J) = 0.0
             SMSTOT (I,J) = 0.0
             SFCRUNOFF(I,J) = SFCRUNOFF(I,J) + RUNSF * DT
             UDRUNOFF (I,J) = UDRUNOFF(I,J) + RUNSB * DT
             IF ( SALB > -999 ) THEN
                ALBEDO(I,J) = SALB
             ENDIF
             SNOWC (I,J) = FSNO
             SMOIS (I, 1:NSOIL,J) = SMC ( 1:NSOIL)
             SH2O (I, 1:NSOIL,J) = SMH2O ( 1:NSOIL)
             TSLB (I, 1:NSOIL,J) = STC ( 1:NSOIL)
             SNOW (I,J) = SWE
             SNOWH (I,J) = SNDPTH
             CANWAT (I,J) = CANLIQ + CANICE
             ACSNOW (I,J) = ACSNOW(I,J) + PRCP * FPICE
             ACSNOM (I,J) = ACSNOM(I,J) + QSNBOT*DT + PONDING + PONDING1 + PONDING2
             EMISS (I,J) = EMISSI
             QSFC (I,J) = QSFC1D

             ISNOWXY (I,J) = ISNOW
             TVXY (I,J) = TV
             TGXY (I,J) = TG
             CANLIQXY (I,J) = CANLIQ
             CANICEXY (I,J) = CANICE
             EAHXY (I,J) = EAH
             TAHXY (I,J) = TAH
             CMXY (I,J) = CM
             CHXY (I,J) = CH
             FWETXY (I,J) = FWET
             SNEQVOXY (I,J) = SNEQVO
             ALBOLDXY (I,J) = ALBOLD
             QSNOWXY (I,J) = QSNOW
             WSLAKEXY (I,J) = WSLAKE
             ZWTXY (I,J) = ZWT
             WAXY (I,J) = WA
             WTXY (I,J) = WT
             TSNOXY (I,-NSNOW+1: 0,J) = STC (-NSNOW+1: 0)
             ZSNSOXY (I,-NSNOW+1:NSOIL,J) = ZSNSO (-NSNOW+1:NSOIL)
             SNICEXY (I,-NSNOW+1: 0,J) = SNICE (-NSNOW+1: 0)
             SNLIQXY (I,-NSNOW+1: 0,J) = SNLIQ (-NSNOW+1: 0)
             LFMASSXY (I,J) = LFMASS
             RTMASSXY (I,J) = RTMASS
             STMASSXY (I,J) = STMASS
             WOODXY (I,J) = WOOD
             STBLCPXY (I,J) = STBLCP
             FASTCPXY (I,J) = FASTCP
             XLAIXY (I,J) = PLAI
             XSAIXY (I,J) = PSAI
             TAUSSXY (I,J) = TAUSS



             T2MVXY (I,J) = T2MV
             T2MBXY (I,J) = T2MB
             Q2MVXY (I,J) = Q2MV/(1.0 - Q2MV)
             Q2MBXY (I,J) = Q2MB/(1.0 - Q2MB)
             TRADXY (I,J) = TRAD
             NEEXY (I,J) = NEE
             GPPXY (I,J) = GPP
             NPPXY (I,J) = NPP
             FVEGXY (I,J) = FVEGMP
             RUNSFXY (I,J) = RUNSF
             RUNSBXY (I,J) = RUNSB
             ECANXY (I,J) = ECAN
             EDIRXY (I,J) = ESOIL
             ETRANXY (I,J) = ETRAN
             FSAXY (I,J) = FSA
             FIRAXY (I,J) = FIRA
             APARXY (I,J) = APAR
             PSNXY (I,J) = PSN
             SAVXY (I,J) = SAV
             SAGXY (I,J) = SAG
             RSSUNXY (I,J) = RSSUN
             RSSHAXY (I,J) = RSSHA
             BGAPXY (I,J) = BGAP
             WGAPXY (I,J) = WGAP
             TGVXY (I,J) = TGV
             TGBXY (I,J) = TGB
             CHVXY (I,J) = CHV
             CHBXY (I,J) = CHB
             IRCXY (I,J) = IRC
             IRGXY (I,J) = IRG
             SHCXY (I,J) = SHC
             SHGXY (I,J) = SHG
             EVGXY (I,J) = EVG
             GHVXY (I,J) = GHV
             IRBXY (I,J) = IRB
             SHBXY (I,J) = SHB
             EVBXY (I,J) = EVB
             GHBXY (I,J) = GHB
             TRXY (I,J) = TR
             EVCXY (I,J) = EVC
             CHLEAFXY (I,J) = CHLEAF
             CHUCXY (I,J) = CHUC
             CHV2XY (I,J) = CHV2
             CHB2XY (I,J) = CHB2

          ENDIF

      ENDDO ILOOP
   ENDDO JLOOP


  END SUBROUTINE noahmplsm


  SUBROUTINE NOAHMP_INIT ( MMINLU, SNOW , SNOWH , CANWAT , ISLTYP , &
       TSLB , SMOIS , SH2O , DZS , FNDSOILW , FNDSNOWH , &
       TSK, isnowxy , tvxy ,tgxy ,canicexy , &
       canliqxy ,eahxy ,tahxy ,cmxy ,chxy , &
       fwetxy ,sneqvoxy ,alboldxy ,qsnowxy ,wslakexy ,zwtxy ,waxy , &
       wtxy ,tsnoxy ,zsnsoxy ,snicexy ,snliqxy ,lfmassxy ,rtmassxy , &
       stmassxy ,woodxy ,stblcpxy ,fastcpxy ,xsaixy , &

       t2mvxy ,t2mbxy ,chstarxy, &

       NSOIL, restart, &
       allowed_to_read , &
       ids,ide, jds,jde, kds,kde, &
       ims,ime, jms,jme, kms,kme, &
       its,ite, jts,jte, kts,kte )



    INTEGER, INTENT(IN ) :: ids,ide, jds,jde, kds,kde, &
         & ims,ime, jms,jme, kms,kme, &
         & its,ite, jts,jte, kts,kte

    INTEGER, INTENT(IN) :: NSOIL

    LOGICAL, INTENT(IN) :: restart, &
         & allowed_to_read

    REAL, DIMENSION( NSOIL), INTENT(IN) :: DZS

    REAL, DIMENSION( ims:ime, NSOIL, jms:jme ) , &
         & INTENT(INOUT) :: SMOIS, &
         & SH2O, &
         & TSLB

    REAL, DIMENSION( ims:ime, jms:jme ) , &
         & INTENT(INOUT) :: SNOW, &
         & SNOWH, &
         & CANWAT

    INTEGER, DIMENSION( ims:ime, jms:jme ), &
         & INTENT(IN) :: ISLTYP

    LOGICAL, INTENT(IN) :: FNDSOILW, &
         & FNDSNOWH

    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: TSK
    INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isnowxy
    REAL, DIMENSION(ims:ime,-2:NSOIL,jms:jme), INTENT(INOUT) :: zsnsoxy
    REAL, DIMENSION(ims:ime,-2: 0,jms:jme), INTENT(INOUT) :: tsnoxy
    REAL, DIMENSION(ims:ime,-2: 0,jms:jme), INTENT(INOUT) :: snicexy
    REAL, DIMENSION(ims:ime,-2: 0,jms:jme), INTENT(INOUT) :: snliqxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tvxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tgxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canicexy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canliqxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: eahxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tahxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: cmxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fwetxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: sneqvoxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: alboldxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: qsnowxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wslakexy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: zwtxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: waxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wtxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lfmassxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: rtmassxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stmassxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: woodxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stblcpxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fastcpxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: xsaixy


    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mvxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mbxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chstarxy



    REAL, DIMENSION(1:NSOIL) :: ZSOIL


    REAL :: BX, SMCMAX, PSISAT

    REAL, PARAMETER :: BLIM = 5.5
    REAL, PARAMETER :: HLICE = 3.335E5
    REAL, PARAMETER :: GRAV = 9.81
    REAL, PARAMETER :: T0 = 273.15

    INTEGER :: errflag

    character(len=80) :: err_message
    character(len=4) :: MMINSL
    character(len=*), intent(in) :: MMINLU
    MMINSL='STAS'

    call read_mp_veg_parameters(trim(MMINLU))




    IF ( allowed_to_read ) THEN
       CALL wrf_message( 'INITIALIZE THREE Noah LSM RELATED TABLES' )
       CALL SOIL_VEG_GEN_PARM( MMINLU, MMINSL )
    ENDIF

    IF( .NOT. restart ) THEN

       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)

       errflag = 0
       DO j = jts,jtf
          DO i = its,itf
             IF ( ISLTYP( i,j ) .LT. 1 ) THEN
                errflag = 1
                WRITE(err_message,*)"module_sf_noahlsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
                CALL wrf_message(err_message)
             ENDIF
          ENDDO
       ENDDO
       IF ( errflag .EQ. 1 ) THEN
          CALL wrf_error_fatal3("<stdin>",921,&
"module_sf_noahlsm.F: lsminit: out of range value "// &
               "of ISLTYP. Is this field in the input?" )
       ENDIF



       DO J = jts , jtf
          DO I = its , itf
             BX = BB(ISLTYP(I,J))
             SMCMAX = MAXSMC(ISLTYP(I,J))
             PSISAT = SATPSI(ISLTYP(I,J))
             IF ( ( BX > 0.0 ) .AND. ( SMCMAX > 0.0 ) .AND. ( PSISAT > 0.0 ) ) THEN
                DO NS=1, NSOIL
                   IF ( TSLB(I,NS,J) < 273.149 ) THEN
                      FK=(( (HLICE/(GRAV*(-PSISAT))) * &
                           ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BX) )*SMCMAX
                      FK = MAX(FK, 0.02)
                      SH2O(I,NS,J) = MIN( FK, SMOIS(I,NS,J) )
                   ELSE
                      SH2O(I,NS,J)=SMOIS(I,NS,J)
                   ENDIF
                END DO
             ELSE
                DO NS=1, NSOIL
                   SH2O(I,NS,J)=SMOIS(I,NS,J)
                END DO
             ENDIF
          ENDDO
       ENDDO






       IF(.NOT.FNDSNOWH)THEN

          CALL wrf_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
          DO J = jts,jtf
             DO I = its,itf
                SNOWH(I,J)=SNOW(I,J)*0.005
             ENDDO
          ENDDO
       ENDIF

       DO J = jts,jtf
          DO I = its,itf
             tvxy (I,J) = TSK(I,J)
             tgxy (I,J) = TSK(I,J)
             CANWAT (I,J) = 0.0
             canliqxy (I,J) = CANWAT(I,J)
             canicexy (I,J) = 0.
             eahxy (I,J) = 2000.
             tahxy (I,J) = TSK(I,J)


             t2mvxy (I,J) = TSK(I,J)
             t2mbxy (I,J) = TSK(I,J)
             chstarxy (I,J) = 0.1


             cmxy (I,J) = 0.0
             chxy (I,J) = 0.0
             fwetxy (I,J) = 0.0
             sneqvoxy (I,J) = 0.0
             alboldxy (I,J) = 0.65
             qsnowxy (I,J) = 0.0
             wslakexy (I,J) = 0.0

             waxy (I,J) = 4900.
             wtxy (I,J) = waxy(i,j)
             zwtxy (I,J) = (25. + 2.0) - waxy(i,j)/1000/0.2

             lfmassxy (I,J) = 50.
             stmassxy (I,J) = 50.0
             rtmassxy (I,J) = 500.0
             woodxy (I,J) = 500.0
             stblcpxy (I,J) = 1000.0
             fastcpxy (I,J) = 1000.0
             xsaixy (I,J) = 0.1

          enddo
       enddo



       ZSOIL(1) = -DZS(1)
       DO NS=2, NSOIL
          ZSOIL(NS) = ZSOIL(NS-1) - DZS(NS)
       END DO



       CALL snow_init ( ims , ime , jms , jme , its , itf , jts , jtf , 3 , &
            & NSOIL , zsoil , snow , tgxy , snowh , &
            & zsnsoxy , tsnoxy , snicexy , snliqxy , isnowxy )

    ENDIF
  END SUBROUTINE NOAHMP_INIT




  SUBROUTINE SNOW_INIT ( ims , ime , jms , jme , its , itf , jts , jtf , &
       & NSNOW , NSOIL , ZSOIL , SWE , TGXY , SNODEP , &
       & ZSNSOXY , TSNOXY , SNICEXY ,SNLIQXY , ISNOWXY )
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ims, ime, jms, jme
    INTEGER, INTENT(IN) :: its, itf, jts, jtf
    INTEGER, INTENT(IN) :: NSNOW
    INTEGER, INTENT(IN) :: NSOIL
    REAL, INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SWE
    REAL, INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SNODEP
    REAL, INTENT(IN), DIMENSION(ims:ime, jms:jme) :: TGXY
    REAL, INTENT(IN), DIMENSION(1:NSOIL) :: ZSOIL
    INTEGER, INTENT(OUT), DIMENSION(ims:ime, jms:jme) :: ISNOWXY
    REAL, INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: ZSNSOXY
    REAL, INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1: 0,jms:jme) :: TSNOXY
    REAL, INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1: 0,jms:jme) :: SNICEXY
    REAL, INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1: 0,jms:jme) :: SNLIQXY
    INTEGER :: I,J,IZ
    REAL, DIMENSION(-NSNOW+1: 0) :: DZSNO
    REAL, DIMENSION(-NSNOW+1:NSOIL) :: DZSNSO
    DO J = jts , jtf
       DO I = its , itf
          IF ( SNODEP(I,J) < 0.025 ) THEN
             ISNOWXY(I,J) = 0
             DZSNO(-NSNOW+1:0) = 0.
          ELSE
             IF ( ( SNODEP(I,J) >= 0.025 ) .AND. ( SNODEP(I,J) <= 0.05 ) ) THEN
                ISNOWXY(I,J) = -1
                DZSNO(0) = SNODEP(I,J)
             ELSE IF ( ( SNODEP(I,J) > 0.05 ) .AND. ( SNODEP(I,J) <= 0.10 ) ) THEN
                ISNOWXY(I,J) = -2
                DZSNO(-1) = SNODEP(I,J)/2.
                DZSNO( 0) = SNODEP(I,J)/2.
             ELSE IF ( (SNODEP(I,J) > 0.10 ) .AND. ( SNODEP(I,J) <= 0.25 ) ) THEN
                ISNOWXY(I,J) = -2
                DZSNO(-1) = 0.05
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1)
             ELSE IF ( ( SNODEP(I,J) > 0.25 ) .AND. ( SNODEP(I,J) <= 0.45 ) ) THEN
                ISNOWXY(I,J) = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.5*(SNODEP(I,J)-DZSNO(-2))
                DZSNO( 0) = 0.5*(SNODEP(I,J)-DZSNO(-2))
             ELSE IF ( SNODEP(I,J) > 0.45 ) THEN
                ISNOWXY(I,J) = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.20
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1) - DZSNO(-2)
             ELSE
                CALL wrf_error_fatal3("<stdin>",1092,&
"Problem with the logic assigning snow layers.")
             END IF
          END IF
          TSNOXY (I,-NSNOW+1:0,J) = 0.
          SNICEXY(I,-NSNOW+1:0,J) = 0.
          SNLIQXY(I,-NSNOW+1:0,J) = 0.
          DO IZ = ISNOWXY(I,J)+1 , 0
             TSNOXY(I,IZ,J) = TGXY(I,J)
             SNLIQXY(I,IZ,J) = 0.00
             SNICEXY(I,IZ,J) = 1.00 * DZSNO(IZ) * (SWE(I,J)/SNODEP(I,J))
          END DO
          DO IZ = ISNOWXY(I,J)+1 , 0
             DZSNSO(IZ) = -DZSNO(IZ)
          END DO
          DZSNSO(1) = ZSOIL(1)
          DO IZ = 2 , NSOIL
             DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
          END DO
          ZSNSOXY(I,ISNOWXY(I,J)+1,J) = DZSNSO(ISNOWXY(I,J)+1)
          DO IZ = ISNOWXY(I,J)+2 , NSOIL
             ZSNSOXY(I,IZ,J) = ZSNSOXY(I,IZ-1,J) + DZSNSO(IZ)
          ENDDO
       END DO
    END DO
  END SUBROUTINE SNOW_INIT
END MODULE module_sf_noahmpdrv
