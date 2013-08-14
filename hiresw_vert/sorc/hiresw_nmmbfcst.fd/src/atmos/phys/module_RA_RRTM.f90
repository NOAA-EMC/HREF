
!
      MODULE MODULE_RA_RRTM
!
!-----------------------------------------------------------------------
!
!***  THE RADIATION DRIVERS AND PACKAGES
!
!-----------------------------------------------------------------------
!
      USE MODULE_INCLUDE
!
      use machine,                   only : kind_phys

      USE MODULE_CONSTANTS,          ONLY : R,CP,PI,EPSQ,STBOLT,EP_2
      USE MODULE_NMMB_RADIATION_DRIVER,   ONLY : RADINIT, GRRAD_NMMB
      USE MODULE_NMMB_RADIATION_ASTRONOMY,     ONLY : ASTRONOMY_NMMB

!     USE n_OZNE_DEF, ONLY: LEVOZC, LATSOZP, BLATC, TIMEOZC, TIMEOZ,   &
      USE OZNE_DEF,   ONLY: LEVOZC, LATSOZP, BLATC, TIMEOZC, TIMEOZ,   &
                            KOZC, DPHIOZC, LATSOZC, PL_COEFF, LEVOZP

      USE MODULE_MP_ETANEW, ONLY : RHgrd,T_ICE,FPVS

      use module_radsw_parameters,  only : topfsw_type, sfcfsw_type
      use module_radlw_parameters,  only : topflw_type, sfcflw_type

!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: RRTM,RRTM_INIT
!
!-----------------------------------------------------------------------
!
!--- Used for Gaussian look up tables
!
      REAL, PRIVATE,PARAMETER :: XSDmax=3.1, DXSD=.01
      INTEGER, PRIVATE,PARAMETER :: NXSD=XSDmax/DXSD
      REAL, DIMENSION(NXSD),PRIVATE,SAVE :: AXSD
      REAL, PRIVATE :: RSQR
      LOGICAL, PRIVATE,SAVE :: SDprint=.FALSE.

!-------------------------------
      INTEGER, SAVE, DIMENSION(3)     :: LTOP
      REAL,SAVE,DIMENSION(4) :: PTOPC
!--------------------------------
!
      REAL, PARAMETER ::  &
     &   TRAD_ice=0.5*T_ice      & !--- Very tunable parameter
     &,  ABSCOEF_W=800.            & !--- Very tunable parameter
     &,  ABSCOEF_I=500.            & !--- Very tunable parameter
     &,  Qconv=0.1e-3            & !--- Very tunable parameter

     &,  CTauCW=ABSCOEF_W*Qconv  &
     &,  CTauCI=ABSCOEF_I*Qconv

!-- Set to TRUE to bogus in small amounts of convective clouds into the
!   input cloud calculations, but only if all of the following conditions 
!   are met:
!     (1) The maximum condensate mixing ratio is < QWmax.
!     (2) Only shallow convection is present, do not apply to deep convection.
!     (3) Only apply if the depth of shallow convection is between 
!         CU_DEEP_MIN (50 hPa) and CU_DEEP_MAX (200 hPa).
!     (4) Convective precipitation rate must be <0.01 mm/h.  
!
      LOGICAL, SAVE :: CUCLD=.TRUE.
!
!-- After several tuning experiments, a value for QW_CU=0.003 g/kg should 
!   produce a cloud fraction of O(25%) and a SW reduction of O(100 W/m**2) 
!   for shallow convection with a maximum depth/thickness of O(200 hPa).
!-- QW_Cu=0.003 g/kg, which translates to a 3% cloud fraction in 
!   subroutine progcld2 at each model layer near line 960 in 
!   radiation_clouds.f, which translates to a O(25%) total cloud fraction
!   in the lower atmosphere (i.e., for "low-level" cloud fractions).
!
      REAL, PARAMETER :: QW_Cu=0.003E-3,QWmax=1.E-7,CUPPT_min=1.e-5   &
                        ,CU_DEEP_MIN=50.E2,CU_DEEP_MAX=200.E2
 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  THE RADIATION PACKAGE OPTIONS
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
      SUBROUTINE RRTM (NTIMESTEP,DT_INT,JDAT                            &
     &                    ,NPHS,GLAT,GLON                               &
     &                    ,NRADS,NRADL                                  &
     &                    ,DSG2,SGML2,PDSG1,PSGML1                      &
     &                    ,PT,PD                                        &
     &                    ,T,Q,CW,O3                                    &
     &                    ,ALBEDO                                       &
     &                    ,F_ICE,F_RAIN                                 &
     &                    ,P_QV,P_QC,P_QR,P_QI,P_QS,P_QG                &
     &                    ,SM,CLDFRA                                    &
     &                    ,NUM_WATER,WATER                              &
     &                    ,RLWTT,RSWTT                                  &
     &                    ,RLWIN,RSWIN                                  &
     &                    ,RSWINC,RSWOUT                                &
     &                    ,RLWTOA,RSWTOA                                &
     &                    ,CZMEAN,SIGT4                                 &
     &                    ,CFRACL,CFRACM,CFRACH                         &
     &                    ,ACFRST,NCFRST                                &
     &                    ,ACFRCV,NCFRCV                                &
     &                    ,CUPPT,SNOW,SI                                &
     &                    ,HTOP,HBOT                                    &
     &                    ,TSKIN,Z0,SICE,F_RIMEF,MXSNAL,SGM,STDH,OMGALF &
     &                    ,IMS,IME,JMS,JME                              &
     &                    ,ITS,ITE,JTS,JTE                              &
     &                    ,LM                                           &
     &                    ,MYPE )
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IME,IMS,ITE,ITS                             &
     &                     ,JME,JMS,JTE,JTS                             &
     &                     ,LM,MYPE                                     &
     &                     ,NTIMESTEP,DT_INT                            &
     &                     ,NPHS,NRADL,NRADS                            &
     &                     ,NUM_WATER                      
!
      INTEGER,INTENT(IN) :: JDAT(8)
      ! INTEGER,INTENT(IN) :: IDAT(8)    ! not in use at this moment
!
      INTEGER,INTENT(IN) :: P_QV,P_QC,P_QR,P_QI,P_QS,P_QG
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: NCFRCV,NCFRST
!
      REAL,INTENT(IN) :: PT
!
      REAL,DIMENSION(1:LM),INTENT(IN) :: DSG2,PDSG1,PSGML1,SGML2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CUPPT               &
                                                   ,GLAT,GLON           &
                                                   ,PD,SM,SNOW,SI        
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ALBEDO

      REAL,DIMENSION(IMS:IME,JMS:JME,LM),INTENT(IN) :: CW,O3,Q,T
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(IN) :: F_ICE,F_RAIN
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACFRCV,ACFRST    &
                                                      ,RLWIN,RLWTOA     &
                                                      ,RSWIN,RSWOUT     &
                                                      ,HBOT,HTOP        &
                                                      ,RSWINC,RSWTOA
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LM),INTENT(INOUT) :: RLWTT,RSWTT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: CFRACH,CFRACL    &
                                                      ,CFRACM,CZMEAN    &
                                                      ,SIGT4
!
      REAL,DIMENSION(IMS:IME,JMS:JME,LM,NUM_WATER),INTENT(INOUT) :: WATER
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(OUT) :: CLDFRA
!
       REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: TSKIN,Z0,SICE      &
                                                    ,MXSNAL,STDH        
!
       REAL,DIMENSION(1:LM+1),INTENT(IN) :: SGM
!
       REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(IN) :: F_RIMEF,OMGALF
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      LOGICAL :: LSLWR, LSSAV, LDIAG3D, LPRNT, SASHAL, LSSWR
      LOGICAL :: norad_precip, crick_proof, ccnorm
!
      INTEGER,PARAMETER :: IFLIP=0, NFLUXR=39
!
      INTEGER :: NP3D, ISOL, ICO2, ICWP, IALB, IEMS, IAER, NUMX, NUMY,    &
                 NFXR, NTRAC, KFLIP,                                      &
                 I, L, J, K, NTOZ, NCLDX, NTCW, IOVR_SW, IOVR_LW,         &
                 ICTM, ISUBCSW, ISUBCLW, IOVRSW, IOVRLW,                  &
                 ipt,  kdt, I_t, J_t, N
!
      INTEGER,SAVE :: K1OZ, K2OZ
!
      REAL*8 :: FHSWR, FHLWR, FHAER, SOLCON,SLAG, SDEC, CDEC,DTSW, DTLW, RTvR
!
      REAL*8,SAVE :: FACOZ
!
      REAL*8,DIMENSION(1) :: FLGMIN_L, CV, CVB, CVT, HPRIME_V, TSEA,      &
                             TISFC, FICE, ZORL, SLMSK, SNWDPH, SNCOVR,    &
                             SNOALB, ALVSF1, ALNSF1, ALVWF1, ALNWF1,      &
                             FACSF1, FACWF1, SFCNSW, SFCDSW, SFALB,       &
                             SFCDLW, TSFLW, TOAUSW, TOADSW, SFCCDSW,      &
                             TOAULW, SFCUSW,                              &
                             SEMIS, XLAT, XLON

                           !===================================
                           ! SEMIS: surface lw emissivity
                           !        is intended output in GLOOPR
                           !        ** not NMMB in RRTM driver
                           !===================================

      INTEGER, DIMENSION(1) :: ICSDSW, ICSDLW

!---  variables of instantaneous calculated toa/sfc radiation fluxes
!      ** IM=1 for the dimension
!
      type (topfsw_type), dimension(1) :: topfsw
      type (sfcfsw_type), dimension(1) :: sfcfsw

      type (topflw_type), dimension(1) :: topflw
      type (sfcflw_type), dimension(1) :: sfcflw


!
      REAL*8,DIMENSION(LM) :: CLDCOV_V,PRSL,PRSLK,GT,GQ, VVEL,F_ICEC,     &
                               F_RAINC,R_RIME,TAUCLOUDS,CLDF

      REAL*8,DIMENSION(LM+1) :: PRSI , RSGM
      real (kind=kind_phys)  :: raddt, fdaer
!
      REAL*8,DIMENSION(5)  :: CLDSA_V
!
      REAL*8,DIMENSION(ITE-ITS+1,JTE-JTS+1) :: SINLAT_t,COSLAT_t,XLON_t   &
                                              ,COSZEN_t,COSZDG_t
!
      REAL*8,DIMENSION(NFLUXR) :: FLUXR_V
!
      REAL*8,DIMENSION(1,LM,3) :: GR1   
!
      REAL*8,DIMENSION(LM) :: SWH, HLW
!
      REAL :: BLATC4
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE,1:LM+1) :: P8W
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE,1:LM)   :: P_PHY
!
      LOGICAL,SAVE :: FIRST
      DATA FIRST / .TRUE. /
!
      INTEGER :: DAYS(13), IDAY, IMON, MIDMON, ID
!
      INTEGER, SAVE :: MIDM, MIDP
!
      LOGICAL :: CHANGE
!
      DATA DAYS / 31,28,31,30,31,30,31,31,30,31,30,31,30 /
!
      REAL :: ZEN,DZEN,ALB1,ALB2
!
      INTEGER :: IR,IQ,JX
!
      REAL,PARAMETER :: TWENTY=20.0, &
                        HP537=0.537, &
                        ONE=1., &
                        DEGRAD1=180.0/PI, &
                        H74E1=74.0, &
                        HAF=0.5, &
                        HNINETY=90., &
                        FIFTY=50., &
                        QUARTR=0.25, &
                        HNINE=9.0, &
                        HP1=0.1, &
                        H15E1=15.0
      REAL,DIMENSION(20) :: ZA
      REAL,DIMENSION(19) :: DZA
      REAL,DIMENSION(21,20) :: ALBD
      REAL,DIMENSION(21) :: TRN
!
      DATA TRN/.00,.05,.10,.15,.20,.25,.30,.35,.40,.45,.50,.55,.60,.65, &
               .70,.75,.80,.85,.90,.95,1.00/
      
      DATA  ALBD/.061,.062,.072,.087,.115,.163,.235,.318,.395,.472,.542, &
       .604,.655,.693,.719,.732,.730,.681,.581,.453,.425,.061,.062,.070, &
       .083,.108,.145,.198,.263,.336,.415,.487,.547,.595,.631,.656,.670, &
       .652,.602,.494,.398,.370,.061,.061,.068,.079,.098,.130,.174,.228, &
       .290,.357,.424,.498,.556,.588,.603,.592,.556,.488,.393,.342,.325, &
       .061,.061,.065,.073,.086,.110,.150,.192,.248,.306,.360,.407,.444, &
       .469,.480,.474,.444,.386,.333,.301,.290,.061,.061,.065,.070,.082, &
       .101,.131,.168,.208,.252,.295,.331,.358,.375,.385,.377,.356,.320, &
       .288,.266,.255,.061,.061,.063,.068,.077,.092,.114,.143,.176,.210, &
       .242,.272,.288,.296,.300,.291,.273,.252,.237,.266,.220,.061,.061, &
       .062,.066,.072,.084,.103,.127,.151,.176,.198,.219,.236,.245,.250, &
       .246,.235,.222,.211,.205,.200,                                    &
                 .061,.061,.061,.065,.071,.079,.094,.113,.134,.154,.173, &
       .185,.190,.193,.193,.190,.188,.185,.182,.180,.178,.061,.061,.061, &
       .064,.067,.072,.083,.099,.117,.135,.150,.160,.164,.165,.164,.162, &
       .160,.159,.158,.157,.157,.061,.061,.061,.062,.065,.068,.074,.084, &
       .097,.111,.121,.127,.130,.131,.131,.130,.129,.127,.126,.125,.122, &
       .061,.061,.061,.061,.062,.064,.070,.076,.085,.094,.101,.105,.107, &
       .106,.103,.100,.097,.096,.095,.095,.095,.061,.061,.061,.060,.061, &
       .062,.065,.070,.075,.081,.086,.089,.090,.088,.084,.080,.077,.075, &
       .074,.074,.074,.061,.061,.060,.060,.060,.061,.063,.065,.068,.072, &
       .076,.077,.076,.074,.071,.067,.064,.062,.061,.061,.061,.061,.061, &
       .060,.060,.060,.060,.061,.062,.065,.068,.069,.069,.068,.065,.061, &
       .058,.055,.054,.053,.052,.052,                                    &
                 .061,.061,.060,.060,.060,.060,.060,.060,.062,.065,.065, &
       .063,.060,.057,.054,.050,.047,.046,.045,.044,.044,.061,.061,.060, &
       .060,.060,.059,.059,.059,.059,.059,.058,.055,.051,.047,.043,.039, &
       .035,.033,.032,.031,.031,.061,.061,.060,.060,.060,.059,.059,.058, &
       .057,.056,.054,.051,.047,.043,.039,.036,.033,.030,.028,.027,.026, &
       .061,.061,.060,.060,.060,.059,.059,.058,.057,.055,.052,.049,.045, &
       .040,.036,.032,.029,.027,.026,.025,.025,.061,.061,.060,.060,.060, &
       .059,.059,.058,.056,.053,.050,.046,.042,.038,.034,.031,.028,.026, &
       .025,.025,.025,.061,.061,.060,.060,.059,.058,.058,.057,.055,.053, &
       .050,.046,.042,.038,.034,.030,.028,.029,.025,.025,.025/
!
      DATA ZA/90.,88.,86.,84.,82.,80.,78.,76.,74.,70.,66.,62.,58.,54., &
              50.,40.,30.,20.,10.,0.0/
!
      DATA DZA/8*2.0,6*4.0,5*10.0/
!
      REAL :: ALBD0,ALVD1,ALND1
!
      REAL,DIMENSION(1) :: ALVB,ALNB,ALVD,ALND
!
      INTEGER :: IRTN, IERROR, o3clm_unit
!
      REAL :: RJDAY, WEI2M, WEI1M, WEI1S, WEI2S, BLTO, BLNO
!
      INTEGER :: JDOY, JDAY, JDOW, MMM, MMP, MM, IRET, MONEND, &
                 MON1, IS2, ISX, KPD9, IS1, NN, MON2, MON, IS, &  
                 LUGB, LEN, M1, M2, K1, K2, JMSK, IMSK       
!
      INTEGER :: KPDALB(4)
!
      CHARACTER*500 :: FNALBC,FNALBC2,FNMSKH
!
      REAL :: ALBCLM(1,4), ALFCLM(1,2),  &
              DAYHF(13)
!
      DATA DAYHF/ 15.5, 45.0, 74.5,105.0,135.5,166.0,          &
                196.5,227.5,258.0,288.5,319.0,349.5,380.5/
!
      REAL,ALLOCATABLE :: ALB(:,:,:), ALF(:,:)
!
      INTEGER :: MON1S, MON2S, SEA1S, SEA2S, SEA1, SEA2

      DATA MON1S/0/, MON2S/0/, SEA1S/0/, SEA2S/0/
!
      SAVE  ALB, ALF, MON1S, MON2S, SEA1S, SEA2S, DAYHF, K1, K2, M1, M2
!
      REAL :: ALBLMX,ALBLMN,ALBOMX,ALBOMN,ALBSMX, &
              ALBSMN,ALBIMX,ALBIMN,ALBJMX,ALBJMN, &
              EPSALB,PERCRIT
!
      REAL :: WV,QICE,QCLD,CLFR,ESAT,QSAT,RHUM,RHtot,ARG,SDM,   &
               PMOD,CONVPRATE,CLSTP,P1,P2,CC1,CC2,CLDMAX,CL1,CL2, &
               CR1,DPCL,PRS1,PRS2,DELP,TCLD,CTau,CFSmax,CFCmax,  &
               CFRAVG,TDUM,CU_DEPTH
!
      INTEGER :: IXSD,NTSPH,NRADPP,NC,NMOD,LCNVT,LCNVB,NLVL,MALVL, &
                 LLTOP,LLBOT,KBT2,KTH1,KBT1,KTH2,KTOP1,LM1,LL
!
      REAL, PARAMETER :: EPSQ1=1.E-5,EPSQ=1.E-12,EPSO3=1.E-10,H0=0., &
                         H1=1.,HALF=.5,T0C=273.15,CUPRATE=24.*1000., &
                         HPINC=HALF*1.E1, CLFRmin=0.01, TAUCmax=4.161, &
                         XSDmin=-XSDmax, DXSD1=-DXSD, STSDM=0.01, & 
                         CVSDM=.04,DXSD2=HALF*DXSD,DXSD2N=-DXSD2,PCLDY=0.25
!
      REAL,DIMENSION(10),SAVE :: CC,PPT
      LOGICAL, SAVE :: CNCLD=.TRUE.
      LOGICAL, SAVE :: OPER=.TRUE.
!
      DATA CC/0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/
      DATA PPT/0.,.14,.31,.70,1.6,3.4,7.7,17.,38.,85./
!
      REAL,DIMENSION(0:LM)  :: CLDAMT
!
      LOGICAL :: BITX,BITY,BITZ,BITW,BIT1,BIT2,NEW_CLOUD,CU_cloud
!
      REAL :: CTHK(3)
      DATA CTHK/20000.0,20000.0,20000.0/
! 
      REAL,DIMENSION(ITS:ITE,JTS:JTE,3):: CLDCFR
      INTEGER,DIMENSION(ITS:ITE,JTS:JTE,3):: MBOT,MTOP

      REAL,DIMENSION(ITS:ITE,JTS:JTE):: CUTOP,CUBOT
      
      REAL,DIMENSION(ITS:ITE,JTS:JTE,LM) :: TauCI,CSMID,CCMID
!
      INTEGER,DIMENSION(ITS:ITE,JTS:JTE,LM+1) :: KTOP, KBTM

      REAL,DIMENSION(ITS:ITE,JTS:JTE,LM+1) :: CAMT
!
      INTEGER,DIMENSION(ITS:ITE,JTS:JTE) :: NCLDS, KCLD
!
      REAL,DIMENSION(ITS:ITE,JTS:JTE,LM) :: TAUTOTAL
!
      INTEGER :: NKTP,NBTM,NCLD,LML
!
      REAL :: CLFR1,TauC,QSUM,DELPTOT,SFCALBEDO
!--------------------------------------------------------------------------------------------------
!
!***THIS SUBROUTINE SELECTS AND PREPARES THE NECESSARY INPUTS FOR GRRAD (GFS RRTM DRIVER)
!
!   GRRAD IS CALLED COLUMN BY COLUMN
!
!INPUTS/OUPUTS OF GRRAD: 
!    INPUT VARIABLES:                                                   !
!      PRSI  (LM+1)    : MODEL LEVEL PRESSURE IN CB (KPA)               !
!      PRSL  (LM)      : MODEL LAYER MEAN PRESSURE IN CB (KPA)          !
!      PRSLK (LM)      : Exner function (dimensionless)                 !
!      GT    (LM)      : MODEL LAYER MEAN TEMPERATURE IN K              !
!      GQ    (LM)      : LAYER SPECIFIC HUMIDITY IN GM/GM               !
!      GR1   (LM,NTRAC): TRACER ARRAY (WATER, OZONE,....)               !
!      VVEL   (LM)      : LAYER MEAN VERTICAL VELOCITY IN CB/SEC         ! !not used
!      SLMSK (1)       : SEA/LAND MASK ARRAY (SEA:0,LAND:1,SEA-ICE:2)   !
!      XLON,XLAT       : GRID LONGITUDE/LATITUDE IN RADIANS             !
!      TSEA  (1)       : SURFACE TEMPERATURE IN K                       !
!      SNWDPH (1)       : SNOW DEPTH WATER EQUIVALENT IN MM              !
!      SNCOVR(1)       : SNOW COVER IN FRACTION                         !
!      SNOALB(1)       : MAXIMUM SNOW ALBEDO IN FRACTION                !
!      ZORL  (1)       : SURFACE ROUGHNESS IN CM                        !
!      HPRIM_V (1)       : TOPOGRAPHIC STANDARD DEVIATION IN M            !
!      ALVSF1 (1)       : MEAN VIS ALBEDO WITH STRONG COSZ DEPENDENCY    !
!      ALNSF1 (1)       : MEAN NIR ALBEDO WITH STRONG COSZ DEPENDENCY    !
!      ALVWF1 (1)       : MEAN VIS ALBEDO WITH WEAK COSZ DEPENDENCY      !
!      ALNWF1 (1)       : MEAN NIR ALBEDO WITH WEAK COSZ DEPENDENCY      !
!      FACSF1 (1)       : FRACTIONAL COVERAGE WITH STRONG COSZ DEPENDEN  !
!      FACWF1 (1)       : FRACTIONAL COVERAGE WITH WEAK COSZ DEPENDENCY  !
!      FICE  (1)       : ICE FRACTION OVER OPEN WATER GRID              !
!      TISFC (1)       : SURFACE TEMPERATURE OVER ICE FRACTION          !
!      SOLCON          : SOLAR CONSTANT (SUN-EARTH DISTANT ADJUSTED)    !
!
!-- Following 5 quantities are defined within a local 'tile':
!      SINLAT_t        : SINE OF LATITUDE                               !
!      COSLAT_t        : COSINE OF LATITUDE                             !
!      XLON_t          : LONGITUDE                                      !
!      COSZEN_t        : MEAN COS OF ZENITH ANGLE OVER RAD CALL PERIOD  !
!      COSZDG_t        : MEAN COS OF ZENITH ANGLE OVER RAD CALL PERIOD  !
!
!      K1OZ,K2OZ,FACOZ : PARAMETERS FOR CLIMATOLOGICAL OZONE            !
!      CV    (1)       : FRACTION OF CONVECTIVE CLOUD                   ! !not used
!      CVT, CVB (1)    : CONVECTIVE CLOUD TOP/BOTTOM PRESSURE IN CB     ! !not used
!      IOVRSW/IOVRLW   : CONTROL FLAG FOR CLOUD OVERLAP (SW/LW RAD)     !
!                        =0 RANDOM OVERLAPPING CLOUDS                   !
!                        =1 MAX/RAN OVERLAPPING CLOUDS                  !
!      F_ICEC (LM)     : FRACTION OF CLOUD ICE  (IN FERRIER SCHEME)     !
!      F_RAINC(LM)     : FRACTION OF RAIN WATER (IN FERRIER SCHEME)     !
!      RRIME  (LM)     : MASS RATIO OF TOTAL TO UNRIMED ICE ( >= 1 )    !
!      FLGMIN_L(1)     : MINIMIM LARGE ICE FRACTION                     !
!      NP3D            : =3 BRAD FERRIER MICROPHYSICS SCHEME            ! 
!                           only stratiform clouds.                     !
!                           optical prop. and cloud fractions are calculated in grrad
!                        =4 ZHAO/CARR/SUNDQVIST MICROPHYSICS SCHEME     ! !not used
!                        =5 NAM stratiform + convective cloud optical   !
!                           clouds optical depth and fraction calculated here and are input for grrad
!      NTCW            : =0 NO CLOUD CONDENSATE CALCULATED              !
!                        >0 ARRAY INDEX LOCATION FOR CLOUD CONDENSATE   !
!      NCLDX           : ONLY USED WHEN NTCW .GT. 0                     !
!      NTOZ            : =0 CLIMATOLOGICAL OZONE PROFILE                !
!                        >0 INTERACTIVE OZONE PROFILE                   ! !does not work currently
!      NTRAC           : DIMENSION VERIABLE FOR ARRAY GR1               !
!      NFXR            : SECOND DIMENSION OF INPUT/OUTPUT ARRAY FLUXR   !
!      DTLW, DTSW      : TIME DURATION FOR LW/SW RADIATION CALL IN SEC  !
!      LSSWR, LSLWR    : LOGICAL FLAGS FOR SW/LW RADIATION CALLS        !
!      LSSAV           : LOGICAL FLAG FOR STORE 3-D CLOUD FIELD         !
!      LDIAG3D         : LOGICAL FLAG FOR STORE 3-D DIAGNOSTIC FIELDS   !
!      LM              : VERTICAL LAYER DIMENSION                       !
!      IFLIP           : CONTROL FLAG FOR IN/OUT VERTICAL INDEXING      !
!                        =0 INDEX FROM TOA TO SURFACE                   !
!                        =1 INDEX FROM SURFACE TO TOA                   !
!      MYPE            : CONTROL FLAG FOR PARALLEL PROCESS              !
!      LPRNT           : CONTROL FLAG FOR DIAGNOSTIC PRINT OUT          !
!
!      LATSOZC,LEVOZC,BLATC,DPHIOZC,TIMEOZC: OZONE PARAMETERS FOR NMMB  ! !new
!      TAUCLOUDS(LM)   : CLOUD OPTICAL DEPTH FROM NMMB (ferrier+bmj)    ! !new
!      CLDF(LM)        : CLOUD FRACTION FROM NMMB (ferrier+bmj)         ! !new
!                                                                       !
!    OUTPUT VARIABLES:                                                  !
!      SWH (LM)       : TOTAL SKY SW HEATING RATE IN K/SEC              !
!      SFCNSW(1)      : TOTAL SKY SURFACE NET SW FLUX IN W/M**2         !
!      SFCDSW(1)      : TOTAL SKY SURFACE DOWNWARD SW FLUX IN W/M**2    !
!      SFALB (1)      : MEAN SURFACE DIFFUSED ALBEDO                    !
!      HLW (LM)       : TOTAL SKY LW HEATING RATE IN K/SEC              !
!      SFCDLW(1)      : TOTAL SKY SURFACE DOWNWARD LW FLUX IN W/M**2    !
!      TSFLW (1)      : SURFACE AIR TEMP DURING LW CALCULATION IN K     !
!
!      TOAUSW (IM)    : TOTAL SKY TOA UPWARD SW FLUX IN W/M**2         ! !new
!      TOADSW (IM)    : TOTAL SKY TOA DOWNWARD SW FLUX IN W/M**2       ! !new
!      SFCCDSW(IM)    : CLEAR SKY SURFACE SW DOWNWARD FLUX IN W/M**2   ! !new
!      TOAULW (IM)    : TOTAL SKY TOA LW FLUX W/M**2                   ! !new
!      SFCUSW (IM)    : TOTAL SKY SURFACE SW UPWARD FLUX IN W/M**2     ! !new
!                                                                       !
!    INPUT AND OUTPUT VARIABLES:                                        !
!      FLUXR_V (IX,NFXR) : TO SAVE 2-D FIELDS                           !
!                          (bucket)                                     !
!      CLDSA_V(IX,5)     : TO SAVE 2-D CLOUD FRACTION. L/M/H/TOT/BL     !
!                          (instantaneous)                              !
!      CLDCOV_V(IX,LM)   : TO SAVE 3-D CLOUD FRACTION                   !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!SELECT OPTIONS IN GRRAD
!
       NP3D=3         ! 3: ferrier's microphysics cloud scheme (only stratiform cloud)
                      !    (set iflagliq>0 in radsw_param.f and radlw_param.f)
                      ! 4: zhao/carr/sundqvist microphysics cloud (now available in the NMMB)
                      ! 5: NAM stratiform + convective cloud optical depth and fraction 
                      !    (set iflagliq=0 in radsw_param.f and radlw_param.f)
       ISOL=0         ! 0: use a fixed solar constant value (default)
                      ! 1: use 11-year cycle solar constant table
       ICO2=1         ! 0: use prescribed global mean co2   (default)
                      ! 1: use observed co2 annual mean value only
                      ! 2: use obs co2 monthly data with 2-d variation
       ICWP=1         ! control flag for cloud generation schemes
                      !  0: use diagnostic cloud scheme
                      ! -1: use diagnostic cloud scheme (use with NMMB for NP3D=5)(GFDL type)
                      !  1: use prognostic cloud scheme (use with NMMB for NP3D=3)
       IALB=2         ! control flag for surface albedo schemes
                      ! 0: climatology, based on surface veg types  ! ONLY THIS ONE WORKS (GFS)
                      ! 1: modis retrieval based surface albedo scheme
                      ! 2: use externally provided albedoes directly. ! ONLY THIS ONE WORKS for regional
                      !    (CALCULATES ALBEDO FROM NMMB MONTHLY CLIMATOLOGY AS IN GFDL RADIATION)
       IEMS=0         ! control flag for surface emissivity schemes 
                      ! 0: fixed value of 1.0   (default)
                      ! 1: varying value based on surface veg types
       IAER= 11       ! flag for aerosols scheme selection (all options work for NMMB)
                      ! - 3-digit aerosol flag (volc,lw,sw)     
                      !    =  0: turn all aeros effects off (sw,lw,volc)                   !
                      !    =  1: use clim tropspheric aerosol for sw only                  !
                      !    = 10: use clim tropspheric aerosol for lw only                  !
                      !    = 11: use clim tropspheric aerosol for both sw and lw           !
                      !    =100: volc aerosol only for both sw and lw                      !
                      !    =101: volc and clim trops aerosol for sw only                   !
                      !    =110: volc and clim trops aerosol for lw only                   !
                      !    =111: volc and clim trops aerosol for both sw and lw            !
                      !    =  2: gocart/BSC-Dust tropspheric aerosol for sw only           !
                      !    = 20: gocart/BSC-Dust tropspheric aerosol for lw only           !
                      !    = 22: gocart/BSC-Dust tropspheric aerosol for both sw and lw    !
                      !    =102: volc and gocart trops aerosol for sw only                 !
                      !    =120: volc and gocart trops aerosol for lw only                 !
                      !    =122: volc and gocart trops aerosol for both sw and lw          !
       FHAER=0.       ! = 0 aerosol determined from gocart clim 
                      !     ** Only this option works since on-line aerosol is not available in this version
                      ! > 0. and <99999. aerosol determined from fcst and gocart clim
                      ! >99999. aerosol determined from fcst


!
       NFXR=NFLUXR    ! second dimension of input/output array fluxr (FLUXR_V)
       NTRAC=3        ! dimension veriable for array oz
       LSSAV=.TRUE.   ! logical flag for store 3-d cloud field
                      !  ** need to be .TRUE. for non-zero FLUXR_V & CLDCOV_V off GRRAD
       LDIAG3D=.FALSE.! logical flag for store 3-d diagnostic fields
       LPRNT=.FALSE.
       SASHAL=.FALSE. ! New Massflux based shallow convection           ! Not in use for NMMB
       NTOZ=0         !  =0 climatological ozone profile                !
                      !  >0 interactive ozone profile                   !
       NCLDX=1        !  : only used when ntcw .gt. 0
       NTCW=3         !  =0 no cloud condensate calculated              !
                      !  >0 array index location for cloud condensate   !
       IOVR_SW=1      ! 0 sw: random overlap clouds
                      ! 1 sw: max-random overlap clouds
       IOVR_LW=1      ! 0 lw: random overlap clouds
                      ! 1 lw: max-random overlap clouds
       ICTM=1         ! ictm=0 => use data at initial cond time, if not
                      !     available, use latest, no extrapolation.
                      ! ictm=1 => use data at the forecast time, if not
                      !     available, use latest and extrapolation.
                      ! ictm=yyyy0 => use yyyy data for the forecast time,
                      !     no further data extrapolation.
                      ! ictm=yyyy1 = > use yyyy data for the fcst.
                      !     if needed, do extrapolation to match the fcst time.
                      ! ictm=-1 => use user provided external data for
                      !     the fcst time, no extrapolation.
                      ! ictm=-2 => same as ictm=0, but add seasonal cycle
                      !     from climatology. no extrapolation.

                      ! isubcsw/isubclw 
                      !    sub-column cloud approx control flag (sw/lw rad)
       ISUBCSW=0      ! 0: with out sub-column cloud approximation
                      ! 1: mcica sub-col approx. prescribed random seed
                      ! 2: mcica sub-col approx. provided random seed
       ISUBCLW=ISUBCSW

       ICSDSW(:)=0    ! auxiliary special cloud related array for SW
                      ! *** not used in this version of code ***
                      ! can be any value at this moment
       ICSDLW(:)=0    ! auxiliary special cloud related array for LW
                      ! *** not used in this version of code ***
                      ! can be any value at this moment

       norad_precip=.FALSE.  ! flag for precip in radiation
                             ! .true. snow/rain has no impact on radiation
       crick_proof=.FALSE.    ! flag for eliminating CRICK (smooths profiles)
       ccnorm=.TRUE.         ! flag for incloud condensate mixing ratio

!
!=========================================================================
!
       IF (ICWP/=-1 .AND. CNCLD) THEN
          CNCLD=.FALSE.        !-- Not used when ICWP=1, 0
       ENDIF

       FHSWR=FLOAT(NRADS*DT_INT)/3600.   ! [h]
       FHLWR=FLOAT(NRADL*DT_INT)/3600.   ! [h]
       DTLW =FLOAT(NRADL*DT_INT)         ! [s]
       DTSW =FLOAT(NRADS*DT_INT)         ! [s]
       LSSWR=MOD(NTIMESTEP,NRADS)==0
       LSLWR=MOD(NTIMESTEP,NRADL)==0
!
! --- TEMPORARY SETTING FOR THE "RADDT & FDAER" FOR THE GOCART VALUE
!     NEED TO MODIFY THIS SECTION WHEN USE THE DATA
!
       raddt = min(dtsw, dtlw)
       FDAER = 0.
!
!INIT RADIATION
!
       DO L=1,LM+1
        RSGM(L)=SGM(L)
       ENDDO

      ! CALL n_RADINIT (RSGM, LM, IFLIP, NP3D, ISOL, ICO2,     &
      !                ICWP, IALB, IEMS, IAER, JDAT, MYPE,     &
                    ! =================================
                    !  THE FOLLOWINGS ARE DUMMY INPUTS
                    ! =================================
      !                FHSWR, FHLWR, FHAER )

      !---- for forcast purpose IDAT=JDAT
      !
      ! CALL RADINIT (RSGM, LM, IFLIP, IDAT, JDAT, ICTM, ISOL, ICO2,   &
       CALL RADINIT (RSGM, LM, IFLIP, JDAT, JDAT, ICTM, ISOL, ICO2,   &
                     IAER, IALB, IEMS, ICWP, NP3D, ISUBCSW, ISUBCLW,  &
                     IOVR_SW, IOVR_LW, MYPE, raddt, fdaer )
!
!==========================================================================
!-- Begin 20120703 modifications (BSF)
!-----
! Calculate quantities within local tiles, denoted by variables with '_t' 
! appended to their names (Hsin-Mu Lin, BSF)
!==========================================================================
!
       NUMX=ITE-ITS+1    !-- # of zonal (x-direction) grid points
       NUMY=JTE-JTS+1    !-- # of meridional (y-direction) grid points
       DO J=JTS,JTE
          J_t=J-JTS+1
          DO I=ITS,ITE
             I_t=I-ITS+1
             SINLAT_t(I_t,J_t)=SIN( GLAT(I,J) )
             COSLAT_t(I_t,J_t)=COS( GLAT(I,J) ) 
             XLON_t(I_t,J_t)=GLON(I,J)
          ENDDO
       ENDDO

       CALL ASTRONOMY_NMMB                                              &
!  --- inputs:
           ( SINLAT_t, COSLAT_t, XLON_t,                                &
             FHSWR, JDAT, NRADS,                                        &
             NUMX, NUMY, LSSWR, MYPE,                                   &
!  --- outputs:
             SOLCON, SLAG, SDEC, CDEC, COSZEN_t, COSZDG_t               &
            )

!==========================================================================
!-- End 20120703 modifications (BSF)
!==========================================================================

! ----
!
!OZONE CLIMATOLOGY
!from gfs_physics_initialize_mod.f (module_PHYSICS_GRID_COMP.F90)
!
! there is no header in global_o3clim.txt file

        IF (NTOZ .LE. 0) THEN      ! DIAGNOSTIC OZONE, ONLY THIS ONE WORKS
           LEVOZC  = 17
           LATSOZC = 18
           BLATC   = -85.0
           TIMEOZC = 12            !!!  this is not in header
           LATSOZP   = 2
           LEVOZP    = 1
           TIMEOZ    = 1
           PL_COEFF  = 0
        ENDIF

        DPHIOZC = -(BLATC+BLATC)/(LATSOZC-1)
!
!from gloobr.f
!
       IF (NTOZ .LE. 0) THEN      ! CLIMATOLOGICAL OZONE!
!
         IDAY   = JDAT(3)
         IMON   = JDAT(2)
         MIDMON = DAYS(IMON)/2 + 1
         CHANGE = FIRST .OR.( (IDAY .EQ. MIDMON) .AND. (JDAT(5).EQ.0) )
!
         IF (CHANGE) THEN
             IF (IDAY .LT. MIDMON) THEN
                 K1OZ = MOD(IMON+10,12) + 1
                 MIDM = DAYS(K1OZ)/2 + 1
                 K2OZ = IMON
                 MIDP = DAYS(K1OZ) + MIDMON
              ELSE
                 K1OZ = IMON
                 MIDM = MIDMON
                 K2OZ = MOD(IMON,12) + 1
                 MIDP = DAYS(K2OZ)/2 + 1 + DAYS(K1OZ)
              ENDIF
          ENDIF
!
          IF (IDAY .LT. MIDMON) THEN
             ID = IDAY + DAYS(K1OZ)
          ELSE
             ID = IDAY
          ENDIF
          FACOZ = REAL (ID-MIDM) / REAL (MIDP-MIDM)
!
       ENDIF
!
!CLOUDS
!
!----------------------CONVECTION--------------------------------------
!  NRADPP IS THE NUMBER OF TIME STEPS TO ACCUMULATE CONVECTIVE PRECIP
!     FOR RADIATION
!   NOTE: THIS WILL NOT WORK IF NRADS AND NRADL ARE DIFFERENT UNLESS
!         THEY ARE INTEGER MULTIPLES OF EACH OTHER
!  CLSTP IS THE NUMBER OF HOURS OF THE ACCUMULATION PERIOD
!
      NTSPH=NINT(3600./FLOAT(DT_INT))
      NRADPP=MIN(NRADS,NRADL)
      CLSTP=1.0*NRADPP/NTSPH
      CONVPRATE=CUPRATE/CLSTP

      IF (ICWP>0 .AND. CUCLD) CONVPRATE=1000./CLSTP    !-- convert to mm/h
!
      LM1=LM-1
!
      DO J=JTS,JTE
      DO I=ITS,ITE
!
        P8W(I,J,1)=PT
!
        DO K=1,LM
          P8W(I,J,K+1)=P8W(I,J,K)+PDSG1(K)+DSG2(K)*PD(I,J)
          P_PHY(I,J,K)=SGML2(K)*PD(I,J)+PSGML1(K)
          CCMID(I,J,K)=0.
          CSMID(I,J,K)=0.
        ENDDO
      ENDDO
      ENDDO
!
      DO K=1,LM
      DO J=JTS,JTE
      DO I=ITS,ITE
        CLDFRA(I,J,K)=0.
        TAUTOTAL(I,J,K)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
          CFRACH(I,J)=0.
          CFRACL(I,J)=0.
          CFRACM(I,J)=0.
          CZMEAN(I,J)=0.
          SIGT4(I,J)=0.
      ENDDO
      ENDDO
!
      DO K=1,3
      DO J=JTS,JTE
      DO I=ITS,ITE
        CLDCFR(I,J,K)=0.
        MTOP(I,J,K)=0
        MBOT(I,J,K)=0
      ENDDO
      ENDDO
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
       CUTOP(I,J)=LM+1-HTOP(I,J)
       CUBOT(I,J)=LM+1-HBOT(I,J)
      ENDDO
      ENDDO
!      
!-----------------------------------------------------------------------
!---  COMPUTE GRID-SCALE CLOUD COVER FOR RADIATION  (Ferrier, Nov '04)
!
!--- Assumes Gaussian-distributed probability density functions (PDFs) for
!    total relative humidity (RHtot) within the grid for convective and
!    grid-scale cloud processes.  The standard deviation of RHtot is assumed
!    to be larger for convective clouds than grid-scale (stratiform) clouds.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
      ICWP_Test: IF (ICWP==-1) THEN   !-- *** Start of old NAM/GFDL cloud inputs ***
!-----------------------------------------------------------------------
       DO J=JTS,JTE
       DO I=ITS,ITE 
!
        DO 255 L=1,LM
!
            WV=MAX(EPSQ,Q(I,J,L))/(1.-MAX(EPSQ,Q(I,J,L)))   !-- Water vapor mixing ratio
            QICE=MAX(WATER(I,J,L,P_QS),0.)                  !-- Ice mixing ratio
            QCLD=QICE+MAX(WATER(I,J,L,P_QC),0.)             !-- Total cloud water + ice mixing ratio
!rv------------------------------------
!rv   This should be temporary fix!!!!!
!rv   New (currently operational) calculation of cloud fraction is
!rv   causing different results with different decomposition
!rv   We should find cause of this!!!!!
!rv------------------------------------
          OPER_flag: IF (OPER) THEN
!rv------------------------------------
!-- From model tuning experiments vs CLAVR grid-to-grid verification:
!-- 100% cloud fractions at 0.01 g/kg (1.e-5 kg/kg) cloud mixing ratios
!-- 10% cloud fractions at 1.e-4 g/kg (1.e-7 kg/kg) cloud mixing ratios
!-- 1% cloud fractions at 1.e-6 g/kg (1.e-9 kg/kg) cloud mixing ratios
!
            CLFR=MIN(H1, MAX(H0,1.e5*QCLD))
            CLFR=SQRT(CLFR)
            IF (CLFR>=CLFRmin) CSMID(I,J,L)=CLFR
!rv------------------------------------
          else OPER_flag
!rv------------------------------------

!
            IF (QCLD .LE. EPSQ) GO TO 255                               !--- Skip if no condensate is present
            CLFR=H0
!
            WV=MAX(EPSQ,Q(I,J,L))/(1.-MAX(EPSQ,Q(I,J,L)))
!
!--- Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
!
            ESAT=1000.*FPVS(T(I,J,L))                                   !--- Saturation vapor pressure (Pa)
            QSAT=EP_2*ESAT/(P_PHY(I,J,L)-ESAT)                          !--- Saturation mixing ratio
!
            RHUM=WV/QSAT                                                !--- Relative humidity
!
!--- Revised cloud cover parameterization (temporarily ignore rain)
!
            RHtot=(WV+QCLD)/QSAT                                        !--- Total relative humidity
!
            LCNVT=NINT(CUTOP(I,J))
            LCNVT=MIN(LM,LCNVT)
            LCNVB=NINT(CUBOT(I,J))
            LCNVB=MIN(LM,LCNVB)
            IF (L.GE.LCNVT .AND. L.LE.LCNVB) THEN
               SDM=CVSDM
            ELSE
               SDM=STSDM
            ENDIF
            ARG=(RHtot-RHgrd)/SDM
            IF (ARG.LE.DXSD2 .AND. ARG.GE.DXSD2N) THEN
               CLFR=HALF
            ELSE IF (ARG .GT. DXSD2) THEN
               IF (ARG .GE. XSDmax) THEN
                  CLFR=H1
               ELSE
                  IXSD=INT(ARG/DXSD+HALF)
                  IXSD=MIN(NXSD, MAX(IXSD,1))
                  CLFR=HALF+AXSD(IXSD)
               ENDIF              !--- End IF (ARG .GE. XSDmax)
            ELSE
               IF (ARG .LE. XSDmin) THEN
                  CLFR=H0
               ELSE
                  IXSD=INT(ARG/DXSD1+HALF)
                  IXSD=MIN(NXSD, MAX(IXSD,1))
                  CLFR=HALF-AXSD(IXSD)
                  IF (CLFR .LT. CLFRmin) CLFR=H0
               ENDIF        !--- End IF (ARG .LE. XSDmin)
            ENDIF           !--- IF (ARG.LE.DXSD2 .AND. ARG.GE.DXSD2N)
            CSMID(I,J,L)=CLFR
!rv------------------------------------
          endif  OPER_flag
!rv------------------------------------
!
255       CONTINUE         !--- End DO L=1,LM

       ENDDO ! End DO I=ITS,ITE
       ENDDO ! End DO J=JTS,JTE

!***********************************************************************
!******************  END OF GRID-SCALE CLOUD FRACTIONS  ****************
!
!---  COMPUTE CONVECTIVE CLOUD COVER FOR RADIATION
!
!--- The parameterization of Slingo (1987, QJRMS, Table 1, p. 904) is
!    used for convective cloud fraction as a function of precipitation
!    rate.  Cloud fractions have been increased by 20% for each rainrate
!    interval so that shallow, nonprecipitating convection is ascribed a
!    constant cloud fraction of 0.1  (Ferrier, Feb '02).
!***********************************************************************
!
      GFDL_Conv: IF (CNCLD) THEN

       DO J=JTS,JTE
        DO I=ITS,ITE
!
!***  CLOUD TOPS AND BOTTOMS COME FROM CUCNVC
!     Convective clouds need to be at least 2 model layers thick
!
          IF (CUBOT(I,J)-CUTOP(I,J) .GT. 1.0) THEN
!--- Compute convective cloud fractions if appropriate  (Ferrier, Feb '02)
            CLFR=CC(1)
            PMOD=CUPPT(I,J)*CONVPRATE
            IF (PMOD .GT. PPT(1)) THEN
              DO NC=1,10
                IF(PMOD.GT.PPT(NC)) NMOD=NC
              ENDDO
              IF (NMOD .GE. 10) THEN
                CLFR=CC(10)
              ELSE
                CC1=CC(NMOD)
                CC2=CC(NMOD+1)
                P1=PPT(NMOD)
                P2=PPT(NMOD+1)
                CLFR=CC1+(CC2-CC1)*(PMOD-P1)/(P2-P1)
              ENDIF      !--- End IF (NMOD .GE. 10) ...
              CLFR=MIN(H1, CLFR)
            ENDIF        !--- End IF (PMOD .GT. PPT(1)) ...
!
!***  ADD LVL TO BE CONSISTENT WITH OTHER WORKING ARRAYS
!
            LCNVT=NINT(CUTOP(I,J))
            LCNVT=MIN(LM,LCNVT)
            LCNVB=NINT(CUBOT(I,J))
            LCNVB=MIN(LM,LCNVB)
!
!--- Build in small amounts of subgrid-scale convective condensate
!    (simple assumptions), but only if the convective cloud fraction
!    exceeds that of the grid-scale cloud fraction
!
            DO L=LCNVT,LCNVB
              ARG=MAX(H0, H1-CSMID(I,J,L))
              CCMID(I,J,L)=MIN(ARG,CLFR)
            ENDDO           !--- End DO LL=LCNVT,LCNVB
          ENDIF             !--- IF (CUBOT(I,J)-CUTOP(I,J) .GT. 1.0) ...
        ENDDO               ! End DO I=ITS,ITE
       ENDDO                ! End DO J=JTS,JTE
      ENDIF  GFDL_Conv      !--- End IF (CNCLD) ...
!
!*********************************************************************
!***************  END OF CONVECTIVE CLOUD FRACTIONS  *****************
!*********************************************************************
!***
!*** INITIALIZE ARRAYS FOR USES LATER
!***

      DO I=ITS,ITE
      DO J=JTS,JTE
!
      LML=LM
!***
!*** NOTE: LAYER=1 IS THE SURFACE, AND LAYER=2 IS THE FIRST CLOUD
!***       LAYER ABOVE THE SURFACE AND SO ON.
!***
      KTOP(I,J,1)=LM+1
      KBTM(I,J,1)=LM+1
      CAMT(I,J,1)=1.0
      KCLD(I,J)=2
!
      DO 510 L=2,LM+1
      CAMT(I,J,L)=0.0
      KTOP(I,J,L)=1
      KBTM(I,J,L)=1
  510 CONTINUE
!### End changes so far
!***
!*** NOW CALCULATE THE AMOUNT, TOP, BOTTOM AND TYPE OF EACH CLOUD LAYER
!*** CLOUD TYPE=1: STRATIFORM CLOUD
!***       TYPE=2: CONVECTIVE CLOUD
!*** WHEN BOTH CONVECTIVE AND STRATIFORM CLOUDS EXIST AT THE SAME POINT,
!*** SELECT CONVECTIVE CLOUD WITH THE HIGHER CLOUD FRACTION.
!*** CLOUD LAYERS ARE SEPARATED BY TOTAL ABSENCE OF CLOUDINESS.
!*** NOTE: THERE IS ONLY ONE CONVECTIVE CLOUD LAYER IN ONE COLUMN.
!*** KTOP AND KBTM ARE THE TOP AND BOTTOM OF EACH CLOUD LAYER IN TERMS
!*** OF MODEL LEVEL.
!***
      NEW_CLOUD=.TRUE.
!
      DO L=2,LML
        LL=LML-L+1                                  !-- Model layer
        CLFR=MAX(CCMID(I,J,LL),CSMID(I,J,LL))       !-- Cloud fraction in layer
        CLFR1=MAX(CCMID(I,J,LL+1),CSMID(I,J,LL+1))  !-- Cloud fraction in lower layer
!-------------------
        IF (CLFR .GE. CLFRMIN) THEN
!--- Cloud present at level
          IF (NEW_CLOUD) THEN
!--- New cloud layer
            IF(L==2.AND.CLFR1>=CLFRmin)THEN
              KBTM(I,J,KCLD(I,J))=LL+1
              CAMT(I,J,KCLD(I,J))=CLFR1
            ELSE
              KBTM(I,J,KCLD(I,J))=LL
              CAMT(I,J,KCLD(I,J))=CLFR
            ENDIF
            NEW_CLOUD=.FALSE.
          ELSE
!--- Existing cloud layer
            CAMT(I,J,KCLD(I,J))=AMAX1(CAMT(I,J,KCLD(I,J)), CLFR)
          ENDIF        ! End IF (NEW_CLOUD .EQ. 0) ...
        ELSE IF (CLFR1 .GE. CLFRMIN) THEN
!--- Cloud is not present at level but did exist at lower level, then ...
          IF (L .EQ. 2) THEN
!--- For the case of ground fog
           KBTM(I,J,KCLD(I,J))=LL+1
           CAMT(I,J,KCLD(I,J))=CLFR1
          ENDIF
          KTOP(I,J,KCLD(I,J))=LL+1
          NEW_CLOUD=.TRUE.
          KCLD(I,J)=KCLD(I,J)+1
          CAMT(I,J,KCLD(I,J))=0.0
        ENDIF
!-------------------
      ENDDO      !--- End DO L loop
!***
!*** THE REAL NUMBER OF CLOUD LAYERS IS (THE FIRST IS THE GROUND;
!*** THE LAST IS THE SKY):
!***
      NCLDS(I,J)=KCLD(I,J)-2
      NCLD=NCLDS(I,J)
!***
!***  NOW CALCULATE CLOUD RADIATIVE PROPERTIES
!***
      IF(NCLD.GE.1)THEN
!***
!*** NOTE: THE FOLLOWING CALCULATIONS, THE UNIT FOR PRESSURE IS MB!!!
!***
        DO NC=2,NCLD+1
!
        TauC=0.    !--- Total optical depth for each cloud layer (solar & longwave)
        QSUM=0.0
        NKTP=LM+1
        NBTM=0
        BITX=CAMT(I,J,NC).GE.CLFRMIN
        NKTP=MIN(NKTP,KTOP(I,J,NC))
        NBTM=MAX(NBTM,KBTM(I,J,NC))
!
        DO LL=NKTP,NBTM
          L=NBTM-LL+NKTP 
          IF(LL.GE.KTOP(I,J,NC).AND.LL.LE.KBTM(I,J,NC).AND.BITX)THEN
            PRS1=P8W(I,J,L)*0.01 
            PRS2=P8W(I,J,L+1)*0.01
            DELP=PRS2-PRS1
            TCLD=T(I,J,L)-T0C 
            QSUM=QSUM+Q(I,J,L)*DELP*(PRS1+PRS2)      & 
     &           /(120.1612*SQRT(T(I,J,L)))
!
            CTau=0.
!-- For crude estimation of convective cloud optical depths
            IF (CCMID(I,J,L) .GE. CLFRmin) THEN
              IF (TCLD .GE. TRAD_ice) THEN
                CTau=CTauCW            !--- Convective cloud water
              ELSE
                CTau=CTauCI            !--- Convective ice
              ENDIF
            ENDIF
!
!-- For crude estimation of grid-scale cloud optical depths
!
!--   => The following 2 lines were intended to reduce cloud optical depths further
!        than what's parameterized in the NAM and what's theoretically justified
            CTau=CTau+ABSCOEF_W*WATER(I,J,L,P_QC)+ABSCOEF_I*WATER(I,J,L,P_QS)

            TAUTOTAL(I,J,L)=CTau*DELP                          !Total model level cloud optical depth
            CLDFRA(I,J,L)=MAX(CCMID(I,J,LL),CSMID(I,J,LL))     !Cloud fraction at model level           
            TauC=TauC+DELP*CTau                                !Total cloud optical depth as in GFDL
!
          ENDIF      !--- End IF(LL.GE.KTOP(I,NC) ....
        ENDDO        !--- End DO LL
!
      ENDDO
!
      ENDIF       ! NCLD.GE.1
!
      ENDDO  !  DO I=ITS,ITE
      ENDDO  !  DO J=JTS,JTE
!-----------------------------------------------------------------------
      ENDIF  ICWP_Test   !*** End of Old NAM/GFDL cloud inputs ***
!-----------------------------------------------------------------------
!
! Main domain loop: calling grrad
!
      FLGMIN_L(1)= 0.20d0 ! --- for ferrier

      CV (1)=0.d0         ! not in use 
      CVB(1)=0.d0         ! not in use
      CVT(1)=0.d0         ! not in use
!
      DO J=JTS,JTE  !start grrad loop column by column
         J_t=J-JTS+1
      DO I=ITS,ITE
         I_t=I-ITS+1

       CZMEAN(I,J)=COSZEN_t(I_t,J_t)        ! BSF
       XLAT(1)=GLAT(I,J)
       XLON(1)=GLON(I,J)
       TSEA(1)=TSKIN(I,J)
       TISFC(1)=TSKIN(I,J)                  ! change later if necessary
       ZORL(1)=Z0(I,J)*100.d0
       SNWDPH(1)=SI(I,J)                    ! snwdph[mm]
       SNCOVR(1)=SNOW(I,J)/(SNOW(I,J)+70.)  ! FORMULATION OF MARSHALL ET AL. 1994
       SNOALB(1)=MXSNAL(I,J)
       HPRIME_V(1)=STDH(I,J)

       IF(SICE(I,J).GT.0.5) THEN              ! slmsk - ocean  - 0
        SLMSK(1)= 2.0d0                       !         land   - 1
        FICE(1)=SICE(I,J)                     ! change this later
       ELSE                                   !         seaice - 2
        SLMSK(1)= 1.0d0-SM(I,J)               !
        FICE(1)= 0.0d0                        ! change this later
       ENDIF
!
!!!ALBEDOS
! 
       IF (IALB==2) THEN
          SFCALBEDO=ALBEDO(I,J)           ! BSF
!..... THE FOLLOWING CODE GETS ALBEDO FROM PAYNE,1972 TABLES IF
!         1) OPEN SEA POINT (SLMSK=1);2) KALB=0
            IQ=INT(TWENTY*HP537+ONE)
            IF(CZMEAN(I,J).GT.0.0 .AND. SM(I,J).GT.0.5) THEN
                ZEN=DEGRAD1*ACOS(MAX(CZMEAN(I,J),0.0))
                IF(ZEN.GE.H74E1) JX=INT(HAF*(HNINETY-ZEN)+ONE)
                IF(ZEN.LT.H74E1.AND.ZEN.GE.FIFTY) &
                JX=INT(QUARTR*(H74E1-ZEN)+HNINE)
             IF(ZEN.LT.FIFTY) JX=INT(HP1*(FIFTY-ZEN)+H15E1)
             DZEN=-(ZEN-ZA(JX))/DZA(JX)
             ALB1=ALBD(IQ,JX)+DZEN*(ALBD(IQ,JX+1)-ALBD(IQ,JX))
             ALB2=ALBD(IQ+1,JX)+DZEN*(ALBD(IQ+1,JX+1)-ALBD(IQ+1,JX))
             SFCALBEDO=ALB1+TWENTY*(ALB2-ALB1)*(HP537-TRN(IQ))    ! BSF
            ENDIF
!.....     VISIBLE AND NEAR IR DIFFUSE ALBEDO
             ALVD(1) = SFCALBEDO    ! BSF
             ALND(1) = SFCALBEDO    ! BSF
!.....     VISIBLE AND NEAR IR DIRECT BEAM ALBEDO
             ALVB(1) = SFCALBEDO    ! BSF
             ALNB(1) = SFCALBEDO    ! BSF
!
!--- Remove diurnal variation of land surface albedos (Ferrier, 6/28/05)
!--- Turn back on to mimic NAM 8/17/05
!
!.....     VISIBLE AND NEAR IR DIRECT BEAM ALBEDO, IF NOT OCEAN NOR SNOW
!        ..FUNCTION OF COSINE SOLAR ZENITH ANGLE..
!
!=== The following line commented out by the "fixed" are used instead of the
!    original version (09/2012)
!
!fixed            IF (SM(I,J).LT.0.5) THEN
!fixed             IF (SFCALBEDO.LE.0.5) THEN
!fixed             ALBD0=-18.0 * (0.5 - ACOS(CZMEAN(I,J))/PI)
!fixed             ALBD0=EXP (ALBD0)
!fixed             ALVD1=(ALVD(1) - 0.054313) / 0.945687
!fixed             ALND1=(ALND(1) - 0.054313) / 0.945687
!fixed             ALVB(1)=ALVD1 + (1.0 - ALVD1) * ALBD0
!fixed             ALNB(1)=ALND1 + (1.0 - ALND1) * ALBD0
!fixed! !-- Put in an upper limit on beam albedos
!fixed             ALVB(1)=MIN(0.5,ALVB(1))
!fixed             ALNB(1)=MIN(0.5,ALNB(1))
!fixed            END IF
!fixed           END IF

!!! WE INTRODUCE HERE DIRECT AND DIFFUSE ALBEDO... FOR THIS OPTION, THERE IS A CHANGE IN GRRAD.f
            ALVSF1(1)=ALVB(1) !For this option ALVSF1 is direct visible albedo
            ALNSF1(1)=ALNB(1) !For this option ALNSF1 is direct nir albedo
            ALVWF1(1)=ALVD(1) !For this option ALVWF1 is diffuse visible albedo
            ALNWF1(1)=ALND(1) !For this option ALNWF1 is diffuse nir albedo
            FACSF1(1)=0.      !not used with this option
            FACWF1(1)=0.      !not used for this option
!
       ENDIF    !---- end of IALB=2  GFDL TYPE RADIATION 

!
!---
      PRSI(1)=P8W(I,J,1)/1000.                                  ! [kPa]
!
      DO L=1,LM
        PRSI(L+1)=P8W(I,J,L+1)/1000.                          ! (pressure on interface) [kPa]
        PRSL(L)=P_PHY(I,J,L)/1000.                            ! (pressure on mid-layer) [kPa] 
        PRSLK(L)=(PRSL(L)*0.01d0)**(R/CP)
        RTvR=1./(R*(Q(I,J,L)*0.608+1.-CW(I,J,L))*T(I,J,L))
        VVEL(L)=OMGALF(I,J,L)*1000.d0*PRSL(L)*RTvR            !not used
        GT(L)=T(I,J,L)
        GQ(L)=Q(I,J,L)
!
        if(ntoz.le.0) then
          gr1(1,l,1)=0.d0
        else
          gr1(1,l,1)=max(o3(i,j,l),epso3)
        endif
!
        GR1(1,L,2)=0.d0
        GR1(1,L,3)=CW(I,J,L)
        CLDCOV_V(L)=0.d0                !used for prognostic cloud
        F_ICEC(L)=F_ICE(I,J,L)
        F_RAINC(L)=F_RAIN(I,J,L)
        R_RIME(L)=F_RIMEF(I,J,L)
        TAUCLOUDS(L)=TAUTOTAL(I,J,L)    !CLOUD OPTICAL DEPTH (ICWP==-1)
        CLDF(L)=CLDFRA(I,J,L)           !CLOUD FRACTION (ICWP==-1)
      ENDDO
!
!-- Bogus in tiny amounts of shallow convection, but only if there are no
!   grid-scale clouds nor convective precipitation present.  Arrays CUTOP,
!   CUBOT are flipped with 1 at the top & LM at the surface (BSF, 7/18/2012)
!-- There are extra, nested IF statements to filter conditions as an extra
!   layer of caution.  
!
      CU_cloud=.FALSE.
      CU_Bogus1: IF (CUCLD) THEN
         LCNVT=MIN(LM, NINT(CUTOP(I,J)) )   !-- Convective cloud top
         LCNVB=MIN(LM, NINT(CUBOT(I,J)) )   !-- Convective cloud base
         CU_DEPTH=0.
         CU_Index: IF (LCNVB-LCNVT>1) THEN
            CU_DEPTH=1000.*(PRSL(LCNVB)-PRSL(LCNVT))   !- Pa
            CU_Deep: IF (CU_DEPTH>=CU_DEEP_MIN .AND. CU_DEPTH<=CU_DEEP_MAX) THEN
               QCLD=MAXVAL( GR1(1,1:LM,3) )       !-- Maximum condensate
               PMOD=CUPPT(I,J)*CONVPRATE
               CU_Clds: IF (QCLD<QWmax .AND. PMOD<=CUPPT_min) THEN
                  CU_cloud=.TRUE.
                  DO L=LCNVT,LCNVB
                     GR1(1,L,3)=GR1(1,L,3)+QW_Cu
                  ENDDO
               ENDIF CU_Clds
            ENDIF CU_Deep
         ENDIF CU_Index
      ENDIF CU_Bogus1
!
      DO NC=1,5
        CLDSA_V(NC)=0.d0                 !used for prognostic cloud
      ENDDO
      DO NC=1,NFLUXR
        FLUXR_V(NC)=0.d0                 !used for prognostic cloud
      ENDDO
!
!---
      CALL GRRAD_NMMB                                                        &
!  ---  INPUTS:
           ( PRSI,PRSL,PRSLK,GT,GQ,GR1,VVEL,SLMSK,                           &
             XLON,XLAT,TSEA,SNWDPH,SNCOVR,SNOALB,ZORL,HPRIME_V,              &
             ALVSF1,ALNSF1,ALVWF1,ALNWF1,FACSF1,FACWF1,FICE,TISFC,           &
             SOLCON,COSZEN_t(I_t,J_t),COSZDG_t(I_t,J_t),K1OZ,K2OZ,FACOZ,     &   !-- BSF
             CV,CVT,CVB,IOVR_SW, IOVR_LW, F_ICEC, F_RAINC, R_RIME, FLGMIN_L, &
             ICSDSW, ICSDLW,                                                 &
             NP3D,NTCW,NCLDX,NTOZ,NTRAC,NFXR,                                &
        !     DTLW,DTSW,LSSWR,LSLWR,LSSAV,LDIAG3D,SASHAL,                     &
             DTLW,DTSW,LSSWR,LSLWR,LSSAV,SASHAL,                             &
             norad_precip, crick_proof, ccnorm,                              &
             1, 1, LM, IFLIP, MYPE, LPRNT,                                   &
             ipt, kdt,                                                       &
!  ---  ADDITIONAL INPUTS:
        !     LATSOZC,LEVOZC,BLATC,DPHIOZC,TIMEOZC,                           &
             TAUCLOUDS,CLDF,                                                 &
!  ---  OUTPUTS:
        !     SWH,SFCNSW,SFCDSW,SFALB,                                        &
        !     HLW,SFCDLW,TSFLW,                                               &
             SWH,TOPFSW,SFCFSW,SFALB,                                        &
             HLW,TOPFLW,SFCFLW,TSFLW,SEMIS,CLDCOV_V,CLDSA_V,                 &
!  ---  ADDITIONAL OUTPUT
        !     TOAUSW,TOADSW,SFCCDSW,TOAULW,SFCUSW,                    &
!  ---  INPUT/OUTPUT:
             FLUXR_V                                                         &
           )
!
      DO L=1,LM
        RLWTT(I,J,L)=HLW(L)
        RSWTT(I,J,L)=SWH(L)
      ENDDO

!=================================================================
! For non GFDL type cloud (use cloud fields from outputs of GRRAD)
!=================================================================

      IF (ICWP /= -1) THEN
         IF ( LSSAV ) THEN
            DO L=1,LM
               CLDFRA(I,J,L)=CLDCOV_V(L)
               CSMID(I,J,L)=CLDCOV_V(L)
            ENDDO

            CFRACL(I,J)=CLDSA_V(1)
            CFRACM(I,J)=CLDSA_V(2)
            CFRACH(I,J)=CLDSA_V(3)
!
!@@@ To Do:  @@@
!@@@ Add CFRACT array to calculate the total cloud fraction, replace
!    the instantaneous cloud fraction in the post
!
            ACFRST(I,J)=ACFRST(I,J) + CLDSA_V(4)
            NCFRST(I,J)=NCFRST(I,J) + 1
!-- Added a time-averaged convective cloud fraction calculation
            IF (CU_cloud) ACFRCV(I,J)=ACFRCV(I,J)+CLDSA_V(4)
            NCFRCV(I,J)=NCFRCV(I,J)+1
         ELSE
            PRINT *, '*** CLDFRA=0, need to set LSSAV=TRUE'
            STOP
         ENDIF
      ENDIF
!

!=========================================================
! modify this section by using TOPFSW,SFCFSW,TOPFLW,SFCFLW
! instead of TOAUSW,TOADSW,SFCCDSW,TOAULW,SFCUSW
!=========================================================

      ! RLWIN(I,J)=SFCDLW(1)
      ! RSWIN(I,J)=SFCDSW(1)
      ! RSWINC(I,J)=SFCCDSW(1) 
      ! RSWOUT(I,J)=RSWIN(I,J)*SFALB(1)
      ! RLWTOA(I,J)=TOAULW(1)
      ! RSWTOA(I,J)=TOAUSW(1)

      ! RSWOUT(I,J)=RSWIN(I,J)*SFALB(1)

      RLWIN(I,J) =SFCFLW(1)%dnfxc
      RSWIN(I,J) =SFCFSW(1)%dnfxc
      RSWOUT(I,J)=SFCFSW(1)%upfxc
      RSWINC(I,J)=SFCFSW(1)%dnfx0

      RLWTOA(I,J)=TOPFLW(1)%upfxc
      RSWTOA(I,J)=TOPFSW(1)%upfxc

!================== END OF modification =================

!
      ENDDO     ! --- END I LOOP for grrad
      ENDDO     ! --- END J LOOP for grrad

!
!*** --------------------------------------------------------------------------
!***  DETERMINE THE FRACTIONAL CLOUD COVERAGE FOR HIGH, MID
!***  AND LOW OF CLOUDS FROM THE CLOUD COVERAGE AT EACH LEVEL
!***
!***  NOTE: THIS IS FOR DIAGNOSTICS ONLY!!!
!***
!***
!
!----------------------------------------------------------------------------
      ICWP_Test2: IF (ICWP==-1) THEN   !-- *** Start of old NAM/GFDL cloud ***
!----------------------------------------------------------------------------

       DO J=JTS,JTE
       DO I=ITS,ITE
!!
       DO L=0,LM
         CLDAMT(L)=0.
       ENDDO
!!
!!***  NOW GOES LOW, MIDDLE, HIGH
!!
       DO 480 NLVL=1,3
       CLDMAX=0.
       MALVL=LM
       LLTOP=LM+1-LTOP(NLVL)   !!!!COMES FROM GFDL INIT
!!***
!!***  GO TO THE NEXT CLOUD LAYER IF THE TOP OF THE CLOUD-TYPE IN
!!***  QUESTION IS BELOW GROUND OR IS IN THE LOWEST LAYER ABOVE GROUND.
!!***
       IF(LLTOP.GE.LM)GO TO 480
!!
       IF(NLVL.GT.1)THEN
         LLBOT=LM+1-LTOP(NLVL-1)-1
         LLBOT=MIN(LLBOT,LM1)
       ELSE
         LLBOT=LM1
       ENDIF
!!
       DO 435 L=LLTOP,LLBOT
       CLDAMT(L)=AMAX1(CSMID(I,J,L),CCMID(I,J,L))
       IF(CLDAMT(L).GT.CLDMAX)THEN
         MALVL=L
         CLDMAX=CLDAMT(L)
       ENDIF
   435 CONTINUE
!!*********************************************************************
!! NOW, CALCULATE THE TOTAL CLOUD FRACTION IN THIS PRESSURE DOMAIN
!! USING THE METHOD DEVELOPED BY Y.H., K.A.C. AND A.K. (NOV., 1992).
!! IN THIS METHOD, IT IS ASSUMED THAT SEPERATED CLOUD LAYERS ARE
!! RADOMLY OVERLAPPED AND ADJACENT CLOUD LAYERS ARE MAXIMUM OVERLAPPED.
!! VERTICAL LOCATION OF EACH TYPE OF CLOUD IS DETERMINED BY THE THICKEST
!! CONTINUING CLOUD LAYERS IN THE DOMAIN.
!!*********************************************************************
       CL1=0.0
       CL2=0.0
       KBT1=LLBOT
       KBT2=LLBOT
       KTH1=0
       KTH2=0
!!
       DO 450 LL=LLTOP,LLBOT
       L=LLBOT-LL+LLTOP
       BIT1=.FALSE.
       CR1=CLDAMT(L)
       BITX=(P8W(I,J,L).GE.PTOPC(NLVL+1)).AND.                           &
      &     (P8W(I,J,L).LT.PTOPC(NLVL)).AND.                             &
      &     (CLDAMT(L).GT.0.0)
       BIT1=BIT1.OR.BITX
       IF(.NOT.BIT1)GO TO 450
!!***
!!***  BITY=T: FIRST CLOUD LAYER; BITZ=T:CONSECUTIVE CLOUD LAYER
!!***  NOTE:  WE ASSUME THAT THE THICKNESS OF EACH CLOUD LAYER IN THE
!!***         DOMAIN IS LESS THAN 200 MB TO AVOID TOO MUCH COOLING OR
!!***         HEATING. SO WE SET CTHK(NLVL)=200*E2. BUT THIS LIMIT MAY
!!***         WORK WELL FOR CONVECTIVE CLOUDS. MODIFICATION MAY BE
!!***         NEEDED IN THE FUTURE.
!!***
       BITY=BITX.AND.(KTH2.LE.0)
       BITZ=BITX.AND.(KTH2.GT.0)
!!
       IF(BITY)THEN
         KBT2=L
         KTH2=1
       ENDIF
!!
       IF(BITZ)THEN
         KTOP1=KBT2-KTH2+1
         DPCL=P_PHY(I,J,KBT2)-P_PHY(I,J,KTOP1)
         IF(DPCL.LT.CTHK(NLVL))THEN
           KTH2=KTH2+1
         ELSE
           KBT2=KBT2-1
         ENDIF
       ENDIF
       IF(BITX)CL2=AMAX1(CL2,CR1)
!!***
!!*** AT THE DOMAIN BOUNDARY OR SEPARATED CLD LAYERS, RANDOM OVERLAP.
!!*** CHOOSE THE THICKEST OR THE LARGEST FRACTION AMT AS THE CLD
!!*** LAYER IN THAT DOMAIN.
!!***
       BIT2=.FALSE.
       BITY=BITX.AND.(CLDAMT(L-1).LE.0.0.OR. &
            P8W(I,J,L-1).LT.PTOPC(NLVL+1))
       BITZ=BITY.AND.CL1.GT.0.0
       BITW=BITY.AND.CL1.LE.0.0
       BIT2=BIT2.OR.BITY
       IF(.NOT.BIT2)GO TO 450
!!
!!
       IF(BITZ)THEN
         KBT1=INT((CL1*KBT1+CL2*KBT2)/(CL1+CL2))
         KTH1=INT((CL1*KTH1+CL2*KTH2)/(CL1+CL2))+1
         CL1=CL1+CL2-CL1*CL2
       ENDIF
!!
       IF(BITW)THEN
         KBT1=KBT2
         KTH1=KTH2
         CL1=CL2
       ENDIF
!!
       IF(BITY)THEN
         KBT2=LLBOT
         KTH2=0
         CL2=0.0
       ENDIF
  450 CONTINUE
!
        CLDCFR(I,J,NLVL)=AMIN1(1.0,CL1)
        MTOP(I,J,NLVL)=MIN(KBT1,KBT1-KTH1+1)
        MBOT(I,J,NLVL)=KBT1

  480 CONTINUE

      ENDDO ! End DO I=ITS,ITE
      ENDDO ! End DO J=ITS,JTE

!!
      DO J=JTS,JTE
      DO I=ITS,ITE

        CFRACL(I,J)=CLDCFR(I,J,1)
        CFRACM(I,J)=CLDCFR(I,J,2)
        CFRACH(I,J)=CLDCFR(I,J,3)

        IF(CNCLD)THEN
          CFSmax=0.   !-- Maximum cloud fraction (stratiform component)
          CFCmax=0.   !-- Maximum cloud fraction (convective component)
          DO L=1,LM
            CFSmax=MAX(CFSmax, CSMID(I,J,L) )
            CFCmax=MAX(CFCmax, CCMID(I,J,L) )
          ENDDO
          ACFRST(I,J)=ACFRST(I,J)+CFSmax
          NCFRST(I,J)=NCFRST(I,J)+1
          ACFRCV(I,J)=ACFRCV(I,J)+CFCmax
          NCFRCV(I,J)=NCFRCV(I,J)+1
        ELSE
  !--- Count only locations with grid-scale cloudiness, ignore convective clouds
  !    (option not used, but if so set to the total cloud fraction)
          CFRAVG=1.-(1.-CFRACL(I,J))*(1.-CFRACM(I,J))*(1.-CFRACH(I,J))
          ACFRST(I,J)=ACFRST(I,J)+CFRAVG
          NCFRST(I,J)=NCFRST(I,J)+1
        ENDIF

      ENDDO  !  DO I=ITS,ITE
      ENDDO  !  DO J=JTS,JTE

!-----------------------------------------------------------------------
      ENDIF  ICWP_Test2   !*** End of Old NAM/GFDL cloud ***
!-----------------------------------------------------------------------

!
!-----------------------------------------------------------------------
!***  LONGWAVE
!-----------------------------------------------------------------------
!
      IF(MOD(NTIMESTEP,NRADL)==0)THEN
!.......................................................................
!$omp parallel do                                                       &
!$omp& private(i,j,k,kflip,tdum)
!.......................................................................
        DO J=JTS,JTE
          DO I=ITS,ITE
!
            TDUM=T(I,J,LM)
            SIGT4(I,J)=STBOLT*TDUM*TDUM*TDUM*TDUM
!
          ENDDO
        ENDDO
!.......................................................................
!$omp end parallel do
!.......................................................................
!
      ENDIF
!      
      IF (FIRST) FIRST=.FALSE.

!-----------------------------------------------------------------------
!
      END SUBROUTINE RRTM
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
      SUBROUTINE RRTM_INIT(EMISS,SFULL,SHALF,PPTOP,                     &
     &                     JULYR,MONTH,IDAY,GMT,                        &
     &                     CO2TF,                                       &
     &                     IDS, IDE, JDS, JDE, KDS, KDE,                &
     &                     IMS, IME, JMS, JME, KMS, KME,                &
     &                     ITS, ITE, JTS, JTE, KTS, KTE              )
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,INTENT(IN) :: JULYR,MONTH,IDAY,CO2TF
      REAL,INTENT(IN) :: GMT,PPTOP
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: SFULL, SHALF
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: EMISS
!
      INTEGER :: I,IHRST,J,N
      REAL :: PCLD,XSD,PI,SQR2PI
      REAL :: SSLP=1013.25
      REAL, PARAMETER :: PTOP_HI=150.,PTOP_MID=350.,PTOP_LO=642.,       &
     &                   PLBTM=105000.
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!***  INITIALIZE DIAGNOSTIC LOW,MIDDLE,HIGH CLOUD LAYER PRESSURE LIMITS.
!
      LTOP(1)=0
      LTOP(2)=0
      LTOP(3)=0
!
      DO N=1,KTE
        PCLD=(SSLP-PPTOP*10.)*SHALF(N)+PPTOP*10.
        IF(PCLD>=PTOP_LO)LTOP(1)=N
        IF(PCLD>=PTOP_MID)LTOP(2)=N
        IF(PCLD>=PTOP_HI)LTOP(3)=N
!       PRINT *,N,PCLD,SHALF(N),PSTAR,PPTOP
      ENDDO
!***
!***  ASSIGN THE PRESSURES FOR CLOUD DOMAIN BOUNDARIES
!***
      PTOPC(1)=PLBTM
      PTOPC(2)=PTOP_LO*100.
      PTOPC(3)=PTOP_MID*100.
      PTOPC(4)=PTOP_HI*100.
!
!***  FOR NOW, GFDL RADIATION ASSUMES EMISSIVITY = 1.0
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        EMISS(I,J) = 1.0
      ENDDO
      ENDDO
!
!---  Calculate the area under the Gaussian curve at the start of the
!---  model run and build the look up table AXSD
!
      PI=ACOS(-1.)
      SQR2PI=SQRT(2.*PI)
      RSQR=1./SQR2PI
      DO I=1,NXSD
        XSD=REAL(I)*DXSD
        AXSD(I)=GAUSIN(XSD)
      ENDDO
!
!-----------------------------------------------------------------------
      END SUBROUTINE RRTM_INIT
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      REAL FUNCTION GAUSIN(xsd)
      REAL, PARAMETER :: crit=1.e-3
      REAL A1,A2,RN,B1,B2,B3,SUM,xsd
!
!  This function calculate area under the Gaussian curve between mean
!  and xsd # of standard deviation (03/22/2004  Hsin-mu Lin)
!
      a1=xsd*RSQR
      a2=exp(-0.5*xsd**2)
      rn=1.
      b1=1.
      b2=1.
      b3=1.
      sum=1.
      do while (b2 .gt. crit)
         rn=rn+1.
         b2=xsd**2/(2.*rn-1.)
         b3=b1*b2
         sum=sum+b3
         b1=b3
      enddo
      GAUSIN=a1*a2*sum
      RETURN
      END FUNCTION GAUSIN
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      END MODULE MODULE_RA_RRTM
!
!-----------------------------------------------------------------------
