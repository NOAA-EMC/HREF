
#include "../../ESMFVersionDefine.h"

#if (ESMF_MAJOR_VERSION < 5 || ESMF_MINOR_VERSION < 2)
#undef ESMF_520r
#else
#define ESMF_520r
#endif

!-----------------------------------------------------------------------

      MODULE MODULE_SOLVER_INTERNAL_STATE

!-----------------------------------------------------------------------
!***  Declare the derived datatype called SOLVER_INTERNAL_STATE.
!***  For now the components of this datatype will include everything 
!***  needed to advance the model integration, i.e. everything that 
!***  would be part of a restart file.  Specifically this will include 
!***  those quantities that evolve during the integration, the namelist
!***  variables, and the grid decomposition variables.
!-----------------------------------------------------------------------
!
      USE ESMF_Mod
!
      USE module_INCLUDE
      USE module_CONTROL, ONLY: NUM_DOMAINS_MAX

      USE module_LS_NOAHLSM, ONLY: NUM_SOIL_LAYERS
      USE module_MICROPHYSICS_NMM, ONLY: MICRO_RESTART

      USE module_ERR_MSG, ONLY: ERR_MSG,MESSAGE_CHECK
      USE module_VARS
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: SOLVER_INTERNAL_STATE                                   &
               ,SET_INTERNAL_STATE_SOLVER                               &
               ,WRAP_SOLVER_INT_STATE 
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      INTEGER, PARAMETER :: MAX_VARS = 300
!
      TYPE SOLVER_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!***  Begin with the Configure File variables.
!-----------------------------------------------------------------------
!
        INTEGER(kind=KINT), POINTER :: IM,JM,LM
        INTEGER(kind=KINT) :: INPES,JNPES                               &
                             ,DFIHR_BOCO                                &
                             ,FILTER_METHOD                             &
                             ,FILTER_METHOD_LAST                        &
                             ,MINUTES_HISTORY                           &
                             ,MINUTES_RESTART                           &
                             ,MY_DOMAIN_ID                              &
                             ,NCOUNT                                    &
                             ,NHOURS_FCST                               &
                             ,NSTEPS_BC_RESTART                         &
                             ,NSTEPS_PER_CHECK                          &
                             ,NSTEPS_PER_HOUR                           &
                             ,NSTEPS_PER_RESET                          &
                             ,NUM_TRACERS_MET                           &  !<-- Number of meteorological tracers (e.g. water)
                             ,NUM_TRACERS_CHEM                          &  !<-- Number of chem/aerosol tracers
                             ,START_YEAR                                &
                             ,START_MONTH                               &
                             ,START_DAY                                 &
                             ,START_HOUR                                &
                             ,START_MINUTE                              &
                             ,START_SECOND
!
        REAL(kind=KFPT), POINTER :: DT,SBD,TSTART,TPH0D,TLM0D,WBD
        REAL(kind=KFPT) :: CLEFFAMP                                     &
                          ,CODAMP                                       &
                          ,DPMIN                                        &
                          ,FACTOP                                       &
                          ,RLOLEV                                       &
                          ,RUN_DURATION                                 &
                          ,SIGFAC                                       &       
                          ,SMAG2                                        &       
                          ,WCOR
!
        LOGICAL(kind=KLOG) :: ADIABATIC                                 &
                             ,ADVECT_TRACERS                            &
                             ,FIRST_NMM                                 &
                             ,FREERUN                                   &
                             ,GLOBAL                                    &
                             ,HYDRO                                     &
                             ,NEMSIO_INPUT                              &
                             ,OPER                                      &
                             ,OPERATIONAL_PHYSICS                       &
                             ,PRINT_ALL                                 &
                             ,PRINT_OUTPUT                              &
                             ,PRINT_DIAG                                &
                             ,PRINT_ESMF                                &
                             ,RESTART                                   &
                             ,SECADV                                    &
                             ,SPEC_ADV                                  &
                             ,USE_ALLREDUCE                             &
                             ,READ_GLOBAL_SUMS                          &
                             ,WRITE_GLOBAL_SUMS
!
#ifdef ESMF_3
        TYPE(ESMF_Logical) :: MY_DOMAIN_MOVES
#else
        LOGICAL(kind=KLOG) :: MY_DOMAIN_MOVES
#endif
!
!-----------------------------------------------------------------------
!***  Distributed memory information.
!-----------------------------------------------------------------------
!
        INTEGER(kind=KINT) :: IHALO,JHALO,MYPE,NHALO,NUM_PES            &
                             ,NUM_PTS_MAX                               &
                             ,WRITE_GROUPS,WRITE_TASKS_PER_GROUP        
!
        INTEGER(kind=KINT) :: ITS,ITE,JTS,JTE                           &
                             ,IMS,IME,JMS,JME                           &
                             ,IDS,IDE,JDS,JDE
!
        INTEGER(kind=KINT) :: ITE_B1,ITE_B2,ITE_B1_H1,ITE_B1_H2         &
                             ,ITE_H1,ITE_H2                             &                    
                             ,ITS_B1,ITS_B2,ITS_B1_H1,ITS_B1_H2         &
                             ,ITS_H1,ITS_H2                             &                    
                             ,JTE_B1,JTE_B2,JTE_B1_H1,JTE_B1_H2         &
                             ,JTE_H1,JTE_H2                             &                    
                             ,JTS_B1,JTS_B2,JTS_B1_H1,JTS_B1_H2         &
                             ,JTS_H1,JTS_H2
!
        INTEGER(kind=KINT) :: MPI_COMM_COMP
!
        INTEGER(kind=KINT),DIMENSION(1:8) :: MY_NEB
!
        INTEGER(kind=KINT),DIMENSION(:),POINTER :: LOCAL_ISTART         &
                                                  ,LOCAL_IEND           &
                                                  ,LOCAL_JSTART         &
                                                  ,LOCAL_JEND
!
        LOGICAL(kind=KLOG) :: E_BDY,N_BDY,S_BDY,W_BDY
!
!-----------------------------------------------------------------------
!***  Horizontal and vertical grid-related variables.
!-----------------------------------------------------------------------
!
        INTEGER(kind=KINT), POINTER :: LPT2
!
        INTEGER(kind=KINT),DIMENSION(:),POINTER :: KHFILT,KVFILT        &
                                                  ,NFFTRH,NFFTRW        &
                                                  ,NHSMUD
!
        REAL(kind=KFPT), POINTER :: DPHD,DLMD,DYH,PDTOP,PT
        REAL(kind=KFPT) :: DDMPV,DYV,EF4T                               &
                          ,GLAT_SW,GLON_SW                              &
                          ,RDYH,RDYV 
!
        REAL(kind=KFPT),DIMENSION(:),POINTER :: SG1,PSG1                &
                                               ,DSG1,PDSG1              &
                                               ,SG2,DSG2                &
                                               ,SGML1,PSGML1            &
                                               ,SGML2,SGM               &
                                               ,DDMPU,WPDAR             &
                                               ,FCP,FDIV                &
                                               ,CURV,DDV                &
                                               ,DARE,RARE               &
                                               ,FAD,FAH                 &
                                               ,DXH,RDXH                &
                                               ,DXV,RDXV                &
                                               ,RDDV                    &
                                               ,WFFTRH,WFFTRW
!
        REAL(kind=KFPT),DIMENSION(:,:),POINTER :: BARO,F,FIS            &
                                                 ,GLAT,GLON             &
                                                 ,HDACX,HDACY           &
                                                 ,HDACVX,HDACVY         &
                                                 ,HFILT,VFILT           &
                                                 ,VLAT,VLON
!
        REAL(kind=KFPT),DIMENSION(:,:),POINTER :: PD,PDO,PSDT,SICE,SM
!
!-----------------------------------------------------------------------
!***  Integration quantities.
!-----------------------------------------------------------------------
!
        LOGICAL(kind=KLOG) :: FIRST_STEP,READBC                         &
                             ,ADV_STANDARD,ADV_UPSTREAM
!
        INTEGER(kind=KINT), POINTER :: IHRST,MDRMINout,MDRMAXout        &
                                      ,MDIMINout,MDIMAXout
!
        INTEGER(kind=KINT) :: NTSD,IDTAD,IDTADT,IHR,IHREND              &
                             ,LNSAD,NBOCO,NTSTI,NTSTM,NTSTM_MAX
!
        INTEGER(kind=KINT),DIMENSION(:), POINTER :: IDAT
!
        REAL(kind=KFPT) :: DT_TEST_RATIO                                &
                          ,DT_LAST
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: PINT,RTOP           &
                                                   ,T,U,V               &
                                                   ,Q,CW,O3             &
                                                   ,Q2,E2               &
                                                   ,DIV                 &
                                                   ,DEF                 &
                                                   ,TDIV                &
                                                   ,W,W_TOT,Z           &
                                                   ,DWDT,PDWDT          &
                                                   ,OMGALF              &
                                                   ,PSGDT               &
                                                   ,TP,UP,VP            &
                                                   ,PCNE,PCNW           &
                                                   ,PFNE,PFNW           &
                                                   ,PCX,PCY             &
                                                   ,PFX,PFY             &
                                                   ,TCT,TCU,TCV         &
                                                   ,Told,Tadj
!
        LOGICAL(kind=KLOG) :: FIRST_PASS                                &
                             ,LMPRATE,RUN
!
!-----------------------------------------------------------------------
!***  The general 4-D arrays for 3-D "tracers".
!-----------------------------------------------------------------------
!
        INTEGER(kind=KINT) :: NUM_TRACERS_TOTAL                            !<-- Total number of "tracer" variables.  
        INTEGER(kind=KINT) :: D_SS                                         !<-- Total number of mp "source/sink" variables.
!
!-----------------------------------------------
!***  Declare indices of meteorological tracers
!-----------------------------------------------
!
        INTEGER(kind=KINT) :: INDX_Q                                    &  !<-- Location of Q in tracer arrays
                             ,INDX_CW                                   &  !<-- Location of CW in tracer arrays
                             ,INDX_O3                                   &  !<-- Location of O3 in tracer arrays
                             ,INDX_Q2                                      !<-- Location of Q2 in tracer arrays
!
        REAL(kind=KFPT),DIMENSION(:,:,:,:),POINTER :: TRACERS           &  !<-- Tracers variable
                                                     ,TRACERS_SQRT      &  !<-- Sqrt of the tracer variables (for advection)
                                                     ,TRACERS_PREV      &  !<-- Values of tracer variables in prev timestep (for advection)
                                                     ,TRACERS_TEND         !<-- Tendencies of tracer variables (for advection)
!
        REAL(kind=KFPT),DIMENSION(:),POINTER :: TRACERS_ARR             &  !<-- Storage array for "tracer" variables.
                                               ,TRACERS_PREV_ARR           !<-- Storage array for "Values of tracer variables in prev timestep"
!
!-----------------------------------------------------------------------
!***  Boundary conditions.
!-----------------------------------------------------------------------
!
        INTEGER(kind=KINT) :: IHRSTBC,LNSH,LNSV
!
        INTEGER(kind=KINT),DIMENSION(3) :: IDATBC
!
        REAL(kind=KFPT) :: TBOCO
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: PDBS,PDBN           &
                                                   ,PDBW,PDBE
!
        REAL(kind=KFPT),DIMENSION(:,:,:,:),POINTER :: TBS,TBN           & 
                                                     ,TBW,TBE           &
                                                     ,QBS,QBN           &
                                                     ,QBW,QBE           &
                                                     ,UBS,UBN           &
                                                     ,UBW,UBE           &
                                                     ,VBS,VBN           &
                                                     ,VBW,VBE           &
                                                     ,WBS,WBN           &
                                                     ,WBW,WBE
!
        LOGICAL(kind=KLOG) :: RUNBC
!
!----------------------------
!***  For 1-D restart output
!----------------------------
!
!
      INTEGER(kind=KINT) :: NUM_WORDS_BC_SOUTH                          &  !<-- Word counts of 1-D boundary data strings
                           ,NUM_WORDS_BC_NORTH                          &  !    for each side of the domain.
                           ,NUM_WORDS_BC_WEST                           &  !
                           ,NUM_WORDS_BC_EAST                              !<--
!
      REAL(kind=KFPT),DIMENSION(:),ALLOCATABLE :: RST_BC_DATA_SOUTH     &  !<-- 1-D strings of boundary data
                                                 ,RST_BC_DATA_NORTH     &  !    for each side of the domain.
                                                 ,RST_BC_DATA_WEST      &  !
                                                 ,RST_BC_DATA_EAST         !<--
!
!-----------------------------------------------------------------------
!***  FFT arrays.
!-----------------------------------------------------------------------
!
        REAL(kind=KDBL),DIMENSION(:),POINTER :: CRAUX1,CRAUX2,CRAUX3    &
                                               ,RCAUX1,RCAUX2,RCAUX3
!
!-----------------------------------------------------------------------
!***  Some physics variables are needed.
!-----------------------------------------------------------------------
!
        CHARACTER(99) :: SHORTWAVE                                      &
                        ,LONGWAVE                                       &   
                        ,LAND_SURFACE                                   &
                        ,SFC_LAYER                                      &
                        ,TURBULENCE                                     &
                        ,CONVECTION                                     &
                        ,MICROPHYSICS
!
        INTEGER(kind=KINT) :: NUM_WATER                                 &  !<-- 1 + types of water substance in microphysics
                             ,P_QV                                      &  !<-- Index for water vapor in WATER array
                             ,P_QC                                      &  !<-- Index for cloud water in WATER array
                             ,P_QR                                      &  !<-- Index for rain in WATER array
                             ,P_QI                                      &  !<-- Index for cloud ice in WATER array
                             ,P_QS                                      &  !<-- Index for snow in WATER array
                             ,P_QG                                         !<-- Index for graupel in WATER array
!
        INTEGER(kind=KINT) :: INDX_WATER_START                          &  !<-- Start index of the water in tracers array
                             ,INDX_WATER_END                               !<-- End index of the water in tracers array
!
        REAL(kind=KFPT),DIMENSION(:,:,:,:),POINTER :: MPRATES,WATER         !<-- Storage array for MP source/sink terms and water substance
!
        REAL(kind=KFPT),DIMENSION(:),POINTER :: MASSRout,MASSIout           !<-- Mass of rain and ice for different particle sizes-Fer/Ferhires
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: F_ICE,F_RAIN        &  !<-- Fractions of ice, rain, and rime
                                                   ,F_RIMEF
!
        LOGICAL :: F_QV,F_QC,F_QR,F_QI,F_QS,F_QG
!
!-----------------------------------------------------------------------
!***  Nesting
!-----------------------------------------------------------------------
!
        INTEGER(kind=KINT),POINTER :: I_PAR_STA                         &  !<-- SW corner of nest domain on this parent I
                                     ,J_PAR_STA                         &  !<-- SW corner of nest domain on this parent J
                                     ,LAST_STEP_MOVED                   &  !<-- Last time step this domain moved
                                     ,NMTS                                 !<-- Next Move TimeStep for a nest
!
        INTEGER(kind=KINT) :: PARENT_CHILD_TIME_RATIO                      !<-- # of child timesteps per parent timestep
!
        INTEGER(kind=KINT),DIMENSION(:),POINTER :: NTSCM                   !<-- Next TimeStep a Child nest Moves
!
#ifdef ESMF_3
        TYPE(ESMF_Logical) :: I_AM_A_NEST                                  !<-- Am I in a nested domain?
#else
        LOGICAL(kind=KLOG) :: I_AM_A_NEST                                  !<-- Am I in a nested domain?
#endif
!
!-----------------------------------------------------------------------
! FROM PHY start
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Begin with the namelist variables.
!-----------------------------------------------------------------------
!
        INTEGER(kind=KINT), POINTER :: NSOIL
!
        INTEGER(kind=KINT), POINTER :: NPHS
        INTEGER(kind=KINT) :: DT_INT,NPRECIP,NRADL,NRADS                &
                             ,PCPHR,UCMCALL
!
        LOGICAL ::        GWDFLG,                   NESTED,NHRS_UDEF    &
                  ,PCPFLG,        SPECIFIED,WRITE_PREC_ADJ              &
                  ,ENTRAIN,NEWALL,NEWSWAP,NEWUPUP,NODEEP,RST_OUT_00
!
!-----------------------------------------------------------------------
!***  Horizontal/Vertical grid
!-----------------------------------------------------------------------
!
        REAL(kind=KFPT) :: FRES,FR,FSL,FSS
!
!-----------------------------------------------------------------------
!***  Integration quantities.
!-----------------------------------------------------------------------
!
        INTEGER(kind=KINT) :: NHRS_CLOD                                 &  !<-- Fcst hours cloud is accumulated
                             ,NHRS_HEAT                                 &  !<-- Fcst hours heating is accumulated
                             ,NHRS_PREC                                 &  !<-- Fcst hours precip is accumulated
                             ,NHRS_RDLW                                 &  !<-- Fcst hours LW radiation is accumulated
                             ,NHRS_RDSW                                 &  !<-- Fcst hours SW radiation is accumulated
                             ,NHRS_SRFC                                    !<-- Fcst hours sfc evap/flux is accumulated
!
        INTEGER(kind=KINT), POINTER :: NCLOD                            &  !<-- # of fundamental timesteps cloud is accumulated
                                      ,NHEAT                            &  !<-- # of fundamental timesteps latent heating is accumulated
                                      ,NPREC                            &  !<-- # of fundamental timesteps precip is accumulated
                                      ,NRDLW                            &  !<-- # of fundamental timesteps LW radiation is accumulated
                                      ,NRDSW                            &  !<-- # of fundamental timesteps SW radiation is accumulated
                                      ,NSRFC                            &  !<-- # of fundamental timesteps sfc evap/flux is accumulated
                                      ,IVEGSRC                          &  !<-- Vegetation map identifier, 0--> USGS, 1--> IGBP
                                      ,CU_PHYSICS                       &  !<-- Convection physics, 1--> KF, 2--> BMJ, 3--> GD, 4--> SAS, 5--> RAS
                                      ,AVGMAXLEN                           !<-- Fcst sec over which avg/max diags are accumulated
!
        INTEGER(kind=KINT),DIMENSION(:,:),POINTER :: ISLTYP,IVGTYP      &
                                                    ,LPBL               &
                                                    ,NCFRCV,NCFRST
!
        REAL(kind=KFPT), POINTER :: UPHLCRIT                               !<-- Value of updraft helicity above which point values will be written for objects
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: DUDT,DVDT
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: RLWTT,RSWTT
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: EXCH_H,PPTDAT
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: TCUCN,W0AVG
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: CLDFRA              &
                                                   ,TRAIN,XLEN_MIX
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: SH2O,SMC,STC
!
        REAL(kind=KFPT),DIMENSION(:,:),POINTER :: CROT,SROT             &
                                                 ,HANGL,HANIS,HASYS     &
                                                 ,HASYSW,HASYNW,HASYW   &
                                                 ,HCNVX                 &
                                                 ,HLENNW,HLENSW         &
                                                 ,HLENW,HLENS           &
                                                 ,HSLOP,HSTDV,HZMAX
!
        REAL(kind=KFPT),DIMENSION(:,:),POINTER :: ACFRCV,ACFRST         &
                                                 ,AKHS,AKHS_OUT         &
                                                 ,AKMS,AKMS_OUT         &
                                                 ,CFRACH,CFRACL,CFRACM  &
                                                 ,CMC,CNVBOT,CNVTOP     &
                                                 ,CPRATE,CUPPT          &
                                                 ,CZMEAN,CZEN,LSPA      &
                                                 ,DDATA,EPSR            &
                                                 ,HBOT,HBOTD,HBOTS      &
                                                 ,HTOP,HTOPD,HTOPS      &
                                                 ,MIXHT,PBLH,QSH,QZ0    &
                                                 ,RLWIN,RSWIN,RSWINC    &
                                                 ,RSWOUT,RLWTOA,RSWTOA  &
                                                 ,SIGT4                 &
                                                 ,SST,STDH              & !zj
                                                 ,THS,THZ0,USTAR        & !zj
                                                 ,UZ0,VZ0               &
                                                 ,Z0,Z0BASE
!
        REAL(kind=KFPT),DIMENSION(:,:),POINTER :: ALBASE,ALBEDO         &
                                                 ,ALWIN,ALWOUT,ALWTOA   &
                                                 ,ASWIN,ASWOUT,ASWTOA   &
                                                 ,BGROFF,GRNFLX         &
                                                 ,MAVAIL,MXSNAL         &
                                                 ,POTEVP,POTFLX         &
                                                 ,QCG,QSG,QVG,QWBS      &
                                                 ,RADOT,RMOL            &
                                                 ,SFCEVP,SFCEXC         &
                                                 ,SFCLHX,SFCSHX         &
                                                 ,SHDMAX,SHDMIN         &
                                                 ,SI,SMSTAV,SMSTOT      &
                                                 ,SNO,SNOAVG,SNOPCX     &
                                                 ,SOILT1,SOILTB         &
                                                 ,SR,SSROFF,SUBSHX      &
                                                 ,TG,TSKIN,TSNAV,TWBS   &
                                                 ,VEGFRC
!
        REAL(kind=KFPT),DIMENSION(:,:),POINTER :: ACPREC,ACSNOM,ACSNOW  &
                                                 ,CUPREC,CLDEFI         &
                                                 ,PREC,PSHLTR,P10,Q02   &
                                                 ,Q10,QSHLTR,PSFC       &
                                                 ,PSFCAVG               &
                                                 ,T2,TH02,TH10,TSHLTR   &
                                                 ,T02MAX,T02MIN         &
                                                 ,RH02MAX,RH02MIN       &
                                                 ,U10,V10,T10,T10AVG    &
                                                 ,U10MAX,V10MAX,SPD10MAX &
                                                 ,TLMIN,TLMAX           &
                                                 ,UPVVELMAX,DNVVELMAX   &
                                                 ,UPHLMAX,UPHLOBJMAX,REFDMAX       &
                                                 ,AKHSAVG,AKMSAVG
!
!-----------------------------------------------------------------------
!***  The following are 2-D arrays needed only to hold scalars.
!***  This is done since ESMF does not permit scalar Attributes
!***  to evolve.
!-----------------------------------------------------------------------
!
        REAL(kind=KFPT),DIMENSION(:,:),POINTER :: ACUTIM                &  !<-- Counter for cloud processes, used by post0 code
                                                 ,APHTIM                &  !<-- Counter for other processes, used by post0 code
                                                 ,ARDLW                 &  !<-- Counter in summing LW radiation flux
                                                 ,ARDSW                 &  !<-- Counter in summing SW radiation flux
                                                 ,ASRFC                 &  !<-- Counter in summing sfc flux
                                                 ,AVRAIN                &  !<-- Counter in summing latent heating from grid microphysics
                                                 ,AVCNVC                   !<-- Counter in summing latent heating from convection
!
        REAL(kind=KFPT),DIMENSION(:),POINTER :: MP_RESTART_STATE        &
                                               ,SLDPTH                  &
                                               ,TBPVS_STATE,TBPVS0_STATE
!
!-----------------------------------------------------------------------
!***  GFS physics additional arrays
!-----------------------------------------------------------------------
!
        REAL(kind=KDBL)                              :: CDEC,SDEC       &
                                                       ,SLAG,SOLCON
        INTEGER        ,DIMENSION(:)      ,POINTER   :: JINDX1,JINDX2
        REAL(kind=KDBL),DIMENSION(:)      ,POINTER   :: DDY
        REAL(kind=KDBL),DIMENSION(:,:)    ,POINTER   :: TMPMIN,TMPMAX
        REAL(kind=KDBL),DIMENSION(:,:)    ,POINTER   :: DUGWD,DVGWD
        REAL(kind=KDBL),DIMENSION(:,:)    ,POINTER   :: SEMIS,SFALB     &
                                                       ,SFCDLW,SFCDSW   &
                                                       ,SFCNSW,TSFLW 
        REAL(kind=KDBL),DIMENSION(:,:)    ,POINTER   :: SICFCS,SIHFCS   &
                                                       ,SLPFCS,SOTFCS   &
                                                       ,TG3FCS          &
                                                       ,VEGFCS,VETFCS   &
                                                       ,ZORFCS
        REAL(kind=KDBL),DIMENSION(:,:,:)  ,POINTER   :: ALBFC1,ALFFC1
        REAL(kind=KDBL),DIMENSION(:,:,:)  ,POINTER   :: PHY_F2DV   ! save last time step 2Ds
        REAL(kind=KDBL),DIMENSION(:,:,:,:),POINTER   :: PHY_F3DV   ! save last time step 3Ds
        REAL(kind=KDBL),DIMENSION(:,:,:,:),POINTER   :: OZPLIN
!
!-----------------------------------------------------------------------
!***  GFS microphysics additional arrays saving surface pressure,
!     Temperature,water vapor at previous time steps, Weiguo Wang 11-22-2010
!-----------------------------------------------------------------------
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: TP1,QP1
        REAL(kind=KFPT),DIMENSION(:,:),  POINTER :: PSP1
!
        LOGICAL :: GFS
!
        INTEGER :: CO2TF
!
! FROM PHY end
!-----------------------------------------------------------------------
!***  Output
!-----------------------------------------------------------------------
!
        INTEGER :: NUM_VARS = 0
!
        TYPE(VAR),DIMENSION(MAX_VARS) :: VARS
!
        TYPE(ESMF_FieldBundle),DIMENSION(1:2) :: BUNDLE_ARRAY
!
!-----------------------------------------------------------------------
!
      END TYPE SOLVER_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!***  The INTERNAL_STATE type is supported by a C pointer (not an F90
!***  pointer) and therefore the following type is needed.
!-----------------------------------------------------------------------
!
      TYPE WRAP_SOLVER_INT_STATE
        TYPE(SOLVER_INTERNAL_STATE),POINTER :: INT_STATE
      END TYPE WRAP_SOLVER_INT_STATE
!
!-----------------------------------------------------------------------
!
      INTEGER(kind=KINT) :: ITS,ITE,IMS,IME,IDS,IDE                     &
                           ,JTS,JTE,JMS,JME,JDS,JDE
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_INTERNAL_STATE_SOLVER(INT_STATE                    &
                                          ,LM                           &
                                          ,ITS,ITE,JTS,JTE              &
                                          ,IMS,IME,JMS,JME              &
                                          ,IDS,IDE,JDS,JDE              &
                                          ,IHALO,JHALO                  &
                                          ,MYPE                         &
                                          ,RC )
!
!-----------------------------------------------------------------------
!***  Allocate the internal state quantities in the Solver component's
!***  Init step.  
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(SOLVER_INTERNAL_STATE),INTENT(INOUT) :: INT_STATE               !<-- The SOLVER internal state
!
      INTEGER(kind=KINT),INTENT(IN) :: ITS,ITE,JTS,JTE                  &  !<-- Integration limits of task subdomains
                                      ,IMS,IME,JMS,JME                  &  !<-- Memory dimensions of task subdomain
                                      ,IDS,IDE,JDS,JDE                  &  !<-- Dimensions of full domain
                                      ,IHALO,JHALO                      &  !<-- Width of haloes in I and J
                                      ,LM                               &  !<-- Number of model layers
                                      ,MYPE                                !<-- Local task rank on this domain
!
      INTEGER, INTENT(OUT) :: RC
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER(kind=KINT) :: I,I_CYCLE,J,L,LNSH,LNSV,N,NV
      INTEGER(kind=KINT) :: TRACER_SIZE_1, TRACER_SIZE
!
      INTEGER :: LATSOZP,TIMEOZ,LEVOZP,PL_COEFF,KOZPL=28
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  The array called WATER is a special case needed to satisfy
!***  various WRF physics options.  The 4th dimension is set to
!***  1+Number_of_species including vapor.  The "1+" is needed
!***  because WRF never touches the first level.
!
!***  Set the P_ and F_ variables.
!***  V=>vapor; C=>cloudwater; R=>rain; I=>cloudice; S=>snow; G=>graupel
!***  Set the appropriate value for the logical F_ variables.
!***  For each species that is .TRUE., set the integer P_ variable
!***  to monotonically increasing values STARTING WITH 2.
!***  For the excluded species (F_*=.FALSE.), set the P_ variable to 1.
!-----------------------------------------------------------------------
!
        int_state%D_SS=1
      IF(TRIM(int_state%MICROPHYSICS)=='fer'.OR. &
         TRIM(int_state%MICROPHYSICS)=='fer_hires')THEN
        int_state%NUM_WATER=1+4
        int_state%P_QV=2
        int_state%P_QC=3
        int_state%P_QR=4
        int_state%P_QS=5
        int_state%P_QI=1
        int_state%P_QG=1
        int_state%F_QV=.TRUE.
        int_state%F_QC=.TRUE.
        int_state%F_QR=.TRUE.
        int_state%F_QS=.TRUE.
        int_state%F_QI=.FALSE.
        int_state%F_QG=.FALSE.
        if(int_state%lmprate) int_state%D_SS=14
!      ELSEIF(TRIM(int_state%MICROPHYSICS)=='wsm3')THEN
!        int_state%NUM_WATER=1+3
!        int_state%P_QV=2
!        int_state%P_QC=3
!        int_state%P_QR=4
!        int_state%P_QS=1
!        int_state%P_QI=1
!        int_state%P_QG=1
!        int_state%F_QV=.TRUE.
!        int_state%F_QC=.TRUE.
!        int_state%F_QR=.TRUE.
!        int_state%F_QS=.FALSE.
!        int_state%F_QI=.FALSE.
!        int_state%F_QG=.FALSE.
      ELSEIF(TRIM(int_state%MICROPHYSICS)=='wsm6')THEN
        int_state%NUM_WATER=1+6
        int_state%P_QV=2
        int_state%P_QC=3
        int_state%P_QR=4
        int_state%P_QS=5
        int_state%P_QI=6
        int_state%P_QG=7
        int_state%F_QV=.TRUE.
        int_state%F_QC=.TRUE.
        int_state%F_QR=.TRUE.
        int_state%F_QS=.TRUE.
        int_state%F_QI=.TRUE.
        int_state%F_QG=.TRUE.
        if(int_state%lmprate) int_state%D_SS=40
      ELSEIF(TRIM(int_state%MICROPHYSICS)=='gfs')THEN
        int_state%NUM_WATER=1+3
        int_state%P_QV=2
        int_state%P_QC=3
        int_state%P_QR=1
        int_state%P_QS=1
        int_state%P_QI=4
        int_state%P_QG=1
        int_state%F_QV=.TRUE.
        int_state%F_QC=.TRUE.
        int_state%F_QR=.FALSE.
        int_state%F_QS=.FALSE.
        int_state%F_QI=.TRUE.
        int_state%F_QG=.FALSE.
      ENDIF
!
      int_state%NUM_TRACERS_TOTAL=                                      &  !<-- # of 3-D arrays in 4-D TRACERS array
                                  int_state%NUM_TRACERS_MET             &  !<-- # of water, etc. tracers specified now (see below)
                                 +int_state%NUM_TRACERS_CHEM            &  !<-- # of specified scalars (chem, aerosol, etc.)
                                 +int_state%NUM_WATER                      !<-- # of water types
!
!------------------------------------------------
!***  Read and store the specifications for each
!***  internal state variable listed by the user
!***  in the Solver text file. 
!------------------------------------------------
!
      CALL READ_CONFIG('solver_state.txt'                           &
                      ,MYPE                                         &
                      ,int_state%VARS                               &
                      ,int_state%NUM_VARS                           &
                      ,RC )
      IF (RC /= 0) RETURN
!
!-------------------------------------------------------------------
!***  Allocate appropriate memory within the Solver's composite 
!***  VARS array for all internal state variables that are 'Owned'
!***  and point those variables into that memory.
!-------------------------------------------------------------------
!
      NV=int_state%NUM_VARS

      CALL SET_VAR_PTR(int_state%VARS,NV,'IM'               ,int_state%IM              )
      CALL SET_VAR_PTR(int_state%VARS,NV,'JM'               ,int_state%JM              )
      CALL SET_VAR_PTR(int_state%VARS,NV,'LM'               ,int_state%LM              )
      CALL SET_VAR_PTR(int_state%VARS,NV,'IHRST'            ,int_state%IHRST           )
      CALL SET_VAR_PTR(int_state%VARS,NV,'I_PAR_STA'        ,int_state%I_PAR_STA       )
      CALL SET_VAR_PTR(int_state%VARS,NV,'J_PAR_STA'        ,int_state%J_PAR_STA       )
      CALL SET_VAR_PTR(int_state%VARS,NV,'LAST_STEP_MOVED'  ,int_state%LAST_STEP_MOVED )
      CALL SET_VAR_PTR(int_state%VARS,NV,'LPT2'             ,int_state%LPT2            )
      CALL SET_VAR_PTR(int_state%VARS,NV,'NMTS'             ,int_state%NMTS            )

      CALL SET_VAR_PTR(int_state%VARS,NV,'MDRMINout' ,int_state%MDRMINout )
      CALL SET_VAR_PTR(int_state%VARS,NV,'MDRMAXout' ,int_state%MDRMAXout )
      CALL SET_VAR_PTR(int_state%VARS,NV,'MDIMINout' ,int_state%MDIMINout )
      CALL SET_VAR_PTR(int_state%VARS,NV,'MDIMAXout' ,int_state%MDIMAXout )

      IF(TRIM(int_state%MICROPHYSICS)=='fer') THEN
        int_state%MDRMINout=50
        int_state%MDRMAXout=450
        int_state%MDIMINout=50
        int_state%MDIMAXout=1000
      ELSEIF  (TRIM(int_state%MICROPHYSICS)=='fer_hires')THEN
        int_state%MDRMINout=50
        int_state%MDRMAXout=1000
        int_state%MDIMINout=50
        int_state%MDIMAXout=1000
      ENDIF

      CALL SET_VAR_PTR(int_state%VARS,NV,'NSOIL'      ,int_state%NSOIL )
      CALL SET_VAR_PTR(int_state%VARS,NV,'NPHS'       ,int_state%NPHS  )
      CALL SET_VAR_PTR(int_state%VARS,NV,'NCLOD'      ,int_state%NCLOD )
      CALL SET_VAR_PTR(int_state%VARS,NV,'NHEAT'      ,int_state%NHEAT )
      CALL SET_VAR_PTR(int_state%VARS,NV,'NPREC'      ,int_state%NPREC )
      CALL SET_VAR_PTR(int_state%VARS,NV,'NRDLW'      ,int_state%NRDLW )
      CALL SET_VAR_PTR(int_state%VARS,NV,'NRDSW'      ,int_state%NRDSW )
      CALL SET_VAR_PTR(int_state%VARS,NV,'NSRFC'      ,int_state%NSRFC )
      CALL SET_VAR_PTR(int_state%VARS,NV,'AVGMAXLEN'  ,int_state%AVGMAXLEN  )
      CALL SET_VAR_PTR(int_state%VARS,NV,'UPHLCRIT'  ,int_state%UPHLCRIT  )
      CALL SET_VAR_PTR(int_state%VARS,NV,'IVEGSRC'    ,int_state%IVEGSRC    )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CU_PHYSICS' ,int_state%CU_PHYSICS )

      CALL SET_VAR_PTR(int_state%VARS,NV,'DT'        ,int_state%DT        )
      CALL SET_VAR_PTR(int_state%VARS,NV,'DYH'       ,int_state%DYH       )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PDTOP'     ,int_state%PDTOP     )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PT'        ,int_state%PT        )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TLM0D'     ,int_state%TLM0D     )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TPH0D'     ,int_state%TPH0D     )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TSTART'    ,int_state%TSTART    )
      CALL SET_VAR_PTR(int_state%VARS,NV,'DPHD'      ,int_state%DPHD      )
      CALL SET_VAR_PTR(int_state%VARS,NV,'DLMD'      ,int_state%DLMD      )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SBD'       ,int_state%SBD       )
      CALL SET_VAR_PTR(int_state%VARS,NV,'WBD'       ,int_state%WBD       )

      CALL SET_VAR_PTR(int_state%VARS,NV,'IDAT'      ,int_state%IDAT    ,1 ,3 )

      CALL SET_VAR_PTR(int_state%VARS,NV,'DXH'       ,int_state%DXH     ,JDS, JDE )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SG1'       ,int_state%SG1     ,1, LM+1  )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SG2'       ,int_state%SG2     ,1, LM+1  )
      CALL SET_VAR_PTR(int_state%VARS,NV,'DSG1'      ,int_state%DSG1    ,1, LM    )
      CALL SET_VAR_PTR(int_state%VARS,NV,'DSG2'      ,int_state%DSG2    ,1, LM    )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SGML1'     ,int_state%SGML1   ,1, LM    )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SGML2'     ,int_state%SGML2   ,1, LM    )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SGM'       ,int_state%SGM     ,1, LM+1  )

      CALL SET_VAR_PTR(int_state%VARS,NV,'NTSCM'     ,int_state%NTSCM   ,1, NUM_DOMAINS_MAX )

      CALL SET_VAR_PTR(int_state%VARS,NV,'MASSRout'  ,int_state%MASSRout ,1, int_state%MDRMAXout-int_state%MDRMINout+1 )
      CALL SET_VAR_PTR(int_state%VARS,NV,'MASSIout'  ,int_state%MASSIout ,1, int_state%MDIMAXout-int_state%MDIMINout+1 )

      CALL SET_VAR_PTR(int_state%VARS,NV,'ISLTYP'     ,int_state%ISLTYP   ,(/ IMS,JMS /),(/ IME,JME /) )  
      CALL SET_VAR_PTR(int_state%VARS,NV,'IVGTYP'     ,int_state%IVGTYP   ,(/ IMS,JMS /),(/ IME,JME /) )  
      CALL SET_VAR_PTR(int_state%VARS,NV,'NCFRCV'     ,int_state%NCFRCV   ,(/ IMS,JMS /),(/ IME,JME /) )  
      CALL SET_VAR_PTR(int_state%VARS,NV,'NCFRST'     ,int_state%NCFRST   ,(/ IMS,JMS /),(/ IME,JME /) )  

      CALL SET_VAR_PTR(int_state%VARS,NV,'SLDPTH'     ,int_state%SLDPTH	      ,1 ,NUM_SOIL_LAYERS  ) 
      CALL SET_VAR_PTR(int_state%VARS,NV,'MP_RESTART' ,int_state%MP_RESTART_STATE  ,1 ,MICRO_RESTART    ) 
      CALL SET_VAR_PTR(int_state%VARS,NV,'TBPVS_STAT' ,int_state%TBPVS_STATE       ,1 ,MICRO_RESTART    ) 
      CALL SET_VAR_PTR(int_state%VARS,NV,'TBPVS0_STA' ,int_state%TBPVS0_STATE      ,1 ,MICRO_RESTART    ) 

      CALL SET_VAR_PTR(int_state%VARS,NV,'BARO'      ,int_state%BARO    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'FIS'       ,int_state%FIS     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'F'         ,int_state%F       ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'GLAT'      ,int_state%GLAT    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'GLON'      ,int_state%GLON    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HDACX'     ,int_state%HDACX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HDACY'     ,int_state%HDACY   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PD'        ,int_state%PD      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'VLAT'      ,int_state%VLAT    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'VLON'      ,int_state%VLON    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HDACVX'    ,int_state%HDACVX  ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HDACVY'    ,int_state%HDACVY  ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PDO'       ,int_state%PDO     ,(/ IMS,JMS /),(/ IME,JME /) )

      CALL SET_VAR_PTR(int_state%VARS,NV,'T'         ,int_state%T       ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'Q'         ,int_state%Q       ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'U'         ,int_state%U       ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'V'         ,int_state%V       ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'Q2'        ,int_state%Q2      ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CW'        ,int_state%CW      ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'W'         ,int_state%W       ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'W_TOT'     ,int_state%W_TOT   ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'DWDT'      ,int_state%DWDT    ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PINT'      ,int_state%PINT    ,(/ IMS,JMS,1 /),(/ IME,JME,LM+1 /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'OMGALF'    ,int_state%OMGALF  ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'O3'        ,int_state%O3      ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'DIV'       ,int_state%DIV     ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RTOP'      ,int_state%RTOP    ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TCU'       ,int_state%TCU     ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TCV'       ,int_state%TCV     ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TCT'       ,int_state%TCT     ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TP'        ,int_state%TP      ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'UP'        ,int_state%UP      ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'VP'        ,int_state%VP      ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'E2'        ,int_state%E2      ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PSGDT'     ,int_state%PSGDT   ,(/ IMS,JMS,1 /),(/ IME,JME,LM-1 /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'Z'         ,int_state%Z       ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'Told'      ,int_state%Told    ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'Tadj'      ,int_state%Tadj    ,(/ IMS,JMS,1 /),(/ IME,JME,LM /) )

      CALL SET_VAR_PTR(int_state%VARS,NV,'ACFRCV'     ,int_state%ACFRCV   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ACFRST'     ,int_state%ACFRST   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ACPREC'     ,int_state%ACPREC   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ACSNOM'     ,int_state%ACSNOM   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ACSNOW'     ,int_state%ACSNOW   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'AKHS_OUT'   ,int_state%AKHS_OUT ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'AKHSAVG'    ,int_state%AKHSAVG  ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'AKMS_OUT'   ,int_state%AKMS_OUT ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'AKMSAVG'    ,int_state%AKMSAVG  ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ALBASE'     ,int_state%ALBASE   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ALBEDO'     ,int_state%ALBEDO   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ALWIN'      ,int_state%ALWIN    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ALWOUT'     ,int_state%ALWOUT   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ALWTOA'     ,int_state%ALWTOA   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ASWIN'      ,int_state%ASWIN    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ASWOUT'     ,int_state%ASWOUT   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ASWTOA'     ,int_state%ASWTOA   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'BGROFF'     ,int_state%BGROFF   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CFRACH'     ,int_state%CFRACH   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CFRACL'     ,int_state%CFRACL   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CFRACM'     ,int_state%CFRACM   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CLDEFI'     ,int_state%CLDEFI   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CMC'        ,int_state%CMC      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CNVBOT'     ,int_state%CNVBOT   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CNVTOP'     ,int_state%CNVTOP   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CPRATE'     ,int_state%CPRATE   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CUPPT'      ,int_state%CUPPT    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CUPREC'     ,int_state%CUPREC   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CZEN'       ,int_state%CZEN     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'CZMEAN'     ,int_state%CZMEAN   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'DNVVELMAX'  ,int_state%DNVVELMAX,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'EPSR'       ,int_state%EPSR     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'FIS'        ,int_state%FIS      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'GRNFLX'     ,int_state%GRNFLX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HBOTD'      ,int_state%HBOTD    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HBOTS'      ,int_state%HBOTS    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HTOPD'      ,int_state%HTOPD    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HTOPS'      ,int_state%HTOPS    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'MIXHT'      ,int_state%MIXHT    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'MXSNAL'     ,int_state%MXSNAL   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PBLH'       ,int_state%PBLH     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PD'         ,int_state%PD       ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'POTEVP'     ,int_state%POTEVP   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PREC'       ,int_state%PREC     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PSFCAVG'    ,int_state%PSFCAVG  ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PSHLTR'     ,int_state%PSHLTR   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'P10'        ,int_state%P10      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'Q10'        ,int_state%Q10      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'QSH'        ,int_state%QSH      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'QSHLTR'     ,int_state%QSHLTR   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'QWBS'       ,int_state%QWBS     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'QZ0'        ,int_state%QZ0      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RADOT'      ,int_state%RADOT    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'REFDMAX'    ,int_state%REFDMAX  ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RH02MAX'    ,int_state%RH02MAX  ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RH02MIN'    ,int_state%RH02MIN  ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RLWIN'      ,int_state%RLWIN    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RLWTOA'     ,int_state%RLWTOA   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RSWIN'      ,int_state%RSWIN    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RSWINC'     ,int_state%RSWINC   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RSWOUT'     ,int_state%RSWOUT   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SFCEVP'     ,int_state%SFCEVP   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SFCEXC'     ,int_state%SFCEXC   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SFCLHX'     ,int_state%SFCLHX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SFCSHX'     ,int_state%SFCSHX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SI'         ,int_state%SI       ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SICE'       ,int_state%SICE     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SIGT4'      ,int_state%SIGT4    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SM'         ,int_state%SM       ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SMSTAV'     ,int_state%SMSTAV   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SMSTOT'     ,int_state%SMSTOT   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SNO'        ,int_state%SNO      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SNOAVG'     ,int_state%SNOAVG   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SNOPCX'     ,int_state%SNOPCX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SOILTB'     ,int_state%SOILTB   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SPD10MAX'   ,int_state%SPD10MAX ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SR'         ,int_state%SR       ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SSROFF'     ,int_state%SSROFF   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SST'        ,int_state%SST      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'SUBSHX'     ,int_state%SUBSHX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'T02MAX'     ,int_state%T02MAX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'T02MIN'     ,int_state%T02MIN   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'T10'        ,int_state%T10      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'T10AVG'     ,int_state%T10AVG   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TG'         ,int_state%TG       ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TH10'       ,int_state%TH10     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'THS'        ,int_state%THS      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'THZ0'       ,int_state%THZ0     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TSHLTR'     ,int_state%TSHLTR   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TWBS'       ,int_state%TWBS     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'UPHLMAX'    ,int_state%UPHLMAX  ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'UPHLOBJMAX' ,int_state%UPHLOBJMAX,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'UPVVELMAX'  ,int_state%UPVVELMAX,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'U10'        ,int_state%U10      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'U10MAX'     ,int_state%U10MAX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'USTAR'      ,int_state%USTAR    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'UZ0'        ,int_state%UZ0      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'V10'        ,int_state%V10      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'V10MAX'     ,int_state%V10MAX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'VEGFRC'     ,int_state%VEGFRC   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'VZ0'        ,int_state%VZ0      ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'Z0'         ,int_state%Z0       ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TSKIN'      ,int_state%TSKIN    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'AKHS'       ,int_state%AKHS     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'AKMS'       ,int_state%AKMS     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HBOT'       ,int_state%HBOT     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'HTOP'       ,int_state%HTOP     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RSWTOA'     ,int_state%RSWTOA   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'POTFLX'     ,int_state%POTFLX   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'RMOL'       ,int_state%RMOL     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'T2'         ,int_state%T2       ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'Z0BASE'     ,int_state%Z0BASE   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'PSFC'       ,int_state%PSFC     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TLMIN'      ,int_state%TLMIN    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TLMAX'      ,int_state%TLMAX    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'LSPA'       ,int_state%LSPA     ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ACUTIM'     ,int_state%ACUTIM   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'APHTIM'     ,int_state%APHTIM   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ARDLW'      ,int_state%ARDLW    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ARDSW'      ,int_state%ARDSW    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'ASRFC'      ,int_state%ASRFC    ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'AVRAIN'     ,int_state%AVRAIN   ,(/ IMS,JMS /),(/ IME,JME /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'AVCNVC'     ,int_state%AVCNVC   ,(/ IMS,JMS /),(/ IME,JME /) )

      CALL SET_VAR_PTR(int_state%VARS,NV,'RLWTT'      ,int_state%RLWTT    ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'RSWTT'      ,int_state%RSWTT    ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'EXCH_H'     ,int_state%EXCH_H   ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'CLDFRA'     ,int_state%CLDFRA   ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'F_ICE'      ,int_state%F_ICE    ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'F_RAIN'     ,int_state%F_RAIN   ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'F_RIMEF'    ,int_state%F_RIMEF  ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'TRAIN'      ,int_state%TRAIN    ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'XLEN_MIX'   ,int_state%XLEN_MIX ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'TCUCN'      ,int_state%TCUCN    ,(/ IMS,JMS,1 /),(/ IME,JME,LM /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'SMC'        ,int_state%SMC      ,(/ IMS,JMS,1 /),(/ IME,JME,NUM_SOIL_LAYERS /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'STC'        ,int_state%STC      ,(/ IMS,JMS,1 /),(/ IME,JME,NUM_SOIL_LAYERS /))
      CALL SET_VAR_PTR(int_state%VARS,NV,'SH2O'       ,int_state%SH2O     ,(/ IMS,JMS,1 /),(/ IME,JME,NUM_SOIL_LAYERS /))

      CALL SET_VAR_PTR(int_state%VARS,NV,'TRACERS'     ,int_state%TRACERS      ,(/ IMS,JMS,1,1 /),(/ IME,JME,LM,int_state%NUM_TRACERS_TOTAL /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'TRACERS_PREV',int_state%TRACERS_PREV ,(/ IMS,JMS,1,1 /),(/ IME,JME,LM,int_state%NUM_TRACERS_TOTAL /) )
      CALL SET_VAR_PTR(int_state%VARS,NV,'MPRATES'     ,int_state%MPRATES      ,(/ IMS,JMS,1,1 /),(/ IME,JME,LM,int_state%D_SS /) )

      DO N=1,NV
        IF (int_state%VARS(N)%TKR==0) THEN
          write(0,*)' Error in SET_DYN_VAR_PTR. '
          write(0,*)' Variable ',TRIM(int_state%VARS(N)%VBL_NAME),' is not associated to an internal state fortran pointer'
          STOP
        END IF
      END DO

!
!-----------------------------------------------------------------------
!***  Calculate the size of the storage needed for one tracer variable (TRACER_SIZE_1)
!***  and the size for all tracers (TRACER_SIZE). Then allocate one-dimensional
!***  arrays that will serve as actual storage.  The actual storage
!***  array must be 1-D because multi-dimensional pointers are used 
!***  to represent the actual tracer variables.  Fortran will allow
!***  a multi-dimensional pointer to point only into a 1-D target
!***  when remapping the memory to the pointer's dimensions.
!-----------------------------------------------------------------------
!
      TRACER_SIZE_1 = (IME-IMS+1)*(JME-JMS+1)*LM
      TRACER_SIZE = TRACER_SIZE_1*int_state%NUM_TRACERS_TOTAL 

      ALLOCATE(int_state%TRACERS_ARR     (tracer_size))           ;int_state%TRACERS_ARR      = R4_IN 
      ALLOCATE(int_state%TRACERS_PREV_ARR(tracer_size))           ;int_state%TRACERS_PREV_ARR = R4_IN 
!
!-----------------------------------------------------------------------
!***  Point TRACERS as 4D array at the TRACERS_ARR (one-dimensional storage array)
!-----------------------------------------------------------------------
!
      CALL FIND_VAR_INDX('TRACERS',int_state%VARS,int_state%NUM_VARS,I)
      int_state%VARS(I)%R4D (IMS:IME,JMS:JME,1:LM,1:int_state%NUM_TRACERS_TOTAL) => int_state%TRACERS_ARR
      int_state%TRACERS=>int_state%VARS(I)%R4D
!
!-----------------------------------------------------------------------
!***  Point TRACERS_PREV as 4D array at the TRACERS_PREV_ARR (one-dimensional storage array)
!-----------------------------------------------------------------------
!
      CALL FIND_VAR_INDX('TRACERS_PREV',int_state%VARS,int_state%NUM_VARS,I)
      int_state%VARS(I)%R4D (IMS:IME,JMS:JME,1:LM,1:int_state%NUM_TRACERS_TOTAL) => int_state%TRACERS_PREV_ARR
      int_state%TRACERS_PREV=>int_state%VARS(I)%R4D
      
!-----------------------------------------------------------------------
!***  Point Q at level 1(INDX_Q) of the Tracers array.
!-----------------------------------------------------------------------
!
      int_state%INDX_Q=1
      CALL FIND_VAR_INDX('Q',int_state%VARS,int_state%NUM_VARS,I)
      int_state%VARS(I)%R3D(IMS:IME,JMS:JME,1:LM) => int_state%TRACERS_ARR( (int_state%INDX_Q-1)*TRACER_SIZE_1+1 : int_state%INDX_Q*TRACER_SIZE_1)
      int_state%Q=>int_state%VARS(I)%R3D
!
!-----------------------------------------------------------------------
!***  Point CW (Combined cloud water array) at level 2(INDX_CW) of the Tracers array.
!-----------------------------------------------------------------------
!
      int_state%INDX_CW=2
      CALL FIND_VAR_INDX('CW',int_state%VARS,int_state%NUM_VARS,I)
      int_state%VARS(I)%R3D(IMS:IME,JMS:JME,1:LM) => int_state%TRACERS_ARR( (int_state%INDX_CW-1)*TRACER_SIZE_1+1 : int_state%INDX_CW*TRACER_SIZE_1)
      int_state%CW=>int_state%VARS(I)%R3D
!
!-----------------------------------------------------------------------
!***  Point E2 (Turbulence kinetic energy) at level 3(INDX_Q2) of the Tracers array.
!-----------------------------------------------------------------------
!
      int_state%INDX_Q2=3
      CALL FIND_VAR_INDX('E2',int_state%VARS,int_state%NUM_VARS,I)
      int_state%VARS(I)%R3D(IMS:IME,JMS:JME,1:LM) => int_state%TRACERS_ARR( (int_state%INDX_Q2-1)*TRACER_SIZE_1+1 : int_state%INDX_Q2*TRACER_SIZE_1)
      int_state%E2=>int_state%VARS(I)%R3D
!
!-----------------------------------------------------------------------
!***  Point O3 (General tracer for testin) at level 4(INDX_O3) of the Tracers array.
!-----------------------------------------------------------------------
!
      int_state%INDX_O3=4
      CALL FIND_VAR_INDX('O3',int_state%VARS,int_state%NUM_VARS,I)
      int_state%VARS(I)%R3D(IMS:IME,JMS:JME,1:LM) => int_state%TRACERS_ARR( (int_state%INDX_O3-1)*TRACER_SIZE_1+1 : int_state%INDX_O3*TRACER_SIZE_1)
      int_state%O3=>int_state%VARS(I)%R3D
!
!--------------------------------
!***  Water tracers
!--------------------------------
!
      int_state%INDX_WATER_START = int_state%NUM_TRACERS_MET + int_state%NUM_TRACERS_CHEM + 1
      int_state%INDX_WATER_END = int_state%INDX_WATER_START + int_state%NUM_WATER - 1
      int_state%WATER(IMS:IME,JMS:JME,1:LM,1:int_state%NUM_WATER) => int_state%TRACERS_ARR( (int_state%INDX_WATER_START-1)*TRACER_SIZE_1+1 : int_state%INDX_WATER_END*TRACER_SIZE_1)
!
!-----------------------------------------------------------------------
!***  We can retrieve LM from the internal state since it was
!***  placed there already from the config file.
!-----------------------------------------------------------------------
!
      I_CYCLE=IDE-3
!
      LNSH=int_state%LNSH
      LNSV=int_state%LNSV
!
!-----------------------------------------------------------------------
!***  Explicitly allocate standard arrays in the Solver internal state.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Grid-related constants.
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%PDSG1 (1:LM))             ;int_state%PDSG1  = R4_IN  !<-- Thicknesses of pressure layers in press. range
      ALLOCATE(int_state%PSGML1(1:LM))             ;int_state%PSGML1 = R4_IN  !<-- Pressure at midlayers in pressure range
!   
      ALLOCATE(int_state%PSG1(1:LM+1))             ;int_state%PSG1   = R4_IN  !<-- Pressure at interfaces in pressure range  (Pa)
!     
      ALLOCATE(int_state%KHFILT(JDS:JDE))          ;int_state%KHFILT = I4_IN  !<-- Polar filter, truncation wave #, h points
      ALLOCATE(int_state%KVFILT(JDS:JDE))          ;int_state%KVFILT = I4_IN  !<-- Polar filter, truncation wave #, v points
      ALLOCATE(int_state%NHSMUD(JMS:JME))          ;int_state%NHSMUD = I4_IN  !<-- Polar smoother for unfiltered variables
!     
      ALLOCATE(int_state%HFILT(IDS:IDE,JDS:JDE))   ;int_state%HFILT  = R4_IN  !<-- Polar filter, h points
      ALLOCATE(int_state%VFILT(IDS:IDE,JDS:JDE))   ;int_state%VFILT  = R4_IN  !<-- Polar filter, v points
!     
      ALLOCATE(int_state%CURV (JDS:JDE))           ;int_state%CURV   = R4_IN  !<-- Curvature term in coriolis force  (m-1)
      ALLOCATE(int_state%DARE (JDS:JDE))           ;int_state%DARE   = R4_IN  !<-- Gridbox area  (m2)
      ALLOCATE(int_state%DDMPU(JDS:JDE))           ;int_state%DDMPU  = R4_IN  !<-- Divergence damping coefficient, x direction  (m)
      ALLOCATE(int_state%DDV  (JDS:JDE))           ;int_state%DDV    = R4_IN  !<-- Gridbox diagonal distance  (m)
      ALLOCATE(int_state%DXV  (JDS:JDE))           ;int_state%DXV    = R4_IN  !<-- Delta x, v points  (m)
      ALLOCATE(int_state%FAD  (JDS:JDE))           ;int_state%FAD    = R4_IN  !<-- Momentum advection factor
      ALLOCATE(int_state%FAH  (JDS:JDE))           ;int_state%FAH    = R4_IN  !<-- z, w advection factor
      ALLOCATE(int_state%FCP  (JDS:JDE))           ;int_state%FCP    = R4_IN  !<-- Temperature advection factor
      ALLOCATE(int_state%FDIV (JDS:JDE))           ;int_state%FDIV   = R4_IN  !<-- Divergence factor
      ALLOCATE(int_state%RARE (JDS:JDE))           ;int_state%RARE   = R4_IN  !<-- 1 / gridbox area  (m-2)
      ALLOCATE(int_state%RDDV (JDS:JDE))           ;int_state%RDDV   = R4_IN  !<-- 1 / gridbox diagonal distance  (m-1)
      ALLOCATE(int_state%RDXH (JDS:JDE))           ;int_state%RDXH   = R4_IN  !<-- 1 / delta x, h points  (m-1)
      ALLOCATE(int_state%RDXV (JDS:JDE))           ;int_state%RDXV   = R4_IN  !<-- 1 / delta x, v points  (m-1)
      ALLOCATE(int_state%WPDAR(JDS:JDE))           ;int_state%WPDAR  = R4_IN  !<-- Weight of grid separation filter
!     
      ALLOCATE(int_state%WFFTRH(1:2*I_CYCLE))      ;int_state%WFFTRH = R4_IN  !<-- FFT working field, h points
      ALLOCATE(int_state%WFFTRW(1:2*I_CYCLE))      ;int_state%WFFTRW = R4_IN  !<-- FFT working field, v points
      ALLOCATE(int_state%NFFTRH(1:15))             ;int_state%NFFTRH = I4_IN  !<-- FFT working field, h points
      ALLOCATE(int_state%NFFTRW(1:15))             ;int_state%NFFTRW = I4_IN  !<-- FFT working field, v points
!
!-----------------------------------------------------------------------
!***  Local horizontal subdomain limits for all forecast tasks.
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%LOCAL_ISTART(0:int_state%NUM_PES-1))  ;int_state%LOCAL_ISTART = I4_IN 
      ALLOCATE(int_state%LOCAL_IEND  (0:int_state%NUM_PES-1))  ;int_state%LOCAL_IEND   = I4_IN 
      ALLOCATE(int_state%LOCAL_JSTART(0:int_state%NUM_PES-1))  ;int_state%LOCAL_JSTART = I4_IN 
      ALLOCATE(int_state%LOCAL_JEND  (0:int_state%NUM_PES-1))  ;int_state%LOCAL_JEND   = I4_IN 
!
      int_state%IMS=IMS
      int_state%IME=IME
      int_state%JMS=JMS
      int_state%JME=JME
      int_state%IDS=IDS
      int_state%IDE=IDE
      int_state%JDS=JDS
      int_state%JDE=JDE
!
      int_state%IHALO=IHALO
      int_state%JHALO=JHALO
!
!-----------------------------------------------------------------------
!***  Regional boundary conditions.
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%UBN(IMS:IME,1:LNSV,1:LM,1:2))    ;int_state%UBN  = R4_IN  !<-- U wind component at northern boundary  (m s-1)
      ALLOCATE(int_state%UBS(IMS:IME,1:LNSV,1:LM,1:2))    ;int_state%UBS  = R4_IN  !<-- U wind component at southern boundary  (m s-1)
      ALLOCATE(int_state%VBN(IMS:IME,1:LNSV,1:LM,1:2))    ;int_state%VBN  = R4_IN  !<-- V wind component at northern boundary  (m s-1)
      ALLOCATE(int_state%VBS(IMS:IME,1:LNSV,1:LM,1:2))    ;int_state%VBS  = R4_IN  !<-- V wind component at southern boundary  (m s-1)
!
      ALLOCATE(int_state%UBE(1:LNSV,JMS:JME,1:LM,1:2))    ;int_state%UBE  = R4_IN  !<-- U wind component at eastern boundary  (m s-1)
      ALLOCATE(int_state%UBW(1:LNSV,JMS:JME,1:LM,1:2))    ;int_state%UBW  = R4_IN  !<-- U wind component at western boundary  (m s-1)
      ALLOCATE(int_state%VBE(1:LNSV,JMS:JME,1:LM,1:2))    ;int_state%VBE  = R4_IN  !<-- V wind component at eastern boundary  (m s-1)
      ALLOCATE(int_state%VBW(1:LNSV,JMS:JME,1:LM,1:2))    ;int_state%VBW  = R4_IN  !<-- V wind component at western boundary  (m s-1)
!
      ALLOCATE(int_state%PDBN(IMS:IME,1:LNSH,1:2))        ;int_state%PDBN = R4_IN  !<-- Pressure difference at northern boundary  (Pa)
      ALLOCATE(int_state%PDBS(IMS:IME,1:LNSH,1:2))        ;int_state%PDBS = R4_IN  !<-- Pressure difference at southern boundary  (Pa)
!
      ALLOCATE(int_state%PDBE(1:LNSH,JMS:JME,1:2))        ;int_state%PDBE = R4_IN  !<-- Pressure difference at eastern boundary  (Pa)
      ALLOCATE(int_state%PDBW(1:LNSH,JMS:JME,1:2))        ;int_state%PDBW = R4_IN  !<-- Pressure difference at western boundary  (Pa)
!
      ALLOCATE(int_state%QBN(IMS:IME,1:LNSH,1:LM,1:2))    ;int_state%QBN  = R4_IN  !<-- Specific humidity at northern boundary  (kg kg-1)
      ALLOCATE(int_state%QBS(IMS:IME,1:LNSH,1:LM,1:2))    ;int_state%QBS  = R4_IN  !<-- Specific humidity at southern boundary  (kg kg-1)
      ALLOCATE(int_state%TBN(IMS:IME,1:LNSH,1:LM,1:2))    ;int_state%TBN  = R4_IN  !<-- Temperature at northern boundary  (K)
      ALLOCATE(int_state%TBS(IMS:IME,1:LNSH,1:LM,1:2))    ;int_state%TBS  = R4_IN  !<-- Temperature at southern boundary  (K)
      ALLOCATE(int_state%WBN(IMS:IME,1:LNSH,1:LM,1:2))    ;int_state%WBN  = R4_IN  !<-- Vertical velocity at northern boundary  (m s-1)
      ALLOCATE(int_state%WBS(IMS:IME,1:LNSH,1:LM,1:2))    ;int_state%WBS  = R4_IN  !<-- Vertical velocity at southern boundary  (m s-1)
!
      ALLOCATE(int_state%QBE(1:LNSH,JMS:JME,1:LM,1:2))    ;int_state%QBE  = R4_IN  !<-- Specific humidity at eastern boundary  (kg kg-1)
      ALLOCATE(int_state%QBW(1:LNSH,JMS:JME,1:LM,1:2))    ;int_state%QBW  = R4_IN  !<-- Specific humidity at western boundary  (kg kg-1)
      ALLOCATE(int_state%TBE(1:LNSH,JMS:JME,1:LM,1:2))    ;int_state%TBE  = R4_IN  !<-- Temperature at eastern boundary  (K)
      ALLOCATE(int_state%TBW(1:LNSH,JMS:JME,1:LM,1:2))    ;int_state%TBW  = R4_IN  !<-- Temperature at western boundary  (K)
      ALLOCATE(int_state%WBE(1:LNSH,JMS:JME,1:LM,1:2))    ;int_state%WBE  = R4_IN  !<-- Vertical velocity at eastern boundary  (m s-1)
      ALLOCATE(int_state%WBW(1:LNSH,JMS:JME,1:LM,1:2))    ;int_state%WBW  = R4_IN  !<-- Vertical velocity at western boundary  (m s-1)
!
!-----------------------------------------------------------------------
!***  Atmospheric variables, hydrostatic (mostly)
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%PSDT(IMS:IME,JMS:JME))           ;int_state%PSDT = R4_IN  !<-- Hydrostatic surface pressure tendency  (Pa s-1)
!
!-----------------------------------------------------------------------
!***  The TRACERS array holds all general "tracer" variables including
!***  water.  Place the desired variables at the top of the TRACERS
!***  array, level 1 through NUM_TRACERS_MET.  All other scalar variables
!***  (e.g., chemistry and aerosols) will follow.
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%TRACERS_SQRT(IMS:IME,JMS:JME,1:LM,1:int_state%NUM_TRACERS_TOTAL))  ;int_state%TRACERS_SQRT = R4_IN  !<-- Sqrt of tracers (for advection)
      ALLOCATE(int_state%TRACERS_TEND(IMS:IME,JMS:JME,1:LM,1:int_state%NUM_TRACERS_TOTAL))  ;int_state%TRACERS_TEND = R4_IN  !<-- Tendency of tracers (for advection)
!
!-----------------------------------------------------------------------
!***  Atmospheric variables, nonhydrostatic
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%PDWDT(IMS:IME,JMS:JME,1:LM))     ;int_state%PDWDT = R4_IN  !<-- Correction factor, previous step  (m s-2)
!
!-----------------------------------------------------------------------
!***  Working arrays passed as arguments between subroutines.
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%DEF (IMS:IME,JMS:JME,1:LM))      ;int_state%DEF    = R4_IN  !<-- Horizontal flow deformation
      ALLOCATE(int_state%PCNE(IMS:IME,JMS:JME,1:LM))      ;int_state%PCNE   = R4_IN  !<-- 2nd term of pgf, NE direction
      ALLOCATE(int_state%PCNW(IMS:IME,JMS:JME,1:LM))      ;int_state%PCNW   = R4_IN  !<-- 2nd term of pgf, NW direction
      ALLOCATE(int_state%PCX (IMS:IME,JMS:JME,1:LM))      ;int_state%PCX    = R4_IN  !<-- 2nd term of pgf, X direction
      ALLOCATE(int_state%PCY (IMS:IME,JMS:JME,1:LM))      ;int_state%PCY    = R4_IN  !<-- 2nd term of pgf, Y direction
      ALLOCATE(int_state%PFNE(IMS:IME,JMS:JME,1:LM))      ;int_state%PFNE   = R4_IN  !<-- Mass flux, NE direction
      ALLOCATE(int_state%PFNW(IMS:IME,JMS:JME,1:LM))      ;int_state%PFNW   = R4_IN  !<-- Mass flux, NW direction
      ALLOCATE(int_state%PFX (IMS:IME,JMS:JME,1:LM))      ;int_state%PFX    = R4_IN  !<-- Mass flux, X direction
      ALLOCATE(int_state%PFY (IMS:IME,JMS:JME,1:LM))      ;int_state%PFY    = R4_IN  !<-- Mass flux, Y direction
      ALLOCATE(int_state%TDIV(IMS:IME,JMS:JME,1:LM))      ;int_state%TDIV   = R4_IN  !<-- Integrated horizontal mass divergence
!
!-----------------------------------------------------------------------
!***  FFT arrays
!-----------------------------------------------------------------------
!
      ALLOCATE(INT_STATE%CRAUX1(1:25000))                 ;int_state%CRAUX1 = R8_IN  !<-- FFT working field
      ALLOCATE(INT_STATE%CRAUX2(1:20000))                 ;int_state%CRAUX2 = R8_IN  !<-- FFT working field
      ALLOCATE(INT_STATE%CRAUX3(1:1    ))                 ;int_state%CRAUX3 = R8_IN  !<-- FFT working field
      ALLOCATE(INT_STATE%RCAUX1(1:25000))                 ;int_state%RCAUX1 = R8_IN  !<-- FFT working field
      ALLOCATE(INT_STATE%RCAUX2(1:20000))                 ;int_state%RCAUX2 = R8_IN  !<-- FFT working field
      ALLOCATE(INT_STATE%RCAUX3(1:1    ))                 ;int_state%RCAUX3 = R8_IN  !<-- FFT working field
!
!-----------------------------------------------------------------------
!***  Prognostic arrays
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%DUDT(IMS:IME,JMS:JME,1:LM))       ;int_state%DUDT     = R4_IN ! U wind component tendency  (m s-2)
      ALLOCATE(int_state%DVDT(IMS:IME,JMS:JME,1:LM))       ;int_state%DVDT     = R4_IN ! V wind component tendency  (m s-2)
!
      ALLOCATE(int_state%W0AVG(IMS:IME,1:LM+1,JMS:JME))    ;int_state%W0AVG    = R4_IN ! Time-averaged vertical velocity (for K-F)  (m s-1)
!
      ALLOCATE(int_state%PPTDAT(IMS:IME,JMS:JME,1:int_state%PCPHR)) ;int_state%PPTDAT = R4_IN 
!
!-----------------------------------------------------------------------
!*** GFS microphysics, wang, 11-22-2010
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%TP1(IMS:IME,JMS:JME,1:LM))
      ALLOCATE(int_state%QP1(IMS:IME,JMS:JME,1:LM))
      ALLOCATE(int_state%PSP1(IMS:IME,JMS:JME))
      DO I=IMS,IME
      DO J=JMS,JME
        int_state%PSP1(I,J) = -999.0
        DO L=1,LM
         int_state%TP1(I,J,L) = -999.0
         int_state%QP1(I,J,L) = -999.0
        ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  Only for GFS physics
!-----------------------------------------------------------------------
!
      IF ( int_state%GFS ) THEN
        REWIND (KOZPL)
        READ (KOZPL) PL_COEFF, LATSOZP, LEVOZP, TIMEOZ
        ALLOCATE(int_state%OZPLIN(LATSOZP,LEVOZP,PL_COEFF,TIMEOZ)) ;int_state%OZPLIN = R8_IN 
      ENDIF
!
!-----------------------------------------------------------------------
!
      ALLOCATE(int_state%LPBL(IMS:IME,JMS:JME))   ;int_state%LPBL   = I4_IN ! Model layer containing top of the PBL
      ALLOCATE(int_state%DDATA(IMS:IME,JMS:JME))  ;int_state%DDATA  = R4_IN ! Observed precip to each physics timestep (kg m-2)
      ALLOCATE(int_state%MAVAIL(IMS:IME,JMS:JME)) ;int_state%MAVAIL = R4_IN ! Moisture availability
      ALLOCATE(int_state%QCG(IMS:IME,JMS:JME))    ;int_state%QCG    = R4_IN ! Cloud water mixing ratio at the surface  (kg kg-1)
      ALLOCATE(int_state%QSG(IMS:IME,JMS:JME))    ;int_state%QSG    = R4_IN ! Surface saturation water vapor mixing ratio  (kg kg-1)
      ALLOCATE(int_state%QVG(IMS:IME,JMS:JME))    ;int_state%QVG    = R4_IN ! Water vapor mixing ratio at the surface  (kg kg-1)
      ALLOCATE(int_state%SHDMAX(IMS:IME,JMS:JME)) ;int_state%SHDMAX = R4_IN ! Maximum areal fractional coverage of annual green vegetation
      ALLOCATE(int_state%SHDMIN(IMS:IME,JMS:JME)) ;int_state%SHDMIN = R4_IN ! Minimum areal fractional coverage of annual green vegetation
      ALLOCATE(int_state%SOILT1(IMS:IME,JMS:JME)) ;int_state%SOILT1 = R4_IN ! Snow temperature  (K)
      ALLOCATE(int_state%STDH(IMS:IME,JMS:JME))   ;int_state%STDH   = R4_IN ! Standard deviation of topography height (m) !zj
      ALLOCATE(int_state%TSNAV(IMS:IME,JMS:JME))  ;int_state%TSNAV  = R4_IN ! Average snow temperature  (K)
      ALLOCATE(int_state%CROT(IMS:IME,JMS:JME))   ;int_state%CROT   = R4_IN ! Cosine of the angle between Earth and model coordinates
      ALLOCATE(int_state%SROT(IMS:IME,JMS:JME))   ;int_state%SROT   = R4_IN ! Sine of the angle between Earth and model coordinates
      ALLOCATE(int_state%HSTDV(IMS:IME,JMS:JME))  ;int_state%HSTDV  = R4_IN ! Standard deviation of the height (m)
      ALLOCATE(int_state%HCNVX(IMS:IME,JMS:JME))  ;int_state%HCNVX  = R4_IN ! Orographic convexity
      ALLOCATE(int_state%HASYW(IMS:IME,JMS:JME))  ;int_state%HASYW  = R4_IN ! Orographic asymmetry, west wind direction
      ALLOCATE(int_state%HASYS(IMS:IME,JMS:JME))  ;int_state%HASYS  = R4_IN ! Orographic asymmetry, south wind direction
      ALLOCATE(int_state%HASYSW(IMS:IME,JMS:JME)) ;int_state%HASYSW = R4_IN ! Orographic asymmetry, southwest wind direction
      ALLOCATE(int_state%HASYNW(IMS:IME,JMS:JME)) ;int_state%HASYNW = R4_IN ! Orographic asymmetry, northwest wind direction
      ALLOCATE(int_state%HLENW(IMS:IME,JMS:JME))  ;int_state%HLENW  = R4_IN ! Orographic length scale, west wind direction
      ALLOCATE(int_state%HLENS(IMS:IME,JMS:JME))  ;int_state%HLENS  = R4_IN ! Orographic length scale, south wind direction
      ALLOCATE(int_state%HLENSW(IMS:IME,JMS:JME)) ;int_state%HLENSW = R4_IN ! Orographic length scale, southwest wind direction
      ALLOCATE(int_state%HLENNW(IMS:IME,JMS:JME)) ;int_state%HLENNW = R4_IN ! Orographic length scale, northwest wind direction
      ALLOCATE(int_state%HANGL(IMS:IME,JMS:JME))  ;int_state%HANGL  = R4_IN ! Angle of mountain range with respect to east
      ALLOCATE(int_state%HANIS(IMS:IME,JMS:JME))  ;int_state%HANIS  = R4_IN ! Anisotropy/aspect ratio
      ALLOCATE(int_state%HSLOP(IMS:IME,JMS:JME))  ;int_state%HSLOP  = R4_IN ! Slope of orography
      ALLOCATE(int_state%HZMAX(IMS:IME,JMS:JME))  ;int_state%HZMAX  = R4_IN ! Maximum height about mean terrain
      ALLOCATE(int_state%Q02(IMS:IME,JMS:JME))    ;int_state%Q02    = R4_IN ! Specific humidity at 2-m  (kg k-1)
      ALLOCATE(int_state%TH02(IMS:IME,JMS:JME))   ;int_state%TH02   = R4_IN ! Theta at 2-m  (K)
!
!-----------------------------------------------------------------------
!***  GFS physics
!-----------------------------------------------------------------------
!
      gfs_physics: IF(int_state%GFS)THEN
!
        ALLOCATE(int_state%DDY              (JTS:JTE))    ;int_state%DDY    = R8_IN     !
        ALLOCATE(int_state%JINDX1           (JTS:JTE))    ;int_state%JINDX1 = I4_IN     !
        ALLOCATE(int_state%JINDX2           (JTS:JTE))    ;int_state%JINDX2 = I4_IN     !
!
        ALLOCATE(int_state%DUGWD    (IMS:IME,JMS:JME))    ;int_state%DUGWD  = R8_IN     ! U comp. GWD tend (m s-1)
        ALLOCATE(int_state%DVGWD    (IMS:IME,JMS:JME))    ;int_state%DVGWD  = R8_IN     ! V comp. GWD tend (m s-1)
!
        ALLOCATE(int_state%TMPMIN   (IMS:IME,JMS:JME))    ;int_state%TMPMIN = R8_IN     ! Max temp (K)
        ALLOCATE(int_state%TMPMAX   (IMS:IME,JMS:JME))    ;int_state%TMPMAX = R8_IN     ! Min temp (K)
!
        ALLOCATE(int_state%SFALB    (IMS:IME,JMS:JME))    ;int_state%SFALB  = R8_IN     !
        ALLOCATE(int_state%TSFLW    (IMS:IME,JMS:JME))    ;int_state%TSFLW  = R8_IN     !
        ALLOCATE(int_state%SEMIS    (IMS:IME,JMS:JME))    ;int_state%SEMIS  = R8_IN     !
        ALLOCATE(int_state%SFCDLW   (IMS:IME,JMS:JME))    ;int_state%SFCDLW = R8_IN     !
        ALLOCATE(int_state%SFCDSW   (IMS:IME,JMS:JME))    ;int_state%SFCDSW = R8_IN     !
        ALLOCATE(int_state%SFCNSW   (IMS:IME,JMS:JME))    ;int_state%SFCNSW = R8_IN     !
!
        ALLOCATE(int_state%ZORFCS   (IMS:IME,JMS:JME))    ;int_state%ZORFCS = R8_IN     !
        ALLOCATE(int_state%SIHFCS   (IMS:IME,JMS:JME))    ;int_state%SIHFCS = R8_IN     !
        ALLOCATE(int_state%SICFCS   (IMS:IME,JMS:JME))    ;int_state%SICFCS = R8_IN     !
        ALLOCATE(int_state%SLPFCS   (IMS:IME,JMS:JME))    ;int_state%SLPFCS = R8_IN     !
        ALLOCATE(int_state%TG3FCS   (IMS:IME,JMS:JME))    ;int_state%TG3FCS = R8_IN     !
        ALLOCATE(int_state%VEGFCS   (IMS:IME,JMS:JME))    ;int_state%VEGFCS = R8_IN     !
        ALLOCATE(int_state%VETFCS   (IMS:IME,JMS:JME))    ;int_state%VETFCS = R8_IN     !
        ALLOCATE(int_state%SOTFCS   (IMS:IME,JMS:JME))    ;int_state%SOTFCS = R8_IN     !
!
        ALLOCATE(int_state%ALBFC1   (IMS:IME,JMS:JME,4))  ;int_state%ALBFC1 = R8_IN     !
        ALLOCATE(int_state%ALFFC1   (IMS:IME,JMS:JME,2))  ;int_state%ALFFC1 = R8_IN     !
!
        ALLOCATE(int_state%PHY_F2DV (IMS:IME,JMS:JME,3))    ;int_state%PHY_F2DV = R8_IN ! for Zhao =3, Ferr=1
        ALLOCATE(int_state%PHY_F3DV (IMS:IME,JMS:JME,LM,4)) ;int_state%PHY_F3DV = R8_IN ! for Zhao =4, Ferr=3
!
      ENDIF  gfs_physics
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SET_INTERNAL_STATE_SOLVER
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      END MODULE MODULE_SOLVER_INTERNAL_STATE
!
!-----------------------------------------------------------------------
