!-----------------------------------------------------------------------
!
!NCEP_MESO:MEDIATION_LAYER:SOLVER
!
!-----------------------------------------------------------------------
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!-----------------------------------------------------------------------
!
      SUBROUTINE SOLVE_NMM(GRID,CONFIG_FLAGS                            &
!
#include "dummy_args.inc"
!
     &           )
!-----------------------------------------------------------------------
      USE MODULE_DOMAIN,                ONLY : DOMAIN, GET_IJK_FROM_GRID
      USE MODULE_CONFIGURE,             ONLY : GRID_CONFIG_REC_TYPE
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_STATE_DESCRIPTION
      USE MODULE_CTLBLK
!#ifdef DM_PARALLEL
!      USE MODULE_DM
!#endif
      USE MODULE_IGWAVE_ADJUST,         ONLY: PDTE,PFDHT,DDAMP,VTOA
      USE MODULE_ADVECTION,             ONLY: ADVE,VAD2,HAD2,VAD2_SCAL,HAD2_SCAL &
!zj-passive substance variables-wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
                                             ,adv2,mono 
!mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

      USE MODULE_NONHY_DYNAM,           ONLY: EPS,VADZ,HADZ
      USE MODULE_DIFFUSION_NMM,         ONLY: HDIFF
      USE MODULE_BNDRY_COND,            ONLY: BOCOH,BOCOV
      USE MODULE_PHYSICS_CALLS
      USE MODULE_EXT_INTERNAL
      USE MODULE_PRECIP_ADJUST
      USE MODULE_NEST_UTIL     ! USEs module_MPP (contains MYPE,NPES,MPI_COMM_COMP)
#ifdef WRF_CHEM
      USE MODULE_INPUT_CHEM_DATA,       ONLY: GET_LAST_GAS
#endif
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!***  INPUT DATA
!
!-----------------------------------------------------------------------
!
      TYPE(DOMAIN),TARGET :: GRID
!
!***  DEFINITIONS OF DUMMY ARGUMENTS TO THIS ROUTINE (GENERATED FROM REGISTRY)
!
! NOTE, REGISTRY NO LONGER GENERATES DUMMY ARGUMENTS OR DUMMY ARGUMENT
! DECLARATIONS FOR RCONFIG ENTRIES. THEY ARE STILL PART OF STATE. ACCESS
! TO THESE VARIABLES IS NOW THROUGH GRID STRUCTURE, AS MODIFIED BELOW.
! AFFECTED VARIABLES: SIGMA, DT, NPHS, IDTAD, NRADS, NRADL, JULDAY,
! JULYR, NUM_SOIL_LAYERS, NCNVC, ENSDIM, DY, AND SPEC_BDY_WIDTH.
! JM, 20050819
!
!----------------------------
#include <dummy_decl.inc>
!----------------------------
!
!***  STRUCTURE THAT CONTAINS RUN-TIME CONFIGURATION (NAMELIST) DATA FOR DOMAIN
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
#ifdef WRF_CHEM
      INTEGER :: NUMGAS
#endif
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
!zj-passive substance variables-wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

      LOGICAL :: euler
      INTEGER :: idtadt
      INTEGER :: idtadc
integer,parameter:: &
 kss=1 &                     ! starting species index in 4d tracer array
,kse=4                       ! ending species index in 4d tracer array

integer:: &
  ks                         ! species index in 4d tracer array
!zjmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm


      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                & 
     &          ,IPS,IPE,JPS,JPE,KPS,KPE                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER :: I,ICLTEND,IDF,IRTN,J,JC,JDF,K,KDF,LB,N_MOIST &
     &          ,NTSD_current,L
      integer :: ierr
      INTEGER,SAVE :: NTSD_restart
!     INTEGER :: MPI_COMM_COMP,MYPE,MYPROC,NPES
      INTEGER :: MYPROC
      INTEGER :: KVH,NTSD_rad,RC
      INTEGER :: NUM_OZMIXM,NUM_AEROSOLC
!
      REAL :: DT_INV,FICE,FRAIN,GPS,QI,QR,QW,WC,SFENTH
!
      LOGICAL :: LAST_TIME,OPERATIONAL_PHYSICS, micro_has_been_read
!
      CHARACTER(80) :: MESSAGE
!
!***  For precip assimilation:
      INTEGER :: ISTAT
      REAL,ALLOCATABLE,SAVE,DIMENSION(:,:,:) :: PPTDAT
#ifdef WRF_CHEM
      REAL,ALLOCATABLE,DIMENSION(:,:,:,:) :: CHEM_TRANS
#endif
!
!-----------------------------------------------------------------------
!***  For physics compatibility with other packages
!-----------------------------------------------------------------------
!
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: TTEN,QTEN
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: RTHRATEN,RTHBLTEN,RQVBLTEN

      REAL,ALLOCATABLE,DIMENSION(:,:)      ::  upindx
!
!-----------------------------------------------------------------------
!
      LOGICAL wrf_dm_on_monitor
      EXTERNAL wrf_dm_on_monitor
!
!-----------------------------------------------------------------------
!***  TIMING VARIABLES
!-----------------------------------------------------------------------
      real,save :: solve_tim,exch_tim,pdte_tim,adve_tim,vtoa_tim        &
     &,            vadz_tim,hadz_tim,eps_tim,vad2_tim,had2_tim          &
     &,            radiation_tim,rdtemp_tim,turbl_tim,cltend_tim        &
     &,            cucnvc_tim,gsmdrive_tim,hdiff_tim,bocoh_tim          &
     &,            pfdht_tim,ddamp_tim,bocov_tim,uv_htov_tim,sum_tim    &
     &,            adjppt_tim
      real,save :: exch_tim_max
      real :: btim,btimx
      real :: et_max,this_tim,magw,magw2,pup,plow
      real :: P1D, T1D, Q1D, C1D, FI1D, FR1D, FS1D, CUREFL, DBZ1
      real :: P1Da, T1Da, Q1Da, C1Da, FI1Da, FR1Da, FS1Da
      real :: P1Db, T1Db, Q1Db, C1Db, FI1Db, FR1Db, FS1Db
      real :: wgta, wgtb, ZMID, ZMIDP1
      integer :: n_print_time

!microphysics
      REAL, PARAMETER :: DMRmin=.05e-3, DMRmax=1.e-3,           &
     &      DelDMR=1.e-6,XMRmin=1.e6*DMRmin, XMRmax=1.e6*DMRmax
      INTEGER, PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax
      REAL, save, DIMENSION(MDRmin:MDRmax)::    MASSR

!      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3,           &
!     &      DelDMI=1.e-6,XMImin=1.e6*DMImin
!      INTEGER, PARAMETER :: XMImax=1.e6*DMImax, XMIexp=.0536,    &
!     &                             MDImin=XMImin, MDImax=XMImax

      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3,           &
     &      DelDMI=1.e-6,XMImin=1.e6*DMImin

!      REAL, DIMENSION(MDImin:MDImax) :: MASSI

      REAL,save::  C_N0r0,           &
     &  CN0r0, CN0r_DMRmin, CN0r_DMRmax,      &
     &  RQR_DRmin, RQR_DRmax
!
!microphysics

!
#ifdef RSL
      integer rsl_internal_milliclock
      external rsl_internal_milliclock
# define timef rsl_internal_milliclock
#else
      real*8 :: timef
#endif
!-----------------------------------------------------------------------
!
#ifdef DEREF_KLUDGE
! SEE http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
      INTEGER :: SM31,EM31,SM32,EM32,SM33,EM33 
      INTEGER :: SM31X,EM31X,SM32X,EM32X,SM33X,EM33X
      INTEGER :: SM31Y,EM31Y,SM32Y,EM32Y,SM33Y,EM33Y
#endif

!zj-passive substance variables-wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
real,save:: &
 sumdrrw

!inregistry real,allocatable,save,dimension(:,:,:):: & ! i,j,k
!inregistry  rrw                         ! a tracer used for tests

real,allocatable,save,dimension(:,:,:,:):: & ! i,j,k,ks
 szj &                       ! 4d array with tracers
,s1z &                       ! intermediate tracer variable
,spz &                       ! s1z at previous time level
,tcs                         ! time change of tracers

!zj chemistry NOT TESTED !
#ifdef WRF_CHEM
real,allocatable,save,dimension(:,:,:,:):: & ! i,j,k,ks
 che &                       ! 4d i,j,k chem tracers
,ch1 &                       ! intermediate tracer variable
,chp &                       ! ch1 at previous time level
,tcc                         ! time change of tracers
#endif
!zj-mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

!
!-----------------------------------------------------------------------
!
! LIMIT THE NUMBER OF ARGUMENTS IF COMPILED WITH -DLIMIT_ARGS BY COPYING
! SCALAR (NON-ARRAY) ARGUMENTS OUT OF THE GRID DATA STRUCTURE INTO LOCALLY
! DEFINED COPIES (DEFINED IN EM_DUMMY_DECL.INC, ABOVE, AS THEY ARE IF THEY
! ARE ARGUMENTS).  AN EQUIVALENT INCLUDE OF EM_SCALAR_DEREFS.INC APPEARS
! AT THE END OF THE ROUTINE TO COPY BACK ANY CHNAGED NON-ARRAY VALUES.
! THE DEFINITION OF COPY_IN OR COPY_OUT BEFORE THE INCLUDE DEFINES THE
! DIRECTION OF THE COPY.  NMM_SCALAR_DEREFS IS GENERATED FROM REGISTRY.
!
!-----------------------------------------------------------------------
#define COPY_IN
#include <scalar_derefs.inc>
!-----------------------------------------------------------------------
!
! TRICK PROBLEMATIC COMPILERS INTO NOT PERFORMING COPY-IN/COPY-OUT BY ADDING
! INDICES TO ARRAY ARGUMENTS IN THE CALL STATEMENTS IN THIS ROUTINE.
! IT HAS THE EFFECT OF PASSING ONLY THE FIRST ELEMENT OF THE ARRAY, RATHER
! THAN THE ENTIRE ARRAY.  SEE:
! http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
!
!-----------------------------------------------------------------------
#include "deref_kludge.h"
!-----------------------------------------------------------------------
!
! NEEDED BY SOME COMM LAYERS, E.G. RSL.  IF NEEDED, nmm_data_calls.inc IS
! GENERATED FROM THE REGISTRY.  THE DEFINITION OF REGISTER_I1 ALLOWS
! I1 DATA TO BE COMMUNICATED IN THIS ROUTINE IF NECESSARY.
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***********************************************************************
!***
!***               THE MAIN TIME INTEGRATION LOOP
!***
!-----------------------------------------------------------------------
!
!***  NTSD IS THE TIMESTEP COUNTER (Number of Time Steps Done)
!
!-----------------------------------------------------------------------
!***
!***  ADVANCE_count STARTS AT ZERO FOR ALL RUNS (REGULAR AND RESTART).
!***
!-----------------------------------------------------------------------
!
      CALL DOMAIN_CLOCK_GET(GRID,ADVANCEcOUNT=NTSD_current)

!	write(0,*) 'NTSD_current: ', NTSD_current
!
      IF(NTSD_current==0)THEN
!	write(0,*) 'NTSD_current=0 branch'
        micro_has_been_read=.false.
        IF(GRID%RESTART.AND.GRID%TSTART>0.)THEN
          IHRST=NSTART_HOUR
          NTSD_restart=NTSD
        ELSEIF(GRID%TSTART>0 .and. model_config_rec%dfi_opt > 0) THEN
          IHRST=NSTART_HOUR
          NTSD_restart=NTSD
	write(0,*) 'GRID%TSTART in check: ', GRID%TSTART
	write(0,*) 'setting IHRST, NTSD_restart because of DFI', IHRST, NTSD_restart 
        ELSEIF(GRID%TSTART<0 .and. model_config_rec%dfi_opt > 0) THEN
          IHRST=NSTART_HOUR
          NTSD_restart=NTSD
	write(0,*) 'GRID%TSTART in check: ', GRID%TSTART
	write(0,*) 'setting IHRST, NTSD_restart because of DFI', IHRST, NTSD_restart 

        ELSE
          IHRST=GRID%GMT
          NSTART_HOUR=IHRST
          NTSD_restart=0
        ENDIF

      ELSE
!	write(0,*) 'NTSD_current not 0 branch'
        micro_has_been_read=.true.

      ENDIF

!	write(0,*) 'here with micro_has_been_read: ', micro_has_been_read
!
      NTSD=NTSD_restart+NTSD_current
      LAST_TIME=domain_last_time_step(GRID)
!
!-----------------------------------------------------------------------
!
!!!!! IF(WRF_DM_ON_MONITOR() )THEN
        WRITE(MESSAGE,125)NTSD,NTSD*GRID%DT/3600.
  125   FORMAT(' SOLVE_NMM: TIMESTEP IS ',I5,'   TIME IS ',F7.3,' HOURS')
        CALL WRF_MESSAGE(TRIM(MESSAGE))
!!!!  ENDIF
!
!-----------------------------------------------------------------------

      euler=model_config_rec%euler_adv
      idtadt=model_config_rec%idtadt
      idtadc=model_config_rec%idtadc

!        write(0,*)' euler, idtadt, idtadc: ',  euler, idtadt, idtadc

      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL WRF_GET_NPROC(NPES)
      CALL WRF_GET_MYPROC(MYPROC)
      MYPE=MYPROC
!-----------------------------------------------------------------------
!
!***  OBTAIN DIMENSION INFORMATION STORED IN THE GRID DATA STRUCTURE.
!
      CALL GET_IJK_FROM_GRID(GRID                                       &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,IPS,IPE,JPS,JPE,KPS,KPE )
!-----------------------------------------------------------------------
!
!***  COMPUTE THESE STARTING AND STOPPING LOCATIONS FOR EACH TILE AND
!***  NUMBER OF TILES.
!***  SEE: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
!
      CALL SET_TILES(GRID,IDS,IDE,JDS,JDE,IPS,IPE,JPS,JPE)
!
!-----------------------------------------------------------------------
!***  SET FLAG FOR THE OPERATIONAL PHYSICS SUITE.
!***  THIS WILL BE USED TO SAVE CLOCKTIME BY SKIPPING
!***  FREQUENT UPDATES OF THE MOIST ARRAY AND INSTEAD
!***  UPDATE IT ONLY WHEN IT IS NEEDED FOR PHYSICS.
!-----------------------------------------------------------------------
!
      OPERATIONAL_PHYSICS=.FALSE.
!
      IF(CONFIG_FLAGS%RA_SW_PHYSICS    ==GFDLSWSCHEME.AND.              &
     &   CONFIG_FLAGS%RA_LW_PHYSICS    ==GFDLLWSCHEME.AND.              &
     &   CONFIG_FLAGS%SF_SFCLAY_PHYSICS==MYJSFCSCHEME.AND.              &
     &   CONFIG_FLAGS%BL_PBL_PHYSICS   ==MYJPBLSCHEME.AND.              &
     &   CONFIG_FLAGS%CU_PHYSICS       ==BMJSCHEME.AND.                 &
     &   CONFIG_FLAGS%MP_PHYSICS       ==ETAMPNEW)THEN
!
        OPERATIONAL_PHYSICS=.TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!***  TTEN, QTEN are used by GD convection scheme
!
      ALLOCATE(TTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(QTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RQVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHRATEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
#ifdef WRF_CHEM
  ALLOCATE(CHEM_TRANS(IMS:IME,JMS:JME,KMS:KME,1:NUM_CHEM),STAT=ISTAT)
  NUMGAS          = GET_LAST_GAS(CONFIG_FLAGS%CHEM_OPT)
#endif
!
      IF(CONFIG_FLAGS%CU_PHYSICS==GDSCHEME)THEN
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          TTEN(I,K,J)=T(I,J,K)
          QTEN(I,K,J)=Q(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
      GRID%SIGMA=1 
      HYDRO=.FALSE.
!
!
      IDF=IDE-1
      JDF=JDE-1
      KDF=KDE-1
!
!-----------------------------------------------------------------------
!
!***  FOR NOW SET CONTROLS FOR TILES TO PATCHES
!
!-----------------------------------------------------------------------
      ITS=IPS
      ITE=MIN(IPE,IDF)
      JTS=JPS
      JTE=MIN(JPE,JDF)
      KTS=KPS
      KTE=MIN(KPE,KDF)
      if(ntsd==0)then
        write(message,*)' its=',its,' ite=',ite
        call wrf_message(trim(message))
        write(message,*)' jts=',jts,' jte=',jte
        call wrf_message(trim(message))
        write(message,*)' kts=',kts,' kte=',kte
        call wrf_message(trim(message))
!zj-passive substance variables-wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
if(euler) then
  !inregistry allocate (rrw(ims:ime,jms:jme,kms:kme        ),stat=istat)
  allocate (szj(ims:ime,jms:jme,kms:kme,kss:kse),stat=istat)
  allocate (s1z(ims:ime,jms:jme,kms:kme,kss:kse),stat=istat)
  allocate (spz(ims:ime,jms:jme,kms:kme,kss:kse),stat=istat)
  allocate (tcs(ims:ime,jms:jme,kms:kme,kss:kse),stat=istat)
endif
!zj chemistry NOT TESTED !
#ifdef WRF_CHEM
allocate (che(ims:ime,jms:jme,kms:kme,1:NUM_CHEM),stat=istat)
allocate (ch1(ims:ime,jms:jme,kms:kme,1:NUM_CHEM),stat=istat)
allocate (chp(ims:ime,jms:jme,kms:kme,1:NUM_CHEM),stat=istat)
allocate (tcc(ims:ime,jms:jme,kms:kme,1:NUM_CHEM),stat=istat)
#endif
!zj-mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
      endif


!-----------------------------------------------------------------------
!***  SET TIMING VARIABLES TO ZERO AT START OF FORECAST.
!-----------------------------------------------------------------------
      if(ntsd==0)then
        solve_tim=0.
        exch_tim=0.
        pdte_tim=0.
        adve_tim=0.
        vtoa_tim=0.
        vadz_tim=0.
        hadz_tim=0.
        eps_tim=0.
        vad2_tim=0.
        had2_tim=0.
        radiation_tim=0.
        rdtemp_tim=0.
        turbl_tim=0.
        cltend_tim=0.
        cucnvc_tim=0.
        gsmdrive_tim=0.
        hdiff_tim=0.
        bocoh_tim=0.
        pfdht_tim=0.
        ddamp_tim=0.
        bocov_tim=0.
        uv_htov_tim=0.
        exch_tim_max=0.
        adjppt_tim=0.
      endif
!-----------------------------------------------------------------------
      N_MOIST=NUM_MOIST
!
      DO J=MYJS_P4,MYJE_P4
        IHEG(J)=MOD(J+1,2)
        IHWG(J)=IHEG(J)-1
        IVEG(J)=MOD(J,2)
        IVWG(J)=IVEG(J)-1
      ENDDO

      DO J=MYJS_P4,MYJE_P4
        IVW(J)=IVWG(J)
        IVE(J)=IVEG(J)
        IHE(J)=IHEG(J)
        IHW(J)=IHWG(J)
      ENDDO
!
!***  LATERAL POINTS IN THE BOUNDARY ARRAYS
!
      LB=2*(IDF-IDS+1)+(JDF-JDS+1)-3
!
!***  APPROXIMATE GRIDPOINT SPACING (METERS)
!
      JC=JMS+(JME-JMS)/2
      GPS=SQRT(DX_NMM(IMS,JC)**2+DY_NMM**2)
!
!***  TIMESTEPS PER HOUR
!
      TSPH=3600./GRID%DT
!
      n_print_time=nint(3600./grid%dt)   ! Print stats once per hour
!-----------------------------------------------------------------------
!
      NBOCO=0
!
!-----------------------------------------------------------------------
!
#if (NMM_NEST == 1)
!-----------------------------------------------------------------------------
!***  PATCHING NESTED BOUNDARIES.
!-----------------------------------------------------------------------------
!
      CALL wrf_debug ( 100 , 'nmm: in patch' )

      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_ZZ.inc"
#endif

      IF(GRID%ID/=1)THEN
!
        CALL NESTBC_PATCH (PD_BXS,PD_BXE,PD_BYS,PD_BYE,T_BXS,T_BXE,T_BYS,T_BYE,Q_BXS,Q_BXE         &
                          ,Q_BYS,Q_BYE,U_BXS,U_BXE,U_BYS,U_BYE,V_BXS,V_BXE,V_BYS,V_BYE             &
                          ,Q2_BXS,Q2_BXE,Q2_BYS,Q2_BYE,CWM_BXS,CWM_BXE,CWM_BYS,CWM_BYE             &
                          ,PD_BTXS,PD_BTXE,PD_BTYS,PD_BTYE,T_BTXS,T_BTXE,T_BTYS,T_BTYE             &
                          ,Q_BTXS,Q_BTXE,Q_BTYS,Q_BTYE,U_BTXS,U_BTXE,U_BTYS,U_BTYE                 &
                          ,V_BTXS,V_BTXE,V_BTYS,V_BTYE,Q2_BTXS,Q2_BTXE,Q2_BTYS,Q2_BTYE             &
                          ,CWM_BTXS,CWM_BTXE,CWM_BTYS,CWM_BTYE,PDNEST_B,TNEST_B,QNEST_B,UNEST_B    &
                          ,VNEST_B,Q2NEST_B,CWMNEST_B,PDNEST_BT,TNEST_BT,QNEST_BT                  &
                          ,UNEST_BT,VNEST_BT,Q2NEST_BT,CWMNEST_BT                                  &
                          ,GRID%SPEC_BDY_WIDTH                                                     &
                          ,IDS,IDF,JDS,JDF,KDS,KDE                                                 &
                          ,IMS,IME,JMS,JME,KMS,KME                                                 &
                          ,ITS,ITE,JTS,JTE,KTS,KTE                                                 )

        CALL wrf_debug ( 100 , 'nmm: out of patch' )
!
#ifdef MOVE_NESTS

        IF(GRID%ID/=1.AND.MOD(NTSD,1)==0.AND.GRID%NUM_MOVES==-99)THEN
          XLOC_1=(IDE-1)/2     ! This maneuvers the storm to the center of the nest quickly
          YLOC_1=(JDE-1)/2     ! This maneuvers the storm to the center of the nest quickly
        ENDIF
#endif

      ENDIF
#endif
!
!-----------------------------------------------------------------------
!***  ALLOCATE PPTDAT ARRAY (PRECIP ASSIM):
!-----------------------------------------------------------------------
!
      IF(GRID%PCPFLG.AND..NOT.ALLOCATED(PPTDAT))THEN
        ALLOCATE(PPTDAT(IMS:IME,JMS:JME,3),STAT=ISTAT)
      ENDIF
!
!-----------------------------------------------------------------------
!***
!***      Call READPCP to
!***            1) READ IN PRECIPITATION FOR HOURS 1, 2 and 3;
!***            2) Initialize DDATA to 999. (this is the amount
!***               of input precip allocated to each physics time step
!***               in ADJPPT; TURBL/SURFCE, which uses DDATA, is called
!***               before ADJPPT)
!***            3) Initialize LSPA to zero
!***
!-----------------------------------------------------------------------
      IF (NTSD==0) THEN
        IF (GRID%PCPFLG) THEN
          CALL READPCP(PPTDAT,DDATA,LSPA                                &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
!
      btim=timef()
!
!-----------------------------------------------------------------------
!***  ZERO OUT ACCUMULATED QUANTITIES WHEN NEEDED.
!-----------------------------------------------------------------------
!
      CALL BUCKETS(NTSD,NPREC,NSRFC,NRDSW,NRDLW                         &
     &            ,GRID%RESTART,GRID%TSTART                             &
     &            ,NCLOD,NHEAT,GRID%NPHS,TSPH                           &
     &            ,ACPREC,CUPREC,ACSNOW,ACSNOM,SSROFF,BGROFF            &
     &            ,SFCEVP,POTEVP,SFCSHX,SFCLHX,SUBSHX,SNOPCX            &
     &            ,SFCUVX,POTFLX                                        &
     &            ,ARDSW,ASWIN,ASWOUT,ASWTOA                            &
     &            ,ARDLW,ALWIN,ALWOUT,ALWTOA                            &
     &            ,ACFRST,NCFRST,ACFRCV,NCFRCV                          &
     &            ,AVCNVC,AVRAIN,TCUCN,TRAIN                            &
     &            ,ASRFC                                                &
     &            ,T,TLMAX,TLMIN,TSHLTR,PSHLTR,QSHLTR                   &
     &            ,T02_MAX,T02_MIN,RH02_MAX,RH02_MIN                    &
     &            ,IDS,IDE,JDS,JDE,KDS,KDE                              &
     &            ,IMS,IME,JMS,JME,KMS,KME                              &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      IF(NTSD==0)THEN
        FIRST=.TRUE.
!       call hpm_init()
        btimx=timef()

!zj-first step initialization of passive substance variables-wwwwwwwwwwww
      if(euler) then
        sumdrrw=0.
!
        DO K=KTS,KTE
          DO J=JMS,JME
            DO I=IMS,IME
              rrw(i,j,k)=0. !zj
              if(i.ge.ide/2-6.and.i.le.ide/2+6.and. &
                 j.ge.jde/2-6.and.j.le.jde/2+6 .and. K .eq. KTE/2) then
                rrw(i,j,k)=10.0 !youhua
!                rrw(i,j,k)=0.9 !zj
              endif
            enddo
          enddo
        enddo
        do ks=kss,kse
          DO K=KMS,KME
            DO J=JMS,JME
              DO I=IMS,IME
                szj(i,j,k,ks)=0.
                s1z(i,j,k,ks)=0.
                spz(i,j,k,ks)=0.
                tcs(i,j,k,ks)=0.
              enddo
            enddo
          enddo
        enddo
      endif
!zj chemistry NOT TESTED !
#ifdef WRF_CHEM
      do ks=1,NUM_CHEM
        DO K=KMS,KME
          DO J=JMS,JME
            DO I=IMS,IME
              che(i,j,k,ks)=0.
              ch1(i,j,k,ks)=0.
              chp(i,j,k,ks)=0.
              tcc(i,j,k,ks)=0.
            enddo
          enddo
        enddo
      enddo
#endif
!zj-mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

!
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
!
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
      IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
#    include "HALO_NMM_A_3.inc"
      ENDIF
#endif
!-----------------------------------------------------------------------

!zj-first step initialization of passive substance variables-wwwwwwwwwww
      if(euler) then
        DO K=KTS,KTE
          DO J=JMS,JME
            DO I=IMS,IME
              spz(i,j,k,1)=sqrt(max(q  (i,j,k),epsq))
              spz(i,j,k,2)=sqrt(max(cwm(i,j,k),epsq))
              spz(i,j,k,4)=sqrt(max(rrw(i,j,k),0.  ))
            enddo
          enddo
        enddo
        DO J=JMS,JME
          DO I=IMS,IME
            spz(i,j,kte,3)=sqrt(max((q2(i,j,kte)+epsq2)*0.5,epsq2))
          enddo
        enddo
        DO K=KTE-1,KTS,-1
          DO J=JMS,JME
            DO I=IMS,IME
              spz(i,j,k,3)=sqrt(max((q2(i,j,k)+q2(i,j,k+1))*0.5,epsq2))
            enddo
          enddo
        enddo
      endif
!zj chemistry NOT TESTED !
#ifdef WRF_CHEM
      do ks=1,Num_chem
        DO K=KMS,KME
          DO J=JMS,JME
            DO I=IMS,IME
              chp(i,j,k,ks)=sqrt(max(chem(i,k,j,ks),0.  ))
            enddo
          enddo
        enddo
      enddo
#endif
!zj-mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
!-----------------------------------------------------------------------
!
!***  Only for chemistry:
!
#ifdef WRF_CHEM   
#ifdef DM_PARALLEL
#    include "HALO_NMM_A_2.inc"
#endif
#endif
!
!-----------------------------------------------------------------------
!***  USE THE FOLLOWING VARIABLES TO KEEP TRACK OF EXCHANGE TIMES.
!-----------------------------------------------------------------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!-----------------------------------------------------------------------
!
        GO TO 2003
      ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 2000 CONTINUE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, SIGMA DOT, VERTICAL PART OF OMEGA-ALPHA
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_D.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!

      CALL PDTE(                                                        &
#ifdef DM_PARALLEL
     &            GRID,MYPE,MPI_COMM_COMP,                              &
#endif
     &            NTSD,GRID%DT,PT,ETA2,RES,HYDRO,HBM2                   &
     &           ,PD,PDSL,PDSLO                                         &
     &           ,PETDT,DIV,PSDT                                        &
     &           ,IHE,IHW,IVE,IVW                                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)


      pdte_tim=pdte_tim+timef()-btimx

!
!-----------------------------------------------------------------------
!***  ADVECTION OF T, U, AND V
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_F.inc"
#    include "HALO_NMM_F1.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
      btimx=timef()
!
      CALL ADVE(NTSD,GRID%DT,DETA1,DETA2,PDTOP                          &
     &         ,CURV,F,FAD,F4D,EM_LOC,EMT_LOC,EN,ENT,DX_NMM,DY_NMM      &
     &         ,HBM2,VBM2                                               &
     &         ,T,U,V,PDSLO,TOLD,UOLD,VOLD                              &
     &         ,PETDT,UPSTRM                                            &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,ADT,ADU,ADV                                             & 
     &         ,N_IUP_H,N_IUP_V                                         &
     &         ,N_IUP_ADH,N_IUP_ADV                                     &
     &         ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                             &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      adve_tim=adve_tim+timef()-btimx

!zj-passive substance working part-wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if(euler) then ! eulerian advection for model tracers
!
        if(config_flags%mp_physics.ne.etampnew .and. config_flags%dfi_opt .eq. 0) then
          write(0,*) 'Stopping.  Only etampnew implemented w. euler'
          stop
        endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!-----------------------------------------------------------------------
        if(mod(ntsd,idtadt).eq.0) then
          btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_I.inc"
#endif
          exch_tim=exch_tim+timef()-btimx
!
          btimx=timef()
!
          DO K=KTS,KTE
            DO J=JMS,JME
              DO I=IMS,IME
                szj(i,j,k,1)=max(q  (i,j,k),epsq)
                szj(i,j,k,2)=max(cwm(i,j,k),epsq)
                szj(i,j,k,4)=max(rrw(i,j,k),0.  )
              enddo
            enddo
          enddo
          DO J=JMS,JME
            DO I=IMS,IME
              szj(i,j,kte,3)=max((q2 (i,j,kte)+epsq2)*0.5,epsq2)
            enddo
          enddo
          DO K=KTE-1,KTS,-1
            DO J=JMS,JME
              DO I=IMS,IME
                szj(i,j,k,3)=max((q2 (i,j,k)+q2 (i,j,k+1))*0.5,epsq2)
              enddo
            enddo
          enddo
!
          call adv2 &
          (UPSTRM &
          ,mype,kss,kse &
          ,ids,ide,jds,jde,kds,kde &
          ,ims,ime,jms,jme,kms,kme &
          ,its,ite,jts,jte,kts,kte &
          ,N_IUP_H &
          ,N_IUP_ADH &
          ,IUP_H,IUP_ADH &
          ,ENT &
          ,idtadt &
          ,grid%dt,pdtop &
          ,ihe,ihw,ive,ivw &
          ,deta1,deta2 &
          ,EMT_LOC &
          ,fad,hbm2,pdsl,pdslo &
          ,petdt &
          ,UOLD,VOLD &
          ,szj,spz &
          !temporary arguments
          ,fne,fse,few,fns,s1z,tcs)
!
          call mono &
          ( &
#if defined(DM_PARALLEL)
           GRID%DOMDESC, &
#endif
           mype,ntsd,ntsd*grid%dt/3600.,kss,kse &
          ,ids,ide,jds,jde,kds,kde &
          ,ims,ime,jms,jme,kms,kme &
          ,its,ite,jts,jte,kts,kte &
          ,idtadt &
          ,dy_nmm,pdtop &
          ,sumdrrw &
          ,ihe,ihw &
          ,deta1,deta2 &
          ,dx_nmm,hbm2,pdsl &
          ,szj &
! temporary arguments
          ,s1z &
          ,tcs )
!
          do ks=kss,kse ! loop by species
            DO K=KTS,KTE
              DO J=MYJS2,MYJE2
                DO I=MYIS1,MYIE1
                  szj(i,j,k,ks)=szj(i,j,k,ks)+tcs(i,j,k,ks)
                enddo
              enddo
            enddo
          enddo ! end of the loop by the species
!
          DO K=KTS,KTE
            DO J=MYJS2,MYJE2
              DO I=MYIS1,MYIE1
                q  (i,j,k)=szj(i,j,k,1)
                cwm(i,j,k)=szj(i,j,k,2)
                rrw(i,j,k)=szj(i,j,k,4)
!	if (rrw(i,j,k) .gt. 1.) then
!	write(0,*) 'i,j,k,rrw(i,j,k): ', i,j,k,rrw(i,j,k)
!	endif
              enddo
            enddo
          enddo
!
          DO J=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              q2(i,j,kte)=max(szj(i,j,kte,3)+szj(i,j,kte,3)-epsq2 &
                             ,epsq2)
            enddo
          enddo
!
          DO K=KTE-1,KTS+1,-1
            DO J=MYJS2,MYJE2
              DO I=MYIS1,MYIE1
                if(k>kts)then
                  q2(i,j,k)=max(szj(i,j,k,3)+szj(i,j,k,3)-q2(i,j,k+1) &
                               ,epsq2)
                else
                  q2(i,j,k)=q2(i,j,k+1)
                endif
              enddo
            enddo
          enddo
!-----------------------------------------------------------------------

!
!***  UPDATE MOIST ARRAY.
!***  REMEMBER THAT MOIST IS ONLY USED WITH THE PHYSICS AND THUS
!***  THE P_QV SLOT (=2) IS MIXING RATIO, NOT SPECIFIC HUMIDITY.
!***  ALTHOUGH MOIST IS ONLY USED FOR PHYSICS IN OPERATIONS, IT IS
!***  UPDATED HERE FROM Q EVERY ADVECTION TIMESTEP FOR NON-OPERATIONAL
!***  CONFIGURATIONS WHERE IT MAY BE USED OUTSIDE OF THE PHYSICS.
!
        IF(.NOT.OPERATIONAL_PHYSICS)THEN
          DO K=KTS,KTE
            DO J=MYJS,MYJE
              DO I=MYIS,MYIE
                MOIST(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))
                WC = CWM(I,J,K)
                QI = 0.
                QR = 0.
                QW = 0.
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
        ENDIF
!
        had2_tim=had2_tim+timef()-btimx
!-----------------------------------------------------------------------
        endif
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! eulerian advection for model tracers
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!
!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, ETA/SIGMADOT, VERTICAL PART OF OMEGA-ALPHA TERM
!-----------------------------------------------------------------------
!
      btimx=timef()
!
      CALL VTOA(                                                        &
#ifdef DM_PARALLEL
     &          GRID,                                                   &
#endif
     &          NTSD,GRID%DT,PT,ETA2                                    &
     &         ,HBM2,EF4T                                               &
     &         ,T,DWDT,RTOP,OMGALF                                      &
     &         ,PINT,DIV,PSDT,RES                                       &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      vtoa_tim=vtoa_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      btimx=timef()
!



      CALL VADZ(NTSD,GRID%DT,FIS,GRID%SIGMA,DFL,HBM2                    &
     &         ,DETA1,DETA2,PDTOP                                       &
     &         ,PINT,PDSL,PDSLO,PETDT                                   &
     &         ,RTOP,T,Q,CWM,Z,W,DWDT,PDWDT                             &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      vadz_tim=vadz_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_G.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL HADZ(NTSD,GRID%DT,HYDRO,HBM2,DETA1,DETA2,PDTOP               &
     &         ,DX_NMM,DY_NMM,FAD                                       &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,PDSL,U,V,W,Z                                            &
     &         ,IHE,IHW,IVE,IVW                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      hadz_tim=hadz_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  ADVECTION OF W
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_H.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL EPS(NTSD,GRID%DT,HYDRO,DX_NMM,DY_NMM,FAD                     &
     &        ,DETA1,DETA2,PDTOP,PT                                     &
     &        ,HBM2,HBM3                                                &
     &        ,PDSL,PDSLO,PINT,RTOP,PETDT,PDWDT                         &
     &        ,DWDT,DWDTMN,DWDTMX                                       &
     &        ,FNS,FEW,FNE,FSE                                          &
     &        ,T,U,V,W,Q,CWM                                            &
     &        ,IHE,IHW,IVE,IVW                                          &
     &        ,IDS,IDF,JDS,JDF,KDS,KDE                                  &
     &        ,IMS,IME,JMS,JME,KMS,KME                                  &
     &        ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      eps_tim=eps_tim+timef()-btimx
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if(.not.euler) then ! Lagrangian model tracer advection
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF Q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%IDTAD)==0)THEN
        btimx=timef()
!
        vad2_micro_check: IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
          CALL VAD2(NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM               &
     &             ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP,HBM2             &
     &             ,Q,Q2,CWM,PETDT                                      &
     &             ,N_IUP_H,N_IUP_V                                     &
     &             ,N_IUP_ADH,N_IUP_ADV                                 &
     &             ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                         &
     &             ,IHE,IHW,IVE,IVW                                     &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &             ,IMS,IME,JMS,JME,KMS,KME                             &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        ELSE vad2_micro_check
          CALL VAD2_SCAL(NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM          &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP             &
     &                  ,HBM2                                           &
     &                  ,Q2,PETDT                                       &
     &                  ,N_IUP_H,N_IUP_V                                &
     &                  ,N_IUP_ADH,N_IUP_ADV                            &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                    &
     &                  ,IHE,IHW,IVE,IVW                                &
     &                  ,1,1                                            &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)                              
     
          CALL VAD2_SCAL(NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM          &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP             &
     &                  ,HBM2                                           &
     &                  ,MOIST,PETDT                                    &
     &                  ,N_IUP_H,N_IUP_V                                &
     &                  ,N_IUP_ADH,N_IUP_ADV                            &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                    &
     &                  ,IHE,IHW,IVE,IVW                                &
     &                  ,NUM_MOIST,2                                    &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!
          CALL VAD2_SCAL(NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM          &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP             &
     &                  ,HBM2                                           &
     &                  ,SCALAR,PETDT                                   &
     &                  ,N_IUP_H,N_IUP_V                                &
     &                  ,N_IUP_ADH,N_IUP_ADV                            &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                    &
     &                  ,IHE,IHW,IVE,IVW                                &
     &                  ,NUM_SCALAR,2                                   &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!

          DO K=KTS,KTE
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            Q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))
          ENDDO
          ENDDO   
          ENDDO   
!
        ENDIF vad2_micro_check
!
        vad2_tim=vad2_tim+timef()-btimx
!
      ENDIF
!    
!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF CHEMISTRY
!-----------------------------------------------------------------------
!
#ifdef WRF_CHEM  
      IF(MOD(NTSD,GRID%IDTAD)==0)THEN
#ifdef IBM
        btimx=timef()
#endif
!
        DO L=1,NUM_CHEM
        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
           CHEM_TRANS(I,J,K,L)=CHEM(I,K,J,L)
        ENDDO
        ENDDO
        ENDDO
        ENDDO

         
        CALL VAD2_SCAL(NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM            &
     &           ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                    &
     &           ,HBM2                                                  &
     &           ,CHEM_TRANS,PETDT                                            & 
     &           ,N_IUP_H,N_IUP_V                                       &
     &           ,N_IUP_ADH,N_IUP_ADV                                   &
     &           ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                           &
     &           ,IHE,IHW,IVE,IVW                                       &
     &           ,NUM_CHEM,1                                            & 
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      ENDIF      
#endif 
!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF Q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%IDTAD)==0)THEN
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_I.inc"
#endif
!
#ifdef DM_PARALLEL
        IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
#    include "HALO_NMM_I_3.inc"
        ENDIF
#endif
!
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        btimx=timef()
!
!-----------------------------------------------------------------------
        had2_micro_check: IF(CONFIG_FLAGS%MP_PHYSICS==ETAMPNEW)THEN
!-----------------------------------------------------------------------
!
          CALL HAD2(                                                   &
#if defined(DM_PARALLEL)
     &              GRID%DOMDESC,                                      &
#endif
     &              NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM              &
     &             ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                 &
     &             ,HBM2,HBM3                                          &
     &             ,Q,Q2,CWM,U,V,Z,HYDRO                               &
     &             ,N_IUP_H,N_IUP_V                                    &
     &             ,N_IUP_ADH,N_IUP_ADV                                &
     &             ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                        &
     &             ,IHE,IHW,IVE,IVW                                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!***  UPDATE MOIST ARRAY.
!***  REMEMBER THAT MOIST IS ONLY USED WITH THE PHYSICS AND THUS
!***  THE P_QV SLOT (=2) IS MIXING RATIO, NOT SPECIFIC HUMIDITY.
!***  ALTHOUGH MOIST IS ONLY USED FOR PHYSICS IN OPERATIONS, IT IS 
!***  UPDATED HERE FROM Q EVERY ADVECTION TIMESTEP FOR NON-OPERATIONAL
!***  CONFIGURATIONS WHERE IT MAY BE USED OUTSIDE OF THE PHYSICS.
!
          IF(.NOT.OPERATIONAL_PHYSICS)THEN
            DO K=KTS,KTE
            DO J=MYJS,MYJE
            DO I=MYIS,MYIE
              MOIST(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))
              WC = CWM(I,J,K)
              QI = 0.
              QR = 0.
              QW = 0.
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
          ENDIF
!
!-----------------------------------------------------------------------
        ELSE had2_micro_check
!-----------------------------------------------------------------------
!
          CALL HAD2_SCAL(                                              &
#if defined(DM_PARALLEL)
     &                   GRID%DOMDESC,                                 &
#endif        
     &                   NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM         &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP            &
     &                  ,HBM2,HBM3                                     &
     &                  ,Q2,U,V,Z,HYDRO                                &
     &                  ,N_IUP_H,N_IUP_V                               &
     &                  ,N_IUP_ADH,N_IUP_ADV                           &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                   &
     &                  ,IHE,IHW,IVE,IVW                               &
     &                  ,1,1                                           &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!        
          CALL HAD2_SCAL(                                              &
#if defined(DM_PARALLEL)
     &                   GRID%DOMDESC,                                 &
#endif
     &                   NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM         &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP            &
     &                  ,HBM2,HBM3                                     &
     &                  ,MOIST,U,V,Z,HYDRO                             &
     &                  ,N_IUP_H,N_IUP_V                               &
     &                  ,N_IUP_ADH,N_IUP_ADV                           &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                   &
     &                  ,IHE,IHW,IVE,IVW                               &
     &                  ,NUM_MOIST,2                                   &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!
          CALL HAD2_SCAL(                                              &
#if defined(DM_PARALLEL)
     &                   GRID%DOMDESC,                                 &
#endif
     &                   NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM         &
     &                  ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP            &
     &                  ,HBM2,HBM3                                     &
     &                  ,SCALAR,U,V,Z,HYDRO                            &
     &                  ,N_IUP_H,N_IUP_V                               &
     &                  ,N_IUP_ADH,N_IUP_ADV                           &
     &                  ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                   &
     &                  ,IHE,IHW,IVE,IVW                               &
     &                  ,NUM_SCALAR,2                                  &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)                             
!    
          DO K=KTS,KTE 
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            Q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))           
          ENDDO
          ENDDO    
          ENDDO   
!
!-----------------------------------------------------------------------
        ENDIF had2_micro_check
!-----------------------------------------------------------------------
!
        had2_tim=had2_tim+timef()-btimx
      ENDIF
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif ! Lagrangian model tracer advection
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF CHEMISTRY
!-----------------------------------------------------------------------
!
#ifdef WRF_CHEM
      IF(MOD(NTSD,GRID%IDTAD)==0)THEN
        btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_I_2.inc"
#endif
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        btimx=timef()
!
        CALL HAD2_SCAL(                                                &
#if defined(DM_PARALLEL)
     &                 GRID%DOMDESC,                                   &
#endif
     &                 NTSD,GRID%DT,GRID%IDTAD,DX_NMM,DY_NMM           &
     &                ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP              &
     &                ,HBM2,HBM3                                       &
     &                ,CHEM_TRANS,U,V,Z,HYDRO                                &
     &                ,N_IUP_H,N_IUP_V                                 &
     &                ,N_IUP_ADH,N_IUP_ADV                             &
     &                ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                     &
     &                ,IHE,IHW,IVE,IVW                                 &
     &                ,NUM_CHEM,1                                      &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)

      ENDIF
        DO L=1,NUM_CHEM
        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
           CHEM(I,K,J,L)=CHEM_TRANS(I,J,K,L)
        ENDDO
        ENDDO
        ENDDO
        ENDDO
#endif

!
!----------------------------------------------------------------------
!***  RADIATION
!----------------------------------------------------------------------
!
!***  When allocating CAM radiation 4d arrays (ozmixm, aerosolc), 
!***  the following two scalars are not needed.
!
      NUM_OZMIXM=1
      NUM_AEROSOLC=1
!
      IF(NTSD<=0)THEN
        NTSD_rad=NTSD
      ELSE
!
!***  Call radiation just BEFORE the top of the hour
!***  so that updated fields are written to history files.
!
        NTSD_rad=NTSD+1
      ENDIF
!
      IF(MOD(NTSD_rad,GRID%NRADS)==0.OR.                               &
     &   MOD(NTSD_rad,GRID%NRADL)==0)THEN
!
        btimx=timef()
        IF(OPERATIONAL_PHYSICS)THEN
          CALL UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
!
        CALL RADIATION(NTSD_rad,GRID%DT,GRID%JULDAY,GRID%JULYR         &
     &                ,GRID%XTIME,GRID%JULIAN                          &
     &                ,IHRST,GRID%NPHS                                 &
     &                ,GLAT,GLON,GRID%NRADS,GRID%NRADL                 &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT      &
     &                ,PD,RES,PINT,T,Q,MOIST,THS,ALBEDO,EPSR           &
     &                ,F_ICE,F_RAIN                                    &
#ifdef WRF_CHEM
     &                ,GD_CLOUD,GD_CLOUD2                              &
#endif
     &                ,SM,HBM2,CLDFRA,N_MOIST,RESTRT                   &
     &                ,RLWTT,RSWTT,RLWIN,RSWIN,RSWINC,RSWOUT           &
     &                ,RLWTOA,RSWTOA,CZMEAN                            &
     &                ,CFRACL,CFRACM,CFRACH,SIGT4                      &
     &                ,ACFRST,NCFRST,ACFRCV,NCFRCV                     &
     &                ,CUPPT,VEGFRC,SNO,HTOP,HBOT                      &
     &                ,Z,SICE,NUM_AEROSOLC,NUM_OZMIXM                  &
     &                ,GRID,CONFIG_FLAGS                               &
     &                ,RTHRATEN                                        &  
#ifdef WRF_CHEM
     &                ,PM2_5_DRY, PM2_5_WATER, PM2_5_DRY_EC            &
     &                ,TAUAER1, TAUAER2, TAUAER3, TAUAER4              & 
     &                ,GAER1, GAER2, GAER3, GAER4                      &
     &                ,WAER1, WAER2, WAER3, WAER4                      &
#endif
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        DO J=JMS,JME
        DO I=IMS,IME
          GSW(I,J)=RSWIN(I,J)-RSWOUT(I,J)
        ENDDO
        ENDDO
!
!                            ***  NOTE  ***
! RLWIN/RSWIN  - downward longwave/shortwave at the surface (formerly TOTLWDN/TOTSWDN)
! RSWINC - CLEAR-SKY downward shortwave at the surface (new for AQ)
!                            ***  NOTE  ***
!
        radiation_tim=radiation_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  APPLY TEMPERATURE TENDENCY DUE TO RADIATION
!----------------------------------------------------------------------
!
      btimx=timef()
!
!     Pass in XTIME (elapsed time from start of parent) to compute
!     the time passed into the zenith angle code consistently between
!     RDTEMP and RADIATION.


      CALL RDTEMP(NTSD,GRID%DT,GRID%JULDAY,GRID%JULYR                  &
     &           ,GRID%XTIME,IHRST,GLAT,GLON                           &
     &           ,CZEN,CZMEAN,T,RSWTT,RLWTT,HBM2                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      rdtemp_tim=rdtemp_tim+timef()-btimx

!
!----------------------------------------------------------------------
!***  TURBULENT PROCESSES 
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%NPHS)==0)THEN
!
        btimx=timef()
!
        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0)THEN
          CALL UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
!
        CALL TURBL(NTSD,GRID%DT,GRID%NPHS,RESTRT                       &
     &            ,N_MOIST,GRID%NUM_SOIL_LAYERS,SLDPTH,DZSOIL          &
     &            ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT          &
     &            ,SM,HBM2,VBM2,DX_NMM,DFRLG                           &
     &            ,CZEN,CZMEAN,SIGT4,RLWIN,RSWIN,RADOT                 &
     &            ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,SR                 &
     &            ,Q2,U,V,THS,NMM_TSK,SST,PREC,SNO                     &
     &            ,FIS,Z0,Z0BASE,USTAR,MIXHT,PBLH,LPBL,EL_MYJ          &   !PLee (3/07)
     &            ,MOIST,RMOL,MOL                                      &
     &            ,EXCH_H,EXCH_M,F,AKHS,AKMS,AKHS_OUT,AKMS_OUT         &
     &            ,THZ0,QZ0,UZ0,VZ0,QSH,MAVAIL                         &
     &            ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF             &
     &            ,IVGTYP,ISLTYP,VEGFRC,SHDMIN,SHDMAX,GRNFLX           &
     &            ,SNOTIME                                             &
     &            ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB          &
     &            ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR,EMBCK             &
     &            ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR               &
     &            ,T2,QSG,QVG,QCG,SOILT1,TSNAV,SMFR3D,KEEPFR3DFLAG     &
#if (NMM_CORE==1)
     &            ,TWBS,QWBS,TAUX,TAUY,SFCSHX,SFCLHX,SFCEVP                      &
#else
     &            ,TWBS,QWBS,SFCSHX,SFCLHX,SFCEVP                      &
#endif
     &            ,POTEVP,POTFLX,SUBSHX                                &
     &            ,APHTIM,ARDSW,ARDLW,ASRFC                            &
     &            ,RSWOUT,RSWTOA,RLWTOA                                &
     &            ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA             &
#if (NMM_CORE==1)
     &            ,UZ0H,VZ0H,DUDT,DVDT,UGWDsfc,VGWDsfc,SFENTH          & 
#else
     &            ,UZ0H,VZ0H,DUDT,DVDT                                 & 
#endif
     &            ,RTHBLTEN,RQVBLTEN                                   & 
     &            ,GRID%PCPFLG,DDATA                                   &
     &            ,HSTDV,HCNVX,HASYW,HASYS,HASYSW,HASYNW,HLENW,HLENS   & ! GWD
     &            ,HLENSW,HLENNW,HANGL,HANIS,HSLOP,HZMAX,CROT,SROT     & ! GWD
     &            ,GRID,CONFIG_FLAGS                                   &
     &            ,IHE,IHW,IVE,IVW                                     &
     &            ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &            ,IMS,IME,JMS,JME,KMS,KME                             &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)

!
!                     ***  NOTE  ***
! RLWIN/RSWIN - downward longwave/shortwave at the surface
!                     ***  NOTE  ***
!
        turbl_tim=turbl_tim+timef()-btimx
!
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
# include "HALO_NMM_TURBL_A.inc"
#endif
!-----------------
#ifdef DM_PARALLEL
# include "HALO_NMM_TURBL_B.inc"
#endif
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
!***  INTERPOLATE WINDS FROM H POINTS BACK TO V POINTS.
!
        btimx=timef()
        CALL UV_H_TO_V(NTSD,GRID%DT,GRID%NPHS,UZ0H,VZ0H,UZ0,VZ0         &
     &                ,DUDT,DVDT,U,V,HBM2,IVE,IVW                       &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        uv_htov_tim=uv_htov_tim+timef()-btimx
!
!----------------------------------------------------------------------
!*** STORE ORIGINAL TEMPERATURE ARRAY
!----------------------------------------------------------------------
!
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_J.inc"
#endif
!
#ifdef DM_PARALLEL
        IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
#    include "HALO_NMM_J_3.inc"
        ENDIF
#endif
!
#ifdef WRF_CHEM
#ifdef DM_PARALLEL
#    include "HALO_NMM_J_2.inc"
#endif
#endif
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        ICLTEND=-1
        btimx=timef()
! 
        CALL CLTEND(ICLTEND,GRID%NPHS,T,T_OLD,T_ADJ                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        cltend_tim=cltend_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  CONVECTIVE PRECIPITATION
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%NCNVC)==0.AND.                                  &
     &   CONFIG_FLAGS%CU_PHYSICS==KFETASCHEME)THEN
!
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max          &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
      ENDIF
!
      convection: IF(CONFIG_FLAGS%CU_PHYSICS/=0)THEN
        btimx=timef()
!
!***  GET TENDENCIES FOR GD SCHEME.
!    
        IF(CONFIG_FLAGS%CU_PHYSICS==GDSCHEME)THEN
          DT_INV=1./GRID%DT
          DO J=JMS,JME
          DO K=KMS,KME
          DO I=IMS,IME
            TTEN(I,K,J)=(T(I,J,K)-TTEN(I,K,J))*DT_INV
            QTEN(I,K,J)=(Q(I,J,K)-QTEN(I,K,J))*DT_INV
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!
!***  UPDATE THE MOIST ARRAY IF NEEDED.
!
        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0                             &
     &    .AND.MOD(NTSD,GRID%NPHS)/=0)THEN
          CALL UPDATE_MOIST(MOIST,Q,CWM,F_ICE,F_RAIN,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
!
!----------------------------------------------------------------------
        CALL CUCNVC(NTSD,GRID%DT,GRID%NCNVC,GRID%NRADS,GRID%NRADL      &
     &             ,GPS,RESTRT,HYDRO,CLDEFI,N_MOIST,GRID%ENSDIM        &
     &             ,MOIST                                              &
     &             ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                  &
     &             ,F_ICE,F_RAIN                                       &
!***  Changes for other cu schemes, most for GD scheme
     &             ,APR_GR,APR_W,APR_MC,TTEN,QTEN                      &
     &             ,APR_ST,APR_AS,APR_CAPMA                            &
     &             ,APR_CAPME,APR_CAPMI                                &
     &             ,MASS_FLUX,XF_ENS                                   &
     &             ,PR_ENS,GSW                                         &
#ifdef WRF_CHEM
     &             ,GD_CLOUD,GD_CLOUD2,RAINCV                          &
#endif
!
     &             ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TCUCN                 &
     &             ,OMGALF,U,V,W,Z,FIS,W0AVG                           &
     &             ,PREC,ACPREC,CUPREC,CUPPT,CPRATE                    &
     &             ,SM,HBM2,LPBL,CNVBOT,CNVTOP                         &
     &             ,HTOP,HBOT,HTOPD,HBOTD,HTOPS,HBOTS                  &
     &             ,RTHBLTEN,RQVBLTEN,RTHRATEN                         & 
     &             ,AVCNVC,ACUTIM,IHE,IHW                              &
     &             ,GRID,CONFIG_FLAGS                                  &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,IPS,IPE,JPS,JPE,KPS,KPE                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!
        cucnvc_tim=cucnvc_tim+timef()-btimx
!
      ENDIF convection
!
!----------------------------------------------------------------------
!***  GRIDSCALE MICROPHYSICS (CONDENSATION & PRECIPITATION)
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,GRID%NPHS)==0)THEN
        btimx=timef()
!
        CALL GSMDRIVE(NTSD,GRID%DT,GRID%NPHS,N_MOIST                   &
     &               ,DX_NMM(ITS,JC),GRID%DY,SM,HBM2,FIS               &
     &               ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                &
     &               ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN               &
     &               ,MOIST,SCALAR,NUM_SCALAR                          &
     &               ,F_ICE,F_RAIN,F_RIMEF,SR                          &
     &               ,PREC,ACPREC,AVRAIN                               &
     &               ,MP_RESTART_STATE                                 &
     &               ,TBPVS_STATE                                      &
     &               ,TBPVS0_STATE                                     &
     &               ,GRID,CONFIG_FLAGS                                &
     &               ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        gsmdrive_tim=gsmdrive_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!---------PRECIPITATION ASSIMILATION------------------------------------
!-----------------------------------------------------------------------
!
        IF (GRID%PCPFLG) THEN
          btimx=timef()
!
          CALL CHKSNOW(NTSD,GRID%DT,GRID%NPHS,SR,PPTDAT                 &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
          CALL ADJPPT(NTSD,GRID%DT,GRID%NPHS,PREC,LSPA,PPTDAT,DDATA     &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
!
          adjppt_tim=adjppt_tim+timef()-btimx
        ENDIF
!
!----------------------------------------------------------------------
!***  CALCULATE TEMP TENDENCIES AND RESTORE ORIGINAL TEMPS
!----------------------------------------------------------------------
!      
        ICLTEND=0
        btimx=timef()
!
        CALL CLTEND(ICLTEND,GRID%NPHS,T,T_OLD,T_ADJ                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        cltend_tim=cltend_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATE TEMP TENDENCIES FROM CLOUD PROCESSES EVERY TIME STEP
!----------------------------------------------------------------------
!
      ICLTEND=1
      btimx=timef()
!
      CALL CLTEND(ICLTEND,GRID%NPHS,T,T_OLD,T_ADJ                      &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      cltend_tim=cltend_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  LATERAL DIFFUSION
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_K.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL HDIFF(NTSD,GRID%DT,FIS,DY_NMM,HDAC,HDACV                    &
     &          ,HBM2,DETA1,GRID%SIGMA                                 &
     &          ,T,Q,cwm,U,V,Q2,Z,W,SM,SICE                                &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      IF(.NOT.OPERATIONAL_PHYSICS)THEN
        DO K=KTS,KTE
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
!!!       MOIST(I,J,K,P_QV)=MAX(0.,Q(I,J,K)/(1.-Q(I,J,K)))
          MOIST(I,J,K,P_QV)=Q(I,J,K)/(1.-Q(I,J,K))           !<-- Update mixing ratio
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
      hdiff_tim=hdiff_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT HEIGHT POINTS
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_L.inc"
#endif
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_L_3.inc"
#endif
!
#ifdef WRF_CHEM
#ifdef DM_PARALLEL
#    include "HALO_NMM_L_2.inc"
#endif
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!

      CALL BOCOH(GRID%ID,NTSD,GRID%DT,NEST,NUNIT_NBC,NBOCO,LAST_TIME,TSPH &
     &          ,LB,ETA1,ETA2,PDTOP,PT,RES                              &
     &          ,PD_BXS,PD_BXE,PD_BYS,PD_BYE,T_BXS,T_BXE,T_BYS,T_BYE    &
     &          ,Q_BXS,Q_BXE,Q_BYS,Q_BYE,U_BXS,U_BXE,U_BYS,U_BYE,V_BXS  &
     &          ,V_BXE,V_BYS,V_BYE,Q2_BXS,Q2_BXE,Q2_BYS,Q2_BYE,CWM_BXS  &
     &          ,CWM_BXE,CWM_BYS,CWM_BYE,PD_BTXS,PD_BTXE,PD_BTYS        &
     &          ,PD_BTYE,T_BTXS,T_BTXE,T_BTYS,T_BTYE,Q_BTXS,Q_BTXE      &
     &          ,Q_BTYS,Q_BTYE,U_BTXS,U_BTXE,U_BTYS,U_BTYE,V_BTXS       &
     &          ,V_BTXE,V_BTYS,V_BTYE,Q2_BTXS,Q2_BTXE,Q2_BTYS,Q2_BTYE   &
     &          ,CWM_BTXS,CWM_BTXE,CWM_BTYS,CWM_BTYE,PD,T,Q,Q2,CWM,PINT &
     &          ,MOIST,N_MOIST,SCALAR,NUM_SCALAR                        &
#ifdef WRF_CHEM
     &          ,CHEM,NUMGAS,CONFIG_FLAGS                               &
#endif
     &          ,GRID%SPEC_BDY_WIDTH,Z                                  &
     &          ,IHE,IHW,IVE,IVW                                        &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

  978	continue

 
!
      bocoh_tim=bocoh_tim+timef()-btimx
!     if(mod(ntsd,n_print_time)==0)then
!       call twr(t,0,'t',ntsd,mype,npes,mpi_comm_comp &
!    &          ,ids,ide,jds,jde,kds,kde                               &
!    &          ,ims,ime,jms,jme,kms,kme                               &
!    &          ,its,ite,jts,jte,kts,kte)
!     endif
!
!----------------------------------------------------------------------
!***  IS IT TIME FOR A CHECK POINT ON THE MODEL HISTORY FILE?
!----------------------------------------------------------------------
!
 2003 CONTINUE
!
!----------------------------------------------------------------------
!***  PRESSURE GRD, CORIOLIS, DIVERGENCE, AND HORIZ PART OF OMEGA-ALPHA
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
!
#ifdef DM_PARALLEL
      IF(CONFIG_FLAGS%MP_PHYSICS/=ETAMPNEW)THEN
#    include "HALO_NMM_A_3.inc"
      ENDIF
#endif
!
#ifdef WRF_CHEM
#ifdef DM_PARALLEL
#    include "HALO_NMM_A_2.inc"
#endif
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL PFDHT(NTSD,LAST_TIME,PT,DETA1,DETA2,PDTOP,RES,FIS           &
     &          ,HYDRO,GRID%SIGMA,FIRST,DX_NMM,DY_NMM                  &
     &          ,HBM2,VBM2,VBM3                                        &
     &          ,FDIV,FCP,WPDAR,DFL,CPGFU,CPGFV                        &
     &          ,PD,PDSL,T,Q,U,V,CWM,OMGALF,PINT,DWDT                  &
     &          ,RTOP,DIV,FEW,FNS,FNE,FSE                              &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

!
      pfdht_tim=pfdht_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  DIVERGENCE DAMPING
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_B.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max            &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL DDAMP(NTSD,GRID%DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2          &
     &          ,T,U,V,DDMPU,DDMPV                                     &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      ddamp_tim=ddamp_tim+timef()-btimx
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      IF(FIRST.AND.NTSD==0)THEN
        FIRST=.FALSE.
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
#ifdef WRF_CHEM
#ifdef DM_PARALLEL
#    include "HALO_NMM_A_2.inc"
#endif
#endif
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max          &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        GO TO 2000
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT VELOCITY POINTS
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max            &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!

      CALL BOCOV(GRID%ID,NTSD,GRID%DT,LB,U_BXS,U_BXE,U_BYS,U_BYE,V_BXS &
     &          ,V_BXE,V_BYS,V_BYE,U_BTXS,U_BTXE,U_BTYS,U_BTYE,V_BTXS  &
     &          ,V_BTXE,V_BTYS,V_BTYE,U,V                              &
     &          ,GRID%SPEC_BDY_WIDTH                                   &
     &          ,IHE,IHW,IVE,IVW                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE )
  
  979   continue
 
!
      bocov_tim=bocov_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  COPY THE NMM VARIABLE Q2 TO THE WRF VARIABLE TKE_MYJ
!----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO J=JTS,JTE
      DO I=ITS,ITE
        TKE_MYJ(I,J,K)=0.5*Q2(I,J,K) !TKE is q squared over 2
      ENDDO
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!
      IF(LAST_TIME.AND.ALLOCATED(PPTDAT))THEN
        DEALLOCATE(PPTDAT,STAT=ISTAT)
      ENDIF
!
!----------------------------------------------------------------------
!
      solve_tim=solve_tim+timef()-btim
!
!----------------------------------------------------------------------
!***  PRINT TIMING VARIABLES WHEN DESIRED.
!----------------------------------------------------------------------
!

!!!!!!!!!! Diagnose maximum values of various items for gridded hourly maxes


!!! for K near 1000 m AGL:

        if (.not. micro_has_been_read) then
!	write(0,*) 'calling MICRO_INIT'
        CALL MICRO_INIT(MASSI,MASSR,RQR_DRmin,RQR_DRmax,CN0r0,CN0r_DMRmin,CN0r_DMRmax)
!	write(0,*) 'beyond MICRO_INIT'
        endif

        IF(.NOT.ALLOCATED(UPINDX))ALLOCATE(UPINDX(IMS:IME,JMS:JME))


!        write(0,*) 'RQR_DRmin, RQR_DRmax right after MICRO_INIT block: ', RQR_DRmin, RQR_DRmax



!      IF(MOD(NTSD,GRID%NPHS)==0)THEN
      IF(MOD(NTSD,GRID%IDTAD)==0)THEN


      DO J=JTS,MIN(JTE,JDF)
      DO I=ITS,MIN(ITE,IDF)
      DO K=2,KTE-1

       ZMID=(Z(I,J,K) + Z(I,J,K+1))/2.
       ZMIDP1=(Z(I,J,K+1) + Z(I,J,K+2))/2.

        if ( (ZMID-Z(I,J,KTS)) .le. 1000. &
        .and.(ZMIDP1-Z(I,J,KTS)) .ge. 1000.) then

        ZMID=ZMID-Z(I,J,KTS)
        ZMIDP1=ZMIDP1-Z(I,J,KTS)

        wgta=1.-((ZMIDP1-1000.)/(ZMIDP1-ZMID))
        wgtb=1.-wgta

          P1Da=AETA1(K+1)*PDTOP+AETA2(K+1)*PD(I,J)+PT
          T1Da=T(I,J,K+1)
          Q1Da=Q(I,J,K+1)
          C1Da=CWM(I,J,K+1)
          FI1Da=F_ICE(I,K+1,J)
          FR1Da=F_RAIN(I,K+1,J)
          FS1Da=F_RIMEF(I,K+1,J)

          P1Db=AETA1(K)*PDTOP+AETA2(K)*PD(I,J)+PT
          T1Db=T(I,J,K)
          Q1Db=Q(I,J,K)
          C1Db=CWM(I,J,K)
          FI1Db=F_ICE(I,K,J)
          FR1Db=F_RAIN(I,K,J)
          FS1Db=F_RIMEF(I,K,J)

        P1D=wgta*P1Da+wgtb*P1Db
        T1D=wgta*T1Da+wgtb*T1Db
        Q1D=wgta*Q1Da+wgtb*Q1Db
        C1D=wgta*C1Da+wgtb*C1Db
        FI1D=wgta*FI1Da+wgtb*FI1Db
        FR1D=wgta*FR1Da+wgtb*FR1Db
        FS1D=wgta*FS1Da+wgtb*FS1Db
          CUREFL=0.

        if (RQR_DRmin .eq. 0.) then
        call wrf_error_fatal("bad RQR_DRmin")
        endif

        if (C1D .ge. 1.e-12) then
          CALL CALMICT(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL, &
                       DBZ1, TBPVS_STATE,CN0r_DMRmin, CN0r_DMRmax, RQR_DRmin, RQR_DRmax, CN0R0,  MASSI, &
                        NX, I, J)
        else
               DBZ1=-20.
        endif

          MAXDBZ(I,J)=max(MAXDBZ(I,J),DBZ1)

!       if (mod(I,100) .eq. 0 .and. mod(J,100) .eq. 0) then
!       write(0,*) 'K, P1D, T1D, Q1D, C1D, FI1D: ', K, P1D, T1D, Q1D, C1D, FI1D, DBZ1
!       endif

!        if (I .eq. 44 .and. J .eq. 247) then
!        write(0,*) 'K, Q1D, C1D,  DBZ, MAXDBZ: ', K, Q1D, C1D, DBZ1, MAXDBZ(I,J)
!        endif

       endif

      ENDDO
      ENDDO
      ENDDO

      ENDIF

! !!!!!!!!!!!!!!!!!!!!
! !!!!!!!!!!!!!!!!!!!!

      DO K=KTS,KTE-1
      DO J=JTS,MIN(JTE,JDF)
      DO I=ITS,MIN(ITE,IDF)
        plow=AETA1(K)*PDTOP+AETA2(K)*PD(I,J)+PT
        pup=AETA1(K+1)*PDTOP+AETA2(K+1)*PD(I,J)+PT
        if (plow .ge. 40000. .and. pup  .le. 40000.) then
        UPINDX(I,J)=K
        endif
      ENDDO
      ENDDO
      ENDDO

      DO K=KTS,KTE-1
      DO J=JTS,MIN(JTE,JDF)
      DO I=ITS,MIN(ITE,IDF)
        if (K .le. UPINDX(I,J)) then
        maxupdr(I,J)=max(maxupdr(I,J),w(I,J,K))
        maxdndr(I,J)=min(maxdndr(I,J),w(I,J,K))
        endif
      ENDDO
      ENDDO
      ENDDO

      DO J=JTS,MIN(JTE,JDF)
      DO I=ITS,MIN(ITE,IDF)
       magw2=(U10(I,J)**2.+V10(I,J)**2.)
       magw=magw2**(0.5)
        if (NTSD >= 2) then
          if (magw .gt. max10mw(I,J)) then
           max10u(I,J)=U10(I,J)
           max10v(I,J)=V10(I,J)
           max10mw(I,J)=MAX(max10mw(I,J), magw)
          endif
        endif

      ENDDO
      ENDDO

        CALL calc_uphlcy(U,V,W,Z,maxhlcy,DX_NMM,DY_NMM &
                          ,IDS,IDF,JDS,JDF,KDS,KDE                                            &
                          ,IMS,IME,JMS,JME,KMS,KME                                            &
                          ,ITS,ITE,JTS,JTE,KTS,KTE                                            )

!      DO J=JTS,MIN(JTE,JDF)
!      DO I=ITS,MIN(ITE,IDF)
!	if (maxhlcy(I,J) .ne. maxhlcy(I,J) ) then
!	write(0,*) 'bad maxhlcy at : ', I,J
!	write(0,*) 'U(I,J,:): ', U(I,J,:)
!	write(0,*) 'V(I,J,:): ', V(I,J,:)
!	write(0,*) 'W(I,J,:): ', W(I,J,:)
!	if (ITS .le. 805 .and. ITE .ge. 805 .and. JTS .le. 907 .and. JTE .ge. 907) then
!        call wrf_error_fatal(" bad maxhlcy ")
!	endif
!        maxhlcy(I,J)=0.
!	endif
	
!        if (maxhlcy(I,J) .ge. 100.) then
!        write(0,*) 'I,J, maxhlcy(I,J): ', I,J, maxhlcy(I,J)
!        endif
!        if (maxhlcy(I,J) .ge. 2000.) then
!        call wrf_error_fatal(" too big ")
!        endif

!      ENDDO
!      ENDDO

      sum_tim=pdte_tim+adve_tim+vtoa_tim+vadz_tim+hadz_tim+eps_tim     &
     &       +vad2_tim+had2_tim+radiation_tim+rdtemp_tim+turbl_tim     &
     &       +cltend_tim+cucnvc_tim+gsmdrive_tim+hdiff_tim             &
     &       +bocoh_tim+pfdht_tim+ddamp_tim+bocov_tim+uv_htov_tim      &
     &       +exch_tim+adjppt_tim
!
      if(mod(ntsd,n_print_time)==0)then
        write(message,*)' ntsd=',ntsd,' solve_tim=',solve_tim*1.e-3          &
     &           ,' sum_tim=',sum_tim*1.e-3
        call wrf_message(trim(message))
        write(message,*)' pdte_tim=',pdte_tim*1.e-3,' pct=',pdte_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' adve_tim=',adve_tim*1.e-3,' pct=',adve_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' vtoa_tim=',vtoa_tim*1.e-3,' pct=',vtoa_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' vadz_tim=',vadz_tim*1.e-3,' pct=',vadz_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' hadz_tim=',hadz_tim*1.e-3,' pct=',hadz_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' eps_tim=',eps_tim*1.e-3,' pct=',eps_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' vad2_tim=',vad2_tim*1.e-3,' pct=',vad2_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' had2_tim=',had2_tim*1.e-3,' pct=',had2_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' radiation_tim=',radiation_tim*1.e-3,' pct=',radiation_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' rdtemp_tim=',rdtemp_tim*1.e-3,' pct=',rdtemp_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' turbl_tim=',turbl_tim*1.e-3,' pct=',turbl_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' cltend_tim=',cltend_tim*1.e-3,' pct=',cltend_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' cucnvc_tim=',cucnvc_tim*1.e-3,' pct=',cucnvc_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' gsmdrive_tim=',gsmdrive_tim*1.e-3,' pct=',gsmdrive_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' adjppt_tim=',adjppt_tim*1.e-3,' pct=',adjppt_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' hdiff_tim=',hdiff_tim*1.e-3,' pct=',hdiff_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' bocoh_tim=',bocoh_tim*1.e-3,' pct=',bocoh_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' pfdht_tim=',pfdht_tim*1.e-3,' pct=',pfdht_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' ddamp_tim=',ddamp_tim*1.e-3,' pct=',ddamp_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' bocov_tim=',bocov_tim*1.e-3,' pct=',bocov_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' uv_h_to_v_tim=',uv_htov_tim*1.e-3,' pct=',uv_htov_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' exch_tim=',exch_tim*1.e-3,' pct=',exch_tim/sum_tim*100.
        call wrf_message(trim(message))
!        call time_stats(exch_tim,'exchange',ntsd,mype,npes,mpi_comm_comp)
!        write(message,*)' exch_tim_max=',exch_tim_max*1.e-3
!        call wrf_message(trim(message))
!
        call field_stats(t,mype,mpi_comm_comp                          &
     &                  ,ids,ide,jds,jde,kds,kde                       &
     &                  ,ims,ime,jms,jme,kms,kme                       &
     &                  ,its,ite,jts,jte,kts,kte)

!!! reset the maximum 10 m wind array and maximum updraft/downdraft arrays
        write(0,*) 'max fields reset'
      DO J=JTS,MIN(JTE,JDE-1)
      DO I=ITS,MIN(ITE,IDE-1)
       max10mw(I,J)=-999.
       max10u(I,J)=-999.
       max10v(I,J)=-999.
       maxupdr(I,J)=-999.
       maxdndr(I,J)=999.
       maxhlcy(I,J)=-999.
       maxdbz(I,J)=-20.
      ENDDO
      ENDDO

      endif
!
!     if(last_time)then
      DEALLOCATE(TTEN,STAT=ISTAT)
      DEALLOCATE(QTEN,STAT=ISTAT)
      DEALLOCATE(RTHRATEN,STAT=ISTAT)
      DEALLOCATE(RTHBLTEN,STAT=ISTAT)
      DEALLOCATE(RQVBLTEN,STAT=ISTAT)
#ifdef WRF_CHEM
      DEALLOCATE(CHEM_TRANS,STAT=ISTAT)
#endif
!
#define COPY_OUT
#include <scalar_derefs.inc>
      Return
!----------------------------------------------------------------------
!**********************************************************************
!**********************************************************************
!*************    EXIT FROM THE TIME LOOP    **************************
!**********************************************************************
!**********************************************************************
!----------------------------------------------------------------------
      END SUBROUTINE SOLVE_NMM
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE TWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
      IMPLICIT NONE
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
!*** LOCAL VARIABLES
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
#endif
      INTEGER,DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND,IUNIT                &
     &          ,J,K,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)
!
      IF(MYPE==0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF
!
!----------------------------------------------------------------------
!!!!  DO 500 K=KTS,KTE+KK     !Unflipped
!!!!  DO 500 K=KTE+KK,KTS,-1
      DO 500 K=KDE-1,KDS,-1   !Write LM layers top down for checking
!----------------------------------------------------------------------
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
!----------------------------------------------------------------------
      ELSE
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
     &               ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
!
        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==0)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO
!
      ENDIF
#else

        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==0)IENDX=IENDX-1
          WRITE(IUNIT)(ARRAY(I,K,J),I=1,IENDX)
        ENDDO

#endif

!
!----------------------------------------------------------------------
  500 CONTINUE
!
      IF(MYPE==0)CLOSE(IUNIT)
!----------------------------------------------------------------------
!
      END SUBROUTINE TWR
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE VWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
      IMPLICIT NONE
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
!*** LOCAL VARIABLES
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
#endif
      INTEGER,DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND,IUNIT                &
     &          ,J,K,L,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME
      LOGICAL :: OPENED
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)
!
      IF(MYPE==0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF
!     IF(MYPE==0)THEN
!       OPEN_UNIT: DO L=51,99
!         INQUIRE(L,OPENED=OPENED)
!         IF(.NOT.OPENED)THEN
!           IUNIT=L
!           OPEN(UNIT=IUNIT,FILE=FILENAME,STATUS='NEW'                 &
!               ,FORM='UNFORMATTED',IOSTAT=IER)
!           IF(IER/=0)THEN
!             WRITE(message,*)' Opening file error=',IER
!             CALL wrf_message(trim(message))
!           ENDIF
!           EXIT OPEN_UNIT
!         ENDIF
!       ENDDO OPEN_UNIT
!     ENDIF
!
!----------------------------------------------------------------------
!!!!  DO 500 K=KTS,KTE+KK     !Unflipped
!!!!  DO 500 K=KTE+KK,KTS,-1
      DO 500 K=KDE-1,KDS,-1   !Write LM layers top down for checking
!----------------------------------------------------------------------
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
!----------------------------------------------------------------------
      ELSE
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
     &               ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
!
        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==1)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO
!
      ENDIF
#else

        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==0)IENDX=IENDX-1
          WRITE(IUNIT)(ARRAY(I,K,J),I=1,IENDX)
        ENDDO

#endif
!
!----------------------------------------------------------------------
  500 CONTINUE
!
      IF(MYPE==0)CLOSE(IUNIT)
!----------------------------------------------------------------------
!
      END SUBROUTINE VWR
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE EXIT(NAME,PINT,T,Q,U,V,Q2,W                           &
     &               ,NTSD,MYPE,MPI_COMM_COMP                          &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,MYPE,MPI_COMM_COMP,NTSD
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT,T,Q   &
                                                           ,U,V,Q2,W
      CHARACTER(*),INTENT(IN) :: NAME
!
      INTEGER :: I,J,K,IEND,IERR,IRET
      CHARACTER(256) :: ERRMESS
      LOGICAL :: E_BDY,S_BDY
!----------------------------------------------------------------------
      IRET=0
  100 FORMAT(' EXIT ',A,' AT NTSD=',I5)
      IEND=ITE
      S_BDY=(JTS==JDS)
      E_BDY=(ITE==IDE-1)
!
      DO K=KTS,KTE
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==0)IEND=ITE-1
!
      DO I=ITS,IEND
        IF(T(I,J,K)>330..OR.T(I,J,K)<180..OR.T(I,J,K)/=T(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,200)I,J,K,T(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  200     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' T=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,205)NAME,T(I,J,K),I,J,K,MYPE
  205     FORMAT(' EXIT ',A,' TEMPERATURE=',E12.5                      &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL WRF_ERROR_FATAL(ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(Q(I,J,K)<-1.E-4.OR.Q(I,J,K)>30.E-3                      &
     &         .OR.Q(I,J,K)/=Q(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,300)I,J,K,Q(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  300     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' Q=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,305)NAME,Q(I,J,K),I,J,K,MYPE
  305     FORMAT(' EXIT ',A,' SPEC HUMIDITY=',E12.5                    &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL WRF_ERROR_FATAL(ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(PINT(I,J,K)<0..OR.PINT(I,J,K)/=PINT(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,315)I,J,K,PINT(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  315     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' PINT=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         CALL WRF_ERROR_FATAL(ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(W(I,J,K)/=W(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,325)I,J,K,W(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  325     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' W=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         CALL WRF_ERROR_FATAL(ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!
      DO K=KTS,KTE
      DO J=JTS,JTE
      IEND=ITE
      IF(E_BDY.AND.MOD(J,2)==1)IEND=ITE-1
      DO I=ITS,IEND
        IF(ABS(U(I,J,K))>125..OR.ABS(V(I,J,K))>125.                    &
     &         .OR.U(I,J,K)/=U(I,J,K).OR.V(I,J,K)/=V(I,J,K))THEN
          WRITE(errmess,100)NAME,NTSD
          CALL wrf_message(trim(errmess))
          WRITE(errmess,400)I,J,K,U(I,J,K),V(I,J,K),MYPE,NTSD
          CALL wrf_message(trim(errmess))
  400     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' U=',E12.5      &
     &,          ' V=',E12.5,' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,405)NAME,U(I,J,K),V(I,J,K),I,J,K,MYPE
  405     FORMAT(' EXIT ',A,' U=',E12.5,' V=',E12.5                    &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL WRF_ERROR_FATAL(ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE EXIT
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE TIME_STATS(TIME_LCL,NAME,NTSD,MYPE,NPES,MPI_COMM_COMP)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE,NPES,NTSD
      REAL,INTENT(IN) :: TIME_LCL
!
      CHARACTER(*),INTENT(IN) :: NAME
!
!*** LOCAL VARIABLES
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
#endif
      INTEGER,ALLOCATABLE,DIMENSION(:) :: ID_PE,IPE_SORT
!
      INTEGER :: IPE,IPE_MAX,IPE_MEDIAN,IPE_MIN,IRECV,IRTN,ISEND       &
     &          ,N,N_MEDIAN,NLEN
!
      REAL,ALLOCATABLE,DIMENSION(:) :: TIME,SORT_TIME
      REAL,DIMENSION(2) :: REMOTE
      REAL :: TIME_MAX,TIME_MEAN,TIME_MEDIAN,TIME_MIN
!
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(25) :: TITLE
      CHARACTER(LEN=256) :: message
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      TITLE=NAME//'_'//TIMESTEP(1:NLEN)
!
!----------------------------------------------------------------------
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      IF(MYPE==0)THEN
        ALLOCATE(TIME(1:NPES))
        ALLOCATE(SORT_TIME(1:NPES))
        ALLOCATE(ID_PE(1:NPES))
        ALLOCATE(IPE_SORT(1:NPES))
!
        TIME(1)=TIME_LCL
        ID_PE(1)=MYPE
!
!***  COLLECT TIMES AND PE VALUES FROM OTHER PEs
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(REMOTE,2,MPI_REAL,IPE,IPE                      &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
!
          TIME(IPE+1)=REMOTE(1)
          ID_PE(IPE+1)=NINT(REMOTE(2))
        ENDDO
!
!***  NOW GET STATS.
!***  FIRST THE MAX, MIN, AND MEAN TIMES.
!
        TIME_MEAN=0.
        TIME_MAX=-1.
        TIME_MIN=1.E10
        IPE_MAX=-1
        IPE_MIN=-1
!
        DO N=1,NPES
          TIME_MEAN=TIME_MEAN+TIME(N)
!
          IF(TIME(N)>TIME_MAX)THEN
            TIME_MAX=TIME(N)
            IPE_MAX=ID_PE(N)
          ENDIF
!
          IF(TIME(N)<TIME_MIN)THEN
            TIME_MIN=TIME(N)
            IPE_MIN=ID_PE(N)
          ENDIF
!
        ENDDO
!
        TIME_MAX=TIME_MAX*1.E-3
        TIME_MIN=TIME_MIN*1.E-3
        TIME_MEAN=TIME_MEAN*1.E-3/REAL(NPES)
!
!***  THEN THE MEDIAN TIME.
!
        CALL SORT(TIME,NPES,SORT_TIME,IPE_SORT)
        N_MEDIAN=(NPES+1)/2
        TIME_MEDIAN=SORT_TIME(N_MEDIAN)*1.E-3
        IPE_MEDIAN=IPE_SORT(N_MEDIAN)
!
!----------------------------------------------------------------------
      ELSE
!
!***  SEND TIME AND PE VALUE TO PE0.
!
        REMOTE(1)=TIME_LCL
        REMOTE(2)=REAL(MYPE)
!
        CALL MPI_SEND(REMOTE,2,MPI_REAL,0,MYPE,MPI_COMM_COMP,ISEND)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
!***  WRITE RESULTS
!
      IF(MYPE==0)THEN
        WRITE(message,100)TITLE
        CALL wrf_message(trim(message))
        WRITE(message,105)TIME_MAX,IPE_MAX
        CALL wrf_message(trim(message))
        WRITE(message,110)TIME_MIN,IPE_MIN
        CALL wrf_message(trim(message))
        WRITE(message,115)TIME_MEDIAN,IPE_MEDIAN
        CALL wrf_message(trim(message))
        WRITE(message,120)TIME_MEAN
        CALL wrf_message(trim(message))
  100   FORMAT(' Time for ',A)
  105   FORMAT(' Maximum=',G11.5,' for PE ',I2.2)
  110   FORMAT(' Minimum=',G11.5,' for PE ',I2.2)
  115   FORMAT(' Median =',G11.5,' for PE ',I2.2)
  120   FORMAT(' Mean   =',G11.5)
      ENDIF
!----------------------------------------------------------------------
!
#endif
      END SUBROUTINE TIME_STATS
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE SORT(DATA,NPES,DATA_SORTED,IPE_SORTED)
!----------------------------------------------------------------------
!***
!***  SORT DATA FROM MULTIPLE PEs.  SEND BACK THE SORTED DATA ITEMS
!***  ALONG WITH THE ASSOCIATED TASK IDs.
!***
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NPES
      REAL,DIMENSION(NPES),INTENT(IN) :: DATA
!
      INTEGER,DIMENSION(NPES),INTENT(OUT) :: IPE_SORTED
      REAL,DIMENSION(NPES),INTENT(OUT) :: DATA_SORTED
!----------------------------------------------------------------------
      TYPE :: DATA_LINK
        REAL :: VALUE
        INTEGER :: IPE
        TYPE(DATA_LINK),POINTER :: NEXT_VALUE
      END TYPE
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!----------------------------------------------------------------------
      INTEGER :: ISTAT,N
!
      TYPE(DATA_LINK),POINTER :: HEAD,TAIL  ! Smallest, largest
      TYPE(DATA_LINK),POINTER :: PTR_NEW    ! Each new value
      TYPE(DATA_LINK),POINTER :: PTR1,PTR2  ! Working pointers
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      pe_loop: DO N=1,NPES
        ALLOCATE(PTR_NEW,STAT=ISTAT)  ! Location for next data items
        PTR_NEW%VALUE=DATA(N)
        PTR_NEW%IPE=N-1
!
!----------------------------------------------------------------------
!***  DETERMINE WHERE IN LIST TO INSERT VALUE.
!***  FIRST THE INITIAL DATA VALUE.
!----------------------------------------------------------------------
!
!       main: IF(.NOT.ASSOCIATED(HEAD))THEN
        main: IF(N==1)THEN
          HEAD=>PTR_NEW
          TAIL=>HEAD
          NULLIFY(PTR_NEW%NEXT_VALUE)
!
!----------------------------------------------------------------------
!***  THE NEW VALUE IS LESS THAN THE SMALLEST VALUE ALREADY SORTED.
!----------------------------------------------------------------------
!
        ELSE
          check: IF(PTR_NEW%VALUE<HEAD%VALUE)THEN
            PTR_NEW%NEXT_VALUE=>HEAD
            HEAD=>PTR_NEW
!
!----------------------------------------------------------------------
!***  THE NEW VALUE IS GREATER THAN THE LARGEST VALUE ALREADY SORTED.
!----------------------------------------------------------------------
!
          ELSEIF(PTR_NEW%VALUE>=TAIL%VALUE)THEN
            TAIL%NEXT_VALUE=>PTR_NEW  ! This is what connects the former
                                      ! final value in the list to
                                      ! the new value being appended.
            TAIL=>PTR_NEW
            NULLIFY(TAIL%NEXT_VALUE)
!
!----------------------------------------------------------------------
!***  THE NEW VALUE IS IN BETWEEN VALUES ALREADY SORTED.
!----------------------------------------------------------------------
!
          ELSE
            PTR1=>HEAD
            PTR2=>PTR1%NEXT_VALUE
!
            search: DO
              IF((PTR_NEW%VALUE>=PTR1%VALUE).AND.                      &
     &           (PTR_NEW%VALUE<PTR2%VALUE))THEN
                PTR_NEW%NEXT_VALUE=>PTR2
                PTR1%NEXT_VALUE=>PTR_NEW
                EXIT search
              ENDIF
!
              PTR1=>PTR2
              PTR2=>PTR2%NEXT_VALUE
            ENDDO search
!
          ENDIF check
!
        ENDIF main
!
      ENDDO pe_loop
!
!----------------------------------------------------------------------
!***  COLLECT THE SORTED NUMBERS FROM THE LINKED LIST.
!----------------------------------------------------------------------
!
      PTR1=>HEAD
!
      DO N=1,NPES
!       IF(.NOT.ASSOCIATED(PTR_NEW))EXIT
        DATA_SORTED(N)=PTR1%VALUE
        IPE_SORTED(N)=PTR1%IPE
        PTR1=>PTR1%NEXT_VALUE
      ENDDO
!
      DEALLOCATE(PTR_NEW)
      NULLIFY (HEAD,TAIL,PTR1,PTR2)
!----------------------------------------------------------------------
      END SUBROUTINE SORT
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE FIELD_STATS(FIELD,MYPE,MPI_COMM_COMP                  &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                   &
     &                      ,IMS,IME,JMS,JME,KMS,KME                   &
     &                      ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!***
!***  GENERATE STANDARD LAYER STATISTICS FOR THE DESIRED FIELD.
!***
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: FIELD
!
!----------------------------------------------------------------------
!***  LOCAL
!----------------------------------------------------------------------
!
      INTEGER,PARAMETER :: DOUBLE=SELECTED_REAL_KIND(15,300)
!
      INTEGER :: I,IEND,IRTN,I_BY_J,J,K,KFLIP
!
      REAL :: FIKJ,FMAXK,FMINK
      REAL(KIND=DOUBLE) :: F_MEAN,POINTS,RMS,ST_DEV,SUMFK,SUMF2K
      REAL,DIMENSION(KTS:KTE) :: FMAX,FMAX_0,FMIN,FMIN_0
      REAL(KIND=DOUBLE),DIMENSION(KTS:KTE) :: SUMF,SUMF_0,SUMF2,SUMF2_0
 
      CHARACTER(LEN=256) :: message
!----------------------------------------------------------------------
!
      I_BY_J=(IDE-IDS)*(JDE-JDS)-(JDE-JDS-1)/2  ! This assumes that
                                                ! IDE and JDE are each
                                                ! one greater than the
                                                ! true grid size.
#if defined(DM_PARALLEL) && !defined(STUBMPI)
!
      layer_loop:  DO K=KTS,KTE
!
        FMAXK=-1.E10
        FMINK=1.E10
        SUMFK=0.
        SUMF2K=0.
!
        DO J=JTS,JTE
          IEND=MIN(ITE,IDE-1)
          IF(MOD(J,2)==0.AND.ITE==IDE-1)IEND=IEND-1
          DO I=ITS,IEND
            FIKJ=FIELD(I,J,K)
            FMAXK=MAX(FMAXK,FIKJ)
            FMINK=MIN(FMINK,FIKJ)
            SUMFK=SUMFK+FIKJ
            SUMF2K=SUMF2K+FIKJ*FIKJ
          ENDDO
        ENDDO
!
        FMAX(K)=FMAXK
        FMIN(K)=FMINK
        SUMF(K)=SUMFK
        SUMF2(K)=SUMF2K
!
      ENDDO layer_loop
!
!----------------------------------------------------------------------
!***  GLOBAL STATS
!----------------------------------------------------------------------
!
      CALL MPI_REDUCE(SUMF,SUMF_0,KTE,MPI_REAL8,MPI_SUM,0              &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(SUMF2,SUMF2_0,KTE,MPI_REAL8,MPI_SUM,0            &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMAX,FMAX_0,KTE,MPI_REAL,MPI_MAX,0               &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMIN,FMIN_0,KTE,MPI_REAL,MPI_MIN,0               &
     &               ,MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
        POINTS=I_BY_J
        DO K=KTE,KTS,-1
          F_MEAN=SUMF_0(K)/POINTS
          ST_DEV=SQRT((POINTS*SUMF2_0(K)-SUMF_0(K)*SUMF_0(K))/         &
     &                (POINTS*(POINTS-1)))
          RMS=SQRT(SUMF2_0(K)/POINTS)
          KFLIP=KTE-K+1
          WRITE(message,101)KFLIP,FMAX_0(K),FMIN_0(K)
          CALL wrf_message(trim(message))
          WRITE(message,102)F_MEAN,ST_DEV,RMS
          CALL wrf_message(trim(message))
  101     FORMAT(' LAYER=',I2,' MAX=',E13.6,' MIN=',E13.6)
  102     FORMAT(9X,' MEAN=',E13.6,' STDEV=',E13.6,' RMS=',E13.6)
        ENDDO
      ENDIF
#endif
!----------------------------------------------------------------------
      END SUBROUTINE FIELD_STATS
!----------------------------------------------------------------------
      FUNCTION TIMEF()
      REAL*8 TIMEF
      INTEGER :: IC,IR
      CALL SYSTEM_CLOCK(COUNT=IC,COUNT_RATE=IR)
      TIMEF=REAL(IC)/REAL(IR)*1000.0
      END

      SUBROUTINE calc_uphlcy(U,V,W,Z,maxhlcy,DX,DY                                            &
                          ,IDS,IDF,JDS,JDF,KDS,KDE                                            &
                          ,IMS,IME,JMS,JME,KMS,KME                                            &
                          ,ITS,ITE,JTS,JTE,KTS,KTE                                            )

      REAL, PARAMETER:: HLOWER=2000.
      REAL, PARAMETER:: HUPPER=5000.

      REAL :: U(IMS:IME,JMS:JME,KMS:KME)
      REAL :: V(IMS:IME,JMS:JME,KMS:KME)
      REAL :: W(IMS:IME,JMS:JME,KMS:KME)
      REAL :: Z(IMS:IME,JMS:JME,KMS:KME)
      REAL :: maxhlcy(IMS:IME,JMS:JME)
      REAL :: DX(IMS:IME,JMS:JME), UPDHEL(IMS:IME,JMS:JME)
      INTEGER:: UPINDX(IMS:IME,JMS:JME), IHW(JMS:JME), IHE(JMS:JME)

      DO J=JTS,min(JTE,JDF)
        IHW(J)=-MOD(J,2)
        IHE(J)=IHW(J)+1
      ENDDO

!       write(0,*) 'Z(ITS,JTS,1): ', Z(ITS,JTS,1)
!       write(0,*) 'Z(ITS,JTS,10): ', Z(ITS,JTS,10)

      UPDHEL=0.

      R2DY=1./(2.*DY)

      DO J=max(JTS,2),min(JTE,JDF-1)
      DO I=max(ITS,2),min(ITE,IDF-1)

      R2DX=1./(2.*DX(I,J))

       l_loop: DO L=KTS,KTE-1
        ZMIDLOC=0.5*(Z(I,J,L)+Z(I,J,L+1))

!       if (I .eq. 5 .and. J .eq. 5) then
!       write(0,*) 'L, ZMIDLOC, AGL: ', L, ZMIDLOC, ZMIDLOC-Z(I,J,1)
!       endif

        IF ( (ZMIDLOC - Z(I,J,1)) .ge. HLOWER  .AND. &
            (ZMIDLOC - Z(I,J,1)) .le. HUPPER ) THEN
           DZ=(Z(I,J,L+1)-Z(I,J,L))

!       if (I .eq. 5 .and. J .eq. 5) then
!       write(0,*) 'I,J,L, DZ: ', I,J,L, DZ
!       endif

           IF (W(I,J,L) .lt. 0) THEN

!          ANY DOWNWARD MOTION IN 2-5 km LAYER KILLS COMPUTATION AND
!          SETS RESULTANT UPDRAFT HELICTY TO ZERO

              UPDHEL(I,J)=0.
              EXIT l_loop

           ENDIF

           DVDX   = (V(I+IHE(J),J,L)-V(I+IHW(J),J,L))*R2DX
           DUDY   = (U(I,J+1,L)-U(I,J-1,L))*R2DY
!       if (L .eq. KTS .and. I .eq. 5 .and. J .eq. 5) then
!       write(0,*) 'I,J,L, dvdx, dudy: ', I,J,L, dvdx, dudy
!       endif

!       if (I .eq. 5 .and. J .eq. 5) then
!       write(0,*) 'W(I,J,L): ', I,J,L,W(I,J,L)
!       endif

         UPDHEL(I,J)=UPDHEL(I,J)+(DVDX-DUDY)*W(I,J,L)*DZ


        ENDIF
      ENDDO l_loop

      if (UPDHEL(I,J) .gt. maxhlcy(I,J)) maxhlcy(I,J)=UPDHEL(I,J)

      ENDDO
      ENDDO



      END SUBROUTINE calc_uphlcy

      SUBROUTINE CALMICT(P1D,T1D,Q1D,C1D,FI1D,FR1D,FS1D,CUREFL, &
                       DBZ1, TBPVS_STATE,CN0r_DMRmin, CN0r_DMRmax, RQR_DRmin, RQR_DRmax,CN0R0,MASSI, NX, I, J)

        IMPLICIT NONE

      REAL, PARAMETER :: &
      FMW=18.015,FMD=28.964,EPS=FMW/FMD,ONEPS=1.-EPS,TFRZ=273.15, &
      PQ0=379.90516,A2=17.2693882,A3=273.16,A4=35.86, EPSQ=1.E-12, &
      QCLDmin=1.E-6,  &
      STBOL=1./5.67E-8, DBZmin=-20., abscoef=8.0, abscoefi=5.0, &
      XLAI=4.0, &
      RHmin=1.0E-6, &
      SMALL=1.E-6 !  RHgrd=1.0

      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3, &
                         XMImin=1.e6*DMImin, XMImax=1.e6*DMImax
      INTEGER, PARAMETER :: MDImin=XMImin, MDImax=XMImax
!-----------------------------------------------------------------------
!--- Mean mass of precpitation ice particles as functions of their mean
!    size (in microns)
!
       REAL MASSI(MDImin:MDImax)
       INTEGER :: NX
       REAL, INTENT(IN):: TBPVS_STATE(NX)
!
!-----------------------------------------------------------------------
!
!--- Mean rain drop diameters vary from 50 microns to 1000 microns (1 mm)
!
      REAL, PARAMETER :: DMRmin=.05E-3, DMRmax=1.E-3, DelDMR=1.E-6, &
         XMRmin=1.E6*DMRmin, XMRmax=1.E6*DMRmax
      INTEGER, PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax

      INTEGER INDEXS, INDEXR, I, J
      REAL, PARAMETER :: Cice=1.634e13

      REAL ::  NLICE, N0r,Ztot,Zrain,Zice,Zconv
      REAL ::  P1D,T1D,Q1D,C1D, &
               FI1D,FR1D,FS1D,CUREFL, &
               QW1,QI1,QR1,QS1, &
               DBZ1

      real :: CN0r_DMRmax, CN0r_DMRmin, RQR_DRmin, RQR_DRmax, CN0R0
      REAL :: FLARGE, FSMALL, WV, ESAT, RD, TC, WC, FPVS_MINE, D608, RHO, &
              QSAT, RRHO, RQR, ZMIN, Fice, Frain, Rimef , XLI, QICE, DRmm, &
              DLI, XSIMASS, XLIMASS, DUM, WVQW, QSIGRD, QLICE, FLIMASS

      REAL, PARAMETER ::                                         &
     &  RHgrd=1.                                                        &
     & ,T_ICE=-40.                                                      &
     & ,NLImax=5.E3                                                     &
     & ,NLImin=1.E3                                                     &
     & ,N0r0=8.E6                                                       &
     & ,N0rmin=1.E4



        RD=287.04
        D608=0.608
!        RQR_DRmin=N0r0*MASSR(MDRmin)    ! Rain content for mean drop diameter of .05 mm
!        RQR_DRmax=N0r0*MASSR(MDRmax)    ! Rain content for mean drop diameter of .45 mm


! ---------
      Zmin=10.**(0.1*DBZmin)
          QW1=0.
          QI1=0.
          QR1=0.
          QS1=0.
          DBZ1=DBZmin
!          DBZR1=DBZmin
!          DBZI1=DBZmin
!          DBZC1=DBZmin

          Zrain=0.            !--- Radar reflectivity from rain
          Zice=0.             !--- Radar reflectivity from ice
          Zconv=CUREFL   !--- Radar reflectivity from convection
          IF (C1D .LE. EPSQ) THEN
!
!--- Skip rest of calculatiions if no condensate is present
!
            GO TO 10
          ELSE
            WC=C1D
          ENDIF
!
!--- Code below is from GSMDRIVE for determining:
!    QI1 - total ice (cloud ice & snow) mixing ratio
!    QW1 - cloud water mixing ratio
!    QR1 - rain mixing ratio
!
          TC=T1D-TFRZ
          Fice=FI1D
          Frain=FR1D
          IF (TC.LE.T_ICE .OR. Fice.GE.1.) THEN
            QI1=WC
          ELSE IF (Fice .LE. 0.) THEN
            QW1=WC
          ELSE
            QI1=Fice*WC
            QW1=WC-QI1
          ENDIF
          IF (QW1.GT.0. .AND. Frain.GT.0.) THEN
            IF (Frain .GE. 1.) THEN
              QR1=QW1
              QW1=0.
            ELSE
              QR1=Frain*QW1
              QW1=QW1-QR1
            ENDIF
          ENDIF
          WV=Q1D/(1.-Q1D)
!
!--- Saturation vapor pressure w/r/t water ( >=0C ) or ice ( <0C )
!
          ESAT=1000.*FPVS_MINE(T1D,TBPVS_STATE)

        if (I .eq. 44 .and. J .eq. 247) then
        write(0,*) 'T1D, FPVS_MINE(T1D), ESAT: ', T1D, FPVS_MINE(T1D, TBPVS_STATE), ESAT
        endif

          QSAT=EPS*ESAT/(P1D-ESAT)
          RHO=P1D/(RD*T1D*(1.+D608*Q1D))

        if (I .eq. 44 .and. J .eq. 247) then
        write(0,*) 'QR1, RHO: ', QR1, RHO
        endif

          RRHO=1./RHO
  !
  !--- Based on code from GSMCOLUMN in model to determine reflectivity from rain
  !
          IF (QR1 .GT. EPSQ) THEN
            RQR=RHO*QR1
        if (I .eq. 44 .and. J .eq. 247) then
        write(0,*) 'RQR, RQR_DRmin, RQR_DRmax: ', RQR, RQR_DRmin, RQR_DRmax
        endif
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
            Zrain=0.72*N0r*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm*DRmm
        if (I .eq. 44 .and. J .eq. 247) then
        write(0,*) 'DRmm, N0r, Zrain: ', DRmm, N0R, Zrain
        endif

          ENDIF        !--- End IF (QR1 .GT. EPSQ) block
!
!--- Based on code from GSMCOLUMN in model to determine partition of
!    total ice into cloud ice & snow (precipitation ice)
!
          IF (QI1 .GT. EPSQ) THEN
            QICE=QI1
            RHO=P1D/(RD*T1D*(1.+ONEPS*Q1D))
            RRHO=1./RHO
            QSIgrd=RHgrd*QSAT
            WVQW=WV+QW1
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
!  * NLICE   - time-averaged number concentration of large ice
!
            IF (TC.GE.0. .OR. WVQW.LT.QSIgrd) THEN
              FLARGE=1.
            ELSE
              FLARGE=.2
              IF (TC.GE.-8. .AND. TC.LE.-3.) FLARGE=.5*FLARGE
            ENDIF
            FSMALL=(1.-FLARGE)/FLARGE
            XSIMASS=RRHO*MASSI(MDImin)*FSMALL
            DUM=XMImax*EXP(.0536*TC)
            INDEXS=MIN(MDImax, MAX(MDImin, INT(DUM) ) )
            RimeF=AMAX1(1., FS1D )
            XLIMASS=RRHO*RimeF*MASSI(INDEXS)
            FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
            QLICE=FLIMASS*QICE
            NLICE=QLICE/XLIMASS
            IF (NLICE.LT.NLImin .OR. NLICE.GT.NLImax) THEN
!
!--- Force NLICE to be between NLImin and NLImax
!
              DUM=MAX(NLImin, MIN(NLImax, NLICE) )
              XLI=RHO*(QICE/DUM-XSIMASS)/RimeF
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
                IF (DUM .GE. NLImax) &
                 RimeF=RHO*(QICE/NLImax-XSIMASS)/MASSI(INDEXS)
              ENDIF             ! End IF (XLI .LE. MASSI(MDImin) )
              XLIMASS=RRHO*RimeF*MASSI(INDEXS)
              FLIMASS=XLIMASS/(XLIMASS+XSIMASS)
              QLICE=FLIMASS*QICE
              NLICE=QLICE/XLIMASS
            ENDIF               ! End IF (NLICE.LT.NLImin ...
            QS1=AMIN1(QI1, QLICE)
            QI1=AMAX1(0., QI1-QS1)
   !
   !--- Equation (C.8) in Ferrier (1994, JAS, p. 272), which when
   !    converted from cgs units to mks units results in the same
   !    value for Cice, which is equal to the {} term below:
   !
   !    Zi={.224*720*(10**18)/[(PI*RHOL)**2]}*(RHO*QLICE)**2/NLICE,
   !    where RHOL=1000 kg/m**3 is the density of liquid water
   !
   !--- Valid only for exponential ice distributions
   !
            Zice=Cice*RHO*RHO*QLICE*QLICE/NLICE
          ENDIF                 ! End IF (QI1 .GT. 0.) THEN
!
!---  Calculate total (convective + grid-scale) radar reflectivity
10        Ztot=Zrain+Zice+Zconv
          IF (Ztot .GT. Zmin)  DBZ1= 10.*ALOG10(Ztot)

        if (DBZ1 .gt. 64.5) then
        write(0,*) 'Zice, Zrain, Zconv, Ztot, DBZ1: ', Zice, Zrain, Zconv, Ztot, DBZ1
        write(0,*) 'Cice, rho, qlice, nlice: ', Cice, rho, qlice, nlice
        endif

!          IF (Zrain .GT. Zmin) DBZR1=10.*ALOG10(Zrain)
!          IF (Zice .GT. Zmin)  DBZI1=10.*ALOG10(Zice)
!          IF (Zconv .GT. Zmin) DBZC1=10.*ALOG10(Zconv)

!       print*, 'DBZ1: ', DBZ1

      RETURN
      END SUBROUTINE CALMICT
!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                     REAL   FUNCTION FPVS_MINE(T, TBPVS_STATE)
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

!      USE MODULE_MP_ETANEW, ONLY: NX,XMIN,XMAX

      IMPLICIT NONE
        integer, parameter :: NX=7501
      REAL, intent(in) :: TBPVS_STATE(NX)
      real,INTENT(IN) :: T
      real XJ
      integer :: JX

        real, parameter:: XMIN=180.0
        real, parameter:: XMAX=330.0

      REAL, PARAMETER :: XINC=(XMAX-XMIN)/(NX-1)
      REAL, PARAMETER :: C1XPVS=1.-XMIN/XINC
      REAL, PARAMETER :: C2XPVS=1./XINC

!-----------------------------------------------------------------------
      XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),FLOAT(NX))
      JX=MIN(XJ,NX-1.)
      FPVS_MINE=TBPVS_STATE(JX)+(XJ-JX)*(TBPVS_STATE(JX+1)-TBPVS_STATE(JX))
!
      END FUNCTION FPVS_MINE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


        SUBROUTINE MICRO_INIT(MASSI,MASSR,RQR_DRmin,RQR_DRmax,CN0r0,CN0r_DMRmin,CN0r_DMRmax)


        IMPLICIT NONE

      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
      LOGICAL :: opened

      REAL ::  C_N0r0,           &
     &  CN0r0, CN0r_DMRmin, CN0r_DMRmax,      &
     &  RQR_DRmin, RQR_DRmax

      REAL, PARAMETER :: DMImin=.05e-3, DMImax=1.e-3, &
       XMImin=1.e6*DMImin, XMImax=1.e6*DMImax

      INTEGER, PARAMETER :: MDImin=XMImin, MDImax=XMImax

      REAL, PARAMETER :: DMRmin=.05E-3, DMRmax=1.E-3, DelDMR=1.E-6, &
        XMRmin=1.E6*DMRmin, XMRmax=1.E6*DMRmax, N0r0=8.E6, N0rmin=1.e4

      INTEGER, PARAMETER :: MDRmin=XMRmin, MDRmax=XMRmax

        REAL, PARAMETER :: RHOL=1000.

       REAL ::  MASSR(MDRmin:MDRmax),MASSI(MDImin:MDImax)
       INTEGER :: I, etampnew_unit1
       REAL :: PI

!------------------------ START EXECUTION ------------------------
!
!---  READ IN MASSI FROM LOOKUP TABLES
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
 2061     CONTINUE

      OPEN (UNIT=etampnew_unit1,FILE="ETAMPNEW_DATA",FORM="UNFORMATTED")

        REWIND(etampnew_unit1)
      DO I=1,3
        READ(etampnew_unit1)
      ENDDO
      READ(etampnew_unit1) MASSR
        write(0,*) 'read MASSR ', MASSR(MDRmin), MASSR(MDRmin+400), MASSR(MDRmax)
      DO I=1,5
        READ(etampnew_unit1)
        write(0,*) 'past READ: ', I
      ENDDO
        write(0,*) 'to MASSI read'
        write(0,*) 'size(MASSI): ', size(MASSI)
      READ(etampnew_unit1) MASSI
      CLOSE(etampnew_unit1)
      RQR_DRmin=N0r0*MASSR(MDRmin)    ! Rain content for mean drop diameter of .05 mm
      RQR_DRmax=N0r0*MASSR(MDRmax)    ! Rain content for mean drop diameter of .45 mm
      PI=ACOS(-1.)
      C_N0r0=PI*RHOL*N0r0
      CN0r0=1.E6/C_N0r0**.25
      CN0r_DMRmin=1./(PI*RHOL*DMRmin**4)
      CN0r_DMRmax=1./(PI*RHOL*DMRmax**4)
        ENDIF

        CALL wrf_dm_bcast_bytes ( MASSR , size ( MASSR ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( MASSI , size ( MASSI ) * RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( RQR_DRmin, RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( RQR_DRmax, RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( CN0r0, RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( CN0r_DMRmin, RWORDSIZE )
        CALL wrf_dm_bcast_bytes ( CN0r_DMRmax, RWORDSIZE )
      print *,'MICROINIT: MDRmin, MASSR(MDRmin)=',MDRmin,MASSR(MDRmin)
      print *,'MICROINIT: MDRmax, MASSR(MDRmax)=',MDRmax,MASSR(MDRmax)
        print*, 'RQR_DRmin, RQR_DRmax: ', RQR_DRmin, RQR_DRmax

        END SUBROUTINE MICRO_INIT
