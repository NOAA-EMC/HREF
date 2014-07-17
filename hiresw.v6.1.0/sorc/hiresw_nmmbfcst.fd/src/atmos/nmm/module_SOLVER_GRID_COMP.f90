










































!-----------------------------------------------------------------------
!
      MODULE module_SOLVER_GRID_COMP
!
!-----------------------------------------------------------------------
!
!***  This module holds the Solver component's  Register, Init, Run, 
!***  and Finalize routines.  They are called from the DOMAIN component
!***  (DOMAIN_INITIALIZE calls SOLVER_INITIALIZE, etc.) 
!***  in MODULE_DOMAIN_GRID_COMP.F90.
!
!-----------------------------------------------------------------------
! HISTORY LOG:
!
!   2008-07-30  Janjic - Add CONVECTION='none' to OPERATIONAL_PHYSICS.
!               Janjic - Fix lower J limit in FFTFHN(WATER).
!   2008-08-23  Janjic - General pressure-sigma hybrid
!               Janjic - Consistent nonhydrostatic correction in the
!                        first term of the pressure gradient force
!   2008-09-03  Black  - Added initialization of boundary arrays
!                        for nests.
!   2009-03-12  Black  - Changes for general hybrid coordinate.
!   2009-11     Jovic  - Modified for ownership/import/export specification
!   2010-11-03  Pyle   - Modifications/corrections for digital filter.
!   2011-02     Yang   - Updated to use both the ESMF 4.0.0rp2 library,
!                        ESMF 5 series library and the the
!                        ESMF 3.1.0rp2 library.
!   2011-05-12  Yang   - Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!   2011-12-22  Jovic  - Combined Dyn and Phy into single component.
!
!   2012-02-08  Yang   - Modified for using the ESMF 5.2.0rp1 library.
!   2012-04-06  Juang  - add passing argument for gbphys for idea
!   2012-07-20  Black  - Modified for generational usage.
!-----------------------------------------------------------------------
!
      USE esmf_mod
      USE MODULE_INCLUDE
      USE MODULE_VARS_STATE
      USE MODULE_SOLVER_INTERNAL_STATE                                     !<-- Horizontal loop limits obtained here
!
      USE MODULE_MY_DOMAIN_SPECS, IDS_share=>IDS,IDE_share=>IDE         &
                                 ,IMS_share=>IMS,IME_share=>IME         &
                                 ,ITS_share=>ITS,ITE_share=>ITE         &
                                 ,JDS_share=>JDS,JDE_share=>JDE         &
                                 ,JMS_share=>JMS,JME_share=>JME         &
                                 ,JTS_share=>JTS,JTE_share=>JTE 
!
      USE MODULE_EXCHANGE,ONLY: HALO_EXCH
!
      USE MODULE_GET_CONFIG
!
      USE MODULE_CONTROL,ONLY : NUM_DOMAINS_MAX,TIMEF
!
      USE MODULE_CONSTANTS,ONLY : A2,A3,A4,CAPPA,CP,ELIV,ELWV,EPSQ,G &
                                 ,P608,PQ0,R_D,TIW
!
      USE MODULE_DIAGNOSE,ONLY : EXIT,FIELD_STATS                       &
                                ,MAX_FIELDS,MAX_FIELDS_HR,MAX_FIELDS_W6 &
                                ,HMAXMIN,TWR,VMAXMIN,VWR,WRT_PCP        &
                                ,LAT_LON_BNDS
!
      USE MODULE_OUTPUT,ONLY: POINT_OUTPUT
!
      USE MODULE_CLOCKTIMES,ONLY : INTEGRATION_TIMERS,TIMERS
!
      USE MODULE_ERR_MSG,ONLY: ERR_MSG,MESSAGE_CHECK
!
      USE MODULE_FLTBNDS,ONLY : POLEHN,POLEWN,SWAPHN,SWAPWN
!
      USE MODULE_RADIATION  ,ONLY : RADIATION
      USE MODULE_RA_GFDL    ,ONLY : GFDL_INIT,RDTEMP,TIME_MEASURE
      USE MODULE_RA_RRTM    ,ONLY : RRTM_INIT
      USE MODULE_TURBULENCE ,ONLY : TURBL
      USE MODULE_SF_JSFC    ,ONLY : JSFC_INIT
      USE MODULE_BL_MYJPBL  ,ONLY : MYJPBL_INIT
      USE MODULE_LS_NOAHLSM ,ONLY : DZSOIL,NOAH_LSM_INIT                &
                                   ,NUM_SOIL_LAYERS,SLDPTH
      USE MODULE_CU_BMJ     ,ONLY : BMJ_INIT
      USE MODULE_CU_SAS     ,ONLY : SAS_INIT
      USE MODULE_CONVECTION ,ONLY : CUCNVC

      USE MODULE_MICROPHYSICS_NMM ,ONLY : GSMDRIVE                      &
                                         ,MICRO_RESTART
      USE MODULE_MP_ETANEW    ,ONLY : FERRIER_INIT
      USE MODULE_MP_FER_HIRES ,ONLY : FERRIER_INIT_HR
      USE MODULE_MP_WSM6      ,ONLY : WSM6INIT
      USE MODULE_MP_GFS       ,ONLY : GFSMP_INIT

      USE MODULE_H_TO_V ,ONLY : H_TO_V,H_TO_V_TEND
      USE MODULE_GWD    ,ONLY : GWD_INIT
      USE MODULE_PRECIP_ADJUST
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: SOLVER_REGISTER                                            
!
      INTEGER(kind=KINT),PUBLIC :: IM,JM,LM,RESTVAL
!
      INTEGER(kind=KINT) :: START_YEAR,START_MONTH,START_DAY            &
                           ,START_HOUR,START_MINUTE,START_SECOND
!
      INTEGER(kind=KINT),SAVE :: JC
!
      INTEGER(kind=KINT) :: NUM_PES
!
      LOGICAL(kind=KLOG) :: I_AM_A_NEST                                    !<-- Flag indicating if DOMAIN Component is a nest
!
      TYPE(SOLVER_INTERNAL_STATE),POINTER :: INT_STATE                     !<-- The Solver component internal state pointer.
!
      TYPE(ESMF_Logical),SAVE :: MOVE_NOW                               &  !<-- Flag indicating if nested moves this timestep
!                               ,MY_DOMAIN_MOVES                        &  !<-- Flag indicating if nested domain moves
                                ,NEST_FLAG                                 !<-- Flag indicating if DOMAIN Component is a nest
!
      REAL(kind=KFPT),SAVE :: PT
!
!-----------------------------------------------------------------------
!***  For determining clocktimes of various pieces of the Solver.
!-----------------------------------------------------------------------
!
      REAL(kind=KDBL) :: btim,btim0
!
      TYPE(INTEGRATION_TIMERS),POINTER :: TD
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE SOLVER_REGISTER(GRID_COMP,RC_REG)
!
!-----------------------------------------------------------------------
!***  Register the Solver component's Initialize, Run, and Finalize
!***  subroutine names.
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument variables
!------------------------
!
      TYPE(ESMF_GridComp) :: GRID_COMP                                    !<-- The Solver Gridded Component
!
      INTEGER(kind=KINT),INTENT(OUT) :: RC_REG                            !<-- Return code for Solver register
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER(kind=KINT) :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC    =ESMF_SUCCESS
      RC_REG=ESMF_SUCCESS                                                 !<-- Initialize error signal variable
!
!-----------------------------------------------------------------------
!***  Register the Solver initialize subroutine.  Since it is just one
!***  subroutine, use ESMF_SINGLEPHASE.  The second argument is
!***  a pre-defined subroutine type, such as ESMF_SETINIT, ESMF_SETRUN,
!***  or ESMF_SETFINAL.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for Solver Initialize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(GRID_COMP                         &  !<-- The gridded component
                                     ,ESMF_SETINIT            &  !<-- Predefined subroutine type
                                     ,SOLVER_INITIALIZE                 &  !<-- User's subroutineName
                                     ,ESMF_SINGLEPHASE                  &
                                     ,RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Register the Solver Run subroutine.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for Solver Run"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(GRID_COMP                         &  !<-- gridcomp
                                     ,ESMF_SETRUN                   &  !<-- subroutineType
                                     ,SOLVER_RUN                        &  !<-- user's subroutineName
                                     ,ESMF_SINGLEPHASE                  &
                                     ,RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Register the Solver Finalize subroutine.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for Solver Finalize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(GRID_COMP                         &  !<-- gridcomp
                                     ,ESMF_SETFINAL              &  !<-- subroutineType
                                     ,SOLVER_FINALIZE                   &  !<-- user's subroutineName
                                     ,ESMF_SINGLEPHASE                  &
                                     ,RC)


!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    CALL ERR_MSG(RC,MESSAGE_CHECK,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Check the error signal variable.
!-----------------------------------------------------------------------
!
      IF(RC_REG==ESMF_SUCCESS)THEN
!       WRITE(0,*)" SOLVER_REGISTER SUCCEEDED"
      ELSE
        WRITE(0,*)" SOLVER_REGISTER FAILED"
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SOLVER_REGISTER
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE SOLVER_INITIALIZE (GRID_COMP                           &
                                   ,IMP_STATE                           &
                                   ,EXP_STATE                           &
                                   ,CLOCK_ATM                           &
                                   ,RC_INIT)
!
!-----------------------------------------------------------------------
!***  Carry out all necessary setups for the model Solver.
!-----------------------------------------------------------------------
!
      USE MODULE_CONTROL,ONLY : BOUNDARY_INIT,CONSTS                       !  <-- Subroutines
!
      USE MODULE_INIT_READ_BIN,ONLY : READ_BINARY
      USE MODULE_INIT_READ_NEMSIO,ONLY : READ_NEMSIO
!



      USE MODULE_FLTBNDS,ONLY : PREFFT, PRESMUD


!------------------------
!***  Argument variables
!------------------------
!
      TYPE(ESMF_GridComp) :: GRID_COMP                                     !<-- The Solver gridded component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The Solver Initialize step's import state
                         ,EXP_STATE                                        !<-- The Solver Initialize step's export state
!
      TYPE(ESMF_Clock) :: CLOCK_ATM                                        !<-- The ATM's ESMF Clock
!
      INTEGER,INTENT(OUT) :: RC_INIT
!
!---------------------
!***  Local variables
!---------------------
!
      INTEGER(kind=KINT),SAVE :: N8=8
!
      INTEGER(kind=KINT) :: IDE,IDS,IME,IMS,ITE,ITS                     &
                           ,JDE,JDS,JME,JMS,JTE,JTS
!
      INTEGER(kind=KINT) :: IHALO,JHALO,MPI_COMM_COMP,MY_DOMAIN_ID      &
                           ,MY_DOMAIN_ID_LOC,MYPE,NUM_PES
!
      INTEGER(kind=KINT) :: I,I_INC,IDENOMINATOR_DT                     &
                           ,IEND,IERR,INTEGER_DT                        &
                           ,J,J_INC,JEND,KK,KOUNT,KSE,KSS,L,LL,LMP1     &
                           ,N,NUMERATOR_DT,RC
!
      INTEGER(kind=KINT) :: ITE_H2,ITS_H2,JTE_H2,JTS_H2
!
      INTEGER(kind=KINT),DIMENSION(1:8) :: MY_NEB
!
      REAL(kind=KFPT) :: DPH,DLM,DT,GLATX,GLONX,SB_1,SBD_1,TLATX,TLONX  &
                        ,TPH0_1,TPH0D_1,TLM0_1,TLM0D_1,WB_1,WBD_1       &
                        ,X,Y,Z
!
      REAL(kind=KFPT),DIMENSION(1:2) :: SW_X
!
      REAL(kind=DOUBLE) :: D2R,D_ONE,D_180,PI
!
      LOGICAL(kind=KLOG) :: RUN_LOCAL
!
      CHARACTER(20) :: FIELD_NAME
!
      TYPE(WRAP_SOLVER_INT_STATE) :: WRAP                                  ! <-- This wrap is a derived type which contains
                                                                           !     only a pointer to the internal state.  It is needed
                                                                           !     for using different architectures or compilers.
!
      TYPE(ESMF_Grid) :: GRID                                              !<-- The ESMF Grid
!
      TYPE(ESMF_VM) :: VM                                                  !<-- The ESMF Virtual Machine
!
      TYPE(ESMF_State) :: IMP_STATE_WRITE                                  !<-- The Solver import state
!
      TYPE(ESMF_Field) :: FIELD
!
      TYPE(ESMF_TimeInterval) :: DT_ESMF                                   !<-- The ESMF fundamental timestep (s)
!
      TYPE(ESMF_Config) :: CF                                              !<-- ESMF configure object
!

      TYPE(ESMF_Logical) :: RESTART_ESMF

!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      btim0=timef()
!
!-----------------------------------------------------------------------
!***  Initialize the error signal variables.
!-----------------------------------------------------------------------
!
      RC     =ESMF_SUCCESS
      RC_INIT=ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Allocate the Solver internal state pointer.
!-----------------------------------------------------------------------
!
      ALLOCATE(INT_STATE,STAT=RC)
!
!-----------------------------------------------------------------------
!***  Attach the internal state to the Solver gridded component.
!-----------------------------------------------------------------------
!
      WRAP%INT_STATE=>INT_STATE
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Attach Solver Internal State to the Gridded Component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetInternalState(GRID_COMP                      &  !<-- The Solver gridded component
                                        ,WRAP                           &  !<-- Pointer to the Solver internal state
                                        ,RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Retrieve fundamental domain characteristics from the Solver   
!***  import state and set them in the internal state so they will
!***  always be available to this component.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Get Domain Dimensions from Solver Import State"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='ITS'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%ITS                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='ITE'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%ITE                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='JTS'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%JTS                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='JTE'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%JTE                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='IMS'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%IMS                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='IME'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%IME                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='JMS'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%JMS                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='JME'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%JME                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='IDS'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%IDS                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='IDE'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%IDE                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='JDS'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%JDS                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='JDE'                                &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%JDE                        &  !<-- Put extracted value here
                            ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Get Halo Widths from Solver Import State"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='IHALO'                              &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%IHALO                      &  !<-- Put extracted value here
                            ,rc   =RC)
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='JHALO'                              &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%JHALO                      &  !<-- Put extracted value here
                            ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Get Fcst/Quilt Task Intracomm from Solver Imp State"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
                            ,name ='Fcst/Quilt Intracommunicators'      &  !<-- Name of variable to get from Solver import state
                            ,value=int_state%MPI_COMM_COMP              &  !<-- Put extracted value here
                            ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Extract Task Neighbors from Solver Import State"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!

      CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- The Solver import state
                            ,name     ='MY_NEB'                       &  !<-- Name of the attribute to extract
                            ,count    =N8                             &  !<-- # of items in attribute
                            ,valueList=int_state%MY_NEB               &  !<-- Insert Attribute into Solver internal state
                            ,rc       =RC)







!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Insert the local domain starting limits and the halo width into
!***  the Solver internal state.
!-----------------------------------------------------------------------
!
      ITS=int_state%ITS
      ITE=int_state%ITE
      IMS=int_state%IMS
      IME=int_state%IME
      IDS=int_state%IDS
      IDE=int_state%IDE
!
      JTS=int_state%JTS
      JTE=int_state%JTE
      JMS=int_state%JMS
      JME=int_state%JME
      JDS=int_state%JDS
      JDE=int_state%JDE
!
      int_state%ITS_B1=MAX(ITS,IDS+1)
      int_state%ITE_B1=MIN(ITE,IDE-1)
      int_state%ITS_B2=MAX(ITS,IDS+2)
      int_state%ITE_B2=MIN(ITE,IDE-2)
      int_state%ITS_B1_H1=MAX(ITS-1,IDS+1)
      int_state%ITE_B1_H1=MIN(ITE+1,IDE-1)
      int_state%ITE_B1_H2=MIN(ITE+2,IDE-1)
      int_state%ITS_H1=MAX(ITS-1,IDS)
      int_state%ITE_H1=MIN(ITE+1,IDE)
      int_state%ITS_H2=MAX(ITS-2,IDS)
      int_state%ITE_H2=MIN(ITE+2,IDE)
      int_state%JTS_B1=MAX(JTS,JDS+1)
      int_state%JTE_B1=MIN(JTE,JDE-1)
      int_state%JTS_B2=MAX(JTS,JDS+2)
      int_state%JTE_B2=MIN(JTE,JDE-2)
      int_state%JTS_B1_H1=MAX(JTS-1,JDS+1)
      int_state%JTE_B1_H1=MIN(JTE+1,JDE-1)
      int_state%JTE_B1_H2=MIN(JTE+2,JDE-1)
      int_state%JTS_H1=MAX(JTS-1,JDS)
      int_state%JTE_H1=MIN(JTE+1,JDE)
      int_state%JTS_H2=MAX(JTS-2,JDS)
      int_state%JTE_H2=MIN(JTE+2,JDE)
!
      IHALO=int_state%IHALO
      JHALO=int_state%JHALO
!
      IF(IHALO==JHALO)THEN
        int_state%NHALO=IHALO
      ELSE
        RC_INIT=ESMF_FAILURE
        WRITE(0,*)'Error due to ihalo /= jhalo'
      ENDIF
!
!-----------------------------------------------------------------------
!***  Use ESMF utilities to get information from the configuration file.
!***  The function is similar to reading a namelist.  The GET_CONFIG
!***  routine is the user's.  It extracts values from the config file
!***  and places them in the namelist components of the internal state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Get Configure File Parameters for Solver"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL GET_CONFIG_DIMS (GRID_COMP                                   &
                           ,int_state%INPES,int_state%JNPES             &
                           ,LM                                          &
                           ,int_state%NUM_TRACERS_MET                   &
                           ,int_state%NUM_TRACERS_CHEM                  &
                           ,int_state%PCPHR                             &
                           ,int_state%GFS                               &
                           ,int_state%MICROPHYSICS                      &
                           ,int_state%LMPRATE                           &
                           ,int_state%LNSH, int_state%LNSV              &
                           ,RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  We must know whether or not this is a global domain.  Get the
!***  configure object from the Solver component and extract the
!***  value of 'global'.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Solver_Init: Retrieve Config Object from Solver Component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompGet(gridcomp=GRID_COMP                          &   !<--- The Solver component
                           ,config  =CF                                 &   !<--- The configure (namelist) object
                           ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Solver_Init: Extract GLOBAL from Config File"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ConfigGetAttribute(config=CF                            &  !<-- The configure file object
                                  ,value =int_state%GLOBAL              &  !<-- Put extracted quantity here
                                  ,label ='global:'                     &  !<-- The quantity's label in the configure file
                                  ,rc    =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Retrieve the VM to obtain the task ID and total number of tasks
!***  for the internal state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Get VM from the Solver Gridded Component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompGet(gridcomp=GRID_COMP                          &  !<-- The Solver gridded component
                           ,vm      =VM                                 &  !<-- The ESMF Virtual Machine
                           ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Get Task IDs and Number of MPI Tasks from VM"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_VMGet(vm      =VM                                       &  !<-- The ESMF virtual machine
                     ,localpet=int_state%MYPE                           &  !<-- My task's local rank on this domain
                     ,petcount=int_state%NUM_PES                        &  !<-- Total number of MPI tasks
                     ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  int_state%NUM_PES taken from VM is the total number of tasks 
!***  on this domain including Write/Quilt tasks.  We want only the
!***  number of forecast tasks.
!-----------------------------------------------------------------------
!
      int_state%NUM_PES=int_state%INPES*int_state%JNPES
!
      NUM_PES=int_state%NUM_PES                                            !<-- The number of forecast tasks
      MYPE=int_state%MYPE                                                  !<-- The local task ID
!
!-----------------------------------------------------------------------
!***  Only forecast tasks are needed for the remaining
!***  initialization process.
!-----------------------------------------------------------------------
!
      fcst_tasks: IF(MYPE<NUM_PES)THEN                                     !<-- Select only forecast tasks
!
!-----------------------------------------------------------------------
!***  Allocate all necessary internal state variables.  Those that
!***  are owned/exported are pointed into allocated memory within
!***  the Solver's composite VARS array.  
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Solver_Init: Allocate internal state variables"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL SET_INTERNAL_STATE_SOLVER(INT_STATE                        &
                                      ,LM                               &
                                      ,ITS,ITE,JTS,JTE                  &
                                      ,IMS,IME,JMS,JME                  &
                                      ,IDS,IDE,JDS,JDE                  &
                                      ,IHALO,JHALO                      &
                                      ,MYPE                             &
                                      ,RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract the ESMF Grid from the Solver Component"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_GridCompGet(gridcomp=GRID_COMP                        &  !<-- The Solver gridded component
                             ,grid    =GRID                             &  !<-- The ESMF Grid
                             ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Put the allocated pointers of all export/import variables
!***  into the Solver export/import states.  
!-----------------------------------------------------------------------
!
        CALL PUT_VARS_IN_STATE(int_state%VARS,int_state%NUM_VARS,'X',GRID,EXP_STATE)
!
        CALL PUT_VARS_IN_STATE(int_state%VARS,int_state%NUM_VARS,'I',GRID,IMP_STATE)
!
!-----------------------------------------------------------------------
!
      ENDIF fcst_tasks
!
!-----------------------------------------------------------------------
!***  Use ESMF utilities to get information from the configuration file.
!***  The function is similar to reading a namelist.  The GET_CONFIG
!***  routine is the user's.  It extracts values from the config file
!***  and places them in the namelist components of the internal state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Get Configure File Parameters for Solver"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL GET_CONFIG(GRID_COMP,INT_STATE,RC)                             !<-- User's routine to extract config file information
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Only forecast tasks are needed for the remaining
!***  initialization process.
!-----------------------------------------------------------------------
!
      fcst_tasks2: IF(int_state%MYPE<int_state%NUM_PES)THEN                !<-- Select only forecast tasks
!
!-----------------------------------------------------------------------
!***  Assign the fundamental timestep retrieved from the clock.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract Fundamental Timestep from ATM's Clock"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_ClockGet(clock   =CLOCK_ATM                           &  !<-- The ATM Clock
                          ,timeStep=DT_ESMF                             &  !<-- Fundamental timestep (s) (ESMF)
                          ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Get Real Timestep from ESMF Timestep"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!

        CALL ESMF_TimeIntervalGet(timeinterval=DT_ESMF                  &  !<-- the ESMF timestep
                                 ,s           =INTEGER_DT               &  !<-- the integer part of the timestep in seconds
                                 ,sN          =NUMERATOR_DT             &  !<-- the numerator of the fractional second
                                 ,sD          =IDENOMINATOR_DT          &  !<-- the denominator of the fractional second
                                 ,rc          =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        int_state%DT=REAL(INTEGER_DT)+REAL(NUMERATOR_DT)                &  !<-- Fundamental tiemstep (s) (REAL)
                                     /REAL(IDENOMINATOR_DT)
        DT=int_state%DT
!
        int_state%NSTEPS_PER_HOUR=NINT(3600./DT)
        int_state%NSTEPS_PER_RESET=NINT(int_state%AVGMAXLEN/DT)
        int_state%NSTEPS_PER_CHECK=MAX(2,NINT(40/DT))
!
!-----------------------------------------------------------------------
!***  Save fundamental timestep to distinguish from filter timestep
!***  which may be shorter
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Set Dyn Timestep to Distinguish from Filter DT"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=IMP_STATE                          &  !<-- The Solver import state
                              ,name ='FUND_DT'                          &  !<-- Name of variable to get from Solver import state
                              ,value=DT                                 &  !<-- Put extracted value here
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        int_state%FIRST_NMM=.TRUE.
!
        int_state%DT_LAST=0.                                               !<-- For use in digital filtering in SOLVE_RUN
        int_state%DT_TEST_RATIO=0.                                         !<-- For use in digital filtering in SOLVE_RUN
!
!-----------------------------------------------------------------------
!***  Retrieve the domain ID from the Solver import state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Get Domain ID from Solver Import State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeGet(state=IMP_STATE                          &  !<-- The Solver import state
                              ,name ='DOMAIN_ID'                        &  !<-- Name of variable to get from Solver import state
                              ,value=MY_DOMAIN_ID_LOC                   &  !<-- Put extracted value here
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        int_state%MY_DOMAIN_ID=MY_DOMAIN_ID_LOC
!
!-----------------------------------------------------------------------
!***  Retrieve the import state of the Write gridded component
!***  from the Solver export state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Write Import State from Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_StateGet(state      =EXP_STATE                        &  !<-- The Solver export state
                          ,itemName   ='Write Import State'             &  !<-- Name of the state to get from Solver export state
                          ,nestedState=IMP_STATE_WRITE                  &  !<-- Extract write component import state from Solver export
                          ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Initialize allocated arrays.
!-----------------------------------------------------------------------
!
        DO N=1,2
        DO L=1,LM
        DO LL=1,int_state%LNSV
        DO I=IMS,IME
          int_state%UBN(I,LL,L,N)=-1.E6
          int_state%UBS(I,LL,L,N)=-1.E6
          int_state%VBN(I,LL,L,N)=-1.E6
          int_state%VBS(I,LL,L,N)=-1.E6
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
        DO N=1,2
        DO L=1,LM
        DO J=JMS,JME
        DO LL=1,int_state%LNSV
          int_state%UBE(LL,J,L,N)=-1.E6
          int_state%UBW(LL,J,L,N)=-1.E6
          int_state%VBE(LL,J,L,N)=-1.E6
          int_state%VBW(LL,J,L,N)=-1.E6
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
        IF(.NOT.int_state%GLOBAL)THEN
!
          DO N=1,2
          DO LL=1,int_state%LNSH
          DO I=IMS,IME
            int_state%PDBN(I,LL,N)=0.
            int_state%PDBS(I,LL,N)=0.
          ENDDO
          ENDDO
          ENDDO
!
          DO N=1,2
          DO J=JMS,JME
          DO LL=1,int_state%LNSH
            int_state%PDBE(LL,J,N)=0.
            int_state%PDBW(LL,J,N)=0.
          ENDDO
          ENDDO
          ENDDO
!
          int_state%NUM_WORDS_BC_SOUTH=-1                                    !<-- Word counts of 1-D boundary data strings
          int_state%NUM_WORDS_BC_NORTH=-1                                    !
          int_state%NUM_WORDS_BC_WEST =-1                                    !
          int_state%NUM_WORDS_BC_EAST =-1                                    !<--
!
        ENDIF
!
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%PD(I,J)=0.
          int_state%PDO(I,J)=0.
        ENDDO
        ENDDO
!
        DO L=1,LM-1
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%PSGDT(I,J,L)=0.
        ENDDO
        ENDDO
        ENDDO
!
        DO L=1,LM
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%Told(I,J,L)=0.
          int_state%Tadj(I,J,L)=0.
          int_state%F_ICE(I,J,L)=0.
          int_state%F_RAIN(I,J,L)=0.
          int_state%F_RIMEF(I,J,L)=0.
        ENDDO
        ENDDO
        ENDDO
!
        DO N=1,int_state%NUM_TRACERS_MET
        DO L=1,LM
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%TRACERS     (I,J,L,N)=1.E-20
          int_state%TRACERS_SQRT(I,J,L,N)=1.E-20
          int_state%TRACERS_PREV(I,J,L,N)=1.E-20
          int_state%TRACERS_TEND(I,J,L,N)=1.E-20
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
        DO L=1,LM
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%Q2(I,J,L)=0.02
          int_state%W_TOT(I,J,L)=0.
        ENDDO
        ENDDO
        ENDDO
!
        DO L=1,LM
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%TCT(I,J,L) =-1.E6
          int_state%TCU(I,J,L) =-1.E6
          int_state%TCV(I,J,L) =-1.E6
        ENDDO
        ENDDO
        ENDDO
!
        int_state%I_PAR_STA=0
        int_state%J_PAR_STA=0
        int_state%NMTS=-999
!
        DO L=1,LM
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%Q2(I,J,L)=0.02
          int_state%OMGALF(I,J,L)=0.
          int_state%T(I,J,L)=-1.E6
          int_state%U(I,J,L)=-1.E6
          int_state%V(I,J,L)=-1.E6

          int_state%RLWTT(I,J,L)=0.
          int_state%RSWTT(I,J,L)=0.

          int_state%EXCH_H(I,J,L)=0.
          int_state%XLEN_MIX(I,J,L)=0.

          int_state%CLDFRA(I,J,L)=0.
          int_state%TRAIN(I,J,L) =0.
          int_state%TCUCN(I,J,L) =0.
        ENDDO
        ENDDO
        ENDDO
!
        DO L=1,NUM_SOIL_LAYERS
          int_state%SLDPTH(L)=SLDPTH(L)
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%SMC(I,J,L)=-1.E6
          int_state%STC(I,J,L)=-1.E6
          int_state%SH2O(I,J,L)=-1.E6
        ENDDO
        ENDDO
        ENDDO
!
        DO L=1,MICRO_RESTART
          int_state%MP_RESTART_STATE(L)=0.
          int_state%TBPVS_STATE(L)=0.
          int_state%TBPVS0_STATE(L)=0.
        ENDDO
        DO L=1, int_state%MDRMAXout-int_state%MDRMINout+1
           int_state%MASSRout(L)=0.
        ENDDO
        DO L=1, int_state%MDIMAXout-int_state%MDIMINout+1
           int_state%MASSIout(L)=0.
        ENDDO
!
        int_state%NSOIL=NUM_SOIL_LAYERS
!
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%LPBL(I,J)    =-999
          int_state%NCFRCV(I,J)  =-999
          int_state%NCFRST(I,J)  =-999
          int_state%ACFRCV(I,J)  =-1.E6
          int_state%ACFRST(I,J)  =-1.E6
          int_state%AKHS(I,J)    = 0.
          int_state%AKHS_OUT(I,J)= 0.
          int_state%AKMS(I,J)    = 0.
          int_state%AKMS_OUT(I,J)= 0.
          int_state%ALBASE(I,J)  =-1.E6
          int_state%ALBEDO(I,J)  =-1.E6
          int_state%ALWIN(I,J)   =-1.E6
          int_state%ALWOUT(I,J)  =-1.E6
          int_state%ALWTOA(I,J)  =-1.E6
          int_state%ASWIN(I,J)   =-1.E6
          int_state%ASWOUT(I,J)  =-1.E6
          int_state%ASWTOA(I,J)  =-1.E6
          int_state%BGROFF(I,J)  =-1.E6
          int_state%CFRACH(I,J)  =-1.E6
          int_state%CFRACM(I,J)  =-1.E6
          int_state%CFRACL(I,J)  =-1.E6
          int_state%CNVBOT(I,J)  =0.0
          int_state%CNVTOP(I,J)  =0.0
          int_state%CMC(I,J)     =-1.E6
          int_state%CPRATE(I,J)  =0.0
          int_state%CUPPT(I,J)   =-1.E6
          int_state%CZMEAN(I,J)  =-1.E6
          int_state%CZEN(I,J)    =-1.E6
          int_state%LSPA(I,J)    =-1.E6
          int_state%EPSR(I,J)    =-1.E6
          int_state%FIS(I,J)     =-1.E6
          int_state%HBOT(I,J)    =-1.E6
          int_state%HBOTD(I,J)   =-1.E6
          int_state%HBOTS(I,J)   =-1.E6
          int_state%HTOP(I,J)    =-1.E6
          int_state%HTOPD(I,J)   =-1.E6
          int_state%HTOPS(I,J)   =-1.E6
          int_state%GRNFLX(I,J)  = 0.
          int_state%MAVAIL(I,J)  = 1.
          int_state%MXSNAL(I,J)  =-1.E6
          int_state%PBLH(I,J)    =-1.E6
          int_state%MIXHT(I,J)   =0.
          int_state%PD(I,J)      =-1.E6
          int_state%POTEVP(I,J)  = 0.
          int_state%POTFLX(I,J)  =-1.E6
          int_state%QSH(I,J)     = 0.
          int_state%QWBS(I,J)    =-1.E6
          int_state%QZ0(I,J)     = 0.
          int_state%RADOT(I,J)   = 0.
          int_state%RLWIN(I,J)   = 0.
          int_state%RMOL(I,J)    =-1.E6
          int_state%RSWIN(I,J)   = 0.
          int_state%RSWINC(I,J)  = 0.
          int_state%RSWOUT(I,J)  = 0.
          int_state%RLWTOA(I,J)  = 0.
          int_state%RSWTOA(I,J)  = 0.
          int_state%SFCEVP(I,J)  = 0.
          int_state%SFCEXC(I,J)  = 0.
          int_state%SFCLHX(I,J)  =-1.E6
          int_state%SFCSHX(I,J)  =-1.E6
          int_state%SICE(I,J)    =-1.E6
          int_state%SIGT4(I,J)   =-1.E6
          int_state%SM(I,J)      =-1.E6
          int_state%SMSTAV(I,J)  = 0.
          int_state%SMSTOT(I,J)  = 0.
          int_state%SNO(I,J)     = 0.
          int_state%SNOPCX(I,J)  =-1.E6
          int_state%SOILTB(I,J)  = 273.
          int_state%SR(I,J)      =-1.E6
          int_state%SSROFF(I,J)  = 0.
          int_state%SST(I,J)     = 273.
          int_state%SUBSHX(I,J)  =-1.E6
          int_state%THS(I,J)     =-1.E6
          int_state%THZ0(I,J)    = 273.
          int_state%TSKIN(I,J)   =-1.E6
          int_state%TWBS(I,J)    =-1.E6
          int_state%USTAR(I,J)   = 0.1
          int_state%UZ0(I,J)     = 0.
          int_state%VEGFRC(I,J)  =-1.E6
          int_state%VZ0(I,J)     = 0.
          int_state%Z0(I,J)      =-1.E6
          int_state%Z0BASE(I,J)  =-1.E6
          int_state%STDH(I,J)    =-1.E6
          int_state%CROT(I,J)    = 0.
          int_state%SROT(I,J)    = 0.
          int_state%HSTDV(I,J)   = 0.
          int_state%HCNVX(I,J)   = 0.
          int_state%HASYW(I,J)   = 0.
          int_state%HASYS(I,J)   = 0.
          int_state%HASYSW(I,J)  = 0.
          int_state%HASYNW(I,J)  = 0.
          int_state%HLENW(I,J)   = 0.
          int_state%HLENS(I,J)   = 0.
          int_state%HLENSW(I,J)  = 0.
          int_state%HLENNW(I,J)  = 0.
          int_state%HANGL(I,J)   = 0.
          int_state%HANIS(I,J)   = 0.
          int_state%HSLOP(I,J)   = 0.
          int_state%HZMAX(I,J)   = 0.
        ENDDO
        ENDDO
!
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%ACSNOM(I,J)= 0.
          int_state%ACSNOW(I,J)= 0.
          int_state%ACPREC(I,J)= 0.
          int_state%CUPREC(I,J)= 0.
          int_state%PREC(I,J)  = 0.
          int_state%CLDEFI(I,J)= 0.
          int_state%PSHLTR(I,J)= 1.E5
          int_state%P10(I,J)   = 1.E5
          int_state%PSFC(I,J)  = 1.E5
          int_state%Q02(I,J)   = 0.
          int_state%Q10(I,J)   = 0.
          int_state%QSHLTR(I,J)= 0.
          int_state%T2(I,J)    = 273.
          int_state%TH02(I,J)  = 0.
          int_state%TH10(I,J)  = 273.
          int_state%TSHLTR(I,J)= 273.
          int_state%U10(I,J)   = 0.
          int_state%V10(I,J)   = 0.
          int_state%TLMIN(I,J) = 0.
          int_state%TLMAX(I,J) = 0.

          int_state%ACUTIM(I,J)= 0.
          int_state%APHTIM(I,J)= 0.
          int_state%ARDLW(I,J) = 0.
          int_state%ARDSW(I,J) = 0.
          int_state%ASRFC(I,J) = 0.
          int_state%AVRAIN(I,J)= 0.
          int_state%AVCNVC(I,J)= 0.
        ENDDO
        ENDDO
!
        DO L=1,LM
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%Told(I,J,L)=0.
          int_state%Tadj(I,J,L)=0.
          int_state%F_ICE(I,J,L)=0.
          int_state%F_RAIN(I,J,L)=0.
          int_state%F_RIMEF(I,J,L)=0.
          do KK=1,int_state%d_ss
            int_state%MPRATES(I,J,L,KK)=0.
          enddo
        ENDDO
        ENDDO
        ENDDO
!
        DO N=1,NUM_DOMAINS_MAX
          int_state%NTSCM(N)=-999
        ENDDO
!
!-----------------------------------------------------------------------
!***  Initialize the timer variables now.
!-----------------------------------------------------------------------
!
        TD=>TIMERS(MY_DOMAIN_ID_LOC)                                       !<-- Abbreviate the name of this domain's timers
!
        td%adv1_tim=0.
        td%adv2_tim=0.
        td%bocoh_tim=0.
        td%bocov_tim=0.
        td%cdwdt_tim=0.
        td%cdzdt_tim=0.
        td%consts_tim=0.
        td%ddamp_tim=0.
        td%dht_tim=0.
        td%exch_tim=0.
        td%fftfhn_tim=0.
        td%fftfwn_tim=0.
        td%hadv2_tim=0.
        td%hdiff_tim=0.
        td%mono_tim=0.
        td%pdtsdt_tim=0.
        td%pgforce_tim=0.
        td%poavhn_tim=0.
        td%polehn_tim=0.
        td%pole_swap_tim=0.
        td%polewn_tim=0.
        td%prefft_tim=0.
        td%presmud_tim=0.
        td%solver_init_tim=0.
        td%solver_dyn_tim=0.
        td%solver_phy_tim=0.
        td%swaphn_tim=0.
        td%swapwn_tim=0.
        td%updatet_tim=0.
        td%vadv2_tim=0.
        td%vsound_tim=0.
        td%vtoa_tim=0.
!
        td%cucnvc_tim=0.
        td%gsmdrive_tim=0.
        td%h_to_v_tim=0.
        td%radiation_tim=0.
        td%rdtemp_tim=0.
        td%turbl_tim=0.
        td%adjppt_tim=0.
        td%gfs_phy_tim=0.
!
!-----------------------------------------------------------------------
!
        ITS=int_state%ITS
        ITE=int_state%ITE
        JTS=int_state%JTS
        JTE=int_state%JTE
        IMS=int_state%IMS
        IME=int_state%IME
        JMS=int_state%JMS
        JME=int_state%JME
        IDS=int_state%IDS
        IDE=int_state%IDE
        JDS=int_state%JDS
        JDE=int_state%JDE
!
        IHALO=int_state%IHALO    
        JHALO=int_state%JHALO    
!
        MYPE=int_state%MYPE
        MY_DOMAIN_ID=int_state%MY_DOMAIN_ID
        MPI_COMM_COMP=int_state%MPI_COMM_COMP
        NUM_PES=int_state%NUM_PES
!
        DO N=1,8
          MY_NEB(N)=int_state%MY_NEB(N)
        ENDDO
!
!-----------------------------------------------------------------------
!***  Extract all forecast tasks' horizontal subdomain limits
!***  from the Solver import state and give them to the
!***  Solver internal state.
!***  This is necessary if quilting is selected because these
!***  limits will be taken from the Solver internal state,
!***  placed into the Write components' import states and
!***  used for the combining of local domain data onto the
!***  global domain.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Local Domain Limits to Solver Internal State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- The Solver import state
                              ,name     ='LOCAL_ISTART'                 &  !<-- Name of the attribute to extract
                              ,count=NUM_PES                        &  !<-- # of items in attribute
                              ,valueList=int_state%LOCAL_ISTART         &  !<-- Insert Attribute into Solver internal state
                              ,rc       =RC)
!
        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- The Solver import state
                              ,name     ='LOCAL_IEND'                   &  !<-- Name of the attribute to extract
                              ,count=NUM_PES                        &  !<-- # of items in attribute
                              ,valueList=int_state%LOCAL_IEND           &  !<-- Insert Attribute into Solver internal state
                              ,rc       =RC)
!
        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- The Solver import state
                              ,name     ='LOCAL_JSTART'                 &  !<-- Name of the attribute to extract
                              ,count=NUM_PES                        &  !<-- # of items in attribute
                              ,valueList=int_state%LOCAL_JSTART         &  !<-- Insert Attribute into Solver internal state
                              ,rc       =RC)
!
        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- The Solver import state
                              ,name     ='LOCAL_JEND'                   &  !<-- Name of the attribute to extract
                              ,count=NUM_PES                        &  !<-- # of items in attribute
                              ,valueList=int_state%LOCAL_JEND           &  !<-- Insert Attribute into Solver internal state
                              ,rc       =RC)

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  The input file is about to be read and halo exchanges will be
!***  done in conjunction with that process.  The halo exchange
!***  routines require 15 domain-related variables so set them now.
!-----------------------------------------------------------------------
!
        CALL SET_DOMAIN_SPECS(int_state%ITS,int_state%ITE               &
                             ,int_state%JTS,int_state%JTE               &
                             ,int_state%IMS,int_state%IME               &
                             ,int_state%JMS,int_state%JME               &
                             ,int_state%IDS,int_state%IDE               &
                             ,int_state%JDS,int_state%JDE               &
                             ,int_state%IHALO,int_state%JHALO           &
                             ,int_state%MY_DOMAIN_ID                    &
                             ,int_state%MYPE                            &
                             ,int_state%MY_NEB                          &
                             ,int_state%MPI_COMM_COMP                   &
                             ,int_state%NUM_PES                         &
                             ,LOCAL_ISTART_IN=int_state%LOCAL_ISTART    &
                             ,LOCAL_IEND_IN=int_state%LOCAL_IEND        &
                             ,LOCAL_JSTART_IN=int_state%LOCAL_JSTART    &
                             ,LOCAL_JEND_IN=int_state%LOCAL_JEND        &
                              )
!
!-----------------------------------------------------------------------
!***  Read the input file.
!-----------------------------------------------------------------------
!
        KSS=1        
        KSE=int_state%NUM_TRACERS_MET
!
        ITS_H2=MAX(ITS-2,int_state%IDS)
        ITE_H2=MIN(ITE+2,int_state%IDE)
        JTS_H2=MAX(JTS-2,int_state%JDS)
        JTE_H2=MIN(JTE+2,int_state%JDE)
!
        btim=timef()
!
        IF(.NOT.int_state%NEMSIO_INPUT)THEN
!
          CALL READ_BINARY(INT_STATE                                    &
                          ,MY_DOMAIN_ID                                 &
                          ,MPI_COMM_COMP                                &
                          ,int_state%MYPE                               &
                          ,int_state%ITS,int_state%ITE                  &
                          ,int_state%JTS,int_state%JTE                  &
                          ,int_state%IMS,int_state%IME                  &
                          ,int_state%JMS,int_state%JME                  &
                          ,int_state%IDS,int_state%IDE                  &
                          ,int_state%JDS,int_state%JDE                  &
                          ,ITS_H2,ITE_H2,JTS_H2,JTE_H2                  &
                          ,LM                                           &
                          ,RC)
!
          IF (RC /= 0) THEN
            RC_INIT = RC
            RETURN
          END IF
!
        ELSE
!
          CALL READ_NEMSIO(int_state,MY_DOMAIN_ID,RC)
!
          IF (RC /= 0) THEN
            RC_INIT = RC
            RETURN
          END IF
!
        ENDIF
!rv
!  Use this (OPER) for operational run, for having vertical velocity
!  in history file (00hr) when starting from restart file
!rv
        IF(int_state%OPER) THEN
          DO L=1,LM
            DO J=JMS,JME
              DO I=IMS,IME
                int_state%W_TOT(I,J,L)=int_state%W(I,J,L)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
!rv
!
        if (mype==-9999) then
          write(0,*)'solver'
          write(0,*)'ihr,ihrst,lpt2,ntsti,ntstm=',int_state%ihr,int_state%ihrst,int_state%lpt2,int_state%ntsti,int_state%ntstm
          write(0,*)'idat=',int_state%idat(1),int_state%idat(2),int_state%idat(3)
          write(0,*)'dsg1=',minval(int_state%dsg1),maxval(int_state%dsg1)
          write(0,*)'pdsg1=',minval(int_state%pdsg1),maxval(int_state%pdsg1)
          write(0,*)'psgml1=',minval(int_state%psgml1),maxval(int_state%psgml1)
          write(0,*)'sgml1=',minval(int_state%sgml1),maxval(int_state%sgml1)
          write(0,*)'sgml2=',minval(int_state%sgml2),maxval(int_state%sgml2)
          write(0,*)'psg1=',minval(int_state%psg1),maxval(int_state%psg1)
          write(0,*)'sg1=',minval(int_state%sg1),maxval(int_state%sg1)
          write(0,*)'sg2=',minval(int_state%sg2),maxval(int_state%sg2)
          write(0,*)'fis=',minval(int_state%fis),maxval(int_state%fis)
          write(0,*)'pd=',minval(int_state%pd),maxval(int_state%pd)
          write(0,*)'pdo=',minval(int_state%pdo),maxval(int_state%pdo)
          write(0,*)'sice=',minval(int_state%sice),maxval(int_state%sice)
          write(0,*)'sm=',minval(int_state%sm),maxval(int_state%sm)
          write(0,*)'cw=',minval(int_state%cw),maxval(int_state%cw)
          write(0,*)'dwdt=',minval(int_state%dwdt),maxval(int_state%dwdt)
          write(0,*)'q=',minval(int_state%q),maxval(int_state%q)
          write(0,*)'q2=',minval(int_state%q2),maxval(int_state%q2)
          write(0,*)'o3=',minval(int_state%o3),maxval(int_state%o3)
          write(0,*)'omgalf=',minval(int_state%omgalf),maxval(int_state%omgalf)
          write(0,*)'div=',minval(int_state%div),maxval(int_state%div)
          write(0,*)'z=',minval(int_state%z),maxval(int_state%z)
          write(0,*)'rtop=',minval(int_state%rtop),maxval(int_state%rtop)
          write(0,*)'tcu=',minval(int_state%tcu),maxval(int_state%tcu)
          write(0,*)'tcv=',minval(int_state%tcv),maxval(int_state%tcv)
          write(0,*)'tct=',minval(int_state%tct),maxval(int_state%tct)
          write(0,*)'t=',minval(int_state%t),maxval(int_state%t)
          write(0,*)'tp=',minval(int_state%tp),maxval(int_state%tp)
          write(0,*)'u=',minval(int_state%u),maxval(int_state%u)
          write(0,*)'up=',minval(int_state%up),maxval(int_state%up)
          write(0,*)'v=',minval(int_state%v),maxval(int_state%v)
          write(0,*)'vp=',minval(int_state%vp),maxval(int_state%vp)
          write(0,*)'e2=',minval(int_state%e2),maxval(int_state%e2)
          write(0,*)'w=',minval(int_state%w),maxval(int_state%w)
          write(0,*)'w_tot=',minval(int_state%w_tot),maxval(int_state%w_tot)
          write(0,*)'pint=',minval(int_state%pint),maxval(int_state%pint)
          write(0,*)'water=',minval(int_state%water),minval(int_state%water)
          write(0,*)'tracers=',minval(int_state%tracers),maxval(int_state%tracers)
!         write(0,*)'sp=',minval(int_state%sp),maxval(int_state%sp)
          write(0,*)'run=',int_state%run 
        endif
!
!-----------------------------------------------------------------------
!***  Check if starting Date/Time in input data file agrees with
!***  the configure file.
!-----------------------------------------------------------------------
!
        IF(.NOT.int_state%RESTART.AND.MYPE==0)THEN
          IF(int_state%START_HOUR /=int_state%IHRST.OR.                 &
             int_state%START_DAY  /=int_state%IDAT(1).OR.               &
             int_state%START_MONTH/=int_state%IDAT(2).OR.               &
             int_state%START_YEAR /=int_state%IDAT(3))THEN
            WRITE(0,*)' *** WARNING *** WARNING *** WARNING *** '
            WRITE(0,*)' *** WARNING *** WARNING *** WARNING *** '
            WRITE(0,*)' DATES IN INPUT AND CONFIGURE FILES DISAGREE!!'
            WRITE(0,*)' INPUT: HOUR=',int_state%IHRST                   &
                      ,       ' DAY=',int_state%IDAT(1)                 &
                      ,     ' MONTH=',int_state%IDAT(2)                 &
                      ,      ' YEAR=',int_state%IDAT(3)
            WRITE(0,*)' CONFIG: HOUR=',int_state%START_HOUR             &
                      ,        ' DAY=',int_state%START_DAY              &
                      ,      ' MONTH=',int_state%START_MONTH            &
                      ,       ' YEAR=',int_state%START_YEAR
            WRITE(0,*)' *** WARNING *** WARNING *** WARNING *** '
            WRITE(0,*)' *** WARNING *** WARNING *** WARNING *** '
          ENDIF
        ENDIF
!
!-----------------------------------------------------------------------
!
        td%solver_init_tim=td%solver_init_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Nested domains do not have boundary condition files since the
!***  boundary values come from their parents.  However the boundary
!***  variable arrays need to contain initial values before tendencies
!***  from the parent can be added.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Retrieve the Nest/Not_A_Nest flag from the Solver import state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Get Nest/Not-a-Nest Flag from Solver Import State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeGet(state=IMP_STATE                          &  !<-- The Solver import state
                              ,name ='I-Am-A-Nest Flag'                 &  !<-- Name of variable to get from Solver import state

                              ,value=NEST_FLAG                          &  !<-- Put extracted value here



                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!

        int_state%I_AM_A_NEST=NEST_FLAG
!
        IF(NEST_FLAG==ESMF_TRUE)THEN
          I_AM_A_NEST=.TRUE.
        ELSE
          I_AM_A_NEST=.FALSE.
        END IF



!
        IF(I_AM_A_NEST)THEN
!
!-----------------------------------------------------------------------
!
          IF (.NOT. int_state%RESTART )THEN

          CALL BOUNDARY_INIT(ITS,ITE,JTS,JTE,LM                         &
                            ,IMS,IME,JMS,JME                            &
                            ,IDS,IDE,JDS,JDE                            &
                            ,int_state%LNSH,int_state%LNSV              &
                            ,int_state%PD                               &
                            ,int_state%PDBS,int_state%PDBN              &
                            ,int_state%PDBW,int_state%PDBE              &
                            ,int_state%T                                &
                            ,int_state%TBS,int_state%TBN                &
                            ,int_state%TBW,int_state%TBE                &
                            ,int_state%Q                                &
                            ,int_state%QBS,int_state%QBN                &
                            ,int_state%QBW,int_state%QBE                &
                            ,int_state%CW                               &
                            ,int_state%WBS,int_state%WBN                &
                            ,int_state%WBW,int_state%WBE                &
                            ,int_state%U                                &
                            ,int_state%UBS,int_state%UBN                &
                            ,int_state%UBW,int_state%UBE                &
                            ,int_state%V                                &
                            ,int_state%VBS,int_state%VBN                &
                            ,int_state%VBW,int_state%VBE                &
                              )
          END IF
!
!-----------------------------------------------------------------------
!***  Also we need to retrieve the Parent-Child timestep ratio in order
!***  to know how often to update the boundary tendencies.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Get Parent-Child Time Ratio from Solver Import State"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state=IMP_STATE                         &  !<-- The Solver import state
                                ,name ='Parent-Child Time Ratio'         &  !<-- Name of variable to get from Solver import state
                                ,value=int_state%PARENT_CHILD_TIME_RATIO &  !<-- Put extracted value here
                                ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Does this nested domain move?
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Get Nest Move Flag from Solver Import State"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state=IMP_STATE                        &  !<-- The Solver import state
                                ,name ='My Domain Moves'                &  !<-- Name of variable to get from Solver import state
                                ,value=int_state%MY_DOMAIN_MOVES        &  !<-- Put extracted value here
                                ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Currently moving nests are not allowed to use gravity wave drag.
!***  One quantity used in that parameterization is the mountains'
!***  angle with respect to east.  From the moving nest's perspective
!***  the mountains are moving and thus that angle would need to be
!***  updated with each shift of the domain.  That is not handled
!***  yet in the code.
!-----------------------------------------------------------------------
!

          IF(int_state%MY_DOMAIN_MOVES==ESMF_True)THEN



!
            int_state%GWDFLG=.FALSE.
!
          ENDIF
!
!-----------------------------------------------------------------------
!***  If the domain does move and this is a restarted run then the
!***  SW corner of the domain needs to be recomputed to account for
!***  motion that occurred since the beginning of the original forecast.
!***  Anchor the computation to the SW corner of the uppermost parent
!***  so that answers will be bit-identical for all circumstances.
!-----------------------------------------------------------------------
!

          IF(int_state%MY_DOMAIN_MOVES==ESMF_True.AND.int_state%RESTART)THEN



!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
            MESSAGE_CHECK="Solver Init: Extract SW Corner of Domain #1"
!           CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
            CALL ESMF_AttributeGet(state=IMP_STATE                      &  !<-- The Solver import state
                                  ,name ='SBD_1'                        &  !<-- Attribute's name
                                  ,value=SBD_1                          &  !<-- Transformed lat (degrees) of domain #1's south bndry
                                  ,rc   =RC)
!
            CALL ESMF_AttributeGet(state=IMP_STATE                      &  !<-- The Solver import state
                                  ,name ='WBD_1'                        &  !<-- Attribute's name
                                  ,value=WBD_1                          &  !<-- Transformed lon (degrees) of domain #1's west bndry
                                  ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
            CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
            MESSAGE_CHECK="Solver Init: Extract Central Lat/Lon of Domain #1"
!           CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
            CALL ESMF_AttributeGet(state=IMP_STATE                      &  !<-- The Solver import state
                                  ,name ='TPH0D_1'                      &  !<-- Attribute's name
                                  ,value=TPH0D_1                        &  !<-- Geographic lat (degrees) of domain #1's center
                                  ,rc   =RC)
!
            CALL ESMF_AttributeGet(state=IMP_STATE                      &  !<-- The Solver import state
                                  ,name ='TLM0D_1'                      &  !<-- Attribute's name
                                  ,value=TLM0D_1                        &  !<-- Geographic lon (degrees) of domain #1's center
                                  ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
            CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  The SW corner of the moving nest domain lies within local task 0
!***  therefore only that task can perform the computation.
!-----------------------------------------------------------------------
!
            IF(MYPE==0)THEN
!
              D_ONE=1.
              D_180=180.
              PI=DACOS(-D_ONE)
              D2R=PI/D_180
!
              TPH0_1=TPH0D_1*D2R                                           !<-- The central lat/lon of domain #1 is the center
              TLM0_1=TLM0D_1*D2R                                           !    for all grid-associated nests
!
              GLATX=int_state%GLAT(ITS,JTS)                                !<-- Geographic lat (radians) of nest's SW corner
              GLONX=int_state%GLON(ITS,JTS)                                !<-- Geographic lon (radians) of nest's SW corner
!
              X=COS(TPH0_1)*COS(GLATX)*COS(GLONX-TLM0_1)+SIN(TPH0_1)*SIN(GLATX)
              Y=COS(GLATX)*SIN(GLONX-TLM0_1)
              Z=-SIN(TPH0_1)*COS(GLATX)*COS(GLONX-TLM0_1)+COS(TPH0_1)*SIN(GLATX)
!
              TLATX=ATAN(Z/SQRT(X*X+Y*Y))                                  !<-- Transformed lat (radians) of nest domain's SW corner
              TLONX=ATAN(Y/X)                                              !<-- Transformed lon (radians) of nest domain's SW corner
              IF(X<0)TLONX=TLONX+PI
!
              SB_1=SBD_1*D2R                                               !<-- Transformed lat (radians) of domain #1's S bndry
              WB_1=WBD_1*D2R                                               !<-- Transformed lon (radians) of domain #1's W bndry
!
              DPH=int_state%DPHD*D2R                                       !<-- Nest's angular grid increment in J (radians)
              DLM=int_state%DLMD*D2R                                       !<-- Nest's angular grid increment in I (radians)
!
              I_INC=NINT((TLONX-WB_1)/DLM)                                 !<-- Nest grid increments (integer) between west/south
              J_INC=NINT((TLATX-SB_1)/DPH)                                 !    boundaries of the nest and domain #1.
!
              SW_X(1)=(SB_1+J_INC*DPH)/D2R                                 !<-- Transformed lat (degrees) of nest domain's S bndry
              SW_X(2)=(WB_1+I_INC*DLM)/D2R                                 !<-- Transformed lon (degrees) of nest domain's S bndry
!
            ENDIF
!
!-----------------------------------------------------------------------
!***  Local task 0 shares the transformed lat/lon of the nest domain's
!***  south and west boundaries with all other fcst tasks.
!-----------------------------------------------------------------------
!
            CALL MPI_BCAST(SW_X                                         &
                          ,2                                            &
                          ,MPI_REAL                                     &
                          ,0                                            &
                          ,MPI_COMM_COMP                                &
                          ,IERR )
!
            int_state%SBD=SW_X(1)
            int_state%WBD=SW_X(2)
!
          ENDIF
!
!-----------------------------------------------------------------------
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  Assign grid-related constants after dereferencing needed variables.
!-----------------------------------------------------------------------
!
        btim=timef()
!
        CALL CONSTS(int_state%GLOBAL                                    &
                   ,int_state%DT                                        &
                   ,int_state%SMAG2                                     &
                   ,int_state%CODAMP,int_state%WCOR                     &
                   ,int_state%PT                                        &
                   ,int_state%TPH0D,int_state%TLM0D                     &
                   ,int_state%SBD,int_state%WBD                         &
                   ,int_state%DPHD,int_state%DLMD                       &
                   ,int_state%DXH,int_state%RDXH                        &
                   ,int_state%DXV,int_state%RDXV                        &
                   ,int_state%DYH,int_state%RDYH                        &
                   ,int_state%DYV,int_state%RDYV                        &
                   ,int_state%DDV,int_state%RDDV                        &
                   ,int_state%DDMPU,int_state%DDMPV                     &
                   ,int_state%EF4T,int_state%WPDAR                      &
                   ,int_state%FCP,int_state%FDIV                        &
                   ,int_state%CURV,int_state%F                          &
                   ,int_state%FAD,int_state%FAH                         &
                   ,int_state%DARE,int_state%RARE                       &
                   ,int_state%GLAT,int_state%GLON                       &
                   ,int_state%GLAT_SW,int_state%GLON_SW                 &
                   ,int_state%VLAT,int_state%VLON                       &
                   ,int_state%HDACX,int_state%HDACY                     &
                   ,int_state%HDACVX,int_state%HDACVY                   &
                   ,int_state%LNSH,int_state%LNSAD                      &
                   ,int_state%ADV_STANDARD,int_state%ADV_UPSTREAM       &
                   ,int_state%E_BDY,int_state%N_BDY                     &
                   ,int_state%S_BDY,int_state%W_BDY                     &
                   ,int_state%NBOCO,int_state%TBOCO                     &
                   ,MY_DOMAIN_ID,MYPE                                   &
                   ,ITS,ITE,JTS,JTE                                     &
                   ,IMS,IME,JMS,JME                                     &
                   ,IDS,IDE,JDS,JDE )
!
        td%consts_tim=td%consts_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Exchange haloes for some grid-related arrays in case there are
!***  moving nests.
!-----------------------------------------------------------------------
!
        CALL HALO_EXCH                                                  &
             (int_state%GLAT,1                                          &
             ,int_state%GLON,1                                          &
             ,int_state%VLAT,1                                          &
             ,int_state%VLON,1                                          &
             ,3,3)
!
        CALL HALO_EXCH                                                  &
             (int_state%HDACX,1                                         &
             ,int_state%HDACY,1                                         &
             ,int_state%HDACVX,1                                        &
             ,int_state%HDACVY,1                                        &
             ,3,3)
!
        CALL HALO_EXCH                                                  &
             (int_state%F,1                                             &
             ,3,3)
!
!-----------------------------------------------------------------------
!*** Search for lat/lon min/max values and store it in file for
!*** later use in creating GrADS ctl file
!-----------------------------------------------------------------------
!
       CALL LAT_LON_BNDS(int_state%GLAT,int_state%GLON                  &
                       ,mype,num_pes,mpi_comm_comp                      &
                       ,ids,ide,jds,jde                                 &
                       ,ims,ime,jms,jme                                 &
                       ,its,ite,jts,jte                                 &
                       ,my_domain_id )
!
!
!-----------------------------------------------------------------------
!***  Initialize the FFT filters.
!-----------------------------------------------------------------------
!
        IF(int_state%GLOBAL)THEN
          btim=timef()
!
          CALL PREFFT(int_state%DLMD,int_state%DPHD,int_state%SBD,LM      &
                     ,int_state%KHFILT,int_state%KVFILT                   &
                     ,int_state%HFILT,int_state%VFILT                     &




                     ,int_state%WFFTRH,int_state%NFFTRH                   &
                     ,int_state%WFFTRW,int_state%NFFTRW                   &

                     ,int_state%INPES,int_state%JNPES,int_state%MYPE)
!
          td%prefft_tim=td%prefft_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!

          btim=timef()
!
!-----------------------------------------------------------------------
!***  Initialize the polar filter for unfiltered variables.
!-----------------------------------------------------------------------
!
          CALL PRESMUD(int_state%DLMD,int_state%DPHD,int_state%SBD      &
                      ,int_state%NHSMUD)
!
          td%presmud_tim=td%presmud_tim+(timef()-btim)

!
        ENDIF
!
!-----------------------------------------------------------------------
!***  Initialize the physics schemes.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Initialize the Physics Schemes"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL PHYSICS_INITIALIZE(int_state%GFS                           &
                               ,int_state%SHORTWAVE                     &
                               ,int_state%LONGWAVE                      &
                               ,int_state%CONVECTION                    &
                               ,int_state%MICROPHYSICS                  &
                               ,int_state%SFC_LAYER                     &
                               ,int_state%TURBULENCE                    &
                               ,int_state%LAND_SURFACE                  &
                               ,int_state%CO2TF                         &
                               ,int_state%SBD                           &
                               ,int_state%WBD                           &
                               ,int_state%DPHD                          &
                               ,int_state%DLMD                          &
                               ,int_state%TPH0D                         &
                               ,int_state%TLM0D                         &
                               ,MY_DOMAIN_ID                            &
                               ,MYPE                                    &
                               ,MPI_COMM_COMP                           &
                               ,IDS,IDE,JDS,JDE,LM                      &
                               ,IMS,IME,JMS,JME                         &
                               ,ITS,ITE,JTS,JTE                         &
                               ,RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!-----------------------------------------------------------------------
!***  Retrieve the ESMF Grid then create the ESMF Fields on that Grid
!***  for the Solver import/export states.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Retrieve ESMF Grid in Solver Initialize"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_GridCompGet(gridcomp=GRID_COMP                        &  !<-- The Solver gridded component
                             ,grid    =GRID                             &  !<-- The ESMF Grid
                             ,rc      =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Insert the value of NUM_TRACERS_TOTAL into the export state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert NUM_TRACERS into Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='NUM_TRACERS_TOTAL'                &  !<-- The inserted quantity will have this name
                              ,value=int_state%NUM_TRACERS_TOTAL        &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Also insert the index values of the 4-D Tracers array where
!***  Q and CW reside.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert INDX_Q into Physics Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Physics export state
                              ,name ='INDX_Q'                           &  !<-- The inserted quantity will have this name
                              ,value=int_state%INDX_Q                   &  !<-- The location of Q in TRACERS
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert INDX_CW into Physics Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Physics export state
                              ,name ='INDX_CW'                          &  !<-- The inserted quantity will have this name
                              ,value=int_state%INDX_CW                  &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Insert this task's integration index limits into the
!***  export state along with the full domain limits.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Add Task Integration Limits to Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='ITS'                              &  !<-- The inserted quantity will have this name
                              ,value=int_state%ITS                      &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='ITE'                              &  !<-- The inserted quantity will have this name
                              ,value=int_state%ITE                      &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='JTS'                              &  !<-- The inserted quantity will have this name
                              ,value=int_state%JTS                      &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='JTE'                              &  !<-- The inserted quantity will have this name
                              ,value=int_state%JTE                      &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='LM'                               &  !<-- The inserted quantity will have this name
                              ,value=int_state%LM                       &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='NHALO'                            &  !<-- The inserted quantity will have this name
                              ,value=int_state%NHALO                    &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='IDS'                              &  !<-- The inserted quantity will have this name
                              ,value=int_state%IDS                      &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='IDE'                              &  !<-- The inserted quantity will have this name
                              ,value=int_state%IDE                      &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='JDS'                              &  !<-- The inserted quantity will have this name
                              ,value=int_state%JDS                      &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='JDE'                              &  !<-- The inserted quantity will have this name
                              ,value=int_state%JDE                      &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Insert the domain's top pressure, the pressure thickness of the
!***  pressure domain, the mid-layer pressures in the pressure domain
!***  and the mid-layer sigmas in the sigma domain.
!-----------------------------------------------------------------------
!
        LMP1=LM+1
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert PT into Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='PT'                               &  !<-- The inserted quantity will have this name
                              ,value=int_state%PT                       &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='PDTOP'                            &  !<-- The inserted quantity will have this name
                              ,value=int_state%PDTOP                    &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)

        CALL ESMF_AttributeSet(state    =EXP_STATE                      &  !<-- The Solver export state
                              ,name     ='PSGML1'                       &  !<-- The inserted quantity will have this name
                              ,count=LM                             &  !<-- The data has this many items
                              ,valueList=int_state%PSGML1               &  !<-- The value of this is associated with the preceding name
                              ,rc       =RC)
!
        CALL ESMF_AttributeSet(state    =EXP_STATE                      &  !<-- The Solver export state
                              ,name     ='SGML2'                        &  !<-- The inserted quantity will have this name
                              ,count=LM                             &  !<-- The data has this many items
                              ,valueList=int_state%SGML2                &  !<-- The value of this is associated with the preceding name
                              ,rc       =RC)
!
        CALL ESMF_AttributeSet(state    =EXP_STATE                      &  !<-- The Solver export state
                              ,name     ='SG1'                          &  !<-- The inserted quantity will have this name
                              ,count=LMP1                           &  !<-- The data has this many items
                              ,valueList=int_state%SG1                  &  !<-- The value of this is associated with the preceding name
                              ,rc       =RC)
!
        CALL ESMF_AttributeSet(state    =EXP_STATE                      &  !<-- The Solver export state
                              ,name     ='SG2'                          &  !<-- The inserted quantity will have this name
                              ,count=LMP1                           &  !<-- The data has this many items
                              ,valueList=int_state%SG2                  &  !<-- The value of this is associated with the preceding name
                              ,rc       =RC)
!
        CALL ESMF_AttributeSet(state    =EXP_STATE                      &  !<-- The Solver export state
                              ,name     ='DSG2'                         &  !<-- The inserted quantity will have this name
                              ,count=LM                             &  !<-- The data has this many items
                              ,valueList=int_state%DSG2                 &  !<-- The value of this is associated with the preceding name
                              ,rc       =RC)
!
        CALL ESMF_AttributeSet(state    =EXP_STATE                      &  !<-- The Solver export state
                              ,name     ='PDSG1'                        &  !<-- The inserted quantity will have this name
                              ,count=LM                             &  !<-- The data has this many items
                              ,valueList=int_state%PDSG1                &  !<-- The value of this is associated with the preceding name
                              ,rc       =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Insert DXH and DYH into the Solver export state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert DYH into the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='DYH'                              &  !<-- The inserted quantity will have this name
                              ,value=int_state%DYH                      &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        KOUNT=SIZE(int_state%DXH)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert DXH into the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state    =EXP_STATE                      &  !<-- The Solver export state
                              ,name     ='DXH'                          &  !<-- The inserted quantity will have this name
                              ,count=KOUNT                          &  !<-- The data has this many items
                              ,valueList=int_state%DXH                  &  !<-- The value of this is associated with the preceding name
                              ,rc       =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Insert the value of LNSH and LNSV (the width of the
!***  blending region along the boundaries for H and V points).
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert LNSH, LNSV into Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='LNSH'                             &  !<-- The inserted quantity will have this name
                              ,value=int_state%LNSH                     &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='LNSV'                             &  !<-- The inserted quantity will have this name
                              ,value=int_state%LNSV                     &  !<-- The value of this is associated with the preceding name
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Insert the geographic latitude and longitude of the grid points
!***  into the export state.  From there they will be updated in 
!***  DOMAIN_RUN when a moving nest moves.  The central lat/lon
!***  of the nest's rotated system and the angular grid increments
!***  are also needed.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Create Field from H-pt Geographic Latitude"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        FIELD=ESMF_FieldCreate(            GRID                         &  !<-- The ESMF Grid
                              ,            int_state%GLAT               &  !<-- The geographic latitude on H points
                              ,maxHaloUWidth=(/IHALO,JHALO/)              &  !<-- Upper bound of halo region
                              ,maxHaloLWidth=(/IHALO,JHALO/)              &  !<-- Lower bound of halo region
                              ,name       ='GLAT'                       &  !<-- Name of Field
                              ,indexFlag  =ESMF_INDEX_GLOBAL            &
                              ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Add GLAT to the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_StateAdd(EXP_STATE                              &  !<-- The Solver export state
                          ,FIELD                     &  !<-- Field with H-pt geographic lat
                          ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Create Field from H-pt Geographic Longitude"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        FIELD=ESMF_FieldCreate(            GRID                         &  !<-- The ESMF Grid
                              ,            int_state%GLON               &  !<-- The geographic longitude on H points
                              ,maxHaloUWidth=(/IHALO,JHALO/)              &  !<-- Upper bound of halo region
                              ,maxHaloLWidth=(/IHALO,JHALO/)              &  !<-- Lower bound of halo region
                              ,name       ='GLON'                       &  !<-- Name of Field
                              ,indexFlag  =ESMF_INDEX_GLOBAL            &
                              ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Add GLON to the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_StateAdd(EXP_STATE                              &  !<-- The Solver export state
                          ,FIELD                     &  !<-- Field with H-pt geographic lon
                          ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Create Field from V-pt Geographic Latitude"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        FIELD=ESMF_FieldCreate(            GRID                         &  !<-- The ESMF Grid
                              ,            int_state%VLAT               &  !<-- The geographic latitude on V points
                              ,maxHaloUWidth=(/IHALO,JHALO/)              &  !<-- Upper bound of halo region
                              ,maxHaloLWidth=(/IHALO,JHALO/)              &  !<-- Lower bound of halo region
                              ,name       ='VLAT'                       &  !<-- Name of Field
                              ,indexFlag  =ESMF_INDEX_GLOBAL            &
                              ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Add VLAT to the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_StateAdd(EXP_STATE                              &  !<-- The Solver export state
                          ,FIELD                     &  !<-- Field with V-pt geographic lat
                          ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Create Field from V-pt Geographic Longitude"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        FIELD=ESMF_FieldCreate(            GRID                         &  !<-- The ESMF Grid
                              ,            int_state%VLON               &  !<-- The geographic longitude on V points
                              ,maxHaloUWidth=(/IHALO,JHALO/)              &  !<-- Upper bound of halo region
                              ,maxHaloLWidth=(/IHALO,JHALO/)              &  !<-- Lower bound of halo region
                              ,name       ='VLON'                       &  !<-- Name of Field
                              ,indexFlag  =ESMF_INDEX_GLOBAL            &
                              ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Add VLON to the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_StateAdd(EXP_STATE                              &  !<-- The Solver export state
                          ,FIELD                     &  !<-- Field with V-pt geographic lon
                          ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert TPH0D, TLM0D into the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='TPH0D'                            &  !<-- Name of the Attribute
                              ,value=int_state%TPH0D                    &  !<-- The central geo lat of the rotated system
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='TLM0D'                            &  !<-- Name of the Attribute
                              ,value=int_state%TLM0D                    &  !<-- The central geo lat of the rotated system
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert DPHD, DLMD into the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='DPHD'                             &  !<-- Name of the Attribute
                              ,value=int_state%DPHD                     &  !<-- The angular grid increment in X
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='DLMD'                             &  !<-- Name of the Attribute
                              ,value=int_state%DLMD                     &  !<-- The angular grid increment in Y
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert Restart Flag into the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!

        RESTART_ESMF=ESMF_False
        IF(int_state%RESTART)THEN
          RESTART_ESMF=ESMF_True
        ENDIF
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='RESTART'                          &  !<-- Name of the Attribute
                              ,value=RESTART_ESMF                       &  !<-- Is this a restarted run?
                              ,rc   =RC)






!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  If this is a nested domain being restarted then it will have
!***  read in the latest values for its SW corner on its parent grid.
!***  Load those into the export state to transfer to the Parent-
!***  Child coupler.  They are only relevant for nests in restarted
!***  runs.  If this is not a nest the values will be dummies and are
!***  never used.  Likewise a moving nest's next move timestep will
!***  have been read from the restart file for a restarted run.
!***  If this is a parent being restarted then it will have read in
!***  the latest value of the next timestep that its moving children
!***  will move.  Add those to the export state to transfer to the
!***  parent-Child coupler.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert SW Corner of Nest into the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='I_PAR_STA'                        &  !<-- Name of the Attribute
                              ,value=int_state%I_PAR_STA                &  !<-- Parent I of SW corner of this nest
                              ,rc   =RC)
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='J_PAR_STA'                        &  !<-- Name of the Attribute
                              ,value=int_state%J_PAR_STA                &  !<-- Parent J of SW corner of this nest
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Insert Next Move Timestep into the Solver Export State"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeSet(state=EXP_STATE                          &  !<-- The Solver export state
                              ,name ='NEXT_MOVE_TIMESTEP'               &  !<-- Name of the Attribute
                              ,value=int_state%NMTS                     &  !<-- Timestep of the nest's next move
                              ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Let SOLVER_RUN know that the first timestep is special as well
!***  as the first time SOLVER_RUN is executed (which might not be the 
!***  first timestep).
!-----------------------------------------------------------------------
!
        int_state%FIRST_STEP=.TRUE.
        int_state%FIRST_PASS=.TRUE.
!
!-----------------------------------------------------------------------
!***  The restart output file must contain the winds from the
!***  boundary arrays UBS,UBN,.... BUT:
!***   (1) They must be passed from the Solver to the Write
!***       component and since they are not on the ESMF Grid
!***       they must be passed as 1-D Attributes.
!***   (2) We do not want to waste clocktime inserting these
!***       BC winds into the 1-D arrays every timestep when
!***       they are only needed at restart output times
!***       so we must inform the Solver when to fill those
!***       arrays.
!
!***  The 1-D arrays are placed into the Write component's
!***  import state in POINT_OUTPUT.  They are unloaded
!***  in WRT_RUN and sent to the lead forecast task to assemble
!***  into a full-domain 1-D datastring that can be sent to the
!***  lead write task for insertion into the restart file.
!-----------------------------------------------------------------------
!
        int_state%NSTEPS_BC_RESTART=NINT((int_state%MINUTES_RESTART*60)   &  !<-- Timestep frequency for BC data insertion into
                                         /int_state%DT)                      !    1-D local datastrings
!
        IEND=ITE
        JEND=JTE
!
!       IF(JTS==1)THEN                                                       !<-- South boundary tasks
          int_state%NUM_WORDS_BC_SOUTH=(5*LM+1)*2*int_state%LNSV*(IEND-ITS+1)
          ALLOCATE(int_state%RST_BC_DATA_SOUTH(1:int_state%NUM_WORDS_BC_SOUTH))
          DO N=1,int_state%NUM_WORDS_BC_SOUTH
            int_state%RST_BC_DATA_SOUTH(N)=0.
          ENDDO
!       ENDIF
!
!       IF(JTE==JM)THEN                                                      !<-- North boundary tasks
          int_state%NUM_WORDS_BC_NORTH=(5*LM+1)*2*int_state%LNSV*(IEND-ITS+1)
          ALLOCATE(int_state%RST_BC_DATA_NORTH(1:int_state%NUM_WORDS_BC_NORTH))
          DO N=1,int_state%NUM_WORDS_BC_NORTH
            int_state%RST_BC_DATA_NORTH(N)=0.
          ENDDO
!       ENDIF
!
!       IF(ITS==1)THEN                                                       !<-- West boundary tasks
          int_state%NUM_WORDS_BC_WEST=(5*LM+1)*2*int_state%LNSV*(JEND-JTS+1)
          ALLOCATE(int_state%RST_BC_DATA_WEST(1:int_state%NUM_WORDS_BC_WEST))
          DO N=1,int_state%NUM_WORDS_BC_WEST
            int_state%RST_BC_DATA_WEST(N)=0.
          ENDDO
!       ENDIF
!
!       IF(ITE==IM)THEN                                                      !<-- East boundary tasks
          int_state%NUM_WORDS_BC_EAST=(5*LM+1)*2*int_state%LNSV*(JEND-JTS+1)
          ALLOCATE(int_state%RST_BC_DATA_EAST(1:int_state%NUM_WORDS_BC_EAST))
          DO N=1,int_state%NUM_WORDS_BC_EAST
            int_state%RST_BC_DATA_EAST(N)=0.
          ENDDO
!       ENDIF
!
!-----------------------------------------------------------------------
!***  Insert history and restart data pointers (from INT_STATE) into the
!***  Write component's import state (IMP_STATE_WRITE).
!-----------------------------------------------------------------------
!
        CALL POINT_OUTPUT(GRID,INT_STATE,IMP_STATE_WRITE)
!
!-----------------------------------------------------------------------
!***  Set flag for the operational physics suite.
!***  This will be used to save clocktime by skipping
!***  frequent updates of the moist array and instead
!***  update it only when it is needed for physics.
!-----------------------------------------------------------------------
!
        int_state%OPERATIONAL_PHYSICS=.FALSE.
!
        IF((int_state%SHORTWAVE   =='gfdl' .OR.                         &
            int_state%SHORTWAVE   =='rrtm').AND.                        &
           (int_state%LONGWAVE    =='gfdl' .OR.                         &
            int_state%LONGWAVE    =='rrtm').AND.                        &
            int_state%SFC_LAYER   =='myj'  .AND.                        &
            int_state%TURBULENCE  =='myj'  .AND.                        &
           (int_state%CONVECTION  =='bmj'  .OR.                         &
            int_state%CONVECTION  =='none').AND.                        &
           (int_state%MICROPHYSICS=='fer'  .OR.                         &
            int_state%MICROPHYSICS=='fer_hires') ) THEN
!
          int_state%OPERATIONAL_PHYSICS=.TRUE.
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ENDIF fcst_tasks2
!
!-----------------------------------------------------------------------
!
      td%solver_init_tim=td%solver_init_tim+(timef()-btim0)
!
!-----------------------------------------------------------------------
!
      IF(RC_INIT==ESMF_SUCCESS)THEN
!       WRITE(0,*)'SOLVER INITIALIZE STEP SUCCEEDED'
      ELSE
        WRITE(0,*)'SOLVER INITIALIZE STEP FAILED RC_INIT=',RC_INIT
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SOLVER_INITIALIZE
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE SOLVER_RUN (GRID_COMP                                  &
                            ,IMP_STATE                                  &
                            ,EXP_STATE                                  &
                            ,CLOCK_ATM                                  &
                            ,RC_RUN)
!
!-----------------------------------------------------------------------
!***  The integration of each timestep of the model Solver is done
!***  through this routine.
!-----------------------------------------------------------------------
!
      USE MODULE_CONSTANTS,ONLY : CP,G,R,RHOWATER,STBOLT,XLV
!
      USE MODULE_DYNAMICS_ROUTINES,ONLY: ADV1,ADV2,AVEQ2                &
                                        ,CDWDT,CDZDT,DDAMP,DHT          &
                                        ,HADV2,HADV2_SCAL,HDIFF         &
                                        ,IUNIT_ADVEC_SUMS               &
                                        ,MONO,PDTSDT,PGFORCE            &
                                        ,UPDATES,UPDATET,UPDATEUV       &
                                        ,VADV2,VADV2_SCAL,VSOUND,VTOA
!
      USE MODULE_FLTBNDS,ONLY: BOCOH,BOCOV,FFTFHN,FFTFUVN               &
                              ,IUNIT_POLE_SUMS                          &
                              ,POAVHN,READ_BC                           &
                              ,WRITE_BC
!
!-----------------------------------------------------------------------
!***  The following USEs are needed only for GFS physics:
!-----------------------------------------------------------------------
!
      USE N_NAMELIST_PHYSICS_DEF,      ONLY: FHSWR, FDAER               &
                                            ,IAER,IALB,ICO2,IEMS,ICTM   &
                                            ,IOVR_LW,IOVR_SW,ISOL       &
                                            ,LDIAG3D,LSCCA,LGGFS3D      &
                                            ,LSLWR,LSM,LSSAV,LSSWR      &
                                            ,PRE_RAD,RAS,SASHAL         &
                                            ,SHAL_CNV                   &
                                            ,GEN_COORD_HYBRID           &
                                            ,CDMBGWD,DLQF,CTEI_RM       &
                                            ,BKGD_VDIF_M                &
                                            ,BKGD_VDIF_H,BKGD_VDIF_S    &
                                            ,PSAUTCO,PRAUTCO,EVPCO      &
                                            ,CAL_PRE,MOM4ICE,MSTRAT     &
                                            ,TRANS_TRAC,NST_FCST        &
                                            ,MOIST_ADJ,WMINCO

      USE N_LAYOUT1,                  ONLY : IPT_LATS_NODE_R            &
                                            ,LATS_NODE_R
!
      USE DATE_DEF,                   ONLY : FHOUR
      USE MODULE_RADIATION_DRIVER,    ONLY : GRRAD,RADINIT
      USE MODULE_RADIATION_ASTRONOMY, ONLY : ASTRONOMY
      USE MERSENNE_TWISTER
      USE N_RESOL_DEF,                ONLY : LATR,LONR                  &
                                            ,NCLD,NFXR,NMTVR            &
                                            ,NTCW,NTOZ                  &
                                            ,THERMODYN_ID, SFCPRESS_ID  &
                                            ,NUM_P2D,NUM_P3D

      USE OZNE_DEF,                   ONLY : LEVOZP,PL_COEFF,PL_PRES
      USE MODULE_RADSW_PARAMETERS,    ONLY : TOPFSW_TYPE, SFCFSW_TYPE
      USE MODULE_RADLW_PARAMETERS,    ONLY : TOPFLW_TYPE, SFCFLW_TYPE

!-----------------------------------------------------------------------
!
!------------------------
!***  Argument variables
!------------------------
!
      TYPE(ESMF_GridComp) :: GRID_COMP                                     !<-- The Solver gridded component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The Solver import state
                         ,EXP_STATE                                        !<-- The Solver export state
!
      TYPE(ESMF_Clock) :: CLOCK_ATM                                        !<-- The ATM's ESMF Clock
!
      INTEGER,INTENT(OUT) :: RC_RUN
!
!---------------------
!***  Local variables
!---------------------
!
      INTEGER(kind=KINT) :: IDE,IDS,IME,IMS,ITE,ITS                     &
                           ,JDE,JDS,JME,JMS,JTE,JTS
!
      INTEGER(kind=KINT) :: ITE_B1,ITE_B2,ITE_B1_H1,ITE_B1_H2           &
                           ,ITE_H1,ITE_H2                               &
                           ,ITS_B1,ITS_B2,ITS_B1_H1,ITS_B1_H2           &
                           ,ITS_H1,ITS_H2                               &
                           ,JTE_B1,JTE_B2,JTE_B1_H1,JTE_B1_H2           &
                           ,JTE_H1,JTE_H2                               &
                           ,JTS_B1,JTS_B2,JTS_B1_H1,JTS_B1_H2           &
                           ,JTS_H1,JTS_H2
!
      INTEGER(kind=KINT) :: IHALO,JHALO,MPI_COMM_COMP,MY_DOMAIN_ID      &
                           ,MYPE,NUM_PES
!
      INTEGER(kind=KINT) :: DFIHR,I,IER,INPES,IRTN,ISTAT,J,JNPES        &
                           ,K,KFLIP,KS,KSE1,L,N,NSTEPS_HISTORY          &
                           ,NTIMESTEP,NTIMESTEP_BC,NTIMESTEP_RAD        &
                           ,RC,ICLTEND                                  &
                           ,WRITE_BC_FLAG,WRITE_BC_FLAG_NEST
!
      INTEGER(kind=KINT) :: FILTER_METHOD,FILTER_METHOD_LAST            &
                           ,JULDAY,JULYR                                &
                           ,NPRECIP,NSTEPS_PER_CHECK,NSTEPS_PER_HOUR    &
                           ,NSTEPS_PER_RESET
!
      INTEGER(kind=KINT),SAVE :: HDIFF_ON                               &
                                ,P_QV,P_QC,P_QR,P_QI,P_QS,P_QG          &
                                ,PARENT_CHILD_TIME_RATIO
!
      INTEGER(kind=ESMF_KIND_I8) :: NTIMESTEP_ESMF
!
      LOGICAL(kind=KLOG) :: READBC                                      &
                           ,E_BDY,N_BDY,S_BDY,W_BDY                     &
                           ,OLD_PASSIVE                                    !<-- Flag for old passive advection scheme
!
      TYPE(ESMF_TimeInterval) :: DT_ESMF                                   !<-- The ESMF fundamental timestep (s)
!
      TYPE(SOLVER_INTERNAL_STATE),POINTER :: INT_STATE                     !<-- The Solver internal state pointer 
!
      TYPE(WRAP_SOLVER_INT_STATE) :: WRAP                                  !<-- The F90 'wrap' for the Solver internal state
!
!-----------------------------------------------------------------------
!***  SAVEs are for dereferenced constant variables.
!-----------------------------------------------------------------------
!
      INTEGER(kind=KINT),SAVE :: IDTAD,IDTADT,IFACT,IHRSTBC             &
                                ,INTEGER_DT                             &
                                ,KSE,KSS                                &
                                ,LNSAD,LNSH,LNSV,LPT2,NBOCO             &
                                ,N_PRINT_STATS                          &  !<--- Timesteps between statistics prints
                                ,NUMERATOR_DT                           &
                                ,IDENOMINATOR_DT
!
      INTEGER(kind=KINT),DIMENSION(3),SAVE :: IDATBC
!
      INTEGER(kind=KINT),DIMENSION(8)  :: IDAT,JDAT
      INTEGER(kind=KINT),DIMENSION(13) :: DAYS
!
      REAL(kind=KFPT) :: FICE,FRAIN,QI,QR,QW,SECONDS_TOTAL,WC
!
      REAL(kind=KFPT) :: DT,DT_TEST,DT_TEST_RATIO,DTPHY
!
      REAL(kind=KFPT),SAVE :: DDMPV                                     &
                             ,DYH,DYV,EF4T,PDTOP,PT                     &
                             ,RDYH,RDYV,TBOCO
!
      REAL(kind=KFPT),DIMENSION(:),ALLOCATABLE,SAVE :: DSG2             &
                                                      ,PDSG1,PSGML1     &
                                                      ,SGML2
!
      REAL(kind=KFPT),DIMENSION(:),ALLOCATABLE,SAVE :: SG1,SG2
!
      REAL(kind=KFPT),DIMENSION(:),ALLOCATABLE,SAVE :: CURV             &
                                                      ,DARE,DDMPU,DXV   &
                                                      ,FAD,FAH          &
                                                      ,FCP,FDIV         &
                                                      ,RARE,RDXH,RDXV   &
                                                      ,WPDAR
!
      REAL(kind=KFPT),DIMENSION(:,:),ALLOCATABLE,SAVE :: F,FIS          &
                                                        ,HDACX,HDACY    &
                                                        ,HDACVX,HDACVY  &
                                                        ,SICE,SM
!
      LOGICAL(kind=KLOG),SAVE :: GLOBAL,HYDRO,RUNBC,SECADV
!
      LOGICAL(kind=KLOG) :: COMPUTE_BC,FIRST_PASS
!
      REAL(kind=KFPT) :: JULIAN,XTIME, FILT_DT, FUND_DT, DTRATIO
!
      REAL(kind=KFPT),DIMENSION(LM+1) :: PSG1
!
      INTEGER :: KK
!
      LOGICAL(kind=KLOG) :: CALL_LONGWAVE                               &
                           ,CALL_SHORTWAVE                              &
                           ,CALL_TURBULENCE                             &
                           ,CALL_PRECIP                                 &
                           ,CALL_GFS_PHY                                &
                           ,LOC_PCPFLG
!
      TYPE(ESMF_Time) :: STARTTIME,CURRTIME,SIMULATION_START_TIME
!
      TYPE(ESMF_TimeInterval),SAVE:: REST_OFFSET
!
      TYPE(ESMF_Field) :: HOLD_FIELD
!
      DATA DAYS / 31,28,31,30,31,30,31,31,30,31,30,31,30 /
!
!---------------------------------
!***  GFS physics local variables
!---------------------------------
!
      LOGICAL,SAVE                                 :: FIRST=.true.
      LOGICAL                                      :: LPRNT=.false.,NORAD_PRECIP=.false.,CRICK_PROOF=.false., CCNORM=.false.
      LOGICAL                                      :: LSFWD,OPENED,FLIPV,CHANGE,LSSAV_CC,LGOCART=.FALSE.
      INTEGER,PARAMETER                            :: IFLIP=1,NTRAC=3            !!!!!! later ntrac read form namelist
      INTEGER                                      :: ICWP,IMJM, IDATE(4)
      INTEGER                                      :: ISEED,IDE_GR
      INTEGER ,SAVE                                :: ID,IDAY,IMON,MIDMON,MIDM,MIDP,K1OZ,K2OZ,SEED0
      INTEGER ,DIMENSION(1)                        :: ICSDSW,ICSDLW
      INTEGER ,DIMENSION(:),ALLOCATABLE            :: LONSPERLAR, GLOBAL_LATS_R,NLNSP
!
      REAL (kind=KDBL)                             :: T850,FACOZ,DTLW,DTSW,DTLWI,DTSWI,RTvR,CLSTP,DTP,DTF,SOLHR,RADDT
      REAL (kind=KDBL)                             :: XLVRW,XLVRWI,DTPHS,DTPHSI,RoCP,MINDT
      REAL (kind=KDBL) ,DIMENSION(1)               :: RCS2_V,XLAT,FLGMIN_L,CV,CVB,CVT  ! (cv, cvb, cvt not in use when ntcw-1 > 0)
      REAL (kind=KDBL) ,DIMENSION(1)               :: TSEA,TISFC,ZORL,SLMSK,SNWDPH,WEASD,SNCOVR,SNOALB
      REAL (kind=KDBL) ,DIMENSION(1)               :: XSIHFCS,XSICFCS,XSLPFCS,XTG3FCS,XVEGFCS,XVETFCS,XSOTFCS
      REAL (kind=KDBL) ,DIMENSION(1)               :: ALVSF,ALNSF,ALVWF,ALNWF,FACSF,FACWF
      REAL (kind=KDBL) ,DIMENSION(1)               :: WRK, DPSHC, GQ, RANNUM_V
      REAL (kind=KDBL) ,DIMENSION(1)               :: ORO, EVAP, HFLX, CDQ, QSS, FSCAV
      REAL (kind=KDBL) ,DIMENSION(:),ALLOCATABLE   :: CLDCOV_V,PRSL,PRSLK,GU,GV,GT,GR,VVEL,F_ICE,F_RAIN,R_RIME
      REAL (kind=KDBL) ,DIMENSION(:),ALLOCATABLE   :: ADT,ADU,ADV,PHIL
      REAL (kind=KDBL) ,DIMENSION(:,:),ALLOCATABLE :: GR3,ADR
      REAL (kind=KDBL) ,DIMENSION(:),ALLOCATABLE   :: PRSI,PRSIK,RSGM,PHII
      REAL (kind=KDBL) ,DIMENSION(:),ALLOCATABLE   :: SINLAT_R,COSLAT_R
      REAL (kind=KDBL) ,DIMENSION(:,:),ALLOCATABLE :: XLON,COSZEN,COSZDG,RANN,SINLAT_V,COSLAT_V
      REAL (kind=KDBL) ,DIMENSION(:),ALLOCATABLE   :: RANNUM
!
      REAL (kind=KDBL) ,DIMENSION(39)              :: FLUXR_V
      REAL (kind=KDBL) ,DIMENSION(:,:,:),ALLOCATABLE :: GR1
!
      REAL (kind=KDBL) ,DIMENSION(1)               :: SFALB,TSFLW,SEMIS,SFCDLW,SFCDSW,SFCNSW

      type (topfsw_type), dimension(1) :: topfsw
      type (sfcfsw_type), dimension(1) :: sfcfsw
      type (topflw_type), dimension(1) :: topflw
      type (sfcflw_type), dimension(1) :: sfcflw

      REAL (kind=KDBL) ,DIMENSION(:),ALLOCATABLE   :: SWH,HLW,DKH,RNP
!--- gbphys ---
      LOGICAL                                      :: OLD_MONIN, CNVGWD, NEWSAS
      INTEGER ,DIMENSION(2)                        :: NCW
      REAL (kind=KDBL)                             :: CCWF,FAC
      REAL (kind=KDBL) ,DIMENSION(1)               :: CNVPRCP, TOTPRCP, TPRCP, SRFLAG, SHDMIN, SHDMAX, CANOPY
      REAL (kind=KDBL) ,DIMENSION(1)               :: RAIN, RAINC
      REAL (kind=KDBL) ,DIMENSION(1)               :: ACV, ACVB, ACVT
      REAL (kind=KDBL) ,DIMENSION(2)               :: FLGMIN
      REAL (kind=KDBL) ,DIMENSION(3)               :: CRTRH
      REAL (kind=KDBL) ,DIMENSION(NUM_SOIL_LAYERS) :: SMC_V, STC_V, SLC_V
      REAL (kind=KDBL) ,DIMENSION(14)              :: HPRIME
      REAL (kind=KDBL) ,DIMENSION(:),ALLOCATABLE   :: UPD_MF, DWN_MF, DET_MF   !!!!!!!!!!! not in use
      REAL (kind=KDBL) ,DIMENSION(:),ALLOCATABLE   :: DQDT                     !!!!!!!!!!! not in use
      REAL (kind=KDBL) ,DIMENSION(:,:),ALLOCATABLE :: DQ3DT                    !!!!!!!!!!!  (9=5+pl_coeff)
      REAL (kind=KDBL) ,DIMENSION(:,:),ALLOCATABLE :: DT3DT                    !!!!!!!!!!! while
      REAL (kind=KDBL) ,DIMENSION(:,:),ALLOCATABLE :: DU3DT, DV3DT             !!!!!!!!!!! LDIAG3D =.FALSE.

      REAL (kind=KDBL) ,DIMENSION(:,:)  ,ALLOCATABLE :: OZPLOUT_V
      REAL (kind=KDBL) ,DIMENSION(:,:,:),ALLOCATABLE :: OZPLOUT

      REAL (kind=KDBL) ,DIMENSION(3)               :: PHY_F2DV   ! NUM_P2D for Zhao =3, Ferr=1 (fix later)
      REAL (kind=KDBL) ,DIMENSION(:,:),ALLOCATABLE :: PHY_F3DV   ! NUM_P3D for Zhao =4, Ferr=3 (fix later)
!--- gbphys output
      REAL (kind=KDBL) ,DIMENSION(1)               :: EVBSA, EVCWA, TRANSA, SBSNOA, SNOWCA, CLDWRK, PSMEAN
      REAL (kind=KDBL) ,DIMENSION(1)               :: CHH, CMM, EP, EPI, DLWSFCI, ULWSFCI, USWSFCI, DSWSFCI
      REAL (kind=KDBL) ,DIMENSION(1)               :: DLWSFC, ULWSFC, DTSFC, DQSFC, DUSFC, DVSFC, GFLUX
      REAL (kind=KDBL) ,DIMENSION(1)               :: DTSFCI, DQSFCI, GFLUXI, T1, Q1, U1, V1
      REAL (kind=KDBL) ,DIMENSION(1)               :: ZLVL, SOILM, RUNOFF, SRUNOFF, SUNTIM
      REAL (kind=KDBL) ,DIMENSION(1)               :: F10M, UUSTAR, FFMM, FFHH, SPFHMIN, SPFHMAX
      REAL (kind=KDBL) ,DIMENSION(1)               :: PSURF, U10M, V10M, T2M, Q2M, HPBL, PWAT, SNOHFA
      REAL (kind=KDBL) ,DIMENSION(1)               :: DLWSFC_CC, ULWSFC_CC, DTSFC_CC, SWSFC_CC
      REAL (kind=KDBL) ,DIMENSION(1)               :: DUSFC_CC, DVSFC_CC, DQSFC_CC, PRECR_CC
      REAL (kind=KDBL) ,DIMENSION(1)               :: XT, XS, XU, XV, XZ, ZM, XTTS
      REAL (kind=KDBL) ,DIMENSION(1)               :: XZTS, D_CONV, IFD, DT_COOL, QRAIN
      REAL (kind=KDBL) ,DIMENSION(1)               :: SMCWLT2, SMCREF2, GSOIL, GTMP2M, GUSTAR, GPBLH, WET1
      REAL (kind=KDBL) ,DIMENSION(1)               :: GU10M, GV10M, GZORL, GORO
      REAL (kind=KDBL) ,DIMENSION(1)               :: XMU_CC, DLW_CC, DSW_CC, SNW_CC, LPREC_CC, TREF
      REAL (kind=KDBL) ,DIMENSION(1)               :: Z_C, C_0, C_D, W_0, W_D, RQTK
      REAL (kind=KDBL) ,DIMENSION(1)               :: HLWD
      LOGICAL, PARAMETER                           :: LSIDEA  = .FALSE.
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      btim0=timef()
!
!-----------------------------------------------------------------------
!
      RC_RUN=ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Extract the Solver internal state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="SOLVER_RUN: Extract Solver Internal State"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompGetInternalState(GRID_COMP                      &  !<-- The Solver component
                                        ,WRAP                           &
                                        ,RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      INT_STATE=>wrap%INT_STATE
!
!-----------------------------------------------------------------------
!***  The total number of forecast tasks.
!-----------------------------------------------------------------------
!
      INPES=int_state%INPES                                                !<-- I fcst tasks
      JNPES=int_state%JNPES                                                !<-- J fcst tasks
      NUM_PES=INPES*JNPES                                                  !<-- # of fcst tasks
!
!-----------------------------------------------------------------------
!***  Is this task on a domain boundary?
!-----------------------------------------------------------------------
!
      S_BDY=int_state%S_BDY
      N_BDY=int_state%N_BDY
      W_BDY=int_state%W_BDY
      E_BDY=int_state%E_BDY
!
!-----------------------------------------------------------------------
!***  Dereference fundamental variables for the dynamics routines.
!-----------------------------------------------------------------------
!
      ITS=int_state%ITS
      ITE=int_state%ITE
      JTS=int_state%JTS
      JTE=int_state%JTE
      IMS=int_state%IMS
      IME=int_state%IME
      JMS=int_state%JMS
      JME=int_state%JME
      IDS=int_state%IDS
      IDE=int_state%IDE
      JDS=int_state%JDS
      JDE=int_state%JDE
!
      ITS_B1=int_state%ITS_B1
      ITE_B1=int_state%ITE_B1
      ITS_B2=int_state%ITS_B2
      ITE_B2=int_state%ITE_B2
      ITS_B1_H1=int_state%ITS_B1_H1
      ITE_B1_H1=int_state%ITE_B1_H1
      ITE_B1_H2=int_state%ITE_B1_H2
      ITS_H1=int_state%ITS_H1
      ITE_H1=int_state%ITE_H1
      ITS_H2=int_state%ITS_H2
      ITE_H2=int_state%ITE_H2
      JTS_B1=int_state%JTS_B1
      JTE_B1=int_state%JTE_B1
      JTS_B2=int_state%JTS_B2
      JTE_B2=int_state%JTE_B2
      JTS_B1_H1=int_state%JTS_B1_H1
      JTE_B1_H1=int_state%JTE_B1_H1
      JTE_B1_H2=int_state%JTE_B1_H2
      JTS_H1=int_state%JTS_H1
      JTE_H1=int_state%JTE_H1
      JTS_H2=int_state%JTS_H2
      JTE_H2=int_state%JTE_H2
!
      LM=int_state%LM
!
      IHALO=int_state%IHALO    
      JHALO=int_state%JHALO    
!
      MYPE=int_state%MYPE                                                  !<-- The local task rank on this domain
      MY_DOMAIN_ID=int_state%MY_DOMAIN_ID
      MPI_COMM_COMP=int_state%MPI_COMM_COMP
!
!-----------------------------------------------------------------------
!***  Nested domains
!-----------------------------------------------------------------------
!
      I_AM_A_NEST=int_state%I_AM_A_NEST
!
!-----------------------------------------------------------------------
!***  Dereference more variables for shorter names.
!-----------------------------------------------------------------------
!
!     firstpass: IF(FIRST_PASS)THEN
!
      DDMPV=int_state%DDMPV
      DT=int_state%DT
      DYH=int_state%DYH
      DYV=int_state%DYV
      EF4T=int_state%EF4T
      GLOBAL=int_state%GLOBAL
      HYDRO=int_state%HYDRO
      IDTAD=int_state%IDTAD
      IDTADT=int_state%IDTADT
      IHRSTBC=int_state%IHRSTBC
      KSE=int_state%NUM_TRACERS_MET
      KSS=1
      LNSAD=int_state%LNSAD
      LNSH=int_state%LNSH
      LNSV=int_state%LNSV
      LPT2=int_state%LPT2
      NBOCO=int_state%NBOCO
      NSTEPS_PER_CHECK=int_state%NSTEPS_PER_CHECK
      NSTEPS_PER_HOUR=int_state%NSTEPS_PER_HOUR
      NSTEPS_PER_RESET=int_state%NSTEPS_PER_RESET
      PDTOP=int_state%PDTOP
      PT=int_state%PT
      RDYH=int_state%RDYH
      RDYV=int_state%RDYV
      RUNBC=int_state%RUNBC
      SECADV=int_state%SECADV
      TBOCO=int_state%TBOCO
      FILTER_METHOD=int_state%FILTER_METHOD      
      FILTER_METHOD_LAST=int_state%FILTER_METHOD_LAST
!
      P_QV=int_state%P_QV
      P_QC=int_state%P_QC
      P_QR=int_state%P_QR
      P_QI=int_state%P_QI
      P_QS=int_state%P_QS
      P_QG=int_state%P_QG
!
      PARENT_CHILD_TIME_RATIO=int_state%PARENT_CHILD_TIME_RATIO
!
      IF(.NOT.ALLOCATED(DSG2))THEN
        ALLOCATE(DSG2(1:LM),stat=ISTAT)
        ALLOCATE(PDSG1(1:LM),stat=ISTAT)
        ALLOCATE(PSGML1(1:LM),stat=ISTAT)
        ALLOCATE(SGML2(1:LM),stat=ISTAT)
!
        ALLOCATE(SG1(1:LM+1),stat=ISTAT)
        ALLOCATE(SG2(1:LM+1),stat=ISTAT)
!
        ALLOCATE(CURV(JDS:JDE),stat=ISTAT)
        ALLOCATE(DARE(JDS:JDE),stat=ISTAT)
        ALLOCATE(DDMPU(JDS:JDE),stat=ISTAT)
        ALLOCATE(DXV(JDS:JDE),stat=ISTAT)
        ALLOCATE(FAD(JDS:JDE),stat=ISTAT)
        ALLOCATE(FAH(JDS:JDE),stat=ISTAT)
        ALLOCATE(FCP(JDS:JDE),stat=ISTAT)
        ALLOCATE(FDIV(JDS:JDE),stat=ISTAT)
        ALLOCATE(RARE(JDS:JDE),stat=ISTAT)
        ALLOCATE(RDXH(JDS:JDE),stat=ISTAT)
        ALLOCATE(RDXV(JDS:JDE),stat=ISTAT)
        ALLOCATE(WPDAR(JDS:JDE),stat=ISTAT)
!
        ALLOCATE(F(IMS:IME,JMS:JME),stat=ISTAT)
        ALLOCATE(FIS(IMS:IME,JMS:JME),stat=ISTAT)
        ALLOCATE(HDACX(IMS:IME,JMS:JME),stat=ISTAT)
        ALLOCATE(HDACY(IMS:IME,JMS:JME),stat=ISTAT)
        ALLOCATE(HDACVX(IMS:IME,JMS:JME),stat=ISTAT)
        ALLOCATE(HDACVY(IMS:IME,JMS:JME),stat=ISTAT)
        ALLOCATE(SICE(IMS:IME,JMS:JME),stat=ISTAT)
        ALLOCATE(SM(IMS:IME,JMS:JME),stat=ISTAT)
      ENDIF
!
      DO N=1,3
        IDATBC(N)=int_state%IDATBC(N)
      ENDDO
!
      DO L=1,LM
        DSG2(L)=int_state%DSG2(L)
        PDSG1(L)=int_state%PDSG1(L)
        PSGML1(L)=int_state%PSGML1(L)
        SGML2(L)=int_state%SGML2(L)
      ENDDO
!
      DO L=1,LM+1
        SG1(L)=int_state%SG1(L)
        SG2(L)=int_state%SG2(L)
      ENDDO
!
      CALL SET_DOMAIN_SPECS(int_state%ITS,int_state%ITE                 &          
                           ,int_state%JTS,int_state%JTE                 &
                           ,int_state%IMS,int_state%IME                 &
                           ,int_state%JMS,int_state%JME                 &
                           ,int_state%IDS,int_state%IDE                 &
                           ,int_state%JDS,int_state%JDE                 &
                           ,int_state%IHALO,int_state%JHALO             &
                           ,int_state%MY_DOMAIN_ID                      &
                           ,int_state%MYPE                              &
                           ,int_state%MY_NEB                            &
                           ,int_state%MPI_COMM_COMP                     &
                           ,int_state%NUM_PES                           &
!
                           ,LOCAL_ISTART_IN=int_state%LOCAL_ISTART      &
                           ,LOCAL_IEND_IN=int_state%LOCAL_IEND          &
                           ,LOCAL_JSTART_IN=int_state%LOCAL_JSTART      &
                           ,LOCAL_JEND_IN=int_state%LOCAL_JEND          &
                           ,ADV_STANDARD_IN=int_state%ADV_STANDARD      &
                           ,ADV_UPSTREAM_IN=int_state%ADV_UPSTREAM      &
                           ,S_BDY_IN=int_state%S_BDY                    &
                           ,N_BDY_IN=int_state%N_BDY                    &
                           ,W_BDY_IN=int_state%W_BDY                    &
                           ,E_BDY_IN=int_state%E_BDY                    &
                             )
!
!-----------------------------------------------------------------------
!***  Extract the timestep count from the Clock.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Solver Run Gets Timestep from the ATM Clock"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_ClockGet(clock       =CLOCK_ATM                         &  !<-- The ESMF Clock
                        ,timeStep    =DT_ESMF                           &  !<-- Fundamental timestep (s) (ESMF)
                        ,currtime    =CURRTIME                          &  !<-- current time
                        ,advanceCount=NTIMESTEP_ESMF                    &  !<-- The number of times the clock has been advanced
                        ,rc          =RC)

!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_TimeIntervalGet(timeinterval=DT_ESMF                    &  !<-- the ESMF timestep
                               ,s           =INTEGER_DT                 &  !<-- the integer part of the timestep in seconds
                               ,sN          =NUMERATOR_DT               &  !<-- the numerator of the fractional second
                               ,sD          =IDENOMINATOR_DT            &  !<-- the denominator of the fractional second
                               ,rc          =RC)
!
      int_state%DT=REAL(INTEGER_DT)+REAL(NUMERATOR_DT)                  &  !<-- Fundamental timestep (s) (REAL)
                                   /REAL(IDENOMINATOR_DT)
      DT=int_state%DT
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &
                            ,name ='FUND_DT'                            &
                            ,value=FUND_DT                              &
                            ,rc   =RC)
!
      DTRATIO=ABS(DT/FUND_DT)
!
      NTIMESTEP=NTIMESTEP_ESMF
      int_state%NTSD=NTIMESTEP
!     
      FIRST_PASS=int_state%FIRST_PASS
!
      NSTEPS_PER_HOUR=NINT(3600./DT)
!
      N_PRINT_STATS=NINT(3600./DT)                                         !<-- Print layer statistics once per forecast hour
!
!-----------------------------------------------------------------------
!***  Extract the horizontal diffusion flag from the import state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Solver Run Extracts Horizontal Diffusion Flag "
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_AttributeGet(state=IMP_STATE                            &
                            ,name ='HDIFF'                              &
                            ,value=HDIFF_ON                             &
                            ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Extract the digital filter method from the import state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     MESSAGE_CHECK="Solver Run Extracts Horizontal Diffusion Flag "
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!     CALL ESMF_AttributeGet(state=IMP_STATE                            &  !<-- The Solver import state
!                           ,name ='Filter_Method'                      &  !<-- Name of the attribute to extract
!                           ,value=int_state%FILTER_METHOD              &  !<-- The scalar being extracted from the import state
!                           ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!     FILTER_METHOD=int_state%FILTER_METHOD      
!     FILTER_METHOD_LAST=int_state%FILTER_METHOD_LAST
!
!-----------------------------------------------------------------------
!
!     ENDIF firstpass
!
!-----------------------------------------------------------------------
!***  The following set of internal state arrays never changes unless
!***  the domain moves in which case they must be dereferenced again.
!-----------------------------------------------------------------------
!

      MOVE_NOW=ESMF_FALSE
      IF(int_state%MY_DOMAIN_MOVES==ESMF_TRUE)THEN




!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract the MOVE_NOW flag in SOLVER_RUN"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_AttributeGet(state=IMP_STATE                          &  !<-- Solver import state
                              ,name ='MOVE_NOW'                         &  !<-- Name of the flag for current domain motion
                              ,value=MOVE_NOW                           &  !<-- Did the nest move this timestep?
                              ,rc   =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      ENDIF

!
!-----------------------------------------------------------------------
!***  If this is a moving nest and it moved this timestep then we
!***  need to update the haloes of the geographic lat/lon and the
!***  HDAC variables because like all variables they are updated
!***  only in the integration region when a nest shifts.
!-----------------------------------------------------------------------
!

      IF(MOVE_NOW==ESMF_TRUE)THEN



!
        CALL HALO_EXCH                                                  &
           (int_state%GLAT,1                                            &
           ,int_state%GLON,1                                            &
           ,int_state%VLAT,1                                            &
           ,int_state%VLAT,1                                            &
           ,2,2)
!
        CALL HALO_EXCH                                                  &
           (int_state%HDACX,1                                           &
           ,int_state%HDACY,1                                           &
           ,int_state%HDACVX,1                                          &
           ,int_state%HDACVY,1                                          &
           ,2,2)
!
!-----------------------------------------------------------------------
!***  Also the geography information for the gravity wave drag
!***  must be updated to account for the domain's new position.
!
!***  NOTE:  Currently the gravity wave drag is turned off in
!***         moving nests.  A quantity used by the parameterization
!***         is mountains' angle with respect to east.  From the
!***         moving nest's perspective the mountains are moving
!***         and thus those angles would need to be updated.
!***         Such updating is not yet included.
!-----------------------------------------------------------------------
!
        IF(int_state%GWDFLG)THEN
!
          DTPHY=int_state%DT*int_state%NPHS
!
          CALL GWD_init(DTPHY,int_state%RESTART                         &
                       ,int_state%TPH0D,int_state%TLM0D                 &
                       ,int_state%GLAT,int_state%GLON                   &
                       ,int_state%CROT,int_state%SROT,int_state%HANGL   &
                       ,IDS,IDE,JDS,JDE                                 &
                       ,IMS,IME,JMS,JME                                 &
                       ,ITS,ITE,JTS,JTE,LM)
        ENDIF
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF (INTEGER_DT >= 0) IFACT=1
      IF (INTEGER_DT <  0) IFACT=-1
!
      IF(FIRST_PASS)THEN
!
        int_state%DDMPV=IFACT*DTRATIO*int_state%DDMPV
        int_state%EF4T=IFACT*DTRATIO*int_state%EF4T
!
        DO J=JDS,JDE
          int_state%DDMPU(J)=IFACT*int_state%DDMPU(J)
          int_state%FAD(J)=IFACT*DTRATIO*int_state%FAD(J)
          int_state%FAH(J)=IFACT*DTRATIO*int_state%FAH(J)
          int_state%FCP(J)=IFACT*DTRATIO*int_state%FCP(J)
          int_state%WPDAR(J)=IFACT*int_state%WPDAR(J)
        ENDDO
!
        DO J=JTS,JTE
        DO I=ITS,ITE
          int_state%HDACX(I,J)=IFACT*DTRATIO*int_state%HDACX(I,J)
          int_state%HDACY(I,J)=IFACT*DTRATIO*int_state%HDACY(I,J)
          int_state%HDACVX(I,J)=IFACT*DTRATIO*int_state%HDACVX(I,J)
          int_state%HDACVY(I,J)=IFACT*DTRATIO*int_state%HDACVY(I,J)
        ENDDO
        ENDDO
!
      ENDIF
!
      DDMPV=int_state%DDMPV
      EF4T=int_state%EF4T
!
      NBOCO=int(0.5+NBOCO/DTRATIO)
!     IF (MYPE == 0) WRITE(0,*) 'NBOCO reset to : ', NBOCO
!
      DO J=JDS,JDE
        CURV(J)=int_state%CURV(J)
        DARE(J)=int_state%DARE(J)
        DDMPU(J)=int_state%DDMPU(J)
        DXV(J)=int_state%DXV(J)
        FAD(J)=int_state%FAD(J)
        FAH(J)=int_state%FAH(J)
        FCP(J)=int_state%FCP(J)
        FDIV(J)=int_state%FDIV(J)
        RARE(J)=int_state%RARE(J)
        RDXV(J)=int_state%RDXV(J)
        RDXH(J)=int_state%RDXH(J)
        WPDAR(J)=int_state%WPDAR(J)
      ENDDO
!
      DO J=JTS,JTE
      DO I=ITS,ITE
        HDACX(I,J)=int_state%HDACX(I,J)
        HDACY(I,J)=int_state%HDACY(I,J)
        HDACVX(I,J)=int_state%HDACVX(I,J)
        HDACVY(I,J)=int_state%HDACVY(I,J)
      ENDDO
      ENDDO
!
      DO J=JMS,JME
      DO I=IMS,IME
        F(I,J)=int_state%F(I,J)
        FIS(I,J)=int_state%FIS(I,J)
        SICE(I,J)=int_state%SICE(I,J)
        SM(I,J)=int_state%SM(I,J)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  Now we need to do some things related to digital filtering
!***  that are only relevant after the first pass through the
!***  Run step.
!-----------------------------------------------------------------------
!
      DT_TEST=INTEGER_DT
      DT_TEST_RATIO=int_state%DT_TEST_RATIO
!
!-----------------------------------------------------------------------
!
      not_firstpass: IF (.NOT. FIRST_PASS) THEN
!
!-----------------------------------------------------------------------
! 
        changedir: IF (int_state%DT_LAST /= DT_TEST                     &
                                 .AND.                                  &
                       ABS(int_state%DT_LAST) == ABS(DT_TEST) ) THEN
!
!-----------------------------------------------------------------------
!
          IF(MYPE == 0)WRITE(0,*)' Change in integration direction...'  &
                                ,' dt_last=',int_state%dt_last          &
                                ,' dt_test=',dt_test
!
!-----------------------------------------------------------------------
!***  Setting previous time level variables (Adams-Bashforth scheme)
!***  to the current time level.  Seems safer than potentially leaving them
!***  defined as values at a very different point in the time integration.
!-----------------------------------------------------------------------
!
          int_state%FIRST_STEP=.TRUE.
!
          int_state%TP=int_state%T
          int_state%UP=int_state%U
          int_state%VP=int_state%V
!
          IFACT=-1
!
          int_state%DDMPV=IFACT*int_state%DDMPV
          int_state%EF4T=IFACT*int_state%EF4T
          DDMPV=int_state%DDMPV
          EF4T=int_state%EF4T
!
          DO J=JDS,JDE
            int_state%DDMPU(J)=IFACT*int_state%DDMPU(J)
            int_state%FAD(J)=IFACT*int_state%FAD(J)
            int_state%FAH(J)=IFACT*int_state%FAH(J)
            int_state%FCP(J)=IFACT*int_state%FCP(J)
            int_state%WPDAR(J)=IFACT*int_state%WPDAR(J)
!
            DDMPU(J)=int_state%DDMPU(J)
            FAD(J)=int_state%FAD(J)
            FAH(J)=int_state%FAH(J)
            FCP(J)=int_state%FCP(J)
            WPDAR(J)=int_state%WPDAR(J)
          ENDDO
!
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%HDACX(I,J)=IFACT*int_state%HDACX(I,J)
            int_state%HDACY(I,J)=IFACT*int_state%HDACY(I,J)
            int_state%HDACVX(I,J)=IFACT*int_state%HDACVX(I,J)
            int_state%HDACVY(I,J)=IFACT*int_state%HDACVY(I,J)
!
            HDACX(I,J)=int_state%HDACX(I,J)
            HDACY(I,J)=int_state%HDACY(I,J)
            HDACVX(I,J)=int_state%HDACVX(I,J)
            HDACVY(I,J)=int_state%HDACVY(I,J)
          ENDDO
          ENDDO
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Solver Run Gets HDIFF from Import State"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state=IMP_STATE                        &  !<-- The Solver import state
                                ,name ='HDIFF'                          &  !<-- Name of the Attribute to extract
                                ,value=HDIFF_ON                         &  !<-- Put the Attribute here
                                ,rc   =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL HALO_EXCH                                                &
             (int_state%T,LM                                            &
             ,int_state%Q,LM                                            &
             ,int_state%CW,LM                                           &
             ,2,2)
!
          CALL HALO_EXCH                                                &
             (int_state%U,LM                                            &
             ,int_state%V,LM                                            &
             ,2,2)
!
          CALL HALO_EXCH                                                &
             (int_state%PD,1                                            &
             ,2,2)
!
          if(.not.int_state%GLOBAL)                                     &
          CALL WRITE_BC(LM,LNSH,LNSV,NTIMESTEP,DT                       &
                       ,RUNBC                                           &
                       ,TBOCO+int_state%DFIHR_BOCO/2.                   &
                       ,int_state%PDBS,int_state%PDBN                   &
                       ,int_state%PDBW,int_state%PDBE                   &
                       ,int_state%TBS,int_state%TBN                     &
                       ,int_state%TBW,int_state%TBE                     &
                       ,int_state%QBS,int_state%QBN                     &
                       ,int_state%QBW,int_state%QBE                     &
                       ,int_state%WBS,int_state%WBN                     &
                       ,int_state%WBW,int_state%WBE                     &
                       ,int_state%UBS,int_state%UBN                     &
                       ,int_state%UBW,int_state%UBE                     &
                       ,int_state%VBS,int_state%VBN                     &
                       ,int_state%VBW,int_state%VBE                     &
                       ,int_state%PD,int_state%T                        &
                       ,int_state%Q,int_state%CW                        &
                       ,int_state%U,int_state%V                         &
                       ,.TRUE.)                                            !<-- Recompute tendencies at this stage?
!
!-----------------------------------------------------------------------
!
        ENDIF changedir
!
!-----------------------------------------------------------------------
!
        end_filt: IF (FILTER_METHOD /= FILTER_METHOD_LAST) THEN
!
!-----------------------------------------------------------------------
!
          DTRATIO=ABS(FUND_DT/DT_TEST_RATIO)
          IF(MYPE == 0) WRITE(0,*) ' 2nd applying DTRATIO: ', DTRATIO
!
!-----------------------------------------------------------------------
!***  Setting previous time level variables (Adams-Bashforth scheme)
!***  to the current time level.  Seems safer than potentially leaving them
!***  defined as values at a very different point in the time integration.
!-----------------------------------------------------------------------
!
          int_state%TP=int_state%T
          int_state%UP=int_state%U
          int_state%VP=int_state%V
!
          IFACT=1
!
          int_state%DDMPV=IFACT*DTRATIO*int_state%DDMPV
          int_state%EF4T=IFACT*DTRATIO*int_state%EF4T
          DDMPV=int_state%DDMPV
          EF4T=int_state%EF4T
          NBOCO=int(0.5+NBOCO/DTRATIO)
!
!         IF (MYPE == 0) WRITE(0,*) 'NBOCO reset to : ', NBOCO
!
          DO J=JDS,JDE
            int_state%DDMPU(J)=IFACT*int_state%DDMPU(J)
            int_state%FAD(J)=IFACT*DTRATIO*int_state%FAD(J)
            int_state%FAH(J)=IFACT*DTRATIO*int_state%FAH(J)
            int_state%FCP(J)=IFACT*DTRATIO*int_state%FCP(J)
            int_state%WPDAR(J)=IFACT*int_state%WPDAR(J)
!
            DDMPU(J)=int_state%DDMPU(J)
            FAD(J)=int_state%FAD(J)
            FAH(J)=int_state%FAH(J)
            FCP(J)=int_state%FCP(J)
            WPDAR(J)=int_state%WPDAR(J)
          ENDDO
!
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%HDACX(I,J)=IFACT*DTRATIO*int_state%HDACX(I,J)
            int_state%HDACY(I,J)=IFACT*DTRATIO*int_state%HDACY(I,J)
            int_state%HDACVX(I,J)=IFACT*DTRATIO*int_state%HDACVX(I,J)
            int_state%HDACVY(I,J)=IFACT*DTRATIO*int_state%HDACVY(I,J)
!
            HDACX(I,J)=int_state%HDACX(I,J)
            HDACY(I,J)=int_state%HDACY(I,J)
            HDACVX(I,J)=int_state%HDACVX(I,J)
            HDACVY(I,J)=int_state%HDACVY(I,J)
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
!
          int_state%FIRST_STEP=.TRUE.
!
        ENDIF end_filt
!
!-----------------------------------------------------------------------
!
      ENDIF not_firstpass
!
!-----------------------------------------------------------------------
!
      IF(FIRST_PASS)THEN
        int_state%FIRST_PASS=.FALSE.
        FIRST_PASS=int_state%FIRST_PASS
      ENDIF
!
!-----------------------------------------------------------------------
!
      TD=>TIMERS(MY_DOMAIN_ID)                                             !<-- Abbreviate the name of this domain's timers.
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Begin the Solver calling sequence.
!***  Note that the first timestep begins differently
!***  than all subsequent timesteps.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      firststep: IF(int_state%FIRST_STEP.AND.                           &  !<--  The following block is used only for
                    .NOT.int_state%RESTART)THEN                            !     the first timestep and cold start
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
          CALL SWAPHN                                                   &
           (int_state%T,IMS,IME,JMS,JME,LM                              &
           ,INPES)
          td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEHN                                                   &
           (int_state%T                                                 &
           ,IMS,IME,JMS,JME,LM                                          &
           ,INPES,JNPES)
          td%polehn_tim=td%polehn_tim+(timef()-btim)
!
        ENDIF
!
        btim=timef()
        CALL HALO_EXCH(int_state%T,LM                                   &
                      ,2,2)
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  The pressure gradient routine.
!-----------------------------------------------------------------------
!
        btim=timef()
!
        CALL PGFORCE                                                    &
          (int_state%FIRST_STEP,int_state%GLOBAL,int_state%RESTART      &
          ,LM,DT,NTIMESTEP                                              &
          ,RDYV,DSG2,PDSG1,RDXV,WPDAR,FIS                               &
          ,int_state%PD                                                 &
          ,int_state%T,int_state%Q,int_state%CW                         &
          ,int_state%PINT                                               &
          ,int_state%RTOP                                               &
          ,int_state%DIV                                                &
          ,int_state%PCNE,int_state%PCNW                                &
          ,int_state%PCX,int_state%PCY                                  &
          ,int_state%TCU,int_state%TCV )
!
        td%pgforce_tim=td%pgforce_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL HALO_EXCH(int_state%DIV,LM                                 &
                      ,2,2)
        CALL HALO_EXCH(int_state%U,LM                                   &
                      ,int_state%V,LM                                   &
                      ,2,2)
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Divergence and horizontal pressure advection in thermo eqn
!-----------------------------------------------------------------------
!
        btim=timef()
!
        CALL DHT                                                        &
          (GLOBAL,LM,DYV,DSG2,PDSG1,DXV                                 &
          ,FCP,FDIV                                                     &
          ,int_state%PD,int_state%PDO                                   &
          ,int_state%U,int_state%V                                      &
          ,int_state%OMGALF                                             &
          ,int_state%PCNE,int_state%PCNW,int_state%PCX,int_state%PCY    &
          ,int_state%PFNE,int_state%PFNW,int_state%PFX,int_state%PFY    &
          ,int_state%DIV,int_state%TDIV)
!
        td%dht_tim=td%dht_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for the global forecast.
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
          CALL FFTFHN                                                   &
           (LM                                                          &
           ,int_state%KHFILT                                            &
           ,int_state%HFILT                                             &
           ,int_state%DIV                                               &




           ,int_state%WFFTRH,int_state%NFFTRH                           &

           ,NUM_PES,MYPE,MPI_COMM_COMP)
          td%fftfhn_tim=td%fftfhn_tim+(timef()-btim)
!
          btim=timef()
          CALL SWAPHN                                                   &
           (int_state%DIV                                               &
           ,IMS,IME,JMS,JME,LM                                          &
           ,INPES)
!
          CALL SWAPHN                                                   &
           (int_state%OMGALF                                            &
           ,IMS,IME,JMS,JME,LM                                          &
           ,INPES)
          td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEHN                                                   &
           (int_state%DIV                                               &
           ,IMS,IME,JMS,JME,LM                                          &
           ,INPES,JNPES)
!
          CALL POLEHN                                                   &
           (int_state%OMGALF                                            &
           ,IMS,IME,JMS,JME,LM                                          &
           ,INPES,JNPES)
          td%polehn_tim=td%polehn_tim+(timef()-btim)
!
          btim=timef()
          CALL SWAPWN                                                   &
            (int_state%U                                                &
            ,IMS,IME,JMS,JME,LM                                         &
            ,INPES)
!
          CALL SWAPWN                                                   &
            (int_state%V                                                &
            ,IMS,IME,JMS,JME,LM                                         &
            ,INPES)
          td%swapwn_tim=td%swapwn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEWN                                                   &
            (int_state%U,int_state%V                                    &
            ,IMS,IME,JMS,JME,LM                                         &
            ,INPES,JNPES)
          td%polewn_tim=td%polewn_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL HALO_EXCH                                                  &
         (int_state%T,LM                                                &
         ,int_state%U,LM                                                &
         ,int_state%V,LM                                                &
         ,2,2)
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!
      ENDIF firststep
!
!-----------------------------------------------------------------------
!
      not_firststep: IF(.NOT.int_state%FIRST_STEP                       &  !<-- The following block is for all timesteps after
                        .OR.int_state%RESTART)THEN                         !    the first or all steps in restart case
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Horizontal diffusion (internal halo exchange for 4th order)
!-----------------------------------------------------------------------
!
        btim=timef()
!
        IF(HDIFF_ON>0)THEN
          CALL HDIFF                                                    &
            (GLOBAL,HYDRO                                               &
            ,INPES,JNPES,LM,LPT2                                        &
            ,DYH,RDYH                                                   &
            ,DXV,RARE,RDXH                                              &
            ,SICE,SM                                                    &
            ,HDACX,HDACY,HDACVX,HDACVY                                  &
            ,int_state%W,int_state%Z                                    &
            ,int_state%CW,int_state%Q,int_state%Q2                      &
            ,int_state%T,int_state%U,int_state%V,int_state%DEF)            
        ENDIF
!
        td%hdiff_tim=td%hdiff_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for the global forecast.
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
!
          CALL POAVHN                                                   &
            (IMS,IME,JMS,JME,LM                                         &
            ,int_state%T                                                &
            ,INPES,JNPES                                                &
            ,int_state%USE_ALLREDUCE                                    &
            ,int_state%READ_GLOBAL_SUMS                                 &
            ,int_state%WRITE_GLOBAL_SUMS)
!
          CALL POAVHN                                                   &
            (IMS,IME,JMS,JME,LM                                         &
            ,int_state%Q                                                &
            ,INPES,JNPES                                                &
            ,int_state%USE_ALLREDUCE                                    &
            ,int_state%READ_GLOBAL_SUMS                                 &
            ,int_state%WRITE_GLOBAL_SUMS)
!
          CALL POAVHN                                                   &
            (IMS,IME,JMS,JME,LM                                         &
            ,int_state%CW                                               &
            ,INPES,JNPES                                                &
            ,int_state%USE_ALLREDUCE                                    &
            ,int_state%READ_GLOBAL_SUMS                                 &
            ,int_state%WRITE_GLOBAL_SUMS)
!
          CALL POAVHN                                                   &
            (IMS,IME,JMS,JME,LM                                         &
            ,int_state%Q2                                               &
            ,INPES,JNPES                                                &
            ,int_state%USE_ALLREDUCE                                    &
            ,int_state%READ_GLOBAL_SUMS                                 &
            ,int_state%WRITE_GLOBAL_SUMS)
!
          td%poavhn_tim=td%poavhn_tim+(timef()-btim)
!
          btim=timef()
          CALL SWAPHN(int_state%T,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%Q,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%CW,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%Q2,IMS,IME,JMS,JME,LM,INPES)
          td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEHN(int_state%T,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%Q,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%CW,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%Q2,IMS,IME,JMS,JME,LM,INPES,JNPES)
          td%polehn_tim=td%polehn_tim+(timef()-btim)
!
          btim=timef()
          CALL SWAPWN(int_state%U,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPWN(int_state%V,IMS,IME,JMS,JME,LM,INPES)
          td%swapwn_tim=td%swapwn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEWN(int_state%U,int_state%V                           &
                     ,IMS,IME,JMS,JME,LM,INPES,JNPES)
          td%polewn_tim=td%polewn_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL HALO_EXCH(int_state%T,LM                                   &
                      ,int_state%Q,LM                                   &
                      ,int_state%CW,LM                                  &
                      ,int_state%Q2,LM                                  &
                      ,2,2)
        CALL HALO_EXCH(int_state%U,LM                                   &
                      ,int_state%V,LM                                   &
                      ,1,1)
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Regional domains that have no children or are uppermost parents
!***  need to set a digital filter flag and exchange haloes.
!-----------------------------------------------------------------------
!
        IF(.NOT.I_AM_A_NEST.AND..NOT.GLOBAL)THEN                           !<-- For single domains or uppermost parents
!
          READBC=(NTIMESTEP==1.OR.MOD(NTIMESTEP,NBOCO)==0)
!
          bc_check: IF(READBC)THEN                                         !<-- Is it time to read BCs?
!
            IF(MYPE==0)THEN
              WRITE_BC_FLAG=0
!
              IF(NTIMESTEP<=1                                           &
                     .AND.                                              &
                 int_state%PDBS(1,1,1)/=0                               &
                     .AND.                                              &
                 int_state%PDBS(1,1,2)/=0) THEN
!
                WRITE_BC_FLAG=1
              ELSE
!
                WRITE_BC_FLAG=0
              ENDIF
            ENDIF
!
            CALL MPI_BCAST(WRITE_BC_FLAG,1,MPI_INTEGER,0                &
                          ,MPI_COMM_COMP,IRTN)
!
            IF(WRITE_BC_FLAG==1)THEN
              CALL HALO_EXCH                                            &
               (int_state%T,LM                                          &
               ,int_state%Q,LM                                          &
               ,int_state%CW,LM                                         &
               ,2,2)
!
              CALL HALO_EXCH                                            &
               (int_state%U,LM                                          &
               ,int_state%V,LM                                          &
               ,2,2)
!
             CALL HALO_EXCH                                             &
              (int_state%PD,1                                           &
              ,2,2)
!
            ENDIF
!
          ENDIF  bc_check
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  Update the boundary mass points.
!
!***  For non-nested regional domains, read new boundary tendencies
!***  at the appropriate times.
!
!***  If this is a nested domain then unload the new boundary data
!***  from the Solver import state and compute the time tendencies.
!-----------------------------------------------------------------------
!
        bc_update: IF(.NOT.GLOBAL)THEN
!
!-----------------------------------------------------------------------
!***  The following block is for digital filtering.
!-----------------------------------------------------------------------
!
          IF(I_AM_A_NEST)THEN
!
            IF(MYPE==0)THEN
              WRITE_BC_FLAG_NEST=0
!
              IF (S_BDY.AND.W_BDY                                       &
                       .AND.                                            &
                  NTIMESTEP <= 1                                        &
                       .AND.                                            &
                  int_state%PDBS(1,1,1)/=0                              &
                       .AND.                                            &
                  int_state%PDBS(1,1,2)/=0) THEN
!
                WRITE_BC_FLAG_NEST=1
              ENDIF
!
            ENDIF
!
            CALL MPI_BCAST(WRITE_BC_FLAG_NEST,1,MPI_INTEGER             &
                          ,0,MPI_COMM_COMP,IRTN)
!
            IF (WRITE_BC_FLAG_NEST == 1) THEN
              CALL HALO_EXCH                                            &
               (int_state%T,LM                                          &
               ,int_state%Q,LM                                          &
               ,int_state%CW,LM                                         &
               ,2,2)
!
              CALL HALO_EXCH                                            &
               (int_state%U,LM                                          &
               ,int_state%V,LM                                          &
               ,2,2)
!
              CALL HALO_EXCH                                            &
               (int_state%PD,1                                          &
               ,2,2)
            ENDIF
          ENDIF
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Set SIMULATION_START_TIME for Filter in Solver Run"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_TimeSet(time=SIMULATION_START_TIME                  &
                           ,yy  =START_YEAR                             &
                           ,mm  =START_MONTH                            &
                           ,dd  =START_DAY                              &
                           ,h   =START_HOUR                             &
                           ,rc  =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          IF (FILTER_METHOD == 1 .and. NTIMESTEP == 0) THEN
!
            REST_OFFSET=CURRTIME-SIMULATION_START_TIME
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
            MESSAGE_CHECK="Get Time Offset for Filter in Solver Run"
!           CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
            CALL ESMF_TimeIntervalGet(timeinterval=REST_OFFSET          &
                                     ,s           =JDAT(7)              &
                                     ,rc          =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
            CALL ERR_MSG(RC,MESSAGE_CHECK,RC_RUN)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
            RESTVAL=JDAT(7)
            IF (MYPE == 0) WRITE(0,*) 'set RESTVAL to: ', RESTVAL
!
          ENDIF
!
!-----------------------------------------------------------------------
!
          boundary_tendencies: IF(S_BDY.OR.N_BDY.OR.W_BDY.OR.E_BDY)THEN
!
!-----------------------------------------------------------------------
!***  Nests update boundary tendencies based on data from parent.
!-----------------------------------------------------------------------
!
            nest_or_parent: IF(I_AM_A_NEST)THEN
!
!-----------------------------------------------------------------------
!***  The following block is for digital filtering.
!-----------------------------------------------------------------------
!
              IF(NTIMESTEP<=1.AND.WRITE_BC_FLAG_NEST==1)THEN
!
                TBOCO=PARENT_CHILD_TIME_RATIO*DT
                CALL WRITE_BC(LM,LNSH,LNSV,NTIMESTEP,DT                 &
                             ,RUNBC,TBOCO                               &
                             ,int_state%PDBS,int_state%PDBN             &
                             ,int_state%PDBW,int_state%PDBE             &
                             ,int_state%TBS,int_state%TBN               &
                             ,int_state%TBW,int_state%TBE               &
                             ,int_state%QBS,int_state%QBN               &
                             ,int_state%QBW,int_state%QBE               &
                             ,int_state%WBS,int_state%WBN               &
                             ,int_state%WBW,int_state%WBE               &
                             ,int_state%UBS,int_state%UBN               &
                             ,int_state%UBW,int_state%UBE               &
                             ,int_state%VBS,int_state%VBN               &
                             ,int_state%VBW,int_state%VBE               &
                             ,int_state%PD,int_state%T                  &
                             ,int_state%Q,int_state%CW                  &
                             ,int_state%U,int_state%V                   &
                             ,.FALSE.)                                     !<-- Are tendencies recomputed?
!
              ENDIF
!
!-----------------------------------------------------------------------
!
              COMPUTE_BC=(NTIMESTEP==1.OR.                              &
                          MOD(NTIMESTEP,PARENT_CHILD_TIME_RATIO)==0)
!
              IF(COMPUTE_BC)THEN
!   
                CALL UPDATE_BC_TENDS(IMP_STATE                          &
                                    ,LM,LNSH,LNSV                       &
                                    ,PARENT_CHILD_TIME_RATIO,DT         &
                                    ,S_BDY,N_BDY,W_BDY,E_BDY            &
                                    ,int_state%PDBS,int_state%PDBN      &
                                    ,int_state%PDBW,int_state%PDBE      &
                                    ,int_state%TBS,int_state%TBN        &
                                    ,int_state%TBW,int_state%TBE        &
                                    ,int_state%QBS,int_state%QBN        &
                                    ,int_state%QBW,int_state%QBE        &
                                    ,int_state%WBS,int_state%WBN        &
                                    ,int_state%WBW,int_state%WBE        &
                                    ,int_state%UBS,int_state%UBN        &
                                    ,int_state%UBW,int_state%UBE        &
                                    ,int_state%VBS,int_state%VBN        &
                                    ,int_state%VBW,int_state%VBE        &
                                    ,int_state%ITS,int_state%ITE        &
                                    ,int_state%JTS,int_state%JTE        &
                                    ,int_state%IMS,int_state%IME        &
                                    ,int_state%JMS,int_state%JME        &
                                    ,int_state%IDS,int_state%IDE        &
                                    ,int_state%JDS,int_state%JDE        &
                                                                 )
!
              ENDIF
!
!-----------------------------------------------------------------------
!***  Single/uppermost domain reads its own boundary input data
!-----------------------------------------------------------------------
!
            ELSE nest_or_parent
!
              CALL ESMF_TimeSet(time=SIMULATION_START_TIME              &
                               ,yy  =START_YEAR                         &
                               ,mm  =START_MONTH                        &
                               ,dd  =START_DAY                          &
                               ,h   =START_HOUR)
!
              IF (FILTER_METHOD > 0 .and. NTIMESTEP == 0) THEN
                REST_OFFSET=CURRTIME-SIMULATION_START_TIME
                CALL ESMF_TimeIntervalGet(timeinterval=REST_OFFSET, s=JDAT(7))
                NTIMESTEP_BC=(NTIMESTEP)+NINT(JDAT(7)/abs(DT))
              ELSE
                NTIMESTEP_BC=NTIMESTEP
              ENDIF
!
!-----------------------------------------------------------------------
!***  Set logical flag to read the BCs
!-----------------------------------------------------------------------
!
              READBC=( (NTIMESTEP==0 .AND. MOD(NTIMESTEP_BC,NBOCO)==0)     &  !<-- Filter related?
!
                                          .OR.                             &
!
                       NTIMESTEP_BC==1                                     &  !<-- First timestep
!
                                          .OR.                             &
!
                     ((MOD(NTIMESTEP_BC,NBOCO)==0) .AND. FILTER_METHOD==0) )  !<-- Non-filter, NBOCO coincident time
!
!-----------------------------------------------------------------------
!
              bc_read: IF(READBC)THEN
!
                bc_flag: IF(WRITE_BC_FLAG==0)THEN
!
                  CALL READ_BC(LM,LNSH,LNSV,NTIMESTEP_BC,DT             &
                              ,RUNBC,IDATBC,IHRSTBC,TBOCO               &
                              ,int_state%PDBS,int_state%PDBN            &
                              ,int_state%PDBW,int_state%PDBE            &
                              ,int_state%TBS,int_state%TBN              &
                              ,int_state%TBW,int_state%TBE              &
                              ,int_state%QBS,int_state%QBN              &
                              ,int_state%QBW,int_state%QBE              &
                              ,int_state%WBS,int_state%WBN              &
                              ,int_state%WBW,int_state%WBE              &
                              ,int_state%UBS,int_state%UBN              &
                              ,int_state%UBW,int_state%UBE              &
                              ,int_state%VBS,int_state%VBN              &
                              ,int_state%VBW,int_state%VBE              &
                                )
!
                ELSE
!
                  IF (NTIMESTEP==0) THEN
                    CALL WRITE_BC(LM,LNSH,LNSV,NTIMESTEP,DT             &
                            ,RUNBC,TBOCO                                &
                            ,int_state%PDBS,int_state%PDBN              &
                            ,int_state%PDBW,int_state%PDBE              &
                            ,int_state%TBS,int_state%TBN                &
                            ,int_state%TBW,int_state%TBE                &
                            ,int_state%QBS,int_state%QBN                &
                            ,int_state%QBW,int_state%QBE                &
                            ,int_state%WBS,int_state%WBN                &
                            ,int_state%WBW,int_state%WBE                &
                            ,int_state%UBS,int_state%UBN                &
                            ,int_state%UBW,int_state%UBE                &
                            ,int_state%VBS,int_state%VBN                &
                            ,int_state%VBW,int_state%VBE                &
                            ,int_state%PD,int_state%T                   &
                            ,int_state%Q,int_state%CW                   &
                            ,int_state%U,int_state%V                    &
                            ,.TRUE.)                                       !<-- Are tendencies recomputed?
                 ENDIF
!
                ENDIF  bc_flag
!
              ENDIF  bc_read
!
            ENDIF  nest_or_parent
!
!-----------------------------------------------------------------------
!
          ENDIF boundary_tendencies
!
!-----------------------------------------------------------------------
!
          btim=timef()
!
          CALL BOCOH                                                    &
            (LM,LNSH,DT,PT,DSG2,PDSG1                                   &
             ,int_state%PD                                              &
             ,int_state%PDBE,int_state%PDBN                             &
             ,int_state%PDBS,int_state%PDBW                             &
             ,int_state%TBE,int_state%TBN                               &
             ,int_state%TBS,int_state%TBW                               &
             ,int_state%QBE,int_state%QBN                               &
             ,int_state%QBS,int_state%QBW                               &
             ,int_state%WBE,int_state%WBN                               &
             ,int_state%WBS,int_state%WBW                               &
             ,int_state%T,int_state%Q,int_state%CW                      &
             ,int_state%PINT)
!
          td%bocoh_tim=td%bocoh_tim+(timef()-btim)
!
        ENDIF bc_update
!
!-----------------------------------------------------------------------
!***  The pressure gradient routine.
!-----------------------------------------------------------------------
!
        btim=timef()
!
        CALL PGFORCE                                                    &
          (int_state%FIRST_STEP,int_state%GLOBAL,int_state%RESTART      &
          ,LM,DT,NTIMESTEP                                              &
          ,RDYV,DSG2,PDSG1,RDXV,WPDAR,FIS                               &
          ,int_state%PD                                                 &
          ,int_state%T,int_state%Q,int_state%CW                         &
          ,int_state%PINT                                               &
          ,int_state%RTOP                                               &
          ,int_state%DIV                                                &
          ,int_state%PCNE,int_state%PCNW                                &
          ,int_state%PCX,int_state%PCY                                  &
          ,int_state%TCU,int_state%TCV )
!
        td%pgforce_tim=td%pgforce_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for the global forecast.
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
          CALL FFTFUVN                                                  &
            (LM                                                         &
            ,int_state%KVFILT,int_state%VFILT                           &
            ,int_state%TCU,int_state%TCV                                &




            ,int_state%WFFTRW,int_state%NFFTRW                          &

            ,NUM_PES,MYPE,MPI_COMM_COMP)
          td%fftfwn_tim=td%fftfwn_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  Update the wind field.
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL UPDATEUV                                                   &
         (LM                                                            &
         ,int_state%U,int_state%V                                       &
         ,int_state%TCU,int_state%TCV )
!
        td%updatet_tim=td%updatet_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for the global forecast.
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
          CALL SWAPWN(int_state%U,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPWN(int_state%V,IMS,IME,JMS,JME,LM,INPES)
          td%swapwn_tim=td%swapwn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEWN(int_state%U,int_state%V                           &
                     ,IMS,IME,JMS,JME,LM,INPES,JNPES)
          td%polewn_tim=td%polewn_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL HALO_EXCH(int_state%DIV,LM                                 &
                      ,2,2)
        CALL HALO_EXCH(int_state%U,LM                                   &
                      ,int_state%V,LM                                   &
                      ,2,2)
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Update the boundary velocity points for the regional forecast.
!-----------------------------------------------------------------------
!
        IF(.NOT.GLOBAL)THEN
!
          btim=timef()
          CALL BOCOV                                                    &
            (LM,LNSV,DT                                                 &
            ,int_state%UBE,int_state%UBN,int_state%UBS,int_state%UBW    &
            ,int_state%VBE,int_state%VBN,int_state%VBS,int_state%VBW    &
            ,int_state%U,int_state%V)
          td%bocov_tim=td%bocov_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  The boundary winds have just been updated.  In order to replicate
!***  the integration of a restarted run compared to its free-forecast
!***  counterpart then we must save the wind data in the boundary
!***  arrays for the restart files at this place in the runstream.
!-----------------------------------------------------------------------
!
        IF(MOD(NTIMESTEP+1,int_state%NSTEPS_BC_RESTART)==0)THEN            !<-- Look ahead to the end of this timestep
          CALL SAVE_BC_DATA                                             &
            (LM,LNSV                                                    &
            ,int_state%PDBS,int_state%PDBN,int_state%PDBW,int_state%PDBE&
            ,int_state%TBS,int_state%TBN,int_state%TBW,int_state%TBE    &
            ,int_state%QBS,int_state%QBN,int_state%QBW,int_state%QBE    &
            ,int_state%WBS,int_state%WBN,int_state%WBW,int_state%WBE    &
            ,int_state%UBS,int_state%UBN,int_state%UBW,int_state%UBE    &
            ,int_state%VBS,int_state%VBN,int_state%VBW,int_state%VBE    &
            ,int_state%NUM_WORDS_BC_SOUTH,int_state%RST_BC_DATA_SOUTH   &
            ,int_state%NUM_WORDS_BC_NORTH,int_state%RST_BC_DATA_NORTH   &
            ,int_state%NUM_WORDS_BC_WEST ,int_state%RST_BC_DATA_WEST    &
            ,int_state%NUM_WORDS_BC_EAST ,int_state%RST_BC_DATA_EAST    &
            ,EXP_STATE                                                  &
            ,int_state%ITS,int_state%ITE,int_state%JTS,int_state%JTE    &
            ,int_state%IMS,int_state%IME,int_state%JMS,int_state%JME    &
            ,int_state%IDS,int_state%IDE,int_state%JDS,int_state%JDE    &
              )
!
        ENDIF
!
!-----------------------------------------------------------------------
!***  Divergence and horizontal pressure advection in thermo eqn
!-----------------------------------------------------------------------
!
        btim=timef()
!
        CALL DHT                                                        &
          (GLOBAL,LM,DYV,DSG2,PDSG1,DXV                                 &
          ,FCP,FDIV                                                     &
          ,int_state%PD,int_state%PDO                                   &
          ,int_state%U,int_state%V                                      &
          ,int_state%OMGALF                                             &
          ,int_state%PCNE,int_state%PCNW,int_state%PCX,int_state%PCY    &
          ,int_state%PFNE,int_state%PFNW,int_state%PFX,int_state%PFY    &
          ,int_state%DIV,int_state%TDIV)
!
        td%dht_tim=td%dht_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for the global forecast.
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
          CALL FFTFHN                                                   &
           (LM                                                          &
           ,int_state%KHFILT                                            &
           ,int_state%HFILT                                             &
           ,int_state%DIV                                               &




           ,int_state%WFFTRH,int_state%NFFTRH                           &

           ,NUM_PES,MYPE,MPI_COMM_COMP)
          td%fftfhn_tim=td%fftfhn_tim+(timef()-btim)
!
          btim=timef()
          CALL SWAPHN                                                   &
           (int_state%DIV                                               &
           ,IMS,IME,JMS,JME,LM                                          &
           ,INPES)
!
          CALL SWAPHN                                                   &
           (int_state%OMGALF                                            &
           ,IMS,IME,JMS,JME,LM                                          &
           ,INPES)
          td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEHN                                                   &
           (int_state%DIV                                               &
           ,IMS,IME,JMS,JME,LM                                          &
           ,INPES,JNPES)
!
          CALL POLEHN                                                   &
           (int_state%OMGALF                                            &
           ,IMS,IME,JMS,JME,LM                                          &
           ,INPES,JNPES)
          td%polehn_tim=td%polehn_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL HALO_EXCH(int_state%DIV,LM                                 &
                      ,int_state%OMGALF,LM                              &
                      ,2,2)
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Divergence damping
!-----------------------------------------------------------------------
!
        btim=timef()
!
        IF(HDIFF_ON>0)THEN
          CALL DDAMP                                                    &
            (LM                                                         &
            ,DDMPV,PDTOP                                                &
            ,DSG2,PDSG1                                                 &
            ,SG1,SG2                                                    &
            ,DDMPU                                                      &
            ,int_state%FREERUN                                          &
            ,int_state%PD,int_state%PDO                                 &
            ,int_state%U,int_state%V                                    &
            ,int_state%DIV)
        ENDIF
!
        td%ddamp_tim=td%ddamp_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for the global forecast.
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
          CALL SWAPWN                                                   &
            (int_state%U                                                &
            ,IMS,IME,JMS,JME,LM                                         &
            ,INPES)
!
          CALL SWAPWN                                                   &
            (int_state%V                                                &
            ,IMS,IME,JMS,JME,LM                                         &
            ,INPES)
          td%swapwn_tim=td%swapwn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEWN                                                   &
            (int_state%U,int_state%V                                    &
            ,IMS,IME,JMS,JME,LM                                         &
            ,INPES,JNPES)
          td%polewn_tim=td%polewn_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL HALO_EXCH(int_state%U,int_state%LM                         &
                      ,int_state%V,int_state%LM                         &
                      ,2,2)
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!
      ENDIF not_firststep
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  The remainder of the Solver integration call sequence
!***  is the same for all timesteps.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      int_state%FIRST_STEP=.FALSE.
!
!-----------------------------------------------------------------------
!***  Update the surface pressure.
!-----------------------------------------------------------------------
!
      btim=timef()
!
      CALL PDTSDT                                                       &
        (LM,DT,SG2                                                      &
        ,int_state%PD                                                   &
        ,int_state%PDO,int_state%PSDT                                   &
        ,int_state%PSGDT                                                &
!
!***  Temporary argument
!
       ,int_state%DIV,int_state%TDIV)
!
      td%pdtsdt_tim=td%pdtsdt_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions
!-----------------------------------------------------------------------
!
      IF(GLOBAL)THEN
        btim=timef()
        CALL SWAPHN(int_state%PD,IMS,IME,JMS,JME,1,INPES)
        CALL SWAPHN(int_state%PSDT,IMS,IME,JMS,JME,1,INPES)
        td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
        btim=timef()
        CALL POLEHN(int_state%PD,IMS,IME,JMS,JME,1,INPES,JNPES)
        CALL POLEHN(int_state%PSDT,IMS,IME,JMS,JME,1,INPES,JNPES)
        td%polehn_tim=td%polehn_tim+(timef()-btim)
!
        btim=timef()
        CALL SWAPHN(int_state%PSGDT,IMS,IME,JMS,JME,LM-1,INPES)
        td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
        btim=timef()
        CALL POLEHN(int_state%PSGDT,IMS,IME,JMS,JME,LM-1,INPES,JNPES)
        td%polehn_tim=td%polehn_tim+(timef()-btim)
      ENDIF
!
!-----------------------------------------------------------------------
!
      btim=timef()
      CALL HALO_EXCH(int_state%PD,1                                     &
                    ,int_state%PSDT,1                                   &
                    ,int_state%PSGDT,LM-1                               &
                    ,2,2)
      td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Advection of T, U, and V
!-----------------------------------------------------------------------
!
      btim=timef()
!
      CALL ADV1                                                         &
        (GLOBAL,SECADV                                                  &
        ,LM,LNSAD,INPES,JNPES                                           &
        ,DT,DYV,RDYH,RDYV                                               &
        ,DSG2,PDSG1                                                     &
        ,CURV,DXV,FAD,FAH,RDXH,RDXV,F                                   &
        ,int_state%PD,int_state%PDO                                     &
        ,int_state%OMGALF,int_state%PSGDT                               &
        ,int_state%T,int_state%U,int_state%V                            &
        ,int_state%TP,int_state%UP,int_state%VP                         &
!
!***  Temporary arguments
!
        ,int_state%PFNE,int_state%PFNW                                  &
        ,int_state%PFX,int_state%PFY                                    &
        ,int_state%TCT,int_state%TCU,int_state%TCV)
!
      td%adv1_tim=td%adv1_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Advection of tracers
!-----------------------------------------------------------------------
! 
      tracers: IF(int_state%ADVECT_TRACERS                              &
                           .AND.                                        &
                  MOD(ABS(NTIMESTEP),IDTADT)==0)THEN
!
!-----------------------------------------------------------------------
!
        btim=timef()
!
        IF(int_state%SPEC_ADV)THEN
          KSE1=int_state%NUM_TRACERS_TOTAL
        ELSE
          KSE1=KSE
        ENDIF

!        if (MYPE .eq. 0) then
!        write(0,*) 'KSS, KSE1 into ADV2: ', KSS, KSE1
!        endif
!
        CALL ADV2                                                       &
          (GLOBAL                                                       &
          ,IDTADT,KSS,KSE1,LM,LNSAD                                     &
          ,DT,RDYH                                                      &
          ,DSG2,PDSG1                                                   &
          ,FAH,RDXH                                                     &
          ,int_state%PD,int_state%PDO                                   &
          ,int_state%PSGDT                                              &
          ,int_state%UP,int_state%VP                                    &
          ,int_state%Q2,int_state%INDX_Q2                               &
          ,int_state%TRACERS                                            &
          ,int_state%TRACERS_PREV                                       &
!
!***  Temporary arguments
!
          ,int_state%PFNE,int_state%PFNW                                &
          ,int_state%PFX,int_state%PFY                                  &
          ,int_state%TRACERS_SQRT                                       &
          ,int_state%TRACERS_TEND)
!
        td%adv2_tim=td%adv2_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for global forecasts
!-----------------------------------------------------------------------
!
          IF(GLOBAL)THEN
!
            btim=timef()
!
              DO KS=KSS,KSE1
                CALL FFTFHN                                             &
                  (LM                                                   &
                  ,int_state%KHFILT                                     &
                  ,int_state%HFILT                                      &
                  ,int_state%TRACERS_TEND(IMS:IME,JMS:JME,1:LM,KS)      &




                  ,int_state%WFFTRH,int_state%NFFTRH                    &

                  ,NUM_PES,MYPE,MPI_COMM_COMP)
              ENDDO
! 
            td%fftfhn_tim=td%fftfhn_tim+(timef()-btim)
!
          ENDIF
!
!-----------------------------------------------------------------------
!***  Tracer monotonization
!-----------------------------------------------------------------------
!
        btim=timef()
!
        CALL MONO                                                       &
          (IDTADT,KSS,KSE1,LM                                           &
          ,DSG2,PDSG1                                                   &
          ,DARE                                                         &
          ,int_state%PD                                                 &
          ,int_state%INDX_Q2                                            &
          ,int_state%TRACERS                                            &
          ,INPES,JNPES                                                  &
          ,int_state%USE_ALLREDUCE                                      &
          ,int_state%READ_GLOBAL_SUMS                                   &
          ,int_state%WRITE_GLOBAL_SUMS                                  &
!
!***  Temporary arguments
!
          ,int_state%TRACERS_SQRT                                       &
          ,int_state%TRACERS_TEND)
!
        td%mono_tim=td%mono_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Update tracers
!-----------------------------------------------------------------------
!
        btim=timef()
!
!---------
!***  Q
!---------
!
        CALL UPDATES                                                    &
          (LM                                                           &
          ,int_state%Q                                                  &
!
!***  Temporary argument
!
          ,int_state%TRACERS_TEND(IMS:IME,JMS:JME,1:LM,int_state%INDX_Q))
!
!---------
!***  CW
!---------
!
        CALL UPDATES                                                    &
          (LM                                                           &
          ,int_state%CW                                                 &
!
!***  Temporary argument
!
          ,int_state%TRACERS_TEND(IMS:IME,JMS:JME,1:LM,int_state%INDX_CW))
!
!---------
!***  O3
!---------
!
        CALL UPDATES                                                    &
          (LM                                                           &
          ,int_state%O3                                                 &
!
!***  Temporary argument
!
          ,int_state%TRACERS_TEND(IMS:IME,JMS:JME,1:LM,int_state%INDX_O3))
!
!---------
!***  Q2
!---------
!
        CALL UPDATES                                                    &
          (LM                                                           &
          ,int_state%Q2                                                 &
!
!***  Temporary argument
!
          ,int_state%TRACERS_TEND(IMS:IME,JMS:JME,1:LM,int_state%INDX_Q2))
!
        IF(int_state%SPEC_ADV)THEN
          DO KS=KSS,KSE1
!
           IF(KS/=int_state%INDX_Q  .AND.                               &
              KS/=int_state%INDX_CW .AND.                               &
              KS/=int_state%INDX_O3 .AND.                               &
              KS/=int_state%INDX_Q2) THEN
!
             CALL UPDATES                                               &
              (LM                                                       &
              ,int_state%TRACERS(IMS:IME,JMS:JME,1:LM,KS)               &
              ,int_state%TRACERS_TEND(IMS:IME,JMS:JME,1:LM,KS))
!
            ENDIF
!
          ENDDO
        ENDIF
!
        td%updatet_tim=td%updatet_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
          CALL SWAPHN(int_state%Q,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%CW,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%O3,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%Q2,IMS,IME,JMS,JME,LM,INPES)
!
          td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEHN(int_state%Q,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%CW,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%O3,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%Q2,IMS,IME,JMS,JME,LM,INPES,JNPES)
!
          td%polehn_tim=td%polehn_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL HALO_EXCH(int_state%Q,LM                                   &
                      ,int_state%CW,LM                                  &
                      ,int_state%O3,LM                                  &
                      ,int_state%Q2,LM                                  &
                      ,2,2)
!
        IF(int_state%SPEC_ADV)THEN
          DO KS=KSS,KSE1
!
            IF(KS /= int_state%INDX_Q .AND.                             &
               KS /= int_state%INDX_CW .AND.                            &
               KS /= int_state%INDX_O3 .AND.                            &
               KS /= int_state%INDX_Q2 ) THEN
!
              CALL HALO_EXCH(                                           &
                int_state%TRACERS(IMS:IME,JMS:JME,1:LM,KS),LM           &
               ,2,2)
!
            ENDIF
!
          ENDDO
        ENDIF
!
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!
      ENDIF tracers
!
!-----------------------------------------------------------------------
!***  Interface pressures and horizontal part of Omega-Alpha term
!-----------------------------------------------------------------------
!
      btim=timef()
!
      CALL VTOA                                                         &
        (LM,DT,EF4T,PT,SG2                                              &
        ,int_state%PSDT                                                 &
        ,int_state%DWDT,int_state%RTOP                                  &
        ,int_state%OMGALF                                               &
        ,int_state%PINT                                                 &
!
!***  Temporary arguments
!
        ,int_state%TDIV,int_state%TCT)
!
      td%vtoa_tim=td%vtoa_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for global forecasts
!-----------------------------------------------------------------------
!
      IF(GLOBAL)THEN
!
        btim=timef()
        CALL FFTFHN                                                     &
          (LM                                                           &
          ,int_state%KHFILT                                             &
          ,int_state%HFILT                                              &
          ,int_state%TCT                                                &




          ,int_state%WFFTRH,int_state%NFFTRH                            &

          ,NUM_PES,MYPE,MPI_COMM_COMP)
        td%fftfhn_tim=td%fftfhn_tim+(timef()-btim)
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  Update the temperature field.
!-----------------------------------------------------------------------
!
      btim=timef()
!
      CALL UPDATET                                                      &
        (LM                                                             &
        ,int_state%T                                                    &
!
!***  Temporary argument
!
        ,int_state%TCT)
!
      td%updatet_tim=td%updatet_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for global forecasts
!-----------------------------------------------------------------------
!
      IF(GLOBAL)THEN
!
        btim=timef()
        CALL SWAPHN(int_state%OMGALF,IMS,IME,JMS,JME,LM,INPES)
        CALL SWAPHN(int_state%PINT,IMS,IME,JMS,JME,LM+1,INPES)
        CALL SWAPHN(int_state%T,IMS,IME,JMS,JME,LM,INPES)
        td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
        btim=timef()
        CALL POLEHN(int_state%OMGALF,IMS,IME,JMS,JME,LM,INPES,JNPES)
        CALL POLEHN(int_state%PINT,IMS,IME,JMS,JME,LM+1,INPES,JNPES)
        CALL POLEHN(int_state%T,IMS,IME,JMS,JME,LM,INPES,JNPES)
        td%polehn_tim=td%polehn_tim+(timef()-btim)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      btim=timef()
      CALL HALO_EXCH(int_state%OMGALF,LM                                &
                    ,int_state%PINT,LM+1                                &
                    ,2,2)
      CALL HALO_EXCH(int_state%T,LM                                     &
                    ,2,2)
      td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Nonhydrostatic advection of height
!-----------------------------------------------------------------------
!
      btim=timef()
!
      CALL CDZDT                                                        &
        (GLOBAL,HYDRO                                                   &
        ,LM,DT,DSG2,PDSG1,FAH,FIS                                       &
        ,int_state%PD,int_state%PDO                                     &
        ,int_state%PSGDT                                                &
        ,int_state%CW,int_state%Q,int_state%RTOP,int_state%T            & 
        ,int_state%PINT                                                 &
        ,int_state%DWDT,int_state%PDWDT,int_state%W,int_state%BARO      &
        ,int_state%Z                                                    &
!
!***  temporary arguments
!
        ,int_state%PFNE,int_state%PFNW,int_state%PFX,int_state%PFY)
!
      td%cdzdt_tim=td%cdzdt_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for global forecasts
!-----------------------------------------------------------------------
!
      IF(GLOBAL)THEN
!
        btim=timef()
        CALL FFTFHN                                                     &
          (LM                                                           &
          ,int_state%KHFILT                                             &
          ,int_state%HFILT                                              &
          ,int_state%W                                                  &




          ,int_state%WFFTRH,int_state%NFFTRH                            &

          ,NUM_PES,MYPE,MPI_COMM_COMP)
        td%fftfhn_tim=td%fftfhn_tim+(timef()-btim)
!
        btim=timef()
        CALL SWAPHN(int_state%W,IMS,IME,JMS,JME,LM,INPES)
        td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
        btim=timef()
        CALL POLEHN(int_state%W,IMS,IME,JMS,JME,LM,INPES,JNPES)
        td%polehn_tim=td%polehn_tim+(timef()-btim)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      btim=timef()
      CALL HALO_EXCH(int_state%W,LM                                     &
                    ,3,3)
      td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Advection of W (with internal halo exchange)
!-----------------------------------------------------------------------
!
      btim=timef()
!
      CALL CDWDT                                                        &
        (GLOBAL,HYDRO,int_state%RESTART                                 &
        ,INPES,JNPES,LM,ABS(NTIMESTEP)                                  &
        ,DT,G,DSG2,PDSG1,PSGML1,FAH                                     &
        ,int_state%HDACX,int_state%HDACY                                &
        ,int_state%PD,int_state%PDO                                     &
        ,int_state%PSGDT                                                &
        ,int_state%DWDT,int_state%PDWDT,int_state%W                     &
        ,int_state%PINT                                                 &
!
!***  External scratch areas
!
        ,int_state%DEF,int_state%PFX,int_state%PFY                      &
        ,int_state%PFNE,int_state%PFNW)
!
      td%cdwdt_tim=td%cdwdt_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for global forecasts
!-----------------------------------------------------------------------
!
      IF(GLOBAL)THEN
!
        btim=timef()
        CALL FFTFHN                                                     &
          (LM                                                           &
          ,int_state%KHFILT                                             &
          ,int_state%HFILT                                              &
          ,int_state%DWDT                                               &




          ,int_state%WFFTRH,int_state%NFFTRH                            &

          ,NUM_PES,MYPE,MPI_COMM_COMP)
        td%fftfhn_tim=td%fftfhn_tim+(timef()-btim)
!
        btim=timef()
        CALL SWAPHN(int_state%DWDT,IMS,IME,JMS,JME,LM,INPES)
        td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
        btim=timef()
        CALL POLEHN(int_state%DWDT,IMS,IME,JMS,JME,LM,INPES,JNPES)
        td%polehn_tim=td%polehn_tim+(timef()-btim)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      btim=timef()
      CALL HALO_EXCH(int_state%DWDT,LM                                  &
                    ,2,2)
      td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Vertically propagating fast waves
!-----------------------------------------------------------------------
!
      btim=timef()
!
      CALL VSOUND                                                       &
        (GLOBAL,HYDRO,int_state%RESTART                                 &
        ,LM,ABS(NTIMESTEP)                                              &
        ,CP,DT,PT,DSG2,PDSG1                                            &
        ,int_state%PD                                                   &
        ,int_state%CW,int_state%Q,int_state%RTOP                        &
        ,int_state%DWDT,int_state%T,int_state%W,int_state%W_TOT         &
        ,int_state%BARO                                                 &
        ,int_state%PINT)
!
      td%vsound_tim=td%vsound_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for global forecasts
!-----------------------------------------------------------------------
!
      IF(GLOBAL)THEN
!
        btim=timef()
        CALL POAVHN                                                     &
          (IMS,IME,JMS,JME,LM                                           &
          ,int_state%DWDT                                               &
          ,INPES,JNPES                                                  &
          ,int_state%USE_ALLREDUCE                                      &
          ,int_state%READ_GLOBAL_SUMS                                   &
          ,int_state%WRITE_GLOBAL_SUMS)
        CALL POAVHN                                                     &
          (IMS,IME,JMS,JME,LM                                           &
          ,int_state%W                                                  &
          ,INPES,JNPES                                                  &
          ,int_state%USE_ALLREDUCE                                      &
          ,int_state%READ_GLOBAL_SUMS                                   &
          ,int_state%WRITE_GLOBAL_SUMS)
        CALL POAVHN                                                     &
          (IMS,IME,JMS,JME,LM                                           &
          ,int_state%PINT                                               &
          ,INPES,JNPES                                                  &
          ,int_state%USE_ALLREDUCE                                      &
          ,int_state%READ_GLOBAL_SUMS                                   &
          ,int_state%WRITE_GLOBAL_SUMS)
        td%poavhn_tim=td%poavhn_tim+(timef()-btim)
!
        btim=timef()
        CALL SWAPHN(int_state%DWDT,IMS,IME,JMS,JME,LM,INPES)
        CALL SWAPHN(int_state%T,IMS,IME,JMS,JME,LM,INPES)
        CALL SWAPHN(int_state%W,IMS,IME,JMS,JME,LM,INPES)
        CALL SWAPHN(int_state%PINT,IMS,IME,JMS,JME,LM+1,INPES)
        td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
        btim=timef()
        CALL POLEHN(int_state%DWDT,IMS,IME,JMS,JME,LM,INPES,JNPES)
        CALL POLEHN(int_state%T,IMS,IME,JMS,JME,LM,INPES,JNPES)
        CALL POLEHN(int_state%W,IMS,IME,JMS,JME,LM,INPES,JNPES)
        CALL POLEHN(int_state%PINT,IMS,IME,JMS,JME,LM+1,INPES,JNPES)
        td%polehn_tim=td%polehn_tim+(timef()-btim)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      btim=timef()
      CALL HALO_EXCH(int_state%DWDT,LM                                  &
                    ,int_state%T,LM                                     &
                    ,2,2)
      CALL HALO_EXCH(int_state%W,LM                                     &
                    ,int_state%PINT,LM+1                                &
                    ,2,2)
      td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!
      OLD_PASSIVE=.NOT.int_state%ADVECT_TRACERS                            !<-- The old scheme and new scheme are mutually exclusive
!
      passive_advec: IF(MOD(ABS(NTIMESTEP),IDTAD)==0.AND.OLD_PASSIVE)THEN
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Vertical advection of passive quantities 
!-----------------------------------------------------------------------
!
        btim=timef()
!
        vadv2_micro_check: IF(.NOT.int_state%SPEC_ADV)THEN
          CALL AVEQ2                                                    &
            (LM                                                         &
            ,DSG2,PDSG1,PSGML1,SGML2                                    &
            ,int_state%PD                                               &
            ,int_state%Q2,int_state%E2                                  &
            ,1)
!
          CALL VADV2_SCAL                                               &
            (LM,IDTAD                                                   &
            ,DT,DSG2,PDSG1,PSGML1,SGML2                                 &
            ,int_state%PD,int_state%PSGDT                               &
            ,int_state%TRACERS                                          &
            ,int_state%NUM_TRACERS_MET,1,int_state%INDX_Q2)
!
        ELSE vadv2_micro_check
          CALL VADV2_SCAL                                               &
            (LM,IDTAD                                                   &
            ,DT,DSG2,PDSG1,PSGML1,SGML2                                 &
            ,int_state%PD,int_state%PSGDT                               &
            ,int_state%Q2                                               &
            ,1,1,int_state%INDX_Q2)
!
          CALL VADV2_SCAL                                               &
            (LM,IDTAD                                                   &
            ,DT,DSG2,PDSG1,PSGML1,SGML2                                 &
            ,int_state%PD,int_state%PSGDT                               &
            ,int_state%WATER                                            &
            ,int_state%NUM_WATER,2,int_state%INDX_Q2)
!
          DO K=1,LM
          DO J=JTS,JTE
          DO I=ITS,ITE
     !      int_state%Q(I,J,K)=int_state%WATER(I,J,K,P_QV)              &
     !                  /(1.+int_state%WATER(I,J,K,P_QV))
          ENDDO
          ENDDO
          ENDDO
!
          int_state%Q(:,:,:)=int_state%WATER(:,:,:,P_QV)                &
                      /(1.+int_state%WATER(:,:,:,P_QV))
!
        ENDIF vadv2_micro_check
!
        td%vadv2_tim=td%vadv2_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for global forecasts
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
          CALL FFTFHN                                                   &
            (LM                                                         &
            ,int_state%KHFILT                                           &
            ,int_state%HFILT                                            &
            ,int_state%CW                                               &




            ,int_state%WFFTRH,int_state%NFFTRH                          &

            ,NUM_PES,MYPE,MPI_COMM_COMP)
!
          CALL FFTFHN                                                   &
            (LM                                                         &
            ,int_state%KHFILT                                           &
            ,int_state%HFILT                                            &
            ,int_state%Q                                                &




            ,int_state%WFFTRH,int_state%NFFTRH                          &

            ,NUM_PES,MYPE,MPI_COMM_COMP)
!
          CALL FFTFHN                                                   &
            (LM                                                         &
            ,int_state%KHFILT                                           &
            ,int_state%HFILT                                            &
            ,int_state%E2                                               &




            ,int_state%WFFTRH,int_state%NFFTRH                          &

            ,NUM_PES,MYPE,MPI_COMM_COMP)
!
          CALL FFTFHN                                                   &
            (LM                                                         &
            ,int_state%KHFILT                                           &
            ,int_state%HFILT                                            &
            ,int_state%O3                                               &




            ,int_state%WFFTRH,int_state%NFFTRH                          &

            ,NUM_PES,MYPE,MPI_COMM_COMP)
!
          IF(int_state%SPEC_ADV)THEN
!
            DO N=2,int_state%NUM_WATER
              CALL FFTFHN                                               &
                (LM                                                     &
                ,int_state%KHFILT                                       &
                ,int_state%HFILT                                        &
                ,int_state%WATER(:,:,:,N)                               &




                ,int_state%WFFTRH,int_state%NFFTRH                      &

                ,NUM_PES,MYPE,MPI_COMM_COMP)
            ENDDO
!
          ENDIF
!
          td%fftfhn_tim=td%fftfhn_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!
          btim=timef()
          CALL SWAPHN(int_state%Q,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%CW,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%O3,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%Q2,IMS,IME,JMS,JME,LM,INPES)
!
          IF(int_state%SPEC_ADV)THEN
            DO N=2,int_state%NUM_WATER
              CALL SWAPHN(int_state%WATER(:,:,:,N)                      &
                         ,IMS,IME,JMS,JME,LM,INPES)
            ENDDO
          ENDIF
!
          td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEHN(int_state%Q,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%CW,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%O3,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%Q2,IMS,IME,JMS,JME,LM,INPES,JNPES)
!
          IF(int_state%SPEC_ADV)THEN
            DO N=2,int_state%NUM_WATER
              CALL POLEHN(int_state%WATER(:,:,:,N)                      &
                         ,IMS,IME,JMS,JME,LM,INPES,JNPES)
            ENDDO
          ENDIF
!
          td%polehn_tim=td%polehn_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL HALO_EXCH(int_state%Q,LM                                   &
                      ,int_state%CW,LM                                  &
                      ,int_state%O3,LM                                  &
                      ,int_state%Q2,LM                                  &
                      ,2,2)
!
        CALL HALO_EXCH(int_state%E2,LM                                  &
                      ,1,1)
!
        IF(int_state%SPEC_ADV)THEN
          CALL HALO_EXCH(int_state%WATER,LM,int_state%NUM_WATER,2       &
                        ,2,2)
        ENDIF
!
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Horizontal advection of passive quantities
!***  (internal halo exchange)
!-----------------------------------------------------------------------
!
        btim=timef()
!
        hadv2_micro_check: IF(.NOT.int_state%SPEC_ADV)THEN
!
          CALL HADV2_SCAL                                               &
            (GLOBAL,INPES,JNPES                                         &
            ,LM,IDTAD,DT,RDYH                                           &
            ,DSG2,PDSG1,PSGML1,SGML2                                    &
            ,DARE,RDXH                                                  &
            ,int_state%PD                                               &
            ,int_state%U,int_state%V                                    &
            ,int_state%TRACERS                                          &
            ,int_state%NUM_TRACERS_MET,1,int_state%INDX_Q2              &
            ,int_state%READ_GLOBAL_SUMS                                 &
            ,int_state%WRITE_GLOBAL_SUMS)
!
          CALL AVEQ2                                                    &
            (LM                                                         &
            ,DSG2,PDSG1,PSGML1,SGML2                                    &
            ,int_state%PD                                               &
            ,int_state%Q2,int_state%E2                                  &
            ,2)
!
!-----------------------------------------------------------------------
!***  Update the WATER array.
!***  Remember that WATER is used with the WRF physics and thus
!***  the P_QV slot (=2) is mixing ratio, not specific humidity.
!***  Although WATER is only used for physics in operations, it is
!***  updated here from Q every advection timestep for non-operational
!***  configurations where it may be used outside of the physics.
!-----------------------------------------------------------------------
!
          IF(.NOT.int_state%OPERATIONAL_PHYSICS)THEN
!
            DO K=1,LM
            KFLIP=LM+1-K
            DO J=JTS,JTE
            DO I=ITS,ITE
              int_state%WATER(I,J,K,P_QV)=int_state%Q(I,J,K)/(1.-int_state%Q(I,J,K))
              WC = int_state%CW(I,J,K)
              QI = 0.
              QR = 0.
              QW = 0.
              FICE=int_state%F_ICE(I,J,KFLIP)
              FRAIN=int_state%F_RAIN(I,J,KFLIP)
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
              int_state%WATER(I-ITS+1,J-JTS+1,K,P_QC)=QW
              int_state%WATER(I-ITS+1,J-JTS+1,K,P_QR)=QR
              int_state%WATER(I-ITS+1,J-JTS+1,K,P_QI)=0.
              int_state%WATER(I-ITS+1,J-JTS+1,K,P_QS)=QI
              int_state%WATER(I-ITS+1,J-JTS+1,K,P_QG)=0.
            ENDDO
            ENDDO
            ENDDO
          ENDIF
!
        ELSE hadv2_micro_check
!
          CALL HADV2_SCAL                                               &
            (GLOBAL,INPES,JNPES                                         &
            ,LM,IDTAD,DT,RDYH                                           &
            ,DSG2,PDSG1,PSGML1,SGML2                                    &
            ,DARE,RDXH                                                  &
            ,int_state%PD                                               &
            ,int_state%U,int_state%V                                    &
            ,int_state%Q2                                               &
            ,1,1,int_state%INDX_Q2                                      &
            ,int_state%READ_GLOBAL_SUMS                                 &
            ,int_state%WRITE_GLOBAL_SUMS)
!
          CALL HADV2_SCAL                                               &
            (GLOBAL,INPES,JNPES                                         &
            ,LM,IDTAD,DT,RDYH                                           &
            ,DSG2,PDSG1,PSGML1,SGML2                                    &
            ,DARE,RDXH                                                  &
            ,int_state%PD                                               &
            ,int_state%U,int_state%V                                    &
            ,int_state%WATER                                            &
            ,int_state%NUM_WATER,2,int_state%INDX_Q2                    &
            ,int_state%READ_GLOBAL_SUMS                                 &
            ,int_state%WRITE_GLOBAL_SUMS)
!
        ENDIF hadv2_micro_check
!
        td%hadv2_tim=td%hadv2_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Filtering and boundary conditions for global forecasts
!-----------------------------------------------------------------------
!
        IF(GLOBAL)THEN
!
          btim=timef()
          CALL SWAPHN(int_state%Q,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%CW,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%O3,IMS,IME,JMS,JME,LM,INPES)
          CALL SWAPHN(int_state%Q2,IMS,IME,JMS,JME,LM,INPES)
!
          IF(int_state%SPEC_ADV)THEN
            DO N=2,int_state%NUM_WATER
              CALL SWAPHN(int_state%WATER(:,:,:,N)                      &
                         ,IMS,IME,JMS,JME,LM,INPES)
            ENDDO
          ENDIF
!
          td%swaphn_tim=td%swaphn_tim+(timef()-btim)
!
          btim=timef()
          CALL POLEHN(int_state%Q,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%CW,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%O3,IMS,IME,JMS,JME,LM,INPES,JNPES)
          CALL POLEHN(int_state%Q2,IMS,IME,JMS,JME,LM,INPES,JNPES)
!
          IF(int_state%SPEC_ADV)THEN
            DO N=2,int_state%NUM_WATER
              CALL POLEHN(int_state%WATER(:,:,:,N)                      &
                         ,IMS,IME,JMS,JME,LM,INPES,JNPES)
            ENDDO
          ENDIF
!
          td%polehn_tim=td%polehn_tim+(timef()-btim)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
        btim=timef()
        CALL HALO_EXCH(int_state%Q,LM                                   &
                      ,int_state%CW,LM                                  &
                      ,int_state%O3,LM                                  &
                      ,int_state%Q2,LM                                  &
                      ,2,2)
!
        IF(int_state%SPEC_ADV)THEN
          CALL HALO_EXCH(int_state%WATER,LM,int_state%NUM_WATER,2       &
                        ,2,2)
        ENDIF
!
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!
      ENDIF passive_advec
!
!-----------------------------------------------------------------------
!***  Close the file units used for Reads/Writes of global sums
!***  if the forecast is finished.
!-----------------------------------------------------------------------
!
      IF(ESMF_ClockIsStopTime(clock=CLOCK_ATM,rc=RC))THEN
        IF(int_state%WRITE_GLOBAL_SUMS.AND.MYPE==0)THEN
          CLOSE(IUNIT_ADVEC_SUMS)
          CLOSE(IUNIT_POLE_SUMS)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!***  Save DT to compare and see if sign has changed for filtering.
!-----------------------------------------------------------------------
!
      int_state%DT_LAST=DT_TEST
      int_state%DT_TEST_RATIO=REAL(INTEGER_DT)+REAL(NUMERATOR_DT)       &
                                              /REAL(IDENOMINATOR_DT)
      int_state%FILTER_METHOD_LAST=FILTER_METHOD
!
!-----------------------------------------------------------------------
!***  NOTE:  The Solver export state is fully updated now
!***         because subroutine SOLVER_INITIALIZE inserted the 
!***         appropriate ESMF Fields into it.  Those Fields 
!***         contain pointers to the actual data and those
!***         pointers are never re-directed, i.e., no explicit
!***         action is needed to update the Solver export state.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Write the layer statistics for temperature.
!-----------------------------------------------------------------------
!
      IF(MOD(ABS(NTIMESTEP)+1,N_PRINT_STATS)==0)THEN
!
        IF(int_state%PRINT_DIAG .OR. int_state%PRINT_ALL) &
        CALL FIELD_STATS(INT_STATE%T,MYPE,MPI_COMM_COMP,LM              &
                        ,ITS,ITE,JTS,JTE                                &
                        ,IMS,IME,JMS,JME                                &
                        ,IDS,IDE,JDS,JDE)
      ENDIF
!
      td%solver_dyn_tim=td%solver_dyn_tim+(timef()-btim0)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----PHY_RUN START -----------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!rv - please do not remove this template call:
!     if(mod(nint(dt*ntimestep),60)==0.and.nint(dt*ntimestep)<=1800)then
!       call twr(int_state%t,lm,'tphy',ntimestep,mype,num_pes,mpi_comm_comp &
!               ,ids,ide,jds,jde &
!               ,ims,ime,jms,jme &
!               ,its,ite,jts,jte &
!               ,my_domain_id )
!       call vwr(int_state%u,lm,'uphy',ntimestep,mype,num_pes,mpi_comm_comp &
!               ,ids,ide,jds,jde &
!               ,ims,ime,jms,jme &
!               ,its,ite,jts,jte &
!               ,my_domain_id )
!rv
!
      physics: IF(INTEGER_DT>0)THEN                                     !<-- Physics is active
!
      btim0=timef()
!
!-----------------------------------------------------------------------
!***  Call radiation so that updated fields are written to the
!***  history files after 0 hours.
!-----------------------------------------------------------------------
!
      IF(NTIMESTEP==0)THEN
         NTIMESTEP_RAD=NTIMESTEP
      ELSE
         NTIMESTEP_RAD=NTIMESTEP+1
      ENDIF
!
!-----------------------------------------------------------------------
!***  Dereference some internal state components for convenience.
!-----------------------------------------------------------------------
!
      NPRECIP=int_state%NPRECIP
      PDTOP=int_state%PDTOP
      PT=int_state%PT
!
      DO L=1,LM
        DSG2(L)=int_state%DSG2(L)
        PDSG1(L)=int_state%PDSG1(L)
        PSGML1(L)=int_state%PSGML1(L)
        SGML2(L)=int_state%SGML2(L)
      ENDDO
!
      DO L=1,LM+1
        SG2(L)=INT_STATE%SG2(L)
        PSG1(L)=INT_STATE%PSG1(L)
      ENDDO
!
!-----------------------------------------------------------------------
!
      gfs_phys_test: IF(.NOT.int_state%GFS)THEN                            !<-- NMM-B physics is NOT the GFS package
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  At the appropriate times, reset the various min/max/average
!***  diagnostic fields to begin accumulating for the next period
!-----------------------------------------------------------------------
!
      IF(NTIMESTEP == 0 .or. MOD(NTIMESTEP,NSTEPS_PER_RESET)==0) THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          int_state%TLMAX(I,J)=-999.
          int_state%TLMIN(I,J)=999.
          int_state%T02MAX(I,J)=-999.
          int_state%T02MIN(I,J)=999.
          int_state%RH02MAX(I,J)=-999.
          int_state%RH02MIN(I,J)=999.
          int_state%SPD10MAX(I,J)=-999.
          int_state%UPHLMAX(I,J)=0.
          int_state%U10MAX(I,J)=-999.
          int_state%V10MAX(I,J)=-999.
          int_state%UPVVELMAX(I,J)=-999.
          int_state%DNVVELMAX(I,J)=999.
          int_state%T10AVG(I,J)=0.
          int_state%PSFCAVG(I,J)=0.
          int_state%AKHSAVG(I,J)=0.
          int_state%AKMSAVG(I,J)=0.
          int_state%SNOAVG(I,J)=0.
          int_state%REFDMAX(I,J)=-999.
          int_state%UPHLMAX(I,J)=-999.
        ENDDO
        ENDDO
!
        int_state%NCOUNT=0
      ENDIF
!
!     IF (mod(int_state%NTSD,NSTEPS_PER_CHECK) == 0) THEN
      IF (mod(int_state%NTSD,NSTEPS_PER_CHECK) == 0 .and. FILTER_METHOD==0 ) THEN
!
        IF (TRIM(int_state%MICROPHYSICS) == 'fer') THEN
!
          CALL MAX_FIELDS(int_state%T,int_state%Q,int_state%U            &
                         ,int_state%V,int_state%CW                       &
                         ,int_state%F_RAIN,int_state%F_ICE               &
                         ,int_state%F_RIMEF,int_state%Z                  &
                         ,int_state%W_TOT,int_state%PINT                 &
                         ,int_state%PD                                   &
                         ,int_state%CPRATE,int_state%HTOP                &
                         ,int_state%T2,int_state%U10,int_state%V10       &
                         ,int_state%PSHLTR,int_state%TSHLTR              &
                         ,int_state%QSHLTR                               &
                         ,int_state%SGML2,int_state%PSGML1               &
                         ,int_state%REFDMAX                              &
                         ,int_state%UPVVELMAX,int_state%DNVVELMAX        &
                         ,int_state%TLMAX,int_state%TLMIN                &
                         ,int_state%T02MAX,int_state%T02MIN              &
                         ,int_state%RH02MAX,int_state%RH02MIN            &
                         ,int_state%U10MAX,int_state%V10MAX              &
                         ,int_state%TH10,int_state%T10                   &
                         ,int_state%SPD10MAX,int_state%T10AVG            &
                         ,int_state%PSFCAVG                              &
                         ,int_state%AKHS,int_state%AKMS                  &
                         ,int_state%AKHSAVG,int_state%AKMSAVG            &
                         ,int_state%SNO,int_state%SNOAVG                 &
                         ,int_state%UPHLMAX                              &
                         ,int_state%DT,int_state%NPHS,int_state%NTSD     &
                         ,int_state%DXH,int_state%DYH                    &
                         ,int_state%FIS                                  &
                         ,ITS,ITE,JTS,JTE                                &
                         ,IMS,IME,JMS,JME                                &
                         ,IDE,JDE                                        &
                         ,ITS_B1,ITE_B1,JTS_B1,JTE_B1                    &
                         ,LM,int_state%NCOUNT,int_state%FIRST_NMM)
!
        ELSEIF (TRIM(int_state%MICROPHYSICS) == 'fer_hires') THEN
!
          CALL MAX_FIELDS_HR(int_state%T,int_state%Q,int_state%U         &
                            ,int_state%V,int_state%CW                    &
                            ,int_state%F_RAIN,int_state%F_ICE            &
                            ,int_state%F_RIMEF,int_state%Z               &
                            ,int_state%W_TOT,int_state%PINT              &
                            ,int_state%PD                                &
                            ,int_state%CPRATE,int_state%HTOP             &
                            ,int_state%T2,int_state%U10,int_state%V10    &
                            ,int_state%PSHLTR,int_state%TSHLTR           &
                            ,int_state%QSHLTR                            &
                            ,int_state%SGML2,int_state%PSGML1            &
                            ,int_state%REFDMAX                           &
                            ,int_state%UPVVELMAX,int_state%DNVVELMAX     &
                            ,int_state%TLMAX,int_state%TLMIN             &
                            ,int_state%T02MAX,int_state%T02MIN           &
                            ,int_state%RH02MAX,int_state%RH02MIN         &
                            ,int_state%U10MAX,int_state%V10MAX           &
                            ,int_state%TH10,int_state%T10                &
                            ,int_state%SPD10MAX,int_state%T10AVG         &
                            ,int_state%PSFCAVG                           &
                            ,int_state%AKHS,int_state%AKMS               &
                            ,int_state%AKHSAVG,int_state%AKMSAVG         &
                            ,int_state%SNO,int_state%SNOAVG              &
                            ,int_state%UPHLMAX                           &
                            ,int_state%DT,int_state%NPHS,int_state%NTSD  &
                            ,int_state%DXH,int_state%DYH                 &
                            ,int_state%FIS                               &
                            ,ITS,ITE,JTS,JTE                             &
                            ,IMS,IME,JMS,JME                             &
                            ,IDE,JDE                                     &
                            ,ITS_B1,ITE_B1,JTS_B1,JTE_B1                 &
                            ,LM,int_state%NCOUNT,int_state%FIRST_NMM)
!
       ELSEIF (TRIM(int_state%MICROPHYSICS) == 'wsm6') THEN
!
         CALL MAX_FIELDS_W6(int_state%T,int_state%Q,int_state%U         &
                           ,int_state%V,int_state%Z,int_state%W_TOT     &
                           ,int_state%WATER                             &
                           ,int_state%PINT,int_state%PD                 &
                           ,int_state%CPRATE,int_state%HTOP             &
                           ,int_state%T2,int_state%U10,int_state%V10    &
                           ,int_state%PSHLTR,int_state%TSHLTR           &
                           ,int_state%QSHLTR                            &
                           ,int_state%SGML2,int_state%PSGML1            &
                           ,int_state%REFDMAX                           &
                           ,int_state%UPVVELMAX,int_state%DNVVELMAX     &
                           ,int_state%TLMAX,int_state%TLMIN             &
                           ,int_state%T02MAX,int_state%T02MIN           &
                           ,int_state%RH02MAX,int_state%RH02MIN         &
                           ,int_state%U10MAX,int_state%V10MAX           &
                           ,int_state%TH10,int_state%T10                &
                           ,int_state%SPD10MAX,int_state%T10AVG         &
                           ,int_state%PSFCAVG                           &
                           ,int_state%AKHS,int_state%AKMS               &
                           ,int_state%AKHSAVG,int_state%AKMSAVG         &
                           ,int_state%SNO,int_state%SNOAVG              &
                           ,int_state%UPHLMAX                           &
                           ,int_state%DT,int_state%NPHS,int_state%NTSD  &
                           ,int_state%DXH,int_state%DYH                 &
                           ,int_state%FIS                               &
                           ,int_state%P_QR,int_state%P_QS               &
                           ,int_state%P_QG                              &
                           ,ITS,ITE,JTS,JTE                             &
                           ,IMS,IME,JMS,JME                             &
                           ,IDE,JDE                                     &
                           ,ITS_B1,ITE_B1,JTS_B1,JTE_B1                 &
                           ,LM,int_state%NUM_WATER                      &
                           ,int_state%NCOUNT,int_state%FIRST_NMM)
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  Set logical switches for calling each of the Physics schemes.
!-----------------------------------------------------------------------
!
        CALL_SHORTWAVE=MOD(NTIMESTEP_RAD,int_state%NRADS)==0
        CALL_LONGWAVE=MOD(NTIMESTEP_RAD,int_state%NRADL)==0
        CALL_TURBULENCE=MOD(NTIMESTEP,int_state%NPHS)==0
        CALL_PRECIP=MOD(NTIMESTEP,NPRECIP)==0
!
!-----------------------------------------------------------------------
!***  Update WATER array from CWM, F_ICE, F_RAIN for Ferrier 
!***  microphysics but only if any of the Physics subroutines 
!***  are called (subroutine UPDATE_WATER is after subroutine
!***  PHYSICS_INITIALIZE in this module).
!
!***  Expanded to also update CWM, F_ICE, F_RAIN, F_RIMEF for non-Ferrier
!***  microphysics.
!-----------------------------------------------------------------------
!
        update_wtr: IF((int_state%MICROPHYSICS=='fer'                   &
                                   .OR.                                 &
                        int_state%MICROPHYSICS=='fer_hires'             &
                                   .OR.                                 &
                        int_state%MICROPHYSICS=='gfs'                   &
                                   .OR.                                 &
                        int_state%MICROPHYSICS=='wsm6')                 &
                                   .AND.                                &
                       (CALL_SHORTWAVE .OR. CALL_LONGWAVE .OR.          &
                        CALL_TURBULENCE .OR. CALL_PRECIP) ) THEN
!
           CALL UPDATE_WATER(int_state%CW                               &
                            ,int_state%F_ICE                            &
                            ,int_state%F_RAIN                           &
                            ,int_state%F_RIMEF                          &
                            ,int_state%NUM_WATER                        &
                            ,int_state%WATER                            &
                            ,int_state%T                                &
                            ,int_state%P_QC                             &
                            ,int_state%P_QR                             &
                            ,int_state%P_QS                             &
                            ,int_state%P_QI                             &
                            ,int_state%P_QG                             &
                            ,int_state%MICROPHYSICS                     &
                            ,int_state%SPEC_ADV                         &
                            ,NTIMESTEP                                  &
                            ,IDS,IDE,JDS,JDE,LM                         &
                            ,IMS,IME,JMS,JME                            &
                            ,ITS,ITE,JTS,JTE)
        ENDIF update_wtr
!
!---------------------------------------------------------------------
!***  Precipitation Adjustment
!-----------------------------------------------------------------------
!
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
!
        IF(int_state%NTSD==0)THEN
          IF(int_state%PCPFLG .and. FILTER_METHOD == 0)THEN
            CALL READPCP(MYPE,MPI_COMM_COMP                             &
                        ,int_state%PPTDAT                               &
                        ,int_state%DDATA                                &
                        ,int_state%LSPA                                 &
                        ,int_state%PCPHR                                &
                        ,MY_DOMAIN_ID                                   &
                        ,IDS,IDE,JDS,JDE,LM                             &
                        ,IMS,IME,JMS,JME                                &
                        ,ITS,ITE,JTS,JTE                                &
                        ,ITS_B1,ITE_B1,JTS_B2,JTE_B2)
          ENDIF
        ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Call the individual physical processes.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Radiation
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!***  Radiation needs some specific time quantities.  Use NTIMESTEP_rad 
!***  for the next time step ahead of the current time so that the
!***  radiation fields can be updated prior to being written to
!***  output (BSF 10/6/2010).
!
        CALL TIME_MEASURE(START_YEAR,START_MONTH,START_DAY,START_HOUR   &
                         ,START_MINUTE,START_SECOND                     &
                         ,NTIMESTEP_rad,int_state%DT                    &
                         ,JULDAY,JULYR,JULIAN,XTIME)
!
!-----------------------------------------------------------------------
        radiatn: IF(CALL_SHORTWAVE.OR.CALL_LONGWAVE)THEN
!-----------------------------------------------------------------------
!
          btim=timef()
!
!-----------------------------------------------------------------------
!***  Temporary switch between radiation schemes placed in SOLVER_RUN
!***  rather than inside RADIATION_DRIVER (will be done later)
!-----------------------------------------------------------------------
!
          CALL ESMF_ClockGet(clock       =CLOCK_ATM                     &  !<-- The ESMF Clock
                            ,startTime   =STARTTIME                     &  !<-- The start time (ESMF) on the clock
                            ,currTime    =CURRTIME                      &  !<-- The current time (ESMF) on the clock
                            ,rc          =RC)
!
          CALL ESMF_TimeGet(time=STARTTIME                              &  !<-- The start forecast time (ESMF)
                           ,yy  =IDAT(1)                                &  !<-- The start forecast year (integer)
                           ,mm  =IDAT(2)                                &  !<-- The start forecast month (integer)
                           ,dd  =IDAT(3)                                &  !<-- The start forecast day (integer)
                           ,h   =IDAT(5)                                &  !<-- The start forecast hour (integer)
                           ,m   =IDAT(6)                                &  !<-- The start forecast minute (integer)
                           ,s   =IDAT(7)                                &  !<-- The start forecast second (integer)
                           ,rc  =RC)
          IDAT(4)=0
          IDAT(8)=0
!
          CALL ESMF_TimeGet(time=CURRTIME                               &  !<-- The cuurent forecast time (ESMF)
                           ,yy  =JDAT(1)                                &  !<-- The current forecast year (integer)
                           ,mm  =JDAT(2)                                &  !<-- The current forecast month (integer)
                           ,dd  =JDAT(3)                                &  !<-- The current forecast day (integer)
                           ,h   =JDAT(5)                                &  !<-- The current forecast hour (integer)
                           ,m   =JDAT(6)                                &  !<-- The current forecast minute (integer)
                           ,s   =JDAT(7)                                &  !<-- The current forecast second (integer)
                           ,rc  =RC)
          JDAT(4)=0
          JDAT(8)=0
!
          CALL RADIATION(NTIMESTEP_RAD                                  &
                        ,int_state%DT,JULDAY,JULYR,XTIME,JULIAN         &
                        ,START_HOUR,int_state%NPHS                      &
                        ,int_state%GLAT,int_state%GLON                  &
                        ,int_state%NRADS,int_state%NRADL                &
                        ,DSG2,SGML2,PDSG1,PSGML1                        &
                        ,int_state%PT,int_state%PD                      &
                        ,int_state%T,int_state%Q                        &
                        ,int_state%THS,int_state%ALBEDO                 &
                        ,int_state%P_QV,int_state%P_QC,int_state%P_QR   &
                        ,int_state%P_QI,int_state%P_QS,int_state%P_QG   &
                        ,int_state%F_QV,int_state%F_QC,int_state%F_QR   &
                        ,int_state%F_QI,int_state%F_QS,int_state%F_QG   &
                        ,int_state%SM,int_state%CLDFRA                  &
                        ,int_state%NUM_WATER,int_state%WATER            &
                        ,int_state%RLWTT,int_state%RSWTT                &
                        ,int_state%RLWIN,int_state%RSWIN                &
                        ,int_state%RSWINC,int_state%RSWOUT              &
                        ,int_state%RLWTOA,int_state%RSWTOA              &
                        ,int_state%CZMEAN,int_state%SIGT4               &
                        ,int_state%CFRACL,int_state%CFRACM              &
                        ,int_state%CFRACH                               &
                        ,int_state%ACFRST,int_state%NCFRST              &
                        ,int_state%ACFRCV,int_state%NCFRCV              &
                        ,int_state%CUPPT,int_state%SNO                  &
                        ,int_state%HTOP,int_state%HBOT                  &
                        ,int_state%SHORTWAVE,int_state%LONGWAVE         &
!---- RRTM part ---------------------------------------------------------
                        ,int_state%DT_INT,JDAT                          &
                        ,int_state%CW,int_state%O3                      &
                        ,int_state%F_ICE,int_state%F_RAIN               &
                        ,int_state%F_RIMEF                              &
                        ,int_state%SI,int_state%TSKIN                   &
                        ,int_state%Z0,int_state%SICE                    &
                        ,int_state%MXSNAL,int_state%SGM                 &
                        ,int_state%STDH,int_state%OMGALF                &
!------------------------------------------------------------------------
                        ,LM)
!
          td%radiation_tim=td%radiation_tim+(timef()-btim)
!
        ENDIF radiatn
!
!-----------------------------------------------------------------------
!***  Empty the ACFRST and ACFRCV accumulation arrays if it is time
!***  to do so prior to their being updated by the radiation.
!-----------------------------------------------------------------------
!
        IF(MOD(NTIMESTEP,int_state%NCLOD)==0)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%ACFRST(I,J)=0.
            int_state%ACFRCV(I,J)=0.
            int_state%NCFRST(I,J)=0
            int_state%NCFRCV(I,J)=0
          ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!***  Update the temperature with the radiative tendency.
!-----------------------------------------------------------------------
!
        btim=timef()
!
        CALL RDTEMP(NTIMESTEP,int_state%DT,JULDAY,JULYR,START_HOUR      &
                   ,int_state%GLAT,int_state%GLON                       &
                   ,int_state%CZEN,int_state%CZMEAN,int_state%T         &
                   ,int_state%RSWTT,int_state%RLWTT                     &
                   ,IDS,IDE,JDS,JDE,LM                                  &
                   ,IMS,IME,JMS,JME                                     &
                   ,ITS,ITE,JTS,JTE                                     &
                   ,ITS_B1,ITE_B1,JTS_B1,JTE_B1)
!
        td%rdtemp_tim=td%rdtemp_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Poles and East-West boundary.
!-----------------------------------------------------------------------
!
        IF(int_state%GLOBAL)THEN
          btim=timef()
!
          CALL SWAPHN(int_state%RSWIN,IMS,IME,JMS,JME,1,int_state%INPES)
          CALL POLEHN(int_state%RSWIN,IMS,IME,JMS,JME,1                  &
                     ,int_state%INPES,int_state%JNPES)
!
          CALL SWAPHN(int_state%T,IMS,IME,JMS,JME,LM,int_state%INPES)
          CALL POLEHN(int_state%T,IMS,IME,JMS,JME,LM                     &
                     ,int_state%INPES,int_state%JNPES)
!
          td%pole_swap_tim=td%pole_swap_tim+(timef()-btim)
        ENDIF
!
!-----------------------------------------------------------------------
!***  Empty the accumulators of sfc energy flux and sfc hydrology if
!***  it is time to do so prior to their being updated by turbulence.
!-----------------------------------------------------------------------
!
        IF(MOD(NTIMESTEP,int_state%NRDLW)==0)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%ALWIN(I,J) =0.
            int_state%ALWOUT(I,J)=0.
            int_state%ALWTOA(I,J)=0.
            int_state%ARDLW(I,J) =0.                                       !<-- An artificial 2-D array
                                                                           !    (ESMF cannot have an evolving scalar Attribute)
          ENDDO
          ENDDO
        ENDIF
!
        IF(MOD(NTIMESTEP,int_state%NRDSW)==0)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%ASWIN(I,J)=0.
            int_state%ASWOUT(I,J)=0.
            int_state%ASWTOA(I,J)=0.
            int_state%ARDSW(I,J) =0.                                       !<-- An artificial 2-D array 
                                                                           !    (ESMF cannot have an evolving scalar Attribute)
          ENDDO
          ENDDO
        ENDIF
!
        IF(MOD(NTIMESTEP,int_state%NSRFC)==0)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%SFCSHX(I,J)=0.
            int_state%SFCLHX(I,J)=0.
            int_state%SUBSHX(I,J)=0.
            int_state%SNOPCX(I,J)=0.
            int_state%POTFLX(I,J)=0.
            int_state%ASRFC(I,J) =0.                                       !<-- An artificial 2-D array
                                                                           !    (ESMF cannot have an evolving scalar Attribute)
          ENDDO
          ENDDO
        ENDIF
!
        IF(MOD(NTIMESTEP,int_state%NPREC)==0)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%ACSNOW(I,J)=0.
              int_state%ACSNOM(I,J)=0.
            int_state%SSROFF(I,J)=0.
            int_state%BGROFF(I,J)=0.
            int_state%SFCEVP(I,J)=0.
            int_state%POTEVP(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Turbulence, Sfc Layer, and Land Surface
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
        turbulence: IF(CALL_TURBULENCE)THEN
!
          btim=timef()
!
          DO L=1,NUM_SOIL_LAYERS
            DZSOIL(L)=SLDPTH(L)
          ENDDO
!
          IF(int_state%PCPFLG .and. FILTER_METHOD == 0)THEN
            LOC_PCPFLG=int_state%PCPFLG
          ELSE
            LOC_PCPFLG=.FALSE.
          ENDIF
!
        
!        if (ITS .le. 959 .and. ITE .ge. 959 .and. & 
!            JTS .le. 439 .and. JTE .ge. 439) then
!            write(0,*) 'T,Q,THS into TURBL: ', int_state%T(959,439,LM), & 
!                                               int_state%Q(959,439,LM), & 
!                                          int_state%THS(959,439)
!        write(0,*) 'VEGFRC,Z0: ', int_state%VEGFRC(959,439), &
!                                  int_state%Z0(959,439)
!        write(0,*) 'STC: ', int_state%STC(959,439,1:4)
!        write(0,*) 'SMC: ', int_state%SMC(959,439,1:4)
!        endif

        

          CALL TURBL(NTIMESTEP,int_state%DT,int_state%NPHS              &
                    ,int_state%NUM_WATER,NUM_SOIL_LAYERS,SLDPTH,DZSOIL  &
                    ,DSG2,SGML2,SG2,PDSG1,PSGML1,PSG1,PT                &
                    ,int_state%SM,int_state%CZEN,int_state%CZMEAN       &
                    ,int_state%SIGT4,int_state%RLWIN,int_state%RSWIN    &
                    ,int_state%RADOT                                    &
                    ,int_state%RLWTT,int_state%RSWTT                    &   !! added by wang 2010-10-6
                    ,int_state%PD,int_state%T                           &
                    ,int_state%Q,int_state%CW                           &
                    ,int_state%F_ICE,int_state%F_RAIN,int_state%SR      &
                    ,int_state%Q2,int_state%U,int_state%V               &
                    ,int_state%DUDT,int_state%DVDT                      &
                    ,int_state%THS,int_state%TSKIN,int_state%SST        &
                    ,int_state%PREC,int_state%SNO                       &
                    ,int_state%WATER                                    &
                    ,int_state%P_QV,int_state%P_QC,int_state%P_QR       &
                    ,int_state%P_QI,int_state%P_QS,int_state%P_QG       &
                    ,int_state%F_QV,int_state%F_QC,int_state%F_QR       &
                    ,int_state%F_QI,int_state%F_QS,int_state%F_QG       &
                    ,int_state%FIS,int_state%Z0,int_state%Z0BASE        &
                    ,int_state%USTAR,int_state%PBLH,int_state%LPBL      &
                    ,int_state%XLEN_MIX,int_state%RMOL                  &
                    ,int_state%EXCH_H,int_state%AKHS,int_state%AKMS     &
                    ,int_state%AKHS_OUT,int_state%AKMS_OUT              &
                    ,int_state%THZ0,int_state%QZ0                       &
                    ,int_state%UZ0,int_state%VZ0                        &
                    ,int_state%QSH,int_state%MAVAIL                     &
                    ,int_state%STC,int_state%SMC,int_state%CMC          &
                    ,int_state%SMSTAV,int_state%SMSTOT                  &
                    ,int_state%SSROFF,int_state%BGROFF                  &
                    ,int_state%IVGTYP,int_state%ISLTYP,int_state%VEGFRC &
                    ,int_state%GRNFLX                                   &
                    ,int_state%SFCEXC,int_state%ACSNOW,int_state%ACSNOM &
                    ,int_state%SNOPCX,int_state%SICE                    &
                    ,int_state%TG,int_state%SOILTB                      &
                    ,int_state%ALBASE,int_state%MXSNAL,int_state%ALBEDO &
                    ,int_state%SH2O,int_state%SI,int_state%EPSR         &
                    ,int_state%U10,int_state%V10                        &
                    ,int_state%TH10,int_state%Q10                       &
                    ,int_state%TSHLTR,int_state%QSHLTR,int_state%PSHLTR &
                    ,int_state%PSFC,int_state%T2                        &
                    ,int_state%QSG,int_state%QVG,int_state%QCG          &
                    ,int_state%SOILT1,int_state%TSNAV                   &
                    ,int_state%TWBS,int_state%QWBS                      &
                    ,int_state%SFCSHX,int_state%SFCLHX,int_state%SFCEVP &
                    ,int_state%POTEVP,int_state%POTFLX,int_state%SUBSHX &
                    ,int_state%APHTIM                                   &
                    ,int_state%ARDSW,int_state%ARDLW                    &
                    ,int_state%ASRFC                                    &
                    ,int_state%CROT,int_state%SROT,int_state%MIXHT      &
                    ,int_state%HSTDV,int_state%HCNVX,int_state%HASYW    &
                    ,int_state%HASYS,int_state%HASYSW,int_state%HASYNW  &
                    ,int_state%HLENW,int_state%HLENS,int_state%HLENSW   &
                    ,int_state%HLENNW,int_state%HANGL,int_state%HANIS   &
                    ,int_state%HSLOP,int_state%HZMAX                    &
                    ,int_state%CLEFFAMP,int_state%SIGFAC                &
                    ,int_state%FACTOP,int_state%RLOLEV                  &
                    ,int_state%DPMIN                                    &
                    ,int_state%RSWOUT,int_state%RSWTOA,int_state%RLWTOA &
                    ,int_state%ASWIN,int_state%ASWOUT,int_state%ASWTOA  &
                    ,int_state%ALWIN,int_state%ALWOUT,int_state%ALWTOA  &
                    ,int_state%GWDFLG,LOC_PCPFLG                        &
                    ,int_state%DDATA,int_state%UCMCALL,int_state%IVEGSRC&
                    ,int_state%TURBULENCE,int_state%SFC_LAYER           &
                    ,int_state%LAND_SURFACE                             &
                    ,int_state%MICROPHYSICS                             &
                    ,int_state%GLOBAL                                   &
                    ,IDS,IDE,JDS,JDE,LM                                 &
                    ,IMS,IME,JMS,JME                                    &
                    ,ITS,ITE,JTS,JTE)
!
          td%turbl_tim=td%turbl_tim+(timef()-btim)

!        if (ITS .le. 959 .and. ITE .ge. 959 .and. & 
!            JTS .le. 439 .and. JTE .ge. 439) then
!            write(0,*) 'T,Q,THS out of TURBL: ', int_state%T(959,439,LM), & 
!                                               int_state%Q(959,439,LM), & 
!                                          int_state%THS(959,439)
!        endif
!
!-----------------------------------------------------------------------
!***  Exchange wind tendencies.
!-----------------------------------------------------------------------
!
          btim=timef()
!
          CALL HALO_EXCH(int_state%DUDT,LM,int_state%DVDT,LM,1,1)
!
          td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Now interpolate wind tendencies from H to V points.
!-----------------------------------------------------------------------
!
          btim=timef()
!
          CALL H_TO_V_TEND(int_state%DUDT,int_state%DT,int_state%NPHS,LM &
                          ,int_state%U)
          CALL H_TO_V_TEND(int_state%DVDT,int_state%DT,int_state%NPHS,LM &
                          ,int_state%V)
!
          td%h_to_v_tim=td%h_to_v_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Poles and East-West boundary.
!-----------------------------------------------------------------------
!
          IF(int_state%GLOBAL)THEN
            btim=timef()
!
            CALL SWAPHN(int_state%T,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%T,IMS,IME,JMS,JME,LM                  &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPHN(int_state%Q,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%Q,IMS,IME,JMS,JME,LM                  &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPHN(int_state%CW,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%CW,IMS,IME,JMS,JME,LM                 &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPHN(int_state%Q2,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%Q2,IMS,IME,JMS,JME,LM                 &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPWN(int_state%U,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL SWAPWN(int_state%V,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEWN(int_state%U,int_state%V,IMS,IME,JMS,JME,LM      &
                       ,int_state%INPES,int_state%JNPES)
!
            td%pole_swap_tim=td%pole_swap_tim+(timef()-btim)
          ENDIF
!
!-----------------------------------------------------------------------
!***  Exchange wind components and TKE.
!-----------------------------------------------------------------------
!
          btim=timef()
!
          CALL HALO_EXCH(int_state%U,LM,int_state%V,LM                  &
                        ,2,2)
!
          CALL HALO_EXCH(int_state%UZ0,1,int_state%VZ0,1                &
                        ,int_state%Q2,LM                                &
                        ,1,1)
!
          td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Exchange other variables that are needed for parents' 
!***  interpolations to interior points of moving nests.
!-----------------------------------------------------------------------
!
          CALL HALO_EXCH(int_state%ALBEDO,1                             &
                        ,int_state%EPSR,1                               &
                        ,int_state%QSH,1                                &
                        ,int_state%QWBS,1,1,1)
          CALL HALO_EXCH(int_state%QZ0,1                                &
                        ,int_state%SOILTB,1                             &
                        ,int_state%THS,1                                &
                        ,int_state%THZ0,1,1,1)
          CALL HALO_EXCH(int_state%USTAR,1                              &
                        ,int_state%UZ0,1                                &
                        ,int_state%VZ0,1                                &
                        ,int_state%Z0,1,1,1)
          CALL HALO_EXCH(int_state%TSKIN,1                              &
                        ,int_state%CMC,1,1,1)
          CALL HALO_EXCH(int_state%SMC,NUM_SOIL_LAYERS                  &
                        ,int_state%SH2O,NUM_SOIL_LAYERS                 &
                        ,int_state%STC,NUM_SOIL_LAYERS,1,1)
!
!-----------------------------------------------------------------------
!
        ENDIF turbulence
!
!----------------------------------------------------------------------- 
!***  Empty the accumulators of precipitation and latent heating if is
!***  is time prior to their being updated by convection/microphysics.
!-----------------------------------------------------------------------
!
        IF(MOD(NTIMESTEP,int_state%NPREC)==0)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%ACPREC(I,J)=0.
            int_state%CUPREC(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!
        IF(MOD(NTIMESTEP,int_state%NHEAT)==0)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%AVCNVC(I,J)=0.   !- was a scalar, now 2D for ESMF
            int_state%AVRAIN(I,J)=0.   !- was a scalar, now 2D for ESMF
          ENDDO
          ENDDO
!
          DO L=1,LM
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%TRAIN(I,J,L)=0.
            int_state%TCUCN(I,J,L)=0.
            do KK=1,int_state%d_ss
              int_state%MPRATES(I,J,L,KK)=0.
            enddo
          ENDDO
          ENDDO
          ENDDO
        ENDIF    !-- IF(MOD(NTSD_BUCKET,NHEAT)==0)THEN
!
!-----------------------------------------------------------------------
!***  1 of 3 calls to CLTEND, save Told array before convection & microphysics
!-----------------------------------------------------------------------
!
        cld_tend: IF(CALL_PRECIP)THEN
            ICLTEND=-1
            CALL CLTEND(ICLTEND,int_state%NPRECIP,int_state%T           &
                       ,int_state%Told,int_state%Tadj                   &
                       ,IDS,IDE,JDS,JDE,LM                              &
                       ,IMS,IME,JMS,JME                                 &
                       ,ITS,ITE,JTS,JTE)
         ENDIF cld_tend
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Convection
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
        convection: IF(CALL_PRECIP.AND.int_state%CONVECTION/='none')THEN
!
          btim=timef()
!
!-----------------------------------------------------------------------
          IF(int_state%CONVECTION=='bmj' .OR. &
             int_state%CONVECTION=='sas') THEN
!
            CALL CUCNVC(NTIMESTEP,int_state%DT,int_state%NPRECIP          &
                       ,int_state%NRADS,int_state%NRADL                   &
                       ,int_state%MINUTES_HISTORY                         &
                       ,int_state%ENTRAIN,int_state%NEWALL                &
                       ,int_state%NEWSWAP,int_state%NEWUPUP               &
                       ,int_state%NODEEP                                  &
                       ,int_state%FRES,int_state%FR                       &
                       ,int_state%FSL,int_state%FSS                       &
                       ,int_state%DYH,int_state%RESTART,int_state%HYDRO   &
                       ,int_state%CLDEFI,int_state%NUM_WATER              &
                       ,int_state%F_ICE,int_state%F_RAIN                  &
                       ,int_state%P_QV,int_state%P_QC,int_state%P_QR      &
                       ,int_state%P_QI,int_state%P_QS,int_state%P_QG      &
                       ,int_state%F_QV,int_state%F_QC,int_state%F_QR      &
                       ,int_state%F_QI,int_state%F_QS,int_state%F_QG      &
                       ,DSG2,SGML2,SG2,PDSG1,PSGML1,PSG1                  &
                       ,int_state%dxh                                     &
                       ,int_state%PT,int_state%PD                         &
                       ,int_state%T,int_state%Q                           &
                       ,int_state%CW,int_state%TCUCN,int_state%WATER      &
                       ,int_state%OMGALF                                  &
                       ,int_state%U,int_state%V                           &
                       ,int_state%FIS,int_state%W0AVG                     &
                       ,int_state%PREC,int_state%ACPREC,int_state%CUPREC  &
                       ,int_state%CUPPT,int_state%CPRATE                  &
                       ,int_state%CNVBOT,int_state%CNVTOP                 &
                       ,int_state%SM,int_state%LPBL                       &
                       ,int_state%HTOP,int_state%HTOPD,int_state%HTOPS    &
                       ,int_state%HBOT,int_state%HBOTD,int_state%HBOTS    &
                       ,int_state%AVCNVC,int_state%ACUTIM                 &
                       ,int_state%RSWIN,int_state%RSWOUT                  &
                       ,int_state%CONVECTION,int_state%CU_PHYSICS         &
                       ,int_state%SICE,int_state%QWBS,int_state%TWBS      &
                       ,int_state%PBLH,int_state%DUDT,int_state%DVDT      &
                       ,A2,A3,A4,CAPPA,CP,ELIV,ELWV,EPSQ,G                &
                       ,P608,PQ0,R_D,TIW                                  &
                       ,IDS,IDE,JDS,JDE,LM                                &
                       ,IMS,IME,JMS,JME                                   &
                       ,ITS,ITE,JTS,JTE                                   &
                       ,ITS_B1,ITE_B1,JTS_B1,JTE_B1)
!
          ELSE
!
!           write(0,*)' Invalid selection for convection scheme'
          STOP
!
          ENDIF
!
          td%cucnvc_tim=td%cucnvc_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***    Poles and East-West boundary.
!-----------------------------------------------------------------------
!
          IF(int_state%GLOBAL)THEN
            btim=timef()
!
            CALL SWAPHN(int_state%T,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%T,IMS,IME,JMS,JME,LM                  &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPHN(int_state%Q,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%Q,IMS,IME,JMS,JME,LM                  &
                       ,int_state%INPES,int_state%JNPES)
!
            td%pole_swap_tim=td%pole_swap_tim+(timef()-btim)
          ENDIF
!
!-----------------------------------------------------------------------
!***  Exchange wind tendencies for SAS and bmj schemes.
!-----------------------------------------------------------------------
!
          wind: IF (int_state%CONVECTION=='sas' .or. &
                    int_state%CONVECTION=='bmj') THEN !zj
!
!-----------------------------------------------------------------------
!
            btim=timef()
            CALL HALO_EXCH(int_state%DUDT,LM,int_state%DVDT,LM,1,1)
            td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Now interpolate wind tendencies from H to V points.
!-----------------------------------------------------------------------
!
            btim=timef()
            CALL H_TO_V_TEND(int_state%DUDT,int_state%DT                &
                            ,int_state%NPRECIP,LM                       &
                            ,int_state%U)
            CALL H_TO_V_TEND(int_state%DVDT,int_state%DT                &
                            ,int_state%NPRECIP,LM                       &
                            ,int_state%V)
            td%h_to_v_tim=td%h_to_v_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Poles and East-West boundary.
!-----------------------------------------------------------------------
!
            IF(int_state%GLOBAL)THEN
              btim=timef()
!
              CALL SWAPWN(int_state%U,IMS,IME,JMS,JME,LM                &
                         ,int_state%INPES)
              CALL SWAPWN(int_state%V,IMS,IME,JMS,JME,LM                &
                         ,int_state%INPES)
              CALL POLEWN(int_state%U,int_state%V,IMS,IME,JMS,JME,LM    &
                         ,int_state%INPES,int_state%JNPES)
!
              td%pole_swap_tim=td%pole_swap_tim+(timef()-btim)
            ENDIF
!
!-----------------------------------------------------------------------
!***  Exchange wind components.
!-----------------------------------------------------------------------
!
            btim=timef()
            CALL HALO_EXCH(int_state%U,LM,int_state%V,LM                &
                          ,2,2)
            td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!
          ENDIF wind
!
!-----------------------------------------------------------------------
!
        ENDIF convection
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Microphysics
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
        microphysics: IF(CALL_PRECIP)THEN
!
          btim=timef()
!
          CALL GSMDRIVE(NTIMESTEP,int_state%DT                             &
                       ,NPRECIP,int_state%NUM_WATER                        &
                       ,int_state%DXH(JC),int_state%DYH                    &
                       ,int_state%SM,int_state%FIS                         &
                       ,DSG2,SGML2,PDSG1,PSGML1                            &
                       ,int_state%PT,int_state%PD                          &
                       ,int_state%T,int_state%Q                            &
                       ,int_state%CW,int_state%OMGALF                      &
                       ,int_state%WATER                                    &
                       ,int_state%TRAIN,int_state%SR                       &
                       ,int_state%F_ICE,int_state%F_RAIN,int_state%F_RIMEF &
                       ,int_state%P_QV,int_state%P_QC,int_state%P_QR       &
                       ,int_state%P_QI,int_state%P_QS,int_state%P_QG       &
                       ,int_state%F_QV,int_state%F_QC,int_state%F_QR       &
                       ,int_state%F_QI,int_state%F_QS,int_state%F_QG       &
                       ,int_state%PREC,int_state%ACPREC,int_state%AVRAIN   &
                       ,int_state%MP_RESTART_STATE                         &
                       ,int_state%TBPVS_STATE,int_state%TBPVS0_STATE       &
                       ,int_state%SPECIFIED,int_state%NESTED               &
                       ,int_state%MICROPHYSICS                             &
                       ,int_state%TP1                                      &  !gfs mod-brad
                       ,int_state%QP1                                      &  !gfs mod-brad
                       ,int_state%PSP1                                     &  !gfs mod-brad
                       ,IDS,IDE,JDS,JDE,LM                                 &
                       ,IMS,IME,JMS,JME                                    &
                       ,ITS,ITE,JTS,JTE                                    &
                       ,ITS_B1,ITE_B1,JTS_B1,JTE_B1,int_state%MPRATES      &
                       ,int_state%D_SS)
!
          td%gsmdrive_tim=td%gsmdrive_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  2 of 3 calls to CLTEND, calculate Tadj and replace T with Told
!-----------------------------------------------------------------------
!
          ICLTEND=0
          CALL CLTEND(ICLTEND,int_state%NPRECIP,int_state%T             &
                     ,int_state%Told,int_state%Tadj                     &
                     ,IDS,IDE,JDS,JDE,LM                                &
                     ,IMS,IME,JMS,JME                                   &
                     ,ITS,ITE,JTS,JTE)
!
!-----------------------------------------------------------------------
!***  Precipitation Assimilation
!-----------------------------------------------------------------------
!
          IF (int_state%PCPFLG .and. FILTER_METHOD == 0) THEN
!
            btim=timef()
            CALL CHKSNOW(MYPE                                           &
                        ,int_state%NTSD                                 &
                        ,int_state%DT                                   &
                        ,int_state%NPHS                                 &
                        ,int_state%SR                                   &
                        ,int_state%PPTDAT                               &
                        ,int_state%PCPHR                                &
                        ,IDS,IDE,JDS,JDE,LM                             &
                        ,IMS,IME,JMS,JME                                &
                        ,ITS,ITE,JTS,JTE                                &
                        ,ITS_B1,ITE_B1,JTS_B2,JTE_B2)
!
            CALL ADJPPT(MYPE                                            &
                       ,int_state%NTSD                                  &
                       ,int_state%DT                                    &
                       ,int_state%NPHS                                  &
                       ,int_state%PREC                                  &
                       ,int_state%LSPA                                  &
                       ,int_state%PPTDAT                                &
                       ,int_state%DDATA                                 &
                       ,int_state%PCPHR                                 &
                       ,IDS,IDE,JDS,JDE,LM                              &
                       ,IMS,IME,JMS,JME                                 &
                       ,ITS,ITE,JTS,JTE                                 &
                       ,ITS_B1,ITE_B1,JTS_B2,JTE_B2)
!
            td%adjppt_tim=td%adjppt_tim+(timef()-btim)
!
          ENDIF
!
!-----------------------------------------------------------------------
!***  Poles and East-West boundary.
!-----------------------------------------------------------------------
!
          IF(int_state%GLOBAL)THEN
            btim=timef()
!
!bsf: Apply these after last (3rd) call to CLTEND below
!
!            CALL SWAPHN(int_state%T,IMS,IME,JMS,JME,LM,int_state%INPES)
!            CALL POLEHN(int_state%T,IMS,IME,JMS,JME,LM                  &
!                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPHN(int_state%Q,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%Q,IMS,IME,JMS,JME,LM                  &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPHN(int_state%CW,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%CW,IMS,IME,JMS,JME,LM                 &
                       ,int_state%INPES,int_state%JNPES)
!
            td%pole_swap_tim=td%pole_swap_tim+(timef()-btim)
          ENDIF
!
!-----------------------------------------------------------------------
!***    Exchange Q and CW.
!-----------------------------------------------------------------------
!
          btim=timef()
!
          CALL HALO_EXCH(int_state%Q,LM,int_state%CW,LM                 &
                        ,2,2)
!
          td%exch_tim=td%exch_tim+(timef()-btim)

!
!-----------------------------------------------------------------------
!
        ENDIF microphysics
!
!-----------------------------------------------------------------------
!***  3 of 3 calls to CLTEND, incremental updates of T using Told & Tadj
!-----------------------------------------------------------------------
!
        ICLTEND=1
        CALL CLTEND(ICLTEND,int_state%NPRECIP,int_state%T               &
                   ,int_state%Told,int_state%Tadj                       &
                   ,IDS,IDE,JDS,JDE,LM                                  &
                   ,IMS,IME,JMS,JME                                     &
                   ,ITS,ITE,JTS,JTE)
!
!bsf: Call SWAPHN & POLEHN for temperature here after temperature update
!
        IF(int_state%GLOBAL)THEN
           btim=timef()
!
           CALL SWAPHN(int_state%T,IMS,IME,JMS,JME,LM,int_state%INPES)
           CALL POLEHN(int_state%T,IMS,IME,JMS,JME,LM                  &
                      ,int_state%INPES,int_state%JNPES)
           td%pole_swap_tim=td%pole_swap_tim+(timef()-btim)
        ENDIF
!
!-----------------------------------------------------------------------
!***  Always exchange Temperature array since radiative updates
!***  are done every timestep.
!-----------------------------------------------------------------------
!
        btim=timef()
!
        CALL HALO_EXCH(int_state%T,LM                                   &
                      ,2,2)
!
!-----------------------------------------------------------------------
!***  If advection is on, cloud species are advected.
!-----------------------------------------------------------------------
!
        IF(int_state%SPEC_ADV)THEN
          CALL HALO_EXCH(int_state%WATER,LM,int_state%NUM_WATER,2       &
                       ,2,2)
        ENDIF

!
        td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  NOTE:  The Physics export state is fully updated now
!***         because subroutine PHY_INITIALIZE inserted the
!***         appropriate ESMF Fields into it.  Those Fields
!***         contain pointers to the actual data and those
!***         pointers are never re-directed.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ELSE gfs_phys_test                                                   !<-- Use GFS physics package
!
!-----------------------------------------------------------------------
!
!#######################################################################
!#######################################################################
!############ G F S   P H Y S I C S   D R I V E R ######################
!#######################################################################
!#######################################################################
!
        btim=timef()
!
        ALLOCATE(LONSPERLAR(JTS:JTE))
        ALLOCATE(NLNSP(JTS:JTE))
        ALLOCATE(GLOBAL_LATS_R(JTS:JTE))
        ALLOCATE(CLDCOV_V(LM))
        ALLOCATE(PRSL(LM))
        ALLOCATE(PRSLK(LM))
        ALLOCATE(GU(LM))
        ALLOCATE(GV(LM))
        ALLOCATE(GT(LM))
        ALLOCATE(GR(LM))
        ALLOCATE(VVEL(LM))
        ALLOCATE(F_ICE(LM))
        ALLOCATE(F_RAIN(LM))
        ALLOCATE(R_RIME(LM))
        ALLOCATE(ADT(LM))
        ALLOCATE(ADU(LM))
        ALLOCATE(ADV(LM))
        ALLOCATE(PHIL(LM))
        ALLOCATE(GR3(LM,NTRAC))
        ALLOCATE(ADR(LM,NTRAC))
        ALLOCATE(PRSI(LM+1))
        ALLOCATE(PRSIK(LM+1))
        ALLOCATE(RSGM(LM+1))
        ALLOCATE(PHII(LM+1))
        ALLOCATE(SINLAT_R(JTS:JTE))
        ALLOCATE(COSLAT_R(JTS:JTE))
        ALLOCATE(XLON(ITS:ITE,JTS:JTE))
        ALLOCATE(COSZEN(ITS:ITE,JTS:JTE))
        ALLOCATE(COSZDG(ITS:ITE,JTS:JTE))
        ALLOCATE(SINLAT_V(ITS:ITE,JTS:JTE))
        ALLOCATE(COSLAT_V(ITS:ITE,JTS:JTE))
        ALLOCATE(RANN(ITS:ITE,JTS:JTE))
        ALLOCATE(RANNUM((ITE-ITS+1)*(JTE-JTS+1)))
        ALLOCATE(GR1(1,LM,NTRAC-1))
        ALLOCATE(SWH(LM))
        ALLOCATE(HLW(LM))
        ALLOCATE(DKH(LM))
        ALLOCATE(RNP(LM))
        ALLOCATE(UPD_MF(LM))
        ALLOCATE(DWN_MF(LM))
        ALLOCATE(DET_MF(LM))
        ALLOCATE(DQDT(LM))
        ALLOCATE(DQ3DT(LM,9))
        ALLOCATE(DT3DT(LM,6))
        ALLOCATE(DU3DT(LM,4))
        ALLOCATE(DV3DT(LM,4))
        ALLOCATE(PHY_F3DV(LM,4))
!
        CALL ESMF_ClockGet(clock       =CLOCK_ATM                       &  !<-- The ESMF Clock
                          ,currTime    =CURRTIME                        &  !<-- The current time (ESMF) on the clock
                          ,rc          =RC)
!
        CALL ESMF_TimeGet(time=CURRTIME                                 &  !<-- The cuurent forecast time (ESMF)
                         ,yy  =JDAT(1)                                  &  !<-- The current forecast year (integer)
                         ,mm  =JDAT(2)                                  &  !<-- The current forecast month (integer)
                         ,dd  =JDAT(3)                                  &  !<-- The current forecast day (integer)
                         ,h   =JDAT(5)                                  &  !<-- The current forecast hour (integer)
                         ,m   =JDAT(6)                                  &  !<-- The current forecast minute (integer)
                         ,s   =JDAT(7)                                  &  !<-- The current forecast second (integer)
                         ,rc  =RC)
        JDAT(4)=0
        JDAT(8)=0
!
        DO J=JTS,JTE
          GLOBAL_LATS_R(J) = J-JTS+1
          LONSPERLAR(J)    = ITE-ITS+1
          SINLAT_R(J)      = SIN(int_state%GLAT( (ITS+ITE)/2 ,J))
          COSLAT_R(J)      = SQRT( 1.d0 - SINLAT_R(J)*SINLAT_R(J) )
          DO I=ITS,ITE
            XLON(I,J)        = int_state%GLON(I,J)
            IF(int_state%GLON(I,J)<0) &
             XLON(I,J)        = 2.0d0*3.14159d0+XLON(I,J)
            COSZEN(I,J)      = int_state%CZEN(I,J)
            COSZDG(I,J)      = int_state%CZMEAN(I,J)
          ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  GFS Radiation
!-----------------------------------------------------------------------
!
        CALL_GFS_PHY = MOD(NTIMESTEP,int_state%NPHS)==0

        FHSWR        = FLOAT(int_state%NRADS)*int_state%DT/3600.   ! [h]
        LSCCA        = MOD(NTIMESTEP+1,int_state%NRADS)==0         ! logical true during a step for which convective clouds
                                                                   ! are calculated from convective precipitation rates
        LSSWR        = MOD(NTIMESTEP,int_state%NRADS)==0
        LSLWR        = MOD(NTIMESTEP,int_state%NRADL)==0
!
!-----------------------------------------------------------------------
        lw_or_sw: IF (LSSWR .OR. LSLWR ) THEN
!-----------------------------------------------------------------------
!
          DO L=1,LM+1
            KFLIP=LM-L+2
            RSGM(KFLIP)=int_state%SGM(L)
          ENDDO
! 
          ICWP=0                  ! control flag for cloud generation schemes
          IF (NTCW > 0) ICWP = 1  ! 0: use diagnostic cloud scheme
                                  ! 1: use prognostic cloud scheme (default)
!
! ----
!rv - find IOVR_SW,IOVR_LW,isubc_sw, isubc_lw
          CALL RADINIT ( RSGM, LM, IFLIP, IDAT, JDAT, ICTM, ISOL, ICO2, &
                         IAER, IALB, IEMS, ICWP, NUM_P3D, 0, 0,         &
                         0, 0, MYPE, RADDT, FDAER )
! ----

          IF (NTOZ .LE. 0) THEN                ! Climatological Ozone
!
            IDAY   = JDAT(3)
            IMON   = JDAT(2)
            MIDMON = DAYS(IMON)/2 + 1
            CHANGE = FIRST .OR. ( (IDAY .EQ. MIDMON) .AND. (JDAT(5).EQ.0) )
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
!
            FACOZ = REAL (ID-MIDM) / REAL (MIDP-MIDM)
!
          ELSE
!
            K1OZ = 0
            K2OZ = 0
            FACOZ = 1.0D0
!
          ENDIF
!
          FLGMIN_L(1)     = 0.2D0      ! --- for ferrier (for now, any number)

          DO J=JTS,JTE
            DO I=ITS,ITE
              SINLAT_V(I,J) = SINLAT_R(J)
              COSLAT_V(I,J) = COSLAT_R(J)
            ENDDO
          ENDDO
          DO J=JTS,JTE
            NLNSP(J) = LONR
          ENDDO
         

! ----
          CALL ASTRONOMY                                                &
!  ---  inputs:
             ( SINLAT_V, COSLAT_V, XLON, FHSWR, JDAT,                   &
               LONR, LATS_NODE_R, NLNSP, LSSWR, MYPE,                   &
!  ---  outputs:
               int_state%SOLCON, int_state%SLAG, int_state%SDEC,        &
               int_state%CDEC, COSZEN, COSZDG )
!
!-----------------------------------------------------------------------
!
        ENDIF  lw_or_sw
!
!-----------------------------------------------------------------------
!
!---
        IF (FIRST) THEN
!
          SEED0 = JDAT(4) + JDAT(3) + JDAT(2) + JDAT(1)
          CALL RANDOM_SETSEED(SEED0)
          CALL RANDOM_NUMBER(WRK)
          SEED0 = SEED0 + NINT(WRK(1)*1000.0)
          FIRST = .FALSE.
!
        ENDIF
!---
        FHOUR=NTIMESTEP*int_state%DT/3600.d0
        ISEED = MOD(100.0*SQRT(FHOUR*3600),1.0d9) + 1 + SEED0
        CALL RANDOM_SETSEED(ISEED)
        CALL RANDOM_NUMBER(RANNUM)
        N=0
!
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          RANN(I,J) = RANNUM(N)
        ENDDO
        ENDDO
!---
        DTF=int_state%NPHS*int_state%DT
        DTP=int_state%NPHS*int_state%DT
!---
        SOLHR=MOD(FHOUR+START_HOUR,24.d0)
!---
!...  set switch for saving convective clouds
        IF(LSCCA.AND.LSSWR) THEN
          CLSTP=1100+MIN(FHSWR,FHOUR,99.d0)  !initialize,accumulate,convert
        ELSEIF(LSCCA) THEN
          CLSTP=0100+MIN(FHSWR,FHOUR,99.d0)  !accumulate,convert
        ELSEIF(LSSWR) THEN
          CLSTP=1100                         !initialize,accumulate
        ELSE
          CLSTP=0100                         !accumulate
        ENDIF
!---
!---- OZONE ------------------------------------------------------------
!
        IF(.NOT.ALLOCATED(OZPLOUT_V)) &
                           ALLOCATE (OZPLOUT_V(LEVOZP,        PL_COEFF))
        IF(.NOT.ALLOCATED(OZPLOUT  )) &
                           ALLOCATE (OZPLOUT  (LEVOZP,JTS:JTE,PL_COEFF))
!
        IDATE(1)=JDAT(5)
        IDATE(2)=JDAT(2)
        IDATE(3)=JDAT(3)
        IDATE(4)=JDAT(1)
!
        IF (NTOZ .GT. 0) THEN
          CALL OZINTERPOL(MYPE,LATS_NODE_R,LATS_NODE_R,IDATE,FHOUR,     &
                          int_state%JINDX1,int_state%JINDX2,            &
                          int_state%OZPLIN,OZPLOUT,int_state%DDY)
        ENDIF
!
!---- OZONE ------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Set diagnostics to 0.
!-----------------------------------------------------------------------
!
        DT3DT=0.0d0
        DU3DT=0.0d0
        DV3DT=0.0d0
        DQ3DT=0.0d0
!
        CV (1) = 0.d0       !!!!! not in use if ntcw-1 > 0
        CVB(1) = 0.d0       !!!!! not in use if ntcw-1 > 0
        CVT(1) = 0.d0       !!!!! not in use if ntcw-1 > 0
!
!-----------------------------------------------------------------------
!***  Empty the radiation flux and precipitation arrays if it is time.
!-----------------------------------------------------------------------
!
        IF(MOD(NTIMESTEP,int_state%NRDLW)==0)THEN
!
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%ALWIN(I,J) =0.
            int_state%ALWOUT(I,J)=0.
            int_state%ALWTOA(I,J)=0.
            int_state%ARDLW (I,J)=0.   !<-- An artificial 2-D array (ESMF
                                       !<-- cannot have evolving scalar Attributes)
          ENDDO
          ENDDO
!
        ENDIF
!
        IF(MOD(NTIMESTEP,int_state%NRDSW)==0)THEN
!
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%ASWIN(I,J)=0.
            int_state%ASWOUT(I,J)=0.
            int_state%ASWTOA(I,J)=0.
            int_state%ARDSW (I,J)=0.   !<-- An artificial 2-D array (ESMF
                                       !<-- cannot have evolving scalar Attributes)
          ENDDO
          ENDDO
!
        ENDIF
!
        IF(MOD(NTIMESTEP,int_state%NSRFC)==0)THEN
!
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%ACSNOW(I,J)=0.
            int_state%POTEVP(I,J)=0.
            int_state%SFCEVP(I,J)=0.
            int_state%SFCLHX(I,J)=0.
            int_state%SFCSHX(I,J)=0.
            int_state%SUBSHX(I,J)=0.
            int_state%BGROFF(I,J)=0.
            int_state%SSROFF(I,J)=0.
            int_state%ASRFC (I,J)=0.   !<-- An artificial 2-D array (ESMF
                                       !<-- cannot have evolving scalar Attributes)
          ENDDO
          ENDDO
!
        ENDIF
!
        IF(MOD(NTIMESTEP,int_state%NPREC)==0)THEN
          DO J=JTS,JTE
          DO I=ITS,ITE
            int_state%ACPREC(I,J)=0.
            int_state%CUPREC(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
        gfs_physics: IF(CALL_GFS_PHY)THEN
!-----------------------------------------------------------------------
!
          DTLW     = FLOAT(int_state%NRADL)*int_state%DT   ! [s]
          DTSW     = FLOAT(int_state%NRADS)*int_state%DT   ! [s]
          RADDT    = MIN(DTSW,DTLW)
          DTLWI    = 1./DTLW
          DTSWI    = 1./DTSW
          MINDT    = 1./MIN(DTLW,DTSW)
          DTPHS    = int_state%NPHS*int_state%DT
          DTPHSI   = 1./DTPHS
          XLVRW    = XLV*RHOWATER
          XLVRWI   = 1./XLVRW
          RoCP     = R/CP
          LSSAV_CC = LSSAV
          IDE_GR   = IDE-1
          IF(int_state%GLOBAL) IDE_GR = IDE-3
!
!-----------------------------------------------------------------------
! ***  MAIN GFS-PHYS DOMAIN LOOP
!-----------------------------------------------------------------------
!
          j_loop: DO J=JTS,JTE
!
            i_loop: DO I=ITS,ITE
!
              int_state%ACUTIM(I,J) = int_state%ACUTIM(I,J) + 1.     ! advance counters
              int_state%APHTIM(I,J) = int_state%APHTIM(I,J) + 1.
              int_state%ARDLW(I,J)  = int_state%ARDLW(I,J)  + 1.
              int_state%ARDSW(I,J)  = int_state%ARDSW(I,J)  + 1.
              int_state%ASRFC(I,J)  = int_state%ASRFC(I,J)  + 1.
              int_state%AVRAIN(I,J) = int_state%AVRAIN(I,J) + 1.
              int_state%AVCNVC(I,J) = int_state%AVCNVC(I,J) + 1.
!
              T1(1)           = 0.0D0        ! initialize all local variables
              Q1(1)           = 0.0D0        ! used in gfs_physics
              U1(1)           = 0.0D0
              V1(1)           = 0.0D0
              QSS(1)          = 0.0D0
              FSCAV(1)        = 0.0D0
              CDQ(1)          = 0.0D0
              HFLX(1)         = 0.0D0
              EVAP(1)         = 0.0D0
              DTSFC(1)        = 0.0D0
              DQSFC(1)        = 0.0D0
              DUSFC(1)        = 0.0D0
              DVSFC(1)        = 0.0D0
              PSMEAN(1)       = 0.0D0
              EPI(1)          = 0.0D0
              EVBSA(1)        = 0.0D0
              EVCWA(1)        = 0.0D0
              TRANSA(1)       = 0.0D0
              SBSNOA(1)       = 0.0D0
              SOILM(1)        = 0.0D0
              SNOWCA(1)       = 0.0D0
              DLWSFC_CC(1)    = 0.0D0
              ULWSFC_CC(1)    = 0.0D0
              DTSFC_CC(1)     = 0.0D0
              SWSFC_CC(1)     = 0.0D0
              DUSFC_CC(1)     = 0.0D0
              DVSFC_CC(1)     = 0.0D0
              DQSFC_CC(1)     = 0.0D0
              PRECR_CC(1)     = 0.0D0
              XT(1)           = 0.0D0
              XS(1)           = 0.0D0
              XU(1)           = 0.0D0
              XV(1)           = 0.0D0
              XZ(1)           = 0.0D0
              ZM(1)           = 0.0D0
              XTTS(1)         = 0.0D0
              XZTS(1)         = 0.0D0
              D_CONV(1)       = 0.0D0
              IFD(1)          = 0.0D0
              DT_COOL(1)      = 0.0D0
              QRAIN(1)        = 0.0D0
              XMU_CC(1)       = 0.0D0
              DLW_CC(1)       = 0.0D0
              DSW_CC(1)       = 0.0D0
              SNW_CC(1)       = 0.0D0
              LPREC_CC(1)     = 0.0D0
              TREF(1)         = 0.0D0
              Z_C(1)          = 0.0D0
              C_0(1)          = 0.0D0
              C_D(1)          = 0.0D0
              W_0(1)          = 0.0D0
              W_D(1)          = 0.0D0
              RQTK(1)         = 0.0D0
              SNOHFA(1)       = 0.0D0
              SMCWLT2(1)      = 0.0D0
              SMCREF2(1)      = 0.0D0
              WET1(1)         = 0.0D0
              GSOIL(1)        = 0.0D0
              GTMP2M(1)       = 0.0D0
              GUSTAR(1)       = 0.0D0
              GPBLH(1)        = 0.0D0
              GU10M(1)        = 0.0D0
              GV10M(1)        = 0.0D0
              GZORL(1)        = 0.0D0
              GORO(1)         = 0.0D0
              SPFHMIN(1)      = 0.0D0
              SPFHMAX(1)      = 0.0D0
              CLDWRK(1)       = 0.0D0
              ZLVL(1)         = 0.0D0
              PHII            = 0.0D0
              PHIL            = 0.0D0
              CHH(1)          = 0.0D0
              HPBL(1)         = 0.0D0
              PSURF(1)        = 100000.0D0
              T2M(1)          = 273.0D0
              Q2M(1)          = 0.0D0
              U10M(1)         = 0.0D0
              V10M(1)         = 0.0D0
              ADR             = 0.0D0
              ADT             = 0.0D0
              ADU             = 0.0D0
              ADV             = 0.0D0
              SUNTIM          = 0.0D0
              ICSDSW(1)       = 0
              ICSDLW(1)       = 0

         IF(int_state%TSKIN(I,J) .LT. 50. ) THEN
             TSEA(1)         = int_state%SST(I,J)
             TISFC(1)        = int_state%SST(I,J)
         ELSE
             TSEA(1)         = int_state%TSKIN(I,J)
             TISFC(1)        = int_state%TSKIN(I,J)
         ENDIF

         IF(int_state%SICE(I,J) > 0.5 ) THEN                                ! slmsk - ocean  - 0
             SLMSK(1)        = 2.0D0                                        !         land   - 1
         ELSE                                                               !         seaice - 2
             SLMSK(1)        = 1.0D0-int_state%SM(I,J)                      !
         ENDIF

         DO L=1,LM
            KFLIP=LM+1-L
!            CLDCOV_V(KFLIP) = 0.0D0                      ! GRRAD now returns instant cloud cover (Sarah Lu)
             F_ICE(KFLIP)    = int_state%F_ICE(I,J,L)                       ! for ferrier phy, do init first
             F_RAIN(KFLIP)   = int_state%F_RAIN(I,J,L)
             R_RIME(KFLIP)   = int_state%F_RIMEF(I,J,L)
         ENDDO

             XLAT(1)         = int_state%GLAT(I,J)
             ZORL(1)         = int_state%ZORFCS(I,J)
             SNCOVR(1)       = int_state%SNO(I,J)/(int_state%SNO(I,J)+70.)  ! FORMULATION OF MARSHALL ET AL. 1994
                                                                            ! change this later only initially, add new int_state
             SNWDPH(1)       = int_state%SI(I,J)                            ! snwdph[mm]
             WEASD(1)        = int_state%SNO(I,J)                           ! snow water eq.[mm]
             SNOALB(1)       = int_state%MXSNAL(I,J)
             ALVSF(1)        = int_state%ALBFC1(I,J,1)                      ! VIS, direct
             ALVWF(1)        = int_state%ALBFC1(I,J,2)                      ! VIS, diffuse
             ALNSF(1)        = int_state%ALBFC1(I,J,3)                      ! NIR, direct
             ALNWF(1)        = int_state%ALBFC1(I,J,4)                      ! NIR, diffuse
             FACSF(1)        = int_state%ALFFC1(I,J,1)                      ! direct
             FACWF(1)        = int_state%ALFFC1(I,J,2)                      ! diffuse
!
             PRSI (LM+1)     = int_state%PT                                 ! [ Pa]
             PRSIK(LM+1)     = (PRSI(LM+1)*0.00001d0)**RoCP
         DO L=1,LM
            KFLIP=LM+1-L
             PRSI (KFLIP)    = PRSI(KFLIP+1) + &
                                (DSG2(L)*int_state%PD(I,J)+PDSG1(L))         ! (pressure on interface) [ Pa]
             PRSIK(KFLIP)    = (PRSI(KFLIP)*0.00001d0)**RoCP

             PRSL (KFLIP)    = (PRSI(KFLIP)+PRSI(KFLIP+1))*0.5d0             ! (pressure on mid-layer) [kPa]
             PRSLK(KFLIP)    = (PRSL(KFLIP)*0.00001d0)**RoCP
!
             RTvR = 1. / ( R * (int_state%Q(I,J,L)*0.608+1.-int_state%CW(I,J,L) ) * int_state%T(I,J,L) )
             VVEL(KFLIP)     = int_state%OMGALF(I,J,L) * PRSL(KFLIP) * RTvR
!
             GU(KFLIP)       = (int_state%U(I,J  ,L) + int_state%U(I-1,J  ,L) +                    &
                                int_state%U(I,J-1,L) + int_state%U(I-1,J-1,L))*0.25d0
             GV(KFLIP)       = (int_state%V(I,J  ,L) + int_state%V(I-1,J  ,L) +                    &
                                int_state%V(I,J-1,L) + int_state%V(I-1,J-1,L))*0.25d0
             GT(KFLIP)       = int_state%T(I,J,L)
             GR(KFLIP)       = int_state%Q(I,J,L)
             GR3(KFLIP,1)    = int_state%Q(I,J,L)
           IF (NTIMESTEP == 0 ) THEN
             GR3(KFLIP,2)    = 0.0d0
             GR3(KFLIP,3)    = 0.0d0
           ELSE
             GR3(KFLIP,2)    = int_state%O3(I,J,L)
             GR3(KFLIP,3)    = int_state%CW(I,J,L)
           ENDIF
             GR1(1,KFLIP,1)  = GR3(KFLIP,2)
             GR1(1,KFLIP,2)  = int_state%CW(I,J,L)
         ENDDO
!---
             DLWSFC(1)       = int_state%ALWIN(I,J)
             ULWSFC(1)       = int_state%ALWOUT(I,J)
             DLWSFCI(1)      = int_state%RLWIN(I,J)
             ULWSFCI(1)      = int_state%RADOT(I,J)
             DSWSFCI(1)      = int_state%RSWIN(I,J)
             USWSFCI(1)      = int_state%RSWOUT(I,J)
!---
             GFLUX(1)        = 0.0D0
             DQSFCI(1)       = 0.0D0
             DTSFCI(1)       = 0.0D0
             GFLUXI(1)       = 0.0D0
             EP(1)           = int_state%POTEVP(I,J)*XLVRW
!---
             XSIHFCS(1)      = int_state%SIHFCS(I,J)
             XSICFCS(1)      = int_state%SICFCS(I,J)
             XSLPFCS(1)      = int_state%SLPFCS(I,J)
             XTG3FCS(1)      = int_state%TG3FCS(I,J)
             XVEGFCS(1)      = int_state%VEGFCS(I,J)
             XVETFCS(1)      = int_state%VETFCS(I,J)
             XSOTFCS(1)      = int_state%SOTFCS(I,J)
!---
             FLUXR_V         = 0.0D0
             IF(.NOT.LSLWR) FLUXR_V(1) = int_state%RLWTOA(I,J)*DTLW
             IF(.NOT.LSSWR) FLUXR_V(2) = int_state%RSWTOA(I,J)*DTSW
!---
             HPRIME (1)      = int_state%HSTDV(I,J)
             HPRIME (2)      = int_state%HCNVX(I,J)
             HPRIME (3)      = int_state%HASYW(I,J)
             HPRIME (4)      = int_state%HASYS(I,J)
             HPRIME (5)      = int_state%HASYSW(I,J)
             HPRIME (6)      = int_state%HASYNW(I,J)
             HPRIME (7)      = int_state%HLENW(I,J)
             HPRIME (8)      = int_state%HLENS(I,J)
             HPRIME (9)      = int_state%HLENSW(I,J)
             HPRIME(10)      = int_state%HLENNW(I,J)
             HPRIME(11)      = int_state%HANGL(I,J)*180.D0/3.14159D0
             HPRIME(12)      = int_state%HANIS(I,J)
             HPRIME(13)      = int_state%HSLOP(I,J)
             HPRIME(14)      = int_state%HZMAX(I,J)
!---
             RUNOFF(1)       = int_state%BGROFF(I,J)*0.001D0
             SRUNOFF(1)      = int_state%SSROFF(I,J)*0.001D0
!---
           DO L=1,LM
            KFLIP=LM+1-L
             DKH(L)          = 0.0D0
             RNP(L)          = 0.0D0
             SWH(KFLIP)      = int_state%RSWTT(I,J,L)
             HLW(KFLIP)      = int_state%RLWTT(I,J,L)
           ENDDO
           DO N=1,3                                    ! for Zhao =3, Ferr=1
             PHY_F2DV(N)     = int_state%PHY_F2DV (I,J,N)
           ENDDO
           DO N=1,4                                    ! for Zhao =4, Ferr=3
           DO L=1,LM
            PHY_F3DV(L,N)    = int_state%PHY_F3DV (I,J,L,N)
           ENDDO
           ENDDO
!
!-----------------------------------------------------------------------
          CALL GRRAD                                                 &
!-----------------------------------------------------------------------
!  ---  inputs:
          (PRSI,PRSL,PRSLK,GT,GR,GR1,VVEL,SLMSK,                     &
           XLON(I,J),XLAT,TSEA,SNWDPH,SNCOVR,SNOALB,ZORL,HPRIME(1),  &
           ALVSF,ALNSF,ALVWF,ALNWF,FACSF,FACWF,XSICFCS,TISFC,        &
           int_state%SOLCON,COSZEN(I,J),COSZDG(I,J),K1OZ,K2OZ,FACOZ, &
           CV,CVT,CVB,IOVR_SW,IOVR_LW,F_ICE,F_RAIN,R_RIME,FLGMIN_L,  &
           ICSDSW,ICSDLW,NUM_P3D,NTCW-1,NCLD,NTOZ-1,NTRAC-1,NFXR,    &
           DTLW,DTSW,LSSWR,LSLWR,LSSAV,SASHAL,NORAD_PRECIP,          &
           CRICK_PROOF,CCNORM,                                       &
           1,1,LM,IFLIP,MYPE,LPRNT,1,NTIMESTEP,                      &
!  ---  outputs:
           SWH,TOPFSW,SFCFSW,SFALB,                                  &
           HLW,TOPFLW,SFCFLW,TSFLW,SEMIS,CLDCOV_V,                   &
!  ---  input/output:
           FLUXR_V                                                   &
          )
!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!  GBPHYS   !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!---
        IF (LSSWR .OR. LSLWR ) THEN
          SFCDLW(1)             = SFCFLW(1)%DNFXC
          SFCDSW(1)             = SFCFSW(1)%DNFXC
          SFCNSW(1)             = SFCFSW(1)%DNFXC - SFCFSW(1)%UPFXC
          int_state%SFCDLW(I,J) = SFCDLW(1)
          int_state%SFCDSW(I,J) = SFCDSW(1)
          int_state%SFCNSW(I,J) = SFCNSW(1)
          int_state%SFALB(I,J)  = SFALB(1)
          int_state%TSFLW(I,J)  = TSFLW(1)
          int_state%SEMIS(I,J)  = SEMIS(1)
        ELSE
          SFCDLW(1)   = int_state%SFCDLW(I,J)
          SFCDSW(1)   = int_state%SFCDSW(I,J)
          SFCNSW(1)   = int_state%SFCNSW(I,J)
          SFALB(1)    = int_state%SFALB(I,J)
          TSFLW(1)    = int_state%TSFLW(I,J)
          SEMIS(1)    = int_state%SEMIS(I,J)
        ENDIF
!---
          DPSHC(1)    = 0.3 * PRSI(1)
          GQ(1)       = PRSI(1)
!---
          RANNUM_V(1) = RANN(I,J)
!---
          RCS2_V(1)   = 1.0d0/(0.0001d0+COS(XLAT(1))*COS(XLAT(1))) ! fixed XLAT=+/-90
!---
          TPRCP(1)    = int_state%PREC(I,J)
          CNVPRCP(1)  = int_state%CUPREC(I,J)
          TOTPRCP(1)  = int_state%ACPREC(I,J)

         DO L=1,NUM_SOIL_LAYERS
          SMC_V(L)    = int_state%SMC(I,J,L)
          STC_V(L)    = int_state%STC(I,J,L)
          SLC_V(L)    = int_state%SH2O(I,J,L)
         ENDDO

         SHDMIN(1)   = int_state%SHDMIN(I,J)
         SHDMAX(1)   = int_state%SHDMAX(I,J)

         UUSTAR(1)   = int_state%USTAR(I,J)
         CANOPY(1)   = int_state%CMC(I,J)*1000.
         FLGMIN      = 0.2d0 !!! put this in ferrier init
         CRTRH       = 0.85d0
         FLIPV       = .FALSE.
         NCW(1)      = 50
         NCW(2)      = 150
         OLD_MONIN   = .FALSE.
         CNVGWD      = .FALSE.
         NEWSAS      = .FALSE.
         CCWF        = 0.5d0  ! only for RAS scheme
!
! ---- ESTIMATE T850 FOR RAIN-SNOW DECISION ----------------------------
!
          T850 = GT(1)
      DO L = 1, LM - 1
        IF(PRSL(L) .GT. 85000.d0 .AND. PRSL(L+1) .LE. 85000.d0) THEN
          T850 = GT(L) - (PRSL(L)-85000.d0) / (PRSL(L)-PRSL(L+1)) * (GT(L)-GT(L+1))
        ENDIF
      ENDDO
!
      SRFLAG(1) = 0.0d0
      IF(T850 .LE. 273.16d0) SRFLAG(1) = 1.0d0
!
!---- OZONE ------------------------------------------------------------
      IF (NTOZ .GT. 0) THEN
        DO N=1,PL_COEFF
          DO L=1,LEVOZP
              OZPLOUT_V(L,N) = OZPLOUT(L,j,N)
          ENDDO
        ENDDO
      ELSE
              OZPLOUT_V      = 0.0d0
      ENDIF
!-----------------------------------------------------------------------
      CALL GBPHYS(1, 1, LM, NUM_SOIL_LAYERS, LSM, NTRAC, NCLD, NTOZ, NTCW,  &
           NMTVR, 1, LEVOZP, IDE_GR, LATR, 62, NUM_P3D, NUM_P2D,            &
           NTIMESTEP, J-JTS+1, MYPE, PL_COEFF, LONR, NCW, FLGMIN, CRTRH,    &
             CDMBGWD, &
           CCWF, DLQF, CTEI_RM, CLSTP, DTP, DTF, FHOUR, SOLHR,              &
           int_state%SLAG, int_state%SDEC, int_state%CDEC, SINLAT_R(J),     &
           COSLAT_R(J), GQ, GU, GV,                                         &
           GT, GR3, VVEL, PRSI, PRSL, PRSLK, PRSIK, PHII, PHIL,             &
           RANN, OZPLOUT_V, PL_PRES, DPSHC, HPRIME, XLON(I,J), XLAT,        &
           XSLPFCS, SHDMIN, SHDMAX, SNOALB, XTG3FCS, SLMSK, XVEGFCS,        &
           XVETFCS, XSOTFCS, UUSTAR, ORO, oro, COSZEN(I,J), SFCDSW, SFCNSW, &
           SFCDLW, TSFLW, SEMIS, SFALB, SWH, HLW, RAS, PRE_RAD,             &
           LDIAG3D, LGGFS3D, LGOCART, LSSAV, LSSAV_CC,                      &
           BKGD_VDIF_M, BKGD_VDIF_H, BKGD_VDIF_S, PSAUTCO, PRAUTCO, EVPCO,  &
           WMINCO,                                                          &
           FLIPV, OLD_MONIN, CNVGWD, SHAL_CNV, SASHAL, NEWSAS, CAL_PRE,     &
           MOM4ICE, MSTRAT, TRANS_TRAC, NST_FCST, MOIST_ADJ, FSCAV,         &
           THERMODYN_ID, SFCPRESS_ID, GEN_COORD_HYBRID,                     &
           XSIHFCS, XSICFCS, TISFC, TSEA, TPRCP, CV, CVB, CVT,              &
           SRFLAG, SNWDPH, WEASD, SNCOVR, ZORL, CANOPY,                     &
           FFMM, FFHH, F10M, SRUNOFF, EVBSA, EVCWA, SNOHFA,                 &
           TRANSA, SBSNOA, SNOWCA, SOILM, int_state%TMPMIN(I,J),            &
           int_state%TMPMAX(I,J),                                           &
           DUSFC, DVSFC, DTSFC, DQSFC, TOTPRCP, GFLUX,                      &
           DLWSFC, ULWSFC, SUNTIM, RUNOFF, EP, CLDWRK,                      &
           int_state%DUGWD(I,J), int_state%DVGWD(I,J), PSMEAN, CNVPRCP,     &
           SPFHMIN, SPFHMAX, RAIN, RAINC,                                   &
           DT3DT, DQ3DT, DU3DT, DV3DT, DQDT, ACV, ACVB, ACVT,               &
           SLC_V, SMC_V, STC_V, UPD_MF, DWN_MF, DET_MF, DKH, RNP, PHY_F3DV, &
             PHY_F2DV,                                                      &
           DLWSFC_CC, ULWSFC_CC, DTSFC_CC, SWSFC_CC,                        &
           DUSFC_CC, DVSFC_CC, DQSFC_CC, PRECR_CC,                          &
           XT, XS, XU, XV, XZ, ZM, XTTS, XZTS, D_CONV, IFD, DT_COOL, QRAIN, &
           ADT, ADR, ADU, ADV, T2M, Q2M, U10M, V10M,                        &
           ZLVL, PSURF, HPBL, PWAT, T1, Q1, U1, V1,                         &
           CHH, CMM, DLWSFCI, ULWSFCI, DSWSFCI, USWSFCI,                    &
           DTSFCI, DQSFCI, GFLUXI, EPI, SMCWLT2, SMCREF2, WET1,             &
           GSOIL, GTMP2M, GUSTAR, GPBLH, GU10M, GV10M, GZORL, GORO,         &
           XMU_CC, DLW_CC, DSW_CC, SNW_CC, LPREC_CC,                        &
           TREF, Z_C, C_0, C_D, W_0, W_D, RQTK, HLWD, LSIDEA)
!-----------------------------------------------------------------------
! ***     UPDATE AFTER PHYSICS
!-----------------------------------------------------------------------
             int_state%SIHFCS(I,J)        = XSIHFCS(1)
             int_state%SICFCS(I,J)        = XSICFCS(1)
             int_state%ZORFCS(I,J)        = ZORL(1)

             int_state%CZEN(I,J)          = COSZEN(I,J)
             int_state%CZMEAN(I,J)        = COSZDG(I,J)

             int_state%SI(I,J)            = SNWDPH(1)
             int_state%SNO(I,J)           = WEASD(1)
             int_state%MXSNAL(I,J)        = SNOALB(1)

         DO L=1,NUM_SOIL_LAYERS
             int_state%SMC(I,J,L)         = SMC_V(L)
             int_state%STC(I,J,L)         = STC_V(L)
             int_state%SH2O(I,J,L)        = SLC_V(L)
         ENDDO

             int_state%CMC(I,J)           = CANOPY(1)*0.001
             int_state%USTAR(I,J)         = UUSTAR(1)
             int_state%SMSTOT(I,J)        = SOILM(1)*1000.

         DO L=1,LM
            KFLIP=LM+1-L
             int_state%RSWTT(I,J,L)       = SWH(KFLIP)
             int_state%RLWTT(I,J,L)       = HLW(KFLIP)
         ENDDO

         DO N=1,3                                    ! for Zhao =3, Ferr=1
             int_state%PHY_F2DV (I,J,N)   = PHY_F2DV(N)
         ENDDO

         DO N=1,4                                    ! for Zhao =4, Ferr=3
         DO L=1,LM
             int_state%PHY_F3DV (I,J,L,N) = PHY_F3DV(L,N)
         ENDDO
         ENDDO

             int_state%ALWIN(I,J)         = int_state%ALWIN(I,J)  + int_state%RLWIN(I,J)
             int_state%ALWOUT(I,J)        = int_state%ALWOUT(I,J) - int_state%RADOT(I,J)
             int_state%ASWIN(I,J)         = int_state%ASWIN(I,J)  + int_state%RSWIN(I,J)
             int_state%ASWOUT(I,J)        = int_state%ASWOUT(I,J) - int_state%RSWOUT(I,J)
             int_state%RLWIN(I,J)         = DLWSFCI(1)
             int_state%RADOT(I,J)         = ULWSFCI(1)
             int_state%RSWIN(I,J)         = DSWSFCI(1)
             int_state%RSWOUT(I,J)        = USWSFCI(1)
             int_state%RSWINC(I,J)        = int_state%RSWIN(I,J)/(1.-int_state%ALBEDO(I,J))

             int_state%RLWTOA(I,J)        = FLUXR_V(1)*DTLWI
             int_state%RSWTOA(I,J)        = FLUXR_V(2)*DTSWI
             int_state%ALWTOA(I,J)        = int_state%ALWTOA(I,J) + FLUXR_V(1)*DTLWI
             int_state%ASWTOA(I,J)        = int_state%ASWTOA(I,J) + FLUXR_V(2)*DTSWI

             int_state%TWBS(I,J)          = -DQSFCI(1)
             int_state%QWBS(I,J)          = -DTSFCI(1)
             int_state%SFCSHX(I,J)        = int_state%SFCSHX(I,J) + int_state%QWBS(I,J)
             int_state%SFCLHX(I,J)        = int_state%SFCLHX(I,J) + int_state%TWBS(I,J)
             int_state%SUBSHX(I,J)        = int_state%SUBSHX(I,J) + GFLUXI(1)
             int_state%GRNFLX(I,J)        = GFLUXI(1)
             int_state%POTEVP(I,J)        = EP(1)*XLVRWI
             int_state%POTFLX(I,J)        = -EP(1)*DTPHSI
             int_state%SFCEVP(I,J)        = int_state%SFCEVP(I,J) + DQSFCI(1)*DTPHS*XLVRWI

             int_state%SFCEXC(I,J)        = CDQ(1)                                       !need CDQ  from GFS
             int_state%PBLH(I,J)          = HPBL(1)
             int_state%PSFC(I,J)          = PSURF(1)
             int_state%PREC(I,J)          = TPRCP(1)
             int_state%CUPPT(I,J)         = CNVPRCP(1)-int_state%CUPREC(I,J)
             int_state%CPRATE(I,J)        = CNVPRCP(1)-int_state%CUPREC(I,J)
             int_state%CUPREC(I,J)        = CNVPRCP(1)
             int_state%ACPREC(I,J)        = TOTPRCP(1)

             int_state%BGROFF(I,J)        = RUNOFF(1)*1000.
             int_state%SSROFF(I,J)        = SRUNOFF(1)*1000.

             int_state%TSKIN(I,J)         = TISFC(1)
             int_state%SST(I,J)           = TSEA(1)
             int_state%SOILTB(I,J)        = XTG3FCS(1)
             IF( SRFLAG(1) >= 0.5 .AND. SLMSK(1) >= 0.5 ) &
             int_state%ACSNOW(I,J)        = int_state%ACSNOW(I,J) + int_state%ACPREC(I,J)*100.

             int_state%PSHLTR(I,J)        = PSURF(1)*EXP(0.06823/T2M(1))
             int_state%TSHLTR(I,J)        = T2M(1)
             int_state%QSHLTR(I,J)        = Q2M(1)
             int_state%QSH(I,J)           = QSS(1)                                    !need QSS  from GFS
             int_state%T2(I,J)            = T2M(1)
             int_state%TH02(I,J)          = T2M(1)*(100000./PSURF(1))**RoCP
             int_state%Q02(I,J)           = Q2M(1)
             int_state%U10(I,J)           = U10M(1)
             int_state%V10(I,J)           = V10M(1)
             int_state%THS(I,J)           = TSFLW(1)*(100000./PSURF(1))**RoCP
             int_state%SIGT4(I,J)         = int_state%T(I,J,LM)*int_state%T(I,J,LM) * &
                                            int_state%T(I,J,LM)*int_state%T(I,J,LM) * STBOLT
         DO L=1,LM
            KFLIP=LM+1-L
             int_state%T(I,J,L)           = ADT(KFLIP)
             int_state%DUDT(I,J,L)        = (ADU(KFLIP) - GU(KFLIP)) / DTP
             int_state%DVDT(I,J,L)        = (ADV(KFLIP) - GV(KFLIP)) / DTP
             int_state%CLDFRA(I,J,L)      = CLDCOV_V(KFLIP) 
             int_state%Q (I,J,L)          = ADR(KFLIP,1)
             int_state%O3(I,J,L)          = ADR(KFLIP,2)
             int_state%CW(I,J,L)          = ADR(KFLIP,3)
         ENDDO
!
!-----------------------------------------------------------------------
! ***     End update after Physics
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
! ***  End GFS-PHYS domain loop
!-----------------------------------------------------------------------
!
            ENDDO  i_loop
!
          ENDDO    j_loop
!
!-----------------------------------------------------------------------
!
          td%gfs_phy_tim=td%gfs_phy_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Exchange wind tendencies
!-----------------------------------------------------------------------
!
          btim=timef()
!
          CALL HALO_EXCH(int_state%DUDT,LM,int_state%DVDT,LM,3,3)
!
          td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Now interpolate wind tendencies from H to V points.
!-----------------------------------------------------------------------
!
          btim=timef()
!
          CALL H_TO_V_TEND(int_state%DUDT,int_state%DT,int_state%NPHS,LM &
                          ,int_state%U)
          CALL H_TO_V_TEND(int_state%DVDT,int_state%DT,int_state%NPHS,LM &
                          ,int_state%V)
!
          td%h_to_v_tim=td%h_to_v_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!***  Poles and East-West boundary.
!-----------------------------------------------------------------------
!
          IF(int_state%GLOBAL)THEN
            btim=timef()
!
            CALL SWAPHN(int_state%T,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%T,IMS,IME,JMS,JME,LM                  &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPHN(int_state%Q,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%Q,IMS,IME,JMS,JME,LM                  &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPHN(int_state%CW,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%CW,IMS,IME,JMS,JME,LM                 &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPHN(int_state%O3,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEHN(int_state%O3,IMS,IME,JMS,JME,LM                 &
                       ,int_state%INPES,int_state%JNPES)
!
            CALL SWAPWN(int_state%U,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL SWAPWN(int_state%V,IMS,IME,JMS,JME,LM,int_state%INPES)
            CALL POLEWN(int_state%U,int_state%V,IMS,IME,JMS,JME,LM      &
                       ,int_state%INPES,int_state%JNPES)
!
            td%pole_swap_tim=td%pole_swap_tim+(timef()-btim)
          ENDIF
!
!-----------------------------------------------------------------------
!***  Exchange U, V, T, Q and CW
!-----------------------------------------------------------------------
!
          btim=timef()
!
          CALL HALO_EXCH(int_state%T,LM                                 &
                        ,3,3)
!
          CALL HALO_EXCH(int_state%Q,LM,int_state%CW,LM                 &
                        ,3,3)
!
          CALL HALO_EXCH(int_state%O3,LM                                &
                        ,3,3)
!
          CALL HALO_EXCH(int_state%U,LM,int_state%V,LM                  &
                        ,3,3)
!
          td%exch_tim=td%exch_tim+(timef()-btim)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
        ENDIF gfs_physics
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
        DEALLOCATE(LONSPERLAR)
        DEALLOCATE(GLOBAL_LATS_R)
        DEALLOCATE(CLDCOV_V)
        DEALLOCATE(PRSL)
        DEALLOCATE(PRSLK)
        DEALLOCATE(GU)
        DEALLOCATE(GV)
        DEALLOCATE(GT)
        DEALLOCATE(GR)
        DEALLOCATE(VVEL)
        DEALLOCATE(F_ICE)
        DEALLOCATE(F_RAIN)
        DEALLOCATE(R_RIME)
        DEALLOCATE(ADT)
        DEALLOCATE(ADU)
        DEALLOCATE(ADV)
        DEALLOCATE(PHIL)
        DEALLOCATE(GR3)
        DEALLOCATE(ADR)
        DEALLOCATE(PRSI)
        DEALLOCATE(PRSIK)
        DEALLOCATE(RSGM)
        DEALLOCATE(PHII)
        DEALLOCATE(SINLAT_R)
        DEALLOCATE(COSLAT_R)
        DEALLOCATE(XLON)
        DEALLOCATE(COSZEN)
        DEALLOCATE(COSZDG)
        DEALLOCATE(RANN)
        DEALLOCATE(RANNUM)
        DEALLOCATE(GR1)
        DEALLOCATE(SWH)
        DEALLOCATE(HLW)
        DEALLOCATE(DKH)
        DEALLOCATE(RNP)
        DEALLOCATE(UPD_MF)
        DEALLOCATE(DWN_MF)
        DEALLOCATE(DET_MF)
        DEALLOCATE(DQDT)
        DEALLOCATE(DQ3DT)
        DEALLOCATE(DT3DT)
        DEALLOCATE(DU3DT)
        DEALLOCATE(DV3DT)
        DEALLOCATE(PHY_F3DV)
!
!#######################################################################
!#######################################################################
!######### E N D   O F   G F S   P H Y S I C S   D R I V E R ###########
!#######################################################################
!#######################################################################
!-----------------------------------------------------------------------
!
      ENDIF  gfs_phys_test 
!
!-----------------------------------------------------------------------
!***  Write precipitation files for ADJPPT regression test
!-----------------------------------------------------------------------
!
      IF( int_state%WRITE_PREC_ADJ   .AND.                              &
          MOD(XTIME,60.) <= 0.001    .AND.                              &
          INT(XTIME/60.) <= int_state%PCPHR ) THEN
        CALL WRT_PCP(int_state%PREC                                     &
                ,MYPE,NUM_PES,MPI_COMM_COMP,MY_DOMAIN_ID                &
                ,INT(XTIME/60.)+1                                       &
                ,IDS,IDE,JDS,JDE                                        &
                ,IMS,IME,JMS,JME                                        &
                ,ITS,ITE,JTS,JTE)
      ENDIF
!
      ENDIF  physics
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!---- PHY_RUN END ------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Deallocate temporary arrays.
!-----------------------------------------------------------------------
!
      DEALLOCATE(DSG2)
      DEALLOCATE(PDSG1)
      DEALLOCATE(PSGML1)
      DEALLOCATE(SGML2)
!
      DEALLOCATE(SG1)
      DEALLOCATE(SG2)
!
      DEALLOCATE(CURV)
      DEALLOCATE(DARE)
      DEALLOCATE(DDMPU)
      DEALLOCATE(DXV)
      DEALLOCATE(FAD)
      DEALLOCATE(FAH)
      DEALLOCATE(FCP)
      DEALLOCATE(FDIV)
      DEALLOCATE(RARE)
      DEALLOCATE(RDXH)
      DEALLOCATE(RDXV)
      DEALLOCATE(WPDAR)
!
      DEALLOCATE(F)
      DEALLOCATE(FIS)
      DEALLOCATE(HDACX)
      DEALLOCATE(HDACY)
      DEALLOCATE(HDACVX)
      DEALLOCATE(HDACVY)
      DEALLOCATE(SICE)
      DEALLOCATE(SM)
!
      RC=0
!
      IF(RC_RUN==ESMF_SUCCESS)THEN
!       WRITE(0,*)'SOLVER RUN STEP SUCCEEDED'
      ELSE
        WRITE(0,*)'SOLVER RUN STEP FAILED RC_RUN=',RC_RUN
      ENDIF
!
!-----------------------------------------------------------------------
!
      td%solver_phy_tim=td%solver_phy_tim+(timef()-btim0)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SOLVER_RUN
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE SOLVER_FINALIZE (GRID_COMP                             &
                                 ,IMP_STATE                             &
                                 ,EXP_STATE                             &
                                 ,CLOCK_ATM                             &
                                 ,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!***  Finalize the Solver component.
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
      TYPE(ESMF_GridComp) :: GRID_COMP                                     !<-- The Solver gridded component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The Solver import state
                         ,EXP_STATE                                        !<-- The Solver export state
!
      TYPE(ESMF_Clock) :: CLOCK_ATM                                        !<-- The ATM component's ESMF Clock.
!
      INTEGER,INTENT(OUT) :: RC_FINALIZE
!      
!---------------------
!***  Local Variables
!---------------------
!
      TYPE(SOLVER_INTERNAL_STATE),POINTER :: INT_STATE                     !<-- The Solver internal state pointer 
!
      TYPE(WRAP_SOLVER_INT_STATE) :: WRAP                                  !<-- The F90 'wrap' for the Solver internal state
!
      INTEGER(kind=KINT) :: MYPE,RC,RC_FINAL
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC      =ESMF_SUCCESS
      RC_FINAL=ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Extract the Solver internal state.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="SOLVER_FINALIZE: Extract Solver Internal State"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompGetInternalState(GRID_COMP                      &  !<-- The Solver component
                                        ,WRAP                           &
                                        ,RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_FINAL)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      INT_STATE=>wrap%INT_STATE
!
      MYPE=int_state%MYPE                                                  !<-- The local task rank
!
      IF(MYPE==0)THEN
        WRITE(0,*)' Solver Completed Normally.'
      ENDIF
!
!-----------------------------------------------------------------------
!***  DO NOT DEALLOCATE THE SOLVER INTERNAL STATE POINTER 
!***  WITHOUT DEALLOCATING ITS CONTENTS.
!-----------------------------------------------------------------------
!
!!!   DEALLOCATE(INT_STATE,stat=RC)
!
!-----------------------------------------------------------------------
!
      IF(RC_FINAL==ESMF_SUCCESS)THEN
        WRITE(0,*)'SOLVER FINALIZE STEP SUCCEEDED'
      ELSE
        WRITE(0,*)'SOLVER FINALIZE STEP FAILED'
      ENDIF
!
!     IF(PRESENT(RC_FINALIZE))THEN
        RC_FINALIZE=RC_FINAL
!     ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SOLVER_FINALIZE
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE UPDATE_BC_TENDS(IMP_STATE                              &
                                ,LM,LNSH,LNSV                           &
                                ,PARENT_CHILD_TIME_RATIO,DT             &
                                ,S_BDY,N_BDY,W_BDY,E_BDY                &
                                ,PDBS,PDBN,PDBW,PDBE                    &
                                ,TBS,TBN,TBW,TBE                        &
                                ,QBS,QBN,QBW,QBE                        &
                                ,WBS,WBN,WBW,WBE                        &
                                ,UBS,UBN,UBW,UBE                        &
                                ,VBS,VBN,VBW,VBE                        &
                                ,ITS,ITE,JTS,JTE                        &
                                ,IMS,IME,JMS,JME                        &
                                ,IDS,IDE,JDS,JDE )
! 
!-----------------------------------------------------------------------
!***  This routine extracts boundary data from the Solver import
!***  state of nested domains that was received from their parents.
!***  This data is then used to update the time tendencies of the
!***  boundary variables.  Those tendencies are valid through each
!***  timestep of the nested domain's parent.
!***  Note that this data was first loaded into the export state of
!***  the Parent-Child coupler in subroutine EXPORT_CHILD_BOUNDARY.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      INTEGER,INTENT(IN) :: LNSH                                        &  !<-- # of boundary blending rows for H points
                           ,LNSV                                        &  !<-- # of boundary blending rows for V points
                           ,PARENT_CHILD_TIME_RATIO                        !<-- # of child timesteps per parent timestep
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE                             &  !
                           ,IMS,IME,JMS,JME                             &  !<-- Array dimensions
                           ,ITS,ITE,JTS,JTE                             &  !
                           ,LM                                             !
!
      REAL,INTENT(IN) :: DT                                                !<-- This domain's fundamental timestep
!
      LOGICAL(kind=KLOG),INTENT(IN) :: E_BDY,N_BDY,S_BDY,W_BDY             !<-- Is this task on any side of its domain boundary?
!
      TYPE(ESMF_State),INTENT(INOUT) :: IMP_STATE                          !<-- Solver import state
!
      REAL,DIMENSION(IMS:IME,1:LNSH,     1:2),INTENT(INOUT) :: PDBS,PDBN   !<-- South/North PD values/tendencies
!
      REAL,DIMENSION(IMS:IME,1:LNSH,1:LM,1:2),INTENT(INOUT) :: TBS,TBN  &  !<-- South/North temperature values/tendencies
                                                              ,QBS,QBN  &  !<-- South/North specific humidity values/tendencies
                                                              ,WBS,WBN     !<-- South/North cloud condensate values/tendencies
!
      REAL,DIMENSION(IMS:IME,1:LNSV,1:LM,1:2),INTENT(INOUT) :: UBS,UBN  &  !<-- South/North U wind values/tendencies
                                                              ,VBS,VBN     !<-- South/North V wind values/tendencies
!
      REAL,DIMENSION(1:LNSH,JMS:JME,     1:2),INTENT(INOUT) :: PDBW,PDBE   !<-- West/East PD values/tendencies
!
      REAL,DIMENSION(1:LNSH,JMS:JME,1:LM,1:2),INTENT(INOUT) :: TBW,TBE  &  !<-- West/East temperature values/tendencies
                                                              ,QBW,QBE  &  !<-- West/East specific humidity values/tendencies
                                                              ,WBW,WBE     !<-- West/East cloud condensate values/tendencies
!
      REAL,DIMENSION(1:LNSV,JMS:JME,1:LM,1:2),INTENT(INOUT) :: UBW,UBE  &  !<-- West/East U wind values/tendencies
                                                              ,VBW,VBE     !<-- West/East V wind values/tendencies
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER,SAVE :: I1=-1,I2_H=-1,I2_V,J1=-1,J2_H=-1,J2_V
!
      INTEGER,SAVE :: KOUNT_S_H,KOUNT_S_V,KOUNT_N_H,KOUNT_N_V           &
                     ,KOUNT_W_H,KOUNT_W_V,KOUNT_E_H,KOUNT_E_V
!
      INTEGER      :: I,J,K,KOUNT,RC,RC_BCT
!
      REAL,SAVE :: RECIP
!
      REAL,DIMENSION(:),ALLOCATABLE,SAVE :: BND_DATA_S_H                &
                                           ,BND_DATA_S_V                & 
                                           ,BND_DATA_N_H                & 
                                           ,BND_DATA_N_V                & 
                                           ,BND_DATA_W_H                & 
                                           ,BND_DATA_W_V                & 
                                           ,BND_DATA_E_H                & 
                                           ,BND_DATA_E_V
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC    =ESMF_SUCCESS
      RC_BCT=ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Gridpoint index limits along the South/North and West/East
!***  boundaries for mass (H) and velocity (V) points.  Note that
!***  the boundary data goes two points into the halo.
!-----------------------------------------------------------------------
!
      I1  =MAX(ITS-2,IDS)
      I2_H=MIN(ITE+2,IDE)
      I2_V=MIN(ITE+2,IDE-1)
      J1  =MAX(JTS-2,JDS)
      J2_H=MIN(JTE+2,JDE)
      J2_V=MIN(JTE+2,JDE-1)
!
!-----------------------------------------------------------------------
!***  The following 'KOUNT' variables are the number of gridpoints
!***  on the given task subdomain's South/North/West/East boundaries
!***  for all quantities on mass and velocity points.
!-----------------------------------------------------------------------
!
      KOUNT_S_H=(3*LM+1)*(I2_H-I1+1)*LNSH
      KOUNT_N_H=(3*LM+1)*(I2_H-I1+1)*LNSH
      KOUNT_S_V=2*LM*(I2_V-I1+1)*LNSV
      KOUNT_N_V=2*LM*(I2_V-I1+1)*LNSV
      KOUNT_W_H=(3*LM+1)*(J2_H-J1+1)*LNSH
      KOUNT_E_H=(3*LM+1)*(J2_H-J1+1)*LNSH
      KOUNT_W_V=2*LM*(J2_V-J1+1)*LNSV
      KOUNT_E_V=2*LM*(J2_V-J1+1)*LNSV
!
!-----------------------------------------------------------------------
!***  Allocate the boundary pointer arrays into which the boundary
!***  data from the Solver import state will be unloaded.
!-----------------------------------------------------------------------
!
      ALLOCATE(BND_DATA_S_H(1:KOUNT_S_H))
      ALLOCATE(BND_DATA_S_V(1:KOUNT_S_V))
      ALLOCATE(BND_DATA_N_H(1:KOUNT_N_H))
      ALLOCATE(BND_DATA_N_V(1:KOUNT_N_V))
      ALLOCATE(BND_DATA_W_H(1:KOUNT_W_H))
      ALLOCATE(BND_DATA_W_V(1:KOUNT_W_V))
      ALLOCATE(BND_DATA_E_H(1:KOUNT_E_H))
      ALLOCATE(BND_DATA_E_V(1:KOUNT_E_V))
!
!-----------------------------------------------------------------------
!***  Compute RECIP every time in case the sign of DT has changed 
!***  due to digital filtering.
!-----------------------------------------------------------------------
!
      RECIP=1./(DT*PARENT_CHILD_TIME_RATIO)
!
!-----------------------------------------------------------------------
!***  Unload the boundary data from the import state and compute
!***  the time tendencies for the time period spanning the number
!***  of this nest's timesteps needed to reach the end of its 
!***  parent's timestep (from which the data was sent).
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  If this is a moving nest SOLVER_RUN already knows if it moved at 
!***  the beginning of this timestep.  If it has then the import state 
!***  not only contains the usual boundary data from one parent timestep 
!***  in the future but it also contains boundary data for the current
!***  timestep for the domain's new location.  We would then need to
!***  fill the current time level of the boundary variable arrays 
!***  before differencing with the values from the future to obtain
!***  the tendencies.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
      south: IF(S_BDY)THEN
!
!-----------------------------------------------------------------------
!
!-------------
!***  South H
!-------------
!

        move_now_south_h: IF(MOVE_NOW==ESMF_TRUE)THEN



!
!-----------------------------------------------------------------------
!***  Time level 1 (current) south boundary H values for new location
!***  of this nest.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Extract South Boundary H Data in UPDATE_BC_TENDS for Time N"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state    =IMP_STATE                    &  !<-- Solver import state
                                ,name     ='SOUTH_H_Current'            &  !<-- Name of south boundary H data at time N
                                ,count=KOUNT_S_H                    &  !<-- # of words in this boundary data
                                ,valueList=BND_DATA_S_H                 &  !<-- The south boundary H data at time N
                                ,rc       =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          KOUNT=0
!
          DO J=1,LNSH
          DO I=I1,I2_H
            KOUNT=KOUNT+1
            PDBS(I,J,1)=BND_DATA_S_H(KOUNT)
          ENDDO
          ENDDO
!
          DO K=1,LM
          DO J=1,LNSH
          DO I=I1,I2_H
            TBS(I,J,K,1)=BND_DATA_S_H(KOUNT+1)
            QBS(I,J,K,1)=BND_DATA_S_H(KOUNT+2)
            WBS(I,J,K,1)=BND_DATA_S_H(KOUNT+3)
            KOUNT=KOUNT+3
          ENDDO
          ENDDO
          ENDDO
!
        ENDIF move_now_south_h
!
!-----------------------------------------------------------------------
!***  Use time level 2 (future) south boundary H values to compute
!***  new tendencies.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract South Boundary H Data in UPDATE_BC_TENDS for Time N+1"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- Solver import state
                              ,name     ='SOUTH_H_Future'               &  !<-- Name of south boundary H data at time N+1
                              ,count=KOUNT_S_H                      &  !<-- # of words in this boundary data
                              ,valueList=BND_DATA_S_H                   &  !<-- The boundary data
                              ,rc       =RC )

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        KOUNT=0
!
        DO J=1,LNSH
        DO I=I1,I2_H
          KOUNT=KOUNT+1
          PDBS(I,J,2)=(BND_DATA_S_H(KOUNT)-PDBS(I,J,1))*RECIP
        ENDDO
        ENDDO
!
        DO K=1,LM
        DO J=1,LNSH
        DO I=I1,I2_H
          TBS(I,J,K,2)=(BND_DATA_S_H(KOUNT+1)-TBS(I,J,K,1))*RECIP
          QBS(I,J,K,2)=(BND_DATA_S_H(KOUNT+2)-QBS(I,J,K,1))*RECIP
          WBS(I,J,K,2)=(BND_DATA_S_H(KOUNT+3)-WBS(I,J,K,1))*RECIP
          KOUNT=KOUNT+3
        ENDDO
        ENDDO
        ENDDO
!
!-------------
!***  South V
!-------------
!

        move_now_south_v: IF(MOVE_NOW==ESMF_TRUE)THEN



!
!-----------------------------------------------------------------------
!***  Time level 1 (current) south boundary V values for new location
!***  of this nest.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Extract South Boundary V Data in UPDATE_BC_TENDS for Time N"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state    =IMP_STATE                    &  !<-- Solver import state
                                ,name     ='SOUTH_V_Current'            &  !<-- Name of south boundary V data at time N
                                ,count=KOUNT_S_V                    &  !<-- # of words in this boundary data
                                ,valueList=BND_DATA_S_V                 &  !<-- The south boundary V data at time N
                                ,rc       =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          KOUNT=0
!
          DO K=1,LM
          DO J=1,LNSV
          DO I=I1,I2_V
            UBS(I,J,K,1)=BND_DATA_S_V(KOUNT+1)
            VBS(I,J,K,1)=BND_DATA_S_V(KOUNT+2)
            KOUNT=KOUNT+2
          ENDDO
          ENDDO
          ENDDO
!
        ENDIF move_now_south_v
!
!-----------------------------------------------------------------------
!***  Use time level 2 (future) south boundary V values to compute
!***  new tendencies.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract South Boundary V Data in UPDATE_BC_TENDS"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- Solver import state
                              ,name     ='SOUTH_V_Future'               &  !<-- Name of south boundary V data at time N+1
                              ,count=KOUNT_S_V                      &  !<-- # of words in this boundary data
                              ,valueList=BND_DATA_S_V                   &  !<-- The boundary data
                              ,rc       =RC )

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        KOUNT=0
!
        DO K=1,LM
        DO J=1,LNSV
        DO I=I1,I2_V
          UBS(I,J,K,2)=(BND_DATA_S_V(KOUNT+1)-UBS(I,J,K,1))*RECIP
          VBS(I,J,K,2)=(BND_DATA_S_V(KOUNT+2)-VBS(I,J,K,1))*RECIP
          KOUNT=KOUNT+2
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF south
!
!-----------------------------------------------------------------------
!
      north: IF(N_BDY)THEN
!
!-----------------------------------------------------------------------
!
!-------------
!***  North H
!-------------
!

        move_now_north_h: IF(MOVE_NOW==ESMF_TRUE)THEN



!
!-----------------------------------------------------------------------
!***  Time level 1 (current) north boundary H values for new location
!***  of this nest.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Extract North Boundary H Data in UPDATE_BC_TENDS for Time N"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state    =IMP_STATE                    &  !<-- Solver import state
                                ,name     ='NORTH_H_Current'            &  !<-- Name of north boundary H data at time N
                                ,count=KOUNT_N_H                    &  !<-- # of words in this boundary data
                                ,valueList=BND_DATA_N_H                 &  !<-- The north boundary H data at time N
                                ,rc       =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          KOUNT=0
!
          DO J=1,LNSH
          DO I=I1,I2_H
            KOUNT=KOUNT+1
            PDBN(I,J,1)=BND_DATA_N_H(KOUNT)
          ENDDO
          ENDDO
!
          DO K=1,LM
          DO J=1,LNSH
          DO I=I1,I2_H
            TBN(I,J,K,1)=BND_DATA_N_H(KOUNT+1)
            QBN(I,J,K,1)=BND_DATA_N_H(KOUNT+2)
            WBN(I,J,K,1)=BND_DATA_N_H(KOUNT+3)
            KOUNT=KOUNT+3
          ENDDO
          ENDDO
          ENDDO
!
        ENDIF move_now_north_h
!
!-----------------------------------------------------------------------
!***  Use time level 2 (future) north boundary H values to compute
!***  new tendencies.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract North Boundary H Data in UPDATE_BC_TENDS for time N+1"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- Solver import state
                              ,name     ='NORTH_H_Future'               &  !<-- Name of north boundary H data for time N+1
                              ,count=KOUNT_N_H                      &  !<-- # of words in this boundary data
                              ,valueList=BND_DATA_N_H                   &  !<-- The boundary data
                              ,rc       =RC )

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        KOUNT=0
!
        DO J=1,LNSH
        DO I=I1,I2_H
          KOUNT=KOUNT+1
          PDBN(I,J,2)=(BND_DATA_N_H(KOUNT)-PDBN(I,J,1))*RECIP
        ENDDO
        ENDDO
!
        DO K=1,LM
        DO J=1,LNSH
        DO I=I1,I2_H
          TBN(I,J,K,2)=(BND_DATA_N_H(KOUNT+1)-TBN(I,J,K,1))*RECIP
          QBN(I,J,K,2)=(BND_DATA_N_H(KOUNT+2)-QBN(I,J,K,1))*RECIP
          WBN(I,J,K,2)=(BND_DATA_N_H(KOUNT+3)-WBN(I,J,K,1))*RECIP
          KOUNT=KOUNT+3
        ENDDO
        ENDDO
        ENDDO
!
!-------------
!***  North V
!-------------
!

        move_now_north_v: IF(MOVE_NOW==ESMF_TRUE)THEN



!
!-----------------------------------------------------------------------
!***  Time level 1 (current) north boundary V values for new location
!***  of this nest.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Extract North Boundary V Data in UPDATE_BC_TENDS for Time N"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state    =IMP_STATE                    &  !<-- Solver import state
                                ,name     ='NORTH_V_Current'            &  !<-- Name of north boundary V data at time N
                                ,count=KOUNT_N_V                    &  !<-- # of words in this boundary data
                                ,valueList=BND_DATA_N_V                 &  !<-- The north boundary V data at time N
                                ,rc       =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          KOUNT=0
!
          DO K=1,LM
          DO J=1,LNSV
          DO I=I1,I2_V
            UBN(I,J,K,1)=BND_DATA_N_V(KOUNT+1)
            VBN(I,J,K,1)=BND_DATA_N_V(KOUNT+2)
            KOUNT=KOUNT+2
          ENDDO
          ENDDO
          ENDDO
!
        ENDIF move_now_north_v
!
!-----------------------------------------------------------------------
!***  Use time level 2 (future) north boundary H values to compute
!***  new tendencies.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract North Boundary V Data in UPDATE_BC_TENDS for Time N+1"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- Solver import state
                              ,name     ='NORTH_V_Future'               &  !<-- Name of north boundary V data at time N+1
                              ,count=KOUNT_N_V                      &  !<-- # of words in this boundary data
                              ,valueList=BND_DATA_N_V                   &  !<-- The boundary data
                              ,rc       =RC )

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        KOUNT=0
!
        DO K=1,LM
        DO J=1,LNSV
        DO I=I1,I2_V
          UBN(I,J,K,2)=(BND_DATA_N_V(KOUNT+1)-UBN(I,J,K,1))*RECIP
          VBN(I,J,K,2)=(BND_DATA_N_V(KOUNT+2)-VBN(I,J,K,1))*RECIP
          KOUNT=KOUNT+2
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF north
!
!-----------------------------------------------------------------------
!
      west: IF(W_BDY)THEN
!
!-----------------------------------------------------------------------
!
!------------
!***  West H
!------------
!

        move_now_west_h: IF(MOVE_NOW==ESMF_TRUE)THEN



!
!-----------------------------------------------------------------------
!***  Time level 1 (current) west boundary H values for new location
!***  of this nest.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Extract West Boundary H Data in UPDATE_BC_TENDS for Time N"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state    =IMP_STATE                    &  !<-- Solver import state
                                ,name     ='WEST_H_Current'             &  !<-- Name of west boundary H data at time N
                                ,count=KOUNT_W_H                    &  !<-- # of words in this boundary data
                                ,valueList=BND_DATA_W_H                 &  !<-- The west boundary H data at time N
                                ,rc       =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          KOUNT=0
!
          DO J=J1,J2_H
          DO I=1,LNSH
            KOUNT=KOUNT+1
            PDBW(I,J,1)=BND_DATA_W_H(KOUNT)
          ENDDO
          ENDDO
!
          DO K=1,LM
          DO J=J1,J2_H
          DO I=1,LNSH
            TBW(I,J,K,1)=BND_DATA_W_H(KOUNT+1)
            QBW(I,J,K,1)=BND_DATA_W_H(KOUNT+2)
            WBW(I,J,K,1)=BND_DATA_W_H(KOUNT+3)
            KOUNT=KOUNT+3
          ENDDO
          ENDDO
          ENDDO
!
        ENDIF move_now_west_h
!
!-----------------------------------------------------------------------
!***  Use time level 2 (future) west boundary H values to compute
!***  new tendencies.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract West Boundary H Data in UPDATE_BC_TENDS at Time N+1"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- Solver import state
                              ,name     ='WEST_H_Future'                &  !<-- Name of west boundary H data at time N+1
                              ,count=KOUNT_W_H                      &  !<-- # of words in this boundary data
                              ,valueList=BND_DATA_W_H                   &  !<-- The boundary data
                              ,rc       =RC )

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        KOUNT=0
!
        DO J=J1,J2_H
        DO I=1,LNSH
          KOUNT=KOUNT+1
          PDBW(I,J,2)=(BND_DATA_W_H(KOUNT)-PDBW(I,J,1))*RECIP
        ENDDO
        ENDDO
!
        DO K=1,LM
        DO J=J1,J2_H
        DO I=1,LNSH
          TBW(I,J,K,2)=(BND_DATA_W_H(KOUNT+1)-TBW(I,J,K,1))*RECIP
          QBW(I,J,K,2)=(BND_DATA_W_H(KOUNT+2)-QBW(I,J,K,1))*RECIP
          WBW(I,J,K,2)=(BND_DATA_W_H(KOUNT+3)-WBW(I,J,K,1))*RECIP
          KOUNT=KOUNT+3
        ENDDO
        ENDDO
        ENDDO
!
!------------
!***  West V
!------------
!

        move_now_west_v: IF(MOVE_NOW==ESMF_TRUE)THEN



!
!-----------------------------------------------------------------------
!***  Time level 1 (current) west boundary V values for new location
!***  of this nest.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Extract West Boundary V Data in UPDATE_BC_TENDS for Time N"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state    =IMP_STATE                    &  !<-- Solver import state
                                ,name     ='WEST_V_Current'             &  !<-- Name of west boundary V data at time N
                                ,count=KOUNT_W_V                    &  !<-- # of words in this boundary data
                                ,valueList=BND_DATA_W_V                 &  !<-- The west boundary V data at time N
                                ,rc       =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          KOUNT=0
!
          DO K=1,LM
          DO J=J1,J2_V
          DO I=1,LNSV
            UBW(I,J,K,1)=BND_DATA_W_V(KOUNT+1)
            VBW(I,J,K,1)=BND_DATA_W_V(KOUNT+2)
            KOUNT=KOUNT+2
          ENDDO
          ENDDO
          ENDDO
!
        ENDIF move_now_west_v
!
!-----------------------------------------------------------------------
!***  Use time level 2 (future) west boundary V values to compute
!***  new tendencies.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract West Boundary V Data in UPDATE_BC_TENDS at Time N+1"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- Solver import state
                              ,name     ='WEST_V_Future'                &  !<-- Name of west boundary V data at time N+1
                              ,count=KOUNT_W_V                      &  !<-- # of words in this boundary data
                              ,valueList=BND_DATA_W_V                   &  !<-- The boundary data
                              ,rc       =RC )

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        KOUNT=0
!
        DO K=1,LM
        DO J=J1,J2_V
        DO I=1,LNSV
          UBW(I,J,K,2)=(BND_DATA_W_V(KOUNT+1)-UBW(I,J,K,1))*RECIP
          VBW(I,J,K,2)=(BND_DATA_W_V(KOUNT+2)-VBW(I,J,K,1))*RECIP
          KOUNT=KOUNT+2
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF west
!
!-----------------------------------------------------------------------
!
      east: IF(E_BDY)THEN
!
!-----------------------------------------------------------------------
!
!------------
!***  East H
!------------
!

        move_now_east_h: IF(MOVE_NOW==ESMF_TRUE)THEN



!
!-----------------------------------------------------------------------
!***  Time level 1 (current) east boundary H values for new location
!***  of this nest.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Extract East Boundary H Data in UPDATE_BC_TENDS for Time N"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state    =IMP_STATE                    &  !<-- Solver import state
                                ,name     ='EAST_H_Current'             &  !<-- Name of east boundary H data at time N
                                ,count=KOUNT_E_H                    &  !<-- # of words in this boundary data
                                ,valueList=BND_DATA_E_H                 &  !<-- The east boundary H data at time N
                                ,rc       =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          KOUNT=0
!
          DO J=J1,J2_H
          DO I=1,LNSH
            KOUNT=KOUNT+1
            PDBE(I,J,1)=BND_DATA_E_H(KOUNT)
          ENDDO
          ENDDO
!
          DO K=1,LM
          DO J=J1,J2_H
          DO I=1,LNSH
            TBE(I,J,K,1)=BND_DATA_E_H(KOUNT+1)
            QBE(I,J,K,1)=BND_DATA_E_H(KOUNT+2)
            WBE(I,J,K,1)=BND_DATA_E_H(KOUNT+3)
            KOUNT=KOUNT+3
          ENDDO
          ENDDO
          ENDDO
!
        ENDIF move_now_east_h
!
!-----------------------------------------------------------------------
!***  Use time level 2 (future) east boundary H values to compute
!***  new tendencies.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract East Boundary H Data in UPDATE_BC_TENDS at Time N+1"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- Solver import state
                              ,name     ='EAST_H_Future'                &  !<-- Name of east boundary H data at time N+1
                              ,count=KOUNT_E_H                      &  !<-- # of words in this boundary data
                              ,valueList=BND_DATA_E_H                   &  !<-- The boundary data
                              ,rc       =RC )

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        KOUNT=0
!
        DO J=J1,J2_H
        DO I=1,LNSH
          KOUNT=KOUNT+1
          PDBE(I,J,2)=(BND_DATA_E_H(KOUNT)-PDBE(I,J,1))*RECIP
        ENDDO
        ENDDO
!
        DO K=1,LM
        DO J=J1,J2_H
        DO I=1,LNSH
          TBE(I,J,K,2)=(BND_DATA_E_H(KOUNT+1)-TBE(I,J,K,1))*RECIP
          QBE(I,J,K,2)=(BND_DATA_E_H(KOUNT+2)-QBE(I,J,K,1))*RECIP
          WBE(I,J,K,2)=(BND_DATA_E_H(KOUNT+3)-WBE(I,J,K,1))*RECIP
          KOUNT=KOUNT+3
        ENDDO
        ENDDO
        ENDDO
!
!------------
!***  East V
!------------
!

        move_now_east_v: IF(MOVE_NOW==ESMF_TRUE)THEN



!
!-----------------------------------------------------------------------
!***  Time level 1 (current) east boundary V values for new location
!***  of this nest.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          MESSAGE_CHECK="Extract East Boundary V Data in UPDATE_BC_TENDS for Time N"
!         CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          CALL ESMF_AttributeGet(state    =IMP_STATE                    &  !<-- Solver import state
                                ,name     ='EAST_V_Current'             &  !<-- Name of esat boundary V data at time N
                                ,count=KOUNT_E_V                    &  !<-- # of words in this boundary data
                                ,valueList=BND_DATA_E_V                 &  !<-- The east boundary V data at time N
                                ,rc       =RC )
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
          CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
          KOUNT=0
!
          DO K=1,LM
          DO J=J1,J2_V
          DO I=1,LNSV
            UBE(I,J,K,1)=BND_DATA_E_V(KOUNT+1)
            VBE(I,J,K,1)=BND_DATA_E_V(KOUNT+2)
            KOUNT=KOUNT+2
          ENDDO
          ENDDO
          ENDDO
!
        ENDIF move_now_east_v
!
!-----------------------------------------------------------------------
!***  Use time level 2 (future) east boundary V values to compute
!***  new tendencies.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract East Boundary V Data in UPDATE_BC_TENDS for Time N+1"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeGet(state    =IMP_STATE                      &  !<-- Solver import state
                              ,name     ='EAST_V_Future'                &  !<-- Name of east boundary V data at time N+1
                              ,count=KOUNT_E_V                      &  !<-- # of words in this boundary data
                              ,valueList=BND_DATA_E_V                   &  !<-- The boundary data
                              ,rc       =RC )

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_BCT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        KOUNT=0
!
        DO K=1,LM
        DO J=J1,J2_V
        DO I=1,LNSV
          UBE(I,J,K,2)=(BND_DATA_E_V(KOUNT+1)-UBE(I,J,K,1))*RECIP
          VBE(I,J,K,2)=(BND_DATA_E_V(KOUNT+2)-VBE(I,J,K,1))*RECIP
          KOUNT=KOUNT+2
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF east
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(BND_DATA_S_H)
      DEALLOCATE(BND_DATA_S_V)
      DEALLOCATE(BND_DATA_N_H)
      DEALLOCATE(BND_DATA_N_V)
      DEALLOCATE(BND_DATA_W_H)
      DEALLOCATE(BND_DATA_W_V)
      DEALLOCATE(BND_DATA_E_H)
      DEALLOCATE(BND_DATA_E_V)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE UPDATE_BC_TENDS
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE SAVE_BC_DATA(LM,LNSV                                   &
                             ,PDBS,PDBN,PDBW,PDBE                       &
                             ,TBS,TBN,TBW,TBE                           &
                             ,QBS,QBN,QBW,QBE                           &
                             ,WBS,WBN,WBW,WBE                           &
                             ,UBS,UBN,UBW,UBE                           &
                             ,VBS,VBN,VBW,VBE                           &
                             ,NUM_WORDS_BC_SOUTH,RST_BC_DATA_SOUTH      &
                             ,NUM_WORDS_BC_NORTH,RST_BC_DATA_NORTH      &
                             ,NUM_WORDS_BC_WEST ,RST_BC_DATA_WEST       &
                             ,NUM_WORDS_BC_EAST ,RST_BC_DATA_EAST       &
                             ,EXP_STATE_SOLVER                          &
                             ,ITS,ITE,JTS,JTE                           &
                             ,IMS,IME,JMS,JME                           &
                             ,IDS,IDE,JDS,JDE                           &
                               )
! 
!-----------------------------------------------------------------------
!***  Boundary array winds are needed in the restart file in order to
!***  achieve bit identical answers between restarted runs and their
!***  free-forecast analogs.  The boundary arrays do not span the
!***  integration grid thus they can only be transmitted through
!***  ESMF States as Attributes.  Non-scalar Attributes can only
!***  contain one dimension therefore the boundary data is moved
!***  into 1-D arrays in this routine then inserted into the
!***  Write component's import state.
!-----------------------------------------------------------------------
!
!---------------------
!***  Input Arguments
!---------------------
!
      INTEGER(kind=KINT),INTENT(IN) :: LNSV                             &  !<-- # of boundary blending rows for V points
                                      ,NUM_WORDS_BC_SOUTH               &  !<-- Total # of words in south bndry winds, this fcst task
                                      ,NUM_WORDS_BC_NORTH               &  !<-- Total # of words in north bndry winds, this fcst task
                                      ,NUM_WORDS_BC_WEST                &  !<-- Total # of words in west bndry winds, this fcst task
                                      ,NUM_WORDS_BC_EAST                   !<-- Total # of words in east bndry winds, this fcst task
!
      INTEGER(kind=KINT),INTENT(IN) :: IDS,IDE,JDS,JDE                  &  !<-- 
                                      ,IMS,IME,JMS,JME                  &  !<-- Array dimensions
                                      ,ITS,ITE,JTS,JTE                  &  !<-- 
                                      ,LM                                  !<--
!
      REAL(kind=KFPT),DIMENSION(IMS:IME,1:LNSV,1:2),INTENT(IN) ::  &
                                                             PDBS,PDBN     !<-- South/north boundary PD
!
      REAL(kind=KFPT),DIMENSION(IMS:IME,1:LNSV,1:LM,1:2),INTENT(IN) ::  &
                                                               TBS,TBN  &  !<-- South/north boundary T
                                                              ,QBS,QBN  &  !<-- South/north boundary Q
                                                              ,WBS,WBN  &  !<-- South/north boundary CW
                                                              ,UBS,UBN  &  !<-- South/north boundary U
                                                              ,VBS,VBN     !<-- South/north boundary V
!
      REAL(kind=KFPT),DIMENSION(1:LNSV,JMS:JME,1:2),INTENT(IN) ::  &
                                                             PDBW,PDBE     !<-- West/east boundary PS
!
      REAL(kind=KFPT),DIMENSION(1:LNSV,JMS:JME,1:LM,1:2),INTENT(IN) ::  &
                                                               TBW,TBE  &  !<-- West/east boundary T
                                                              ,QBW,QBE  &  !<-- West/east boundary Q
                                                              ,WBW,WBE  &  !<-- West/east boundary CW
                                                              ,UBW,UBE  &  !<-- West/east boundary U
                                                              ,VBW,VBE     !<-- West/east boundary V
!
!---------------------
!***  Inout Arguments
!---------------------
!
      TYPE(ESMF_State),INTENT(INOUT) :: EXP_STATE_SOLVER                   !<-- The Solver export state
!
!----------------------
!***  Output Arguments
!----------------------
!
      REAL(kind=KFPT),DIMENSION(1:NUM_WORDS_BC_SOUTH),INTENT(OUT) ::    &
                                                     RST_BC_DATA_SOUTH     !<-- All south bndry wind data on this fcst task
      REAL(kind=KFPT),DIMENSION(1:NUM_WORDS_BC_NORTH),INTENT(OUT) ::    &
                                                     RST_BC_DATA_NORTH     !<-- All north bndry wind data on this fcst task
      REAL(kind=KFPT),DIMENSION(1:NUM_WORDS_BC_WEST ),INTENT(OUT) ::    &
                                                     RST_BC_DATA_WEST      !<-- All west bndry wind data on this fcst task
      REAL(kind=KFPT),DIMENSION(1:NUM_WORDS_BC_EAST ),INTENT(OUT) ::    &
                                                     RST_BC_DATA_EAST      !<-- All east bndry wind data on this fcst task
!
!-----------------------------------------------------------------------
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER(kind=KINT) :: IB,JB,KOUNT,L,RC,RC_SAVE
!
      TYPE(ESMF_State) :: IMP_STATE_WRITE
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Southern boundary data to 1-D
!-----------------------------------------------------------------------
!
      IF(JTS==JDS)THEN                                                     !<-- Tasks on south boundary
        KOUNT=0
!
          DO JB=1,LNSV
            DO IB=ITS,ITE
              RST_BC_DATA_SOUTH(KOUNT+ 1)=PDBS(IB,JB,1)
              RST_BC_DATA_SOUTH(KOUNT+ 2)=PDBS(IB,JB,2)
              KOUNT=KOUNT+2
            ENDDO
          ENDDO

        DO L=1,LM
          DO JB=1,LNSV
            DO IB=ITS,ITE
              RST_BC_DATA_SOUTH(KOUNT+ 1)=TBS(IB,JB,L,1)
              RST_BC_DATA_SOUTH(KOUNT+ 2)=TBS(IB,JB,L,2)
              RST_BC_DATA_SOUTH(KOUNT+ 3)=QBS(IB,JB,L,1)
              RST_BC_DATA_SOUTH(KOUNT+ 4)=QBS(IB,JB,L,2)
              RST_BC_DATA_SOUTH(KOUNT+ 5)=WBS(IB,JB,L,1)
              RST_BC_DATA_SOUTH(KOUNT+ 6)=WBS(IB,JB,L,2)
              RST_BC_DATA_SOUTH(KOUNT+ 7)=UBS(IB,JB,L,1)
              RST_BC_DATA_SOUTH(KOUNT+ 8)=UBS(IB,JB,L,2)
              RST_BC_DATA_SOUTH(KOUNT+ 9)=VBS(IB,JB,L,1)
              RST_BC_DATA_SOUTH(KOUNT+10)=VBS(IB,JB,L,2)
              KOUNT=KOUNT+10
            ENDDO
          ENDDO
        ENDDO
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract Write Import State in SAVE_BC_DATA"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_StateGet(state      =EXP_STATE_SOLVER                 &  !<-- The Solver export state
                          ,itemName   ='Write Import State'             &  !<-- Name of the state to get from Solver export state
                          ,nestedState=IMP_STATE_WRITE                  &  !<-- Extract Write Component import state from Solver export
                          ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_SAVE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Set BC South Data Attribute in SAVE_BC_DATA"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeSet(state    =IMP_STATE_WRITE                &  !<-- The Write component import state
                              ,name     ='RST_BC_DATA_SOUTH'            &  !<-- Name of 1-D string of south boundary values
                              ,count=NUM_WORDS_BC_SOUTH             &  !<-- # of south boundary words on this fcst task
                              ,valueList=RST_BC_DATA_SOUTH              &  !<-- The 1-D data being inserted into the Write import state
                              ,rc       =RC)

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_SAVE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  Northern boundary data to 1-D
!-----------------------------------------------------------------------
!
      IF(JTE==JDE)THEN                                                     !<-- Tasks on north boundary
        KOUNT=0
!
          DO JB=1,LNSV
            DO IB=ITS,ITE
              RST_BC_DATA_NORTH(KOUNT+ 1)=PDBN(IB,JB,1)
              RST_BC_DATA_NORTH(KOUNT+ 2)=PDBN(IB,JB,2)
              KOUNT=KOUNT+2
            ENDDO
          ENDDO

        DO L=1,LM
          DO JB=1,LNSV
            DO IB=ITS,ITE
              RST_BC_DATA_NORTH(KOUNT+ 1)=TBN(IB,JB,L,1)
              RST_BC_DATA_NORTH(KOUNT+ 2)=TBN(IB,JB,L,2)
              RST_BC_DATA_NORTH(KOUNT+ 3)=QBN(IB,JB,L,1)
              RST_BC_DATA_NORTH(KOUNT+ 4)=QBN(IB,JB,L,2)
              RST_BC_DATA_NORTH(KOUNT+ 5)=WBN(IB,JB,L,1)
              RST_BC_DATA_NORTH(KOUNT+ 6)=WBN(IB,JB,L,2)
              RST_BC_DATA_NORTH(KOUNT+ 7)=UBN(IB,JB,L,1)
              RST_BC_DATA_NORTH(KOUNT+ 8)=UBN(IB,JB,L,2)
              RST_BC_DATA_NORTH(KOUNT+ 9)=VBN(IB,JB,L,1)
              RST_BC_DATA_NORTH(KOUNT+10)=VBN(IB,JB,L,2)
              KOUNT=KOUNT+10
            ENDDO
          ENDDO
        ENDDO
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract Write Import State in SAVE_BC_DATA"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_StateGet(state      =EXP_STATE_SOLVER                 &  !<-- The Solver export state
                          ,itemName   ='Write Import State'             &  !<-- Name of the state to get from Solver export state
                          ,nestedState=IMP_STATE_WRITE                  &  !<-- Extract Write Component import state from Solver export
                          ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_SAVE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Set BC North Data Attribute in SAVE_BC_DATA"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeSet(state    =IMP_STATE_WRITE                &  !<-- The Write component import state
                              ,name     ='RST_BC_DATA_NORTH'            &  !<-- Name of 1-D string of north boundary values
                              ,count=NUM_WORDS_BC_NORTH             &  !<-- # of north boundary words on this fcst task
                              ,valueList=RST_BC_DATA_NORTH              &  !<-- The 1-D data being inserted into the Write import state
                              ,rc       =RC)

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      CALL ERR_MSG(RC,MESSAGE_CHECK,RC_SAVE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  Western boundary data to 1-D
!-----------------------------------------------------------------------
!
      IF(ITS==IDS)THEN                                                     !<-- Tasks on west boundary
        KOUNT=0
!
          DO JB=JTS,JTE
            DO IB=1,LNSV
              RST_BC_DATA_WEST(KOUNT+ 1)=PDBW(IB,JB,1)
              RST_BC_DATA_WEST(KOUNT+ 2)=PDBW(IB,JB,2)
              KOUNT=KOUNT+2
            ENDDO
          ENDDO

        DO L=1,LM
          DO JB=JTS,JTE
            DO IB=1,LNSV
              RST_BC_DATA_WEST(KOUNT+ 1)=TBW(IB,JB,L,1)
              RST_BC_DATA_WEST(KOUNT+ 2)=TBW(IB,JB,L,2)
              RST_BC_DATA_WEST(KOUNT+ 3)=QBW(IB,JB,L,1)
              RST_BC_DATA_WEST(KOUNT+ 4)=QBW(IB,JB,L,2)
              RST_BC_DATA_WEST(KOUNT+ 5)=WBW(IB,JB,L,1)
              RST_BC_DATA_WEST(KOUNT+ 6)=WBW(IB,JB,L,2)
              RST_BC_DATA_WEST(KOUNT+ 7)=UBW(IB,JB,L,1)
              RST_BC_DATA_WEST(KOUNT+ 8)=UBW(IB,JB,L,2)
              RST_BC_DATA_WEST(KOUNT+ 9)=VBW(IB,JB,L,1)
              RST_BC_DATA_WEST(KOUNT+10)=VBW(IB,JB,L,2)
              KOUNT=KOUNT+10
            ENDDO
          ENDDO
        ENDDO
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract Write Import State in SAVE_BC_DATA"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_StateGet(state      =EXP_STATE_SOLVER                 &  !<-- The Solver export state
                          ,itemName   ='Write Import State'             &  !<-- Name of the state to get from Solver export state
                          ,nestedState=IMP_STATE_WRITE                  &  !<-- Extract Write Component import state from Solver export
                          ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_SAVE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Set BC West Data Attribute in SAVE_BC_DATA"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeSet(state    =IMP_STATE_WRITE                &  !<-- The Write component import state
                              ,name     ='RST_BC_DATA_WEST'             &  !<-- Name of 1-D string of west boundary values
                              ,count=NUM_WORDS_BC_WEST              &  !<-- # of west boundary words on this fcst task
                              ,valueList=RST_BC_DATA_WEST               &  !<-- The 1-D data being inserted into the Write import state
                              ,rc       =RC)

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_SAVE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  Eastern boundary data to 1-D
!-----------------------------------------------------------------------
!
      IF(ITE==IDE)THEN                                                     !<-- Tasks on east boundary
        KOUNT=0
!
          DO JB=JTS,JTE
            DO IB=1,LNSV
              RST_BC_DATA_EAST(KOUNT+ 1)=PDBE(IB,JB,1)
              RST_BC_DATA_EAST(KOUNT+ 2)=PDBE(IB,JB,2)
              KOUNT=KOUNT+2
            ENDDO
          ENDDO

        DO L=1,LM
          DO JB=JTS,JTE
            DO IB=1,LNSV
              RST_BC_DATA_EAST(KOUNT+ 1)=TBE(IB,JB,L,1)
              RST_BC_DATA_EAST(KOUNT+ 2)=TBE(IB,JB,L,2)
              RST_BC_DATA_EAST(KOUNT+ 3)=QBE(IB,JB,L,1)
              RST_BC_DATA_EAST(KOUNT+ 4)=QBE(IB,JB,L,2)
              RST_BC_DATA_EAST(KOUNT+ 5)=WBE(IB,JB,L,1)
              RST_BC_DATA_EAST(KOUNT+ 6)=WBE(IB,JB,L,2)
              RST_BC_DATA_EAST(KOUNT+ 7)=UBE(IB,JB,L,1)
              RST_BC_DATA_EAST(KOUNT+ 8)=UBE(IB,JB,L,2)
              RST_BC_DATA_EAST(KOUNT+ 9)=VBE(IB,JB,L,1)
              RST_BC_DATA_EAST(KOUNT+10)=VBE(IB,JB,L,2)
              KOUNT=KOUNT+10
            ENDDO
          ENDDO
        ENDDO
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Extract Write Import State in SAVE_BC_DATA"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_StateGet(state      =EXP_STATE_SOLVER                 &  !<-- The Solver export state
                          ,itemName   ='Write Import State'             &  !<-- Name of the state to get from Solver export state
                          ,nestedState=IMP_STATE_WRITE                  &  !<-- Extract Write Component import state from Solver export
                          ,rc         =RC)
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_SAVE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        MESSAGE_CHECK="Set BC East Data Attribute in SAVE_BC_DATA"
!       CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        CALL ESMF_AttributeSet(state    =IMP_STATE_WRITE                &  !<-- The Write component import state
                              ,name     ='RST_BC_DATA_EAST'             &  !<-- Name of 1-D string of east boundary values
                              ,count=NUM_WORDS_BC_EAST              &  !<-- # of east boundary words on this fcst task
                              ,valueList=RST_BC_DATA_EAST               &  !<-- The 1-D data being inserted into the Write import state
                              ,rc       =RC)

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        CALL ERR_MSG(RC,MESSAGE_CHECK,RC_SAVE)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE SAVE_BC_DATA
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE PHYSICS_INITIALIZE(GFS                                 &
                                   ,SHORTWAVE                           &
                                   ,LONGWAVE                            &
                                   ,CONVECTION                          &
                                   ,MICROPHYSICS                        &
                                   ,SFC_LAYER                           &
                                   ,TURBULENCE                          &
                                   ,LAND_SURFACE                        &
                                   ,CO2TF                               &
                                   ,SBD,WBD                             &
                                   ,DPHD,DLMD                           &
                                   ,TPH0D,TLM0D                         &
                                   ,MY_DOMAIN_ID                        &
                                   ,MYPE                                &
                                   ,MPI_COMM_COMP                       &
                                   ,IDS,IDE,JDS,JDE,LM                  &
                                   ,IMS,IME,JMS,JME                     &
                                   ,ITS,ITE,JTS,JTE                     &
                                   ,RC)
!
!-----------------------------------------------------------------------
!
      USE MODULE_CONSTANTS,ONLY : A,CLIQ,CV,DTR,PI                      &
                                 ,RHOAIR0,RHOWATER,RHOSNOW
!
      USE MODULE_INIT_READ_BIN,ONLY : physics_read_gwd
!
!-----------------------------------------------------------------------
!***  Only for GFS physics
!-----------------------------------------------------------------------
!
      USE FUNCPHYS
      USE MERSENNE_TWISTER
      USE N_LAYOUT1,        ONLY : LATS_NODE_R,IPT_LATS_NODE_R
      USE TRACER_CONST,     ONLY : SET_TRACER_CONST
      USE DATE_DEF,         ONLY : FHOUR
      USE N_RESOL_DEF,      ONLY : LSOIL,LEVR,NXPT,JCAP,LEVS,NYPT       &
                                  ,JINTMX,THERMODYN_ID,SFCPRESS_ID      &
                                  ,NUM_P3D,NUM_P2D,NTOZ,NTCW,NCLD       &
                                  ,NMTVR,NFXR,LONR,LATR
!
      USE OZNE_DEF,         ONLY: LEVOZC,LATSOZP,BLATC,TIMEOZC,TIMEOZ   &
                                 ,KOZPL,LEVOZP,PL_TIME,PL_LAT,PL_PRES   &
                                 ,KOZC,DPHIOZC,LATSOZC,PL_COEFF
 
      USE N_NAMELIST_PHYSICS_DEF, ONLY: ISOL,ICO2,IALB,IEMS,IAER,ICTM   &
                                       ,IOVR_SW,IOVR_LW,LSSAV,LDIAG3D   &
                                       ,FHCYC,SASHAL,PRE_RAD,RAS,LSM    &
                                       ,CDMBGWD,DLQF,CTEI_RM,LGGFS3D    &
                                       ,BKGD_VDIF_M, SHAL_CNV           &
                                       ,BKGD_VDIF_H,BKGD_VDIF_S         &
                                       ,PSAUTCO,PRAUTCO,EVPCO           &
                                       ,CAL_PRE,MOM4ICE,MSTRAT          &
                                       ,TRANS_TRAC,NST_FCST             &
                                       ,MOIST_ADJ

!
!-----------------------------------------------------------------------
!***  For threading safe  (rad_initialize)
!-----------------------------------------------------------------------
!

      USE physpara,      ONLY : ISOLx,ICO2x,IALBx,IEMSx,IAERx,ICTMx     &
     &                         ,IOVR_SWx,IOVR_LWx,SASHALx               &
     &                         ,NTOZx,NTCWx,IFLIPx,IAER_MDL             &
     &                         ,NP3Dx, ISUBCSWx, ISUBCLWx               &
     &                         ,CRICK_PROOFx,CCNORMx,NORAD_PRECIPx

!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument variables
!------------------------
!
      INTEGER(kind=KINT),INTENT(IN) :: CO2TF                            &
                                      ,MPI_COMM_COMP                    &
                                      ,MY_DOMAIN_ID                     &
                                      ,MYPE
!
      INTEGER(kind=KINT),INTENT(IN) :: IDS,IDE,JDS,JDE,LM               &
                                      ,IMS,IME,JMS,JME                  &
                                      ,ITS,ITE,JTS,JTE
!
      REAL(kind=KFPT),INTENT(INOUT) :: DLMD,DPHD                        &
                                      ,TPH0D,TLM0D                      &
                                      ,SBD,WBD
!
      LOGICAL,INTENT(IN) :: GFS
!
      CHARACTER(99),INTENT(IN) :: CONVECTION,LONGWAVE,MICROPHYSICS      &
                                 ,SFC_LAYER,SHORTWAVE,TURBULENCE        &
                                 ,LAND_SURFACE
!
      INTEGER(kind=KINT),INTENT(OUT) :: RC
!
!---------------------
!***  Local variables
!---------------------
!
      INTEGER :: I,I_HI,I_LO,IHRST,IRTN,J,J_HI,J_LO,JULDAY,JULYR        &
                ,K,KFLIP,L,LPT2,N,NFCST,NRECS_SKIP_FOR_PT               &
                ,NSOIL,NSTEPS_PER_HOUR,NTIMESTEP
!
      INTEGER :: LDIM1,LDIM2,UDIM1,UDIM2
!
      INTEGER,DIMENSION(3) :: IDAT
!
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: ITEMP,LOWLYR
!
      REAL :: SECOND_FCST
!
      REAL :: SWRAD_SCAT=1.
!
      REAL :: DELX,DELY,DPH,DT,DT_MICRO,DTPHS                           &
             ,GMT,JULIAN,PDBOT,PDTOP,PDTOT,PT_CB,RELM,RPDTOT            &
             ,SB,THETA_HALF,TPV,XTIME
!
      REAL,DIMENSION(LM) :: DSG1,DSG2,PDSG1,PSGML1,SGML1,SGML2
      REAL,DIMENSION(LM+1) :: PSG1,SG1,SG2,SGM                          &
                             ,SFULL,SFULL_FLIP,SMID,SMID_FLIP
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: EMISS
      REAL,DIMENSION(:,:),ALLOCATABLE :: TEMP1,TEMP_GWD
      REAL,DIMENSION(:,:,:),ALLOCATABLE :: TEMPSOIL
      REAL,DIMENSION(NUM_SOIL_LAYERS)   :: SOIL1DIN
!
      CHARACTER(LEN=256) :: INFILE
!
      LOGICAL,SAVE :: ALLOWED_TO_READ=.TRUE.
      LOGICAL :: OPENED
!
!---------------------------------
!***  GFS physics local variables
!---------------------------------
!
      CHARACTER(80)   :: GFS_PHY_NAMELIST
      INTEGER         :: JDAT(8),NLUNIT,NTRAC,IRET,IMJM,NIJ
      REAL(kind=KDBL) :: DELTIM,GAUL

      REAL *4              :: BLATC4
      REAL *4, ALLOCATABLE :: PL_LAT4(:), PL_PRES4(:), PL_TIME4(:), TEMPIN(:)

      REAL(kind=KDBL),DIMENSION(:),ALLOCATABLE ::                             &
                     SIG1T, RLA, RLO, SLMASK, OROG, AISFCS,                   &
                     SIHFCS, SICFCS, SITFCS, SWDFCS, VMNFCS, VMXFCS, SLPFCS,  &
                     ABSFCS, TSFFCS, SNOFCS, ZORFCS, TG3FCS, CNPFCS, SLIFCS,  &
                     F10MFCS, VEGFCS, VETFCS, SOTFCS, CVFCS, CVBFCS, CVTFCS
!
      REAL(kind=KDBL),DIMENSION(:,:),ALLOCATABLE :: ALFFC1              &
                                                   ,SMCFC1              &
                                                   ,STCFC1              &
                                                   ,SLCFC1              &
                                                   ,ALBFC1
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  Initialize allocated arrays
!-----------------------------------------------------------------------
!
      NSOIL=NUM_SOIL_LAYERS                                              !<-- From Landsurface module
!
      IF(GFS)THEN

        int_state%SOLCON=0.0D0
        int_state%SLAG  =0.0D0
        int_state%SDEC  =0.0D0
        int_state%CDEC  =0.0D0
!
        DO J=JTS,JTE
          int_state%DDY   (J)=0.0D0
          int_state%JINDX1(J)=0
          int_state%JINDX2(J)=0
        ENDDO
!
        DO J=JMS,JME
        DO I=IMS,IME
!
          int_state%DUGWD  (I,J)=0.0D0
          int_state%DVGWD  (I,J)=0.0D0
!
          int_state%TMPMIN (I,J)=373.0D0
          int_state%TMPMAX (I,J)=173.0D0
!
          int_state%SHDMIN (I,J)=0.0
          int_state%SHDMAX (I,J)=0.0
!
          int_state%SFALB  (I,J)=0.0D0
          int_state%TSFLW  (I,J)=0.0D0
          int_state%SEMIS  (I,J)=0.0D0
          int_state%SFCDLW (I,J)=0.0D0
          int_state%SFCDSW (I,J)=0.0D0
          int_state%SFCNSW (I,J)=0.0D0
          int_state%ZORFCS (I,J)=-1.D6
          int_state%SIHFCS (I,J)=-1.D6
          int_state%SICFCS (I,J)=-1.D6
          int_state%SLPFCS (I,J)=-1.D6
          int_state%TG3FCS (I,J)=-1.D6
          int_state%VEGFCS (I,J)=-1.D6
          int_state%VETFCS (I,J)=-1.D6
          int_state%SOTFCS (I,J)=-1.D6
!
          DO N=1,4
            int_state%ALBFC1(I,J,N)=-1.D6
          ENDDO
!
          DO N=1,2
            int_state%ALFFC1(I,J,N)=-1.D6
          ENDDO
!
          DO N=1,3                                 ! for Zhao =3, Ferr=1
            int_state%PHY_F2DV (I,J,N)=0.0D0
          ENDDO
!
          DO N=1,4                                 ! for Zhao =4, Ferr=3
          DO L=1,LM
            int_state%PHY_F3DV (I,J,L,N)=0.0D0
          ENDDO
          ENDDO
!
        ENDDO
        ENDDO

        rewind (kozpl)
        read   (kozpl) pl_coeff, latsozp, levozp, timeoz

        DO N=1,TIMEOZ
        DO L=1,PL_COEFF
        DO J=1,LEVOZP
        DO I=1,LATSOZP
          int_state%OZPLIN(I,J,L,N)=-1.D6
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!***  Dereference the start time.
!-----------------------------------------------------------------------
!
      START_YEAR=int_state%START_YEAR
      START_MONTH=int_state%START_MONTH
      START_DAY=int_state%START_DAY
      START_HOUR=int_state%START_HOUR
      START_MINUTE=int_state%START_MINUTE
      START_SECOND=int_state%START_SECOND
      DT=int_state%DT
!
!-----------------------------------------------------------------------
!***  Radiation needs some specific time quantities.
!-----------------------------------------------------------------------
!
      CALL TIME_MEASURE(START_YEAR,START_MONTH,START_DAY,START_HOUR     &
                       ,START_MINUTE,START_SECOND                       &
                       ,NTIMESTEP,DT                                    &
                       ,JULDAY,JULYR,JULIAN,XTIME)
!
!-----------------------------------------------------------------------
!***  Open and read GWD data file (14 orography fields)
!-----------------------------------------------------------------------
!
      gwd_read: IF(int_state%GWDFLG) THEN
!
        select_GWD_unit: DO N=51,59
          INQUIRE(N,OPENED=OPENED)
          IF(.NOT.OPENED)THEN
            NFCST=N
            EXIT select_GWD_unit
          ENDIF
        ENDDO select_GWD_unit
!
        WRITE(INFILE,'(A,I2.2)')'GWD_bin_',MY_DOMAIN_ID
!
!-----------------------------------------------------------------------
!
        CALL PHYSICS_READ_GWD(INFILE,NFCST,INT_STATE                    &
                             ,MYPE,MPI_COMM_COMP                        &
                             ,IDS,IDE,JDS,JDE,RC)
!
        IF (RC /= 0) THEN
          RETURN
        ENDIF
!
!-----------------------------------------------------------------------
!
      ENDIF gwd_read
!
!-----------------------------------------------------------------------
!
      PT_CB=int_state%PT*1.0E-3   !<-- Convert pascals to centibars for GFDL initialization
!
!-----------------------------------------------------------------------
!***  Make up a potential skin temperature.
!-----------------------------------------------------------------------
!
      IF(.NOT.int_state%RESTART) THEN
!
        DO J=JTS,JTE
        DO I=ITS,ITE
          int_state%THS(I,J)=int_state%TSKIN(I,J)                       &
                       *(100000./(int_state%SG2(LM+1)*int_state%PD(I,J) &
                                 +int_state%PSG1(LM+1)))**CAPPA
        ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!*** Initializing TLMAX, TLMIN
!-----------------------------------------------------------------------
!
      DO J=JTS,JTE
        DO I=ITS,ITE
          int_state%TLMAX(I,J)=int_state%T(I,J,1)
          int_state%TLMIN(I,J)=int_state%T(I,J,1)
       ENDDO
     ENDDO
!
!-----------------------------------------------------------------------
!***  Recreate sigma values at layer interfaces for the full vertical
!***  domain. 
!-----------------------------------------------------------------------
!
      DO L=1,LM+1
        SFULL(L)=int_state%SGM(L)
      ENDDO
!
      DO L=1,LM
        SMID(L)=(SFULL(L)+SFULL(L+1))*0.5
      ENDDO
!
      SMID(LM+1)=-9999999.
!
!-----------------------------------------------------------------------
!***  The radiative emissivity
!-----------------------------------------------------------------------
!
      DO J=JMS,JME
      DO I=IMS,IME
        EMISS(I,J)=1.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  Choose a J index for an "average" DX.
!***  Select the J that divides the domain's area in half.
!-----------------------------------------------------------------------
!
      SB=int_state%SBD*DTR
      DPH=int_state%DPHD*DTR
!!!   THETA_HALF=ASIN(0.5*SIN(-SB))
      THETA_HALF=0.
      JC=NINT(0.5*(JDE-JDS+1)+THETA_HALF/DPH)
!
!-----------------------------------------------------------------------
!***  Set time variables needed for history output.
!-----------------------------------------------------------------------
!
      NSTEPS_PER_HOUR=NINT(3600./int_state%DT)
      int_state%NPREC=NSTEPS_PER_HOUR*int_state%NHRS_PREC
      int_state%NCLOD=NSTEPS_PER_HOUR*int_state%NHRS_CLOD
      int_state%NHEAT=NSTEPS_PER_HOUR*int_state%NHRS_HEAT
      int_state%NRDLW=NSTEPS_PER_HOUR*int_state%NHRS_RDLW
      int_state%NRDSW=NSTEPS_PER_HOUR*int_state%NHRS_RDSW
      int_state%NSRFC=NSTEPS_PER_HOUR*int_state%NHRS_SRFC
!
!-----------------------------------------------------------------------
!***  If this is a restarted run frim timestep 0 then zero out
!***  the accumulated precip since they pass through the analysis
!***  with nonzero values from the first guess.
!-----------------------------------------------------------------------
!
      IF(int_state%RST_OUT_00)THEN
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%ACPREC(I,J)=0.
          int_state%CUPREC(I,J)=0.
        ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!***  Finally initialize individual schemes.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  The GFS physics suite is considered a single package here.
!----------------------------------------------------------------------
!
      package: IF(GFS)THEN
!
!-----------------------------------------------------------------------
!***  The GFS physics suite is considered a single package here.
!----------------------------------------------------------------------
!
      namelist_unit: DO N=101,151
        INQUIRE(N,OPENED=OPENED)
        IF(.NOT.OPENED)THEN
          NLUNIT=N
          EXIT namelist_unit
        ENDIF
      ENDDO namelist_unit

      GFS_PHY_NAMELIST = 'atm_namelist'
      DELTIM           = int_state%DT
      LEVS             = LM
      CALL N_COMPNS_PHYSICS(DELTIM,    IRET,                 &
                   NTRAC,   NXPT,    NYPT,  JINTMX,          &
                   JCAP,    LEVS,    LEVR,    LONR,   LATR,  &
                   NTOZ,    NTCW,    NCLD,                   &
                   LSOIL,   NMTVR,   NUM_P3D, NUM_P2D,       &
                   THERMODYN_ID,     SFCPRESS_ID,            &
                   NLUNIT,  MYPE,    GFS_PHY_NAMELIST)
!--------------
        IALB            = 0
        DELTIM          = int_state%DT
        LONR            = ITE-ITS+1  ! this is changed in compns_physics from
        LATR            = JTE-JTS+1  ! atm_namelist (restore it back)
        LATS_NODE_R     = JTE-JTS+1
        IPT_LATS_NODE_R = 1
        NFXR            = 39
        LSSAV           = .TRUE.   ! logical flag for store 3-d cloud field
        LDIAG3D         = .FALSE.  ! logical flag for store 3-d diagnostic fields
        LGGFS3D         = .FALSE.
!--------------
      CALL SET_SOILVEG(MYPE,NLUNIT)
      CALL SET_TRACER_CONST(NTRAC,MYPE,NLUNIT)
      CALL GFUNCPHYS
!
!-----------------------------------------------------------------------
!***  Initialize ozone
!-----------------------------------------------------------------------
     IF( NTOZ .LE. 0 ) THEN        ! Diagnostic ozone
!
!!        rewind (kozc)            ! there is no header in global_o3clim.txt file
!!        read   (kozc,end=101) latsozc, levozc, timeozc, blatc4
!!101     if (levozc .lt. 10 .or. levozc .gt. 100) then
!!          rewind (kozc)
            levozc  = 17
            latsozc = 18
            blatc   = -85.0
!!        else
!!          blatc   = blatc4
!!        endif
          latsozp   = 2
          levozp    = 1
          timeoz    = 1
          pl_coeff  = 1  !!!  0 (MUST set > 0, used in GBPHYS for allocation)
          timeozc   = 12 !!!  this is not in header
!
     ELSE                          ! Prognostic Ozone
!
         rewind (kozpl)
         read   (kozpl) pl_coeff, latsozp, levozp, timeoz
       IF(.NOT.ALLOCATED(pl_lat))THEN
         allocate (pl_lat (latsozp), pl_pres (levozp),pl_time (timeoz+1))
       ENDIF
       IF(.NOT.ALLOCATED(pl_lat4))THEN
         allocate (pl_lat4(latsozp), pl_pres4(levozp),pl_time4(timeoz+1))
       ENDIF
       IF(.NOT.ALLOCATED(tempin)) allocate (tempin(latsozp))
         rewind (kozpl)
         read (kozpl) pl_coeff, latsozp, levozp, timeoz, pl_lat4, pl_pres4, pl_time4
         pl_pres(:) = pl_pres4(:)
         pl_lat(:)  = pl_lat4(:)
         pl_time(:) = pl_time4(:)
!
         DO J=JTS,JTE

           gaul=int_state%GLAT( (ITS+ITE)/2 ,J)*180.0d0/3.14159d0

           int_state%jindx2(j) = latsozp + 1
           do i=1,latsozp
             if (gaul.lt. pl_lat(i)) then
               int_state%jindx2(j) = i
               exit
             endif
           enddo
           int_state%jindx1(j) = max(int_state%jindx2(j)-1,1)
           int_state%jindx2(j) = min(int_state%jindx2(j),latsozp)

           if (int_state%jindx2(j) .ne. int_state%jindx1(j)) then
             int_state%ddy(j) = (gaul                        - pl_lat(int_state%jindx1(j)))  &
                              / (pl_lat(int_state%jindx2(j)) - pl_lat(int_state%jindx1(j)))
           else
             int_state%ddy(j) = 1.0
           endif

         ENDDO
!
         DO I=1,TIMEOZ
           DO N=1,PL_COEFF
             DO K=1,LEVOZP
               READ(KOZPL) TEMPIN
               int_state%OZPLIN(:,K,N,I) = TEMPIN(:)
             ENDDO
           ENDDO
         ENDDO
!
     ENDIF                          ! Diagnostic/Prognostic Ozone
!
        dphiozc = -(blatc+blatc)/(latsozc-1)
!-----------------------------------------------------------------------
!***  End initialization  of ozone
!-----------------------------------------------------------------------
!
       IMJM=(ITE-ITS+1)*(JTE-JTS+1)
       ALLOCATE(SIG1T(IMJM),RLA(IMJM),RLO(IMJM),SLMASK(IMJM),OROG(IMJM)   &
               ,AISFCS(IMJM),SIHFCS(IMJM),SICFCS(IMJM),SITFCS(IMJM)       &
               ,SWDFCS(IMJM),VMNFCS(IMJM),VMXFCS(IMJM),SLPFCS(IMJM)       &
               ,ABSFCS(IMJM),TSFFCS(IMJM),SNOFCS(IMJM),ZORFCS(IMJM)       &
               ,TG3FCS(IMJM),CNPFCS(IMJM),SLIFCS(IMJM),F10MFCS(IMJM)      &
               ,VEGFCS(IMJM),VETFCS(IMJM),SOTFCS(IMJM),CVFCS(IMJM)        &
               ,CVBFCS(IMJM),CVTFCS(IMJM),ALFFC1(IMJM,2),ALBFC1(IMJM,4)   &
               ,SMCFC1(IMJM,NSOIL),STCFC1(IMJM,NSOIL),SLCFC1(IMJM,NSOIL) )

       SIHFCS  = 0.0d0
       SICFCS  = 0.0d0
       SITFCS  = 0.0d0
       SWDFCS  = 0.0d0
       VMNFCS  = 0.0d0
       VMXFCS  = 0.0d0
       SLPFCS  = 0.0d0
       ABSFCS  = 0.0d0
       TSFFCS  = 0.0d0
       SNOFCS  = 0.0d0
       ZORFCS  = 0.0d0
       TG3FCS  = 0.0d0
       CNPFCS  = 0.0d0
       SLIFCS  = 0.0d0
       F10MFCS = 0.0d0
       VEGFCS  = 0.0d0
       VETFCS  = 0.0d0
       SOTFCS  = 0.0d0
       CVFCS   = 0.0d0
       CVBFCS  = 0.0d0
       CVTFCS  = 0.0d0
       ALFFC1  = 0.0d0
       ALBFC1  = 0.0d0
       SMCFC1  = 0.0d0
       STCFC1  = 0.0d0
       SLCFC1  = 0.0d0


       JDAT(1)=int_state%IDAT(3)
       JDAT(2)=int_state%IDAT(2)
       JDAT(3)=int_state%IDAT(1)
       JDAT(4)=0
       JDAT(5)=int_state%IHRST
       JDAT(6)=0
       JDAT(7)=0
       JDAT(8)=0
       FHOUR=FLOAT(int_state%IHRST)
!
    NIJ=0
    DO J=JTS,JTE
      DO I=ITS,ITE

       NIJ=NIJ+1

       SIG1T(NIJ)    = 0.d0
       RLA(NIJ)      = int_state%GLAT(I,J)*180.0d0/3.14159d0
       RLO(NIJ)      = int_state%GLON(I,J)*180.0d0/3.14159d0
       SLMASK(NIJ)   = 1.0d0-int_state%SM(I,J)
       OROG(NIJ)     = int_state%FIS(I,J)/9.81d0
       AISFCS(NIJ)   = int_state%SICE(I,J)

      ENDDO
    ENDDO
!
      CALL SFCCYCLE(204,(ITE-ITS+1)*(JTE-JTS+1),4,SIG1T,FHCYC              &
     &,             JDAT(1), JDAT(2), JDAT(3), JDAT(5), FHOUR              &
!    &,             RLA, RLO, SLMASK, OROG                                 &
     &,             RLA, RLO, SLMASK, OROG, OROG, .FALSE.                  &
     &,             SIHFCS,   SICFCS, SITFCS                               &
     &,             SWDFCS,   SLCFC1                                       &
     &,             VMNFCS,   VMXFCS, SLPFCS, ABSFCS                       &
     &,             TSFFCS,   SNOFCS, ZORFCS, ALBFC1, TG3FCS               &
     &,             CNPFCS,   SMCFC1, STCFC1, SLIFCS, AISFCS, F10MFCS      &
     &,             VEGFCS,   VETFCS, SOTFCS, ALFFC1                       &
     &,             CVFCS,    CVBFCS, CVTFCS, MYPE, NLUNIT, IALB)
!
     NIJ=0
     DO J=JTS,JTE
       DO I=ITS,ITE
        NIJ=NIJ+1

        SIHFCS(NIJ) = int_state%SICE(I,J) * 1.0d0  ! initialize like this
        SICFCS(NIJ) = int_state%SICE(I,J) * 0.9d0  ! initialize like this

        int_state%ZORFCS(I,J)   = ZORFCS(NIJ)
        int_state%SIHFCS(I,J)   = SIHFCS(NIJ)
        int_state%SICFCS(I,J)   = SICFCS(NIJ)
        int_state%SLPFCS(I,J)   = SLPFCS(NIJ)
        int_state%TG3FCS(I,J)   = TG3FCS(NIJ)
        int_state%VEGFCS(I,J)   = VEGFCS(NIJ)
        int_state%VETFCS(I,J)   = VETFCS(NIJ)
        int_state%SOTFCS(I,J)   = SOTFCS(NIJ)

        int_state%ALBFC1(I,J,1) = ALBFC1(NIJ,1)
        int_state%ALBFC1(I,J,2) = ALBFC1(NIJ,2)
        int_state%ALBFC1(I,J,3) = ALBFC1(NIJ,3)
        int_state%ALBFC1(I,J,4) = ALBFC1(NIJ,4)

        int_state%ALFFC1(I,J,1) = ALFFC1(NIJ,1)
        int_state%ALFFC1(I,J,2) = ALFFC1(NIJ,2)

       ENDDO
     ENDDO

     DEALLOCATE(SIG1T,RLA,RLO,SLMASK,OROG     &
               ,AISFCS,SIHFCS,SICFCS,SITFCS   &
               ,SWDFCS,VMNFCS,VMXFCS,SLPFCS   &
               ,ABSFCS,TSFFCS,SNOFCS,ZORFCS   &
               ,TG3FCS,CNPFCS,SLIFCS,F10MFCS  &
               ,VEGFCS,VETFCS,SOTFCS,CVFCS    &
               ,CVBFCS,CVTFCS,ALFFC1,ALBFC1   &
               ,SMCFC1,STCFC1,SLCFC1)
!
!----------------------------------------------------------------------
!***  Set fluxes to zero
!----------------------------------------------------------------------
!
             int_state%ALWIN(I,J)         = 0.
             int_state%ALWOUT(I,J)        = 0.
             int_state%ASWIN(I,J)         = 0.
             int_state%ASWOUT(I,J)        = 0.
             int_state%RLWIN(I,J)         = 0.
             int_state%RADOT(I,J)         = 0.
             int_state%RSWIN(I,J)         = 0.
             int_state%RSWOUT(I,J)        = 0.

             int_state%ALWTOA(I,J)        = 0.
             int_state%ASWTOA(I,J)        = 0.
             int_state%RLWTOA(I,J)        = 0.
             int_state%RSWTOA(I,J)        = 0.

             int_state%SFCSHX(I,J)        = 0.
             int_state%SFCLHX(I,J)        = 0.
             int_state%TWBS(I,J)          = 0.
             int_state%QWBS(I,J)          = 0.

             int_state%BGROFF(I,J)        = 0.
             int_state%SSROFF(I,J)        = 0.
             int_state%ACSNOW(I,J)        = 0.

             int_state%CUPPT(I,J)         = 0.
!
!----------------------------------------------------------------------
!***  End GFS package init
!----------------------------------------------------------------------
!
      ELSE
!
!----------------------------------------------------------------------
!***  If not selecting the GFS suite, each of the physics groups is
!***  treated individually.
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  Longwave radiation
!----------------------------------------------------------------------
!
        SELECT CASE (longwave)  
          CASE ('gfdl')
!
!***  We are calling a WRF routine thus flip the vertical.
!
            DO K=1,LM
              KFLIP=LM+1-K
              SFULL_FLIP(KFLIP)=SFULL(K+1)
              SMID_FLIP(KFLIP)=SMID(K)
            ENDDO
            SFULL_FLIP(LM+1)=SFULL(1)
!
            GMT=REAL(START_HOUR)
            CALL GFDL_INIT(EMISS,SFULL_FLIP,SMID_FLIP,PT_CB            &
                          ,JULYR,START_MONTH,START_DAY,GMT             &
                          ,CO2TF                                       &
                          ,IDS,IDE,JDS,JDE,1,LM+1                      &
                          ,IMS,IME,JMS,JME,1,LM+1                      &
                          ,ITS,ITE,JTS,JTE,1,LM)
          CASE ('rrtm')

            CALL GPKAP    ! for ozone by using the unified RRTM from GFS
            CALL GPVS     ! for aerosol by using the unified RRTM from GFS

!-----------------------------------------------------------------------
!***  Initialize ozone
!-----------------------------------------------------------------------

!OZONE CLIMATOLOGY
!
! there is no header in global_o3clim.txt file

            IF (NTOZx .LE. 0) THEN     ! DIAGNOSTIC OZONE, ONLY THIS ONE WORKS
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

!-----------------------------------------------------------------------
!***  End initialization  of ozone
!-----------------------------------------------------------------------

!==========================================================================
!  Similar to GFS "GFS_Initialize_ESMFMod.f" line #1103
!==========================================================================

            call rad_initialize                                        &
!        ---  inputs:
     &       ( SFULL,LM,ICTMx,ISOLx,ICO2x,IAERx,IAER_MDL,IALBx,IEMSx,  &
     &         NTCWx,NP3Dx,NTOZx,IOVR_SWx,IOVR_LWx,ISUBCSWx,ISUBCLWx,  &
     &         SASHALx,CRICK_PROOFx,CCNORMx,NORAD_PRECIPx,IFLIPx,MYPE )
!  ---        outputs:
!                ( none )

!==========================================================================
!==========================================================================


            DO K=1,LM
              KFLIP=LM+1-K
              SFULL_FLIP(KFLIP)=SFULL(K+1)
              SMID_FLIP(KFLIP)=SMID(K)
            ENDDO
            SFULL_FLIP(LM+1)=SFULL(1)
!
            GMT=REAL(START_HOUR)


!==========================================================================
! This following "RRTM_INIT" is only a L,M,H  DIAGNOSTIC cloud.
! It is not a real RRTM initialization
!==========================================================================


            CALL RRTM_INIT(EMISS,SFULL_FLIP,SMID_FLIP,PT_CB            &
                          ,JULYR,START_MONTH,START_DAY,GMT             &
                          ,CO2TF                                       &
                          ,IDS,IDE,JDS,JDE,1,LM+1                      &
                          ,IMS,IME,JMS,JME,1,LM+1                      &
                          ,ITS,ITE,JTS,JTE,1,LM)
!
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF LONGWAVE SCHEME: INIT '
        END SELECT
!
!----------------------------------------------------------------------
!***  Shortwave radiation
!----------------------------------------------------------------------
!
        SELECT CASE (shortwave)
          CASE ('gfdl')
!           WRITE(0,*)' Already called GFDL_INIT from LONGWAVE'
          CASE ('rrtm')
!           WRITE(0,*)' Already called RRTM_INIT from LONGWAVE'
!!!       CASE ('gsfc')
!!!         CALL GSFC_INIT
          CASE ('dudh')
!!!         CALL SWINIT(SWRAD_SCAT,int_state%RESTART                   &
!!!                    ,ALLOWED_TO_READ                                &
!!!                    ,IDS,IDE,JDS,JDE,1,LM+1                         &
!!!                    ,IMS,IME,JMS,JME,1,LM+1                         &
!!!                    ,ITS,ITE,JTS,JTE,1,LM)
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF SHORTWAVE SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!***  Surface layer
!----------------------------------------------------------------------
!
        ALLOCATE(LOWLYR(IMS:IME,JMS:JME),STAT=I)
!
        SELECT CASE (sfc_layer)
          CASE ('myj')
            CALL JSFC_INIT(LOWLYR                                      &  !<-- Placeholder (computed in TURBULENCE)
                          ,int_state%USTAR,int_state%Z0                &
                          ,int_state%SM,int_state%SICE                 &
                          ,int_state%IVGTYP,int_state%RESTART          &            
                          ,ALLOWED_TO_READ                             &
                          ,IDS,IDE,JDS,JDE,1,LM+1                      &
                          ,IMS,IME,JMS,JME,1,LM+1                      &
                          ,ITS,ITE,JTS,JTE,1,LM                        &
                          ,MPI_COMM_COMP )
!!!       CASE ('mm5')
!!!         CALL SFCLYR_INIT
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF SURFACE LAYER SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!***  Turbulence
!----------------------------------------------------------------------
!
        SELECT CASE (turbulence)
          CASE ('myj')
            CALL MYJPBL_INIT(int_state%EXCH_H,int_state%RESTART        &
                            ,IDS,IDE,JDS,JDE,LM                        &
                            ,IMS,IME,JMS,JME                           &
                            ,ITS,ITE,JTS,JTE)
          CASE ('gfs')
!!!       CASE ('ysu')
!!!         CALL YSU_INIT
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF TURBULENCE SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!***  Land surface
!----------------------------------------------------------------------
!
        SELECT CASE (land_surface)
          CASE ('noah')

          CALL NOAH_LSM_INIT(int_state%CMC,     int_state%ISLTYP       &
                            ,int_state%STC,     int_state%SMC          &
                            ,int_state%IVEGSRC                         &
                            ,int_state%SH2O,    NUM_SOIL_LAYERS        &
                            ,int_state%RESTART, ALLOWED_TO_READ        &
                            ,IDS,IDE, JDS,JDE                          &
                            ,IMS,IME, JMS,JME                          &
                            ,ITS,ITE, JTS,JTE                          &
                            ,MYPE,MPI_COMM_COMP )
          CASE ('liss')

!!!         CALL LSM_INIT

          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF LAND SURFACE SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!****  Convection
!----------------------------------------------------------------------
!
        SELECT CASE (convection)
          CASE ('bmj')
            int_state%CU_PHYSICS=2
            CALL BMJ_INIT(int_state%CLDEFI,int_state%RESTART &
                         ,a2,a3,a4,cappa,cp &
                         ,pq0,r_d &
                         ,IDS,IDE,JDS,JDE &
                         ,IMS,IME,JMS,JME &
                         ,ITS,ITE,JTS,JTE,LM)

          CASE ('sas')
            int_state%CU_PHYSICS=4
            CALL SAS_INIT
!
          CASE('kf')
            int_state%CU_PHYSICS=1
!           CALL KF_INIT
          CASE ('gd')
            int_state%CU_PHYSICS=3
!           CALL GD_INIT
          CASE ('none')
!           WRITE(0,*)' User has chosen to run with no parameterized convection.'
          CASE DEFAULT
             WRITE(0,*)' BAD SELECTION OF CONVECTION SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!***  Microphysics
!----------------------------------------------------------------------
!
        SELECT CASE (microphysics)
!
          CASE ('fer')
            DT_MICRO=int_state%NPRECIP*DT
            DELX=-2.*int_state%WBD*111.3/REAL(int_state%IM) !DX at rotated equator (km)
            DELY=-2.*int_state%SBD*111.3/REAL(int_state%JM) !DY at rotated equator (km)
!
            CALL FERRIER_INIT(DT_MICRO,DT,DELX,DELY,int_state%RESTART  &
                             ,int_state%F_ICE                          &
                             ,int_state%F_RAIN                         &
                             ,int_state%F_RIMEF                        &
                             ,int_state%MP_RESTART_STATE               &
                             ,int_state%TBPVS_STATE                    &
                             ,int_state%TBPVS0_STATE                   &
                             ,ALLOWED_TO_READ                          &
                             ,IDS,IDE,JDS,JDE,1,LM+1                   &
                             ,IMS,IME,JMS,JME,1,LM                     &
                             ,ITS,ITE,JTS,JTE,1,LM                     &
                             ,MPI_COMM_COMP,MYPE,int_state%MASSRout    &
                             ,int_state%MASSIout)
!
          CASE ('fer_hires')
            DT_MICRO=int_state%NPRECIP*DT
            DELX=-2.*int_state%WBD*111.3/REAL(int_state%IM) !DX at rotated equator (km)
            DELY=-2.*int_state%SBD*111.3/REAL(int_state%JM) !DY at rotated equator (km)
!
            CALL FERRIER_INIT_HR(DT_MICRO,DT,DELX,DELY,int_state%RESTART  &
                                ,int_state%F_ICE                          &
                                ,int_state%F_RAIN                         &
                                ,int_state%F_RIMEF                        &
                                ,int_state%MP_RESTART_STATE               &
                                ,int_state%TBPVS_STATE                    &
                                ,int_state%TBPVS0_STATE                   &
                                ,ALLOWED_TO_READ                          &
                                ,IDS,IDE,JDS,JDE,1,LM+1                   &
                                ,IMS,IME,JMS,JME,1,LM                     &
                                ,ITS,ITE,JTS,JTE,1,LM                     &
                                ,MPI_COMM_COMP,MYPE,int_state%MASSRout    &
                                ,int_state%MASSIout)

!
          CASE ('gfs')
             CALL GFSMP_INIT

          CASE ('wsm6')
             CALL WSM6INIT(RHOAIR0,RHOWATER,RHOSNOW,CLIQ,CV             &
                          ,ALLOWED_TO_READ )
!!!       CASE ('kes')
!!!         CALL KESSLER_INIT
!!!       CASE ('tho')
!!!         CALL THOMPSON_INIT
          CASE DEFAULT
            WRITE(0,*)' BAD SELECTION OF MICROPHYSICS SCHEME: INIT'
        END SELECT
!
!----------------------------------------------------------------------
!****  Gravity wave drag (GWD) & mountain blocking (MB) initialization
!----------------------------------------------------------------------
!
        DTPHS=int_state%DT*int_state%NPHS
!
        CALL GWD_init(DTPHS,int_state%RESTART                           &
                      ,int_state%TPH0D,int_state%TLM0D                  &
                      ,int_state%GLAT,int_state%GLON                    &
                      ,int_state%CROT,int_state%SROT,int_state%HANGL    &
                      ,IDS,IDE,JDS,JDE                                  &
                      ,IMS,IME,JMS,JME                                  &
                      ,ITS,ITE,JTS,JTE,LM)
!
! uncomment this for output in future
!
!       IF(.NOT.int_state%RESTART)THEN
!         DO J=JMS,JME
!         DO I=IMS,IME
!           UGWDsfc(I,J)=0.
!           VGWDsfc(I,J)=0.
!         ENDDO
!         ENDDO
!       ENDIF
!
!
!----------------------------------------------------------------------
!
      ENDIF package
!
!----------------------------------------------------------------------
!
      END SUBROUTINE PHYSICS_INITIALIZE
!

!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE UPDATE_WATER(CWM,F_ICE,F_RAIN,F_RIMEF                  &
                             ,NUM_WATER,WATER,T                         &
                             ,P_QC,P_QR,P_QS,P_QI,P_QG                  &
                             ,MICROPHYSICS,SPEC_ADV,NTIMESTEP           &
                             ,IDS,IDE,JDS,JDE,LM                        &
                             ,IMS,IME,JMS,JME                           &
                             ,ITS,ITE,JTS,JTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    UPDATE_WATER          UPDATE WATER ARRAY
!   PRGRMMR: FERRIER         ORG: NP22     DATE: 3 AUG 2009
!
! ABSTRACT:
!     UPDATE WATER ARRAY FOR FERRIER MICROPHYSICS
!
! PROGRAM HISTORY LOG (with changes to called routines) :
!   2009-08     FERRIER     - Synchronize WATER array with CWM, F_rain, F_ice arrays
!
! USAGE: CALL UPDATE_WATER FROM PHY_RUN
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBMP6
!-----------------------------------------------------------------------
      USE MODULE_CONSTANTS,ONLY : EPSQ,TIW
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!
!----------------------
!-- Argument Variables
!----------------------
!
      INTEGER,INTENT(IN) :: NUM_WATER,NTIMESTEP                         &
                           ,P_QC,P_QR,P_QS,P_QI,P_QG                    &
!
                           ,IDS,IDE,JDS,JDE,LM                          &
                           ,IMS,IME,JMS,JME                             &
                           ,ITS,ITE,JTS,JTE
!
      CHARACTER(99),INTENT(IN) :: MICROPHYSICS
!
      LOGICAL,INTENT(IN) :: SPEC_ADV
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(INOUT) :: CWM         &
                                                           ,F_ICE       &
                                                           ,F_RAIN      &
                                                           ,F_RIMEF     &
                                                           ,T
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM,NUM_WATER),INTENT(INOUT) :: WATER
!
!--------------------
!--  Local Variables
!--------------------
!
      INTEGER :: I,J,K
      REAL :: FRACTION, LIQW, OLDCWM
      LOGICAL :: CLD_INIT
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      IF(NTIMESTEP<=1)THEN
        CLD_INIT=.TRUE.
        DO K=1,LM
         DO J=JMS,JME
          DO I=IMS,IME
            IF (P_QC==1) WATER(I,J,K,P_QC)=0.0                          
            IF (P_QR==1) WATER(I,J,K,P_QR)=0.0                          
            IF (P_QI==1) WATER(I,J,K,P_QI)=0.0                          
            IF (P_QS==1) WATER(I,J,K,P_QS)=0.0                          
            IF (P_QG==1) WATER(I,J,K,P_QG)=0.0                          
          ENDDO
         ENDDO
        ENDDO
      ELSE
        CLD_INIT=.FALSE.
      ENDIF
!
!----------------------------------------------------------------------
!-- Couple 2 sets of condensed water arrays for different microphysics: 
!   4D WATER(:,:,:,P_Qx) array <=> CWM,F_ice,F_rain,F_RimeF 3D arrays
!----------------------------------------------------------------------
!
      SELECT CASE ( TRIM(MICROPHYSICS) )
!
!----------------------------------------------------------------------
        CASE ('fer','fer_hires')  !-- Update fields for Ferrier microphysics
!----------------------------------------------------------------------
!
          spec_adv_fer: IF (.NOT.SPEC_ADV .OR. CLD_INIT) THEN
!-- Update WATER arrays when advecting only total condensate (spec_adv=F)
!   or at the initial time step
            DO K=1,LM
             DO J=JMS,JME
              DO I=IMS,IME
                IF (CWM(I,J,K)>EPSQ) THEN
                  LIQW=(1.-F_ice(I,J,K))*CWM(I,J,K)
                  WATER(I,J,K,P_QC)=(1.-F_rain(I,J,K))*LIQW
                  WATER(I,J,K,P_QR)=F_rain(I,J,K)*LIQW
                  WATER(I,J,K,P_QS)=F_ice(I,J,K)*CWM(I,J,K)
                ELSE
                  WATER(I,J,K,P_QC)=0.
                  WATER(I,J,K,P_QR)=0.
                  WATER(I,J,K,P_QS)=0.
                ENDIF
              ENDDO
             ENDDO
            ENDDO
!
          ELSE spec_adv_fer
!-- Update CWM,F_ICE,F_RAIN arrays from separate species advection (spec_adv=T)
            DO K=1,LM
             DO J=JMS,JME
              DO I=IMS,IME
                CWM(I,J,K)=WATER(I,J,K,P_QC)+WATER(I,J,K,P_QR)+WATER(I,J,K,P_QS)
                IF (WATER(I,J,K,P_QS)>EPSQ) THEN
                  F_ICE(I,J,K)=WATER(I,J,K,P_QS)/CWM(I,J,K)
                ELSE
                  F_ICE(I,J,K)=0.0
                ENDIF
                IF (WATER(I,J,K,P_QR)>EPSQ) THEN
                  F_RAIN(I,J,K)=WATER(I,J,K,P_QR)/(WATER(I,J,K,P_QC)+WATER(I,J,K,P_QR))
                ELSE
                  F_RAIN(I,J,K)=0.
                ENDIF
              ENDDO
             ENDDO
            ENDDO
          ENDIF spec_adv_fer
!
!----------------------------------------------------------------------
        CASE ('gfs')       !-- Update fields for GFS microphysics
!----------------------------------------------------------------------
!
          spec_adv_gfs: IF (.NOT.SPEC_ADV .OR. CLD_INIT) THEN
            cld_init_gfs: IF (CLD_INIT) THEN
!-- Initialize F_ICE, F_RAIN, & F_RIMEF arrays
              IF (SPEC_ADV) THEN
                WRITE(0,*) 'Never ran GFS microphysics with SPEC_ADV=T.'   &
                          ,'  Use at your own risk.'
              ENDIF
              DO K=1,LM
               DO J=JMS,JME
                DO I=IMS,IME
                  F_RAIN(I,J,K)=0.
                  F_RIMEF(I,J,K)=1.
                  IF (CWM(I,J,K)>EPSQ .AND. T(I,J,K)<233.15) THEN
                    F_ICE(I,J,K)=1.
                  ELSE
                    F_ICE(I,J,K)=0.
                  ENDIF
                ENDDO
               ENDDO
              ENDDO
            ENDIF  cld_init_gfs
!-- Update WATER arrays when advecting only total condensate (spec_adv=F)
!   or initialize them at the start of the forecast (CLD_INIT=T).
            DO K=1,LM
             DO J=JMS,JME
              DO I=IMS,IME
                IF (CWM(I,J,K)>EPSQ) THEN
                  WATER(I,J,K,P_QC)=(1.-F_ice(I,J,K))*CWM(I,J,K)
                  WATER(I,J,K,P_QI)=F_ice(I,J,K)*CWM(I,J,K)
                ELSE
                  WATER(I,J,K,P_QC)=0.
                  WATER(I,J,K,P_QI)=0.
                ENDIF
              ENDDO
             ENDDO
            ENDDO
          ELSE spec_adv_gfs
!-- Update CWM, F_ICE arrays from separate species advection (spec_adv=T)
            DO K=1,LM
             DO J=JMS,JME
              DO I=IMS,IME
                CWM(I,J,K)=WATER(I,J,K,P_QC)+WATER(I,J,K,P_QI)
                IF (CWM(I,J,K)>EPSQ) THEN
                  F_ICE(I,J,K)=WATER(I,J,K,P_QI)/CWM(I,J,K)
                ELSE
                  F_ICE(I,J,K)=0.
                ENDIF
              ENDDO
             ENDDO
            ENDDO
          ENDIF  spec_adv_gfs
!
!----------------------------------------------------------------------
        CASE ('wsm6')      !-- Update fields for WSM6 microphysics
!----------------------------------------------------------------------
!
          init_adv_wsm6: IF (CLD_INIT) THEN
!-- Assume only cloud ice is present at initial time
            DO K=1,LM
             DO J=JMS,JME
              DO I=IMS,IME
                WATER(I,J,K,P_QS)=0.0
                WATER(I,J,K,P_QG)=0.0
                IF (CWM(I,J,K)>EPSQ) THEN
                  LIQW=(1.-F_ice(I,J,K))*CWM(I,J,K)
                  WATER(I,J,K,P_QC)=(1.-F_rain(I,J,K))*LIQW
                  WATER(I,J,K,P_QR)=F_rain(I,J,K)*LIQW
                  WATER(I,J,K,P_QI)=F_ice(I,J,K)*CWM(I,J,K)
                ELSE
                  WATER(I,J,K,P_QC)=0.
                  WATER(I,J,K,P_QR)=0.
                  WATER(I,J,K,P_QI)=0.
                ENDIF
              ENDDO
             ENDDO
            ENDDO
          ELSE init_adv_wsm6
            notspec_adv_wsm6: IF (.NOT.SPEC_ADV) THEN
!-- Update WATER arrays when advecting only total condensate (spec_adv=F).
!-- Assume fraction of each water category is unchanged by advection. 
              DO K=1,LM
               DO J=JMS,JME
                DO I=IMS,IME
                  OLDCWM=WATER(I,J,K,P_QC)+WATER(I,J,K,P_QR)   &
                        +WATER(I,J,K,P_QI)+WATER(I,J,K,P_QS)   &
                        +WATER(I,J,K,P_QG)
                  IF (OLDCWM>EPSQ) THEN
                    FRACTION=CWM(I,J,K)/OLDCWM
                    WATER(I,J,K,P_QC)=FRACTION*WATER(I,J,K,P_QC)
                    WATER(I,J,K,P_QR)=FRACTION*WATER(I,J,K,P_QR)
                    WATER(I,J,K,P_QI)=FRACTION*WATER(I,J,K,P_QI)
                    WATER(I,J,K,P_QS)=FRACTION*WATER(I,J,K,P_QS)
                    WATER(I,J,K,P_QG)=FRACTION*WATER(I,J,K,P_QG)
                  ELSE
                    WATER(I,J,K,P_QC)=0.0
                    WATER(I,J,K,P_QR)=0.0
                    WATER(I,J,K,P_QI)=0.0
                    WATER(I,J,K,P_QS)=0.0
                    WATER(I,J,K,P_QG)=0.0
                    IF (T(I,J,K)<233.15) THEN
                      WATER(I,J,K,P_QI)=CWM(I,J,K)
                    ELSE
                      WATER(I,J,K,P_QC)=CWM(I,J,K)
                    ENDIF
                  ENDIF
                ENDDO
               ENDDO
              ENDDO
            ENDIF  notspec_adv_wsm6
!
!-- Couple 4D WATER(:,:,:,P_Qx) <=> CWM,F_ice,F_rain,F_RimeF arrays
!-- Update CWM,F_XXX arrays from separate species advection (spec_adv=T)
!
            DO K=1,LM
             DO J=JMS,JME
              DO I=IMS,IME
                CWM(I,J,K)=WATER(I,J,K,P_QC)+WATER(I,J,K,P_QR)      &
                          +WATER(I,J,K,P_QI)+WATER(I,J,K,P_QS)      &
                          +WATER(I,J,K,P_QG)
                IF (CWM(I,J,K)>EPSQ) THEN
                  LIQW=WATER(I,J,K,P_QI)+WATER(I,J,K,P_QS)+WATER(I,J,K,P_QG)
                  F_ICE(I,J,K)=LIQW/CWM(I,J,K)
                ELSE
                  F_ICE(I,J,K)=0.
                ENDIF
                IF (WATER(I,J,K,P_QR)>EPSQ) THEN
                  F_RAIN(I,J,K)=WATER(I,J,K,P_QR)/(WATER(I,J,K,P_QC)+WATER(I,J,K,P_QR))
                ELSE
                  F_RAIN(I,J,K)=0.
                ENDIF
                IF (WATER(I,J,K,P_QG)>EPSQ) THEN
!-- Update F_RIMEF: assume 5x higher graupel density (500 kg/m**3) vs snow (100 kg/m**3)
                  LIQW=5.*WATER(I,J,K,P_QG)+WATER(I,J,K,P_QS)
                  F_RIMEF(I,J,K)=LIQW/(WATER(I,J,K,P_QS)+WATER(I,J,K,P_QG))
                ELSE
                  F_RIMEF(I,J,K)=1.
                ENDIF
              ENDDO
             ENDDO
            ENDDO
!
          ENDIF init_adv_wsm6
!
!----------------------------------------------------------------------
        CASE DEFAULT
!----------------------------------------------------------------------
!
          IF (CLD_INIT) THEN
            WRITE(0,*) 'Do nothing for default option'
          ENDIF
!
      END SELECT   ! MICROPHYSICS
!
!----------------------------------------------------------------------
!
      END SUBROUTINE UPDATE_WATER
 
!----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
 
      SUBROUTINE CLTEND (ICLTEND,NPRECIP, T,Told,Tadj                    &
                        ,IDS,IDE,JDS,JDE,LM                              &
                        ,IMS,IME,JMS,JME                                 &
                        ,ITS,ITE,JTS,JTE)
!----------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CLTEND      TEMPERATURE CHANGE BY CLOUD PROCESSES
!   PRGRMMR: FERRIER         ORG: W/NP22     DATE: 01-09-26
!     
! ABSTRACT:
!     CLTEND GRADUALLY UPDATES TEMPERATURE TENDENCIES FROM CONVECTION 
!     AND GRID-SCALE MICROPHYSICS.
!     
! USAGE: CALL CLTEND FROM SOLVER_RUN
!   INPUT ARGUMENT LIST:
!     ICLTEND - FLAG SET TO -1 PRIOR TO PHYSICS CALLS, 0 AFTER PHYSICS
!               CALLS, AND 1 FOR UPDATING TEMPERATURES EVERY TIME STEP
!  
!   OUTPUT ARGUMENT LIST:  NONE
!     
!   OUTPUT FILES:  NONE
!     
!   SUBPROGRAMS CALLED:  NONE
!  
!   UNIQUE: NONE
!  
!   LIBRARY: NONE
!  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: ICLTEND,NPRECIP                            &
                           ,IDS,IDE,JDS,JDE,LM                         &
                           ,IMS,IME,JMS,JME                            &
                           ,ITS,ITE,JTS,JTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,1:LM),INTENT(INOUT) :: T          &
                                                           ,Tadj       &
                                                           ,Told
!
!***  LOCAL VARIABLES 
!
      INTEGER :: I,J,K
      REAL :: RDTPH
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      IF(ICLTEND<0)THEN
         DO K=1,LM
         DO J=JTS,JTE
         DO I=ITS,ITE
            Told(I,J,K)=T(I,J,K)
         ENDDO
         ENDDO
         ENDDO
      ELSE IF(ICLTEND==0)THEN
         RDTPH=1./REAL(NPRECIP)
         DO K=1,LM
         DO J=JTS,JTE
         DO I=ITS,ITE
            Tadj(I,J,K)=RDTPH*(T(I,J,K)-Told(I,J,K))
            T(I,J,K)=Told(I,J,K)
         ENDDO
         ENDDO
         ENDDO
      ELSE
         DO K=1,LM
         DO J=JTS,JTE
         DO I=ITS,ITE
            T(I,J,K)=T(I,J,K)+Tadj(I,J,K)
         ENDDO
         ENDDO
         ENDDO
      ENDIF
!----------------------------------------------------------------------
!
      END SUBROUTINE CLTEND
!
!-----------------------------------------------------------------------
 
!----------------------------------------------------------------------
!######################################################################
!-----------------------------------------------------------------------
 
      END MODULE MODULE_SOLVER_GRID_COMP
!
!-----------------------------------------------------------------------
