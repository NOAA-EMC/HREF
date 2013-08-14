!  $Id: MAPL_Cap.F90,v 1.24 2009/02/23 21:30:13 trayanov Exp $

#include "MAPL_Generic.h"

module MAPL_CapMod

!BOP

! !MODULE: MAPL_CapMod --- Implements the top entry point for MAPL components

! !USES:

  use ESMF_Mod
  use MAPL_BaseMod
  use MAPL_ConstantsMod
  use MAPL_ProfMod
  use MAPL_MemUtilsMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_GenericMod
  use ESMFL_Mod
  use MAPL_HistoryGridCompMod, only : Hist_SetServices => SetServices

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public MAPL_Cap

! !DESCRIPTION: 

! \input{TeX/MAPL_CapIntro.tex}
 
!EOP

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOPI

! !IROUTINE: MAPL_Cap -- Implements generic Cap functionality

! !INTERFACE:

  subroutine MAPL_CAP(ROOT_SetServices, Name, RC)

! !ARGUMENTS:

    external                             :: ROOT_SetServices
    character*(*), optional, intent(IN ) :: Name
    integer,       optional, intent(OUT) :: rc

!EOPI

! Handles to the CAP's Gridded Components GCs
! -------------------------------------------

   integer                      :: ROOT
   integer                      :: HIST
   character(len=ESMF_MAXSTR)   :: ROOT_NAME

! A MAPL object for the cap
!--------------------------

   type(MAPL_MetaComp)          :: MAPLOBJ

! The children's GCs and IM/Ex states
!------------------------------------

   type(ESMF_GridComp), pointer :: GCS(:)
   type(ESMF_State),    pointer :: IMPORTS(:)
   type(ESMF_State),    pointer :: EXPORTS(:)

! ESMF stuff
!-----------

   type(ESMF_VM)                :: VM
   type(ESMF_Config)            :: config
   type(ESMF_Clock)             :: clock
   type(ESMF_Grid)              :: grid

! ErrLog variables
!-----------------

   integer                      :: STATUS
   character(len=ESMF_MAXSTR)   :: Iam="MAPL_Cap"

! Misc locals
!------------

   character(len=ESMF_MAXSTR)   :: ROOT_CF
   character(len=ESMF_MAXSTR)   :: HIST_CF
   character(len=ESMF_MAXSTR)   :: enableTimers
   character(len=ESMF_MAXSTR)   :: enableMemUtils

   logical                      :: done

! Begin
!------

!  Initialize ESMF
!-----------------

#if defined(ENABLE_ESMF_ERR_LOGGING)
   call ESMF_Initialize (vm=vm, rc=status)
#else
   call ESMF_Initialize (vm=vm, defaultLogType=ESMF_LOG_NONE, rc=status)
#endif
   VERIFY_(STATUS)

!  Open the CAP's configuration from CAP.rc
!------------------------------------------

   config = ESMF_ConfigCreate (                   rc=STATUS )
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile   ( config, 'CAP.rc', rc=STATUS )
   VERIFY_(STATUS)

!  CAP's MAPL MetaComp
!---------------------

   if(present(Name)) then
      call MAPL_Set (MAPLOBJ, name= Name, cf=CONFIG,    rc=STATUS )
      VERIFY_(STATUS)
   else
      call MAPL_Set (MAPLOBJ, name='CAP', cf=CONFIG,    rc=STATUS )
      VERIFY_(STATUS)
   end if

!  Create Clock. This is a private routine that sets the start and 
!   end times and the time interval of the clock from the configuration.
!   The start time is temporarily set to 1 interval before the time in the
!   configuration. Once the Alarms are set in intialize, the clock will
!   be advanced to guarantee it and its alarms are in the same state as they
!   were after the last advance before the previous Finalize.
!---------------------------------------------------------------------------

   call MAPL_ClockInit ( MAPLOBJ, clock,             rc=STATUS )
   VERIFY_(STATUS)

!  Create grid
! -----------

   GRID = MAPL_LogRectGridCreate( MAPLOBJ,            rc=STATUS ) 
   VERIFY_(STATUS)

!  Get configurable info to create HIST 
!  and the ROOT of the computational hierarchy
!---------------------------------------------

!BOR

! !xRESOURCE_ITEM: string :: Name of ROOT's config file
   call MAPL_GetResource(MAPLOBJ, ROOT_CF,      "ROOT_CF:", &
                         default="ROOT.rc",       RC=STATUS ) 
   VERIFY_(STATUS)

! !xRESOURCE_ITEM: string :: Name to assign to the ROOT component
   call MAPL_GetResource(MAPLOBJ, ROOT_NAME,    "ROOT_NAME:",  &
                         default="ROOT",           RC=STATUS ) 
   VERIFY_(STATUS)

! !xRESOURCE_ITEM: string :: Name of HISTORY's config file 
   call MAPL_GetResource(MAPLOBJ, HIST_CF,      "HIST_CF:", &
                         default="HIST.rc",        RC=STATUS ) 
   VERIFY_(STATUS)

! !xRESOURCE_ITEM: string :: Control Timers 
   call MAPL_GetResource(MAPLOBJ, enableTimers, "MAPL_ENABLE_TIMERS:", &
                         default='NO',             RC=STATUS )
   VERIFY_(STATUS)

! !xRESOURCE_ITEM: string :: Control Memory Diagnostic Utility 
   call MAPL_GetResource(MAPLOBJ, enableMemUtils, "MAPL_ENABLE_MEMUTILS:", &
                         default='NO',             RC=STATUS )
   VERIFY_(STATUS)

   if (enableTimers /= 'YES' .and. enableTimers /= 'yes') then
      call MAPL_ProfDisable( rc=STATUS )
      VERIFY_(STATUS)
   end if

  if (enableMemUtils /= 'YES' .and. enableMemUtils /= 'yes') then
     call MAPL_MemUtilsDisable( rc=STATUS )
     VERIFY_(STATUS)
  else
     call MAPL_MemUtilsInit( rc=STATUS )
     VERIFY_(STATUS)
  end if

! Register the children with MAPL
!--------------------------------

!  Create Root child
!-------------------

   ROOT = MAPL_AddChild ( MAPLOBJ,     &
        name       = ROOT_NAME,        &
        grid       = GRID,             &
        configfile = ROOT_CF,          &
        SS         = ROOT_SetServices, &
                             rc=STATUS )  
   VERIFY_(STATUS)

!  Create History child
!----------------------

   HIST = MAPL_AddChild ( MAPLOBJ,        &
        name       = 'HIST',           &
        grid       = GRID,             &
        configfile = HIST_CF,          &
        SS         = HIST_SetServices, &
                             rc=STATUS )  
   VERIFY_(STATUS)

!  Query MAPL for the the children's for GCS, IMPORTS, EXPORTS
!-------------------------------------------------------------

   call MAPL_Get ( MAPLOBJ, GCS=GCS, GIM=IMPORTS, GEX=EXPORTS,      RC=STATUS )
   VERIFY_(STATUS)

!  Initialize the Computational Hierarchy
!----------------------------------------

   call ESMF_GridCompInitialize ( GCS(ROOT), IMPORTS(ROOT), EXPORTS(ROOT), &
                                                          CLOCK, RC=STATUS )
   VERIFY_(STATUS)

! All the EXPORTS of the Hierachy are made IMPORTS of History
!------------------------------------------------------------

   call ESMF_StateAdd ( IMPORTS(HIST), EXPORTS(ROOT),       RC=STATUS )
   VERIFY_(STATUS)

! this probably is a "bug" in HISTORY but it is required :(

   call ESMF_StateAdd ( IMPORTS(HIST), EXPORTS(HIST),       RC=STATUS )
   VERIFY_(STATUS)

! Initialize the History
!------------------------

   call ESMF_GridCompInitialize ( GCS(HIST), IMPORTS(HIST), EXPORTS(HIST), & 
                                                         CLOCK,  RC=STATUS )
   VERIFY_(STATUS)
 
! Time Loop starts by checking for Segment Ending Time
!-----------------------------------------------------

   TIME_LOOP: do

      call MAPL_MemUtilsWrite(vm, 'MAPL_Cap:TimeLoop',           RC=STATUS )
      VERIFY_(STATUS)

      DONE = ESMF_ClockIsStopTime( CLOCK,                        RC=STATUS )
      VERIFY_(STATUS)

      if ( DONE ) exit

! Call Record for intermediate checkpoint (if desired)
!  This is currently implemented as a phase of Finalize
!  rather than as a separate registered method. Note that
!  we are not doing a Record for History.
! ------------------------------------------------------

      call ESMF_GridCompFinalize( GCS(ROOT), IMPORTS(ROOT), EXPORTS(ROOT), &
                                 CLOCK,  phase=MAPL_RecordPhase, RC=STATUS )
      VERIFY_(STATUS)

! Run the Gridded Component
! --------------------------

      call ESMF_GridCompRun     ( GCS(ROOT), IMPORTS(ROOT), EXPORTS(ROOT), &
                                                          CLOCK, RC=STATUS )
      VERIFY_(STATUS)

! Advance the Clock before running History and Record
! ---------------------------------------------------

      call ESMF_ClockAdvance     ( CLOCK,                        RC=STATUS )
      VERIFY_(STATUS)

! Call History Run for Output
! ---------------------------

      call ESMF_GridCompRun     ( GCS(HIST), IMPORTS(HIST), EXPORTS(HIST), &
                                                          CLOCK, RC=STATUS )
      VERIFY_(STATUS)

! Synchronize for Next TimeStep
! -----------------------------

      call ESMF_VMBarrier       ( VM,                            RC=STATUS )
      VERIFY_(STATUS)

   enddo TIME_LOOP ! end of time loop

!  Finalize
!  --------

   call ESMF_GridCompFinalize( GCS(ROOT),IMPORTS(ROOT),EXPORTS(ROOT),CLOCK,RC=STATUS )
   VERIFY_(STATUS)
   call ESMF_GridCompFinalize( GCS(HIST),IMPORTS(HIST),EXPORTS(HIST),CLOCK,RC=STATUS )

!  Finalize itselt
! ----------------

   call CAP_Finalize(CLOCK, "cap_restart", rc=STATUS)
   VERIFY_(STATUS)

!  Finalize framework
!  ------------------

   call ESMF_Finalize (RC=status)
   VERIFY_(STATUS)

   RETURN_(ESMF_SUCCESS)

 end subroutine MAPL_CAP

 subroutine CAP_FINALIZE ( clock,filen, rc )

   type(ESMF_Clock),    intent(in   ) :: clock
   character(len=*),    optional      :: filen
   integer, optional,   intent(  out) :: rc

   integer        :: UNIT
   integer        :: datetime(2)
   integer        :: YY, MM, DD, H, M, S
   integer        :: status
   character(len=ESMF_MAXSTR), parameter :: IAm="CAP_FINALIZE"
   character(len=ESMF_MAXSTR)            :: filen_

   type(ESMF_Time)     :: CurrentTime
   
   filen_ = "cap_restart"
   if (present(filen))     filen_ = trim(filen )
   
! Retrieve Current Time for Cap Restart
! -------------------------------------

   call ESMF_ClockGet ( clock, currTime=currentTime, rc=status )
   VERIFY_(STATUS)
   call ESMF_TimeGet  ( CurrentTime, YY = YY, &
                                     MM = MM, &
                                     DD = DD, &
                                     H  = H , &
                                     M  = M , &
                                     S  = S, rc=status )
   VERIFY_(STATUS)

   CALL MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)

! Write CAP Restart File and Ending Time for Current Segment
! ----------------------------------------------------------

    if( MAPL_AM_I_ROOT() ) then
       UNIT = GETFILE( filen_, form="formatted" )
       write(unit,100) datetime
100    format(i8.8,1x,i6.6)
       call FREE_FILE (UNIT)
    endif

    RETURN_(ESMF_SUCCESS)
  end subroutine CAP_FINALIZE

!BOPI

! !IROUTINE: MAPL_ClockInit -- Sets the clock

! !INTERFACE: 

  subroutine MAPL_ClockInit ( MAPLOBJ, Clock, rc)

! !ARGUMENTS:

     type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
     type(ESMF_Clock),    intent(  out) :: Clock
     integer, optional,   intent(  out) :: rc

!  !DESCRIPTION:

!   This is a private routine that sets the start and 
!   end times and the time interval of the application clock from the configuration.
!   This time interal is the ``heartbeat'' of the application.
!   The Calendar is set to Gregorian by default. 
!   The start time is temporarily set to 1 interval before the time in the
!   configuration. Once the Alarms are set in intialize, the clock will
!   be advanced to guarantee it and its alarms are in the same state as they
!   were after the last advance before the previous Finalize.
!

!EOPI

     type(ESMF_Time)          :: StartTime    ! Initial     Begin  Time of Experiment
     type(ESMF_Time)          :: EndTime      ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: StopTime     ! Final       Ending Time of Experiment
     type(ESMF_Time)          :: CurrTime     ! Current     Current Time of Experiment
     type(ESMF_TimeInterval)  :: timeStep     ! HEARTBEAT
     type(ESMF_TimeInterval)  :: duration
     type(ESMF_Calendar)      :: cal
     character(ESMF_MAXSTR)   :: CALENDAR

     integer                  :: STATUS
     character(ESMF_MAXSTR)   :: IAM="MAPL_ClockInit"

     integer        :: BEG_YY
     integer        :: BEG_MM
     integer        :: BEG_DD
     integer        :: BEG_H
     integer        :: BEG_M
     integer        :: BEG_S

     integer        :: CUR_YY
     integer        :: CUR_MM
     integer        :: CUR_DD
     integer        :: CUR_H
     integer        :: CUR_M
     integer        :: CUR_S

     integer        :: END_YY
     integer        :: END_MM
     integer        :: END_DD
     integer        :: END_H
     integer        :: END_M
     integer        :: END_S

     integer        :: DUR_YY
     integer        :: DUR_MM
     integer        :: DUR_DD
     integer        :: DUR_H
     integer        :: DUR_M
     integer        :: DUR_S

     integer        :: RUN_DT
     integer        :: NUM_DT
     integer        :: DEN_DT

     integer        :: UNIT
     integer        :: datetime(2)

! Begin
!------

! Read Times From Config
! ----------------------

!BOR

     call MAPL_GetResource( MAPLOBJ, datetime, label='BEG_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, BEG_YY, BEG_MM, BEG_DD, BEG_H, BEG_M, BEG_S)
     else

! !xRESOURCE_ITEM: year :: Beginning year (integer)
        call MAPL_GetResource( MAPLOBJ, BEG_YY, label='BEG_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: month :: Beginning month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, BEG_MM, label='BEG_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: day  :: Beginning day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, BEG_DD, label='BEG_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: hour :: Beginning hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, BEG_H , label='BEG_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: minute :: Beginning minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_M , label='BEG_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: second :: Beginning second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, BEG_S , label='BEG_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

     call MAPL_GetResource( MAPLOBJ, datetime, label='END_DATE:', rc=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, END_YY, END_MM, END_DD, END_H, END_M, END_S)
     else
! !xRESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, END_YY, label='END_YY:', DEFAULT=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, END_MM, label='END_MM:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, END_DD, label='END_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, END_H , label='END_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_M , label='END_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, END_S , label='END_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

     call MAPL_GetResource( MAPLOBJ, datetime, label='JOB_DURATION:', rc=STATUS )

     if(STATUS==ESMF_SUCCESS) then
        CALL MAPL_UnpackDateTime(DATETIME, DUR_YY, DUR_MM, DUR_DD, DUR_H, DUR_M, DUR_S)
     else
! !xRESOURCE_ITEM: year :: Ending year (integer)
        call MAPL_GetResource( MAPLOBJ, DUR_YY, label='DUR_YY:', DEFAULT=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: month :: Ending month (integer 1-12)
        call MAPL_GetResource( MAPLOBJ, DUR_MM, label='DUR_MM:', default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: day  :: Ending day of month (integer 1-31)
        call MAPL_GetResource( MAPLOBJ, DUR_DD, label='DUR_DD:', default=1, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: hour :: Ending hour of day (integer 0-23)
        call MAPL_GetResource( MAPLOBJ, DUR_H , label='DUR_H:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: minute :: Ending minute (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_M , label='DUR_M:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
! !xRESOURCE_ITEM: second :: Ending second (integer 0-59)
        call MAPL_GetResource( MAPLOBJ, DUR_S , label='DUR_S:' , default=0, rc=STATUS )
        VERIFY_(STATUS)
     end if

! !xRESOURCE_ITEM: seconds :: Interval of the application clock (the Heartbeat)
     call MAPL_GetResource( MAPLOBJ, RUN_DT, label='RUN_DT:',            rc=STATUS )
     VERIFY_(STATUS)
! !xRESOURCE_ITEM: 1 :: numerator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, NUM_DT, label='NUM_DT:', default=0, rc=STATUS )
     VERIFY_(STATUS)
! !xRESOURCE_ITEM: 1 :: denominator of decimal fraction of time step
     call MAPL_GetResource( MAPLOBJ, DEN_DT, label='DEN_DT:', default=1, rc=STATUS )
     VERIFY_(STATUS)
! !xRESOURCE_ITEM: string :: Calendar type
     call MAPL_GetResource( MAPLOBJ, CALENDAR, label='CALENDAR:', default="GREGORIAN", rc=STATUS )
     VERIFY_(STATUS)

!EOR

     ASSERT_(NUM_DT>=0)
     ASSERT_(DEN_DT> 0)
     ASSERT_(RUN_DT>=0)
!     ASSERT_(NUM_DT*RUN_DT>0)
     ASSERT_(NUM_DT<DEN_DT)

! initialize calendar to be Gregorian type
! ----------------------------------------

     if    (CALENDAR=="GREGORIAN") then
        cal = ESMF_CalendarCreate( "ApplicationCalendar", ESMF_CAL_GREGORIAN, rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CAL_GREGORIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="JULIAN"   ) then
        cal = ESMF_CalendarCreate( "ApplicationCalendar", ESMF_CAL_JULIAN,    rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CAL_JULIAN, RC=STATUS)
        VERIFY_(STATUS)
     elseif(CALENDAR=="NOLEAP"   ) then
        cal = ESMF_CalendarCreate( "ApplicationCalendar", ESMF_CAL_NOLEAP,    rc=status )
        VERIFY_(STATUS)
        call ESMF_CalendarSetDefault(ESMF_CAL_NOLEAP, RC=STATUS)
        VERIFY_(STATUS)
     else
        ASSERT_(.false.)
     endif

! initialize start time for Alarm frequencies
! -------------------------------------------

     call ESMF_TimeSet( StartTime, YY = BEG_YY, &
                                   MM = BEG_MM, &
                                   DD = BEG_DD, &
                                    H = BEG_H , &
                                    M = BEG_M , &
                                    S = BEG_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

     call ESMF_TimeSet(   EndTime, YY = END_YY, &
                                   MM = END_MM, &
                                   DD = END_DD, &
                                    H = END_H , &
                                    M = END_M , &
                                    S = END_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)  

! Read CAP Restart File for Current Time
! --------------------------------------

     CUR_YY = BEG_YY
     CUR_MM = BEG_MM
     CUR_DD = BEG_DD
     CUR_H  = BEG_H
     CUR_M  = BEG_M
     CUR_S  = BEG_S

     UNIT = GETFILE ( "cap_restart", form="formatted", ALL_PES=.true., rc=status )
     VERIFY_(STATUS)

     read(UNIT,100,err=999,end=999) datetime
100  format(i8.8,1x,i6.6)

     if( MAPL_AM_I_ROOT() ) then
         print *, 'Read CAP restart properly, Current Date = ', datetime(1)
         print *, '                           Current Time = ', datetime(2)
         print *
     endif

     CALL MAPL_UnpackDateTime(DATETIME, CUR_YY, CUR_MM, CUR_DD, CUR_H, CUR_M, CUR_S)


999  continue  ! Initialize Current time

     call FREE_FILE (UNIT)

     call ESMF_TimeSet( CurrTime, YY = CUR_YY, &
                                  MM = CUR_MM, &
                                  DD = CUR_DD, &
                                   H = CUR_H , &
                                   M = CUR_M , &
                                   S = CUR_S , &
                    calendar=cal,  rc = STATUS  )
     VERIFY_(STATUS)

! initialize final stop time
! --------------------------

     call ESMF_TimeIntervalSet(  duration, YY = DUR_YY, &
                                   MM = DUR_MM, &
                                    D = DUR_DD, &
                                    H = DUR_H , &
                                    M = DUR_M , &
                                    S = DUR_S , &
!                                    calendar = cal, &
                                    startTime = currTime, &
                                    rc = STATUS  )
     VERIFY_(STATUS)

     stopTime = currTime + duration

! initialize model time step
! --------------------------

     call ESMF_TimeIntervalSet( timeStep, S=RUN_DT, sN=NUM_DT, sD=DEN_DT, rc=STATUS )
     VERIFY_(STATUS)

! Create Clock and set it to one time step before StartTime.
! After Initialize has created all alarms, we will advance the
! clock to ensure the proper ringing state of all alarms
!-------------------------------------------------------------

     if (endTime < stopTime) then
        clock = ESMF_ClockCreate( "ApplClock",timeStep,StartTime,EndTime, rc=STATUS )
     else
        clock = ESMF_ClockCreate( "ApplClock",timeStep,StartTime,StopTime, rc=STATUS )
     end if
     VERIFY_(STATUS)

     call ESMF_ClockSet ( clock, CurrTime=CurrTime, rc=status )
     VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine MAPL_ClockInit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)
     integer, intent(IN   ) :: YY, MM, DD, H, M, S
     integer, intent(  OUT) :: DATETIME(:)

     datetime(1) = 10000*YY + 100*MM + DD
     datetime(2) = 10000* H + 100* M + S
     return
   end subroutine MAPL_PackDateTime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine MAPL_UnpackDateTime(DATETIME, YY, MM, DD, H, M, S)
     integer, intent(IN   ) :: DATETIME(:)
     integer, intent(  OUT) :: YY, MM, DD, H, M, S

     YY =     datetime(1)/10000
     MM = mod(datetime(1),10000)/100
     DD = mod(datetime(1),100)
     H  =     datetime(2)/10000
     M  = mod(datetime(2),10000)/100
     S  = mod(datetime(2),100)
     return
   end subroutine MAPL_UnpackDateTime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IROUTINE: MAPL_LogRectGridCreate

! !DESCRIPTION: This is a private routine that creates certain standard
!      logically-rectangular 3-D grids.
!      The nature of the grid can be controlled from the configuration.
!      In all cases the horizontal is represented by the first two
!      dimensions. Uniform Lat-Lon grids that spans the sphere are created
!      directly, relying on the configuration for grid parameters.
!      Other, more complicated grids require that a special grid create
!      function (AppGridCreate) with the same signature as MAPL\_LogRectGridCreate
!      be suppied by the user. A stub of this function lives in MAPL.
!      
!      The grid is created in the current VM with a layout obtained from the 
!      configuration. If the number of grid points per PET is not specified
!      in the configuration a default distribution is used.
!
! !INTERFACE:

  function MAPL_LogRectGridCreate (MAPLOBJ, RANK, RC) result(GRID)

! !ARGUMENTS:

    type(MAPL_MetaComp), intent(INOUT) :: MAPLOBJ
    integer, optional,   intent(  IN)  :: rank ! 2 for 2D grids
    integer, optional,   intent(  OUT) :: rc
    type (ESMF_Grid)                   :: grid

!EOP

! Local vars

    integer                         :: STATUS
    character(len=ESMF_MAXSTR)      :: IAm='MAPL_LogRectGridCreate'

    integer                         :: IM_WORLD
    integer                         :: JM_WORLD
    integer                         :: LM
    integer                         :: L
    integer                         :: NX, NY
    integer                         :: POLEEDGE
    integer, allocatable            :: IMS(:), JMS(:)
    character(len=ESMF_MAXSTR)      :: gridname
    real(ESMF_KIND_R8)              :: minCoord(3)
    real(ESMF_KIND_R8)              :: deltaX, deltaY, deltaZ
    real                            :: LON0
    type (ESMF_VM)                  :: VM
    integer                         :: latlon, rank_
    integer                         :: I, J, I1, IN, J1, JN
    real(ESMF_KIND_R8), pointer     :: centerX(:,:)
    real(ESMF_KIND_R8), pointer     :: centerY(:,:)
    real(ESMF_KIND_R8), allocatable :: cornerX(:)
    real(ESMF_KIND_R8), allocatable :: cornerY(:)

!BOR

   if ( present(rank) ) then
        rank_ = rank
   else                 
        rank_ = 3  ! default is 3D
   end if

! The default is a lat-lon grid, which MAPL can create. If it is not
! lat-lon, a special funcion must be provided in the executable to create
! the APP grid. (Later this can be extended to exchange grid creation,
! leaving the special function for still more exotic cases.)
!------------------------------------------------------------------------

! !xRESOURCE_ITEM: 0 or 1 :: 1 -> regular lat-lon; 0 -> custom grid
    call MAPL_GetResource( MAPLOBJ, LATLON, 'LATLON:', default=1, rc = status )
    VERIFY_(STATUS)

    if(LATLON /= 0) then

! We need the VM to create the grid ??
!------------------------------------

       call ESMF_VMGetCurrent(vm, rc=STATUS)
       VERIFY_(STATUS)

! Get Decomposition from CF
!--------------------------

! !xRESOURCE_ITEM: none :: Processing elements in 1st dimension
       call MAPL_GetResource( MAPLOBJ, NX,       label ='NX:', default=1, rc = status )
       VERIFY_(STATUS)
! !xRESOURCE_ITEM: none :: Processing elements in 2nd dimension
       call MAPL_GetResource( MAPLOBJ, NY,       label ='NY:', default=1, rc = status )
       VERIFY_(STATUS)

! Get World problem size from CF
!-------------------------------

! !xRESOURCE_ITEM: none :: Grid size in 1st dimension
       call MAPL_GetResource( MAPLOBJ, IM_WORLD, 'IM:',            rc = status )
       VERIFY_(STATUS)
! !xRESOURCE_ITEM: none :: Grid size in 2nd dimension
       call MAPL_GetResource( MAPLOBJ, JM_WORLD, 'JM:',            rc = status )
       VERIFY_(STATUS)
! !xRESOURCE_ITEM: none :: Grid size in 3rd dimension
       call MAPL_GetResource( MAPLOBJ, LM,       'LM:', default=1, rc = status )
       VERIFY_(STATUS)

! The grid's name is optional
!----------------------------

! !xRESOURCE_ITEM: none :: Optional grid name
       call MAPL_GetResource( MAPLOBJ, GRIDNAME, 'GRIDNAME:', default='APPGRID', rc = status )
       VERIFY_(STATUS)

! Give the IMS and JMS the MAPL default distribution
! --------------------------------------------------

       allocate( IMS(0:NX-1) )  
       allocate( JMS(0:NY-1) )  

       call MAPL_DecomposeDim ( IM_WORLD, IMS, NX )
       call MAPL_DecomposeDim ( JM_WORLD, JMS, NY )

! Override them with alues in CF, if any
!---------------------------------------

! !xRESOURCE_ITEM: none :: gridpoints in each PE along 1st dimension
       call MAPL_GetResource( MAPLOBJ, IMS, 'IMS:', default=IMS, rc = status )
       VERIFY_(STATUS)
! !xRESOURCE_ITEM: none :: gridpoints in each PE along 2nd dimension
       call MAPL_GetResource( MAPLOBJ, JMS, 'JMS:', default=JMS, rc = status )
       VERIFY_(STATUS)

! Lat-Lon grids cover the sphere, but the first grid box can have the pole on the
!  center or on the edge. The latter is the FV way, in which there is an "extra"
!  grid box in the meridional direction. This is the default.
!--------------------------------------------------------------------------------

! !xRESOURCE_ITEM: 0 or 1 :: 1->gridedge at pole; 0->gridpoint at pole
       call MAPL_GetResource( MAPLOBJ, POLEEDGE, 'POLEEDGE:', default=0       , rc = status )
       VERIFY_(STATUS)
! !xRESOURCE_ITEM: degrees :: Longituce of center of first gridbox
       call MAPL_GetResource( MAPLOBJ,     LON0, 'BEGLON:'  , default=-180., rc = status )
       VERIFY_(STATUS)

       LON0 = LON0 * (MAPL_PI/180.)
!EOR

! Lat-Lon Grid definition
!-------------------------
       
       deltaX      = 2.0*MAPL_PI/IM_WORLD
       minCoord(1) = LON0-deltaX/2 

       if(POLEEDGE==0) then
          deltaY = MAPL_PI/(JM_WORLD-1)
          minCoord(2) = -MAPL_PI/2-deltaY/2
       else
          deltaY = MAPL_PI/(JM_WORLD  )
          minCoord(2) = -MAPL_PI/2
       end if

       deltaZ = 1.0D0
       minCoord(3) = deltaZ/2

! We should have a much simpler create with the next ESMF grid design
!--------------------------------------------------------------------
       if ( rank_ == 2 ) then
          GRID = ESMF_GridCreateShapeTile(    &
               name=gridname,                 &
#ifndef USE_ESMF_DOMAIN_DECOMP
               countsPerDEDim1=ims,           &
               countsPerDEDim2=jms,           &
#endif
               coordDep1 = (/1,2/),           &
               coordDep2 = (/1,2/),           &
               gridEdgeLWidth = (/0,0/),      &
               gridEdgeUWidth = (/0,0/),      &
               rc=status)
          VERIFY_(STATUS)
       else
          GRID = ESMF_GridCreateShapeTile(    &
               name=gridname,                 &
#ifndef USE_ESMF_DOMAIN_DECOMP
               countsPerDEDim1=ims,           &
               countsPerDEDim2=jms,           &
               countsPerDEDim3=(/LM/),        &
#else
               maxIndex=(/IM_WORLD,JM_WORLD,LM/), &
               regDecomp=(/NX,NY,1/),           &
#endif
               indexFlag = ESMF_INDEX_USER,   &
               gridMemLBound = (/1,1,1/),     &
               coordDep1 = (/1,2/),             &
               coordDep2 = (/1,2/),             &
               coordDep3 = (/3/),             &
               gridEdgeLWidth = (/0,0,0/),    &
               gridEdgeUWidth = (/0,0,0/),    &
               rc=status)
          VERIFY_(STATUS)
       endif

! Allocate coords at default stagger location
       call ESMF_GridAddCoord(grid, rc=status)
       VERIFY_(STATUS)

! Compute the the coordinates (the corner/center is for backward compatibility)

       allocate(cornerX(IM_WORLD+1),cornerY(JM_WORLD+1), stat=status)
       VERIFY_(STATUS)

       cornerX(1) = minCoord(1)
       do i = 1,IM_WORLD
          cornerX(i+1) = cornerX(i) + deltaX
       enddo
       
       cornerY(1) = minCoord(2)
       do j = 1,JM_WORLD
          cornerY(j+1) = cornerY(j) + deltaY
       enddo

! Retrieve the coordinates so we can set them
       call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            fptr=centerX, rc=status)
       VERIFY_(STATUS)

       call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            fptr=centerY, rc=status)
       VERIFY_(STATUS)

       call ESMF_GRID_INTERIOR(GRID,I1,IN,J1,JN)

       do i = 1,size(centerX,1)
          centerX(I,:) = 0.5d0*(cornerX(I+I1-1)+cornerX(I+I1))
       end do

       do j = 1,size(centerY,2)
          centerY(:,J) = 0.5d0*(cornerY(J+J1-1)+cornerY(J+J1))
       enddo

       deallocate(cornerY,cornerX)

       call ESMF_GridValidate(grid,rc=status)
       VERIFY_(STATUS)

       deallocate(ims)
       deallocate(jms)

    else

       call AppGridCreate(MAPLOBJ, Grid, STATUS)
       VERIFY_(STATUS)

    endif

! All Done
!---------

    RETURN_(STATUS)
  end function MAPL_LogRectGridCreate

end module MAPL_CapMod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! GridCreate Stub
!----------------
subroutine AppGridCreate (MAPLOBJ, GRID, RC)

  use ESMF_Mod
  use MAPL_BaseMod
  use MAPL_GenericMod

  implicit none

  type(MAPL_MetaComp), intent(INOUT) :: MAPLOBJ
  type (ESMF_Grid),    intent(  OUT) :: grid
  integer, optional,   intent(  OUT) :: rc

  integer                            :: STATUS
  character(len=ESMF_MAXSTR)         :: IAm='AppGridCreate_STUB'

  RETURN_(ESMF_FAILURE)

end subroutine AppGridCreate

function AppGridCreateF(IM_WORLD, JM_WORLD, LM, NX, NY, rc) result(grid)

  use ESMF_Mod
  use MAPL_BaseMod
  use MAPL_GenericMod

  implicit none

! !ARGUMENTS:
    integer,           intent(IN)    :: IM_WORLD, JM_WORLD, LM
    integer,           intent(IN)    :: NX, NY
    integer, optional, intent(OUT)   :: rc
    type (ESMF_Grid)                 :: grid

  integer                            :: STATUS
  character(len=ESMF_MAXSTR)         :: IAm='AppGridCreateF_STUB'

  RETURN_(ESMF_FAILURE)
end function AppGridCreateF
