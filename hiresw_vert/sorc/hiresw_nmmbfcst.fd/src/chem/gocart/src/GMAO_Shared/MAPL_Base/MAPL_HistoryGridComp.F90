
! $Id: MAPL_HistoryGridComp.F90,v 1.12.4.7.2.26.8.1 2009/12/16 17:22:44 dasilva Exp $

#include "MAPL_Generic.h"

module MAPL_HistoryGridCompMod

!BOP

! !MODULE: MAPL_HistoryGridCompMod

! !USES:

  use ESMF_Mod
  use ESMFL_Mod
  use MAPL_BaseMod
  use MAPL_VarSpecMod
  use MAPL_ConstantsMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_GenericMod
  use MAPL_LocStreamMod
  use MAPL_CFIOMod
  use MAPL_GenericCplCompMod
  use ESMF_CFIOMOD, only:  StrTemplate => ESMF_CFIOstrTemplate
!  use ESMF_CFIOMOD

  implicit none
  private
#include "mpif.h"

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

! !DESCRIPTION: 
!                \input{TeX/MAPL_HistoryDescr.tex}
!
!EOP

! Define History Lists for Output
! -------------------------------
  type    history_list
     character(len=ESMF_MAXSTR)         :: collection
     character(len=ESMF_MAXSTR)         :: filename
     character(len=ESMF_MAXSTR)         :: template
     character(len=ESMF_MAXSTR)         :: format
     character(len=ESMF_MAXSTR)         :: mode
     character(len=ESMF_MAXSTR)         :: descr
     character(len=ESMF_MAXSTR),pointer :: fields (:,:)
     integer                            :: frequency
     integer                            :: acc_interval
     integer                            :: ref_date
     integer                            :: ref_time
     integer                            :: end_date
     integer                            :: end_time
     integer                            :: duration
     type(ESMF_Alarm)                   :: his_alarm
     type(ESMF_Alarm)                   :: seg_alarm
     type(ESMF_Alarm)                   :: mon_alarm
     type(ESMF_Alarm)                   :: end_alarm
     integer,pointer                    :: expSTATE (:)
     integer                            :: unit
     integer                            :: nfield
     type(ESMF_FieldBundle)             :: bundle
     type(MAPL_CFIO)                    :: MCFIO
     real   , pointer                   :: levels(:)     => null()
     integer, pointer                   :: resolution(:) => null()
     integer                            :: verbose
     integer                            :: xyoffset
     logical                            :: disabled
     real                               :: vscale
     character(len=ESMF_MAXSTR)         :: vunit
     character(len=ESMF_MAXSTR)         :: vvars(2)
     integer                            :: nbits
     integer                            :: deflate 
     integer                            :: slices
     integer                            :: Root
     integer                            :: Psize
  endtype history_list

  type SpecWrapper
     type (MAPL_VarSpec),              pointer :: SPEC(:)
  end type SpecWrapper

  type ExchangeRegridType
     type(MAPL_LocStreamXform) :: XFORM
     type(MAPL_LocStream)      :: LocIn
     type(MAPL_LocStream)      :: LocOut
     type(ESMF_State)          :: state_out
     integer                   :: ntiles_in
     integer                   :: ntiles_out
!ALT: this will not be needed when we modify LocStream to take vm instead of layout
     character(len=ESMF_MAXSTR)     :: tilefile
     character(len=ESMF_MAXSTR)     :: gridname
     logical                        :: noxform
     logical                        :: ontiles
  end type ExchangeRegridType

  type ExchangeRegrid
     type(ExchangeRegridType), pointer :: PTR
  end type ExchangeRegrid

  type HISTORY_STATE
     type (history_list),        pointer :: list(:)       => null()
     type (ExchangeRegrid),      pointer :: Regrid(:)     => null()
!     character(len=ESMF_MAXSTR), pointer :: GCNameList(:) => null()
!     type (ESMF_GridComp),       pointer :: gcs(:)        => null()
     type (ESMF_State),          pointer :: GIM(:)        => null()
     type (ESMF_State),          pointer :: GEX(:)        => null()
     type (ESMF_CplComp),        pointer :: CCS(:)        => null()
     type (ESMF_State),          pointer :: CIM(:)        => null()
     type (ESMF_State),          pointer :: CEX(:)        => null()
     type (ESMF_TimeInterval),   pointer :: STAMPOFFSET(:) => null()
     logical,                    pointer :: LCTL(:)       => null()
     logical,                    pointer :: average(:)    => null()
     type (SpecWrapper),         pointer :: SRCS(:)       => null()
     type (SpecWrapper),         pointer :: DSTS(:)       => null()
     character(len=ESMF_MAXSTR)          :: expid
     character(len=ESMF_MAXSTR)          :: expdsc
     integer                             :: CoresPerNode, mype, npes
  end type HISTORY_STATE
  


  type HISTORY_wrap
     type (HISTORY_STATE), pointer :: PTR
  end type HISTORY_wrap
  
contains

!=====================================================================
  subroutine SetServices ( gc, rc )
    type(ESMF_GridComp), intent(inout) :: gc     ! composite gridded component
    integer, optional               :: rc     ! return code
    
    integer                         :: status
    character(len=ESMF_MAXSTR)      :: IAm="History:SetServices" 
    type (HISTORY_wrap)             :: wrap
    type (HISTORY_STATE), pointer   :: internal_state

! Register services for this component
! ------------------------------------

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETINIT, Initialize, rc=status)
    VERIFY_(status)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETRUN,   Run,       rc=status)
    VERIFY_(status)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_SETFINAL, Finalize,  rc=status)
    VERIFY_(status)

! Allocate an instance of the private internal state...
!------------------------------------------------------

    allocate(internal_state, stat=status)
    VERIFY_(status)

! and save its pointer in the GC
!-------------------------------

    wrap%ptr => internal_state
    call ESMF_GridCompSetInternalState(gc, wrap, status)
    VERIFY_(status)
 
! Set the Profiling timers
! ------------------------

    call MAPL_TimerAdd (gc,name="Initialize"     ,rc=status)
    call MAPL_TimerAdd (gc,name="Finalize"       ,rc=status)
    call MAPL_TimerAdd (gc,name="Run"            ,rc=status)
    call MAPL_TimerAdd (gc,name="--Couplers"     ,rc=status)
    call MAPL_TimerAdd (gc,name="--I/O"          ,rc=status)
    call MAPL_TimerAdd (gc,name="----IO Create"  ,rc=status)
    call MAPL_TimerAdd (gc,name="----IO Write"   ,rc=status)
    call MAPL_TimerAdd (gc,name="-----IO Post"   ,rc=status)
    call MAPL_TimerAdd (gc,name="-----IO Wait"   ,rc=status)

! Generic Set Services
! --------------------
    call MAPL_GenericSetServices ( gc,RC=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

  end subroutine SetServices

!======================================================
!!BOP
! !IROUTINE: Initialize -- Initializes MAPL History Lists for Diagnostic Output

! !INTERFACE:

  subroutine Initialize ( gc, import, dumexport, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout)    :: gc     ! composite gridded component 
    type(ESMF_State),       intent(inout) :: import ! import state
    type(ESMF_State),       intent(inout) :: dumexport ! export state
    type(ESMF_Clock),       intent(inout) :: clock  ! the clock
      integer, intent(out), OPTIONAL        :: rc     ! Error code:

! !DESCRIPTION:
! Initialize initializes MAPL History Lists for Diagnostic Output.
! Diagnostics have the following attributes:
!
! \begin{description}
! \item[1)] Diagnostics may be "instantaneous" or "time-averaged"
! \item[2)] Diagnostics have a "frequency" and an associated "ref_date" and "ref_time"
!           from which the frequency is based.  An "end_date" and "end_time" may also be used
!           to turn off diagnostics after a given date and time.
! \item[3)] Time-Averaged Diagnostics have an associated accumulation interval, "acc_interval",
!           which may be <= to the diagnostic "frequency"
! \item[4)] Diagnostics are "time-stamped" with the center of the time-averaged period.
! \item[5)] The default "acc_interval" is the diagnostic "frequency"
! \item[6)] The default "ref_date" is the beginning date of the experiment
! \item[7)] The default "ref_time" is 0z
! \item[8)] The default "end_date" and "end_time" is disabled
! \end{description}
!
! Through the use of History Lists, the user may define the type of diagnostic output desired.
! History Lists contain the following attributes:
!
! \begin{description}
! \item[filename]     Character string defining the filename of a particular diagnostic output stream.
! \item[template]     Character string defining the time stamping template following GrADS convensions. The default value depends on the duration of the file.
! \item[format]       Character string defining file format ("flat" or "CFIO"). Default = "flat".
! \item[mode]         Character string equal to "instantaneous" or "time-averaged". Default = "instantaneous".
! \item[descr]        Character string equal to the list description. Defaults to "expdsc".
! \item[frequency]    Integer (HHMMSS) for the frequency of output.  Default = 060000.
! \item[acc_interval] Integer (HHMMSS) for the acculation interval (<= frequency) for time-averaged diagnostics.
!                     Default = Diagnostic Frequency.
! \item[ref_date]     Integer (YYYYMMDD) reference date from which the frequency is based.
!                     Default is the Experiment beginning date.
! \item[ref_time]     Integer (HHMMSS) reference time from which the frequency is based.
!                     Default is the Experiment beginning time.
! \item[end_date]     Integer (YYYYMMDD) ending date to stop diagnostic output.  Default is disabled.
! \item[end_time]     Integer (HHMMSS) ending time to stop diagnostic output. Default is disabled.
! \item[duration]     Integer (HHMMSS) for the duration of each file.  Default = 0 (duration is infinite).
! \item[fields]       Paired character strings for the diagnostic Name and its associated Gridded Component.
! \item[resolution]   Optional resolution (IM JM) for the ouput stream. Default is the native resolution.
! \item[xyoffset]     Optional Flag for Grid Staggering (0:DcPc, 1:DePc, 2:DcPe, 3:DePe)
! \item[levels]       Optional list of output levels (Default is all levels on Native Grid).
! \item[vvars]        Optional Field (and Transform) to use for Vertical Interpolation (eg., 'log(PLE)' , 'DYN' ).
! \item[vunit]        Optional Units to use for Vertical Index of Output File.
! \item[vscale]       Optional Scaling to use between Output Unit and VVARS unit.
! \end{description}
!
! !REVISION HISTORY:
!   14Jan2005 Todling  Implemented GRADS template-ready CFIO filename.
!!EOP

    integer                         :: status
    character(len=ESMF_MAXSTR)      :: IAm="History:Initalize" 

    logical                         :: errorFound
    logical                         :: found
    type(history_list), pointer     :: list(:)
    type(HISTORY_wrap)              :: wrap
    type (HISTORY_STATE), pointer   :: IntState

    type(ESMF_State), pointer      :: export (:)
    type(ESMF_State), pointer      :: exptmp (:)
    type(ESMF_Time)                :: StartTime
    type(ESMF_Time)                :: CurrTime
    type(ESMF_Time)                ::  RingTime
    type(ESMF_Time)                ::   RefTime
    type(ESMF_TimeInterval)        :: Frequency
    type(ESMF_Array)               :: array
    type(ESMF_Field)               :: field
    type(ESMF_Field)               :: f
    type(ESMF_Calendar)            ::  cal
    type(ESMF_Config)              :: config
    type(ESMF_DELayout)            :: layout
    type(MAPL_MetaComp), pointer   :: GENSTATE

    character(len=ESMF_MAXSTR)     :: string
    character(len=ESMF_MAXSTR)     :: tmpstring
    character(len=ESMF_MAXSTR)     :: tilefile
    character(len=ESMF_MAXSTR)     :: gridname
    character(len=ESMF_MAXSTR), pointer :: gnames(:)
    character(len=2)               :: POLE, DTLN
    integer                        :: L, LM, layers
    integer                        :: NMLEN
    integer                        :: NG
    integer                        :: NGRIDS
    integer                        :: COUNTS(3)
    integer                        :: DECOUNT(2)
    integer                        :: NX, NY
    integer, pointer               :: gridim(:), gridjm(:)
    integer, allocatable           :: IMXY(:), JMXY(:)
    real(ESMF_KIND_R8)             :: X0, Y0, deltaX, deltaY, deltaZ
    integer                        :: DIMS
    integer                        :: VLOCATION
    character(ESMF_MAXSTR)         :: FRIENDLYTO
    integer                        :: avgint
    integer                        :: REFRESH
    integer                        :: NumSlices
    character(ESMF_MAXSTR)         :: SHORT_NAME
    character(ESMF_MAXSTR)         :: LONG_NAME
    character(ESMF_MAXSTR)         :: UNITS
    character(ESMF_MAXSTR), pointer:: VVARn(:)
    character(ESMF_MAXSTR)         :: VVAR
    character(ESMF_MAXSTR), pointer:: fields (:,:)
    character(ESMF_MAXSTR)         :: fields1
    character(ESMF_MAXSTR)         :: fields2
    character(ESMF_MAXSTR)         :: fields3
    logical                        :: tend
    character(len=ESMF_MAXSTR),allocatable :: statelist(:)
    character(len=ESMF_MAXSTR),allocatable ::   tmplist(:)
    
    integer :: nlist,unit,nfield,nstatelist
!    integer :: nlist,unit,nsecf,nfield,nstatelist
    integer :: k,m,n,sec,rank,nhms,size0
    integer :: year,month,day,hour,minute,second,nymd0,nhms0
    integer :: ref_time(6)
    integer :: len, i, mype, npes

    real, pointer   :: Q1D(:)
    real, pointer   :: Q2D(:,:)
    real, pointer   :: Q3D(:,:,:)

    type (ESMF_Grid)                          :: grid
    type (ESMF_DistGrid)                      :: distgrid
    type (ESMF_Grid)                          :: grid_in, grid_out
    type (ESMF_Grid), pointer                 :: grids(:)
    type (MAPL_LocStream)                     :: exch
    type (ESMF_VM)                            :: vm
    logical                                   :: use_this_gridname
    logical                                   :: ontiles
    character(len=ESMF_MAXSTR)                :: tmpstr

! Begin
!------

    call MAPL_GetObjectFromGC ( gc, GENSTATE, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerOn(GENSTATE,"TOTAL")
    call MAPL_TimerOn(GENSTATE,"Initialize")

! Retrieve the pointer to the state
    call ESMF_GridCompGetInternalState(gc, wrap, status)
    VERIFY_(status)
    IntState => wrap%ptr

    call ESMF_GridCompGet(gc, vm=vm, rc=status)
    VERIFY_(status)

    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(status)
    call ESMF_VMGet       (VM, localpet=MYPE, petcount=NPES,  RC=STATUS)
    VERIFY_(STATUS)

    IntState%mype = mype
    IntState%npes = npes


! Get Clock StartTime for Default ref_date, ref_time
! --------------------------------------------------
    call ESMF_ClockGet ( clock,     calendar=cal,       rc=STATUS ) ; VERIFY_(STATUS)
    call ESMF_ClockGet ( clock,     currTime=CurrTime,  rc=STATUS ) ; VERIFY_(STATUS)
    call ESMF_ClockGet ( clock,     StartTime=StartTime,rc=STATUS ) ; VERIFY_(STATUS)
    call ESMF_TimeGet  ( StartTime, TimeString=string  ,rc=STATUS ) ; VERIFY_(STATUS)
    
    read(string( 1: 4),'(i4.4)') year
    read(string( 6: 7),'(i2.2)') month
    read(string( 9:10),'(i2.2)') day
    read(string(12:13),'(i2.2)') hour
    read(string(15:16),'(i2.2)') minute
    read(string(18:18),'(i2.2)') second
    
    nymd0 =  year*10000 +  month*100 + day
    nhms0 =  hour*10000 + minute*100 + second

! Read User-Supplied History Lists from Config File
! -------------------------------------------------
    call ESMF_GridCompGet( gc, config=config, rc=STATUS ) ; VERIFY_(STATUS)

    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expid, &
                                   label ='EXPID:', default='', rc=status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expdsc, &
                                   label ='EXPDSC:', default='', rc=status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%CoresPerNode, &
                                   label ='CoresPerNode:', default=8, rc=status )
    VERIFY_(STATUS)

    if( MAPL_AM_I_ROOT() ) then
       print *
       print *, 'EXPID: ',trim(INTSTATE%expid)
       print *, 'Descr: ',trim(INTSTATE%expdsc)
       print *
    endif

! Determine Number of Output Streams
! ----------------------------------
    call ESMF_ConfigFindLabel ( config,'COLLECTIONS:',rc=STATUS )
    VERIFY_(STATUS)
    tend  = .false.
    nlist = 0
    allocate(IntState%list(nlist), stat=status)
    VERIFY_(STATUS)
    do while (.not.tend)
          call ESMF_ConfigGetAttribute ( config,value=tmpstring,default='',rc=STATUS) !ALT: we don't check retrun status!!!
          if (tmpstring /= '')  then
             nlist = nlist + 1
             allocate( list(nlist), stat=status )
             VERIFY_(STATUS)
             list(1:nlist-1)=IntState%list
             list(nlist)%collection = tmpstring
             list(nlist)%filename = list(nlist)%collection
             deallocate(IntState%list)
             IntState%list => list
          end if
          call ESMF_ConfigNextLine     ( config,tableEnd=tend,rc=STATUS )
          VERIFY_(STATUS)
    enddo

    if (nlist == 0) then
       RETURN_(ESMF_SUCCESS)
    end if

    allocate(IntState%Regrid(nlist), stat=STATUS)
    VERIFY_(STATUS)
    allocate(          Vvarn(nlist), stat=STATUS)
    VERIFY_(STATUS)

! Initialize History Lists
! ------------------------
 
    LISTLOOP: do n=1,nlist
       list(n)%unit = 0

       string = trim( list(n)%collection ) // '.'

       if (trim(list(n)%filename) == "/dev/null") then
          list(n)%disabled = .true.
       else
          list(n)%disabled = .false.
       end if
       call ESMF_ConfigGetAttribute ( config, value=list(n)%template, default="", &
                                      label=trim(string) // 'template:' ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, value=list(n)%format,default='flat', &
                                      label=trim(string) // 'format:' ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, value=list(n)%mode,default='instantaneous', &
                                      label=trim(string) // 'mode:' ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, value=list(n)%descr, &
                                      default=INTSTATE%expdsc, &
                                      label=trim(string) // 'descr:' ,rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( config, list(n)%frequency, default=060000, &
	                              label=trim(string) // 'frequency:',rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, list(n)%acc_interval, default=list(n)%frequency, &
	                              label=trim(string) // 'acc_interval:',rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( config, list(n)%ref_date, default=nymd0, &
	                              label=trim(string) // 'ref_date:',rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, list(n)%ref_time, default=nhms0, &
                                      label=trim(string) // 'ref_time:',rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( config, list(n)%end_date, default=-999, &
	                              label=trim(string) // 'end_date:',rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, list(n)%end_time, default=-999, &
                                      label=trim(string) // 'end_time:',rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( config, list(n)%duration, default=0, &
	                              label=trim(string) // 'duration:'  ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, list(n)%verbose, default=0, &
	                              label=trim(string) // 'verbose:'  ,rc=status )
       VERIFY_(STATUS)

       call ESMF_ConfigGetAttribute ( config, list(n)%vscale, default=1.0, &
	                              label=trim(string) // 'vscale:'  ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, list(n)%vunit, default="", &
	                              label=trim(string) // 'vunit:'  ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, list(n)%nbits, default=100, &
                                      label=trim(string) // 'nbits:' ,rc=status )
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute ( config, list(n)%deflate, default=0, &
                                      label=trim(string) // 'deflate:' ,rc=status )
       VERIFY_(STATUS)



!      Disable streams when frequencies, times are negative
!      ----------------------------------------------------
       if ( list(n)%frequency < 0 .OR. &
            list(n)%ref_date  < 0 .OR. &
            list(n)%ref_time  < 0 .OR. &
            list(n)%duration  < 0      )   list(n)%disabled = .true.

       call ESMF_ConfigFindLabel ( config,trim(string) // 'fields:',rc=STATUS )
       tend = .false.
       m = 0
       do while (.not.tend)
          m = m+1
          allocate( fields(3,m), stat=status )
          VERIFY_(STATUS)

#ifdef DO_COMMAS
          call ESMF_ConfigGetAttribute ( config,value=fields1,rc=STATUS)
          VERIFY_(STATUS)
          call ESMF_ConfigGetAttribute ( config,value=tmpstring ,rc=STATUS)
          VERIFY_(STATUS)
          call ESMF_ConfigGetAttribute ( config,value=fields2,rc=STATUS)
          VERIFY_(STATUS)
          call ESMF_ConfigGetAttribute ( config,value=tmpstring ,rc=STATUS)
          VERIFY_(STATUS)
          call ESMF_ConfigGetAttribute ( config,value=fields3,default=fields1,rc=STATUS)
!ALT: we should not check STATUS here since fields3 is optional and if missing the last call will return error
#else
          call ESMF_ConfigGetAttribute ( config,value=fields1,                rc=STATUS)
          VERIFY_(STATUS)
          call ESMF_ConfigGetAttribute ( config,value=fields2,                rc=STATUS)
          VERIFY_(STATUS)
          call ESMF_ConfigGetAttribute ( config,value=fields3,default=fields1,rc=STATUS)
!ALT: we should not check STATUS here since fields3 is optional and if missing the last call will return error
#endif
          call ESMF_ConfigNextLine  ( config,tableEnd=tend,rc=STATUS )
          VERIFY_(STATUS)
          if( m==1 ) then
             fields(1,m)     = fields1
             fields(2,m)     = fields2
             fields(3,m)     = fields3
             allocate( list(n)%fields(3,m), stat=status)
             VERIFY_(STATUS)
             list(n)%fields = fields
          else
             fields(1,1:m-1) = list(n)%fields(1,:)
             fields(2,1:m-1) = list(n)%fields(2,:)
             fields(3,1:m-1) = list(n)%fields(3,:)
             fields(1,m)     = fields1
             fields(2,m)     = fields2
             fields(3,m)     = fields3
             deallocate (list(n)%fields)
             allocate( list(n)%fields(3,m), stat=status )
             VERIFY_(STATUS)
             list(n)%fields = fields
          endif
          deallocate (fields)
       enddo

       list(n)%nfield = m

! Get an optional list of output levels
! -------------------------------------

       list(n)%vvars = ""
 
       len = ESMF_ConfigGetLen( config, trim(trim(string) // 'levels:'), rc = status )

       LEVELS: if( status == ESMF_SUCCESS ) then
          allocate( list(n)%levels(len), stat = status )  
          VERIFY_(STATUS)
          call ESMF_ConfigFindLabel( config, trim(trim(string) // 'levels:'), rc = status )
          VERIFY_(STATUS)
          do i = 1, len
             call ESMF_ConfigGetAttribute( config, list(n)%levels(i), rc = status )
             VERIFY_(STATUS)
          enddo

! Get an interpolating variable
! -----------------------------

          call ESMF_ConfigFindLabel ( config,trim(string) // 'vvars:',rc=STATUS )
          VINTRP: if( status == ESMF_SUCCESS ) then
             call ESMF_ConfigGetAttribute ( config,value=list(n)%vvars(1), rc=STATUS)
             VERIFY_(STATUS) 
#ifdef DO_COMMAS
             call ESMF_ConfigGetAttribute ( config,value=tmpstring,        rc=STATUS)
             VERIFY_(STATUS)
#endif
             call ESMF_ConfigGetAttribute ( config,value=list(n)%vvars(2), rc=STATUS)
             VERIFY_(STATUS)

! Add Vertical Coordinate Variables to Field List (if not already present)
! ------------------------------------------------------------------------

             list(n)%vvars(1) = trim(adjustl(list(n)%vvars(1)))
             vvar = adjustl(list(n)%vvars(1))
             if(vvar/="") then
                if    (Vvar(1:3)=='log') then
                   Vvar  = adjustl(Vvar(index(vvar,'(')+1:index(vvar,')')-1))
                elseif(Vvar(1:3)=='pow') then 
                   Vvar  = adjustl(Vvar(index(vvar,'(')+1:index(vvar,',')-1))
                endif

                do i=1,list(n)%nfield
                   found = list(n)%fields(1,i).eq.vvar   .and. &
                        list(n)%fields(2,i).eq.list(n)%vvars(2)
                   if(found)exit
                enddo

                if( .not.found ) then
                   list(n)%nfield = list(n)%nfield + 1
                   allocate( fields(3,  list(n)%nfield), stat=status )
                   fields(1,1:list(n)%nfield-1) = list(n)%fields(1,:)
                   fields(2,1:list(n)%nfield-1) = list(n)%fields(2,:)
                   fields(3,1:list(n)%nfield-1) = list(n)%fields(3,:)
                   fields(1,  list(n)%nfield  ) = Vvar
                   fields(2,  list(n)%nfield  ) = list(n)%vvars (2)
                   fields(3,  list(n)%nfield  ) = Vvar
                   deallocate( list(n)%fields, stat=status )
                   VERIFY_(STATUS)
                   list(n)%fields => fields
                endif
             end if
          endif VINTRP ! Vertical interp var

       endif LEVELS ! selected levels

       vvarn(n) = vvar

! Get an optional output resolution
! ---------------------------------
       list(n)%xyoffset = 0
       len = ESMF_ConfigGetLen( config, trim(trim(string) // 'resolution:'), rc = status )
       if( status == ESMF_SUCCESS ) then
          ASSERT_(len == 2)
          allocate( list(n)%resolution(len), stat = status )  
          VERIFY_(STATUS)
          call ESMF_ConfigFindLabel( config, trim(trim(string) // 'resolution:'), rc = status )
          VERIFY_(STATUS)
          do i = 1, size(list(n)%resolution)
             call ESMF_ConfigGetAttribute( config, list(n)%resolution(i), rc = status )
             VERIFY_(STATUS)
          enddo
          call ESMF_ConfigGetAttribute ( config, list(n)%xyoffset, default=0, &
                                         label=trim(string) // 'xyoffset:' ,rc=status )
          VERIFY_(STATUS)
       end if

! Get an optional tile file for regridding the output
! ---------------------------------------------------
       call ESMF_ConfigGetAttribute ( config, value=tilefile, default="", &
                                      label=trim(string) // 'regrid_exch:' ,rc=status )
       VERIFY_(STATUS)

       NULLIFY(IntState%Regrid(n)%PTR)
       if (tilefile /= '') then
          allocate(IntState%Regrid(n)%PTR, stat=status)
          VERIFY_(STATUS)
          IntState%Regrid(n)%PTR%tilefile = tilefile
          call ESMF_ConfigGetAttribute ( config, value=gridname, default="", &
                                         label=trim(string) // 'regrid_name:' ,rc=status )
          VERIFY_(STATUS)
          IntState%Regrid(n)%PTR%gridname = gridname
       end if
           
! Set Alarms
! ----------

       if (list(n)%disabled) cycle

! His and Seg Alarms based on Reference Date and Time
! ---------------------------------------------------
       REF_TIME(1) =     list(n)%ref_date/10000
       REF_TIME(2) = mod(list(n)%ref_date,10000)/100
       REF_TIME(3) = mod(list(n)%ref_date,100)
       REF_TIME(4) =     list(n)%ref_time/10000
       REF_TIME(5) = mod(list(n)%ref_time,10000)/100
       REF_TIME(6) = mod(list(n)%ref_time,100)
       
       call ESMF_TimeSet( RefTime, YY = REF_TIME(1), &
                                   MM = REF_TIME(2), &
                                   DD = REF_TIME(3), &
                                   H  = REF_TIME(4), &
                                   M  = REF_TIME(5), &
                                   S  = REF_TIME(6), calendar=cal, rc=rc )

       sec = nsecf( list(n)%frequency )
       call ESMF_TimeIntervalSet( Frequency, S=sec, calendar=cal, rc=status ) ; VERIFY_(STATUS)
       RingTime = RefTime

! Added Logic to eliminate BEG_DATE = cap_restart date problem
! ------------------------------------------------------------
       if (RefTime == startTime) then
           RingTime = RefTime + Frequency
       end if

       if (RingTime < currTime .and. sec /= 0 ) then
           RingTime = RingTime + (INT((currTime - RingTime)/frequency)+1)*frequency
       endif
       list(n)%his_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, rc=status )
       VERIFY_(STATUS)
       
       if( list(n)%duration.ne.0 ) then
          sec = nsecf( list(n)%duration )
          call ESMF_TimeIntervalSet( Frequency, S=sec, calendar=cal, rc=status ) ; VERIFY_(STATUS)
          RingTime = RefTime
          if (RingTime < currTime) then
              RingTime = RingTime + (INT((currTime - RingTime)/frequency)+1)*frequency
          endif
          list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, rc=status )
          VERIFY_(STATUS)
       else
          list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, RingTime=RingTime, rc=status )
          VERIFY_(STATUS)
       endif

! Mon Alarm based on 1st of Month 00Z
! -----------------------------------
       REF_TIME(1) =     list(n)%ref_date/10000
       REF_TIME(2) = mod(list(n)%ref_date,10000)/100
       REF_TIME(3) = 1
       REF_TIME(4) = 0
       REF_TIME(5) = 0
       REF_TIME(6) = 0

       call ESMF_TimeSet( RefTime, YY = REF_TIME(1), &
                                   MM = REF_TIME(2), &
                                   DD = REF_TIME(3), &
                                   H  = REF_TIME(4), &
                                   M  = REF_TIME(5), &
                                   S  = REF_TIME(6), calendar=cal, rc=rc )

       call ESMF_TimeIntervalSet( Frequency, MM=1, calendar=cal, rc=status ) ; VERIFY_(STATUS)
       RingTime = RefTime
       do while ( RingTime < currTime )
          RingTime = RingTime + Frequency
       enddo
       list(n)%mon_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, rc=status )
       VERIFY_(STATUS)
       
! End Alarm based on end_date and end_time
! ----------------------------------------
       if( list(n)%end_date.ne.-999 .and. list(n)%end_time.ne.-999 ) then
           REF_TIME(1) =     list(n)%end_date/10000
           REF_TIME(2) = mod(list(n)%end_date,10000)/100
           REF_TIME(3) = mod(list(n)%end_date,100)
           REF_TIME(4) =     list(n)%end_time/10000
           REF_TIME(5) = mod(list(n)%end_time,10000)/100
           REF_TIME(6) = mod(list(n)%end_time,100) + 1 ! Add 1 second to make end_time inclusive
       
           call ESMF_TimeSet( RingTime, YY = REF_TIME(1), &
                                        MM = REF_TIME(2), &
                                        DD = REF_TIME(3), &
                                        H  = REF_TIME(4), &
                                        M  = REF_TIME(5), &
                                        S  = REF_TIME(6), calendar=cal, rc=rc )

           list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=RingTime, rc=status )
           VERIFY_(STATUS)
        else
           list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=CurrTime, rc=status )
           VERIFY_(STATUS)
           call  ESMF_AlarmRingerOff(list(n)%end_alarm, rc=status )
           VERIFY_(STATUS)
       endif
       
    enddo LISTLOOP

! Extract List of Unique Export State Names
! -----------------------------------------

    size0 = 1 !size( export )
    nstatelist = size0
    allocate( statelist(size0) )
    n = 1
    call ESMF_StateGet ( import,name=string,rc=status )
    VERIFY_(STATUS)
    k = index( string,'_' )-1
    statelist(n) = string(1:k)

    
    do n=1,nlist
       do m=1,list(n)%nfield
          k=1
          do while ( k.le.nstatelist )
             if( statelist(k).ne.list(n)%fields(2,m)) then
                k=k+1
             else
                exit
             end if
          enddo
          if(k.eq.nstatelist+1) then
             allocate( tmplist (nstatelist) )
             tmplist = statelist
             nstatelist = k
             deallocate( statelist )
             allocate( statelist(nstatelist) )
             statelist(1:k-1) = tmplist
             statelist(k)     = list(n)%fields(2,m)
             deallocate(   tmplist )
          endif
       enddo
    enddo
    
    if( MAPL_AM_I_ROOT() ) then
       print *
       print *, 'Independent Output Export States:'
       print *, '---------------------------------'
       do n=1,nstatelist
          print *, n,trim(statelist(n))
       enddo
    endif

! Get Output Export States
! ------------------------

    allocate ( exptmp (size0), stat=status )
    VERIFY_(STATUS)
    exptmp(1) = import
!    deallocate ( export )
    allocate ( export(nstatelist), stat=status )
    VERIFY_(STATUS)
    errorFound = .false.
    do n=1,nstatelist
       call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),rc=status )
       if( STATUS/= ESMF_SUCCESS ) then
          call WRITE_PARALLEL('Cannot Find ' // trim(statelist(n)))
          errorFound = .true.
          status=ESMF_SUCCESS
       endif

    enddo
    if(errorFound) then
       call ESMF_VMBarrier(vm, rc=status)
       RETURN_(ESMF_FAILURE)
    end if
    deallocate ( exptmp )

! Associate Output Names with EXPORT State Index
! ----------------------------------------------
    do n=1,nlist
       allocate( list(n)%expSTATE(list(n)%nfield) )
       do m=1,list(n)%nfield
          do k=1,nstatelist
             if( trim(list(n)%fields(2,m)) .eq. trim(statelist(k)) ) list(n)%expSTATE(m) = k
          enddo
       enddo
    enddo
    deallocate( statelist )

! Ensure Diagnostic Output has been Allocated
! -------------------------------------------
    errorFound = .false.
    do n=1,nlist
       if (list(n)%disabled) cycle
       do m=1,list(n)%nfield
          call ESMFL_StateGetFieldArray( export(list(n)%expSTATE(m)),trim(list(n)%fields(1,m)),array,status )
          IF (STATUS /= ESMF_SUCCESS) then
             call WRITE_PARALLEL( "ERROR: cannot find output " // &
                  trim(list(n)%fields(1,m)) // " in " // &
                  trim(list(n)%fields(2,m)))
             errorFound = .true.
             status=ESMF_SUCCESS
          else
             call ESMF_ArrayGet( array, RANK=rank, rc=status)
             VERIFY_(STATUS)
             if( rank==1 ) then
                call ESMFL_StateGetPointerToData( export(list(n)%expSTATE(m)), Q1D, &
                     trim(list(n)%fields(1,m)),alloc=.true.,rc=status )
                VERIFY_(STATUS)
             endif
             if( rank==2 ) then
                call ESMFL_StateGetPointerToData( export(list(n)%expSTATE(m)), Q2D, &
                     trim(list(n)%fields(1,m)),alloc=.true.,rc=status )
                VERIFY_(STATUS)
             endif
             if( rank==3 ) then
                call ESMFL_StateGetPointerToData( export(list(n)%expSTATE(m)), Q3D, &
                     trim(list(n)%fields(1,m)),alloc=.true.,rc=status )
                VERIFY_(STATUS)
             endif
          end IF
       enddo
    enddo

    if (errorFound) then
       call ESMF_VMBarrier(vm, rc=status)
       RETURN_(ESMF_FAILURE)
    end if

   allocate(INTSTATE%AVERAGE    (nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%STAMPOFFSET(nlist), stat=status)
   VERIFY_(STATUS)

   do n=1, nlist
         if (list(n)%disabled) cycle
            if(list(n)%mode == "instantaneous") then
         IntState%average(n) = .false.
         sec = 0
      else
         IntState%average(n) = .true.
         sec = nsecf(list(n)%acc_interval) / 2
      endif
      call ESMF_TimeIntervalSet( INTSTATE%STAMPOFFSET(n), S=sec, rc=status )
      VERIFY_(STATUS)
   end do

   allocate(INTSTATE%CCS(nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%GIM(nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%CIM(nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%SRCS(nlist), stat=status)
   VERIFY_(STATUS)
   allocate(INTSTATE%DSTS(nlist), stat=status)
   VERIFY_(STATUS)
!   allocate(INTSTATE%GEX(nlist), stat=status)
!   VERIFY_(STATUS)
!   allocate(INTSTATE%GCNameList(nlist), stat=status)
!   VERIFY_(STATUS)

! Initialize Logical for Grads Control File
! -----------------------------------------

   allocate( INTSTATE%LCTL(nlist), stat=status )
   VERIFY_(STATUS)
   do n=1,nlist
      if (list(n)%disabled) cycle
      if( list(n)%format == 'flat' ) then
         INTSTATE%LCTL(n) = .true.
      else
         INTSTATE%LCTL(n) = .false.
      endif
   enddo

   do n=1, nlist
      if (list(n)%disabled) cycle
      
      IntState%GIM(n) = ESMF_StateCreate ( trim(list(n)%filename), &
                                                 ESMF_STATE_IMPORT, rc=status )
      VERIFY_(STATUS)
      if(list(n)%mode == "instantaneous") then
         IntState%average(n) = .false.
      else
         IntState%average(n) = .true.
         IntState%CIM(n) = ESMF_StateCreate ( trim(list(n)%filename), &
                                                    ESMF_STATE_IMPORT, rc=status )
         VERIFY_(STATUS)
         NULLIFY(INTSTATE%SRCS(n)%SPEC)
         NULLIFY(INTSTATE%DSTS(n)%SPEC)
      endif

      if (associated(IntState%Regrid(n)%PTR)) then
! query a field from export (arbitrary first field in the stream) for grid_in
         ASSERT_(size(export(list(n)%expSTATE)) > 0)
         call ESMF_StateGet( export(list(n)%expSTATE(1)), &
                             trim(list(n)%fields(1,1)), field, rc=status )
         VERIFY_(STATUS)
         IntState%Regrid(n)%PTR%state_out = ESMF_StateCreate ( trim(list(n)%filename)//'regrid_in', &
                                                 ESMF_STATE_IMPORT, rc=status )
         VERIFY_(STATUS)

! get grid name, layout, dims
         call ESMF_FieldGet(field, grid=grid_in, rc=status)
         VERIFY_(STATUS)
         call ESMF_GridGet(grid_in, name=gridname, distgrid=distgrid, rc=status)
         VERIFY_(STATUS)
         call ESMF_DistGridGet(distgrid, delayout=layout, rc=status)
         VERIFY_(STATUS)

         call MAPL_LocStreamCreate(exch, &
              layout, FILENAME=IntState%Regrid(n)%PTR%TILEFILE, &
              NAME='history_exch', RC=STATUS)
         VERIFY_(STATUS)
         call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locIn, &
              exch, NAME='history_in', MASK=(/MAPL_Ocean/), RC=STATUS)
         VERIFY_(STATUS)
         call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locOut, &
              exch, NAME='history_out',MASK=(/MAPL_Ocean/), RC=STATUS)
         VERIFY_(STATUS)

!        get gridnames from loc_in
         call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
              GRIDNAMES = GNAMES, RC=STATUS)
         VERIFY_(STATUS)
! query loc_in for ngrids
         ngrids = size(gnames)
         ASSERT_(ngrids==2)

         use_this_gridname = .false.
         ontiles = .false.
         IntState%Regrid(n)%PTR%noxform = .false.
         if (gridname(1:10) == 'tile_grid_') then
            ontiles = .true.
            ASSERT_(IntState%Regrid(n)%PTR%gridname /= '')

            call MAPL_Get(GENSTATE, ExchangeGrid=exch, rc=status)
            VERIFY_(STATUS)
            call MAPL_LocStreamGet(EXCH, GRIDS=GRIDS, RC=STATUS)
            VERIFY_(STATUS)

            ! save the "gridname"
            tmpstr = gridname
            I = INDEX(tmpstr,'@')
            ASSERT_(I>0 .and. I < ESMF_MAXSTR)
            gridname = tmpstr(I+1:)

            if (gridname == IntState%Regrid(n)%PTR%gridname) then
               IntState%Regrid(n)%PTR%noxform = .true.
            else
               found = .false.
               DO I = 1, NGRIDS
                  IF (GNAMES(I) == gridname) THEN
                     FOUND = .TRUE.
                     exit
                  ENDIF
               ENDDO
               ASSERT_(FOUND)

!  we need a new grid_in (same layout as original, same ims, jms), counts=gridim(I),gridjm(I)
               grid_in = grids(i)
!ALT????               IntState%Regrid(n)%PTR%locin = exch
               call ESMF_GridGet(grid_in, distgrid=distgrid, rc=status)
               VERIFY_(STATUS)
               call ESMF_DistGridGet(distgrid, delayout=layout, rc=status)
               VERIFY_(STATUS)

               gridname = IntState%Regrid(n)%PTR%gridname
            end if
            use_this_gridname = .true.
         end if
         IntState%Regrid(n)%PTR%ontiles = ontiles
! validate that gridname_in is there
         found = .false.
         DO I = 1, NGRIDS
            IF (GNAMES(I) == GRIDNAME) THEN
               FOUND = .TRUE.
               exit
            ENDIF
         ENDDO
         ASSERT_(FOUND)

! pick gridname_out
         if (use_this_gridname) then
            NG = I
         else
! we pick the "other" gridname. this works only when ngrids==2; 3-1=2;3-2=1
            NG = 3 - I 
         end if
! create grid_out
         if (.not. ontiles) then
            call MAPL_GridGet(GRID, globalCellCountPerDim=COUNTS, RC=STATUS)

            LM = COUNTS(3)

!ALT: for now we parse the grid name to figure out the origin of the grid
            POLE = GNAMES(NG)(1:2)
            NMLEN = LEN_TRIM(GNAMES(NG))
            DTLN = GNAMES(NG)(NMLEN-1:NMLEN)
         
            call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
                 GRIDIM = GRIDIM, GRIDJM=GRIDJM, RC=STATUS)
            VERIFY_(STATUS)

            DeltaX = 2.0*MAPL_PI/GRIDIM(NG)
            DeltaZ = 1.0D0
            select case (DTLN)
            case ('DE')
               X0 = -MAPL_PI                 ! dateline edge
            
            case ('DC')
               X0 = -MAPL_PI - DeltaX/2      ! dateline center
            
            case default
               RETURN_(ESMF_FAILURE)
            end select

            select case (POLE)
            case ('PE')
               DeltaY = MAPL_PI/GRIDJM(NG)
               Y0 = -0.5*MAPL_PI             ! South Pole edge
            
            case ('PC')
               DeltaY = MAPL_PI/(GRIDJM(NG)-1)
               Y0 = -0.5*MAPL_PI - DeltaY/2  ! South Pole center

            case default
               RETURN_(ESMF_FAILURE)
            end select

#if 0
            call ESMF_DELayoutGet(LAYOUT, deCountPerDim = DECOUNT, &
                 RC=STATUS)
            VERIFY_(STATUS)

            NX = DECOUNT(1)
            NY = DECOUNT(2)


! Get the IMXY vector
! -------------------
            allocate( imxy(0:nx-1) )  
            call MAPL_DecomposeDim ( GRIDIM(NG),imxy,nx )

! Get the JMXY vector
! -------------------
            allocate( jmxy(0:ny-1) )  
            call MAPL_DecomposeDim ( GRIDJM(NG),jmxy,ny )

            grid_out = ESMF_GridCreateHorzLatLonUni(     &
                 counts = (/GRIDIM(NG),GRIDJM(NG)/),     &
                 minGlobalCoordPerDim=(/X0, Y0/),        &
                 deltaPerDim=(/deltaX, deltaY /),        &
                 horzStagger=ESMF_Grid_Horz_Stagger_A,   &
                 periodic=(/ESMF_TRUE, ESMF_FALSE/),     &
                 name=GNAMES(NG), rc=status)
            VERIFY_(STATUS)

            call ESMF_GridAddVertHeight(grid_out,        &
                 delta=(/(deltaZ, L=1,LM) /),            &
                 vertStagger=ESMF_GRID_VERT_STAGGER_CENTER, &
                 rc=status)
            VERIFY_(STATUS)

            call ESMF_GridDistribute(grid_out, deLayout=layout, &
                 countsPerDEDim1=imxy,                   &
                 countsPerDEDim2=jmxy,                   &
                 rc=status)
            VERIFY_(STATUS)
#else
            RETURN_(-1)
#endif
            deallocate(jmxy, imxy)
         else
            grid_out = grids(NG)
         endif

! attach loc_out to grid_out
         call MAPL_LocStreamAttachGrid(IntState%Regrid(n)%PTR%locOut, &
              GRID_OUT, NSUBTILES=1, RC=STATUS)
         VERIFY_(STATUS)
! query ntiles
         call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locOut, &
              NT_LOCAL = IntState%Regrid(n)%PTR%ntiles_out, rc=status)
         VERIFY_(STATUS)

         if (.not.INTSTATE%Regrid(n)%PTR%noxform) then
! attach loc_in to grid_in
            call MAPL_LocStreamAttachGrid(IntState%Regrid(n)%PTR%locIn, &
                 GRID_IN, NSUBTILES=1, RC=STATUS)
            VERIFY_(STATUS)
! query ntiles
            call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
                 NT_LOCAL = IntState%Regrid(n)%PTR%ntiles_in, rc=status)
            VERIFY_(STATUS)

! create XFORM
            call MAPL_LocStreamCreateXform ( XFORM=INTSTATE%Regrid(n)%PTR%XFORM, &
                 LocStreamOut=INTSTATE%Regrid(n)%PTR%LocOut, &
                 LocStreamIn=INTSTATE%Regrid(n)%PTR%LocIn, &
                 NAME='historyXFORM', &
                 RC=STATUS )
            VERIFY_(STATUS)
         end if

      endif

      do m=1,list(n)%nfield
         call ESMF_StateGet( export(list(n)%expSTATE(m)), &
                             trim(list(n)%fields(1,m)), field, rc=status )
         VERIFY_(STATUS)

         f = MAPL_FieldCreate(field, name=list(n)%fields(3,m), rc=status)
         VERIFY_(STATUS)

         if (IntState%average(n)) then
            call ESMF_StateAdd(IntState%CIM(N), f, rc=status)
            VERIFY_(STATUS)

            ! borrow SPEC from FIELD
            ! modify SPEC to reflect accum/avg
            call ESMF_FieldGet(f, name=short_name, grid=grid, rc=status)
            VERIFY_(STATUS)

            call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='VLOCATION', VALUE=VLOCATION, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
            VERIFY_(STATUS)

            call ESMF_AttributeGet(FIELD, NAME='REFRESH_INTERVAL', VALUE=REFRESH, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='AVERAGING_INTERVAL', VALUE=avgint, RC=STATUS)
            VERIFY_(STATUS)

            call MAPL_VarSpecCreateInList(INTSTATE%SRCS(n)%SPEC,    &
                 SHORT_NAME = SHORT_NAME,                                 &
                 LONG_NAME  = LONG_NAME,                                  &
                 UNITS      = UNITS,                                      &
                 DIMS       = DIMS,                                       &
                 ACCMLT_INTERVAL= avgint,                                 &
                 COUPLE_INTERVAL= REFRESH,                                &
                 VLOCATION  = VLOCATION,                                  &
                 RC=STATUS  )
            VERIFY_(STATUS)

            call MAPL_VarSpecCreateInList(INTSTATE%DSTS(n)%SPEC,    &
                 SHORT_NAME = list(n)%fields(3,m),                        &
                 LONG_NAME  = LONG_NAME,                                  &
                 UNITS      = UNITS,                                      &
                 DIMS       = DIMS,                                       &
                 ACCMLT_INTERVAL= nsecf(list(n)%acc_interval),            &
                 COUPLE_INTERVAL= nsecf(list(n)%frequency   ),            &
                 VLOCATION  = VLOCATION,                                  &
                 GRID       = GRID,                                       &
                 RC=STATUS  )
            VERIFY_(STATUS)

         else

            REFRESH = nsecf(list(n)%acc_interval)
            AVGINT  = nsecf( list(n)%frequency )
            call ESMF_AttributeSet(F, NAME='REFRESH_INTERVAL', VALUE=REFRESH, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_AttributeSet(F, NAME='AVERAGING_INTERVAL', VALUE=AVGINT, RC=STATUS)
            VERIFY_(STATUS)
            call ESMF_StateAdd(IntState%GIM(N), f, rc=status)
            VERIFY_(STATUS)

         endif


! Handle possible regridding through user supplied exchange grid
!---------------------------------------------------------------
         if (associated(IntState%Regrid(n)%PTR)) then
! replace field with newly create fld on grid_out
            field = MAPL_FieldCreate(f, grid_out, rc=status)
! add field to state_out
            call ESMF_StateAdd(IntState%Regrid(N)%PTR%state_out, &
                 field, rc=status)
            VERIFY_(STATUS)
         endif

      end do

      if (IntState%average(n)) then
         
         call MAPL_StateCreateFromSpec(IntState%GIM(n), &
              IntState%DSTS(n)%SPEC,   &
              RC=STATUS  )
         VERIFY_(STATUS)

!         create CC
         IntState%CCS(n) = ESMF_CplCompCreate (                        &
              NAME       = 'History', & 
              RC=STATUS )
         VERIFY_(STATUS)


!         CCSetServ
         call ESMF_CplCompSetServices (IntState%CCS(n), &
                                       GenericCplSetServices, STATUS )
         VERIFY_(STATUS)

         call MAPL_CplCompSetVarSpecs(IntState%CCS(n), &
                                      INTSTATE%SRCS(n)%SPEC,&
                                      INTSTATE%DSTS(n)%SPEC,RC=STATUS)
         VERIFY_(STATUS)

!         CCInitialize
         call ESMF_CplCompInitialize (INTSTATE%CCS(n), &
                                      INTSTATE%CIM(n), &
                                      INTSTATE%GIM(n), &
                                      CLOCK,                 &
                                      RC=STATUS)
         VERIFY_(STATUS)
      end if

   end do

! CFIO
    do n=1,nlist
       if (list(n)%disabled) cycle

       if (list(n)%format == 'CFIO') then
          if (associated(IntState%Regrid(n)%PTR)) then
             print *,'Regridding feature not yet supported for CFIO'
             RETURN_(ESMF_FAILURE)
          end if
          write(string,'(a,i3.0)') 'STREAM',n

          list(n)%bundle = ESMF_FieldBundleCreate(NAME=string, RC=STATUS)
          VERIFY_(STATUS)

          if(associated(list(n)%levels)) then
             LM = size(list(n)%levels)
          else
             call ESMF_StateGet(INTSTATE%GIM(n), &
                  trim(list(n)%fields(3,1)), field, rc=status )
             VERIFY_(STATUS)
             call ESMF_FieldGet(field, grid=grid,   rc=status )
             VERIFY_(STATUS)
             call MAPL_GridGet(GRID, globalCellCountPerDim=COUNTS, RC=STATUS)
             VERIFY_(STATUS)
             LM = counts(3)
          endif

          list(n)%slices = 0

          do m=1,list(n)%nfield
             call ESMF_StateGet( INTSTATE%GIM(n), &
                  trim(list(n)%fields(3,m)), field, rc=status )
             VERIFY_(STATUS)

             call ESMF_FieldBundleAdd( list(n)%bundle, field, rc=status )
             VERIFY_(STATUS)

             call ESMF_FieldGet(field, Array=array, rc=status)
             VERIFY_(STATUS)
             call ESMF_ArrayGet(array, rank=rank, rc=status)
             VERIFY_(STATUS)

             if(list(n)%fields(3,m)/=vvarn(n)) then
                if(rank==2) then
                   list(n)%slices = list(n)%slices + 1
                elseif(rank==3) then
                   list(n)%slices = list(n)%slices + LM
                endif
             endif
          end do

       endif
    enddo

    deallocate(Vvarn)

    do n=1,nlist
       if (list(n)%format == 'CFIO') then
          call MAPL_CFIOCreate(                        &
               MCFIO      = list(n)%MCFIO,             &
               NAME       = trim(list(n)%filename),    &
               CLOCK      = clock,                     &
               BUNDLE     = list(n)%bundle,            &
               OFFSET     = IntState%stampoffset(n),   &
               RESOLUTION = list(n)%resolution,        &
               FREQUENCY  = nsecf(list(n)%frequency),  &
               LEVELS     = list(n)%levels,            &
               DESCR      = list(n)%descr,             &
               XYOFFSET   = list(n)%xyoffset,          &
               VCOORD     = list(n)%VVARS(1),          &
               VUNIT      = list(n)%VUNIT,             &
               VSCALE     = list(n)%VSCALE,            &
               DEFLATE    = list(n)%deflate,           &
               NBITS      = list(n)%nbits,             &
               NUMCORES   = IntState%CoresPerNode,     &
                                             RC=status )
          VERIFY_(STATUS)
       end if
   end do

! Echo History List Data Structure
! --------------------------------

   if( MAPL_AM_I_ROOT() ) then
      print *
      do n=1,nlist
         if (list(n)%disabled) cycle
         print *, 'Initializing Output Stream: ',  trim(list(n)%filename)
         print *, '--------------------------- '
         print *, '      Format: ',  trim(list(n)%format)
         print *, '        Mode: ',  trim(list(n)%mode)
         print *, '       Nbits: ',       list(n)%nbits
         print *, '      Slices: ',       list(n)%Slices
         print *, '     Deflate: ',       list(n)%deflate
         print *, '   Frequency: ',       list(n)%frequency
         if(IntState%average(n) ) &
              print *, 'Acc_Interval: ',  list(n)%acc_interval
         print *, '    Ref_Date: ',       list(n)%ref_date
         print *, '    Ref_Time: ',       list(n)%ref_time
         print *, '    Duration: ',       list(n)%duration
         if( list(n)%end_date.ne.-999 ) then
         print *, '    End_Date: ',       list(n)%end_date
         print *, '    End_Time: ',       list(n)%end_time
         endif

         if( associated  ( list(n)%resolution ) ) then
                           print *, ' Output RSLV: ',list(n)%resolution
         endif
         select case ( list(n)%xyoffset   )
                case (0)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DcPc: Dateline Center, Pole Center)'
                case (1)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DePc: Dateline Edge, Pole Center)'
                case (2)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DcPe: Dateline Center, Pole Edge)'
                case (3)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DePe: Dateline Edge, Pole Edge)'
                case default
                ASSERT_(.false.)
         end select

         print *, '      Fields: ',(trim(list(n)%fields(3,m))//' ',m=1,list(n)%nfield)

         if( list(n)%vvars(1)/="" ) then
                                           print *, '   Vert Interp  Var: ',  trim(list(n)%vvars(1))
            if( trim(list(n)%vunit)/=""  ) print *, '   Vertical    Unit: ',  trim(list(n)%vunit)
            if(      list(n)%vscale/=1.0 ) print *, '   Vertical Scaling: ',       list(n)%vscale
                                           print *, '   Vertical  Levels: ',       list(n)%levels
         elseif(associated(list(n)%levels)) then
                                           print *, '   Vertical  Levels: ',  nint(list(n)%levels)
         endif

         print *
         print *
      enddo
   endif


    call MAPL_TimerOff(GENSTATE,"Initialize")
    call MAPL_TimerOff(GENSTATE,"TOTAL")

   RETURN_(ESMF_SUCCESS)
 end subroutine Initialize

!======================================================


 subroutine Run ( gc, import, export, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp),    intent(inout) :: gc     
    type(ESMF_State),       intent(inout) :: import 
    type(ESMF_State),       intent(inout) :: export 
    type(ESMF_Clock),       intent(inout) :: clock  
    integer, optional,      intent(  out) :: rc     
                                                    
                                                    
! Locals

    type(MAPL_MetaComp),  pointer  :: GENSTATE
    type(history_list),   pointer  :: list(:)
    type(HISTORY_STATE),  pointer  :: IntState
    type(HISTORY_wrap)             :: wrap
    integer                        :: nlist
    character(len=ESMF_MAXSTR)     :: fntmpl
    character(len=ESMF_MAXSTR),pointer     :: filename(:)
    integer                        :: n,m
    logical                        :: NewSeg
    logical, allocatable           :: Writing(:)
    type(ESMF_State)               :: state_out
    integer                        :: nymd, nhms
!    integer                        :: nsecf, nymd, nhms
    character(len=ESMF_MAXSTR)     :: DateStamp

    character(len=ESMF_MAXSTR)     :: IAm="HistoryRun" 
    integer                        :: status

!=============================================================================

! Begin...

! Retrieve the pointer to the state
!----------------------------------

    call ESMF_GridCompGetInternalState(gc, wrap, status)
    VERIFY_(status)
    IntState => wrap%ptr

! the collections
!----------------

    list => IntState%list
    nlist = size(list)

! Retrieve the pointer to the generic state
!------------------------------------------

    call MAPL_GetObjectFromGC ( gc, GENSTATE, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerOn(GENSTATE,"TOTAL")
    call MAPL_TimerOn(GENSTATE,"Run"  )

! Couplers are done here for now
!-------------------------------

    call MAPL_TimerOn(GENSTATE,"--Couplers")
    do n = 1, nlist
       if (.not.list(n)%disabled .and. IntState%average(n)) then
          
          call ESMF_CplCompRun (INTSTATE%CCS(n), &
                                INTSTATE%CIM(n), &
                                INTSTATE%GIM(n), &
                                CLOCK,                 &
                                RC=STATUS)
          VERIFY_(STATUS)
       end if
    end do
    call MAPL_TimerOff(GENSTATE,"--Couplers")
            
! Check for History Output
! ------------------------

   allocate(Writing (nlist), stat=status)
   VERIFY_(STATUS)
   allocate(filename(nlist), stat=status)
   VERIFY_(STATUS)

   do n=1,nlist
      if (list(n)%disabled .or. ESMF_AlarmIsRinging(list(n)%end_alarm) ) then
         list(n)%disabled = .true.
         Writing(n) = .false.
      else
         Writing(n) = ESMF_AlarmIsRinging ( list(n)%his_alarm )
      endif

      call ESMF_AlarmRingerOff( list(n)%his_alarm,rc=status )
      VERIFY_(STATUS)
   end do

! Write Id and time

   call get_DateStamp (clock, ExpId=INTSTATE%expid, rc=status)
   VERIFY_(STATUS)

   IOTIME: if(any(Writing)) then

      call MAPL_TimerOn(GENSTATE,"--I/O")

      call WRITE_PARALLEL("")

      call SetPsizeStrategy(list(:)%Slices, Writing, IntState%CoresPerNode, &
                            IntState%npes, list(:)%Psize,  list(:)%Root     )
      
      do n=1,nlist
         if( Writing(n) ) then

            call get_DateStamp ( clock, DateStamp=DateStamp,  &
                 OFFSET = INTSTATE%STAMPOFFSET(n),            &
                                                    rc=status )
            VERIFY_(STATUS)

            if (trim(INTSTATE%expid) == "") then
               fntmpl =          trim(list(n)%filename)
            else
               fntmpl = "%s." // trim(list(n)%filename)
            endif

            if (trim(list(n)%template) /= "") then
               fntmpl = trim(fntmpl) // "." //trim(list(n)%template)
            endif

            read(DateStamp( 1: 8),'(i8.8)') nymd
            read(DateStamp(10:15),'(i6.6)') nhms

            call StrTemplate ( filename(n), fntmpl, 'GRADS', &
                 xid=trim(INTSTATE%expid), &
                 nymd=nymd, nhms=nhms, stat=status )
            VERIFY_(STATUS)

         endif
      enddo


      call WRITE_PARALLEL("")
      call MAPL_TimerOn(GENSTATE,"----IO Create")
      VERIFY_(STATUS)

      OPENLOOP: do n=1,nlist
         if( Writing(n) ) then

            call MAPL_CFIOSet(list(n)%mCFIO, Root=list(n)%Root, &
                              Psize=list(n)%Psize,     rc=status)
            VERIFY_(STATUS)

! Check for new segment
!----------------------

            NewSeg = ESMF_AlarmIsRinging ( list(n)%seg_alarm )

            if( NewSeg) then 
               call ESMF_AlarmRingerOff( list(n)%seg_alarm,rc=status )
               VERIFY_(STATUS)
            endif

            if( NewSeg .and. list(n)%unit /= 0 .and. list(n)%duration /= 0 ) then
               if (list(n)%unit > 0 ) then
                  call FREE_FILE( list(n)%unit )
               end if
               list(n)%unit = 0
            endif

            if( list(n)%unit.eq.0 ) then
               if (list(n)%format == 'CFIO') then
                  call MAPL_CFIOSet( list(n)%MCFIO, fName=filename(n), RC=status )
                  VERIFY_(STATUS)
                  call MAPL_CFIOCreateWrite(list(n)%MCFIO, RC=status )
                  VERIFY_(STATUS)
                  list(n)%unit = -1
               else
                  list(n)%unit = GETFILE( trim(filename(n)))
               end if
            end if
            if(  MAPL_AM_I_ROOT() ) then
               write(6,'(1X,3i6,a)') list(n)%Slices, &
                    list(n)%Psize, list(n)%Root, "  " // trim(MAPL_CFIOGetFilename(list(n)%MCFIO))
            endif
         end if

      enddo OPENLOOP
      call MAPL_TimerOff(GENSTATE,"----IO Create")

      call WRITE_PARALLEL("")

      call MAPL_TimerOn(GENSTATE,"----IO Write")
      call MAPL_TimerOn(GENSTATE,"-----IO Post")
      POSTLOOP: do n=1,nlist

         OUTTIME: if( Writing(n) ) then

            IOTYPE: if (list(n)%unit < 0) then    ! CFIO 
               call MAPL_CFIOWriteBundlePost( list(n)%MCFIO,  RC=status)
               VERIFY_(STATUS)

            else

               if (associated(IntState%Regrid(n)%PTR)) then
                  state_out = INTSTATE%REGRID(n)%PTR%state_out
               else
                  state_out = INTSTATE%GIM(n)
               end if

               if( INTSTATE%LCTL(n) ) then
                  call MAPL_GradsCtlWrite ( clock, state_out, list(n), &
                       filename(n), INTSTATE%expid, &
                       list(n)%descr, rc )
                  INTSTATE%LCTL(n) = .false.
               endif

               do m=1,list(n)%nfield

                  if (associated(IntState%Regrid(n)%PTR)) then
                     if (.not. IntState%Regrid(n)%PTR%ontiles) then
                        call RegridTransform(IntState%GIM(n), &
                             IntState%Regrid(n)%PTR%xform, &
                             state_out, &
                             IntState%Regrid(n)%PTR%LocIn, &
                             IntState%Regrid(n)%PTR%LocOut, &
                             IntState%Regrid(n)%PTR%ntiles_in, &
                             IntState%Regrid(n)%PTR%ntiles_out,&
                             rc=status)
                     else
                        if (IntState%Regrid(n)%PTR%noxform) then
                           call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                                STATE_OUT=state_out, &
                                LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                                NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                                rc=status)
                        else
                           call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                                XFORM=IntState%Regrid(n)%PTR%xform, &
                                STATE_OUT=state_out, &
                                LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                                NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                                rc=status)
                        end if
                     end if
                     VERIFY_(STATUS)
                  end if
                  call MAPL_VarWrite ( list(n)%unit, STATE=state_out, &
                       NAME=trim(list(n)%fields(3,m)), rc=status ); VERIFY_(STATUS)
               enddo
               call WRITE_PARALLEL("Wrote GrADS Output for File: "//trim(filename(n)))

            end if IOTYPE

         endif OUTTIME

      enddo POSTLOOP

      call MAPL_TimerOff(GENSTATE,"-----IO Post")
      call MAPL_TimerOff(GENSTATE,"----IO Write")

      call WRITE_PARALLEL("")

      call MAPL_TimerOn(GENSTATE,"----IO Write")
      call MAPL_TimerOn(GENSTATE,"-----IO Wait")

      WAITLOOP: do n=1,nlist

         if( Writing(n) .and. list(n)%unit < 0) then
            if (list(n)%unit == -2) then
               call MAPL_CFIOOpenWrite(list(n)%MCFIO, RC=status)
            endif

            call MAPL_CFIOWriteBundleWait(list(n)%MCFIO, CLOCK, RC=status)
            VERIFY_(STATUS)

            call MAPL_CFIOClose (list(n)%mcfio, rc=status)
            VERIFY_(STATUS)

            list(n)%unit = -2

         end if

      enddo WAITLOOP

      call WRITE_PARALLEL("")

      call MAPL_TimerOff(GENSTATE,"-----IO Wait")
      call MAPL_TimerOff(GENSTATE,"----IO Write")
      call MAPL_TimerOff(GENSTATE,"--I/O"       )

   endif IOTIME

   deallocate(filename)
   deallocate(Writing)

   call MAPL_TimerOff(GENSTATE,"Run"         )
   call MAPL_TimerOff(GENSTATE,"TOTAL"       )

   RETURN_(ESMF_SUCCESS)
 end subroutine Run

!======================================================

  subroutine Finalize ( gc, import, export, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout)    :: gc     ! composite gridded component 
    type(ESMF_State),       intent(inout) :: import ! import state
    type(ESMF_State),       intent(  out) :: export ! export state
    type(ESMF_Clock),       intent(inout) :: clock  ! the clock
  
    integer, intent(out), OPTIONAL        :: rc     ! Error code:
                                                     ! = 0 all is well
                                                     ! otherwise, error

    character(len=ESMF_MAXSTR)      :: IAm="Finalize" 
    integer                         :: status
    type(history_list), pointer     :: list(:)
    type(HISTORY_wrap)              :: wrap
    type (HISTORY_STATE), pointer   :: IntState
    integer                         :: nlist, n
    type (MAPL_MetaComp), pointer :: GENSTATE

! Begin...

    call MAPL_GetObjectFromGC ( gc, GENSTATE, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerOn(GENSTATE,"TOTAL")
    call MAPL_TimerOn(GENSTATE,"Finalize")

! Retrieve the pointer to the state

    call ESMF_GridCompGetInternalState(gc, wrap, status)
    VERIFY_(status)
    IntState => wrap%ptr
    list => IntState%list
    nlist = size(list)

! Close UNITs of GEOSgcm History Data
! -----------------------------------

   do n=1,nlist
      if (list(n)%disabled) cycle
      IF (list(n)%format == 'CFIO' .or. list(n)%format == 'GRADS') then
         if( MAPL_CFIOIsCreated(list(n)%mcfio) ) then
            CALL MAPL_CFIOdestroy (list(n)%mcfio, rc=STATUS)
            VERIFY_(STATUS)
         end if
      ELSE
         if( list(n)%unit.ne.0 ) call FREE_FILE( list(n)%unit )
      END if
   enddo

    call MAPL_TimerOff(GENSTATE,"Finalize")
    call MAPL_TimerOff(GENSTATE,"TOTAL")

    call  MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK, RC=status )
    VERIFY_(STATUS)


    RETURN_(ESMF_SUCCESS)
  end subroutine Finalize

!======================================================
 subroutine MAPL_GradsCtlWrite ( clock, state,list,fname,expid,expdsc,rc )
   
   type(ESMF_Clock),  intent(inout) :: clock
   type(ESMF_State)                 :: state
   type(history_list)               :: list
   character(len=*)                 :: expid
   character(len=*)                 :: expdsc
   character(len=*)                 :: fname
   integer, optional, intent(out)   :: rc
   
   type(ESMF_Array)               :: array
   type(ESMF_LocalArray)          :: larraylist(1)
   type(ESMF_Field)               :: field
   type(ESMF_Grid)                :: grid
   type(ESMF_Time)                :: CurrTime
   type(ESMF_Time)                :: StopTime
   type(ESMF_Calendar)            :: cal
   type(ESMF_TimeInterval)        :: ti, Frequency
   integer                        :: nsteps
   integer, dimension(ESMF_MAXDIM):: lbounds, ubounds
   integer, allocatable           :: vdim(:)
   character(len=ESMF_MAXSTR)     :: TimeString
   character(len=ESMF_MAXSTR)     :: filename
   character(len=ESMF_MAXSTR)     :: options
   integer                        :: DIMS(3)
   integer                        :: COUNTS(3)
   integer                        :: IM,JM,LM
   
   character*3                    :: months(12)
   data months /'JAN','FEB','MAR','APR','MAY','JUN', &
                'JUL','AUG','SEP','OCT','NOV','DEC'/
   
   integer      :: unit,nfield
   character(len=ESMF_MAXSTR)      :: IAm="MAPL_GradsCtlWrite" 
   integer      :: k,m,n,nhms,rank,status
!   integer      :: k,m,n,nsecf,nhms,rank,status
   integer      :: year,month,day,hour,minute
   integer      :: gridRank
   real*8   LONBEG,DLON
   real*8   LATBEG,DLAT
   integer  mass, freq,zero
   real(KIND=4),      pointer :: LATS(:,:), LONS(:,:)

   
! Mass-Weighted Diagnostics
! -------------------------
   integer     km
   parameter ( km = 4 )
   character(len=ESMF_MAXSTR) :: name(2,km)
   data name / 'THIM'     , 'PHYSICS'    , & 
               'SIT'      , 'PHYSICS'    , & 
               'DTDT'     , 'PHYSICS'    , &
               'DTDT'     , 'GWD'        /

!   nsecf(nhms) = nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)

   call ESMF_ClockGet ( clock,  currTime=CurrTime ,rc=STATUS ) ; VERIFY_(STATUS)
   call ESMF_ClockGet ( clock,  StopTime=StopTime ,rc=STATUS ) ; VERIFY_(STATUS)
   call ESMF_ClockGet ( clock,  Calendar=cal      ,rc=STATUS ) ; VERIFY_(STATUS)
   
   call ESMF_TimeGet  ( CurrTime, timeString=TimeString, rc=status ) ; VERIFY_(STATUS)
   
   read(timestring( 1: 4),'(i4.4)') year
   read(timestring( 6: 7),'(i2.2)') month
   read(timestring( 9:10),'(i2.2)') day
   read(timestring(12:13),'(i2.2)') hour
   read(timestring(15:16),'(i2.2)') minute
   
   ti = StopTime-CurrTime
   freq = nsecf( list%frequency )
   call ESMF_TimeIntervalSet( Frequency, S=freq, calendar=cal, rc=status ) ; VERIFY_(STATUS)
   
   nsteps =  ti/Frequency + 1
   
   if( trim(expid) == "" ) then
       filename =                       trim(list%collection)
   else
       filename = trim(expid) // '.' // trim(list%collection)
   endif
           unit = GETFILE( trim(filename) // '.ctl', form="formatted" )

   if( list%template == "" .or. list%duration == 0 ) then
       options  = 'options sequential'
       filename = trim(fname)
   else
       options  = 'options sequential template'
       filename = trim(filename) // '.' // trim(list%template)
   endif

! Get Global Horizontal Dimensions
! --------------------------------
   call ESMF_StateGet ( state,trim(list%fields(3,1)),field,rc=status )
   VERIFY_(STATUS)
   call ESMF_FieldGet ( field, grid, rc=status )
   VERIFY_(STATUS)
   
   call ESMF_GridGet(GRID, dimCount=gridRank, rc=STATUS)
   VERIFY_(STATUS)
   call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
   VERIFY_(STATUS)
   if (gridRank == 3) then
      LM = COUNTS(3)
   else ! if (gridRank == 2)
      LM = 1
   end if

   ZERO   =  0
   IM     =  DIMS(1)
   JM     =  DIMS(2)
   DLON   =  360.0/ IM
   if (JM /= 1) then
      DLAT   =  180.0/(JM-1)
   else
      DLAT   =  1.0
   end if

  call ESMFL_GridCoordGet(   GRID, LATS       , &
                             Name     = "Latitude"              , &
                             Location = ESMF_STAGGERLOC_CENTER  , &
                             Units    = ESMFL_UnitsRadians      , &
                             RC       = STATUS                    )
  VERIFY_(STATUS)

  call ESMFL_GridCoordGet(   GRID, LONS       , &
                             Name     = "Longitude"             , &
                             Location = ESMF_STAGGERLOC_CENTER  , &
                             Units    = ESMFL_UnitsRadians      , &
                             RC       = STATUS                    )
  VERIFY_(STATUS)

!ALT: Note: the LATS(1,1) and LONS(1,1) are correct ONLY on root
   if( MAPL_AM_I_ROOT() ) then
       LONBEG = LONS(1,1)*(180/MAPL_PI)

       LATBEG = LATS(1,1)*(180/MAPL_PI)
       DLAT = (LATS(1,2)-LATS(1,1))*(180/MAPL_PI)
   endif

!
! Check if changing resolution
! -------------------------------------------------------------------------
   if (associated( list%resolution )) then
      IM = list%resolution(1)
      JM = list%resolution(2)
      DLON   =  360.0/float(IM)
      if (JM /= 1) then
         DLAT   =  180.0/float(JM-1)
      else
         DLAT   =  1.0
      end if
      LONBEG = -180.0
      LATBEG =  -90.0
   endif

! Compute Vertical Dimension for each Field (Augment nfield for VDIMS > LM)
! -------------------------------------------------------------------------
   allocate( vdim(list%nfield), stat=status )
   VERIFY_(STATUS)
   vdim = 0
   nfield =   list%nfield
   do m = 1,list%nfield
      call ESMFL_StateGetFieldArray( state,trim(list%fields(3,m)),array,status )
      VERIFY_(STATUS)
      call ESMF_ArrayGet( array, larrayList=larrayList, rc=status )
      VERIFY_(STATUS)
      call ESMF_LocalArrayGet( larrayList(1), RANK=rank, lbounds=lbounds, ubounds=ubounds, rc=status )
      VERIFY_(STATUS)
      if( rank==3 ) then
         vdim(m) = ubounds(3)-lbounds(3)+1
         if( vdim(m).gt.LM ) nfield = nfield+1
      endif
   enddo

! Create Grads Control File
! -------------------------
   if( MAPL_AM_I_ROOT() ) then
      print *
      if ( freq < 3600 ) then
         write(unit,201) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/60, nfield
      else if ( freq < 86400 ) then
         write(unit,202) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/3600, nfield
      else if ( freq < 30*86400 ) then
         write(unit,203) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/86400, nfield
      else 
         write(unit,204) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/(30*86400), nfield
      endif
      do m=1,list%nfield
         mass = 0
         do k=1,km
            if( trim(list%fields(1,m)).eq.trim(name(1,k))  .and. &
                 trim(list%fields(2,m)).eq.trim(name(2,k)) ) mass = 1  ! Check for Mass-Weighted Diagnostics
         enddo
         if( vdim(m).le.LM ) then
            write(unit,102) trim(list%fields(3,m)),vdim(m),mass,trim(list%fields(3,m))
         else
            write(unit,102) trim(list%fields(3,m)),LM     ,mass,trim(list%fields(3,m))
            if( trim(list%fields(1,m)).eq.'PLE' ) then
               write(unit,102) 'PS',zero,mass,'PS'
            else
               write(unit,102) trim(list%fields(3,m)) // 's',zero,mass,trim(list%fields(3,m)) // 's'
            endif
         endif
      enddo
      write(unit,103)
   endif
   call FREE_FILE( unit )
   deallocate( vdim )
   
201     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'mn',/, &
	       'vars  ',i3)
202     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'hr',/, &
	       'vars  ',i3)
203     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'dy',/, &
	       'vars  ',i3)
204     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'mo',/, &
	       'vars  ',i3)
102     format(a,i3,2x,i3,2x,"'",a,"'")
103     format('endvars')

   RETURN_(ESMF_SUCCESS)
 end subroutine MAPL_GradsCtlWrite


  subroutine get_DateStamp (clock, DateStamp, expid, offset, rc)
    type (ESMF_Clock)                   :: clock
    character(len=ESMF_MAXSTR),optional :: DateStamp
    character(len=ESMF_MAXSTR),optional :: expid
    type(ESMF_TimeInterval),   optional :: offset
    integer, optional                   :: rc

    type(ESMF_Time)                   :: currentTime
    character(len=ESMF_MAXSTR)        :: TimeString
    character(len=ESMF_MAXSTR)        :: TimeStamp
    character                         :: String(ESMF_MAXSTR)

    character*4 year
    character*2 month
    character*2 day
    character*2 hour
    character*2 minute
    character*2 second

    integer                    :: STATUS
    character(len=ESMF_MAXSTR) :: Iam="get_DateStamp"


    equivalence ( string(01),TimeString )
    equivalence ( string(01),year       )
    equivalence ( string(06),month      )
    equivalence ( string(09),day        )
    equivalence ( string(12),hour       )
    equivalence ( string(15),minute     )
    equivalence ( string(18),second     )

    call ESMF_ClockGet        (clock, currTime=currentTime,        rc=status)
    VERIFY_(STATUS)

    if (present(offset)) then
       currentTime = currentTime - offset
    end if

    call ESMF_TimeGet         (currentTime, timeString=TimeString, rc=status)
    VERIFY_(STATUS)

    if(present(DateStamp)) then
       DateStamp = year//month//day//'_'//hour//minute//second //'z'
    end if
    
    if(present(ExpId)) then
       TimeStamp = '   Date: ' // year // '/' // month // '/' // day
       TimeStamp = trim(TimeStamp) // '  Time: ' // timestring(12:19)
       call WRITE_PARALLEL ( 'Expid: ' // trim(expid) // trim(TimeStamp) )
    endif

    RETURN_(ESMF_SUCCESS)
  end subroutine get_DateStamp

  subroutine RegridTransform(STATE_IN, XFORM, STATE_OUT, LS_IN, LS_OUT, NTILES_IN, NTILES_OUT, RC)
    type (ESMF_State)        , intent(IN   ) :: STATE_IN
    type (ESMF_State)        , intent(INOUT) :: STATE_OUT
    type(MAPL_LocStreamXform), intent(IN   ) :: XFORM
    type(MAPL_LocStream)     , intent(IN   ) :: LS_IN, LS_OUT
    integer                  , intent(IN   ) :: NTILES_IN, NTILES_OUT
    integer, optional        , intent(  OUT) :: RC

    integer                    :: STATUS
    character(len=ESMF_MAXSTR) :: Iam

    integer                         :: L, LM
    integer                         :: I
    integer                         :: rank_in
    integer                         :: rank_out
    integer                         :: itemcount, itemcount_in, itemcount_out
    real, allocatable, dimension(:) :: tile_in, tile_out
    real, pointer                   :: ptr2d_in(:,:)
    real, pointer                   :: ptr2d_out(:,:)
    real, pointer                   :: ptr3d_in(:,:,:)
    real, pointer                   :: ptr3d_out(:,:,:)
    type(ESMF_Array)                :: array_in
    type(ESMF_Array)                :: array_out
    type(ESMF_Field)                :: field
    type (ESMF_StateItemType), pointer   :: ITEMTYPES_IN(:), ITEMTYPES_OUT(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES_IN(:), ITEMNAMES_OUT(:)

    allocate(tile_in (ntiles_in ), stat=status)
    VERIFY_(STATUS)
    allocate(tile_out(ntiles_out), stat=status)
    VERIFY_(STATUS)


    call ESMF_StateGet(STATE_IN,  ITEMCOUNT=ITEMCOUNT_IN,  RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_StateGet(STATE_OUT, ITEMCOUNT=ITEMCOUNT_OUT, RC=STATUS)
    VERIFY_(STATUS)

    ASSERT_(ITEMCOUNT_IN == ITEMCOUNT_OUT)

    ITEMCOUNT = ITEMCOUNT_IN
    ASSERT_(ITEMCOUNT>0)

    allocate(ITEMNAMES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_IN, ITEMNAMELIST=ITEMNAMES_IN, &
                       STATEITEMTYPELIST=ITEMTYPES_IN, RC=STATUS)
    VERIFY_(STATUS)

    allocate(ITEMNAMES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_OUT, ITEMNAMELIST=ITEMNAMES_OUT, &
                       STATEITEMTYPELIST=ITEMTYPES_OUT, RC=STATUS)
    VERIFY_(STATUS)

    DO I=1, ITEMCOUNT
       ASSERT_(ITEMTYPES_IN (I) == ESMF_StateItem_Field)
       ASSERT_(ITEMTYPES_OUT(I) == ESMF_StateItem_Field)

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_in , rc=status)
       VERIFY_(STATUS)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_out, rc=status)
       VERIFY_(STATUS)

       call ESMF_ArrayGet(array_in , rank=rank_in , rc=status)
       VERIFY_(STATUS)
       call ESMF_ArrayGet(array_out, rank=rank_out, rc=status)
       VERIFY_(STATUS)
       ASSERT_(rank_in == rank_out)
       ASSERT_(rank_in >=2 .and. rank_in <= 3)

       if (rank_in == 2) then
          LM = 1
          call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr2d_in , rc=status)
          VERIFY_(STATUS)
          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, rc=status)
          VERIFY_(STATUS)
       else
          call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr3d_in , rc=status)
          VERIFY_(STATUS)
          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr3d_out, rc=status)
          VERIFY_(STATUS)
          LM = size(ptr3d_in,3)
          ASSERT_(size(ptr3d_out,3) == LM)
       end if

       DO L=1,LM
          if (rank_in == 3) then
             ptr2d_in  => ptr3d_in (:,:,L)
             ptr2d_out => ptr3d_out(:,:,L)
          end if

          call MAPL_LocStreamTransform(LS_IN, TILE_IN, PTR2d_IN, RC=STATUS)
          VERIFY_(STATUS)

          call MAPL_LocStreamTransform( tile_out, XFORM, tile_in, RC=STATUS ) 
          VERIFY_(STATUS)

          call MAPL_LocStreamTransform(LS_OUT, PTR2d_OUT, TILE_OUT, RC=STATUS)
          VERIFY_(STATUS)

       ENDDO

    ENDDO

    deallocate(itemtypes_out)
    deallocate(itemnames_out)
    deallocate(itemtypes_in)
    deallocate(itemnames_in)
    deallocate(tile_out)
    deallocate(tile_in )

    RETURN_(ESMF_SUCCESS)
  end subroutine RegridTransform

  subroutine RegridTransformT2G(STATE_IN, XFORM, STATE_OUT, LS_OUT, NTILES_OUT, RC)
    type (ESMF_State)        , intent(IN   ) :: STATE_IN
    type (ESMF_State)        , intent(INOUT) :: STATE_OUT
    type(MAPL_LocStreamXform), optional, intent(IN   ) :: XFORM
    type(MAPL_LocStream)     , intent(IN   ) :: LS_OUT
    integer                  , intent(IN   ) :: NTILES_OUT
    integer, optional        , intent(  OUT) :: RC

    integer                    :: STATUS
    character(len=ESMF_MAXSTR) :: Iam

    integer                         :: I
    integer                         :: rank_in
    integer                         :: rank_out
    integer                         :: itemcount, itemcount_in, itemcount_out
    real, pointer                   :: tile_in(:), tile_out(:)
    real, pointer                   :: ptr2d_in(:,:)
    real, pointer                   :: ptr2d_out(:,:)
    real, pointer                   :: ptr3d_in(:,:,:)
    real, pointer                   :: ptr3d_out(:,:,:)
    type(ESMF_Array)                :: array_in
    type(ESMF_Array)                :: array_out
    type(ESMF_Field)                :: field
    type (ESMF_StateItemType), pointer   :: ITEMTYPES_IN(:), ITEMTYPES_OUT(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES_IN(:), ITEMNAMES_OUT(:)

    if (present(XFORM)) then
       allocate(tile_out(ntiles_out), stat=status)
       VERIFY_(STATUS)
    end if

    call ESMF_StateGet(STATE_IN,  ITEMCOUNT=ITEMCOUNT_IN,  RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_StateGet(STATE_OUT, ITEMCOUNT=ITEMCOUNT_OUT, RC=STATUS)
    VERIFY_(STATUS)

    ASSERT_(ITEMCOUNT_IN == ITEMCOUNT_OUT)

    ITEMCOUNT = ITEMCOUNT_IN
    ASSERT_(ITEMCOUNT>0)

    allocate(ITEMNAMES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_IN(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_IN, ITEMNAMELIST=ITEMNAMES_IN, &
                       STATEITEMTYPELIST=ITEMTYPES_IN, RC=STATUS)
    VERIFY_(STATUS)

    allocate(ITEMNAMES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)
    allocate(ITEMTYPES_OUT(ITEMCOUNT),STAT=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet(STATE_OUT, ITEMNAMELIST=ITEMNAMES_OUT, &
                       STATEITEMTYPELIST=ITEMTYPES_OUT, RC=STATUS)
    VERIFY_(STATUS)

    DO I=1, ITEMCOUNT
       ASSERT_(ITEMTYPES_IN (I) == ESMF_StateItem_Field)
       ASSERT_(ITEMTYPES_OUT(I) == ESMF_StateItem_Field)

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_in , rc=status)
       VERIFY_(STATUS)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, rc=status)
       VERIFY_(STATUS)
       call ESMF_FieldGet(field, Array=array_out, rc=status)
       VERIFY_(STATUS)

       call ESMF_ArrayGet(array_in , rank=rank_in , rc=status)
       VERIFY_(STATUS)
       call ESMF_ArrayGet(array_out, rank=rank_out, rc=status)
       VERIFY_(STATUS)
       ASSERT_(rank_in == 1)
       ASSERT_(rank_out == 2)

       call ESMF_ArrayGet(array_in , localDE=0, farrayptr=tile_in , rc=status)
       VERIFY_(STATUS)
       call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, rc=status)
       VERIFY_(STATUS)

       if (present(XFORM)) then
          call MAPL_LocStreamTransform( tile_out, XFORM, tile_in, RC=STATUS ) 
          VERIFY_(STATUS)
       else
          tile_out => tile_in
       endif

       call MAPL_LocStreamTransform(LS_OUT, PTR2d_OUT, TILE_OUT, RC=STATUS)
       VERIFY_(STATUS)


    ENDDO

    deallocate(itemtypes_out)
    deallocate(itemnames_out)
    deallocate(itemtypes_in)
    deallocate(itemnames_in)
    if (present(XFORM)) then
       deallocate(tile_out)
    end if

    RETURN_(ESMF_SUCCESS)
  end subroutine RegridTransformT2G

  subroutine SetPsizeStrategy(Slices,Writing,Ncores,npes,Psize,Root)
    integer, intent(IN) :: Slices(:), Ncores, npes
    logical, intent(IN) :: Writing(:)
    integer, intent(OUT) :: Psize(:)
    integer, intent(OUT) :: Root(:)

    integer :: CurrRoot, layers, L, n, status

    Psize = 0
    Root  = 0

    layers = (sum(Slices,mask=writing)-1)/npes + 1

    do L=layers,layers + 5

       where(writing)
          Psize = (slices-1)/L +1
          Psize = ((Psize-1)/Ncores + 1)*Ncores
       endwhere

       if(sum(Psize) <= Npes) exit
    enddo

    if( l>layers + 5 ) Psize = Ncores

    CurrRoot = MAPL_Root
    do n=1,size(Writing)
       if(Writing(n)) then
          Root(n)  = CurrRoot
          CurrRoot = mod(CurrRoot + Psize(n),npes)
       endif
    enddo
  end subroutine SetPsizeStrategy

! IBM AIX does not like mod in statement functions
  integer function nsecf(nhms)
  integer nhms
  nsecf = nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
  end function nsecf


end module MAPL_HistoryGridCompMod

