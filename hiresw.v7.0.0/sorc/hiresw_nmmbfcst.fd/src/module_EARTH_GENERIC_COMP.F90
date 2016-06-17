#include "./ESMFVersionDefine.h"
#ifdef WITH_NUOPC

module module_EARTH_GENERIC_COMP

  !-----------------------------------------------------------------------------
  ! Generic NEMS Earth Driver Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    Driver_routine_SS             => routine_SetServices, &
    Driver_type_IS                => type_InternalState, &
    Driver_type_ISS               => type_InternalStateStruct, &
    Driver_label_IS               => label_InternalState, &
    Driver_label_SetModelCount    => label_SetModelCount, &
    Driver_label_SetModelPetLists => label_SetModelPetLists, &
    Driver_label_SetModelServices => label_SetModelServices, &
    Driver_label_Finalize         => label_Finalize

  implicit none
  
  private
  
  public routine_SetServices
  public type_InternalState, type_InternalStateStruct
  public label_InternalState, label_SetModelPetLists
  public label_SetModelServices, label_Finalize
  
  public NUOPC_DriverAddComp, NUOPC_DriverGetComp
  
  character(*), parameter :: &
    label_InternalState = "NemsEarthGeneric_InternalState"
  character(*), parameter :: &
    label_SetModelPetLists = "NemsEarthGeneric_SetModelPetLists"
  character(*), parameter :: &
    label_SetModelServices = "NemsEarthGeneric_SetModelServices"
  character(*), parameter :: &
    label_Finalize = "NemsEarthGeneric_Finalize"
  
  type type_InternalStateStruct
    integer, pointer    :: atmPetList(:)
    integer, pointer    :: ocnPetList(:)
    integer, pointer    :: icePetList(:)
    integer, pointer    :: medPetList(:)
    real(ESMF_KIND_R8)  :: medAtmCouplingIntervalSec
    real(ESMF_KIND_R8)  :: medOcnCouplingIntervalSec
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type
  
  integer, parameter :: medPhase_slow = 1 ! must match MED implementation
  integer, parameter :: medPhase_fast_before = 2 ! must match MED implementation
  integer, parameter :: medPhase_fast_after  = 3 ! must match MED implementation

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine routine_SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! derive from generic NUOPC_Driver
    call NUOPC_CompDerive(driver, Driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=Driver_label_SetModelCount, &
      specRoutine=SetModelCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=Driver_label_SetModelPetLists, &
      specRoutine=SetModelPetLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=Driver_label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(driver, specLabel=Driver_label_Finalize, &
      specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! register an internal initialization method
    call NUOPC_CompSetInternalEntryPoint(driver, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv04p2"/), userRoutine=ModifyCplLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelCount(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! set the modelCount for ATM-OCN-ICE-MED coupling
    !TODO: in the future this will be unnecessary to set here, and instead
    !TODO: the modelCount will be a dynamic setting depending on how many
    !TODO: models were added via NUOPC_DriverAddComp() calls.
    ! The modelCount number set here is used for Driver internal allocation,
    ! so it needs to be big enough to hold what may be coming via the
    ! DriverAddComp() calls, but then the modelCount needs to be re-set to the
    ! actual number of models (+mediators) in the run. This re-set happens at
    ! the end of SetModelPetLists().
    call NUOPC_DriverSet(driver, modelCount=4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelPetLists(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat, i
    type(type_InternalState)  :: is
    type(Driver_type_IS)      :: superIS
    character(ESMF_MAXSTR)    :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! allocate memory for this internal state and set it in the Component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_UserCompSetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! nullify the petLists
    nullify(is%wrap%atmPetList)
    nullify(is%wrap%ocnPetList)
    nullify(is%wrap%icePetList)
    nullify(is%wrap%medPetList)
    
    ! SPECIALIZE by calling into optional attached method to set modelPetLists
    call ESMF_MethodExecute(driver, label=label_SetModelPetLists, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
      return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
      return  ! bail out

    ! query Component for super internal State
    nullify(superIS%wrap)
    call ESMF_UserCompGetInternalState(driver, Driver_label_IS, superIS, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
    ! set the petLists
    i = 0 ! initialize model counter
    if (associated(is%wrap%atmPetList)) then
      i = i+1
      superIS%wrap%modelPetLists(i)%petList => is%wrap%atmPetList
    endif
    if (associated(is%wrap%ocnPetList)) then
      i = i+1
      superIS%wrap%modelPetLists(i)%petList => is%wrap%ocnPetList
    endif
    if (associated(is%wrap%icePetList)) then
      i = i+1
      superIS%wrap%modelPetLists(i)%petList => is%wrap%icePetList
    endif
    if (associated(is%wrap%medPetList)) then
      i = i+1
      superIS%wrap%modelPetLists(i)%petList => is%wrap%medPetList
    endif
    
    ! re-set the modelCount to the actual number that is now known
    !TODO: This is just another work-around while the modelCount is still 
    !TODO: set explicitly. In the longer run it will be a Driver internal 
    !TODO: variable that is automatically kept correct.
    call NUOPC_DriverSet(driver, modelCount=i, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  
  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    character(ESMF_MAXSTR)    :: name
    type(ESMF_Clock)          :: internalClock, fastClock
    type(ESMF_TimeInterval)   :: couplingStep 
    type(ESMF_TimeInterval)   :: medAtmCouplingStep, medOcnCouplingStep

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! SPECIALIZE by calling into attached method to SetModelServices
    call ESMF_MethodExecute(driver, label=label_SetModelServices, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
      return  ! bail out

    ! determine the coupling time steps      
    call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call ESMF_ClockGet(internalClock, timeStep=couplingStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
    if (is%wrap%medOcnCouplingIntervalSec>0._ESMF_KIND_R8) then
      ! The coupling time step was provided
      call ESMF_TimeIntervalSet(medOcnCouplingStep, &
        s_r8=is%wrap%medOcnCouplingIntervalSec, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    else
      ! Keep the default timeStep, i.e. that of parent
      medOcnCouplingStep = couplingStep
    endif
    
    if (is%wrap%medAtmCouplingIntervalSec>0._ESMF_KIND_R8) then
      ! The coupling time step was provided
      call ESMF_TimeIntervalSet(medAtmCouplingStep, &
        s_r8=is%wrap%medAtmCouplingIntervalSec, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    else
      ! Use the OCN time step as the ATM default.
      medAtmCouplingStep = medOcnCouplingStep
    endif
    
    ! The NEMS Earth Driver implements a run sequence that supports different
    ! coupling intervals for MED-ATM and MED-OCN coupling. These two coupling
    ! intervals are restricted by the following to constraints:
    ! 1) The MED-ATM coupling is the faster one:
    !      medAtmCouplingStep <= medOcnCouplingStep
    ! 2) The MED-OCN coupling interval must be a multiple of the MED-ATM inverv.
    
    if (medAtmCouplingStep > medOcnCouplingStep) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="The MED-ATM coupling interval must not be larger than the "// &
        "MED-OCN coupling interval!", &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)
      return  ! bail out
    
    endif
    
    if (medAtmCouplingStep * (medOcnCouplingStep/medAtmCouplingStep) /= &
      medOcnCouplingStep) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="The MED-OCN coupling interval must be a multiple of "// &
        "the MED-ATM coupling interval", &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif
    
    ! Implement the NEMS Earth Driver run sequence, replacing the default.
    call NUOPC_DriverNewRunSequence(driver, slotCount=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! ocn2med into slot 1
    call NUOPC_DriverAddRunElement(driver, slot=1, &
      srcCompLabel="OCN", dstCompLabel="MED", phase=1, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! med slow phase into slot 1
    call NUOPC_DriverAddRunElement(driver, slot=1, &
      compLabel="MED", phase=medPhase_slow, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! med2ocn into slot 1
    call NUOPC_DriverAddRunElement(driver, slot=1, &
      srcCompLabel="MED", dstCompLabel="OCN", phase=1, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! ocn into slot 1
    call NUOPC_DriverAddRunElement(driver, slot=1, &
      compLabel="OCN", phase=1, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! LINK from slot 1 to slot 2
    call NUOPC_DriverAddRunElement(driver, slot=1, linkSlot=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! med fast_before phase into slot 2
    call NUOPC_DriverAddRunElement(driver, slot=2, &
      compLabel="MED", phase=medPhase_fast_before, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! med2atm into slot 2
    call NUOPC_DriverAddRunElement(driver, slot=2, &
      srcCompLabel="MED", dstCompLabel="ATM", phase=1, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! med2ice into slot 2
    call NUOPC_DriverAddRunElement(driver, slot=2, &
      srcCompLabel="MED", dstCompLabel="ICE", phase=1, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! atm into slot 2
    call NUOPC_DriverAddRunElement(driver, slot=2, &
      compLabel="ATM", phase=1, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! ice into slot 2
    call NUOPC_DriverAddRunElement(driver, slot=2, &
      compLabel="ICE", phase=1, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! atm2med into slot 2
    call NUOPC_DriverAddRunElement(driver, slot=2, &
      srcCompLabel="ATM", dstCompLabel="MED", phase=1, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! ice2med into slot 2
    call NUOPC_DriverAddRunElement(driver, slot=2, &
      srcCompLabel="ICE", dstCompLabel="MED", phase=1, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! med fast_after phase into slot 2
    call NUOPC_DriverAddRunElement(driver, slot=2, &
      compLabel="MED", phase=medPhase_fast_after, &
      relaxedflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! Set the slow (MED-OCN) coupling time as time step for the internal clock.
    ! The internal clock is used by the NUOPC Layer to drive slot 1.
    call ESMF_ClockSet(internalClock, timeStep=medOcnCouplingStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! Set the fast (MED-ATM) coupling time step for slot 2
    fastClock = ESMF_ClockCreate(internalClock, rc=rc)  ! make a copy first
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call ESMF_ClockSet(fastClock, timeStep=medAtmCouplingStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call NUOPC_DriverSetRunSequence(driver, slot=2, clock=fastClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! Diagnostic output
    call NUOPC_DriverPrint(driver, orderflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
  end subroutine
    
  !-----------------------------------------------------------------------------
  
  subroutine Finalize(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc
    
    ! local variables
    integer                   :: localrc, stat
    type(type_InternalState)  :: is
    logical                   :: existflag
    character(ESMF_MAXSTR)    :: name

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! SPECIALIZE by calling into optional attached method
    call ESMF_MethodExecute(driver, label=label_Finalize, existflag=existflag, &
      userRc=localrc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
      return  ! bail out

    ! query Component for this internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(driver, label_InternalState, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
    ! deallocate internal state memory
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine
      
  !-----------------------------------------------------------------------------
  
  recursive subroutine ModifyCplLists(driver, importState, exportState, clock, &
    rc)
    type(ESMF_GridComp)  :: driver
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(len=160)              :: msg    
    type(ESMF_CplComp), pointer     :: connectorList(:)
    integer                         :: i, j, cplListSize
    character(len=160), allocatable :: cplList(:)
    character(len=160)              :: tempString
    
    rc = ESMF_SUCCESS
    
    call ESMF_LogWrite("Driver is in ModifyCplLists()", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    nullify(connectorList)
    call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    write (msg,*) "Found ", size(connectorList), " Connectors."// &
      " Modifying CplList Attribute...."
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    do i=1, size(connectorList)
      ! query the cplList for connector i
      call NUOPC_CplCompAttributeGet(connectorList(i), &
        cplListSize=cplListSize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (cplListSize>0) then
        allocate(cplList(cplListSize))
        call NUOPC_CplCompAttributeGet(connectorList(i), cplList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! go through all of the entries in the cplList and add options
        do j=1, cplListSize
          tempString = trim(cplList(j))//&
            ":DumpWeights=true"//&
            ":SrcTermProcessing=1:TermOrder=SrcSeq"
          cplList(j) = trim(tempString)
        enddo
        ! store the modified cplList in CplList attribute of connector i
        call ESMF_AttributeSet(connectorList(i), &
          name="CplList", valueList=cplList, &
          convention="NUOPC", purpose="General", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        deallocate(cplList)
      endif
    enddo
      
    deallocate(connectorList)
    
  end subroutine

  !-----------------------------------------------------------------------------

end module
#endif
