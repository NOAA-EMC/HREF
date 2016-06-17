#include "../ESMFVersionDefine.h"

#if (ESMF_MAJOR_VERSION < 5 || ESMF_MINOR_VERSION < 2)
#undef ESMF_520r
#define ESMF_LogFoundError ESMF_LogMsgFoundError
#else
#define ESMF_520r
#endif

 SUBROUTINE ENS_bcst_global(var, peid, rc)

!----------------------------------------------------------------------
! SUBROUTINE bcst_global
!
! This subroutine broadcasts the inputted variable var to all PEs and all
! ensemble members.  And output contains all related index parameters.
!
! DESCRIPTION: VM Broadcast tool software.
!
! REVISION HISTORY:
!
!  Setpember 2007     Weiyu Yang Initial code.
!  May       2011     Weiyu yang, Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!  September 2011     Weiyu yang, Modified for using the ESMF 5.2.0r library.
!
!
! INTERFACE:
!   
!   var    -- inputted single variable.
!   peid   -- PE ID of var.
!   vm     -- the global ESMF VM.
!
      USE esmf_mod
 USE machine

 REAL(KIND = kind_evod)                    :: var 
 REAL(ESMF_KIND_R8), DIMENSION(:), POINTER :: var_work 
 INTEGER                                   :: peid
 TYPE(ESMF_VM)                             :: vm
 INTEGER                                   :: rc

 rc = ESMF_SUCCESS

 CALL ESMF_VMGetGlobal(vm, rc = rc)

#ifdef ESMF_520r
 IF(ESMF_LogFoundError(rc, msg='VMGetGlobal Error')) THEN
#else
 IF(ESMF_LogFoundError(rc,     'VMGetGlobal Error')) THEN
#endif
     PRINT*, 'Error Happened When Getting the Global VM, peid, rc=', &
          peid, rc
 END IF

 ALLOCATE(var_work(1))

 var_work(1) = var

 CALL ESMF_VMBroadcast(vm, var_work, 1, peid, rc = rc) 

#ifdef ESMF_520r
 IF(ESMF_LogFoundError(rc, msg='VM Broadcast Error')) THEN
#else
 IF(ESMF_LogFoundError(rc,     'VM Broadcast Error')) THEN
#endif
     PRINT*, 'Error Happened When VM Broadcasting, peid, rc=', &
          peid, rc
 END IF

 var = var_work(1)

 DEALLOCATE(var_work)

 END SUBROUTINE ENS_bcst_global





 SUBROUTINE ENS_bcst_global_i4(var, peid, rc)

!----------------------------------------------------------------------
! SUBROUTINE bcst_global_i4
!
! This subroutine broadcasts the inputted variable var to all PEs and all
! ensemble members.  And output contains all related index parameters.
!
! DESCRIPTION: VM Broadcast tool software.
!
! REVISION HISTORY:
!
!  Setpember 2007     Weiyu Yang Initial code.
!
!
! INTERFACE:
!   
!   var    -- inputted single variable.
!   peid   -- PE ID of var.
!   vm     -- the global ESMF VM.
!
      USE esmf_mod
 USE machine

 INTEGER                                   :: var 
 REAL(ESMF_KIND_I4), DIMENSION(:), POINTER :: var_work 
 INTEGER                                   :: peid
 TYPE(ESMF_VM)                             :: vm
 INTEGER                                   :: rc

 rc = ESMF_SUCCESS

 CALL ESMF_VMGetGlobal(vm, rc = rc)

#ifdef ESMF_520r
 IF(ESMF_LogFoundError(rc, msg='VMGetGlobal Error')) THEN
#else
 IF(ESMF_LogFoundError(rc,     'VMGetGlobal Error')) THEN
#endif
     PRINT*, 'Error Happened When Getting the Global VM, peid, rc=', &
          peid, rc
 END IF

 ALLOCATE(var_work(1))

 var_work(1) = var

 CALL ESMF_VMBroadcast(vm, var_work, 1, peid, rc = rc) 

#ifdef ESMF_520r
 IF(ESMF_LogFoundError(rc, msg='VM Broadcast Error')) THEN
#else
 IF(ESMF_LogFoundError(rc,     'VM Broadcast Error')) THEN
#endif
     PRINT*, 'Error Happened When VM Broadcasting, peid, rc=', &
          peid, rc
 END IF

 var = var_work(1)

 DEALLOCATE(var_work)

 END SUBROUTINE ENS_bcst_global_i4
