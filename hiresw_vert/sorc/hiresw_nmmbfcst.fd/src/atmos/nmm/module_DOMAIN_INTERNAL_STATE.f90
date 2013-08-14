



































!---------------------------------------------------------------------------
!
      MODULE MODULE_DOMAIN_INTERNAL_STATE
!
!---------------------------------------------------------------------------
!***  Define all quantities that lie within the DOMAIN component's
!***  internal state.
!---------------------------------------------------------------------------
!
      USE esmf_mod
!
      USE module_INCLUDE
!
!---------------------------------------------------------------------------
!
      IMPLICIT NONE
!
!---------------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: DOMAIN_INTERNAL_STATE                                       &
               ,WRAP_DOMAIN_INTERNAL_STATE
!
!---------------------------------------------------------------------------
!
      TYPE DOMAIN_INTERNAL_STATE
!
!---------------------------------------------------------------------------
!
        INTEGER(kind=KINT) :: KOUNT_TIMESTEPS
!
        TYPE(ESMF_GridComp),ALLOCATABLE,DIMENSION(:) :: DOMAIN_CHILD_COMP      !<-- DOMAIN components of child domains
!
        TYPE(ESMF_GridComp) :: SOLVER_GRID_COMP                                !<-- The Solver gridded component
!
        TYPE(ESMF_State) :: IMP_STATE_SOLVER                                   !<-- The import state of the Solver component
        TYPE(ESMF_State) :: IMP_STATE_WRITE                                    !<-- The import state of the write components
!
        TYPE(ESMF_State) :: EXP_STATE_SOLVER                                   !<-- The export state of the Solver component
        TYPE(ESMF_State) :: EXP_STATE_WRITE                                    !<-- The export state of the write components
!
        INTEGER(kind=KINT) :: LEAD_TASK_DOMAIN                              &  !<-- The first task on a given domain
                             ,NUM_PES_FCST                                     !<-- The number of forecast tasks
!
        TYPE(ESMF_Logical) :: ALLCLEAR_FROM_PARENT                          &  !<-- Child can proceed after parent is free
                             ,I_AM_A_NEST                                   &  !<-- Am I in a nested domain?
                             ,RECVD_ALL_CHILD_DATA                             !<-- Parent is free after all 2-way data recvd
!
        TYPE(ESMF_Alarm) :: ALARM_HISTORY                                   &  !<-- The ESMF Alarm for history output
                           ,ALARM_RESTART                                   &  !<-- The ESMF Alarm for restart output
                           ,ALARM_CLOCKTIME                                    !<-- The ESMF Alarm for clocktime prints
!
        REAL(ESMF_KIND_R8) :: TIMESTEP_FINAL                                   !<-- The forecast's final timestep
!
        LOGICAL(kind=KLOG) :: FIRST_PASS                                    &  !<-- Note 1st time into NMM_INTEGRATE
                             ,TS_INITIALIZED 
!
!---------------------------------------------------------------------------
!***  The following are specific to asynchronous quilting/writing.
!---------------------------------------------------------------------------
!
        LOGICAL(kind=KLOG) :: QUILTING                                      &  !<-- Is the user selecting asynchronous quilting/writing?
                             ,WRITE_LAST_RESTART                            &  !<-- Shall we write last restart file
                             ,WROTE_1ST_HIST                                   !<-- Has 1st history output been written?
!
        TYPE(ESMF_GridComp),DIMENSION(:),POINTER :: WRITE_COMPS                !<-- The array of Write gridded components
!
        INTEGER(kind=KINT) :: WRITE_GROUPS                                  &  !<-- The number of write groups
                             ,WRITE_GROUP_READY_TO_GO                       &  !<-- The active group of write tasks
                             ,WRITE_TASKS_PER_GROUP                            !<-- The number of write tasks in each write group
!
        INTEGER(kind=KINT),DIMENSION(:),POINTER :: LOCAL_ISTART,LOCAL_IEND  &  !<-- The local I limits of the forecast tasks
                                                  ,LOCAL_JSTART,LOCAL_JEND  &  !<-- The local J limits of the forecast tasks
                                                  ,PETLIST_FCST                !<-- Task ID list of fcst tasks on the domain
!
        INTEGER(kind=KINT),DIMENSION(:,:),POINTER :: PETLIST_WRITE             !<-- Task ID list of fcst tasks w/ write tasks by group
!
!---------------------------------------------------------------------------
!***  The following are specific to digital filtering.
!---------------------------------------------------------------------------
!
        INTEGER(kind=KINT) :: KSTEP,NSTEP
!
        INTEGER(kind=KINT) :: NUM_FIELDS_FILTER_2D                          &
                             ,NUM_FIELDS_FILTER_3D                          &
                             ,NUM_FIELDS_FILTER_4D                          &
                             ,NUM_FIELDS_RESTORE_2D                         &
                             ,NUM_FIELDS_RESTORE_3D
!
        REAL(kind=KFPT) :: TOTALSUM
!
        REAL(kind=KFPT),DIMENSION(:,:,:),POINTER :: SAVE_2D,SAVE_2D_PHYS
        REAL(kind=KFPT),DIMENSION(:,:,:,:),POINTER :: SAVE_3D,SAVE_3D_PHYS
        REAL(kind=KFPT),DIMENSION(:,:,:,:,:),POINTER :: SAVE_4D
!
        REAL(kind=KFPT),DIMENSION(:),POINTER :: DOLPH_WGTS(:)
!
        LOGICAL(kind=KLOG) :: FIRST_FILTER
!
        TYPE(ESMF_FieldBundle) :: FILT_BUNDLE_FILTER                        &  !<-- ESMF Bundle of variables to filter
                                 ,FILT_BUNDLE_RESTORE                          !<-- ESMF Bundle of variables to restore to pre-filtered state
!
!---------------------------------------------------------------------------
!
      END TYPE DOMAIN_INTERNAL_STATE
!
!---------------------------------------------------------------------------
!
!---------------------------------------------------------------------------
!***  This state is supported by C pointers but not by F90 pointers
!***  therefore we use this "WRAP".
!---------------------------------------------------------------------------
!
      TYPE WRAP_DOMAIN_INTERNAL_STATE
        TYPE(DOMAIN_INTERNAL_STATE),POINTER :: DOMAIN_INT_STATE
      END TYPE WRAP_DOMAIN_INTERNAL_STATE
!
!---------------------------------------------------------------------------
!
      END MODULE MODULE_DOMAIN_INTERNAL_STATE
!
!---------------------------------------------------------------------------
