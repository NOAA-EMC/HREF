!STARTOFREGISTRYGENERATEDINCLUDE 'inc/exp_nest_feedbackup_smooth.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
DO itrace = PARAM_FIRST_SCALAR, num_scalar
CALL smoother (                                                               &         
                  scalar(grid%sm31,grid%sm32,grid%sm33,itrace),                                                           &         ! CD field
                 cids, cide, cjds, cjde, ckds, ckde,   &         ! CD dims
                 cims, cime, cjms, cjme, ckms, ckme,   &         ! CD dims
                 cips, cipe, cjps, MIN( (cjde-1), cjpe ), ckps, ckpe,   &         ! CD dims
                 nids, nide, njds, njde, nkds, nkde,   &         ! ND dims
                 nims, nime, njms, njme, nkms, nkme,   &         ! ND dims
                 nips, nipe, njps, MIN( (njde-1), njpe ), nkps, nkpe,   &         ! ND dims
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
                  ) 
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
