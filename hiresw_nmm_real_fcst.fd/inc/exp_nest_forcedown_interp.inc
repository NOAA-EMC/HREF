!STARTOFREGISTRYGENERATEDINCLUDE 'inc/exp_nest_forcedown_interp.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
DO itrace = PARAM_FIRST_SCALAR, num_scalar
CALL bdy_interp (                                                               &         
                  scalar(grid%sm31,grid%sm32,grid%sm33,itrace),                                                           &         ! CD field
                 cids, cide, cjds, cjde, ckds, ckde,   &         ! CD dims
                 cims, cime, cjms, cjme, ckms, ckme,   &         ! CD dims
                 cips, cipe, cjps, MIN( (cjde-1), cjpe ), ckps, ckpe,   &         ! CD dims
                  ngrid%scalar(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),                                                        &   ! ND field
                 nids, nide, njds, njde, nkds, nkde,   &         ! ND dims
                 nims, nime, njms, njme, nkms, nkme,   &         ! ND dims
                 nips, nipe, njps, MIN( (njde-1), njpe ), nkps, nkpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,scalar_b(1,1,1,1,itrace),ngrid%scalar_b(1,1,1,1,itrace)  &
,scalar_bt(1,1,1,1,itrace),ngrid%scalar_bt(1,1,1,1,itrace)  &
,grid%dt,ngrid%dt  &
                  ) 
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
