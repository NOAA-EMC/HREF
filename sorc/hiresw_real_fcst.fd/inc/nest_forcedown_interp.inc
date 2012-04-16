!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_forcedown_interp.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE( scalar, 1 ) * SIZE( scalar, 2 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  scalar(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%scalar(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,scalar_bxs(cjms,1,1,itrace),ngrid%scalar_bxs(njms,1,1,itrace)  &
,scalar_bxe(cjms,1,1,itrace),ngrid%scalar_bxe(njms,1,1,itrace)  &
,scalar_bys(cims,1,1,itrace),ngrid%scalar_bys(nims,1,1,itrace)  &
,scalar_bye(cims,1,1,itrace),ngrid%scalar_bye(nims,1,1,itrace)  &
,scalar_btxs(cjms,1,1,itrace),ngrid%scalar_btxs(njms,1,1,itrace)  &
,scalar_btxe(cjms,1,1,itrace),ngrid%scalar_btxe(njms,1,1,itrace)  &
,scalar_btys(cims,1,1,itrace),ngrid%scalar_btys(nims,1,1,itrace)  &
,scalar_btye(cims,1,1,itrace),ngrid%scalar_btye(nims,1,1,itrace)  &
,grid%dt,ngrid%dt  &
                  ) 
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE( dfi_scalar, 1 ) * SIZE( dfi_scalar, 3 ) .GT. 1 ) THEN 
CALL bdy_interp (  &         
                  dfi_scalar(grid%sm31,grid%sm32,grid%sm33,itrace),   &       ! CD field
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         ! CD dims
                  ngrid%dfi_scalar(ngrid%sm31,ngrid%sm32,ngrid%sm33,itrace),  &   ! ND field
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         ! ND dims
                  config_flags%shw, ngrid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  ngrid%i_parent_start, ngrid%j_parent_start,                     &
                  ngrid%parent_grid_ratio, ngrid%parent_grid_ratio                &
,dfi_scalar_bxs(cjms,1,1,itrace),ngrid%dfi_scalar_bxs(njms,1,1,itrace)  &
,dfi_scalar_bxe(cjms,1,1,itrace),ngrid%dfi_scalar_bxe(njms,1,1,itrace)  &
,dfi_scalar_bys(cims,1,1,itrace),ngrid%dfi_scalar_bys(nims,1,1,itrace)  &
,dfi_scalar_bye(cims,1,1,itrace),ngrid%dfi_scalar_bye(nims,1,1,itrace)  &
,dfi_scalar_btxs(cjms,1,1,itrace),ngrid%dfi_scalar_btxs(njms,1,1,itrace)  &
,dfi_scalar_btxe(cjms,1,1,itrace),ngrid%dfi_scalar_btxe(njms,1,1,itrace)  &
,dfi_scalar_btys(cims,1,1,itrace),ngrid%dfi_scalar_btys(nims,1,1,itrace)  &
,dfi_scalar_btye(cims,1,1,itrace),ngrid%dfi_scalar_btye(nims,1,1,itrace)  &
,grid%dt,ngrid%dt  &
                  ) 
ENDIF
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
