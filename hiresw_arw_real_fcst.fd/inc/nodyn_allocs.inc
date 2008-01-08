!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nodyn_allocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
ALLOCATE(grid%lu_index(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_index(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_index=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lu_mask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_mask(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_mask=initial_data_value
ELSE
ALLOCATE(grid%lu_mask(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_mask(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%nest_pos(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nest_pos(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nest_pos=initial_data_value
ALLOCATE(grid%nest_mask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nest_mask(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nest_mask=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ht_coarse(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_coarse(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_coarse=initial_data_value
ELSE
ALLOCATE(grid%ht_coarse(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_coarse(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%cfn=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cfn1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%step_number=0
ALLOCATE(grid%q2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%q2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2=initial_data_value
ALLOCATE(grid%t2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%t2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2=initial_data_value
ALLOCATE(grid%th2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%th2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th2=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%psfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%psfc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfc=initial_data_value
ELSE
ALLOCATE(grid%psfc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%psfc(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%u10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u10(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u10=initial_data_value
ALLOCATE(grid%v10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v10(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v10=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%uratx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uratx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uratx=initial_data_value
ELSE
ALLOCATE(grid%uratx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uratx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vratx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vratx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vratx=initial_data_value
ELSE
ALLOCATE(grid%vratx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vratx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tratx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tratx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tratx=initial_data_value
ELSE
ALLOCATE(grid%tratx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tratx(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%rdx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rdy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dts=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dtseps=initial_data_value
IF ( setinitval .EQ. 3 ) grid%resm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%zetatop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cf3=initial_data_value
IF ( setinitval .EQ. 3 ) grid%number_at_same_level=0
IF ( setinitval .EQ. 3 ) grid%itimestep=0
IF ( setinitval .EQ. 3 ) grid%xtime=initial_data_value
IF ( setinitval .EQ. 3 ) grid%julian=initial_data_value
IF ( setinitval .EQ. 3 ) grid%lbc_fid=0
IF ( setinitval .EQ. 3 ) grid%tiled=.FALSE.
IF ( setinitval .EQ. 3 ) grid%patched=.FALSE.
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_nostag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_nostag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_nostag=0
ELSE
ALLOCATE(grid%imask_nostag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_nostag(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_xstag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xstag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xstag=0
ELSE
ALLOCATE(grid%imask_xstag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xstag(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_ystag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_ystag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_ystag=0
ELSE
ALLOCATE(grid%imask_ystag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_ystag(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_xystag(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xystag=0
ELSE
ALLOCATE(grid%imask_xystag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%xi=initial_data_value
IF ( setinitval .EQ. 3 ) grid%xj=initial_data_value
IF ( setinitval .EQ. 3 ) grid%vc_i=initial_data_value
IF ( setinitval .EQ. 3 ) grid%vc_j=initial_data_value
ALLOCATE(grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%moist_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist_b=initial_data_value
ELSE
ALLOCATE(grid%moist_b(1,1,1,1,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist_b(1,1,1,1,num_moist).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%moist_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_moist). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist_bt=initial_data_value
ELSE
ALLOCATE(grid%moist_bt(1,1,1,1,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist_bt(1,1,1,1,num_moist).  ')
 endif
ENDIF
ALLOCATE(grid%chem(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%chem(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chem=initial_data_value
ALLOCATE(grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%scalar_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_scalar). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_b=initial_data_value
ELSE
ALLOCATE(grid%scalar_b(1,1,1,1,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_b(1,1,1,1,num_scalar).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%scalar_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4,num_scalar). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_bt=initial_data_value
ELSE
ALLOCATE(grid%scalar_bt(1,1,1,1,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_bt(1,1,1,1,num_scalar).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fcx(1:model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fcx(1:model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fcx=initial_data_value
ELSE
ALLOCATE(grid%fcx(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fcx(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%gcx(1:model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gcx(1:model_config_rec%spec_bdy_width). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gcx=initial_data_value
ELSE
ALLOCATE(grid%gcx(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gcx(1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%dtbc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm000007(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000007(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm000007=initial_data_value
ELSE
ALLOCATE(grid%sm000007(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000007(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm007028(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm007028(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm007028=initial_data_value
ELSE
ALLOCATE(grid%sm007028(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm007028(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm028100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm028100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm028100=initial_data_value
ELSE
ALLOCATE(grid%sm028100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm028100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm100255(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100255(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm100255=initial_data_value
ELSE
ALLOCATE(grid%sm100255(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100255(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st000007(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000007(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st000007=initial_data_value
ELSE
ALLOCATE(grid%st000007(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000007(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st007028(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st007028(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st007028=initial_data_value
ELSE
ALLOCATE(grid%st007028(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st007028(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st028100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st028100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st028100=initial_data_value
ELSE
ALLOCATE(grid%st028100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st028100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st100255(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100255(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st100255=initial_data_value
ELSE
ALLOCATE(grid%st100255(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100255(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000010(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm000010=initial_data_value
ELSE
ALLOCATE(grid%sm000010(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000010(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010040=initial_data_value
ELSE
ALLOCATE(grid%sm010040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm040100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm040100=initial_data_value
ELSE
ALLOCATE(grid%sm040100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm040100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm100200=initial_data_value
ELSE
ALLOCATE(grid%sm100200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010200=initial_data_value
ELSE
ALLOCATE(grid%sm010200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm000(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm000=initial_data_value
ELSE
ALLOCATE(grid%soilm000(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm000(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm005(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm005=initial_data_value
ELSE
ALLOCATE(grid%soilm005(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm005(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm020(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm020=initial_data_value
ELSE
ALLOCATE(grid%soilm020(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm020(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm040=initial_data_value
ELSE
ALLOCATE(grid%soilm040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm160(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm160=initial_data_value
ELSE
ALLOCATE(grid%soilm160(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm160(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilm300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm300=initial_data_value
ELSE
ALLOCATE(grid%soilm300(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm300(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw000010(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw000010=initial_data_value
ELSE
ALLOCATE(grid%sw000010(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw000010(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010040=initial_data_value
ELSE
ALLOCATE(grid%sw010040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw040100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw040100=initial_data_value
ELSE
ALLOCATE(grid%sw040100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw040100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw100200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw100200=initial_data_value
ELSE
ALLOCATE(grid%sw100200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw100200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sw010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010200=initial_data_value
ELSE
ALLOCATE(grid%sw010200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw000(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw000=initial_data_value
ELSE
ALLOCATE(grid%soilw000(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw000(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw005(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw005=initial_data_value
ELSE
ALLOCATE(grid%soilw005(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw005(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw020(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw020=initial_data_value
ELSE
ALLOCATE(grid%soilw020(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw020(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw040=initial_data_value
ELSE
ALLOCATE(grid%soilw040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw160(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw160=initial_data_value
ELSE
ALLOCATE(grid%soilw160(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw160(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilw300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw300=initial_data_value
ELSE
ALLOCATE(grid%soilw300(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw300(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000010(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st000010=initial_data_value
ELSE
ALLOCATE(grid%st000010(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000010(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010040=initial_data_value
ELSE
ALLOCATE(grid%st010040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st040100(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st040100=initial_data_value
ELSE
ALLOCATE(grid%st040100(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st040100(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st100200=initial_data_value
ELSE
ALLOCATE(grid%st100200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%st010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010200(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010200=initial_data_value
ELSE
ALLOCATE(grid%st010200(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010200(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt000(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt000=initial_data_value
ELSE
ALLOCATE(grid%soilt000(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt000(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt005(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt005=initial_data_value
ELSE
ALLOCATE(grid%soilt005(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt005(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt020(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt020=initial_data_value
ELSE
ALLOCATE(grid%soilt020(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt020(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt040(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt040=initial_data_value
ELSE
ALLOCATE(grid%soilt040(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt040(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt160(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt160=initial_data_value
ELSE
ALLOCATE(grid%soilt160(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt160(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt300(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt300=initial_data_value
ELSE
ALLOCATE(grid%soilt300(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt300(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%landmask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landmask(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landmask=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%topostdv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%topostdv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%topostdv=initial_data_value
ELSE
ALLOCATE(grid%topostdv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%topostdv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%toposlpx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpx=initial_data_value
ELSE
ALLOCATE(grid%toposlpx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%toposlpy(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpy(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpy=initial_data_value
ELSE
ALLOCATE(grid%toposlpy(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpy(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%shdmax(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%shdmax(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmax=initial_data_value
ELSE
ALLOCATE(grid%shdmax(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%shdmax(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%shdmin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%shdmin(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmin=initial_data_value
ELSE
ALLOCATE(grid%shdmin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%shdmin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%snoalb(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snoalb(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snoalb=initial_data_value
ELSE
ALLOCATE(grid%snoalb(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snoalb(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%slopecat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%slopecat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%slopecat=initial_data_value
ELSE
ALLOCATE(grid%slopecat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%slopecat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%toposoil(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposoil(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposoil=initial_data_value
ELSE
ALLOCATE(grid%toposoil(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposoil(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landusef=initial_data_value
ELSE
ALLOCATE(grid%landusef(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilctop=initial_data_value
ELSE
ALLOCATE(grid%soilctop(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot=initial_data_value
ELSE
ALLOCATE(grid%soilcbot(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilcat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcat=initial_data_value
ELSE
ALLOCATE(grid%soilcat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vegcat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegcat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegcat=initial_data_value
ELSE
ALLOCATE(grid%vegcat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegcat(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tslb=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%zs(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%zs(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%zs=initial_data_value
ELSE
ALLOCATE(grid%zs(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%zs(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dzs(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzs(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzs=initial_data_value
ELSE
ALLOCATE(grid%dzs(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzs(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dzr(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzr(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzr=initial_data_value
ELSE
ALLOCATE(grid%dzr(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzr(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dzb(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzb(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzb=initial_data_value
ELSE
ALLOCATE(grid%dzb(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzb(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dzg(1:model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzg(1:model_config_rec%num_soil_layers). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzg=initial_data_value
ELSE
ALLOCATE(grid%dzg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dzg(1).  ')
 endif
ENDIF
ALLOCATE(grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smois=initial_data_value
ALLOCATE(grid%sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sh2o=initial_data_value
ALLOCATE(grid%xice(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice=initial_data_value
ALLOCATE(grid%smstav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstav(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstav=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smstot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstot(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstot=initial_data_value
ELSE
ALLOCATE(grid%smstot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstot(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%sfcrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcrunoff(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcrunoff=initial_data_value
ALLOCATE(grid%udrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%udrunoff(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%udrunoff=initial_data_value
ALLOCATE(grid%ivgtyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ivgtyp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ivgtyp=0
ALLOCATE(grid%isltyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%isltyp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%isltyp=0
ALLOCATE(grid%vegfra(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegfra(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegfra=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sfcevp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcevp(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcevp=initial_data_value
ELSE
ALLOCATE(grid%sfcevp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcevp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%grdflx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%grdflx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%grdflx=initial_data_value
ELSE
ALLOCATE(grid%grdflx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%grdflx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sfcexc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcexc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcexc=initial_data_value
ELSE
ALLOCATE(grid%sfcexc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcexc(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%acsnow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnow(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnow=initial_data_value
ALLOCATE(grid%acsnom(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnom(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnom=initial_data_value
ALLOCATE(grid%snow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snow(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snow=initial_data_value
ALLOCATE(grid%snowh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowh=initial_data_value
ALLOCATE(grid%rhosn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rhosn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rhosn=initial_data_value
ALLOCATE(grid%canwat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%canwat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%canwat=initial_data_value
ALLOCATE(grid%sst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sst=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ifndsnowh=0
IF ( setinitval .EQ. 3 ) grid%ifndsoilw=0
ALLOCATE(grid%tr_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tr_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tr_urb2d=initial_data_value
ALLOCATE(grid%tb_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tb_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tb_urb2d=initial_data_value
ALLOCATE(grid%tg_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tg_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tg_urb2d=initial_data_value
ALLOCATE(grid%tc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tc_urb2d=initial_data_value
ALLOCATE(grid%qc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qc_urb2d=initial_data_value
ALLOCATE(grid%uc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uc_urb2d=initial_data_value
ALLOCATE(grid%xxxr_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xxxr_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xxxr_urb2d=initial_data_value
ALLOCATE(grid%xxxb_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xxxb_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xxxb_urb2d=initial_data_value
ALLOCATE(grid%xxxg_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xxxg_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xxxg_urb2d=initial_data_value
ALLOCATE(grid%xxxc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xxxc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xxxc_urb2d=initial_data_value
ALLOCATE(grid%trl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%trl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%trl_urb3d=initial_data_value
ALLOCATE(grid%tbl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbl_urb3d=initial_data_value
ALLOCATE(grid%tgl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tgl_urb3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tgl_urb3d=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sh_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sh_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sh_urb2d=initial_data_value
ELSE
ALLOCATE(grid%sh_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sh_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lh_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lh_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lh_urb2d=initial_data_value
ELSE
ALLOCATE(grid%lh_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lh_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%g_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%g_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%g_urb2d=initial_data_value
ELSE
ALLOCATE(grid%g_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%g_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rn_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rn_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rn_urb2d=initial_data_value
ELSE
ALLOCATE(grid%rn_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rn_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ts_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ts_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_urb2d=initial_data_value
ELSE
ALLOCATE(grid%ts_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ts_urb2d(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%frc_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%frc_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%frc_urb2d=initial_data_value
ALLOCATE(grid%utype_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%utype_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%utype_urb2d=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cosz_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cosz_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cosz_urb2d=initial_data_value
ELSE
ALLOCATE(grid%cosz_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cosz_urb2d(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%omg_urb2d(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%omg_urb2d(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%omg_urb2d=initial_data_value
ELSE
ALLOCATE(grid%omg_urb2d(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%omg_urb2d(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%declin_urb=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smfr3d=initial_data_value
ELSE
ALLOCATE(grid%smfr3d(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smfr3d(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%keepfr3dflag=initial_data_value
ELSE
ALLOCATE(grid%keepfr3dflag(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tke_myj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tke_myj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tke_myj=initial_data_value
ELSE
ALLOCATE(grid%tke_myj(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tke_myj(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%el_myj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%el_myj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%el_myj=initial_data_value
ELSE
ALLOCATE(grid%el_myj(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%el_myj(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%exch_h(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%exch_h(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%exch_h=initial_data_value
ELSE
ALLOCATE(grid%exch_h(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%exch_h(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ct(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ct(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ct=initial_data_value
ELSE
ALLOCATE(grid%ct(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ct(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%thz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thz0=initial_data_value
ELSE
ALLOCATE(grid%thz0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thz0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%z0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%z0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z0=initial_data_value
ELSE
ALLOCATE(grid%z0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%z0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qz0=initial_data_value
ELSE
ALLOCATE(grid%qz0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qz0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%uz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uz0=initial_data_value
ELSE
ALLOCATE(grid%uz0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vz0=initial_data_value
ELSE
ALLOCATE(grid%vz0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qsfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsfc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsfc=initial_data_value
ELSE
ALLOCATE(grid%qsfc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsfc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%akhs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akhs(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akhs=initial_data_value
ELSE
ALLOCATE(grid%akhs(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akhs(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%akms(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akms(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akms=initial_data_value
ELSE
ALLOCATE(grid%akms(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akms(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%kpbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%kpbl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%kpbl=0
ELSE
ALLOCATE(grid%kpbl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%kpbl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%htop(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htop(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htop=initial_data_value
ELSE
ALLOCATE(grid%htop(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htop(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hbot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbot(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbot=initial_data_value
ELSE
ALLOCATE(grid%hbot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%htopr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htopr=initial_data_value
ELSE
ALLOCATE(grid%htopr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hbotr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbotr=initial_data_value
ELSE
ALLOCATE(grid%hbotr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cutop(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cutop(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cutop=initial_data_value
ELSE
ALLOCATE(grid%cutop(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cutop(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cubot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cubot(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cubot=initial_data_value
ELSE
ALLOCATE(grid%cubot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cubot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cuppt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cuppt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cuppt=initial_data_value
ELSE
ALLOCATE(grid%cuppt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cuppt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rswtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rswtoa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rswtoa=initial_data_value
ELSE
ALLOCATE(grid%rswtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rswtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rlwtoa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rlwtoa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rlwtoa=initial_data_value
ELSE
ALLOCATE(grid%rlwtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rlwtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%czmean(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%czmean(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%czmean=initial_data_value
ELSE
ALLOCATE(grid%czmean(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%czmean(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cfracl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfracl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracl=initial_data_value
ELSE
ALLOCATE(grid%cfracl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfracl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cfracm(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfracm(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracm=initial_data_value
ELSE
ALLOCATE(grid%cfracm(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfracm(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cfrach(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfrach(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfrach=initial_data_value
ELSE
ALLOCATE(grid%cfrach(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cfrach(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%acfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acfrst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrst=initial_data_value
ELSE
ALLOCATE(grid%acfrst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acfrst(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ncfrst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ncfrst(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrst=0
ELSE
ALLOCATE(grid%ncfrst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ncfrst(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%acfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acfrcv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrcv=initial_data_value
ELSE
ALLOCATE(grid%acfrcv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acfrcv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ncfrcv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ncfrcv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrcv=0
ELSE
ALLOCATE(grid%ncfrcv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ncfrcv(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%ozmixm(sm31:em31,1:model_config_rec%levsiz,sm33:em33,num_ozmixm),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ozmixm(sm31:em31,1:model_config_rec%levsiz,sm33:em33,num_ozmixm). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ozmixm=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%pin(1:model_config_rec%levsiz),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pin(1:model_config_rec%levsiz). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pin=initial_data_value
ELSE
ALLOCATE(grid%pin(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pin(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%m_ps_1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_1(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%m_ps_1=initial_data_value
ELSE
ALLOCATE(grid%m_ps_1(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_1(1,1).  ')
 endif
ENDIF
ELSE
ALLOCATE(grid%m_ps_1(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_1(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%m_ps_2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_2(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%m_ps_2=initial_data_value
ELSE
ALLOCATE(grid%m_ps_2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_2(1,1).  ')
 endif
ENDIF
ELSE
ALLOCATE(grid%m_ps_2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_ps_2(1,1).  ')
 endif
ENDIF
IF(IAND(1,tl).NE.0)THEN
ALLOCATE(grid%aerosolc_1(sm31:em31,1:model_config_rec%paerlev,sm33:em33,num_aerosolc),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%aerosolc_1(sm31:em31,1:model_config_rec%paerlev,sm33:em33,num_aerosolc). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aerosolc_1=initial_data_value
ELSE
ALLOCATE(grid%aerosolc_1(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%aerosolc_1(1,1,1,1).  ')
 endif
ENDIF
IF(IAND(2,tl).NE.0)THEN
ALLOCATE(grid%aerosolc_2(sm31:em31,1:model_config_rec%paerlev,sm33:em33,num_aerosolc),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%aerosolc_2(sm31:em31,1:model_config_rec%paerlev,sm33:em33,num_aerosolc). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aerosolc_2=initial_data_value
ELSE
ALLOCATE(grid%aerosolc_2(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%aerosolc_2(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%m_hybi(1:model_config_rec%paerlev),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_hybi(1:model_config_rec%paerlev). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%m_hybi=initial_data_value
ELSE
ALLOCATE(grid%m_hybi(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%m_hybi(1).  ')
 endif
ENDIF
ALLOCATE(grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_ice_phy=initial_data_value
ALLOCATE(grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rain_phy=initial_data_value
ALLOCATE(grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rimef_phy=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%h_diabatic(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%h_diabatic(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%h_diabatic=initial_data_value
ELSE
ALLOCATE(grid%h_diabatic(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%h_diabatic(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%msft(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%msft(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msft=initial_data_value
ALLOCATE(grid%msfu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%msfu(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msfu=initial_data_value
ALLOCATE(grid%msfv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%msfv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%msfv=initial_data_value
ALLOCATE(grid%f(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f=initial_data_value
ALLOCATE(grid%e(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%e(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%e=initial_data_value
ALLOCATE(grid%sina(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sina(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sina=initial_data_value
ALLOCATE(grid%cosa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cosa(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cosa=initial_data_value
ALLOCATE(grid%ht(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ht_fine(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_fine(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_fine=initial_data_value
ELSE
ALLOCATE(grid%ht_fine(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_fine(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ht_int(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_int(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_int=initial_data_value
ELSE
ALLOCATE(grid%ht_int(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_int(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ht_input(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_input(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_input=initial_data_value
ELSE
ALLOCATE(grid%ht_input(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ht_input(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%tsk(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsk(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsk=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tsk_save(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsk_save(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsk_save=initial_data_value
ELSE
ALLOCATE(grid%tsk_save(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsk_save(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%u_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_base=initial_data_value
ELSE
ALLOCATE(grid%u_base(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u_base(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%v_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_base=initial_data_value
ELSE
ALLOCATE(grid%v_base(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v_base(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qv_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qv_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qv_base=initial_data_value
ELSE
ALLOCATE(grid%qv_base(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qv_base(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%z_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%z_base(sm32:em32). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z_base=initial_data_value
ELSE
ALLOCATE(grid%z_base(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%z_base(1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%u_frame=initial_data_value
IF ( setinitval .EQ. 3 ) grid%v_frame=initial_data_value
IF ( setinitval .EQ. 3 ) grid%p_top=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthcuten=initial_data_value
ELSE
ALLOCATE(grid%rthcuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthcuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqvcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvcuten=initial_data_value
ELSE
ALLOCATE(grid%rqvcuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvcuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqrcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqrcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqrcuten=initial_data_value
ELSE
ALLOCATE(grid%rqrcuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqrcuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqccuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqccuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqccuten=initial_data_value
ELSE
ALLOCATE(grid%rqccuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqccuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqscuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqscuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqscuten=initial_data_value
ELSE
ALLOCATE(grid%rqscuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqscuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqicuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqicuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqicuten=initial_data_value
ELSE
ALLOCATE(grid%rqicuten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqicuten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%w0avg(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%w0avg(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w0avg=initial_data_value
ELSE
ALLOCATE(grid%w0avg(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%w0avg(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%rainc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainc=initial_data_value
ALLOCATE(grid%rainnc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainnc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainnc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%raincv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%raincv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%raincv=initial_data_value
ELSE
ALLOCATE(grid%raincv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%raincv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rainncv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainncv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainncv=initial_data_value
ELSE
ALLOCATE(grid%rainncv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainncv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rainbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainbl(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rainbl=initial_data_value
ELSE
ALLOCATE(grid%rainbl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rainbl(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%snownc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snownc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snownc=initial_data_value
ALLOCATE(grid%graupelnc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%graupelnc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%graupelnc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%snowncv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowncv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowncv=initial_data_value
ELSE
ALLOCATE(grid%snowncv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowncv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%graupelncv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%graupelncv(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%graupelncv=initial_data_value
ELSE
ALLOCATE(grid%graupelncv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%graupelncv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nca(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nca(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nca=initial_data_value
ELSE
ALLOCATE(grid%nca(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nca(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lowlyr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lowlyr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lowlyr=0
ELSE
ALLOCATE(grid%lowlyr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lowlyr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mass_flux(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mass_flux(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mass_flux=initial_data_value
ELSE
ALLOCATE(grid%mass_flux(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mass_flux(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_gr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_gr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_gr=initial_data_value
ELSE
ALLOCATE(grid%apr_gr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_gr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_w(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_w(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_w=initial_data_value
ELSE
ALLOCATE(grid%apr_w(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_w(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_mc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_mc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_mc=initial_data_value
ELSE
ALLOCATE(grid%apr_mc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_mc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_st(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_st(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_st=initial_data_value
ELSE
ALLOCATE(grid%apr_st(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_st(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_as(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_as(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_as=initial_data_value
ELSE
ALLOCATE(grid%apr_as(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_as(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_capma(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capma(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capma=initial_data_value
ELSE
ALLOCATE(grid%apr_capma(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capma(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_capme(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capme(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capme=initial_data_value
ELSE
ALLOCATE(grid%apr_capme(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capme(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%apr_capmi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capmi(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capmi=initial_data_value
ELSE
ALLOCATE(grid%apr_capmi(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capmi(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xf_ens(sm31:em31,sm33:em33,1:model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xf_ens(sm31:em31,sm33:em33,1:model_config_rec%ensdim). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xf_ens=initial_data_value
ELSE
ALLOCATE(grid%xf_ens(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xf_ens(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%pr_ens(sm31:em31,sm33:em33,1:model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pr_ens(sm31:em31,sm33:em33,1:model_config_rec%ensdim). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pr_ens=initial_data_value
ELSE
ALLOCATE(grid%pr_ens(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pr_ens(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthften=initial_data_value
ELSE
ALLOCATE(grid%rthften(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthften(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqvften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvften=initial_data_value
ELSE
ALLOCATE(grid%rqvften(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvften(1,1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%stepcu=0
ALLOCATE(grid%rthraten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthraten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthraten=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthratenlw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthratenlw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthratenlw=initial_data_value
ELSE
ALLOCATE(grid%rthratenlw(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthratenlw(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthratensw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthratensw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthratensw=initial_data_value
ELSE
ALLOCATE(grid%rthratensw(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthratensw(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cldfra(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cldfra(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldfra=initial_data_value
ELSE
ALLOCATE(grid%cldfra(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cldfra(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%swdown(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swdown(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%swdown=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%swdownc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swdownc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%swdownc=initial_data_value
ELSE
ALLOCATE(grid%swdownc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swdownc(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%gsw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gsw(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gsw=initial_data_value
ALLOCATE(grid%glw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%glw(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%glw=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%swcf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swcf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%swcf=initial_data_value
ELSE
ALLOCATE(grid%swcf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%swcf(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lwcf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lwcf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lwcf=initial_data_value
ELSE
ALLOCATE(grid%lwcf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lwcf(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%olr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%olr(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%olr=initial_data_value
ELSE
ALLOCATE(grid%olr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%olr(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%xlat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlat(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat=initial_data_value
ALLOCATE(grid%xlong(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlong(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%albedo(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albedo(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedo=initial_data_value
ELSE
ALLOCATE(grid%albedo(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albedo(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%albbck(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albbck=initial_data_value
ELSE
ALLOCATE(grid%albbck(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albbck(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%emiss(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%emiss(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emiss=initial_data_value
ELSE
ALLOCATE(grid%emiss(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%emiss(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cldefi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cldefi(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldefi=initial_data_value
ELSE
ALLOCATE(grid%cldefi(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cldefi(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%stepra=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rublten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rublten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rublten=initial_data_value
ELSE
ALLOCATE(grid%rublten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rublten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rvblten=initial_data_value
ELSE
ALLOCATE(grid%rvblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rvblten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthblten=initial_data_value
ELSE
ALLOCATE(grid%rthblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthblten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvblten=initial_data_value
ELSE
ALLOCATE(grid%rqvblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvblten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqcblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqcblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqcblten=initial_data_value
ELSE
ALLOCATE(grid%rqcblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqcblten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqiblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqiblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqiblten=initial_data_value
ELSE
ALLOCATE(grid%rqiblten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqiblten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mp_restart_state(1:7501),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mp_restart_state(1:7501). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mp_restart_state=initial_data_value
ELSE
ALLOCATE(grid%mp_restart_state(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mp_restart_state(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tbpvs_state(1:7501),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbpvs_state(1:7501). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbpvs_state=initial_data_value
ELSE
ALLOCATE(grid%tbpvs_state(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbpvs_state(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tbpvs0_state(1:7501),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbpvs0_state(1:7501). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbpvs0_state=initial_data_value
ELSE
ALLOCATE(grid%tbpvs0_state(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tbpvs0_state(1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%landuse_isice=0
IF ( setinitval .EQ. 3 ) grid%landuse_lucats=0
IF ( setinitval .EQ. 3 ) grid%landuse_luseas=0
IF ( setinitval .EQ. 3 ) grid%landuse_isn=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lu_state(1:7501),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_state(1:7501). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_state=initial_data_value
ELSE
ALLOCATE(grid%lu_state(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_state(1).  ')
 endif
ENDIF
ALLOCATE(grid%tmn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tmn(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tmn=initial_data_value
ALLOCATE(grid%xland(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xland(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xland=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%znt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%znt=initial_data_value
ELSE
ALLOCATE(grid%znt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%znt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ust(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ust(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ust=initial_data_value
ELSE
ALLOCATE(grid%ust(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ust(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rmol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmol(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rmol=initial_data_value
ELSE
ALLOCATE(grid%rmol(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmol(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mol=initial_data_value
ELSE
ALLOCATE(grid%mol(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mol(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%pblh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pblh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pblh=initial_data_value
ELSE
ALLOCATE(grid%pblh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pblh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%capg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%capg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%capg=initial_data_value
ELSE
ALLOCATE(grid%capg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%capg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%thc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thc=initial_data_value
ELSE
ALLOCATE(grid%thc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hfx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hfx=initial_data_value
ELSE
ALLOCATE(grid%hfx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hfx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qfx(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qfx=initial_data_value
ELSE
ALLOCATE(grid%qfx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qfx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lh(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lh=initial_data_value
ELSE
ALLOCATE(grid%lh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%flhc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flhc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flhc=initial_data_value
ELSE
ALLOCATE(grid%flhc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flhc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%flqc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flqc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flqc=initial_data_value
ELSE
ALLOCATE(grid%flqc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flqc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qsg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsg=initial_data_value
ELSE
ALLOCATE(grid%qsg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qvg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qvg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qvg=initial_data_value
ELSE
ALLOCATE(grid%qvg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qvg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qcg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qcg(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qcg=initial_data_value
ELSE
ALLOCATE(grid%qcg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qcg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilt1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt1(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt1=initial_data_value
ELSE
ALLOCATE(grid%soilt1(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt1(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tsnav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsnav(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsnav=initial_data_value
ELSE
ALLOCATE(grid%tsnav(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsnav(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%snowc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowc(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mavail(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mavail(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mavail=initial_data_value
ELSE
ALLOCATE(grid%mavail(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mavail(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tkesfcf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tkesfcf(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tkesfcf=initial_data_value
ELSE
ALLOCATE(grid%tkesfcf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tkesfcf(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%stepbl=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%taucldi(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%taucldi(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldi=initial_data_value
ELSE
ALLOCATE(grid%taucldi(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%taucldi(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%taucldc(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%taucldc(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldc=initial_data_value
ELSE
ALLOCATE(grid%taucldc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%taucldc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor11(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor11(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor11=initial_data_value
ELSE
ALLOCATE(grid%defor11(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor11(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor22(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor22(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor22=initial_data_value
ELSE
ALLOCATE(grid%defor22(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor22(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor12(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor12(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor12=initial_data_value
ELSE
ALLOCATE(grid%defor12(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor12(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor33(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor33(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor33=initial_data_value
ELSE
ALLOCATE(grid%defor33(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor33(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor13(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor13(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor13=initial_data_value
ELSE
ALLOCATE(grid%defor13(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor13(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%defor23(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor23(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor23=initial_data_value
ELSE
ALLOCATE(grid%defor23(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%defor23(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkmv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmv=initial_data_value
ELSE
ALLOCATE(grid%xkmv(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmv(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkmh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmh=initial_data_value
ELSE
ALLOCATE(grid%xkmh(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmh(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkmhd(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmhd(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmhd=initial_data_value
ELSE
ALLOCATE(grid%xkmhd(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkmhd(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkhv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkhv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhv=initial_data_value
ELSE
ALLOCATE(grid%xkhv(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkhv(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xkhh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkhh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhh=initial_data_value
ELSE
ALLOCATE(grid%xkhh(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xkhh(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%div(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%div=initial_data_value
ELSE
ALLOCATE(grid%div(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%div(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%bn2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%bn2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%bn2=initial_data_value
ELSE
ALLOCATE(grid%bn2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%bn2(1,1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%warm_rain=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adv_moist_cond=.FALSE.
IF ( setinitval .EQ. 3 ) grid%stepfg=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rundgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rundgdten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rundgdten=initial_data_value
ELSE
ALLOCATE(grid%rundgdten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rundgdten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rvndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rvndgdten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rvndgdten=initial_data_value
ELSE
ALLOCATE(grid%rvndgdten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rvndgdten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rthndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthndgdten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthndgdten=initial_data_value
ELSE
ALLOCATE(grid%rthndgdten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthndgdten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rqvndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvndgdten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvndgdten=initial_data_value
ELSE
ALLOCATE(grid%rqvndgdten(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvndgdten(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rmundgdten(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmundgdten(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rmundgdten=initial_data_value
ELSE
ALLOCATE(grid%rmundgdten(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmundgdten(1,1).  ')
 endif
ENDIF
ALLOCATE(grid%fdda3d(sm31:em31,sm32:em32,sm33:em33,num_fdda3d),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdda3d(sm31:em31,sm32:em32,sm33:em33,num_fdda3d). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdda3d=initial_data_value
ALLOCATE(grid%fdda2d(sm31:em31,1:1,sm33:em33,num_fdda2d),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdda2d(sm31:em31,1:1,sm33:em33,num_fdda2d). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdda2d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moved=.FALSE.
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%abstot(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim2,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%abstot(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim2,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%abstot=initial_data_value
ELSE
ALLOCATE(grid%abstot(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%abstot(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%absnxt(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim1,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%absnxt(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim1,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%absnxt=initial_data_value
ELSE
ALLOCATE(grid%absnxt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%absnxt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%emstot(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%emstot(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emstot=initial_data_value
ELSE
ALLOCATE(grid%emstot(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%emstot(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dpsdt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dpsdt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dpsdt=initial_data_value
ELSE
ALLOCATE(grid%dpsdt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dpsdt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dmudt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dmudt(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dmudt=initial_data_value
ELSE
ALLOCATE(grid%dmudt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dmudt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%pk1m(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pk1m(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pk1m=initial_data_value
ELSE
ALLOCATE(grid%pk1m(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pk1m(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mu_2m(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mu_2m(sm31:em31,sm33:em33). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mu_2m=initial_data_value
ELSE
ALLOCATE(grid%mu_2m(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mu_2m(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%run_days=0
IF ( setinitval .EQ. 3 ) grid%run_hours=0
IF ( setinitval .EQ. 3 ) grid%run_minutes=0
IF ( setinitval .EQ. 3 ) grid%run_seconds=0
IF ( setinitval .EQ. 3 ) grid%start_year=0
IF ( setinitval .EQ. 3 ) grid%start_month=0
IF ( setinitval .EQ. 3 ) grid%start_day=0
IF ( setinitval .EQ. 3 ) grid%start_hour=0
IF ( setinitval .EQ. 3 ) grid%start_minute=0
IF ( setinitval .EQ. 3 ) grid%start_second=0
IF ( setinitval .EQ. 3 ) grid%end_year=0
IF ( setinitval .EQ. 3 ) grid%end_month=0
IF ( setinitval .EQ. 3 ) grid%end_day=0
IF ( setinitval .EQ. 3 ) grid%end_hour=0
IF ( setinitval .EQ. 3 ) grid%end_minute=0
IF ( setinitval .EQ. 3 ) grid%end_second=0
IF ( setinitval .EQ. 3 ) grid%interval_seconds=0
IF ( setinitval .EQ. 3 ) grid%input_from_file=.FALSE.
IF ( setinitval .EQ. 3 ) grid%fine_input_stream=0
IF ( setinitval .EQ. 3 ) grid%input_from_hires=.FALSE.
IF ( setinitval .EQ. 3 ) grid%oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput10_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_oid=0
IF ( setinitval .EQ. 3 ) grid%history_interval=0
IF ( setinitval .EQ. 3 ) grid%frames_per_outfile=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%restart=.FALSE.
IF ( setinitval .EQ. 3 ) grid%restart_interval=0
IF ( setinitval .EQ. 3 ) grid%io_form_input=0
IF ( setinitval .EQ. 3 ) grid%io_form_history=0
IF ( setinitval .EQ. 3 ) grid%io_form_restart=0
IF ( setinitval .EQ. 3 ) grid%io_form_boundary=0
IF ( setinitval .EQ. 3 ) grid%debug_level=0
IF ( setinitval .EQ. 3 ) grid%self_test_domain=.FALSE.
IF ( setinitval .EQ. 3 ) grid%history_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%history_interval_d=0
IF ( setinitval .EQ. 3 ) grid%history_interval_h=0
IF ( setinitval .EQ. 3 ) grid%history_interval_m=0
IF ( setinitval .EQ. 3 ) grid%history_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_interval=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_d=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_h=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_m=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_s=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_d=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_h=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_m=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_s=0
IF ( setinitval .EQ. 3 ) grid%history_begin_y=0
IF ( setinitval .EQ. 3 ) grid%history_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%history_begin_d=0
IF ( setinitval .EQ. 3 ) grid%history_begin_h=0
IF ( setinitval .EQ. 3 ) grid%history_begin_m=0
IF ( setinitval .EQ. 3 ) grid%history_begin_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_begin_s=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_y=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_d=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_h=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_m=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_s=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_y=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_d=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_h=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_m=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_s=0
IF ( setinitval .EQ. 3 ) grid%history_end_y=0
IF ( setinitval .EQ. 3 ) grid%history_end_mo=0
IF ( setinitval .EQ. 3 ) grid%history_end_d=0
IF ( setinitval .EQ. 3 ) grid%history_end_h=0
IF ( setinitval .EQ. 3 ) grid%history_end_m=0
IF ( setinitval .EQ. 3 ) grid%history_end_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_end_s=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_y=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_mo=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_d=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_h=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_m=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_s=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput5=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput6=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput7=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput8=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput9=0
IF ( setinitval .EQ. 3 ) grid%io_form_gfdda=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput11=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%julyr=0
IF ( setinitval .EQ. 3 ) grid%julday=0
IF ( setinitval .EQ. 3 ) grid%gmt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%write_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%write_restart_at_0h=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_output_times=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_input_times=.FALSE.
IF ( setinitval .EQ. 3 ) grid%diag_print=0
IF ( setinitval .EQ. 3 ) grid%nocolons=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_num=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_den=0
IF ( setinitval .EQ. 3 ) grid%max_dom=0
IF ( setinitval .EQ. 3 ) grid%s_we=0
IF ( setinitval .EQ. 3 ) grid%e_we=0
IF ( setinitval .EQ. 3 ) grid%s_sn=0
IF ( setinitval .EQ. 3 ) grid%e_sn=0
IF ( setinitval .EQ. 3 ) grid%s_vert=0
IF ( setinitval .EQ. 3 ) grid%e_vert=0
IF ( setinitval .EQ. 3 ) grid%num_metgrid_levels=0
IF ( setinitval .EQ. 3 ) grid%p_top_requested=initial_data_value
IF ( setinitval .EQ. 3 ) grid%interp_type=0
IF ( setinitval .EQ. 3 ) grid%lowest_lev_from_sfc=.FALSE.
IF ( setinitval .EQ. 3 ) grid%lagrange_order=0
IF ( setinitval .EQ. 3 ) grid%force_sfc_in_vinterp=0
IF ( setinitval .EQ. 3 ) grid%zap_close_levels=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sfcp_to_sfcp=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_heights=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%grid_id=0
IF ( setinitval .EQ. 3 ) grid%parent_id=0
IF ( setinitval .EQ. 3 ) grid%i_parent_start=0
IF ( setinitval .EQ. 3 ) grid%j_parent_start=0
IF ( setinitval .EQ. 3 ) grid%parent_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%parent_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%feedback=0
IF ( setinitval .EQ. 3 ) grid%smooth_option=0
IF ( setinitval .EQ. 3 ) grid%blend_width=0
IF ( setinitval .EQ. 3 ) grid%ztop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%moad_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%shw=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_x=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_y=0
IF ( setinitval .EQ. 3 ) grid%numtiles=0
IF ( setinitval .EQ. 3 ) grid%nproc_x=0
IF ( setinitval .EQ. 3 ) grid%nproc_y=0
IF ( setinitval .EQ. 3 ) grid%irand=0
IF ( setinitval .EQ. 3 ) grid%dt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_moves=0
IF ( setinitval .EQ. 3 ) grid%vortex_interval=0
IF ( setinitval .EQ. 3 ) grid%max_vortex_speed=0
IF ( setinitval .EQ. 3 ) grid%corral_dist=0
IF ( setinitval .EQ. 3 ) grid%move_id=0
IF ( setinitval .EQ. 3 ) grid%move_interval=0
IF ( setinitval .EQ. 3 ) grid%move_cd_x=0
IF ( setinitval .EQ. 3 ) grid%move_cd_y=0
IF ( setinitval .EQ. 3 ) grid%swap_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%swap_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%reorder_mesh=.FALSE.
IF ( setinitval .EQ. 3 ) grid%perturb_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%eta_levels=initial_data_value
IF ( setinitval .EQ. 3 ) grid%max_dz=initial_data_value
IF ( setinitval .EQ. 3 ) grid%mp_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_lw_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_sw_physics=0
IF ( setinitval .EQ. 3 ) grid%radt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sf_sfclay_physics=0
IF ( setinitval .EQ. 3 ) grid%sf_surface_physics=0
IF ( setinitval .EQ. 3 ) grid%bl_pbl_physics=0
IF ( setinitval .EQ. 3 ) grid%bldt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cu_physics=0
IF ( setinitval .EQ. 3 ) grid%cudt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gsmdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%isfflx=0
IF ( setinitval .EQ. 3 ) grid%ifsnow=0
IF ( setinitval .EQ. 3 ) grid%icloud=0
IF ( setinitval .EQ. 3 ) grid%swrad_scat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%surface_input_source=0
IF ( setinitval .EQ. 3 ) grid%num_soil_layers=0
IF ( setinitval .EQ. 3 ) grid%num_months=0
IF ( setinitval .EQ. 3 ) grid%maxiens=0
IF ( setinitval .EQ. 3 ) grid%maxens=0
IF ( setinitval .EQ. 3 ) grid%maxens2=0
IF ( setinitval .EQ. 3 ) grid%maxens3=0
IF ( setinitval .EQ. 3 ) grid%ensdim=0
IF ( setinitval .EQ. 3 ) grid%chem_opt=0
IF ( setinitval .EQ. 3 ) grid%num_land_cat=0
IF ( setinitval .EQ. 3 ) grid%num_soil_cat=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out_thresh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%seaice_threshold=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sst_update=0
IF ( setinitval .EQ. 3 ) grid%ucmcall=0
IF ( setinitval .EQ. 3 ) grid%co2tf=0
IF ( setinitval .EQ. 3 ) grid%ra_call_offset=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_freq_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%levsiz=0
IF ( setinitval .EQ. 3 ) grid%paerlev=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim1=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim2=0
IF ( setinitval .EQ. 3 ) grid%cu_rad_feedback=.FALSE.
IF ( setinitval .EQ. 3 ) grid%fgdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%grid_fdda=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_uv=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_t=0
IF ( setinitval .EQ. 3 ) grid%if_no_pbl_nudging_q=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_uv=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_uv=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_t=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_t=0
IF ( setinitval .EQ. 3 ) grid%if_zfac_q=0
IF ( setinitval .EQ. 3 ) grid%k_zfac_q=0
IF ( setinitval .EQ. 3 ) grid%guv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gq=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dtramp_min=initial_data_value
IF ( setinitval .EQ. 3 ) grid%if_ramping=0
IF ( setinitval .EQ. 3 ) grid%obs_nudge_opt=0
IF ( setinitval .EQ. 3 ) grid%max_obs=0
IF ( setinitval .EQ. 3 ) grid%nobs_ndg_vars=0
IF ( setinitval .EQ. 3 ) grid%nobs_err_flds=0
IF ( setinitval .EQ. 3 ) grid%fdda_start=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdda_end=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_wind=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_wind=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_temp=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_mois=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_mois=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_nudge_pstr=0
IF ( setinitval .EQ. 3 ) grid%obs_coef_pstr=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_rinxy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_rinsig=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_twindo=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_npfi=0
IF ( setinitval .EQ. 3 ) grid%obs_ionf=0
IF ( setinitval .EQ. 3 ) grid%obs_idynin=0
IF ( setinitval .EQ. 3 ) grid%obs_dtramp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%obs_ipf_in4dob=.FALSE.
IF ( setinitval .EQ. 3 ) grid%obs_ipf_errob=.FALSE.
IF ( setinitval .EQ. 3 ) grid%obs_ipf_nudob=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dyn_opt=0
IF ( setinitval .EQ. 3 ) grid%rk_ord=0
IF ( setinitval .EQ. 3 ) grid%w_damping=0
IF ( setinitval .EQ. 3 ) grid%diff_opt=0
IF ( setinitval .EQ. 3 ) grid%km_opt=0
IF ( setinitval .EQ. 3 ) grid%damp_opt=0
IF ( setinitval .EQ. 3 ) grid%zdamp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dampcoef=initial_data_value
IF ( setinitval .EQ. 3 ) grid%khdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kvdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%diff_6th_factor=initial_data_value
IF ( setinitval .EQ. 3 ) grid%diff_6th_opt=0
IF ( setinitval .EQ. 3 ) grid%smdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%emdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epssm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%non_hydrostatic=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step_sound=0
IF ( setinitval .EQ. 3 ) grid%h_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%h_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%pd_moist=.FALSE.
IF ( setinitval .EQ. 3 ) grid%pd_chem=.FALSE.
IF ( setinitval .EQ. 3 ) grid%pd_scalar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%pd_tke=.FALSE.
IF ( setinitval .EQ. 3 ) grid%top_radiation=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_cr_len=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kh_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kv_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_drag_coefficient=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_heat_flux=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pert_coriolis=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_full_fields=.FALSE.
IF ( setinitval .EQ. 3 ) grid%base_pres=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_lapse=initial_data_value
IF ( setinitval .EQ. 3 ) grid%spec_bdy_width=0
IF ( setinitval .EQ. 3 ) grid%spec_zone=0
IF ( setinitval .EQ. 3 ) grid%relax_zone=0
IF ( setinitval .EQ. 3 ) grid%specified=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%nested=.FALSE.
IF ( setinitval .EQ. 3 ) grid%real_data_init_type=0
IF ( setinitval .EQ. 3 ) grid%background_proc_id=0
IF ( setinitval .EQ. 3 ) grid%forecast_proc_id=0
IF ( setinitval .EQ. 3 ) grid%production_status=0
IF ( setinitval .EQ. 3 ) grid%compression=0
IF ( setinitval .EQ. 3 ) grid%cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cen_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stand_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%bdyfrq=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iswater=0
IF ( setinitval .EQ. 3 ) grid%isice=0
IF ( setinitval .EQ. 3 ) grid%isurban=0
IF ( setinitval .EQ. 3 ) grid%isoilwater=0
IF ( setinitval .EQ. 3 ) grid%map_proj=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_year=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_month=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_day=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_hour=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_minute=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_second=0
IF ( setinitval .EQ. 3 ) grid%nodyn_nodyn_dummy=0
IF ( setinitval .EQ. 3 ) grid%fdob%domain_tot=0
IF ( setinitval .EQ. 3 ) grid%fdob%domain_init=0
IF ( setinitval .EQ. 3 ) grid%fdob%ieodi=0
IF ( setinitval .EQ. 3 ) grid%fdob%iwtsig=0
IF ( setinitval .EQ. 3 ) grid%fdob%nstat=0
IF ( setinitval .EQ. 3 ) grid%fdob%ktaur=0
IF ( setinitval .EQ. 3 ) grid%fdob%sn_maxcg=0
IF ( setinitval .EQ. 3 ) grid%fdob%we_maxcg=0
IF ( setinitval .EQ. 3 ) grid%fdob%sn_end=0
IF ( setinitval .EQ. 3 ) grid%fdob%levidn(max_domains)=0
IF ( setinitval .EQ. 3 ) grid%fdob%ds_cg=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%window=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%rtlast=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%datend=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%rinfmn=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%rinfmx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%pfree=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%dcon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%dpsmx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%tfaci=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fdob%xn=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%varobs(1:model_config_rec%nobs_ndg_vars,1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%varobs(1:model_config_rec%nobs_ndg_vars,1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%varobs=initial_data_value
ELSE
ALLOCATE(grid%fdob%varobs(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%varobs(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%errf(1:model_config_rec%nobs_err_flds,1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%errf(1:model_config_rec%nobs_err_flds,1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%errf=initial_data_value
ELSE
ALLOCATE(grid%fdob%errf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%errf(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%timeob(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%timeob(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%timeob=initial_data_value
ELSE
ALLOCATE(grid%fdob%timeob(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%timeob(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%nlevs_ob(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%nlevs_ob(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%nlevs_ob=initial_data_value
ELSE
ALLOCATE(grid%fdob%nlevs_ob(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%nlevs_ob(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%lev_in_ob(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%lev_in_ob(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%lev_in_ob=initial_data_value
ELSE
ALLOCATE(grid%fdob%lev_in_ob(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%lev_in_ob(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%plfo(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%plfo(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%plfo=initial_data_value
ELSE
ALLOCATE(grid%fdob%plfo(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%plfo(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%elevob(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%elevob(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%elevob=initial_data_value
ELSE
ALLOCATE(grid%fdob%elevob(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%elevob(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%rio(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rio(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%rio=initial_data_value
ELSE
ALLOCATE(grid%fdob%rio(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rio(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%rjo(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rjo(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%rjo=initial_data_value
ELSE
ALLOCATE(grid%fdob%rjo(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rjo(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%fdob%rko(1:model_config_rec%max_obs),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rko(1:model_config_rec%max_obs). ')
 endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdob%rko=initial_data_value
ELSE
ALLOCATE(grid%fdob%rko(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%fdob%rko(1).  ')
 endif
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
