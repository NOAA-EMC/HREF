!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_allocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
ALLOCATE(grid%lu_index(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_index(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_index=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%lu_mask(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_mask(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_mask=initial_data_value
ELSE
ALLOCATE(grid%lu_mask(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%lu_mask(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_p_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_p_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_p_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_p_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_p_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vegcat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegcat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegcat=initial_data_value
ELSE
ALLOCATE(grid%vegcat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegcat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilcat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcat(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%input_soil_cat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%input_soil_cat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%input_soil_cat=initial_data_value
ELSE
ALLOCATE(grid%input_soil_cat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%input_soil_cat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tsk_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tsk_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tsk_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_tsk_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tsk_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xice_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice_gc=initial_data_value
ELSE
ALLOCATE(grid%xice_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ght_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ght_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ght_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_ght_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ght_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rh_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rh_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rh_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_rh_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rh_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_v_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_v_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_v_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_u_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_u_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_u_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rwmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rwmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rwmr_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_rwmr_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rwmr_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_snmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_snmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_snmr_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_snmr_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_snmr_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_clwmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_clwmr_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_clwmr_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_clwmr_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_clwmr_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cice_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cice_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cice_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_cice_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cice_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rimef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rimef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rimef_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_rimef_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rimef_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%snoalb(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snoalb(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%nmm_greenfrac_gc(sm31:em31,sm32:em32,1:12),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_greenfrac_gc(sm31:em31,sm32:em32,1:12). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_greenfrac_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_greenfrac_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_greenfrac_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_albedo12m_gc(sm31:em31,sm32:em32,1:12),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albedo12m_gc(sm31:em31,sm32:em32,1:12). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_albedo12m_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_albedo12m_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albedo12m_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilcbot_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot_gc=initial_data_value
ELSE
ALLOCATE(grid%soilcbot_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%soilctop_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilctop_gc=initial_data_value
ELSE
ALLOCATE(grid%soilctop_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tmn_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tmn_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tmn_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_tmn_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tmn_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_htv_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_htv_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_htv_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_htv_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_htv_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ht_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ht_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ht_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_ht_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ht_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%landusef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_land_cat),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_land_cat). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landusef_gc=initial_data_value
ELSE
ALLOCATE(grid%landusef_gc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef_gc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vlon_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vlon_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vlon_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_vlon_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vlon_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vlat_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vlat_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vlat_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_vlat_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vlat_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hlon_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hlon_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hlon_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_hlon_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hlon_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hlat_gc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hlat_gc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hlat_gc=initial_data_value
ELSE
ALLOCATE(grid%nmm_hlat_gc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hlat_gc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hbm2(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hbm2(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hbm2=initial_data_value
ELSE
ALLOCATE(grid%nmm_hbm2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hbm2(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hbm3(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hbm3(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hbm3=initial_data_value
ELSE
ALLOCATE(grid%nmm_hbm3(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hbm3(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vbm2(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vbm2(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vbm2=initial_data_value
ELSE
ALLOCATE(grid%nmm_vbm2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vbm2(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vbm3(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vbm3(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vbm3=initial_data_value
ELSE
ALLOCATE(grid%nmm_vbm3(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vbm3(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sm(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sm(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sm=initial_data_value
ELSE
ALLOCATE(grid%nmm_sm(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sm(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sice(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sice(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sice=initial_data_value
ELSE
ALLOCATE(grid%nmm_sice(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sice(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_ntsd=0
IF ( setinitval .EQ. 3 ) grid%nmm_nstart_hour=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pd(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pd=initial_data_value
ELSE
ALLOCATE(grid%nmm_pd(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pd_b(max(ed31,ed32),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd_b(max(ed31,ed32),1,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pd_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_pd_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pd_bt(max(ed31,ed32),1,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd_bt(max(ed31,ed32),1,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pd_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_pd_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pd_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fis(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fis(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fis=initial_data_value
ELSE
ALLOCATE(grid%nmm_fis(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fis(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_res(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_res(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_res=initial_data_value
ELSE
ALLOCATE(grid%nmm_res(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_res(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t=initial_data_value
ELSE
ALLOCATE(grid%nmm_t(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q=initial_data_value
ELSE
ALLOCATE(grid%nmm_q(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_q_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_q_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_u(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_u=initial_data_value
ELSE
ALLOCATE(grid%nmm_u(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_u_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_u_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_u_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_u_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_u_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_u_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_u_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_v(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_v=initial_data_value
ELSE
ALLOCATE(grid%nmm_v(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_v_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_v_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_v_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_v_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_v_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_v_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_v_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_told(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_told(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_told=initial_data_value
ELSE
ALLOCATE(grid%nmm_told(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_told(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_uold(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_uold(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_uold=initial_data_value
ELSE
ALLOCATE(grid%nmm_uold(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_uold(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vold(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vold(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vold=initial_data_value
ELSE
ALLOCATE(grid%nmm_vold(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vold(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dx_nmm(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dx_nmm(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dx_nmm=initial_data_value
ELSE
ALLOCATE(grid%nmm_dx_nmm(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dx_nmm(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_wpdar(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_wpdar(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_wpdar=initial_data_value
ELSE
ALLOCATE(grid%nmm_wpdar(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_wpdar(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cpgfu(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cpgfu(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cpgfu=initial_data_value
ELSE
ALLOCATE(grid%nmm_cpgfu(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cpgfu(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_curv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_curv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_curv=initial_data_value
ELSE
ALLOCATE(grid%nmm_curv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_curv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fcp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fcp(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fcp=initial_data_value
ELSE
ALLOCATE(grid%nmm_fcp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fcp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fdiv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fdiv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fdiv=initial_data_value
ELSE
ALLOCATE(grid%nmm_fdiv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fdiv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f=initial_data_value
ELSE
ALLOCATE(grid%nmm_f(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fad(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fad(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fad=initial_data_value
ELSE
ALLOCATE(grid%nmm_fad(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fad(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ddmpu(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddmpu(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ddmpu=initial_data_value
ELSE
ALLOCATE(grid%nmm_ddmpu(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddmpu(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ddmpv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddmpv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ddmpv=initial_data_value
ELSE
ALLOCATE(grid%nmm_ddmpv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddmpv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_deta(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_deta=initial_data_value
ELSE
ALLOCATE(grid%nmm_deta(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rdeta(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rdeta(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rdeta=initial_data_value
ELSE
ALLOCATE(grid%nmm_rdeta(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rdeta(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aeta(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aeta=initial_data_value
ELSE
ALLOCATE(grid%nmm_aeta(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f4q2(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f4q2(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f4q2=initial_data_value
ELSE
ALLOCATE(grid%nmm_f4q2(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f4q2(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_etax(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_etax(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_etax=initial_data_value
ELSE
ALLOCATE(grid%nmm_etax(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_etax(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dfl(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dfl(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dfl=initial_data_value
ELSE
ALLOCATE(grid%nmm_dfl(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dfl(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_deta1(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta1(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_deta1=initial_data_value
ELSE
ALLOCATE(grid%nmm_deta1(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta1(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aeta1(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta1(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aeta1=initial_data_value
ELSE
ALLOCATE(grid%nmm_aeta1(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta1(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_eta1(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_eta1(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_eta1=initial_data_value
ELSE
ALLOCATE(grid%nmm_eta1(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_eta1(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_deta2(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta2(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_deta2=initial_data_value
ELSE
ALLOCATE(grid%nmm_deta2(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deta2(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aeta2(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta2(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aeta2=initial_data_value
ELSE
ALLOCATE(grid%nmm_aeta2(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aeta2(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_eta2(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_eta2(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_eta2=initial_data_value
ELSE
ALLOCATE(grid%nmm_eta2(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_eta2(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_em(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_em(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_em=initial_data_value
ELSE
ALLOCATE(grid%nmm_em(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_em(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_emt(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_emt(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_emt=initial_data_value
ELSE
ALLOCATE(grid%nmm_emt(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_emt(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_adt(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adt(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_adt=initial_data_value
ELSE
ALLOCATE(grid%nmm_adt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_adu(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adu(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_adu=initial_data_value
ELSE
ALLOCATE(grid%nmm_adu(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adu(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_adv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_adv=initial_data_value
ELSE
ALLOCATE(grid%nmm_adv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_adv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_em_loc(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_em_loc(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_em_loc=initial_data_value
ELSE
ALLOCATE(grid%nmm_em_loc(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_em_loc(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_emt_loc(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_emt_loc(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_emt_loc=initial_data_value
ELSE
ALLOCATE(grid%nmm_emt_loc(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_emt_loc(1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_dy_nmm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_cpgfv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_en=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_ent=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_f4d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_f4q=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_ef4t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_upstrm=.FALSE.
IF ( setinitval .EQ. 3 ) grid%nmm_dlmd=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_dphd=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_pdtop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_pt=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pdsl(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdsl(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pdsl=initial_data_value
ELSE
ALLOCATE(grid%nmm_pdsl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdsl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pdslo(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdslo(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pdslo=initial_data_value
ELSE
ALLOCATE(grid%nmm_pdslo(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdslo(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_psdt(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_psdt(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_psdt=initial_data_value
ELSE
ALLOCATE(grid%nmm_psdt(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_psdt(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_div(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_div=initial_data_value
ELSE
ALLOCATE(grid%nmm_div(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_div(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_few(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_few(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_few=initial_data_value
ELSE
ALLOCATE(grid%nmm_few(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_few(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fne(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fne(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fne=initial_data_value
ELSE
ALLOCATE(grid%nmm_fne(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fne(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fns(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fns(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fns=initial_data_value
ELSE
ALLOCATE(grid%nmm_fns(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fns(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_fse(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fse(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_fse=initial_data_value
ELSE
ALLOCATE(grid%nmm_fse(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_fse(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_omgalf(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_omgalf(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_omgalf=initial_data_value
ELSE
ALLOCATE(grid%nmm_omgalf(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_omgalf(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_petdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_petdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_petdt=initial_data_value
ELSE
ALLOCATE(grid%nmm_petdt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_petdt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rtop(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rtop(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rtop=initial_data_value
ELSE
ALLOCATE(grid%nmm_rtop(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rtop(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pblh(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pblh(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pblh=initial_data_value
ELSE
ALLOCATE(grid%nmm_pblh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pblh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_lpbl(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lpbl(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_lpbl=0
ELSE
ALLOCATE(grid%nmm_lpbl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lpbl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ustar(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ustar(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ustar=initial_data_value
ELSE
ALLOCATE(grid%nmm_ustar(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ustar(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_z0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z0(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_z0=initial_data_value
ELSE
ALLOCATE(grid%nmm_z0(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z0(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_z0base(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z0base(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_z0base=initial_data_value
ELSE
ALLOCATE(grid%nmm_z0base(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z0base(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ths(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ths(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ths=initial_data_value
ELSE
ALLOCATE(grid%nmm_ths(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ths(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_mavail(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_mavail(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_mavail=initial_data_value
ELSE
ALLOCATE(grid%nmm_mavail(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_mavail(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_qsh(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qsh(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_qsh=initial_data_value
ELSE
ALLOCATE(grid%nmm_qsh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qsh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_twbs(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_twbs(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_twbs=initial_data_value
ELSE
ALLOCATE(grid%nmm_twbs(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_twbs(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_qwbs(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qwbs(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_qwbs=initial_data_value
ELSE
ALLOCATE(grid%nmm_qwbs(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qwbs(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_prec(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_prec(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_prec=initial_data_value
ELSE
ALLOCATE(grid%nmm_prec(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_prec(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aprec(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aprec(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aprec=initial_data_value
ELSE
ALLOCATE(grid%nmm_aprec(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aprec(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_acprec(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acprec(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_acprec=initial_data_value
ELSE
ALLOCATE(grid%nmm_acprec(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acprec(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cuprec(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cuprec(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cuprec=initial_data_value
ELSE
ALLOCATE(grid%nmm_cuprec(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cuprec(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_lspa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lspa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_lspa=initial_data_value
ELSE
ALLOCATE(grid%nmm_lspa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lspa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ddata(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddata(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ddata=initial_data_value
ELSE
ALLOCATE(grid%nmm_ddata(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ddata(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_accliq(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_accliq(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_accliq=initial_data_value
ELSE
ALLOCATE(grid%nmm_accliq(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_accliq(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sno(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sno(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sno=initial_data_value
ELSE
ALLOCATE(grid%nmm_sno(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sno(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_si(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_si(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_si=initial_data_value
ELSE
ALLOCATE(grid%nmm_si(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_si(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cldefi(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cldefi(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cldefi=initial_data_value
ELSE
ALLOCATE(grid%nmm_cldefi(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cldefi(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_deep(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deep(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_deep=initial_data_value
ELSE
ALLOCATE(grid%nmm_deep(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_deep(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rf(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rf(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rf=initial_data_value
ELSE
ALLOCATE(grid%nmm_rf(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rf(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_th10(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_th10(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_th10=initial_data_value
ELSE
ALLOCATE(grid%nmm_th10(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_th10(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q10(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q10(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q10=initial_data_value
ELSE
ALLOCATE(grid%nmm_q10(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q10(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pshltr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pshltr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pshltr=initial_data_value
ELSE
ALLOCATE(grid%nmm_pshltr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pshltr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tshltr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tshltr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tshltr=initial_data_value
ELSE
ALLOCATE(grid%nmm_tshltr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tshltr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_qshltr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qshltr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_qshltr=initial_data_value
ELSE
ALLOCATE(grid%nmm_qshltr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_qshltr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q2=initial_data_value
ELSE
ALLOCATE(grid%nmm_q2(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q2_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q2_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_q2_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_q2_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_q2_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_q2_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_q2_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_adj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_adj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_adj=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_adj(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_adj(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_t_old(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_old(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_t_old=initial_data_value
ELSE
ALLOCATE(grid%nmm_t_old(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_t_old(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_zero_3d(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_zero_3d(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_zero_3d=initial_data_value
ELSE
ALLOCATE(grid%nmm_zero_3d(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_zero_3d(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_w0avg(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_w0avg(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_w0avg=initial_data_value
ELSE
ALLOCATE(grid%nmm_w0avg(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_w0avg(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_akhs_out(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_akhs_out(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_akhs_out=initial_data_value
ELSE
ALLOCATE(grid%nmm_akhs_out(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_akhs_out(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_akms_out(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_akms_out(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_akms_out=initial_data_value
ELSE
ALLOCATE(grid%nmm_akms_out(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_akms_out(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_albase(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albase(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_albase=initial_data_value
ELSE
ALLOCATE(grid%nmm_albase(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albase(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_albedo(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albedo(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_albedo=initial_data_value
ELSE
ALLOCATE(grid%nmm_albedo(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_albedo(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cnvbot(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cnvbot(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cnvbot=initial_data_value
ELSE
ALLOCATE(grid%nmm_cnvbot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cnvbot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cnvtop(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cnvtop(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cnvtop=initial_data_value
ELSE
ALLOCATE(grid%nmm_cnvtop(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cnvtop(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_czen(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_czen(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_czen=initial_data_value
ELSE
ALLOCATE(grid%nmm_czen(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_czen(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_czmean(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_czmean(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_czmean=initial_data_value
ELSE
ALLOCATE(grid%nmm_czmean(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_czmean(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_epsr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_epsr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_epsr=initial_data_value
ELSE
ALLOCATE(grid%nmm_epsr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_epsr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_gffc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_gffc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_gffc=initial_data_value
ELSE
ALLOCATE(grid%nmm_gffc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_gffc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_glat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_glat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_glat=initial_data_value
ELSE
ALLOCATE(grid%nmm_glat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_glat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_glon(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_glon(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_glon=initial_data_value
ELSE
ALLOCATE(grid%nmm_glon(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_glon(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_nmm_tsk(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_nmm_tsk(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_nmm_tsk=initial_data_value
ELSE
ALLOCATE(grid%nmm_nmm_tsk(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_nmm_tsk(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hdac(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hdac(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hdac=initial_data_value
ELSE
ALLOCATE(grid%nmm_hdac(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hdac(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_hdacv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hdacv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_hdacv=initial_data_value
ELSE
ALLOCATE(grid%nmm_hdacv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_hdacv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_mxsnal(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_mxsnal(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_mxsnal=initial_data_value
ELSE
ALLOCATE(grid%nmm_mxsnal(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_mxsnal(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_radin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_radin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_radin=initial_data_value
ELSE
ALLOCATE(grid%nmm_radin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_radin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_radot(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_radot(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_radot=initial_data_value
ELSE
ALLOCATE(grid%nmm_radot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_radot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sigt4(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sigt4(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sigt4=initial_data_value
ELSE
ALLOCATE(grid%nmm_sigt4(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sigt4(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tg(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tg(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tg=initial_data_value
ELSE
ALLOCATE(grid%nmm_tg(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tg(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dfrlg(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dfrlg(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dfrlg=initial_data_value
ELSE
ALLOCATE(grid%nmm_dfrlg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dfrlg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_lvl(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lvl(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_lvl=0
ELSE
ALLOCATE(grid%nmm_lvl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_lvl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cwm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cwm=initial_data_value
ELSE
ALLOCATE(grid%nmm_cwm(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cwm_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cwm_b=initial_data_value
ELSE
ALLOCATE(grid%nmm_cwm_b(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm_b(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cwm_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cwm_bt=initial_data_value
ELSE
ALLOCATE(grid%nmm_cwm_bt(1,1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cwm_bt(1,1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f_ice(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_ice(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f_ice=initial_data_value
ELSE
ALLOCATE(grid%nmm_f_ice(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_ice(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f_rain(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_rain(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f_rain=initial_data_value
ELSE
ALLOCATE(grid%nmm_f_rain(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_rain(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_f_rimef(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_rimef(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_f_rimef=initial_data_value
ELSE
ALLOCATE(grid%nmm_f_rimef(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_f_rimef(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cldfra(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cldfra(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cldfra=initial_data_value
ELSE
ALLOCATE(grid%nmm_cldfra(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cldfra(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sr(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sr=initial_data_value
ELSE
ALLOCATE(grid%nmm_sr(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sr(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cfrach(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfrach(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cfrach=initial_data_value
ELSE
ALLOCATE(grid%nmm_cfrach(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfrach(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cfracl(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfracl(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cfracl=initial_data_value
ELSE
ALLOCATE(grid%nmm_cfracl(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfracl(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cfracm(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfracm(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cfracm=initial_data_value
ELSE
ALLOCATE(grid%nmm_cfracm(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cfracm(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_micro_start=.FALSE.
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_islope(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_islope(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_islope=0
ELSE
ALLOCATE(grid%nmm_islope(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_islope(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dzsoil(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dzsoil(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dzsoil=initial_data_value
ELSE
ALLOCATE(grid%nmm_dzsoil(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dzsoil(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rtdpth(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rtdpth(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rtdpth=initial_data_value
ELSE
ALLOCATE(grid%nmm_rtdpth(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rtdpth(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sldpth(sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sldpth(sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sldpth=initial_data_value
ELSE
ALLOCATE(grid%nmm_sldpth(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sldpth(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_cmc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cmc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_cmc=initial_data_value
ELSE
ALLOCATE(grid%nmm_cmc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_cmc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_grnflx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_grnflx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_grnflx=initial_data_value
ELSE
ALLOCATE(grid%nmm_grnflx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_grnflx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pctsno(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pctsno(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pctsno=initial_data_value
ELSE
ALLOCATE(grid%nmm_pctsno(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pctsno(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_soiltb(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_soiltb(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_soiltb=initial_data_value
ELSE
ALLOCATE(grid%nmm_soiltb(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_soiltb(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_vegfrc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vegfrc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_vegfrc=initial_data_value
ELSE
ALLOCATE(grid%nmm_vegfrc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_vegfrc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_shdmin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_shdmin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_shdmin=initial_data_value
ELSE
ALLOCATE(grid%nmm_shdmin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_shdmin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_shdmax(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_shdmax(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_shdmax=initial_data_value
ELSE
ALLOCATE(grid%nmm_shdmax(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_shdmax(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sh2o=initial_data_value
ELSE
ALLOCATE(grid%nmm_sh2o(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sh2o(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_smc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_smc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_smc=initial_data_value
ELSE
ALLOCATE(grid%nmm_smc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_smc(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_stc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_stc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_stc=initial_data_value
ELSE
ALLOCATE(grid%nmm_stc(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_stc(1,1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_hydro=.FALSE.
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dwdtmn(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmn(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dwdtmn=initial_data_value
ELSE
ALLOCATE(grid%nmm_dwdtmn(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmn(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dwdtmx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dwdtmx=initial_data_value
ELSE
ALLOCATE(grid%nmm_dwdtmx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdtmx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_dwdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_dwdt=initial_data_value
ELSE
ALLOCATE(grid%nmm_dwdt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_dwdt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pdwdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdwdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pdwdt=initial_data_value
ELSE
ALLOCATE(grid%nmm_pdwdt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pdwdt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_pint(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pint(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_pint=initial_data_value
ELSE
ALLOCATE(grid%nmm_pint(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_pint(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_w(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_w(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_w=initial_data_value
ELSE
ALLOCATE(grid%nmm_w(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_w(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_z=initial_data_value
ELSE
ALLOCATE(grid%nmm_z(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_z(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_acfrcv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acfrcv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_acfrcv=initial_data_value
ELSE
ALLOCATE(grid%nmm_acfrcv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acfrcv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_acfrst(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acfrst(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_acfrst=initial_data_value
ELSE
ALLOCATE(grid%nmm_acfrst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_acfrst(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ssroff(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ssroff(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ssroff=initial_data_value
ELSE
ALLOCATE(grid%nmm_ssroff(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ssroff(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_bgroff(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_bgroff(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_bgroff=initial_data_value
ELSE
ALLOCATE(grid%nmm_bgroff(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_bgroff(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rlwin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rlwin=initial_data_value
ELSE
ALLOCATE(grid%nmm_rlwin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rlwout(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwout(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rlwout=initial_data_value
ELSE
ALLOCATE(grid%nmm_rlwout(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwout(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rlwtoa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwtoa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rlwtoa=initial_data_value
ELSE
ALLOCATE(grid%nmm_rlwtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_alwin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_alwin=initial_data_value
ELSE
ALLOCATE(grid%nmm_alwin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_alwout(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwout(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_alwout=initial_data_value
ELSE
ALLOCATE(grid%nmm_alwout(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwout(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_alwtoa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwtoa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_alwtoa=initial_data_value
ELSE
ALLOCATE(grid%nmm_alwtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_alwtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswin=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswinc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswinc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswinc=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswinc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswinc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswout(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswout(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswout=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswout(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswout(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswtoa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswtoa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswtoa=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aswin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aswin=initial_data_value
ELSE
ALLOCATE(grid%nmm_aswin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aswout(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswout(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aswout=initial_data_value
ELSE
ALLOCATE(grid%nmm_aswout(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswout(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_aswtoa(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswtoa(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_aswtoa=initial_data_value
ELSE
ALLOCATE(grid%nmm_aswtoa(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_aswtoa(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sfcshx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfcshx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sfcshx=initial_data_value
ELSE
ALLOCATE(grid%nmm_sfcshx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfcshx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sfclhx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfclhx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sfclhx=initial_data_value
ELSE
ALLOCATE(grid%nmm_sfclhx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfclhx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_subshx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_subshx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_subshx=initial_data_value
ELSE
ALLOCATE(grid%nmm_subshx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_subshx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_snopcx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_snopcx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_snopcx=initial_data_value
ELSE
ALLOCATE(grid%nmm_snopcx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_snopcx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_sfcuvx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfcuvx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_sfcuvx=initial_data_value
ELSE
ALLOCATE(grid%nmm_sfcuvx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_sfcuvx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_potevp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_potevp(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_potevp=initial_data_value
ELSE
ALLOCATE(grid%nmm_potevp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_potevp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_potflx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_potflx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_potflx=initial_data_value
ELSE
ALLOCATE(grid%nmm_potflx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_potflx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tlmin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tlmin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tlmin=initial_data_value
ELSE
ALLOCATE(grid%nmm_tlmin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tlmin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tlmax(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tlmax(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tlmax=initial_data_value
ELSE
ALLOCATE(grid%nmm_tlmax(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tlmax(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rlwtt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwtt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rlwtt=initial_data_value
ELSE
ALLOCATE(grid%nmm_rlwtt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rlwtt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_rswtt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswtt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_rswtt=initial_data_value
ELSE
ALLOCATE(grid%nmm_rswtt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_rswtt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_tcucn(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tcucn(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tcucn=initial_data_value
ELSE
ALLOCATE(grid%nmm_tcucn(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_tcucn(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_train(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_train(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_train=initial_data_value
ELSE
ALLOCATE(grid%nmm_train(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_train(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ncfrcv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ncfrcv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ncfrcv=0
ELSE
ALLOCATE(grid%nmm_ncfrcv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ncfrcv(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ncfrst(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ncfrst(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ncfrst=0
ELSE
ALLOCATE(grid%nmm_ncfrst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ncfrst(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nmm_nphs0=0
IF ( setinitval .EQ. 3 ) grid%nmm_nprec=0
IF ( setinitval .EQ. 3 ) grid%nmm_nclod=0
IF ( setinitval .EQ. 3 ) grid%nmm_nheat=0
IF ( setinitval .EQ. 3 ) grid%nmm_nrdlw=0
IF ( setinitval .EQ. 3 ) grid%nmm_nrdsw=0
IF ( setinitval .EQ. 3 ) grid%nmm_nsrfc=0
IF ( setinitval .EQ. 3 ) grid%nmm_avrain=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_avcnvc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_aratim=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_acutim=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_ardlw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_ardsw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_asrfc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nmm_aphtim=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ihe(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihe(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ihe=0
ELSE
ALLOCATE(grid%nmm_ihe(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihe(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ihw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihw(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ihw=0
ELSE
ALLOCATE(grid%nmm_ihw(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihw(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ive(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ive(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ive=0
ELSE
ALLOCATE(grid%nmm_ive(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ive(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ivw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ivw(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ivw=0
ELSE
ALLOCATE(grid%nmm_ivw(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ivw(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_irad(sm31:em31),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_irad(sm31:em31). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_irad=0
ELSE
ALLOCATE(grid%nmm_irad(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_irad(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iheg(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iheg(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iheg=0
ELSE
ALLOCATE(grid%nmm_iheg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iheg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ihwg(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihwg(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ihwg=0
ELSE
ALLOCATE(grid%nmm_ihwg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ihwg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iveg(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iveg(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iveg=0
ELSE
ALLOCATE(grid%nmm_iveg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iveg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_ivwg(1:2600),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ivwg(1:2600). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_ivwg=0
ELSE
ALLOCATE(grid%nmm_ivwg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_ivwg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iradg(1:2000),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iradg(1:2000). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iradg=0
ELSE
ALLOCATE(grid%nmm_iradg(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iradg(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_n_iup_h(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_h(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_n_iup_h=0
ELSE
ALLOCATE(grid%nmm_n_iup_h(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_h(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_n_iup_v(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_v(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_n_iup_v=0
ELSE
ALLOCATE(grid%nmm_n_iup_v(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_v(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_n_iup_adh(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adh(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_n_iup_adh=0
ELSE
ALLOCATE(grid%nmm_n_iup_adh(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adh(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_n_iup_adv(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adv(sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_n_iup_adv=0
ELSE
ALLOCATE(grid%nmm_n_iup_adv(1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_n_iup_adv(1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iup_h(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_h(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iup_h=0
ELSE
ALLOCATE(grid%nmm_iup_h(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_h(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iup_v(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_v(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iup_v=0
ELSE
ALLOCATE(grid%nmm_iup_v(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_v(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iup_adh(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_adh(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iup_adh=0
ELSE
ALLOCATE(grid%nmm_iup_adh(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_adh(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_iup_adv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_adv(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_iup_adv=0
ELSE
ALLOCATE(grid%nmm_iup_adv(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_iup_adv(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%imicrogram=0
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%imask_nostag(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_nostag(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%imask_xstag(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xstag(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%imask_ystag(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_ystag(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%imask_xystag(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xystag=0
ELSE
ALLOCATE(grid%imask_xystag(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%imask_xystag(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sm000007(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000007(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sm007028(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm007028(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sm028100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm028100(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sm100255(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100255(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%st000007(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000007(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%st007028(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st007028(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%st028100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st028100(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%st100255(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100255(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sm000010(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm000010(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sm010040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010040(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sm040100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm040100(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sm100200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm100200(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sm010200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sm010200(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilm000(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm000(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilm005(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm005(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilm020(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm020(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilm040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm040(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilm160(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm160(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilm300(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilm300(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sw000010(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw000010(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sw010040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010040(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sw040100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw040100(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sw100200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw100200(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sw010200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sw010200(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilw000(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw000(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilw005(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw005(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilw020(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw020(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilw040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw040(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilw160(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw160(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilw300(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilw300(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%st000010(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st000010(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%st010040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010040(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%st040100(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st040100(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%st100200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st100200(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%st010200(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%st010200(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilt000(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt000(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilt005(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt005(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilt020(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt020(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilt040(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt040(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilt160(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt160(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilt300(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt300(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt300=initial_data_value
ELSE
ALLOCATE(grid%soilt300(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt300(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%landmask(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landmask(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landmask=initial_data_value
ELSE
ALLOCATE(grid%landmask(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landmask(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%topostdv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%topostdv(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%toposlpx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpx(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%toposlpy(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposlpy(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%greenmax(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%greenmax(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%greenmax=initial_data_value
ELSE
ALLOCATE(grid%greenmax(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%greenmax(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%greenmin(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%greenmin(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%greenmin=initial_data_value
ELSE
ALLOCATE(grid%greenmin(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%greenmin(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%albedomx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albedomx(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedomx=initial_data_value
ELSE
ALLOCATE(grid%albedomx(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albedomx(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%slopecat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%slopecat(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%toposoil(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%toposoil(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm32:em32). ')
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
ALLOCATE(grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32). ')
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
ALLOCATE(grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot=initial_data_value
ELSE
ALLOCATE(grid%soilcbot(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilcbot(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist=initial_data_value
ALLOCATE(grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%scalar_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_b(max(ed31,ed32),sd33:ed33,spec_bdy_width,4,num_scalar). ')
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
ALLOCATE(grid%scalar_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_bt(max(ed31,ed32),sd33:ed33,spec_bdy_width,4,num_scalar). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_bt=initial_data_value
ELSE
ALLOCATE(grid%scalar_bt(1,1,1,1,num_scalar),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%scalar_bt(1,1,1,1,num_scalar).  ')
 endif
ENDIF
ALLOCATE(grid%chem(sm31:em31,sm33:em33,sm32:em32,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%chem(sm31:em31,sm33:em33,sm32:em32,num_chem). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chem=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smois=initial_data_value
ELSE
ALLOCATE(grid%smois(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smois(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tslb=initial_data_value
ELSE
ALLOCATE(grid%tslb(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tslb(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%gsw(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gsw(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gsw=initial_data_value
ELSE
ALLOCATE(grid%gsw(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%gsw(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xlat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat=initial_data_value
ELSE
ALLOCATE(grid%xlat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xlong(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlong(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong=initial_data_value
ELSE
ALLOCATE(grid%xlong(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xlong(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xland(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xland(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xland=initial_data_value
ELSE
ALLOCATE(grid%xland(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xland(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%raincv(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%raincv(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%psfc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%psfc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfc=initial_data_value
ELSE
ALLOCATE(grid%psfc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%psfc(1,1).  ')
 endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%dtbc=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%th2(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%th2(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th2=initial_data_value
ELSE
ALLOCATE(grid%th2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%th2(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%t2(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%t2(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2=initial_data_value
ELSE
ALLOCATE(grid%t2(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%t2(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%u10(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u10(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u10=initial_data_value
ELSE
ALLOCATE(grid%u10(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%u10(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%v10(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v10(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v10=initial_data_value
ELSE
ALLOCATE(grid%v10(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%v10(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%xice(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice=initial_data_value
ELSE
ALLOCATE(grid%xice(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xice(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smstav(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstav(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstav=initial_data_value
ELSE
ALLOCATE(grid%smstav(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstav(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smstot(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstot(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstot=initial_data_value
ELSE
ALLOCATE(grid%smstot(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smstot(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sfcrunoff(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcrunoff(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcrunoff=initial_data_value
ELSE
ALLOCATE(grid%sfcrunoff(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcrunoff(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%udrunoff(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%udrunoff(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%udrunoff=initial_data_value
ELSE
ALLOCATE(grid%udrunoff(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%udrunoff(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%ivgtyp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ivgtyp(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ivgtyp=0
ELSE
ALLOCATE(grid%ivgtyp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%ivgtyp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%isltyp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%isltyp(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%isltyp=0
ELSE
ALLOCATE(grid%isltyp(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%isltyp(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vegfra(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegfra(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegfra=initial_data_value
ELSE
ALLOCATE(grid%vegfra(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vegfra(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sfcevp(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcevp(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%grdflx(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%grdflx(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%albbck(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%sfcexc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcexc(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcexc=initial_data_value
ELSE
ALLOCATE(grid%sfcexc(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sfcexc(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%acsnow(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnow(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnow=initial_data_value
ELSE
ALLOCATE(grid%acsnow(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnow(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%acsnom(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnom(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnom=initial_data_value
ELSE
ALLOCATE(grid%acsnom(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%acsnom(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%rmol(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rmol(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%snow(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snow(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snow=initial_data_value
ELSE
ALLOCATE(grid%snow(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snow(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%canwat(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%canwat(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%canwat=initial_data_value
ELSE
ALLOCATE(grid%canwat(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%canwat(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%sst(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sst(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sst=initial_data_value
ELSE
ALLOCATE(grid%sst(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%sst(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%weasd(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%weasd(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%weasd=initial_data_value
ELSE
ALLOCATE(grid%weasd(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%weasd(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%znt(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%mol(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%el_myj(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%el_myj(sm31:em31,sm33:em33,sm32:em32). ')
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
ALLOCATE(grid%exch_h(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%exch_h(sm31:em31,sm33:em33,sm32:em32). ')
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
ALLOCATE(grid%thz0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%thz0(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%qz0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qz0(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%uz0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%vz0(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%flhc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flhc(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%flqc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%flqc(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%qsg(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsg(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%qvg(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qvg(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%qcg(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qcg(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%soilt1(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%soilt1(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%tsnav(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsnav(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsnav=initial_data_value
ELSE
ALLOCATE(grid%tsnav(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%tsnav(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%nmm_psfc_out(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_psfc_out(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_psfc_out=initial_data_value
ELSE
ALLOCATE(grid%nmm_psfc_out(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%nmm_psfc_out(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%uz0h(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0h(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uz0h=initial_data_value
ELSE
ALLOCATE(grid%uz0h(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%uz0h(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%vz0h(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0h(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vz0h=initial_data_value
ELSE
ALLOCATE(grid%vz0h(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%vz0h(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dudt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dudt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dudt=initial_data_value
ELSE
ALLOCATE(grid%dudt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dudt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%dvdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dvdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dvdt=initial_data_value
ELSE
ALLOCATE(grid%dvdt(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%dvdt(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%qsfc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%qsfc(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%akhs(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akhs(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%akms(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%akms(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%htop(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htop(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%hbot(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbot(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%htopr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopr(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%hbotr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotr(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%htopd(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopd(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htopd=initial_data_value
ELSE
ALLOCATE(grid%htopd(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htopd(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hbotd(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotd(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbotd=initial_data_value
ELSE
ALLOCATE(grid%hbotd(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbotd(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%htops(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htops(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htops=initial_data_value
ELSE
ALLOCATE(grid%htops(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%htops(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%hbots(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbots(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbots=initial_data_value
ELSE
ALLOCATE(grid%hbots(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%hbots(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%cuppt(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cuppt(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%cprate(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cprate(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cprate=initial_data_value
ELSE
ALLOCATE(grid%cprate(1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%cprate(1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%f_ice_phy(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_ice_phy(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_ice_phy=initial_data_value
ELSE
ALLOCATE(grid%f_ice_phy(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_ice_phy(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%f_rain_phy(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rain_phy(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rain_phy=initial_data_value
ELSE
ALLOCATE(grid%f_rain_phy(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rain_phy(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%f_rimef_phy(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rimef_phy=initial_data_value
ELSE
ALLOCATE(grid%f_rimef_phy(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(1,1,1).  ')
 endif
ENDIF
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%mass_flux(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%mass_flux(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%apr_gr(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_gr(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%apr_w(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_w(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%apr_mc(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_mc(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%apr_st(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_st(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%apr_as(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_as(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%apr_capma(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capma(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%apr_capme(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capme(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%apr_capmi(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%apr_capmi(sm31:em31,sm32:em32). ')
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
ALLOCATE(grid%xf_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%xf_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim). ')
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
ALLOCATE(grid%pr_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%pr_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim). ')
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
ALLOCATE(grid%rthften(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rthften(sm31:em31,sm33:em33,sm32:em32). ')
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
ALLOCATE(grid%rqvften(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvften(sm31:em31,sm33:em33,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvften=initial_data_value
ELSE
ALLOCATE(grid%rqvften(1,1,1),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rqvften(1,1,1).  ')
 endif
ENDIF
ALLOCATE(grid%snowh(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%snowh(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowh=initial_data_value
ALLOCATE(grid%rhosn(sm31:em31,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%rhosn(sm31:em31,sm32:em32). ')
 endif
IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rhosn=initial_data_value
IF(.NOT.inter_domain)THEN
ALLOCATE(grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
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
ALLOCATE(grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
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
IF ( setinitval .EQ. 3 ) grid%number_at_same_level=0
IF ( setinitval .EQ. 3 ) grid%itimestep=0
IF ( setinitval .EQ. 3 ) grid%xtime=initial_data_value
IF ( setinitval .EQ. 3 ) grid%julian=initial_data_value
IF ( setinitval .EQ. 3 ) grid%lbc_fid=0
IF ( setinitval .EQ. 3 ) grid%tiled=.FALSE.
IF ( setinitval .EQ. 3 ) grid%patched=.FALSE.
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
IF ( setinitval .EQ. 3 ) grid%tstart=initial_data_value
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
IF ( setinitval .EQ. 3 ) grid%ptsgm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_metgrid_levels=0
IF ( setinitval .EQ. 3 ) grid%p_top_requested=initial_data_value
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
IF ( setinitval .EQ. 3 ) grid%idtad=0
IF ( setinitval .EQ. 3 ) grid%nsoil=0
IF ( setinitval .EQ. 3 ) grid%nphs=0
IF ( setinitval .EQ. 3 ) grid%ncnvc=0
IF ( setinitval .EQ. 3 ) grid%nrads=0
IF ( setinitval .EQ. 3 ) grid%nradl=0
IF ( setinitval .EQ. 3 ) grid%tprec=initial_data_value
IF ( setinitval .EQ. 3 ) grid%theat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tclod=initial_data_value
IF ( setinitval .EQ. 3 ) grid%trdsw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%trdlw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tsrfc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pcpflg=.FALSE.
IF ( setinitval .EQ. 3 ) grid%sigma=0
IF ( setinitval .EQ. 3 ) grid%co2tf=0
IF ( setinitval .EQ. 3 ) grid%ra_call_offset=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_freq_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%levsiz=0
IF ( setinitval .EQ. 3 ) grid%paerlev=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim1=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim2=0
IF ( setinitval .EQ. 3 ) grid%cu_rad_feedback=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dyn_opt=0
IF ( setinitval .EQ. 3 ) grid%rk_ord=0
IF ( setinitval .EQ. 3 ) grid%w_damping=0
IF ( setinitval .EQ. 3 ) grid%diff_opt=0
IF ( setinitval .EQ. 3 ) grid%km_opt=0
IF ( setinitval .EQ. 3 ) grid%damp_opt=0
IF ( setinitval .EQ. 3 ) grid%zdamp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_pres=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_lapse=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dampcoef=initial_data_value
IF ( setinitval .EQ. 3 ) grid%khdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kvdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%smdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%emdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epssm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%non_hydrostatic=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step_sound=0
IF ( setinitval .EQ. 3 ) grid%h_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%h_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%top_radiation=.FALSE.
IF ( setinitval .EQ. 3 ) grid%mix_cr_len=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kh_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kv_tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_drag_coefficient=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_heat_flux=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pert_coriolis=.FALSE.
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
!ENDOFREGISTRYGENERATEDINCLUDE
