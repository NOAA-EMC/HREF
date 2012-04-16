!STARTOFREGISTRYGENERATEDINCLUDE 'inc/deallocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( ASSOCIATED( grid%x_1 ) ) THEN 
  DEALLOCATE(grid%x_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%x_1. ')
 endif
  NULLIFY(grid%x_1)
ENDIF
IF ( ASSOCIATED( grid%x_2 ) ) THEN 
  DEALLOCATE(grid%x_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%x_2. ')
 endif
  NULLIFY(grid%x_2)
ENDIF
IF ( ASSOCIATED( grid%lu_index ) ) THEN 
  DEALLOCATE(grid%lu_index,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lu_index. ')
 endif
  NULLIFY(grid%lu_index)
ENDIF
IF ( ASSOCIATED( grid%lu_mask ) ) THEN 
  DEALLOCATE(grid%lu_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lu_mask. ')
 endif
  NULLIFY(grid%lu_mask)
ENDIF
IF ( ASSOCIATED( grid%p_gc ) ) THEN 
  DEALLOCATE(grid%p_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%p_gc. ')
 endif
  NULLIFY(grid%p_gc)
ENDIF
IF ( ASSOCIATED( grid%vegcat ) ) THEN 
  DEALLOCATE(grid%vegcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vegcat. ')
 endif
  NULLIFY(grid%vegcat)
ENDIF
IF ( ASSOCIATED( grid%soilcat ) ) THEN 
  DEALLOCATE(grid%soilcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcat. ')
 endif
  NULLIFY(grid%soilcat)
ENDIF
IF ( ASSOCIATED( grid%input_soil_cat ) ) THEN 
  DEALLOCATE(grid%input_soil_cat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%input_soil_cat. ')
 endif
  NULLIFY(grid%input_soil_cat)
ENDIF
IF ( ASSOCIATED( grid%tsk_gc ) ) THEN 
  DEALLOCATE(grid%tsk_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tsk_gc. ')
 endif
  NULLIFY(grid%tsk_gc)
ENDIF
IF ( ASSOCIATED( grid%xice_gc ) ) THEN 
  DEALLOCATE(grid%xice_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xice_gc. ')
 endif
  NULLIFY(grid%xice_gc)
ENDIF
IF ( ASSOCIATED( grid%ght_gc ) ) THEN 
  DEALLOCATE(grid%ght_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ght_gc. ')
 endif
  NULLIFY(grid%ght_gc)
ENDIF
IF ( ASSOCIATED( grid%rh_gc ) ) THEN 
  DEALLOCATE(grid%rh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rh_gc. ')
 endif
  NULLIFY(grid%rh_gc)
ENDIF
IF ( ASSOCIATED( grid%v_gc ) ) THEN 
  DEALLOCATE(grid%v_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_gc. ')
 endif
  NULLIFY(grid%v_gc)
ENDIF
IF ( ASSOCIATED( grid%u_gc ) ) THEN 
  DEALLOCATE(grid%u_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_gc. ')
 endif
  NULLIFY(grid%u_gc)
ENDIF
IF ( ASSOCIATED( grid%t_gc ) ) THEN 
  DEALLOCATE(grid%t_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_gc. ')
 endif
  NULLIFY(grid%t_gc)
ENDIF
IF ( ASSOCIATED( grid%snoalb ) ) THEN 
  DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snoalb. ')
 endif
  NULLIFY(grid%snoalb)
ENDIF
IF ( ASSOCIATED( grid%greenfrac_gc ) ) THEN 
  DEALLOCATE(grid%greenfrac_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%greenfrac_gc. ')
 endif
  NULLIFY(grid%greenfrac_gc)
ENDIF
IF ( ASSOCIATED( grid%albedo12m_gc ) ) THEN 
  DEALLOCATE(grid%albedo12m_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albedo12m_gc. ')
 endif
  NULLIFY(grid%albedo12m_gc)
ENDIF
IF ( ASSOCIATED( grid%soilcbot_gc ) ) THEN 
  DEALLOCATE(grid%soilcbot_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcbot_gc. ')
 endif
  NULLIFY(grid%soilcbot_gc)
ENDIF
IF ( ASSOCIATED( grid%soilctop_gc ) ) THEN 
  DEALLOCATE(grid%soilctop_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilctop_gc. ')
 endif
  NULLIFY(grid%soilctop_gc)
ENDIF
IF ( ASSOCIATED( grid%tmn_gc ) ) THEN 
  DEALLOCATE(grid%tmn_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tmn_gc. ')
 endif
  NULLIFY(grid%tmn_gc)
ENDIF
IF ( ASSOCIATED( grid%htv_gc ) ) THEN 
  DEALLOCATE(grid%htv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htv_gc. ')
 endif
  NULLIFY(grid%htv_gc)
ENDIF
IF ( ASSOCIATED( grid%ht_gc ) ) THEN 
  DEALLOCATE(grid%ht_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ht_gc. ')
 endif
  NULLIFY(grid%ht_gc)
ENDIF
IF ( ASSOCIATED( grid%landusef_gc ) ) THEN 
  DEALLOCATE(grid%landusef_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landusef_gc. ')
 endif
  NULLIFY(grid%landusef_gc)
ENDIF
IF ( ASSOCIATED( grid%vlon_gc ) ) THEN 
  DEALLOCATE(grid%vlon_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vlon_gc. ')
 endif
  NULLIFY(grid%vlon_gc)
ENDIF
IF ( ASSOCIATED( grid%vlat_gc ) ) THEN 
  DEALLOCATE(grid%vlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vlat_gc. ')
 endif
  NULLIFY(grid%vlat_gc)
ENDIF
IF ( ASSOCIATED( grid%hlon_gc ) ) THEN 
  DEALLOCATE(grid%hlon_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hlon_gc. ')
 endif
  NULLIFY(grid%hlon_gc)
ENDIF
IF ( ASSOCIATED( grid%hlat_gc ) ) THEN 
  DEALLOCATE(grid%hlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hlat_gc. ')
 endif
  NULLIFY(grid%hlat_gc)
ENDIF
IF ( ASSOCIATED( grid%hbm2 ) ) THEN 
  DEALLOCATE(grid%hbm2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbm2. ')
 endif
  NULLIFY(grid%hbm2)
ENDIF
IF ( ASSOCIATED( grid%hbm3 ) ) THEN 
  DEALLOCATE(grid%hbm3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbm3. ')
 endif
  NULLIFY(grid%hbm3)
ENDIF
IF ( ASSOCIATED( grid%vbm2 ) ) THEN 
  DEALLOCATE(grid%vbm2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vbm2. ')
 endif
  NULLIFY(grid%vbm2)
ENDIF
IF ( ASSOCIATED( grid%vbm3 ) ) THEN 
  DEALLOCATE(grid%vbm3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vbm3. ')
 endif
  NULLIFY(grid%vbm3)
ENDIF
IF ( ASSOCIATED( grid%sm ) ) THEN 
  DEALLOCATE(grid%sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm. ')
 endif
  NULLIFY(grid%sm)
ENDIF
IF ( ASSOCIATED( grid%sice ) ) THEN 
  DEALLOCATE(grid%sice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sice. ')
 endif
  NULLIFY(grid%sice)
ENDIF
IF ( ASSOCIATED( grid%pd ) ) THEN 
  DEALLOCATE(grid%pd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pd. ')
 endif
  NULLIFY(grid%pd)
ENDIF
IF ( ASSOCIATED( grid%pd_bxs ) ) THEN 
  DEALLOCATE(grid%pd_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pd_bxs. ')
 endif
  NULLIFY(grid%pd_bxs)
ENDIF
IF ( ASSOCIATED( grid%pd_bxe ) ) THEN 
  DEALLOCATE(grid%pd_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pd_bxe. ')
 endif
  NULLIFY(grid%pd_bxe)
ENDIF
IF ( ASSOCIATED( grid%pd_bys ) ) THEN 
  DEALLOCATE(grid%pd_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pd_bys. ')
 endif
  NULLIFY(grid%pd_bys)
ENDIF
IF ( ASSOCIATED( grid%pd_bye ) ) THEN 
  DEALLOCATE(grid%pd_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pd_bye. ')
 endif
  NULLIFY(grid%pd_bye)
ENDIF
IF ( ASSOCIATED( grid%pd_btxs ) ) THEN 
  DEALLOCATE(grid%pd_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pd_btxs. ')
 endif
  NULLIFY(grid%pd_btxs)
ENDIF
IF ( ASSOCIATED( grid%pd_btxe ) ) THEN 
  DEALLOCATE(grid%pd_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pd_btxe. ')
 endif
  NULLIFY(grid%pd_btxe)
ENDIF
IF ( ASSOCIATED( grid%pd_btys ) ) THEN 
  DEALLOCATE(grid%pd_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pd_btys. ')
 endif
  NULLIFY(grid%pd_btys)
ENDIF
IF ( ASSOCIATED( grid%pd_btye ) ) THEN 
  DEALLOCATE(grid%pd_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pd_btye. ')
 endif
  NULLIFY(grid%pd_btye)
ENDIF
IF ( ASSOCIATED( grid%fis ) ) THEN 
  DEALLOCATE(grid%fis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fis. ')
 endif
  NULLIFY(grid%fis)
ENDIF
IF ( ASSOCIATED( grid%res ) ) THEN 
  DEALLOCATE(grid%res,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%res. ')
 endif
  NULLIFY(grid%res)
ENDIF
IF ( ASSOCIATED( grid%t ) ) THEN 
  DEALLOCATE(grid%t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t. ')
 endif
  NULLIFY(grid%t)
ENDIF
IF ( ASSOCIATED( grid%t_bxs ) ) THEN 
  DEALLOCATE(grid%t_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_bxs. ')
 endif
  NULLIFY(grid%t_bxs)
ENDIF
IF ( ASSOCIATED( grid%t_bxe ) ) THEN 
  DEALLOCATE(grid%t_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_bxe. ')
 endif
  NULLIFY(grid%t_bxe)
ENDIF
IF ( ASSOCIATED( grid%t_bys ) ) THEN 
  DEALLOCATE(grid%t_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_bys. ')
 endif
  NULLIFY(grid%t_bys)
ENDIF
IF ( ASSOCIATED( grid%t_bye ) ) THEN 
  DEALLOCATE(grid%t_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_bye. ')
 endif
  NULLIFY(grid%t_bye)
ENDIF
IF ( ASSOCIATED( grid%t_btxs ) ) THEN 
  DEALLOCATE(grid%t_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_btxs. ')
 endif
  NULLIFY(grid%t_btxs)
ENDIF
IF ( ASSOCIATED( grid%t_btxe ) ) THEN 
  DEALLOCATE(grid%t_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_btxe. ')
 endif
  NULLIFY(grid%t_btxe)
ENDIF
IF ( ASSOCIATED( grid%t_btys ) ) THEN 
  DEALLOCATE(grid%t_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_btys. ')
 endif
  NULLIFY(grid%t_btys)
ENDIF
IF ( ASSOCIATED( grid%t_btye ) ) THEN 
  DEALLOCATE(grid%t_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_btye. ')
 endif
  NULLIFY(grid%t_btye)
ENDIF
IF ( ASSOCIATED( grid%q ) ) THEN 
  DEALLOCATE(grid%q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q. ')
 endif
  NULLIFY(grid%q)
ENDIF
IF ( ASSOCIATED( grid%q_bxs ) ) THEN 
  DEALLOCATE(grid%q_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q_bxs. ')
 endif
  NULLIFY(grid%q_bxs)
ENDIF
IF ( ASSOCIATED( grid%q_bxe ) ) THEN 
  DEALLOCATE(grid%q_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q_bxe. ')
 endif
  NULLIFY(grid%q_bxe)
ENDIF
IF ( ASSOCIATED( grid%q_bys ) ) THEN 
  DEALLOCATE(grid%q_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q_bys. ')
 endif
  NULLIFY(grid%q_bys)
ENDIF
IF ( ASSOCIATED( grid%q_bye ) ) THEN 
  DEALLOCATE(grid%q_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q_bye. ')
 endif
  NULLIFY(grid%q_bye)
ENDIF
IF ( ASSOCIATED( grid%q_btxs ) ) THEN 
  DEALLOCATE(grid%q_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q_btxs. ')
 endif
  NULLIFY(grid%q_btxs)
ENDIF
IF ( ASSOCIATED( grid%q_btxe ) ) THEN 
  DEALLOCATE(grid%q_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q_btxe. ')
 endif
  NULLIFY(grid%q_btxe)
ENDIF
IF ( ASSOCIATED( grid%q_btys ) ) THEN 
  DEALLOCATE(grid%q_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q_btys. ')
 endif
  NULLIFY(grid%q_btys)
ENDIF
IF ( ASSOCIATED( grid%q_btye ) ) THEN 
  DEALLOCATE(grid%q_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q_btye. ')
 endif
  NULLIFY(grid%q_btye)
ENDIF
IF ( ASSOCIATED( grid%u ) ) THEN 
  DEALLOCATE(grid%u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u. ')
 endif
  NULLIFY(grid%u)
ENDIF
IF ( ASSOCIATED( grid%u_bxs ) ) THEN 
  DEALLOCATE(grid%u_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_bxs. ')
 endif
  NULLIFY(grid%u_bxs)
ENDIF
IF ( ASSOCIATED( grid%u_bxe ) ) THEN 
  DEALLOCATE(grid%u_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_bxe. ')
 endif
  NULLIFY(grid%u_bxe)
ENDIF
IF ( ASSOCIATED( grid%u_bys ) ) THEN 
  DEALLOCATE(grid%u_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_bys. ')
 endif
  NULLIFY(grid%u_bys)
ENDIF
IF ( ASSOCIATED( grid%u_bye ) ) THEN 
  DEALLOCATE(grid%u_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_bye. ')
 endif
  NULLIFY(grid%u_bye)
ENDIF
IF ( ASSOCIATED( grid%u_btxs ) ) THEN 
  DEALLOCATE(grid%u_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_btxs. ')
 endif
  NULLIFY(grid%u_btxs)
ENDIF
IF ( ASSOCIATED( grid%u_btxe ) ) THEN 
  DEALLOCATE(grid%u_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_btxe. ')
 endif
  NULLIFY(grid%u_btxe)
ENDIF
IF ( ASSOCIATED( grid%u_btys ) ) THEN 
  DEALLOCATE(grid%u_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_btys. ')
 endif
  NULLIFY(grid%u_btys)
ENDIF
IF ( ASSOCIATED( grid%u_btye ) ) THEN 
  DEALLOCATE(grid%u_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u_btye. ')
 endif
  NULLIFY(grid%u_btye)
ENDIF
IF ( ASSOCIATED( grid%v ) ) THEN 
  DEALLOCATE(grid%v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v. ')
 endif
  NULLIFY(grid%v)
ENDIF
IF ( ASSOCIATED( grid%v_bxs ) ) THEN 
  DEALLOCATE(grid%v_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_bxs. ')
 endif
  NULLIFY(grid%v_bxs)
ENDIF
IF ( ASSOCIATED( grid%v_bxe ) ) THEN 
  DEALLOCATE(grid%v_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_bxe. ')
 endif
  NULLIFY(grid%v_bxe)
ENDIF
IF ( ASSOCIATED( grid%v_bys ) ) THEN 
  DEALLOCATE(grid%v_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_bys. ')
 endif
  NULLIFY(grid%v_bys)
ENDIF
IF ( ASSOCIATED( grid%v_bye ) ) THEN 
  DEALLOCATE(grid%v_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_bye. ')
 endif
  NULLIFY(grid%v_bye)
ENDIF
IF ( ASSOCIATED( grid%v_btxs ) ) THEN 
  DEALLOCATE(grid%v_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_btxs. ')
 endif
  NULLIFY(grid%v_btxs)
ENDIF
IF ( ASSOCIATED( grid%v_btxe ) ) THEN 
  DEALLOCATE(grid%v_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_btxe. ')
 endif
  NULLIFY(grid%v_btxe)
ENDIF
IF ( ASSOCIATED( grid%v_btys ) ) THEN 
  DEALLOCATE(grid%v_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_btys. ')
 endif
  NULLIFY(grid%v_btys)
ENDIF
IF ( ASSOCIATED( grid%v_btye ) ) THEN 
  DEALLOCATE(grid%v_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v_btye. ')
 endif
  NULLIFY(grid%v_btye)
ENDIF
IF ( ASSOCIATED( grid%told ) ) THEN 
  DEALLOCATE(grid%told,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%told. ')
 endif
  NULLIFY(grid%told)
ENDIF
IF ( ASSOCIATED( grid%uold ) ) THEN 
  DEALLOCATE(grid%uold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%uold. ')
 endif
  NULLIFY(grid%uold)
ENDIF
IF ( ASSOCIATED( grid%vold ) ) THEN 
  DEALLOCATE(grid%vold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vold. ')
 endif
  NULLIFY(grid%vold)
ENDIF
IF ( ASSOCIATED( grid%hcoeff ) ) THEN 
  DEALLOCATE(grid%hcoeff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hcoeff. ')
 endif
  NULLIFY(grid%hcoeff)
ENDIF
IF ( ASSOCIATED( grid%dfi_pd ) ) THEN 
  DEALLOCATE(grid%dfi_pd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_pd. ')
 endif
  NULLIFY(grid%dfi_pd)
ENDIF
IF ( ASSOCIATED( grid%dfi_pint ) ) THEN 
  DEALLOCATE(grid%dfi_pint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_pint. ')
 endif
  NULLIFY(grid%dfi_pint)
ENDIF
IF ( ASSOCIATED( grid%dfi_dwdt ) ) THEN 
  DEALLOCATE(grid%dfi_dwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_dwdt. ')
 endif
  NULLIFY(grid%dfi_dwdt)
ENDIF
IF ( ASSOCIATED( grid%dfi_t ) ) THEN 
  DEALLOCATE(grid%dfi_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_t. ')
 endif
  NULLIFY(grid%dfi_t)
ENDIF
IF ( ASSOCIATED( grid%dfi_q ) ) THEN 
  DEALLOCATE(grid%dfi_q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_q. ')
 endif
  NULLIFY(grid%dfi_q)
ENDIF
IF ( ASSOCIATED( grid%dfi_u ) ) THEN 
  DEALLOCATE(grid%dfi_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_u. ')
 endif
  NULLIFY(grid%dfi_u)
ENDIF
IF ( ASSOCIATED( grid%dfi_v ) ) THEN 
  DEALLOCATE(grid%dfi_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_v. ')
 endif
  NULLIFY(grid%dfi_v)
ENDIF
IF ( ASSOCIATED( grid%dfi_q2 ) ) THEN 
  DEALLOCATE(grid%dfi_q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_q2. ')
 endif
  NULLIFY(grid%dfi_q2)
ENDIF
IF ( ASSOCIATED( grid%dfi_cwm ) ) THEN 
  DEALLOCATE(grid%dfi_cwm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_cwm. ')
 endif
  NULLIFY(grid%dfi_cwm)
ENDIF
IF ( ASSOCIATED( grid%dfi_rrw ) ) THEN 
  DEALLOCATE(grid%dfi_rrw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_rrw. ')
 endif
  NULLIFY(grid%dfi_rrw)
ENDIF
IF ( ASSOCIATED( grid%dfi_stc ) ) THEN 
  DEALLOCATE(grid%dfi_stc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_stc. ')
 endif
  NULLIFY(grid%dfi_stc)
ENDIF
IF ( ASSOCIATED( grid%dfi_smc ) ) THEN 
  DEALLOCATE(grid%dfi_smc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_smc. ')
 endif
  NULLIFY(grid%dfi_smc)
ENDIF
IF ( ASSOCIATED( grid%dfi_sh2o ) ) THEN 
  DEALLOCATE(grid%dfi_sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_sh2o. ')
 endif
  NULLIFY(grid%dfi_sh2o)
ENDIF
IF ( ASSOCIATED( grid%dfi_snow ) ) THEN 
  DEALLOCATE(grid%dfi_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_snow. ')
 endif
  NULLIFY(grid%dfi_snow)
ENDIF
IF ( ASSOCIATED( grid%dfi_snowh ) ) THEN 
  DEALLOCATE(grid%dfi_snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_snowh. ')
 endif
  NULLIFY(grid%dfi_snowh)
ENDIF
IF ( ASSOCIATED( grid%dfi_canwat ) ) THEN 
  DEALLOCATE(grid%dfi_canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_canwat. ')
 endif
  NULLIFY(grid%dfi_canwat)
ENDIF
IF ( ASSOCIATED( grid%dfi_nmm_tsk ) ) THEN 
  DEALLOCATE(grid%dfi_nmm_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_nmm_tsk. ')
 endif
  NULLIFY(grid%dfi_nmm_tsk)
ENDIF
IF ( ASSOCIATED( grid%dfi_snowc ) ) THEN 
  DEALLOCATE(grid%dfi_snowc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_snowc. ')
 endif
  NULLIFY(grid%dfi_snowc)
ENDIF
IF ( ASSOCIATED( grid%dx_nmm ) ) THEN 
  DEALLOCATE(grid%dx_nmm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dx_nmm. ')
 endif
  NULLIFY(grid%dx_nmm)
ENDIF
IF ( ASSOCIATED( grid%wpdar ) ) THEN 
  DEALLOCATE(grid%wpdar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%wpdar. ')
 endif
  NULLIFY(grid%wpdar)
ENDIF
IF ( ASSOCIATED( grid%cpgfu ) ) THEN 
  DEALLOCATE(grid%cpgfu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cpgfu. ')
 endif
  NULLIFY(grid%cpgfu)
ENDIF
IF ( ASSOCIATED( grid%curv ) ) THEN 
  DEALLOCATE(grid%curv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%curv. ')
 endif
  NULLIFY(grid%curv)
ENDIF
IF ( ASSOCIATED( grid%fcp ) ) THEN 
  DEALLOCATE(grid%fcp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fcp. ')
 endif
  NULLIFY(grid%fcp)
ENDIF
IF ( ASSOCIATED( grid%fdiv ) ) THEN 
  DEALLOCATE(grid%fdiv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fdiv. ')
 endif
  NULLIFY(grid%fdiv)
ENDIF
IF ( ASSOCIATED( grid%f ) ) THEN 
  DEALLOCATE(grid%f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f. ')
 endif
  NULLIFY(grid%f)
ENDIF
IF ( ASSOCIATED( grid%fad ) ) THEN 
  DEALLOCATE(grid%fad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fad. ')
 endif
  NULLIFY(grid%fad)
ENDIF
IF ( ASSOCIATED( grid%ddmpu ) ) THEN 
  DEALLOCATE(grid%ddmpu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ddmpu. ')
 endif
  NULLIFY(grid%ddmpu)
ENDIF
IF ( ASSOCIATED( grid%ddmpv ) ) THEN 
  DEALLOCATE(grid%ddmpv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ddmpv. ')
 endif
  NULLIFY(grid%ddmpv)
ENDIF
IF ( ASSOCIATED( grid%deta ) ) THEN 
  DEALLOCATE(grid%deta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%deta. ')
 endif
  NULLIFY(grid%deta)
ENDIF
IF ( ASSOCIATED( grid%rdeta ) ) THEN 
  DEALLOCATE(grid%rdeta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rdeta. ')
 endif
  NULLIFY(grid%rdeta)
ENDIF
IF ( ASSOCIATED( grid%aeta ) ) THEN 
  DEALLOCATE(grid%aeta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%aeta. ')
 endif
  NULLIFY(grid%aeta)
ENDIF
IF ( ASSOCIATED( grid%f4q2 ) ) THEN 
  DEALLOCATE(grid%f4q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f4q2. ')
 endif
  NULLIFY(grid%f4q2)
ENDIF
IF ( ASSOCIATED( grid%etax ) ) THEN 
  DEALLOCATE(grid%etax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%etax. ')
 endif
  NULLIFY(grid%etax)
ENDIF
IF ( ASSOCIATED( grid%dfl ) ) THEN 
  DEALLOCATE(grid%dfl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfl. ')
 endif
  NULLIFY(grid%dfl)
ENDIF
IF ( ASSOCIATED( grid%deta1 ) ) THEN 
  DEALLOCATE(grid%deta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%deta1. ')
 endif
  NULLIFY(grid%deta1)
ENDIF
IF ( ASSOCIATED( grid%aeta1 ) ) THEN 
  DEALLOCATE(grid%aeta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%aeta1. ')
 endif
  NULLIFY(grid%aeta1)
ENDIF
IF ( ASSOCIATED( grid%eta1 ) ) THEN 
  DEALLOCATE(grid%eta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%eta1. ')
 endif
  NULLIFY(grid%eta1)
ENDIF
IF ( ASSOCIATED( grid%deta2 ) ) THEN 
  DEALLOCATE(grid%deta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%deta2. ')
 endif
  NULLIFY(grid%deta2)
ENDIF
IF ( ASSOCIATED( grid%aeta2 ) ) THEN 
  DEALLOCATE(grid%aeta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%aeta2. ')
 endif
  NULLIFY(grid%aeta2)
ENDIF
IF ( ASSOCIATED( grid%eta2 ) ) THEN 
  DEALLOCATE(grid%eta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%eta2. ')
 endif
  NULLIFY(grid%eta2)
ENDIF
IF ( ASSOCIATED( grid%em ) ) THEN 
  DEALLOCATE(grid%em,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em. ')
 endif
  NULLIFY(grid%em)
ENDIF
IF ( ASSOCIATED( grid%emt ) ) THEN 
  DEALLOCATE(grid%emt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%emt. ')
 endif
  NULLIFY(grid%emt)
ENDIF
IF ( ASSOCIATED( grid%adt ) ) THEN 
  DEALLOCATE(grid%adt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%adt. ')
 endif
  NULLIFY(grid%adt)
ENDIF
IF ( ASSOCIATED( grid%adu ) ) THEN 
  DEALLOCATE(grid%adu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%adu. ')
 endif
  NULLIFY(grid%adu)
ENDIF
IF ( ASSOCIATED( grid%adv ) ) THEN 
  DEALLOCATE(grid%adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%adv. ')
 endif
  NULLIFY(grid%adv)
ENDIF
IF ( ASSOCIATED( grid%em_loc ) ) THEN 
  DEALLOCATE(grid%em_loc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%em_loc. ')
 endif
  NULLIFY(grid%em_loc)
ENDIF
IF ( ASSOCIATED( grid%emt_loc ) ) THEN 
  DEALLOCATE(grid%emt_loc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%emt_loc. ')
 endif
  NULLIFY(grid%emt_loc)
ENDIF
IF ( ASSOCIATED( grid%pdsl ) ) THEN 
  DEALLOCATE(grid%pdsl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pdsl. ')
 endif
  NULLIFY(grid%pdsl)
ENDIF
IF ( ASSOCIATED( grid%pdslo ) ) THEN 
  DEALLOCATE(grid%pdslo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pdslo. ')
 endif
  NULLIFY(grid%pdslo)
ENDIF
IF ( ASSOCIATED( grid%psdt ) ) THEN 
  DEALLOCATE(grid%psdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%psdt. ')
 endif
  NULLIFY(grid%psdt)
ENDIF
IF ( ASSOCIATED( grid%div ) ) THEN 
  DEALLOCATE(grid%div,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%div. ')
 endif
  NULLIFY(grid%div)
ENDIF
IF ( ASSOCIATED( grid%few ) ) THEN 
  DEALLOCATE(grid%few,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%few. ')
 endif
  NULLIFY(grid%few)
ENDIF
IF ( ASSOCIATED( grid%fne ) ) THEN 
  DEALLOCATE(grid%fne,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fne. ')
 endif
  NULLIFY(grid%fne)
ENDIF
IF ( ASSOCIATED( grid%fns ) ) THEN 
  DEALLOCATE(grid%fns,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fns. ')
 endif
  NULLIFY(grid%fns)
ENDIF
IF ( ASSOCIATED( grid%fse ) ) THEN 
  DEALLOCATE(grid%fse,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%fse. ')
 endif
  NULLIFY(grid%fse)
ENDIF
IF ( ASSOCIATED( grid%omgalf ) ) THEN 
  DEALLOCATE(grid%omgalf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%omgalf. ')
 endif
  NULLIFY(grid%omgalf)
ENDIF
IF ( ASSOCIATED( grid%petdt ) ) THEN 
  DEALLOCATE(grid%petdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%petdt. ')
 endif
  NULLIFY(grid%petdt)
ENDIF
IF ( ASSOCIATED( grid%rtop ) ) THEN 
  DEALLOCATE(grid%rtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rtop. ')
 endif
  NULLIFY(grid%rtop)
ENDIF
IF ( ASSOCIATED( grid%pblh ) ) THEN 
  DEALLOCATE(grid%pblh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pblh. ')
 endif
  NULLIFY(grid%pblh)
ENDIF
IF ( ASSOCIATED( grid%lpbl ) ) THEN 
  DEALLOCATE(grid%lpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lpbl. ')
 endif
  NULLIFY(grid%lpbl)
ENDIF
IF ( ASSOCIATED( grid%mixht ) ) THEN 
  DEALLOCATE(grid%mixht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mixht. ')
 endif
  NULLIFY(grid%mixht)
ENDIF
IF ( ASSOCIATED( grid%ustar ) ) THEN 
  DEALLOCATE(grid%ustar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ustar. ')
 endif
  NULLIFY(grid%ustar)
ENDIF
IF ( ASSOCIATED( grid%z0 ) ) THEN 
  DEALLOCATE(grid%z0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%z0. ')
 endif
  NULLIFY(grid%z0)
ENDIF
IF ( ASSOCIATED( grid%z0base ) ) THEN 
  DEALLOCATE(grid%z0base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%z0base. ')
 endif
  NULLIFY(grid%z0base)
ENDIF
IF ( ASSOCIATED( grid%ths ) ) THEN 
  DEALLOCATE(grid%ths,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ths. ')
 endif
  NULLIFY(grid%ths)
ENDIF
IF ( ASSOCIATED( grid%mavail ) ) THEN 
  DEALLOCATE(grid%mavail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mavail. ')
 endif
  NULLIFY(grid%mavail)
ENDIF
IF ( ASSOCIATED( grid%qsh ) ) THEN 
  DEALLOCATE(grid%qsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qsh. ')
 endif
  NULLIFY(grid%qsh)
ENDIF
IF ( ASSOCIATED( grid%twbs ) ) THEN 
  DEALLOCATE(grid%twbs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%twbs. ')
 endif
  NULLIFY(grid%twbs)
ENDIF
IF ( ASSOCIATED( grid%qwbs ) ) THEN 
  DEALLOCATE(grid%qwbs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qwbs. ')
 endif
  NULLIFY(grid%qwbs)
ENDIF
IF ( ASSOCIATED( grid%taux ) ) THEN 
  DEALLOCATE(grid%taux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%taux. ')
 endif
  NULLIFY(grid%taux)
ENDIF
IF ( ASSOCIATED( grid%tauy ) ) THEN 
  DEALLOCATE(grid%tauy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tauy. ')
 endif
  NULLIFY(grid%tauy)
ENDIF
IF ( ASSOCIATED( grid%prec ) ) THEN 
  DEALLOCATE(grid%prec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%prec. ')
 endif
  NULLIFY(grid%prec)
ENDIF
IF ( ASSOCIATED( grid%aprec ) ) THEN 
  DEALLOCATE(grid%aprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%aprec. ')
 endif
  NULLIFY(grid%aprec)
ENDIF
IF ( ASSOCIATED( grid%acprec ) ) THEN 
  DEALLOCATE(grid%acprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acprec. ')
 endif
  NULLIFY(grid%acprec)
ENDIF
IF ( ASSOCIATED( grid%cuprec ) ) THEN 
  DEALLOCATE(grid%cuprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cuprec. ')
 endif
  NULLIFY(grid%cuprec)
ENDIF
IF ( ASSOCIATED( grid%lspa ) ) THEN 
  DEALLOCATE(grid%lspa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lspa. ')
 endif
  NULLIFY(grid%lspa)
ENDIF
IF ( ASSOCIATED( grid%ddata ) ) THEN 
  DEALLOCATE(grid%ddata,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ddata. ')
 endif
  NULLIFY(grid%ddata)
ENDIF
IF ( ASSOCIATED( grid%accliq ) ) THEN 
  DEALLOCATE(grid%accliq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%accliq. ')
 endif
  NULLIFY(grid%accliq)
ENDIF
IF ( ASSOCIATED( grid%sno ) ) THEN 
  DEALLOCATE(grid%sno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sno. ')
 endif
  NULLIFY(grid%sno)
ENDIF
IF ( ASSOCIATED( grid%si ) ) THEN 
  DEALLOCATE(grid%si,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%si. ')
 endif
  NULLIFY(grid%si)
ENDIF
IF ( ASSOCIATED( grid%cldefi ) ) THEN 
  DEALLOCATE(grid%cldefi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cldefi. ')
 endif
  NULLIFY(grid%cldefi)
ENDIF
IF ( ASSOCIATED( grid%deep ) ) THEN 
  DEALLOCATE(grid%deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%deep. ')
 endif
  NULLIFY(grid%deep)
ENDIF
IF ( ASSOCIATED( grid%rf ) ) THEN 
  DEALLOCATE(grid%rf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rf. ')
 endif
  NULLIFY(grid%rf)
ENDIF
IF ( ASSOCIATED( grid%th10 ) ) THEN 
  DEALLOCATE(grid%th10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%th10. ')
 endif
  NULLIFY(grid%th10)
ENDIF
IF ( ASSOCIATED( grid%q10 ) ) THEN 
  DEALLOCATE(grid%q10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q10. ')
 endif
  NULLIFY(grid%q10)
ENDIF
IF ( ASSOCIATED( grid%pshltr ) ) THEN 
  DEALLOCATE(grid%pshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pshltr. ')
 endif
  NULLIFY(grid%pshltr)
ENDIF
IF ( ASSOCIATED( grid%tshltr ) ) THEN 
  DEALLOCATE(grid%tshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tshltr. ')
 endif
  NULLIFY(grid%tshltr)
ENDIF
IF ( ASSOCIATED( grid%qshltr ) ) THEN 
  DEALLOCATE(grid%qshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qshltr. ')
 endif
  NULLIFY(grid%qshltr)
ENDIF
IF ( ASSOCIATED( grid%q2 ) ) THEN 
  DEALLOCATE(grid%q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2. ')
 endif
  NULLIFY(grid%q2)
ENDIF
IF ( ASSOCIATED( grid%q2_bxs ) ) THEN 
  DEALLOCATE(grid%q2_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2_bxs. ')
 endif
  NULLIFY(grid%q2_bxs)
ENDIF
IF ( ASSOCIATED( grid%q2_bxe ) ) THEN 
  DEALLOCATE(grid%q2_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2_bxe. ')
 endif
  NULLIFY(grid%q2_bxe)
ENDIF
IF ( ASSOCIATED( grid%q2_bys ) ) THEN 
  DEALLOCATE(grid%q2_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2_bys. ')
 endif
  NULLIFY(grid%q2_bys)
ENDIF
IF ( ASSOCIATED( grid%q2_bye ) ) THEN 
  DEALLOCATE(grid%q2_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2_bye. ')
 endif
  NULLIFY(grid%q2_bye)
ENDIF
IF ( ASSOCIATED( grid%q2_btxs ) ) THEN 
  DEALLOCATE(grid%q2_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2_btxs. ')
 endif
  NULLIFY(grid%q2_btxs)
ENDIF
IF ( ASSOCIATED( grid%q2_btxe ) ) THEN 
  DEALLOCATE(grid%q2_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2_btxe. ')
 endif
  NULLIFY(grid%q2_btxe)
ENDIF
IF ( ASSOCIATED( grid%q2_btys ) ) THEN 
  DEALLOCATE(grid%q2_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2_btys. ')
 endif
  NULLIFY(grid%q2_btys)
ENDIF
IF ( ASSOCIATED( grid%q2_btye ) ) THEN 
  DEALLOCATE(grid%q2_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%q2_btye. ')
 endif
  NULLIFY(grid%q2_btye)
ENDIF
IF ( ASSOCIATED( grid%t_adj ) ) THEN 
  DEALLOCATE(grid%t_adj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_adj. ')
 endif
  NULLIFY(grid%t_adj)
ENDIF
IF ( ASSOCIATED( grid%t_old ) ) THEN 
  DEALLOCATE(grid%t_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t_old. ')
 endif
  NULLIFY(grid%t_old)
ENDIF
IF ( ASSOCIATED( grid%zero_3d ) ) THEN 
  DEALLOCATE(grid%zero_3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%zero_3d. ')
 endif
  NULLIFY(grid%zero_3d)
ENDIF
IF ( ASSOCIATED( grid%w0avg ) ) THEN 
  DEALLOCATE(grid%w0avg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%w0avg. ')
 endif
  NULLIFY(grid%w0avg)
ENDIF
IF ( ASSOCIATED( grid%akhs_out ) ) THEN 
  DEALLOCATE(grid%akhs_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%akhs_out. ')
 endif
  NULLIFY(grid%akhs_out)
ENDIF
IF ( ASSOCIATED( grid%akms_out ) ) THEN 
  DEALLOCATE(grid%akms_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%akms_out. ')
 endif
  NULLIFY(grid%akms_out)
ENDIF
IF ( ASSOCIATED( grid%albase ) ) THEN 
  DEALLOCATE(grid%albase,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albase. ')
 endif
  NULLIFY(grid%albase)
ENDIF
IF ( ASSOCIATED( grid%albedo ) ) THEN 
  DEALLOCATE(grid%albedo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albedo. ')
 endif
  NULLIFY(grid%albedo)
ENDIF
IF ( ASSOCIATED( grid%cnvbot ) ) THEN 
  DEALLOCATE(grid%cnvbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cnvbot. ')
 endif
  NULLIFY(grid%cnvbot)
ENDIF
IF ( ASSOCIATED( grid%cnvtop ) ) THEN 
  DEALLOCATE(grid%cnvtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cnvtop. ')
 endif
  NULLIFY(grid%cnvtop)
ENDIF
IF ( ASSOCIATED( grid%czen ) ) THEN 
  DEALLOCATE(grid%czen,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%czen. ')
 endif
  NULLIFY(grid%czen)
ENDIF
IF ( ASSOCIATED( grid%czmean ) ) THEN 
  DEALLOCATE(grid%czmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%czmean. ')
 endif
  NULLIFY(grid%czmean)
ENDIF
IF ( ASSOCIATED( grid%embck ) ) THEN 
  DEALLOCATE(grid%embck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%embck. ')
 endif
  NULLIFY(grid%embck)
ENDIF
IF ( ASSOCIATED( grid%epsr ) ) THEN 
  DEALLOCATE(grid%epsr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%epsr. ')
 endif
  NULLIFY(grid%epsr)
ENDIF
IF ( ASSOCIATED( grid%gffc ) ) THEN 
  DEALLOCATE(grid%gffc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%gffc. ')
 endif
  NULLIFY(grid%gffc)
ENDIF
IF ( ASSOCIATED( grid%glat ) ) THEN 
  DEALLOCATE(grid%glat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%glat. ')
 endif
  NULLIFY(grid%glat)
ENDIF
IF ( ASSOCIATED( grid%glon ) ) THEN 
  DEALLOCATE(grid%glon,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%glon. ')
 endif
  NULLIFY(grid%glon)
ENDIF
IF ( ASSOCIATED( grid%nmm_tsk ) ) THEN 
  DEALLOCATE(grid%nmm_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tsk. ')
 endif
  NULLIFY(grid%nmm_tsk)
ENDIF
IF ( ASSOCIATED( grid%hdac ) ) THEN 
  DEALLOCATE(grid%hdac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hdac. ')
 endif
  NULLIFY(grid%hdac)
ENDIF
IF ( ASSOCIATED( grid%hdacv ) ) THEN 
  DEALLOCATE(grid%hdacv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hdacv. ')
 endif
  NULLIFY(grid%hdacv)
ENDIF
IF ( ASSOCIATED( grid%mxsnal ) ) THEN 
  DEALLOCATE(grid%mxsnal,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mxsnal. ')
 endif
  NULLIFY(grid%mxsnal)
ENDIF
IF ( ASSOCIATED( grid%radin ) ) THEN 
  DEALLOCATE(grid%radin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%radin. ')
 endif
  NULLIFY(grid%radin)
ENDIF
IF ( ASSOCIATED( grid%radot ) ) THEN 
  DEALLOCATE(grid%radot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%radot. ')
 endif
  NULLIFY(grid%radot)
ENDIF
IF ( ASSOCIATED( grid%sigt4 ) ) THEN 
  DEALLOCATE(grid%sigt4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sigt4. ')
 endif
  NULLIFY(grid%sigt4)
ENDIF
IF ( ASSOCIATED( grid%tg ) ) THEN 
  DEALLOCATE(grid%tg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tg. ')
 endif
  NULLIFY(grid%tg)
ENDIF
IF ( ASSOCIATED( grid%dfrlg ) ) THEN 
  DEALLOCATE(grid%dfrlg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfrlg. ')
 endif
  NULLIFY(grid%dfrlg)
ENDIF
IF ( ASSOCIATED( grid%lvl ) ) THEN 
  DEALLOCATE(grid%lvl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lvl. ')
 endif
  NULLIFY(grid%lvl)
ENDIF
IF ( ASSOCIATED( grid%cwm ) ) THEN 
  DEALLOCATE(grid%cwm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cwm. ')
 endif
  NULLIFY(grid%cwm)
ENDIF
IF ( ASSOCIATED( grid%cwm_bxs ) ) THEN 
  DEALLOCATE(grid%cwm_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cwm_bxs. ')
 endif
  NULLIFY(grid%cwm_bxs)
ENDIF
IF ( ASSOCIATED( grid%cwm_bxe ) ) THEN 
  DEALLOCATE(grid%cwm_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cwm_bxe. ')
 endif
  NULLIFY(grid%cwm_bxe)
ENDIF
IF ( ASSOCIATED( grid%cwm_bys ) ) THEN 
  DEALLOCATE(grid%cwm_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cwm_bys. ')
 endif
  NULLIFY(grid%cwm_bys)
ENDIF
IF ( ASSOCIATED( grid%cwm_bye ) ) THEN 
  DEALLOCATE(grid%cwm_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cwm_bye. ')
 endif
  NULLIFY(grid%cwm_bye)
ENDIF
IF ( ASSOCIATED( grid%cwm_btxs ) ) THEN 
  DEALLOCATE(grid%cwm_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cwm_btxs. ')
 endif
  NULLIFY(grid%cwm_btxs)
ENDIF
IF ( ASSOCIATED( grid%cwm_btxe ) ) THEN 
  DEALLOCATE(grid%cwm_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cwm_btxe. ')
 endif
  NULLIFY(grid%cwm_btxe)
ENDIF
IF ( ASSOCIATED( grid%cwm_btys ) ) THEN 
  DEALLOCATE(grid%cwm_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cwm_btys. ')
 endif
  NULLIFY(grid%cwm_btys)
ENDIF
IF ( ASSOCIATED( grid%cwm_btye ) ) THEN 
  DEALLOCATE(grid%cwm_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cwm_btye. ')
 endif
  NULLIFY(grid%cwm_btye)
ENDIF
IF ( ASSOCIATED( grid%rrw ) ) THEN 
  DEALLOCATE(grid%rrw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rrw. ')
 endif
  NULLIFY(grid%rrw)
ENDIF
IF ( ASSOCIATED( grid%rrw_bxs ) ) THEN 
  DEALLOCATE(grid%rrw_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rrw_bxs. ')
 endif
  NULLIFY(grid%rrw_bxs)
ENDIF
IF ( ASSOCIATED( grid%rrw_bxe ) ) THEN 
  DEALLOCATE(grid%rrw_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rrw_bxe. ')
 endif
  NULLIFY(grid%rrw_bxe)
ENDIF
IF ( ASSOCIATED( grid%rrw_bys ) ) THEN 
  DEALLOCATE(grid%rrw_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rrw_bys. ')
 endif
  NULLIFY(grid%rrw_bys)
ENDIF
IF ( ASSOCIATED( grid%rrw_bye ) ) THEN 
  DEALLOCATE(grid%rrw_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rrw_bye. ')
 endif
  NULLIFY(grid%rrw_bye)
ENDIF
IF ( ASSOCIATED( grid%rrw_btxs ) ) THEN 
  DEALLOCATE(grid%rrw_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rrw_btxs. ')
 endif
  NULLIFY(grid%rrw_btxs)
ENDIF
IF ( ASSOCIATED( grid%rrw_btxe ) ) THEN 
  DEALLOCATE(grid%rrw_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rrw_btxe. ')
 endif
  NULLIFY(grid%rrw_btxe)
ENDIF
IF ( ASSOCIATED( grid%rrw_btys ) ) THEN 
  DEALLOCATE(grid%rrw_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rrw_btys. ')
 endif
  NULLIFY(grid%rrw_btys)
ENDIF
IF ( ASSOCIATED( grid%rrw_btye ) ) THEN 
  DEALLOCATE(grid%rrw_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rrw_btye. ')
 endif
  NULLIFY(grid%rrw_btye)
ENDIF
IF ( ASSOCIATED( grid%f_ice ) ) THEN 
  DEALLOCATE(grid%f_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_ice. ')
 endif
  NULLIFY(grid%f_ice)
ENDIF
IF ( ASSOCIATED( grid%f_rain ) ) THEN 
  DEALLOCATE(grid%f_rain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_rain. ')
 endif
  NULLIFY(grid%f_rain)
ENDIF
IF ( ASSOCIATED( grid%f_rimef ) ) THEN 
  DEALLOCATE(grid%f_rimef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_rimef. ')
 endif
  NULLIFY(grid%f_rimef)
ENDIF
IF ( ASSOCIATED( grid%cldfra ) ) THEN 
  DEALLOCATE(grid%cldfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cldfra. ')
 endif
  NULLIFY(grid%cldfra)
ENDIF
IF ( ASSOCIATED( grid%sr ) ) THEN 
  DEALLOCATE(grid%sr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sr. ')
 endif
  NULLIFY(grid%sr)
ENDIF
IF ( ASSOCIATED( grid%cfrach ) ) THEN 
  DEALLOCATE(grid%cfrach,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cfrach. ')
 endif
  NULLIFY(grid%cfrach)
ENDIF
IF ( ASSOCIATED( grid%cfracl ) ) THEN 
  DEALLOCATE(grid%cfracl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cfracl. ')
 endif
  NULLIFY(grid%cfracl)
ENDIF
IF ( ASSOCIATED( grid%cfracm ) ) THEN 
  DEALLOCATE(grid%cfracm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cfracm. ')
 endif
  NULLIFY(grid%cfracm)
ENDIF
IF ( ASSOCIATED( grid%islope ) ) THEN 
  DEALLOCATE(grid%islope,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%islope. ')
 endif
  NULLIFY(grid%islope)
ENDIF
IF ( ASSOCIATED( grid%dzsoil ) ) THEN 
  DEALLOCATE(grid%dzsoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dzsoil. ')
 endif
  NULLIFY(grid%dzsoil)
ENDIF
IF ( ASSOCIATED( grid%rtdpth ) ) THEN 
  DEALLOCATE(grid%rtdpth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rtdpth. ')
 endif
  NULLIFY(grid%rtdpth)
ENDIF
IF ( ASSOCIATED( grid%sldpth ) ) THEN 
  DEALLOCATE(grid%sldpth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sldpth. ')
 endif
  NULLIFY(grid%sldpth)
ENDIF
IF ( ASSOCIATED( grid%cmc ) ) THEN 
  DEALLOCATE(grid%cmc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cmc. ')
 endif
  NULLIFY(grid%cmc)
ENDIF
IF ( ASSOCIATED( grid%grnflx ) ) THEN 
  DEALLOCATE(grid%grnflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%grnflx. ')
 endif
  NULLIFY(grid%grnflx)
ENDIF
IF ( ASSOCIATED( grid%pctsno ) ) THEN 
  DEALLOCATE(grid%pctsno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pctsno. ')
 endif
  NULLIFY(grid%pctsno)
ENDIF
IF ( ASSOCIATED( grid%soiltb ) ) THEN 
  DEALLOCATE(grid%soiltb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soiltb. ')
 endif
  NULLIFY(grid%soiltb)
ENDIF
IF ( ASSOCIATED( grid%vegfrc ) ) THEN 
  DEALLOCATE(grid%vegfrc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vegfrc. ')
 endif
  NULLIFY(grid%vegfrc)
ENDIF
IF ( ASSOCIATED( grid%shdmin ) ) THEN 
  DEALLOCATE(grid%shdmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%shdmin. ')
 endif
  NULLIFY(grid%shdmin)
ENDIF
IF ( ASSOCIATED( grid%shdmax ) ) THEN 
  DEALLOCATE(grid%shdmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%shdmax. ')
 endif
  NULLIFY(grid%shdmax)
ENDIF
IF ( ASSOCIATED( grid%sh2o ) ) THEN 
  DEALLOCATE(grid%sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sh2o. ')
 endif
  NULLIFY(grid%sh2o)
ENDIF
IF ( ASSOCIATED( grid%smc ) ) THEN 
  DEALLOCATE(grid%smc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smc. ')
 endif
  NULLIFY(grid%smc)
ENDIF
IF ( ASSOCIATED( grid%stc ) ) THEN 
  DEALLOCATE(grid%stc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%stc. ')
 endif
  NULLIFY(grid%stc)
ENDIF
IF ( ASSOCIATED( grid%hstdv ) ) THEN 
  DEALLOCATE(grid%hstdv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hstdv. ')
 endif
  NULLIFY(grid%hstdv)
ENDIF
IF ( ASSOCIATED( grid%hcnvx ) ) THEN 
  DEALLOCATE(grid%hcnvx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hcnvx. ')
 endif
  NULLIFY(grid%hcnvx)
ENDIF
IF ( ASSOCIATED( grid%hasyw ) ) THEN 
  DEALLOCATE(grid%hasyw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hasyw. ')
 endif
  NULLIFY(grid%hasyw)
ENDIF
IF ( ASSOCIATED( grid%hasys ) ) THEN 
  DEALLOCATE(grid%hasys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hasys. ')
 endif
  NULLIFY(grid%hasys)
ENDIF
IF ( ASSOCIATED( grid%hasysw ) ) THEN 
  DEALLOCATE(grid%hasysw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hasysw. ')
 endif
  NULLIFY(grid%hasysw)
ENDIF
IF ( ASSOCIATED( grid%hasynw ) ) THEN 
  DEALLOCATE(grid%hasynw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hasynw. ')
 endif
  NULLIFY(grid%hasynw)
ENDIF
IF ( ASSOCIATED( grid%hlenw ) ) THEN 
  DEALLOCATE(grid%hlenw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hlenw. ')
 endif
  NULLIFY(grid%hlenw)
ENDIF
IF ( ASSOCIATED( grid%hlens ) ) THEN 
  DEALLOCATE(grid%hlens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hlens. ')
 endif
  NULLIFY(grid%hlens)
ENDIF
IF ( ASSOCIATED( grid%hlensw ) ) THEN 
  DEALLOCATE(grid%hlensw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hlensw. ')
 endif
  NULLIFY(grid%hlensw)
ENDIF
IF ( ASSOCIATED( grid%hlennw ) ) THEN 
  DEALLOCATE(grid%hlennw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hlennw. ')
 endif
  NULLIFY(grid%hlennw)
ENDIF
IF ( ASSOCIATED( grid%hangl ) ) THEN 
  DEALLOCATE(grid%hangl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hangl. ')
 endif
  NULLIFY(grid%hangl)
ENDIF
IF ( ASSOCIATED( grid%hanis ) ) THEN 
  DEALLOCATE(grid%hanis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hanis. ')
 endif
  NULLIFY(grid%hanis)
ENDIF
IF ( ASSOCIATED( grid%hslop ) ) THEN 
  DEALLOCATE(grid%hslop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hslop. ')
 endif
  NULLIFY(grid%hslop)
ENDIF
IF ( ASSOCIATED( grid%hzmax ) ) THEN 
  DEALLOCATE(grid%hzmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hzmax. ')
 endif
  NULLIFY(grid%hzmax)
ENDIF
IF ( ASSOCIATED( grid%crot ) ) THEN 
  DEALLOCATE(grid%crot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%crot. ')
 endif
  NULLIFY(grid%crot)
ENDIF
IF ( ASSOCIATED( grid%srot ) ) THEN 
  DEALLOCATE(grid%srot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%srot. ')
 endif
  NULLIFY(grid%srot)
ENDIF
IF ( ASSOCIATED( grid%ugwdsfc ) ) THEN 
  DEALLOCATE(grid%ugwdsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ugwdsfc. ')
 endif
  NULLIFY(grid%ugwdsfc)
ENDIF
IF ( ASSOCIATED( grid%vgwdsfc ) ) THEN 
  DEALLOCATE(grid%vgwdsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vgwdsfc. ')
 endif
  NULLIFY(grid%vgwdsfc)
ENDIF
IF ( ASSOCIATED( grid%dwdtmn ) ) THEN 
  DEALLOCATE(grid%dwdtmn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dwdtmn. ')
 endif
  NULLIFY(grid%dwdtmn)
ENDIF
IF ( ASSOCIATED( grid%dwdtmx ) ) THEN 
  DEALLOCATE(grid%dwdtmx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dwdtmx. ')
 endif
  NULLIFY(grid%dwdtmx)
ENDIF
IF ( ASSOCIATED( grid%dwdt ) ) THEN 
  DEALLOCATE(grid%dwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dwdt. ')
 endif
  NULLIFY(grid%dwdt)
ENDIF
IF ( ASSOCIATED( grid%pdwdt ) ) THEN 
  DEALLOCATE(grid%pdwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pdwdt. ')
 endif
  NULLIFY(grid%pdwdt)
ENDIF
IF ( ASSOCIATED( grid%pint ) ) THEN 
  DEALLOCATE(grid%pint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pint. ')
 endif
  NULLIFY(grid%pint)
ENDIF
IF ( ASSOCIATED( grid%w ) ) THEN 
  DEALLOCATE(grid%w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%w. ')
 endif
  NULLIFY(grid%w)
ENDIF
IF ( ASSOCIATED( grid%z ) ) THEN 
  DEALLOCATE(grid%z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%z. ')
 endif
  NULLIFY(grid%z)
ENDIF
IF ( ASSOCIATED( grid%acfrcv ) ) THEN 
  DEALLOCATE(grid%acfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acfrcv. ')
 endif
  NULLIFY(grid%acfrcv)
ENDIF
IF ( ASSOCIATED( grid%acfrst ) ) THEN 
  DEALLOCATE(grid%acfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acfrst. ')
 endif
  NULLIFY(grid%acfrst)
ENDIF
IF ( ASSOCIATED( grid%ssroff ) ) THEN 
  DEALLOCATE(grid%ssroff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ssroff. ')
 endif
  NULLIFY(grid%ssroff)
ENDIF
IF ( ASSOCIATED( grid%bgroff ) ) THEN 
  DEALLOCATE(grid%bgroff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%bgroff. ')
 endif
  NULLIFY(grid%bgroff)
ENDIF
IF ( ASSOCIATED( grid%rlwin ) ) THEN 
  DEALLOCATE(grid%rlwin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rlwin. ')
 endif
  NULLIFY(grid%rlwin)
ENDIF
IF ( ASSOCIATED( grid%rlwout ) ) THEN 
  DEALLOCATE(grid%rlwout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rlwout. ')
 endif
  NULLIFY(grid%rlwout)
ENDIF
IF ( ASSOCIATED( grid%rlwtoa ) ) THEN 
  DEALLOCATE(grid%rlwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rlwtoa. ')
 endif
  NULLIFY(grid%rlwtoa)
ENDIF
IF ( ASSOCIATED( grid%alwin ) ) THEN 
  DEALLOCATE(grid%alwin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%alwin. ')
 endif
  NULLIFY(grid%alwin)
ENDIF
IF ( ASSOCIATED( grid%alwout ) ) THEN 
  DEALLOCATE(grid%alwout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%alwout. ')
 endif
  NULLIFY(grid%alwout)
ENDIF
IF ( ASSOCIATED( grid%alwtoa ) ) THEN 
  DEALLOCATE(grid%alwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%alwtoa. ')
 endif
  NULLIFY(grid%alwtoa)
ENDIF
IF ( ASSOCIATED( grid%rswin ) ) THEN 
  DEALLOCATE(grid%rswin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rswin. ')
 endif
  NULLIFY(grid%rswin)
ENDIF
IF ( ASSOCIATED( grid%rswinc ) ) THEN 
  DEALLOCATE(grid%rswinc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rswinc. ')
 endif
  NULLIFY(grid%rswinc)
ENDIF
IF ( ASSOCIATED( grid%rswout ) ) THEN 
  DEALLOCATE(grid%rswout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rswout. ')
 endif
  NULLIFY(grid%rswout)
ENDIF
IF ( ASSOCIATED( grid%rswtoa ) ) THEN 
  DEALLOCATE(grid%rswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rswtoa. ')
 endif
  NULLIFY(grid%rswtoa)
ENDIF
IF ( ASSOCIATED( grid%aswin ) ) THEN 
  DEALLOCATE(grid%aswin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%aswin. ')
 endif
  NULLIFY(grid%aswin)
ENDIF
IF ( ASSOCIATED( grid%aswout ) ) THEN 
  DEALLOCATE(grid%aswout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%aswout. ')
 endif
  NULLIFY(grid%aswout)
ENDIF
IF ( ASSOCIATED( grid%aswtoa ) ) THEN 
  DEALLOCATE(grid%aswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%aswtoa. ')
 endif
  NULLIFY(grid%aswtoa)
ENDIF
IF ( ASSOCIATED( grid%sfcshx ) ) THEN 
  DEALLOCATE(grid%sfcshx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcshx. ')
 endif
  NULLIFY(grid%sfcshx)
ENDIF
IF ( ASSOCIATED( grid%sfclhx ) ) THEN 
  DEALLOCATE(grid%sfclhx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfclhx. ')
 endif
  NULLIFY(grid%sfclhx)
ENDIF
IF ( ASSOCIATED( grid%subshx ) ) THEN 
  DEALLOCATE(grid%subshx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%subshx. ')
 endif
  NULLIFY(grid%subshx)
ENDIF
IF ( ASSOCIATED( grid%snopcx ) ) THEN 
  DEALLOCATE(grid%snopcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snopcx. ')
 endif
  NULLIFY(grid%snopcx)
ENDIF
IF ( ASSOCIATED( grid%sfcuvx ) ) THEN 
  DEALLOCATE(grid%sfcuvx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcuvx. ')
 endif
  NULLIFY(grid%sfcuvx)
ENDIF
IF ( ASSOCIATED( grid%potevp ) ) THEN 
  DEALLOCATE(grid%potevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%potevp. ')
 endif
  NULLIFY(grid%potevp)
ENDIF
IF ( ASSOCIATED( grid%potflx ) ) THEN 
  DEALLOCATE(grid%potflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%potflx. ')
 endif
  NULLIFY(grid%potflx)
ENDIF
IF ( ASSOCIATED( grid%tlmin ) ) THEN 
  DEALLOCATE(grid%tlmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tlmin. ')
 endif
  NULLIFY(grid%tlmin)
ENDIF
IF ( ASSOCIATED( grid%tlmax ) ) THEN 
  DEALLOCATE(grid%tlmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tlmax. ')
 endif
  NULLIFY(grid%tlmax)
ENDIF
IF ( ASSOCIATED( grid%t02_min ) ) THEN 
  DEALLOCATE(grid%t02_min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t02_min. ')
 endif
  NULLIFY(grid%t02_min)
ENDIF
IF ( ASSOCIATED( grid%t02_max ) ) THEN 
  DEALLOCATE(grid%t02_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t02_max. ')
 endif
  NULLIFY(grid%t02_max)
ENDIF
IF ( ASSOCIATED( grid%rh02_min ) ) THEN 
  DEALLOCATE(grid%rh02_min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rh02_min. ')
 endif
  NULLIFY(grid%rh02_min)
ENDIF
IF ( ASSOCIATED( grid%rh02_max ) ) THEN 
  DEALLOCATE(grid%rh02_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rh02_max. ')
 endif
  NULLIFY(grid%rh02_max)
ENDIF
IF ( ASSOCIATED( grid%rlwtt ) ) THEN 
  DEALLOCATE(grid%rlwtt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rlwtt. ')
 endif
  NULLIFY(grid%rlwtt)
ENDIF
IF ( ASSOCIATED( grid%rswtt ) ) THEN 
  DEALLOCATE(grid%rswtt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rswtt. ')
 endif
  NULLIFY(grid%rswtt)
ENDIF
IF ( ASSOCIATED( grid%tcucn ) ) THEN 
  DEALLOCATE(grid%tcucn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tcucn. ')
 endif
  NULLIFY(grid%tcucn)
ENDIF
IF ( ASSOCIATED( grid%train ) ) THEN 
  DEALLOCATE(grid%train,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%train. ')
 endif
  NULLIFY(grid%train)
ENDIF
IF ( ASSOCIATED( grid%ncfrcv ) ) THEN 
  DEALLOCATE(grid%ncfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ncfrcv. ')
 endif
  NULLIFY(grid%ncfrcv)
ENDIF
IF ( ASSOCIATED( grid%ncfrst ) ) THEN 
  DEALLOCATE(grid%ncfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ncfrst. ')
 endif
  NULLIFY(grid%ncfrst)
ENDIF
IF ( ASSOCIATED( grid%max10mw ) ) THEN 
  DEALLOCATE(grid%max10mw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%max10mw. ')
 endif
  NULLIFY(grid%max10mw)
ENDIF
IF ( ASSOCIATED( grid%max10u ) ) THEN 
  DEALLOCATE(grid%max10u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%max10u. ')
 endif
  NULLIFY(grid%max10u)
ENDIF
IF ( ASSOCIATED( grid%max10v ) ) THEN 
  DEALLOCATE(grid%max10v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%max10v. ')
 endif
  NULLIFY(grid%max10v)
ENDIF
IF ( ASSOCIATED( grid%maxupdr ) ) THEN 
  DEALLOCATE(grid%maxupdr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%maxupdr. ')
 endif
  NULLIFY(grid%maxupdr)
ENDIF
IF ( ASSOCIATED( grid%maxdndr ) ) THEN 
  DEALLOCATE(grid%maxdndr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%maxdndr. ')
 endif
  NULLIFY(grid%maxdndr)
ENDIF
IF ( ASSOCIATED( grid%maxhlcy ) ) THEN 
  DEALLOCATE(grid%maxhlcy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%maxhlcy. ')
 endif
  NULLIFY(grid%maxhlcy)
ENDIF
IF ( ASSOCIATED( grid%maxdbz ) ) THEN 
  DEALLOCATE(grid%maxdbz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%maxdbz. ')
 endif
  NULLIFY(grid%maxdbz)
ENDIF
IF ( ASSOCIATED( grid%ihe ) ) THEN 
  DEALLOCATE(grid%ihe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ihe. ')
 endif
  NULLIFY(grid%ihe)
ENDIF
IF ( ASSOCIATED( grid%ihw ) ) THEN 
  DEALLOCATE(grid%ihw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ihw. ')
 endif
  NULLIFY(grid%ihw)
ENDIF
IF ( ASSOCIATED( grid%ive ) ) THEN 
  DEALLOCATE(grid%ive,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ive. ')
 endif
  NULLIFY(grid%ive)
ENDIF
IF ( ASSOCIATED( grid%ivw ) ) THEN 
  DEALLOCATE(grid%ivw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ivw. ')
 endif
  NULLIFY(grid%ivw)
ENDIF
IF ( ASSOCIATED( grid%irad ) ) THEN 
  DEALLOCATE(grid%irad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%irad. ')
 endif
  NULLIFY(grid%irad)
ENDIF
IF ( ASSOCIATED( grid%iheg ) ) THEN 
  DEALLOCATE(grid%iheg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%iheg. ')
 endif
  NULLIFY(grid%iheg)
ENDIF
IF ( ASSOCIATED( grid%ihwg ) ) THEN 
  DEALLOCATE(grid%ihwg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ihwg. ')
 endif
  NULLIFY(grid%ihwg)
ENDIF
IF ( ASSOCIATED( grid%iveg ) ) THEN 
  DEALLOCATE(grid%iveg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%iveg. ')
 endif
  NULLIFY(grid%iveg)
ENDIF
IF ( ASSOCIATED( grid%ivwg ) ) THEN 
  DEALLOCATE(grid%ivwg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ivwg. ')
 endif
  NULLIFY(grid%ivwg)
ENDIF
IF ( ASSOCIATED( grid%iradg ) ) THEN 
  DEALLOCATE(grid%iradg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%iradg. ')
 endif
  NULLIFY(grid%iradg)
ENDIF
IF ( ASSOCIATED( grid%n_iup_h ) ) THEN 
  DEALLOCATE(grid%n_iup_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%n_iup_h. ')
 endif
  NULLIFY(grid%n_iup_h)
ENDIF
IF ( ASSOCIATED( grid%n_iup_v ) ) THEN 
  DEALLOCATE(grid%n_iup_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%n_iup_v. ')
 endif
  NULLIFY(grid%n_iup_v)
ENDIF
IF ( ASSOCIATED( grid%n_iup_adh ) ) THEN 
  DEALLOCATE(grid%n_iup_adh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%n_iup_adh. ')
 endif
  NULLIFY(grid%n_iup_adh)
ENDIF
IF ( ASSOCIATED( grid%n_iup_adv ) ) THEN 
  DEALLOCATE(grid%n_iup_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%n_iup_adv. ')
 endif
  NULLIFY(grid%n_iup_adv)
ENDIF
IF ( ASSOCIATED( grid%iup_h ) ) THEN 
  DEALLOCATE(grid%iup_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%iup_h. ')
 endif
  NULLIFY(grid%iup_h)
ENDIF
IF ( ASSOCIATED( grid%iup_v ) ) THEN 
  DEALLOCATE(grid%iup_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%iup_v. ')
 endif
  NULLIFY(grid%iup_v)
ENDIF
IF ( ASSOCIATED( grid%iup_adh ) ) THEN 
  DEALLOCATE(grid%iup_adh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%iup_adh. ')
 endif
  NULLIFY(grid%iup_adh)
ENDIF
IF ( ASSOCIATED( grid%iup_adv ) ) THEN 
  DEALLOCATE(grid%iup_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%iup_adv. ')
 endif
  NULLIFY(grid%iup_adv)
ENDIF
IF ( ASSOCIATED( grid%imask_nostag ) ) THEN 
  DEALLOCATE(grid%imask_nostag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_nostag. ')
 endif
  NULLIFY(grid%imask_nostag)
ENDIF
IF ( ASSOCIATED( grid%imask_xstag ) ) THEN 
  DEALLOCATE(grid%imask_xstag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_xstag. ')
 endif
  NULLIFY(grid%imask_xstag)
ENDIF
IF ( ASSOCIATED( grid%imask_ystag ) ) THEN 
  DEALLOCATE(grid%imask_ystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_ystag. ')
 endif
  NULLIFY(grid%imask_ystag)
ENDIF
IF ( ASSOCIATED( grid%imask_xystag ) ) THEN 
  DEALLOCATE(grid%imask_xystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_xystag. ')
 endif
  NULLIFY(grid%imask_xystag)
ENDIF
IF ( ASSOCIATED( grid%sm000007 ) ) THEN 
  DEALLOCATE(grid%sm000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm000007. ')
 endif
  NULLIFY(grid%sm000007)
ENDIF
IF ( ASSOCIATED( grid%sm007028 ) ) THEN 
  DEALLOCATE(grid%sm007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm007028. ')
 endif
  NULLIFY(grid%sm007028)
ENDIF
IF ( ASSOCIATED( grid%sm028100 ) ) THEN 
  DEALLOCATE(grid%sm028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm028100. ')
 endif
  NULLIFY(grid%sm028100)
ENDIF
IF ( ASSOCIATED( grid%sm100255 ) ) THEN 
  DEALLOCATE(grid%sm100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm100255. ')
 endif
  NULLIFY(grid%sm100255)
ENDIF
IF ( ASSOCIATED( grid%st000007 ) ) THEN 
  DEALLOCATE(grid%st000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st000007. ')
 endif
  NULLIFY(grid%st000007)
ENDIF
IF ( ASSOCIATED( grid%st007028 ) ) THEN 
  DEALLOCATE(grid%st007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st007028. ')
 endif
  NULLIFY(grid%st007028)
ENDIF
IF ( ASSOCIATED( grid%st028100 ) ) THEN 
  DEALLOCATE(grid%st028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st028100. ')
 endif
  NULLIFY(grid%st028100)
ENDIF
IF ( ASSOCIATED( grid%st100255 ) ) THEN 
  DEALLOCATE(grid%st100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st100255. ')
 endif
  NULLIFY(grid%st100255)
ENDIF
IF ( ASSOCIATED( grid%sm000010 ) ) THEN 
  DEALLOCATE(grid%sm000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm000010. ')
 endif
  NULLIFY(grid%sm000010)
ENDIF
IF ( ASSOCIATED( grid%sm010040 ) ) THEN 
  DEALLOCATE(grid%sm010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm010040. ')
 endif
  NULLIFY(grid%sm010040)
ENDIF
IF ( ASSOCIATED( grid%sm040100 ) ) THEN 
  DEALLOCATE(grid%sm040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm040100. ')
 endif
  NULLIFY(grid%sm040100)
ENDIF
IF ( ASSOCIATED( grid%sm100200 ) ) THEN 
  DEALLOCATE(grid%sm100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm100200. ')
 endif
  NULLIFY(grid%sm100200)
ENDIF
IF ( ASSOCIATED( grid%sm010200 ) ) THEN 
  DEALLOCATE(grid%sm010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm010200. ')
 endif
  NULLIFY(grid%sm010200)
ENDIF
IF ( ASSOCIATED( grid%soilm000 ) ) THEN 
  DEALLOCATE(grid%soilm000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm000. ')
 endif
  NULLIFY(grid%soilm000)
ENDIF
IF ( ASSOCIATED( grid%soilm005 ) ) THEN 
  DEALLOCATE(grid%soilm005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm005. ')
 endif
  NULLIFY(grid%soilm005)
ENDIF
IF ( ASSOCIATED( grid%soilm020 ) ) THEN 
  DEALLOCATE(grid%soilm020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm020. ')
 endif
  NULLIFY(grid%soilm020)
ENDIF
IF ( ASSOCIATED( grid%soilm040 ) ) THEN 
  DEALLOCATE(grid%soilm040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm040. ')
 endif
  NULLIFY(grid%soilm040)
ENDIF
IF ( ASSOCIATED( grid%soilm160 ) ) THEN 
  DEALLOCATE(grid%soilm160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm160. ')
 endif
  NULLIFY(grid%soilm160)
ENDIF
IF ( ASSOCIATED( grid%soilm300 ) ) THEN 
  DEALLOCATE(grid%soilm300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm300. ')
 endif
  NULLIFY(grid%soilm300)
ENDIF
IF ( ASSOCIATED( grid%sw000010 ) ) THEN 
  DEALLOCATE(grid%sw000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw000010. ')
 endif
  NULLIFY(grid%sw000010)
ENDIF
IF ( ASSOCIATED( grid%sw010040 ) ) THEN 
  DEALLOCATE(grid%sw010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw010040. ')
 endif
  NULLIFY(grid%sw010040)
ENDIF
IF ( ASSOCIATED( grid%sw040100 ) ) THEN 
  DEALLOCATE(grid%sw040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw040100. ')
 endif
  NULLIFY(grid%sw040100)
ENDIF
IF ( ASSOCIATED( grid%sw100200 ) ) THEN 
  DEALLOCATE(grid%sw100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw100200. ')
 endif
  NULLIFY(grid%sw100200)
ENDIF
IF ( ASSOCIATED( grid%sw010200 ) ) THEN 
  DEALLOCATE(grid%sw010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw010200. ')
 endif
  NULLIFY(grid%sw010200)
ENDIF
IF ( ASSOCIATED( grid%soilw000 ) ) THEN 
  DEALLOCATE(grid%soilw000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw000. ')
 endif
  NULLIFY(grid%soilw000)
ENDIF
IF ( ASSOCIATED( grid%soilw005 ) ) THEN 
  DEALLOCATE(grid%soilw005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw005. ')
 endif
  NULLIFY(grid%soilw005)
ENDIF
IF ( ASSOCIATED( grid%soilw020 ) ) THEN 
  DEALLOCATE(grid%soilw020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw020. ')
 endif
  NULLIFY(grid%soilw020)
ENDIF
IF ( ASSOCIATED( grid%soilw040 ) ) THEN 
  DEALLOCATE(grid%soilw040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw040. ')
 endif
  NULLIFY(grid%soilw040)
ENDIF
IF ( ASSOCIATED( grid%soilw160 ) ) THEN 
  DEALLOCATE(grid%soilw160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw160. ')
 endif
  NULLIFY(grid%soilw160)
ENDIF
IF ( ASSOCIATED( grid%soilw300 ) ) THEN 
  DEALLOCATE(grid%soilw300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw300. ')
 endif
  NULLIFY(grid%soilw300)
ENDIF
IF ( ASSOCIATED( grid%st000010 ) ) THEN 
  DEALLOCATE(grid%st000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st000010. ')
 endif
  NULLIFY(grid%st000010)
ENDIF
IF ( ASSOCIATED( grid%st010040 ) ) THEN 
  DEALLOCATE(grid%st010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st010040. ')
 endif
  NULLIFY(grid%st010040)
ENDIF
IF ( ASSOCIATED( grid%st040100 ) ) THEN 
  DEALLOCATE(grid%st040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st040100. ')
 endif
  NULLIFY(grid%st040100)
ENDIF
IF ( ASSOCIATED( grid%st100200 ) ) THEN 
  DEALLOCATE(grid%st100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st100200. ')
 endif
  NULLIFY(grid%st100200)
ENDIF
IF ( ASSOCIATED( grid%st010200 ) ) THEN 
  DEALLOCATE(grid%st010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st010200. ')
 endif
  NULLIFY(grid%st010200)
ENDIF
IF ( ASSOCIATED( grid%soilt000 ) ) THEN 
  DEALLOCATE(grid%soilt000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt000. ')
 endif
  NULLIFY(grid%soilt000)
ENDIF
IF ( ASSOCIATED( grid%soilt005 ) ) THEN 
  DEALLOCATE(grid%soilt005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt005. ')
 endif
  NULLIFY(grid%soilt005)
ENDIF
IF ( ASSOCIATED( grid%soilt020 ) ) THEN 
  DEALLOCATE(grid%soilt020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt020. ')
 endif
  NULLIFY(grid%soilt020)
ENDIF
IF ( ASSOCIATED( grid%soilt040 ) ) THEN 
  DEALLOCATE(grid%soilt040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt040. ')
 endif
  NULLIFY(grid%soilt040)
ENDIF
IF ( ASSOCIATED( grid%soilt160 ) ) THEN 
  DEALLOCATE(grid%soilt160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt160. ')
 endif
  NULLIFY(grid%soilt160)
ENDIF
IF ( ASSOCIATED( grid%soilt300 ) ) THEN 
  DEALLOCATE(grid%soilt300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt300. ')
 endif
  NULLIFY(grid%soilt300)
ENDIF
IF ( ASSOCIATED( grid%landmask ) ) THEN 
  DEALLOCATE(grid%landmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landmask. ')
 endif
  NULLIFY(grid%landmask)
ENDIF
IF ( ASSOCIATED( grid%topostdv ) ) THEN 
  DEALLOCATE(grid%topostdv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%topostdv. ')
 endif
  NULLIFY(grid%topostdv)
ENDIF
IF ( ASSOCIATED( grid%toposlpx ) ) THEN 
  DEALLOCATE(grid%toposlpx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%toposlpx. ')
 endif
  NULLIFY(grid%toposlpx)
ENDIF
IF ( ASSOCIATED( grid%toposlpy ) ) THEN 
  DEALLOCATE(grid%toposlpy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%toposlpy. ')
 endif
  NULLIFY(grid%toposlpy)
ENDIF
IF ( ASSOCIATED( grid%greenmax ) ) THEN 
  DEALLOCATE(grid%greenmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%greenmax. ')
 endif
  NULLIFY(grid%greenmax)
ENDIF
IF ( ASSOCIATED( grid%greenmin ) ) THEN 
  DEALLOCATE(grid%greenmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%greenmin. ')
 endif
  NULLIFY(grid%greenmin)
ENDIF
IF ( ASSOCIATED( grid%albedomx ) ) THEN 
  DEALLOCATE(grid%albedomx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albedomx. ')
 endif
  NULLIFY(grid%albedomx)
ENDIF
IF ( ASSOCIATED( grid%slopecat ) ) THEN 
  DEALLOCATE(grid%slopecat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%slopecat. ')
 endif
  NULLIFY(grid%slopecat)
ENDIF
IF ( ASSOCIATED( grid%toposoil ) ) THEN 
  DEALLOCATE(grid%toposoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%toposoil. ')
 endif
  NULLIFY(grid%toposoil)
ENDIF
IF ( ASSOCIATED( grid%landusef ) ) THEN 
  DEALLOCATE(grid%landusef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landusef. ')
 endif
  NULLIFY(grid%landusef)
ENDIF
IF ( ASSOCIATED( grid%soilctop ) ) THEN 
  DEALLOCATE(grid%soilctop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilctop. ')
 endif
  NULLIFY(grid%soilctop)
ENDIF
IF ( ASSOCIATED( grid%soilcbot ) ) THEN 
  DEALLOCATE(grid%soilcbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcbot. ')
 endif
  NULLIFY(grid%soilcbot)
ENDIF
IF ( ASSOCIATED( grid%ts_hour ) ) THEN 
  DEALLOCATE(grid%ts_hour,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_hour. ')
 endif
  NULLIFY(grid%ts_hour)
ENDIF
IF ( ASSOCIATED( grid%ts_u ) ) THEN 
  DEALLOCATE(grid%ts_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_u. ')
 endif
  NULLIFY(grid%ts_u)
ENDIF
IF ( ASSOCIATED( grid%ts_v ) ) THEN 
  DEALLOCATE(grid%ts_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_v. ')
 endif
  NULLIFY(grid%ts_v)
ENDIF
IF ( ASSOCIATED( grid%ts_q ) ) THEN 
  DEALLOCATE(grid%ts_q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_q. ')
 endif
  NULLIFY(grid%ts_q)
ENDIF
IF ( ASSOCIATED( grid%ts_t ) ) THEN 
  DEALLOCATE(grid%ts_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_t. ')
 endif
  NULLIFY(grid%ts_t)
ENDIF
IF ( ASSOCIATED( grid%ts_psfc ) ) THEN 
  DEALLOCATE(grid%ts_psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_psfc. ')
 endif
  NULLIFY(grid%ts_psfc)
ENDIF
IF ( ASSOCIATED( grid%ts_tsk ) ) THEN 
  DEALLOCATE(grid%ts_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_tsk. ')
 endif
  NULLIFY(grid%ts_tsk)
ENDIF
IF ( ASSOCIATED( grid%ts_tslb ) ) THEN 
  DEALLOCATE(grid%ts_tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_tslb. ')
 endif
  NULLIFY(grid%ts_tslb)
ENDIF
IF ( ASSOCIATED( grid%ts_clw ) ) THEN 
  DEALLOCATE(grid%ts_clw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ts_clw. ')
 endif
  NULLIFY(grid%ts_clw)
ENDIF
IF ( ASSOCIATED( grid%moist ) ) THEN 
  DEALLOCATE(grid%moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%moist. ')
 endif
  NULLIFY(grid%moist)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist ) ) THEN 
  DEALLOCATE(grid%dfi_moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_moist. ')
 endif
  NULLIFY(grid%dfi_moist)
ENDIF
IF ( ASSOCIATED( grid%scalar ) ) THEN 
  DEALLOCATE(grid%scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar. ')
 endif
  NULLIFY(grid%scalar)
ENDIF
IF ( ASSOCIATED( grid%scalar_bxs ) ) THEN 
  DEALLOCATE(grid%scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_bxs. ')
 endif
  NULLIFY(grid%scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_bxe ) ) THEN 
  DEALLOCATE(grid%scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_bxe. ')
 endif
  NULLIFY(grid%scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_bys ) ) THEN 
  DEALLOCATE(grid%scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_bys. ')
 endif
  NULLIFY(grid%scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%scalar_bye ) ) THEN 
  DEALLOCATE(grid%scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_bye. ')
 endif
  NULLIFY(grid%scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxs ) ) THEN 
  DEALLOCATE(grid%scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_btxs. ')
 endif
  NULLIFY(grid%scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxe ) ) THEN 
  DEALLOCATE(grid%scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_btxe. ')
 endif
  NULLIFY(grid%scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_btys ) ) THEN 
  DEALLOCATE(grid%scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_btys. ')
 endif
  NULLIFY(grid%scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%scalar_btye ) ) THEN 
  DEALLOCATE(grid%scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_btye. ')
 endif
  NULLIFY(grid%scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar ) ) THEN 
  DEALLOCATE(grid%dfi_scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar. ')
 endif
  NULLIFY(grid%dfi_scalar)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_bxs. ')
 endif
  NULLIFY(grid%dfi_scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_bxe. ')
 endif
  NULLIFY(grid%dfi_scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_bys. ')
 endif
  NULLIFY(grid%dfi_scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_bye. ')
 endif
  NULLIFY(grid%dfi_scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_btxs. ')
 endif
  NULLIFY(grid%dfi_scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_btxe. ')
 endif
  NULLIFY(grid%dfi_scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_btys. ')
 endif
  NULLIFY(grid%dfi_scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dfi_scalar_btye. ')
 endif
  NULLIFY(grid%dfi_scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%chem ) ) THEN 
  DEALLOCATE(grid%chem,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%chem. ')
 endif
  NULLIFY(grid%chem)
ENDIF
IF ( ASSOCIATED( grid%smois ) ) THEN 
  DEALLOCATE(grid%smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smois. ')
 endif
  NULLIFY(grid%smois)
ENDIF
IF ( ASSOCIATED( grid%tslb ) ) THEN 
  DEALLOCATE(grid%tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tslb. ')
 endif
  NULLIFY(grid%tslb)
ENDIF
IF ( ASSOCIATED( grid%gsw ) ) THEN 
  DEALLOCATE(grid%gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%gsw. ')
 endif
  NULLIFY(grid%gsw)
ENDIF
IF ( ASSOCIATED( grid%xlat ) ) THEN 
  DEALLOCATE(grid%xlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xlat. ')
 endif
  NULLIFY(grid%xlat)
ENDIF
IF ( ASSOCIATED( grid%xlong ) ) THEN 
  DEALLOCATE(grid%xlong,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xlong. ')
 endif
  NULLIFY(grid%xlong)
ENDIF
IF ( ASSOCIATED( grid%xland ) ) THEN 
  DEALLOCATE(grid%xland,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xland. ')
 endif
  NULLIFY(grid%xland)
ENDIF
IF ( ASSOCIATED( grid%raincv ) ) THEN 
  DEALLOCATE(grid%raincv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%raincv. ')
 endif
  NULLIFY(grid%raincv)
ENDIF
IF ( ASSOCIATED( grid%psfc ) ) THEN 
  DEALLOCATE(grid%psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%psfc. ')
 endif
  NULLIFY(grid%psfc)
ENDIF
IF ( ASSOCIATED( grid%th2 ) ) THEN 
  DEALLOCATE(grid%th2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%th2. ')
 endif
  NULLIFY(grid%th2)
ENDIF
IF ( ASSOCIATED( grid%t2 ) ) THEN 
  DEALLOCATE(grid%t2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t2. ')
 endif
  NULLIFY(grid%t2)
ENDIF
IF ( ASSOCIATED( grid%u10 ) ) THEN 
  DEALLOCATE(grid%u10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u10. ')
 endif
  NULLIFY(grid%u10)
ENDIF
IF ( ASSOCIATED( grid%v10 ) ) THEN 
  DEALLOCATE(grid%v10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v10. ')
 endif
  NULLIFY(grid%v10)
ENDIF
IF ( ASSOCIATED( grid%xice ) ) THEN 
  DEALLOCATE(grid%xice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xice. ')
 endif
  NULLIFY(grid%xice)
ENDIF
IF ( ASSOCIATED( grid%lai ) ) THEN 
  DEALLOCATE(grid%lai,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lai. ')
 endif
  NULLIFY(grid%lai)
ENDIF
IF ( ASSOCIATED( grid%smstav ) ) THEN 
  DEALLOCATE(grid%smstav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smstav. ')
 endif
  NULLIFY(grid%smstav)
ENDIF
IF ( ASSOCIATED( grid%smstot ) ) THEN 
  DEALLOCATE(grid%smstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smstot. ')
 endif
  NULLIFY(grid%smstot)
ENDIF
IF ( ASSOCIATED( grid%sfcrunoff ) ) THEN 
  DEALLOCATE(grid%sfcrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcrunoff. ')
 endif
  NULLIFY(grid%sfcrunoff)
ENDIF
IF ( ASSOCIATED( grid%udrunoff ) ) THEN 
  DEALLOCATE(grid%udrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%udrunoff. ')
 endif
  NULLIFY(grid%udrunoff)
ENDIF
IF ( ASSOCIATED( grid%ivgtyp ) ) THEN 
  DEALLOCATE(grid%ivgtyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ivgtyp. ')
 endif
  NULLIFY(grid%ivgtyp)
ENDIF
IF ( ASSOCIATED( grid%isltyp ) ) THEN 
  DEALLOCATE(grid%isltyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%isltyp. ')
 endif
  NULLIFY(grid%isltyp)
ENDIF
IF ( ASSOCIATED( grid%vegfra ) ) THEN 
  DEALLOCATE(grid%vegfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vegfra. ')
 endif
  NULLIFY(grid%vegfra)
ENDIF
IF ( ASSOCIATED( grid%sfcevp ) ) THEN 
  DEALLOCATE(grid%sfcevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcevp. ')
 endif
  NULLIFY(grid%sfcevp)
ENDIF
IF ( ASSOCIATED( grid%grdflx ) ) THEN 
  DEALLOCATE(grid%grdflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%grdflx. ')
 endif
  NULLIFY(grid%grdflx)
ENDIF
IF ( ASSOCIATED( grid%albbck ) ) THEN 
  DEALLOCATE(grid%albbck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albbck. ')
 endif
  NULLIFY(grid%albbck)
ENDIF
IF ( ASSOCIATED( grid%sfcexc ) ) THEN 
  DEALLOCATE(grid%sfcexc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcexc. ')
 endif
  NULLIFY(grid%sfcexc)
ENDIF
IF ( ASSOCIATED( grid%snotime ) ) THEN 
  DEALLOCATE(grid%snotime,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snotime. ')
 endif
  NULLIFY(grid%snotime)
ENDIF
IF ( ASSOCIATED( grid%acsnow ) ) THEN 
  DEALLOCATE(grid%acsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acsnow. ')
 endif
  NULLIFY(grid%acsnow)
ENDIF
IF ( ASSOCIATED( grid%acsnom ) ) THEN 
  DEALLOCATE(grid%acsnom,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acsnom. ')
 endif
  NULLIFY(grid%acsnom)
ENDIF
IF ( ASSOCIATED( grid%rmol ) ) THEN 
  DEALLOCATE(grid%rmol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rmol. ')
 endif
  NULLIFY(grid%rmol)
ENDIF
IF ( ASSOCIATED( grid%snow ) ) THEN 
  DEALLOCATE(grid%snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snow. ')
 endif
  NULLIFY(grid%snow)
ENDIF
IF ( ASSOCIATED( grid%canwat ) ) THEN 
  DEALLOCATE(grid%canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%canwat. ')
 endif
  NULLIFY(grid%canwat)
ENDIF
IF ( ASSOCIATED( grid%sst ) ) THEN 
  DEALLOCATE(grid%sst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sst. ')
 endif
  NULLIFY(grid%sst)
ENDIF
IF ( ASSOCIATED( grid%weasd ) ) THEN 
  DEALLOCATE(grid%weasd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%weasd. ')
 endif
  NULLIFY(grid%weasd)
ENDIF
IF ( ASSOCIATED( grid%znt ) ) THEN 
  DEALLOCATE(grid%znt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%znt. ')
 endif
  NULLIFY(grid%znt)
ENDIF
IF ( ASSOCIATED( grid%mol ) ) THEN 
  DEALLOCATE(grid%mol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mol. ')
 endif
  NULLIFY(grid%mol)
ENDIF
IF ( ASSOCIATED( grid%noahres ) ) THEN 
  DEALLOCATE(grid%noahres,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%noahres. ')
 endif
  NULLIFY(grid%noahres)
ENDIF
IF ( ASSOCIATED( grid%tke_myj ) ) THEN 
  DEALLOCATE(grid%tke_myj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tke_myj. ')
 endif
  NULLIFY(grid%tke_myj)
ENDIF
IF ( ASSOCIATED( grid%el_myj ) ) THEN 
  DEALLOCATE(grid%el_myj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%el_myj. ')
 endif
  NULLIFY(grid%el_myj)
ENDIF
IF ( ASSOCIATED( grid%exch_h ) ) THEN 
  DEALLOCATE(grid%exch_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%exch_h. ')
 endif
  NULLIFY(grid%exch_h)
ENDIF
IF ( ASSOCIATED( grid%exch_m ) ) THEN 
  DEALLOCATE(grid%exch_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%exch_m. ')
 endif
  NULLIFY(grid%exch_m)
ENDIF
IF ( ASSOCIATED( grid%thz0 ) ) THEN 
  DEALLOCATE(grid%thz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%thz0. ')
 endif
  NULLIFY(grid%thz0)
ENDIF
IF ( ASSOCIATED( grid%qz0 ) ) THEN 
  DEALLOCATE(grid%qz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qz0. ')
 endif
  NULLIFY(grid%qz0)
ENDIF
IF ( ASSOCIATED( grid%uz0 ) ) THEN 
  DEALLOCATE(grid%uz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%uz0. ')
 endif
  NULLIFY(grid%uz0)
ENDIF
IF ( ASSOCIATED( grid%vz0 ) ) THEN 
  DEALLOCATE(grid%vz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vz0. ')
 endif
  NULLIFY(grid%vz0)
ENDIF
IF ( ASSOCIATED( grid%flhc ) ) THEN 
  DEALLOCATE(grid%flhc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%flhc. ')
 endif
  NULLIFY(grid%flhc)
ENDIF
IF ( ASSOCIATED( grid%flqc ) ) THEN 
  DEALLOCATE(grid%flqc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%flqc. ')
 endif
  NULLIFY(grid%flqc)
ENDIF
IF ( ASSOCIATED( grid%qsg ) ) THEN 
  DEALLOCATE(grid%qsg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qsg. ')
 endif
  NULLIFY(grid%qsg)
ENDIF
IF ( ASSOCIATED( grid%qvg ) ) THEN 
  DEALLOCATE(grid%qvg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qvg. ')
 endif
  NULLIFY(grid%qvg)
ENDIF
IF ( ASSOCIATED( grid%qcg ) ) THEN 
  DEALLOCATE(grid%qcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qcg. ')
 endif
  NULLIFY(grid%qcg)
ENDIF
IF ( ASSOCIATED( grid%soilt1 ) ) THEN 
  DEALLOCATE(grid%soilt1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt1. ')
 endif
  NULLIFY(grid%soilt1)
ENDIF
IF ( ASSOCIATED( grid%tsnav ) ) THEN 
  DEALLOCATE(grid%tsnav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tsnav. ')
 endif
  NULLIFY(grid%tsnav)
ENDIF
IF ( ASSOCIATED( grid%psfc_out ) ) THEN 
  DEALLOCATE(grid%psfc_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%psfc_out. ')
 endif
  NULLIFY(grid%psfc_out)
ENDIF
IF ( ASSOCIATED( grid%uz0h ) ) THEN 
  DEALLOCATE(grid%uz0h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%uz0h. ')
 endif
  NULLIFY(grid%uz0h)
ENDIF
IF ( ASSOCIATED( grid%vz0h ) ) THEN 
  DEALLOCATE(grid%vz0h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vz0h. ')
 endif
  NULLIFY(grid%vz0h)
ENDIF
IF ( ASSOCIATED( grid%dudt ) ) THEN 
  DEALLOCATE(grid%dudt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dudt. ')
 endif
  NULLIFY(grid%dudt)
ENDIF
IF ( ASSOCIATED( grid%dvdt ) ) THEN 
  DEALLOCATE(grid%dvdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dvdt. ')
 endif
  NULLIFY(grid%dvdt)
ENDIF
IF ( ASSOCIATED( grid%qsfc ) ) THEN 
  DEALLOCATE(grid%qsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qsfc. ')
 endif
  NULLIFY(grid%qsfc)
ENDIF
IF ( ASSOCIATED( grid%akhs ) ) THEN 
  DEALLOCATE(grid%akhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%akhs. ')
 endif
  NULLIFY(grid%akhs)
ENDIF
IF ( ASSOCIATED( grid%akms ) ) THEN 
  DEALLOCATE(grid%akms,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%akms. ')
 endif
  NULLIFY(grid%akms)
ENDIF
IF ( ASSOCIATED( grid%htop ) ) THEN 
  DEALLOCATE(grid%htop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htop. ')
 endif
  NULLIFY(grid%htop)
ENDIF
IF ( ASSOCIATED( grid%hbot ) ) THEN 
  DEALLOCATE(grid%hbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbot. ')
 endif
  NULLIFY(grid%hbot)
ENDIF
IF ( ASSOCIATED( grid%htopr ) ) THEN 
  DEALLOCATE(grid%htopr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htopr. ')
 endif
  NULLIFY(grid%htopr)
ENDIF
IF ( ASSOCIATED( grid%hbotr ) ) THEN 
  DEALLOCATE(grid%hbotr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbotr. ')
 endif
  NULLIFY(grid%hbotr)
ENDIF
IF ( ASSOCIATED( grid%htopd ) ) THEN 
  DEALLOCATE(grid%htopd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htopd. ')
 endif
  NULLIFY(grid%htopd)
ENDIF
IF ( ASSOCIATED( grid%hbotd ) ) THEN 
  DEALLOCATE(grid%hbotd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbotd. ')
 endif
  NULLIFY(grid%hbotd)
ENDIF
IF ( ASSOCIATED( grid%htops ) ) THEN 
  DEALLOCATE(grid%htops,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htops. ')
 endif
  NULLIFY(grid%htops)
ENDIF
IF ( ASSOCIATED( grid%hbots ) ) THEN 
  DEALLOCATE(grid%hbots,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbots. ')
 endif
  NULLIFY(grid%hbots)
ENDIF
IF ( ASSOCIATED( grid%cuppt ) ) THEN 
  DEALLOCATE(grid%cuppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cuppt. ')
 endif
  NULLIFY(grid%cuppt)
ENDIF
IF ( ASSOCIATED( grid%cprate ) ) THEN 
  DEALLOCATE(grid%cprate,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cprate. ')
 endif
  NULLIFY(grid%cprate)
ENDIF
IF ( ASSOCIATED( grid%f_ice_phy ) ) THEN 
  DEALLOCATE(grid%f_ice_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_ice_phy. ')
 endif
  NULLIFY(grid%f_ice_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rain_phy ) ) THEN 
  DEALLOCATE(grid%f_rain_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_rain_phy. ')
 endif
  NULLIFY(grid%f_rain_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rimef_phy ) ) THEN 
  DEALLOCATE(grid%f_rimef_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_rimef_phy. ')
 endif
  NULLIFY(grid%f_rimef_phy)
ENDIF
IF ( ASSOCIATED( grid%mass_flux ) ) THEN 
  DEALLOCATE(grid%mass_flux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mass_flux. ')
 endif
  NULLIFY(grid%mass_flux)
ENDIF
IF ( ASSOCIATED( grid%apr_gr ) ) THEN 
  DEALLOCATE(grid%apr_gr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_gr. ')
 endif
  NULLIFY(grid%apr_gr)
ENDIF
IF ( ASSOCIATED( grid%apr_w ) ) THEN 
  DEALLOCATE(grid%apr_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_w. ')
 endif
  NULLIFY(grid%apr_w)
ENDIF
IF ( ASSOCIATED( grid%apr_mc ) ) THEN 
  DEALLOCATE(grid%apr_mc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_mc. ')
 endif
  NULLIFY(grid%apr_mc)
ENDIF
IF ( ASSOCIATED( grid%apr_st ) ) THEN 
  DEALLOCATE(grid%apr_st,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_st. ')
 endif
  NULLIFY(grid%apr_st)
ENDIF
IF ( ASSOCIATED( grid%apr_as ) ) THEN 
  DEALLOCATE(grid%apr_as,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_as. ')
 endif
  NULLIFY(grid%apr_as)
ENDIF
IF ( ASSOCIATED( grid%apr_capma ) ) THEN 
  DEALLOCATE(grid%apr_capma,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_capma. ')
 endif
  NULLIFY(grid%apr_capma)
ENDIF
IF ( ASSOCIATED( grid%apr_capme ) ) THEN 
  DEALLOCATE(grid%apr_capme,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_capme. ')
 endif
  NULLIFY(grid%apr_capme)
ENDIF
IF ( ASSOCIATED( grid%apr_capmi ) ) THEN 
  DEALLOCATE(grid%apr_capmi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_capmi. ')
 endif
  NULLIFY(grid%apr_capmi)
ENDIF
IF ( ASSOCIATED( grid%xf_ens ) ) THEN 
  DEALLOCATE(grid%xf_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xf_ens. ')
 endif
  NULLIFY(grid%xf_ens)
ENDIF
IF ( ASSOCIATED( grid%pr_ens ) ) THEN 
  DEALLOCATE(grid%pr_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pr_ens. ')
 endif
  NULLIFY(grid%pr_ens)
ENDIF
IF ( ASSOCIATED( grid%rthften ) ) THEN 
  DEALLOCATE(grid%rthften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rthften. ')
 endif
  NULLIFY(grid%rthften)
ENDIF
IF ( ASSOCIATED( grid%rqvften ) ) THEN 
  DEALLOCATE(grid%rqvften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqvften. ')
 endif
  NULLIFY(grid%rqvften)
ENDIF
IF ( ASSOCIATED( grid%snowh ) ) THEN 
  DEALLOCATE(grid%snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snowh. ')
 endif
  NULLIFY(grid%snowh)
ENDIF
IF ( ASSOCIATED( grid%rhosn ) ) THEN 
  DEALLOCATE(grid%rhosn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rhosn. ')
 endif
  NULLIFY(grid%rhosn)
ENDIF
IF ( ASSOCIATED( grid%smfr3d ) ) THEN 
  DEALLOCATE(grid%smfr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smfr3d. ')
 endif
  NULLIFY(grid%smfr3d)
ENDIF
IF ( ASSOCIATED( grid%keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%keepfr3dflag. ')
 endif
  NULLIFY(grid%keepfr3dflag)
ENDIF
IF ( ASSOCIATED( grid%mp_restart_state ) ) THEN 
  DEALLOCATE(grid%mp_restart_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mp_restart_state. ')
 endif
  NULLIFY(grid%mp_restart_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs_state ) ) THEN 
  DEALLOCATE(grid%tbpvs_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tbpvs_state. ')
 endif
  NULLIFY(grid%tbpvs_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs0_state ) ) THEN 
  DEALLOCATE(grid%tbpvs0_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tbpvs0_state. ')
 endif
  NULLIFY(grid%tbpvs0_state)
ENDIF
IF ( ASSOCIATED( grid%lu_state ) ) THEN 
  DEALLOCATE(grid%lu_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lu_state. ')
 endif
  NULLIFY(grid%lu_state)
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
