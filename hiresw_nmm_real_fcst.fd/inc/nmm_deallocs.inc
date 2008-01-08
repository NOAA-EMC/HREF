!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_deallocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
IF ( ASSOCIATED( grid%nmm_p_gc ) ) THEN 
  DEALLOCATE(grid%nmm_p_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_p_gc. ')
 endif
  NULLIFY(grid%nmm_p_gc)
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
IF ( ASSOCIATED( grid%nmm_tsk_gc ) ) THEN 
  DEALLOCATE(grid%nmm_tsk_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tsk_gc. ')
 endif
  NULLIFY(grid%nmm_tsk_gc)
ENDIF
IF ( ASSOCIATED( grid%xice_gc ) ) THEN 
  DEALLOCATE(grid%xice_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xice_gc. ')
 endif
  NULLIFY(grid%xice_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_ght_gc ) ) THEN 
  DEALLOCATE(grid%nmm_ght_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ght_gc. ')
 endif
  NULLIFY(grid%nmm_ght_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_rh_gc ) ) THEN 
  DEALLOCATE(grid%nmm_rh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rh_gc. ')
 endif
  NULLIFY(grid%nmm_rh_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_v_gc ) ) THEN 
  DEALLOCATE(grid%nmm_v_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_v_gc. ')
 endif
  NULLIFY(grid%nmm_v_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_u_gc ) ) THEN 
  DEALLOCATE(grid%nmm_u_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_u_gc. ')
 endif
  NULLIFY(grid%nmm_u_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_gc ) ) THEN 
  DEALLOCATE(grid%nmm_t_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_gc. ')
 endif
  NULLIFY(grid%nmm_t_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_rwmr_gc ) ) THEN 
  DEALLOCATE(grid%nmm_rwmr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rwmr_gc. ')
 endif
  NULLIFY(grid%nmm_rwmr_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_snmr_gc ) ) THEN 
  DEALLOCATE(grid%nmm_snmr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_snmr_gc. ')
 endif
  NULLIFY(grid%nmm_snmr_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_clwmr_gc ) ) THEN 
  DEALLOCATE(grid%nmm_clwmr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_clwmr_gc. ')
 endif
  NULLIFY(grid%nmm_clwmr_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_cice_gc ) ) THEN 
  DEALLOCATE(grid%nmm_cice_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cice_gc. ')
 endif
  NULLIFY(grid%nmm_cice_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_rimef_gc ) ) THEN 
  DEALLOCATE(grid%nmm_rimef_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rimef_gc. ')
 endif
  NULLIFY(grid%nmm_rimef_gc)
ENDIF
IF ( ASSOCIATED( grid%snoalb ) ) THEN 
  DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snoalb. ')
 endif
  NULLIFY(grid%snoalb)
ENDIF
IF ( ASSOCIATED( grid%nmm_greenfrac_gc ) ) THEN 
  DEALLOCATE(grid%nmm_greenfrac_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_greenfrac_gc. ')
 endif
  NULLIFY(grid%nmm_greenfrac_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_albedo12m_gc ) ) THEN 
  DEALLOCATE(grid%nmm_albedo12m_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_albedo12m_gc. ')
 endif
  NULLIFY(grid%nmm_albedo12m_gc)
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
IF ( ASSOCIATED( grid%nmm_tmn_gc ) ) THEN 
  DEALLOCATE(grid%nmm_tmn_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tmn_gc. ')
 endif
  NULLIFY(grid%nmm_tmn_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_htv_gc ) ) THEN 
  DEALLOCATE(grid%nmm_htv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_htv_gc. ')
 endif
  NULLIFY(grid%nmm_htv_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_ht_gc ) ) THEN 
  DEALLOCATE(grid%nmm_ht_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ht_gc. ')
 endif
  NULLIFY(grid%nmm_ht_gc)
ENDIF
IF ( ASSOCIATED( grid%landusef_gc ) ) THEN 
  DEALLOCATE(grid%landusef_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landusef_gc. ')
 endif
  NULLIFY(grid%landusef_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_vlon_gc ) ) THEN 
  DEALLOCATE(grid%nmm_vlon_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vlon_gc. ')
 endif
  NULLIFY(grid%nmm_vlon_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_vlat_gc ) ) THEN 
  DEALLOCATE(grid%nmm_vlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vlat_gc. ')
 endif
  NULLIFY(grid%nmm_vlat_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_hlon_gc ) ) THEN 
  DEALLOCATE(grid%nmm_hlon_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hlon_gc. ')
 endif
  NULLIFY(grid%nmm_hlon_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_hlat_gc ) ) THEN 
  DEALLOCATE(grid%nmm_hlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hlat_gc. ')
 endif
  NULLIFY(grid%nmm_hlat_gc)
ENDIF
IF ( ASSOCIATED( grid%nmm_hbm2 ) ) THEN 
  DEALLOCATE(grid%nmm_hbm2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hbm2. ')
 endif
  NULLIFY(grid%nmm_hbm2)
ENDIF
IF ( ASSOCIATED( grid%nmm_hbm3 ) ) THEN 
  DEALLOCATE(grid%nmm_hbm3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hbm3. ')
 endif
  NULLIFY(grid%nmm_hbm3)
ENDIF
IF ( ASSOCIATED( grid%nmm_vbm2 ) ) THEN 
  DEALLOCATE(grid%nmm_vbm2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vbm2. ')
 endif
  NULLIFY(grid%nmm_vbm2)
ENDIF
IF ( ASSOCIATED( grid%nmm_vbm3 ) ) THEN 
  DEALLOCATE(grid%nmm_vbm3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vbm3. ')
 endif
  NULLIFY(grid%nmm_vbm3)
ENDIF
IF ( ASSOCIATED( grid%nmm_sm ) ) THEN 
  DEALLOCATE(grid%nmm_sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sm. ')
 endif
  NULLIFY(grid%nmm_sm)
ENDIF
IF ( ASSOCIATED( grid%nmm_sice ) ) THEN 
  DEALLOCATE(grid%nmm_sice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sice. ')
 endif
  NULLIFY(grid%nmm_sice)
ENDIF
IF ( ASSOCIATED( grid%nmm_pd ) ) THEN 
  DEALLOCATE(grid%nmm_pd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pd. ')
 endif
  NULLIFY(grid%nmm_pd)
ENDIF
IF ( ASSOCIATED( grid%nmm_pd_b ) ) THEN 
  DEALLOCATE(grid%nmm_pd_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pd_b. ')
 endif
  NULLIFY(grid%nmm_pd_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_pd_bt ) ) THEN 
  DEALLOCATE(grid%nmm_pd_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pd_bt. ')
 endif
  NULLIFY(grid%nmm_pd_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_fis ) ) THEN 
  DEALLOCATE(grid%nmm_fis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fis. ')
 endif
  NULLIFY(grid%nmm_fis)
ENDIF
IF ( ASSOCIATED( grid%nmm_res ) ) THEN 
  DEALLOCATE(grid%nmm_res,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_res. ')
 endif
  NULLIFY(grid%nmm_res)
ENDIF
IF ( ASSOCIATED( grid%nmm_t ) ) THEN 
  DEALLOCATE(grid%nmm_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t. ')
 endif
  NULLIFY(grid%nmm_t)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_b ) ) THEN 
  DEALLOCATE(grid%nmm_t_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_b. ')
 endif
  NULLIFY(grid%nmm_t_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_bt ) ) THEN 
  DEALLOCATE(grid%nmm_t_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_bt. ')
 endif
  NULLIFY(grid%nmm_t_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_q ) ) THEN 
  DEALLOCATE(grid%nmm_q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q. ')
 endif
  NULLIFY(grid%nmm_q)
ENDIF
IF ( ASSOCIATED( grid%nmm_q_b ) ) THEN 
  DEALLOCATE(grid%nmm_q_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q_b. ')
 endif
  NULLIFY(grid%nmm_q_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_q_bt ) ) THEN 
  DEALLOCATE(grid%nmm_q_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q_bt. ')
 endif
  NULLIFY(grid%nmm_q_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_u ) ) THEN 
  DEALLOCATE(grid%nmm_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_u. ')
 endif
  NULLIFY(grid%nmm_u)
ENDIF
IF ( ASSOCIATED( grid%nmm_u_b ) ) THEN 
  DEALLOCATE(grid%nmm_u_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_u_b. ')
 endif
  NULLIFY(grid%nmm_u_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_u_bt ) ) THEN 
  DEALLOCATE(grid%nmm_u_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_u_bt. ')
 endif
  NULLIFY(grid%nmm_u_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_v ) ) THEN 
  DEALLOCATE(grid%nmm_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_v. ')
 endif
  NULLIFY(grid%nmm_v)
ENDIF
IF ( ASSOCIATED( grid%nmm_v_b ) ) THEN 
  DEALLOCATE(grid%nmm_v_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_v_b. ')
 endif
  NULLIFY(grid%nmm_v_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_v_bt ) ) THEN 
  DEALLOCATE(grid%nmm_v_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_v_bt. ')
 endif
  NULLIFY(grid%nmm_v_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_told ) ) THEN 
  DEALLOCATE(grid%nmm_told,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_told. ')
 endif
  NULLIFY(grid%nmm_told)
ENDIF
IF ( ASSOCIATED( grid%nmm_uold ) ) THEN 
  DEALLOCATE(grid%nmm_uold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_uold. ')
 endif
  NULLIFY(grid%nmm_uold)
ENDIF
IF ( ASSOCIATED( grid%nmm_vold ) ) THEN 
  DEALLOCATE(grid%nmm_vold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vold. ')
 endif
  NULLIFY(grid%nmm_vold)
ENDIF
IF ( ASSOCIATED( grid%nmm_dx_nmm ) ) THEN 
  DEALLOCATE(grid%nmm_dx_nmm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dx_nmm. ')
 endif
  NULLIFY(grid%nmm_dx_nmm)
ENDIF
IF ( ASSOCIATED( grid%nmm_wpdar ) ) THEN 
  DEALLOCATE(grid%nmm_wpdar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_wpdar. ')
 endif
  NULLIFY(grid%nmm_wpdar)
ENDIF
IF ( ASSOCIATED( grid%nmm_cpgfu ) ) THEN 
  DEALLOCATE(grid%nmm_cpgfu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cpgfu. ')
 endif
  NULLIFY(grid%nmm_cpgfu)
ENDIF
IF ( ASSOCIATED( grid%nmm_curv ) ) THEN 
  DEALLOCATE(grid%nmm_curv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_curv. ')
 endif
  NULLIFY(grid%nmm_curv)
ENDIF
IF ( ASSOCIATED( grid%nmm_fcp ) ) THEN 
  DEALLOCATE(grid%nmm_fcp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fcp. ')
 endif
  NULLIFY(grid%nmm_fcp)
ENDIF
IF ( ASSOCIATED( grid%nmm_fdiv ) ) THEN 
  DEALLOCATE(grid%nmm_fdiv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fdiv. ')
 endif
  NULLIFY(grid%nmm_fdiv)
ENDIF
IF ( ASSOCIATED( grid%nmm_f ) ) THEN 
  DEALLOCATE(grid%nmm_f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f. ')
 endif
  NULLIFY(grid%nmm_f)
ENDIF
IF ( ASSOCIATED( grid%nmm_fad ) ) THEN 
  DEALLOCATE(grid%nmm_fad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fad. ')
 endif
  NULLIFY(grid%nmm_fad)
ENDIF
IF ( ASSOCIATED( grid%nmm_ddmpu ) ) THEN 
  DEALLOCATE(grid%nmm_ddmpu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ddmpu. ')
 endif
  NULLIFY(grid%nmm_ddmpu)
ENDIF
IF ( ASSOCIATED( grid%nmm_ddmpv ) ) THEN 
  DEALLOCATE(grid%nmm_ddmpv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ddmpv. ')
 endif
  NULLIFY(grid%nmm_ddmpv)
ENDIF
IF ( ASSOCIATED( grid%nmm_deta ) ) THEN 
  DEALLOCATE(grid%nmm_deta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_deta. ')
 endif
  NULLIFY(grid%nmm_deta)
ENDIF
IF ( ASSOCIATED( grid%nmm_rdeta ) ) THEN 
  DEALLOCATE(grid%nmm_rdeta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rdeta. ')
 endif
  NULLIFY(grid%nmm_rdeta)
ENDIF
IF ( ASSOCIATED( grid%nmm_aeta ) ) THEN 
  DEALLOCATE(grid%nmm_aeta,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aeta. ')
 endif
  NULLIFY(grid%nmm_aeta)
ENDIF
IF ( ASSOCIATED( grid%nmm_f4q2 ) ) THEN 
  DEALLOCATE(grid%nmm_f4q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f4q2. ')
 endif
  NULLIFY(grid%nmm_f4q2)
ENDIF
IF ( ASSOCIATED( grid%nmm_etax ) ) THEN 
  DEALLOCATE(grid%nmm_etax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_etax. ')
 endif
  NULLIFY(grid%nmm_etax)
ENDIF
IF ( ASSOCIATED( grid%nmm_dfl ) ) THEN 
  DEALLOCATE(grid%nmm_dfl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dfl. ')
 endif
  NULLIFY(grid%nmm_dfl)
ENDIF
IF ( ASSOCIATED( grid%nmm_deta1 ) ) THEN 
  DEALLOCATE(grid%nmm_deta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_deta1. ')
 endif
  NULLIFY(grid%nmm_deta1)
ENDIF
IF ( ASSOCIATED( grid%nmm_aeta1 ) ) THEN 
  DEALLOCATE(grid%nmm_aeta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aeta1. ')
 endif
  NULLIFY(grid%nmm_aeta1)
ENDIF
IF ( ASSOCIATED( grid%nmm_eta1 ) ) THEN 
  DEALLOCATE(grid%nmm_eta1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_eta1. ')
 endif
  NULLIFY(grid%nmm_eta1)
ENDIF
IF ( ASSOCIATED( grid%nmm_deta2 ) ) THEN 
  DEALLOCATE(grid%nmm_deta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_deta2. ')
 endif
  NULLIFY(grid%nmm_deta2)
ENDIF
IF ( ASSOCIATED( grid%nmm_aeta2 ) ) THEN 
  DEALLOCATE(grid%nmm_aeta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aeta2. ')
 endif
  NULLIFY(grid%nmm_aeta2)
ENDIF
IF ( ASSOCIATED( grid%nmm_eta2 ) ) THEN 
  DEALLOCATE(grid%nmm_eta2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_eta2. ')
 endif
  NULLIFY(grid%nmm_eta2)
ENDIF
IF ( ASSOCIATED( grid%nmm_em ) ) THEN 
  DEALLOCATE(grid%nmm_em,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_em. ')
 endif
  NULLIFY(grid%nmm_em)
ENDIF
IF ( ASSOCIATED( grid%nmm_emt ) ) THEN 
  DEALLOCATE(grid%nmm_emt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_emt. ')
 endif
  NULLIFY(grid%nmm_emt)
ENDIF
IF ( ASSOCIATED( grid%nmm_adt ) ) THEN 
  DEALLOCATE(grid%nmm_adt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_adt. ')
 endif
  NULLIFY(grid%nmm_adt)
ENDIF
IF ( ASSOCIATED( grid%nmm_adu ) ) THEN 
  DEALLOCATE(grid%nmm_adu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_adu. ')
 endif
  NULLIFY(grid%nmm_adu)
ENDIF
IF ( ASSOCIATED( grid%nmm_adv ) ) THEN 
  DEALLOCATE(grid%nmm_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_adv. ')
 endif
  NULLIFY(grid%nmm_adv)
ENDIF
IF ( ASSOCIATED( grid%nmm_em_loc ) ) THEN 
  DEALLOCATE(grid%nmm_em_loc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_em_loc. ')
 endif
  NULLIFY(grid%nmm_em_loc)
ENDIF
IF ( ASSOCIATED( grid%nmm_emt_loc ) ) THEN 
  DEALLOCATE(grid%nmm_emt_loc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_emt_loc. ')
 endif
  NULLIFY(grid%nmm_emt_loc)
ENDIF
IF ( ASSOCIATED( grid%nmm_pdsl ) ) THEN 
  DEALLOCATE(grid%nmm_pdsl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pdsl. ')
 endif
  NULLIFY(grid%nmm_pdsl)
ENDIF
IF ( ASSOCIATED( grid%nmm_pdslo ) ) THEN 
  DEALLOCATE(grid%nmm_pdslo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pdslo. ')
 endif
  NULLIFY(grid%nmm_pdslo)
ENDIF
IF ( ASSOCIATED( grid%nmm_psdt ) ) THEN 
  DEALLOCATE(grid%nmm_psdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_psdt. ')
 endif
  NULLIFY(grid%nmm_psdt)
ENDIF
IF ( ASSOCIATED( grid%nmm_div ) ) THEN 
  DEALLOCATE(grid%nmm_div,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_div. ')
 endif
  NULLIFY(grid%nmm_div)
ENDIF
IF ( ASSOCIATED( grid%nmm_few ) ) THEN 
  DEALLOCATE(grid%nmm_few,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_few. ')
 endif
  NULLIFY(grid%nmm_few)
ENDIF
IF ( ASSOCIATED( grid%nmm_fne ) ) THEN 
  DEALLOCATE(grid%nmm_fne,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fne. ')
 endif
  NULLIFY(grid%nmm_fne)
ENDIF
IF ( ASSOCIATED( grid%nmm_fns ) ) THEN 
  DEALLOCATE(grid%nmm_fns,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fns. ')
 endif
  NULLIFY(grid%nmm_fns)
ENDIF
IF ( ASSOCIATED( grid%nmm_fse ) ) THEN 
  DEALLOCATE(grid%nmm_fse,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_fse. ')
 endif
  NULLIFY(grid%nmm_fse)
ENDIF
IF ( ASSOCIATED( grid%nmm_omgalf ) ) THEN 
  DEALLOCATE(grid%nmm_omgalf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_omgalf. ')
 endif
  NULLIFY(grid%nmm_omgalf)
ENDIF
IF ( ASSOCIATED( grid%nmm_petdt ) ) THEN 
  DEALLOCATE(grid%nmm_petdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_petdt. ')
 endif
  NULLIFY(grid%nmm_petdt)
ENDIF
IF ( ASSOCIATED( grid%nmm_rtop ) ) THEN 
  DEALLOCATE(grid%nmm_rtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rtop. ')
 endif
  NULLIFY(grid%nmm_rtop)
ENDIF
IF ( ASSOCIATED( grid%nmm_pblh ) ) THEN 
  DEALLOCATE(grid%nmm_pblh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pblh. ')
 endif
  NULLIFY(grid%nmm_pblh)
ENDIF
IF ( ASSOCIATED( grid%nmm_lpbl ) ) THEN 
  DEALLOCATE(grid%nmm_lpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_lpbl. ')
 endif
  NULLIFY(grid%nmm_lpbl)
ENDIF
IF ( ASSOCIATED( grid%nmm_ustar ) ) THEN 
  DEALLOCATE(grid%nmm_ustar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ustar. ')
 endif
  NULLIFY(grid%nmm_ustar)
ENDIF
IF ( ASSOCIATED( grid%nmm_z0 ) ) THEN 
  DEALLOCATE(grid%nmm_z0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_z0. ')
 endif
  NULLIFY(grid%nmm_z0)
ENDIF
IF ( ASSOCIATED( grid%nmm_z0base ) ) THEN 
  DEALLOCATE(grid%nmm_z0base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_z0base. ')
 endif
  NULLIFY(grid%nmm_z0base)
ENDIF
IF ( ASSOCIATED( grid%nmm_ths ) ) THEN 
  DEALLOCATE(grid%nmm_ths,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ths. ')
 endif
  NULLIFY(grid%nmm_ths)
ENDIF
IF ( ASSOCIATED( grid%nmm_mavail ) ) THEN 
  DEALLOCATE(grid%nmm_mavail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_mavail. ')
 endif
  NULLIFY(grid%nmm_mavail)
ENDIF
IF ( ASSOCIATED( grid%nmm_qsh ) ) THEN 
  DEALLOCATE(grid%nmm_qsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_qsh. ')
 endif
  NULLIFY(grid%nmm_qsh)
ENDIF
IF ( ASSOCIATED( grid%nmm_twbs ) ) THEN 
  DEALLOCATE(grid%nmm_twbs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_twbs. ')
 endif
  NULLIFY(grid%nmm_twbs)
ENDIF
IF ( ASSOCIATED( grid%nmm_qwbs ) ) THEN 
  DEALLOCATE(grid%nmm_qwbs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_qwbs. ')
 endif
  NULLIFY(grid%nmm_qwbs)
ENDIF
IF ( ASSOCIATED( grid%nmm_prec ) ) THEN 
  DEALLOCATE(grid%nmm_prec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_prec. ')
 endif
  NULLIFY(grid%nmm_prec)
ENDIF
IF ( ASSOCIATED( grid%nmm_aprec ) ) THEN 
  DEALLOCATE(grid%nmm_aprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aprec. ')
 endif
  NULLIFY(grid%nmm_aprec)
ENDIF
IF ( ASSOCIATED( grid%nmm_acprec ) ) THEN 
  DEALLOCATE(grid%nmm_acprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_acprec. ')
 endif
  NULLIFY(grid%nmm_acprec)
ENDIF
IF ( ASSOCIATED( grid%nmm_cuprec ) ) THEN 
  DEALLOCATE(grid%nmm_cuprec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cuprec. ')
 endif
  NULLIFY(grid%nmm_cuprec)
ENDIF
IF ( ASSOCIATED( grid%nmm_lspa ) ) THEN 
  DEALLOCATE(grid%nmm_lspa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_lspa. ')
 endif
  NULLIFY(grid%nmm_lspa)
ENDIF
IF ( ASSOCIATED( grid%nmm_ddata ) ) THEN 
  DEALLOCATE(grid%nmm_ddata,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ddata. ')
 endif
  NULLIFY(grid%nmm_ddata)
ENDIF
IF ( ASSOCIATED( grid%nmm_accliq ) ) THEN 
  DEALLOCATE(grid%nmm_accliq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_accliq. ')
 endif
  NULLIFY(grid%nmm_accliq)
ENDIF
IF ( ASSOCIATED( grid%nmm_sno ) ) THEN 
  DEALLOCATE(grid%nmm_sno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sno. ')
 endif
  NULLIFY(grid%nmm_sno)
ENDIF
IF ( ASSOCIATED( grid%nmm_si ) ) THEN 
  DEALLOCATE(grid%nmm_si,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_si. ')
 endif
  NULLIFY(grid%nmm_si)
ENDIF
IF ( ASSOCIATED( grid%nmm_cldefi ) ) THEN 
  DEALLOCATE(grid%nmm_cldefi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cldefi. ')
 endif
  NULLIFY(grid%nmm_cldefi)
ENDIF
IF ( ASSOCIATED( grid%nmm_deep ) ) THEN 
  DEALLOCATE(grid%nmm_deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_deep. ')
 endif
  NULLIFY(grid%nmm_deep)
ENDIF
IF ( ASSOCIATED( grid%nmm_rf ) ) THEN 
  DEALLOCATE(grid%nmm_rf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rf. ')
 endif
  NULLIFY(grid%nmm_rf)
ENDIF
IF ( ASSOCIATED( grid%nmm_th10 ) ) THEN 
  DEALLOCATE(grid%nmm_th10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_th10. ')
 endif
  NULLIFY(grid%nmm_th10)
ENDIF
IF ( ASSOCIATED( grid%nmm_q10 ) ) THEN 
  DEALLOCATE(grid%nmm_q10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q10. ')
 endif
  NULLIFY(grid%nmm_q10)
ENDIF
IF ( ASSOCIATED( grid%nmm_pshltr ) ) THEN 
  DEALLOCATE(grid%nmm_pshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pshltr. ')
 endif
  NULLIFY(grid%nmm_pshltr)
ENDIF
IF ( ASSOCIATED( grid%nmm_tshltr ) ) THEN 
  DEALLOCATE(grid%nmm_tshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tshltr. ')
 endif
  NULLIFY(grid%nmm_tshltr)
ENDIF
IF ( ASSOCIATED( grid%nmm_qshltr ) ) THEN 
  DEALLOCATE(grid%nmm_qshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_qshltr. ')
 endif
  NULLIFY(grid%nmm_qshltr)
ENDIF
IF ( ASSOCIATED( grid%nmm_q2 ) ) THEN 
  DEALLOCATE(grid%nmm_q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q2. ')
 endif
  NULLIFY(grid%nmm_q2)
ENDIF
IF ( ASSOCIATED( grid%nmm_q2_b ) ) THEN 
  DEALLOCATE(grid%nmm_q2_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q2_b. ')
 endif
  NULLIFY(grid%nmm_q2_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_q2_bt ) ) THEN 
  DEALLOCATE(grid%nmm_q2_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_q2_bt. ')
 endif
  NULLIFY(grid%nmm_q2_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_adj ) ) THEN 
  DEALLOCATE(grid%nmm_t_adj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_adj. ')
 endif
  NULLIFY(grid%nmm_t_adj)
ENDIF
IF ( ASSOCIATED( grid%nmm_t_old ) ) THEN 
  DEALLOCATE(grid%nmm_t_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_t_old. ')
 endif
  NULLIFY(grid%nmm_t_old)
ENDIF
IF ( ASSOCIATED( grid%nmm_zero_3d ) ) THEN 
  DEALLOCATE(grid%nmm_zero_3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_zero_3d. ')
 endif
  NULLIFY(grid%nmm_zero_3d)
ENDIF
IF ( ASSOCIATED( grid%nmm_w0avg ) ) THEN 
  DEALLOCATE(grid%nmm_w0avg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_w0avg. ')
 endif
  NULLIFY(grid%nmm_w0avg)
ENDIF
IF ( ASSOCIATED( grid%nmm_akhs_out ) ) THEN 
  DEALLOCATE(grid%nmm_akhs_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_akhs_out. ')
 endif
  NULLIFY(grid%nmm_akhs_out)
ENDIF
IF ( ASSOCIATED( grid%nmm_akms_out ) ) THEN 
  DEALLOCATE(grid%nmm_akms_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_akms_out. ')
 endif
  NULLIFY(grid%nmm_akms_out)
ENDIF
IF ( ASSOCIATED( grid%nmm_albase ) ) THEN 
  DEALLOCATE(grid%nmm_albase,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_albase. ')
 endif
  NULLIFY(grid%nmm_albase)
ENDIF
IF ( ASSOCIATED( grid%nmm_albedo ) ) THEN 
  DEALLOCATE(grid%nmm_albedo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_albedo. ')
 endif
  NULLIFY(grid%nmm_albedo)
ENDIF
IF ( ASSOCIATED( grid%nmm_cnvbot ) ) THEN 
  DEALLOCATE(grid%nmm_cnvbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cnvbot. ')
 endif
  NULLIFY(grid%nmm_cnvbot)
ENDIF
IF ( ASSOCIATED( grid%nmm_cnvtop ) ) THEN 
  DEALLOCATE(grid%nmm_cnvtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cnvtop. ')
 endif
  NULLIFY(grid%nmm_cnvtop)
ENDIF
IF ( ASSOCIATED( grid%nmm_czen ) ) THEN 
  DEALLOCATE(grid%nmm_czen,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_czen. ')
 endif
  NULLIFY(grid%nmm_czen)
ENDIF
IF ( ASSOCIATED( grid%nmm_czmean ) ) THEN 
  DEALLOCATE(grid%nmm_czmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_czmean. ')
 endif
  NULLIFY(grid%nmm_czmean)
ENDIF
IF ( ASSOCIATED( grid%nmm_epsr ) ) THEN 
  DEALLOCATE(grid%nmm_epsr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_epsr. ')
 endif
  NULLIFY(grid%nmm_epsr)
ENDIF
IF ( ASSOCIATED( grid%nmm_gffc ) ) THEN 
  DEALLOCATE(grid%nmm_gffc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_gffc. ')
 endif
  NULLIFY(grid%nmm_gffc)
ENDIF
IF ( ASSOCIATED( grid%nmm_glat ) ) THEN 
  DEALLOCATE(grid%nmm_glat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_glat. ')
 endif
  NULLIFY(grid%nmm_glat)
ENDIF
IF ( ASSOCIATED( grid%nmm_glon ) ) THEN 
  DEALLOCATE(grid%nmm_glon,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_glon. ')
 endif
  NULLIFY(grid%nmm_glon)
ENDIF
IF ( ASSOCIATED( grid%nmm_nmm_tsk ) ) THEN 
  DEALLOCATE(grid%nmm_nmm_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_nmm_tsk. ')
 endif
  NULLIFY(grid%nmm_nmm_tsk)
ENDIF
IF ( ASSOCIATED( grid%nmm_hdac ) ) THEN 
  DEALLOCATE(grid%nmm_hdac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hdac. ')
 endif
  NULLIFY(grid%nmm_hdac)
ENDIF
IF ( ASSOCIATED( grid%nmm_hdacv ) ) THEN 
  DEALLOCATE(grid%nmm_hdacv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_hdacv. ')
 endif
  NULLIFY(grid%nmm_hdacv)
ENDIF
IF ( ASSOCIATED( grid%nmm_mxsnal ) ) THEN 
  DEALLOCATE(grid%nmm_mxsnal,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_mxsnal. ')
 endif
  NULLIFY(grid%nmm_mxsnal)
ENDIF
IF ( ASSOCIATED( grid%nmm_radin ) ) THEN 
  DEALLOCATE(grid%nmm_radin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_radin. ')
 endif
  NULLIFY(grid%nmm_radin)
ENDIF
IF ( ASSOCIATED( grid%nmm_radot ) ) THEN 
  DEALLOCATE(grid%nmm_radot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_radot. ')
 endif
  NULLIFY(grid%nmm_radot)
ENDIF
IF ( ASSOCIATED( grid%nmm_sigt4 ) ) THEN 
  DEALLOCATE(grid%nmm_sigt4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sigt4. ')
 endif
  NULLIFY(grid%nmm_sigt4)
ENDIF
IF ( ASSOCIATED( grid%nmm_tg ) ) THEN 
  DEALLOCATE(grid%nmm_tg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tg. ')
 endif
  NULLIFY(grid%nmm_tg)
ENDIF
IF ( ASSOCIATED( grid%nmm_dfrlg ) ) THEN 
  DEALLOCATE(grid%nmm_dfrlg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dfrlg. ')
 endif
  NULLIFY(grid%nmm_dfrlg)
ENDIF
IF ( ASSOCIATED( grid%nmm_lvl ) ) THEN 
  DEALLOCATE(grid%nmm_lvl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_lvl. ')
 endif
  NULLIFY(grid%nmm_lvl)
ENDIF
IF ( ASSOCIATED( grid%nmm_cwm ) ) THEN 
  DEALLOCATE(grid%nmm_cwm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cwm. ')
 endif
  NULLIFY(grid%nmm_cwm)
ENDIF
IF ( ASSOCIATED( grid%nmm_cwm_b ) ) THEN 
  DEALLOCATE(grid%nmm_cwm_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cwm_b. ')
 endif
  NULLIFY(grid%nmm_cwm_b)
ENDIF
IF ( ASSOCIATED( grid%nmm_cwm_bt ) ) THEN 
  DEALLOCATE(grid%nmm_cwm_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cwm_bt. ')
 endif
  NULLIFY(grid%nmm_cwm_bt)
ENDIF
IF ( ASSOCIATED( grid%nmm_f_ice ) ) THEN 
  DEALLOCATE(grid%nmm_f_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f_ice. ')
 endif
  NULLIFY(grid%nmm_f_ice)
ENDIF
IF ( ASSOCIATED( grid%nmm_f_rain ) ) THEN 
  DEALLOCATE(grid%nmm_f_rain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f_rain. ')
 endif
  NULLIFY(grid%nmm_f_rain)
ENDIF
IF ( ASSOCIATED( grid%nmm_f_rimef ) ) THEN 
  DEALLOCATE(grid%nmm_f_rimef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_f_rimef. ')
 endif
  NULLIFY(grid%nmm_f_rimef)
ENDIF
IF ( ASSOCIATED( grid%nmm_cldfra ) ) THEN 
  DEALLOCATE(grid%nmm_cldfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cldfra. ')
 endif
  NULLIFY(grid%nmm_cldfra)
ENDIF
IF ( ASSOCIATED( grid%nmm_sr ) ) THEN 
  DEALLOCATE(grid%nmm_sr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sr. ')
 endif
  NULLIFY(grid%nmm_sr)
ENDIF
IF ( ASSOCIATED( grid%nmm_cfrach ) ) THEN 
  DEALLOCATE(grid%nmm_cfrach,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cfrach. ')
 endif
  NULLIFY(grid%nmm_cfrach)
ENDIF
IF ( ASSOCIATED( grid%nmm_cfracl ) ) THEN 
  DEALLOCATE(grid%nmm_cfracl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cfracl. ')
 endif
  NULLIFY(grid%nmm_cfracl)
ENDIF
IF ( ASSOCIATED( grid%nmm_cfracm ) ) THEN 
  DEALLOCATE(grid%nmm_cfracm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cfracm. ')
 endif
  NULLIFY(grid%nmm_cfracm)
ENDIF
IF ( ASSOCIATED( grid%nmm_islope ) ) THEN 
  DEALLOCATE(grid%nmm_islope,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_islope. ')
 endif
  NULLIFY(grid%nmm_islope)
ENDIF
IF ( ASSOCIATED( grid%nmm_dzsoil ) ) THEN 
  DEALLOCATE(grid%nmm_dzsoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dzsoil. ')
 endif
  NULLIFY(grid%nmm_dzsoil)
ENDIF
IF ( ASSOCIATED( grid%nmm_rtdpth ) ) THEN 
  DEALLOCATE(grid%nmm_rtdpth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rtdpth. ')
 endif
  NULLIFY(grid%nmm_rtdpth)
ENDIF
IF ( ASSOCIATED( grid%nmm_sldpth ) ) THEN 
  DEALLOCATE(grid%nmm_sldpth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sldpth. ')
 endif
  NULLIFY(grid%nmm_sldpth)
ENDIF
IF ( ASSOCIATED( grid%nmm_cmc ) ) THEN 
  DEALLOCATE(grid%nmm_cmc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_cmc. ')
 endif
  NULLIFY(grid%nmm_cmc)
ENDIF
IF ( ASSOCIATED( grid%nmm_grnflx ) ) THEN 
  DEALLOCATE(grid%nmm_grnflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_grnflx. ')
 endif
  NULLIFY(grid%nmm_grnflx)
ENDIF
IF ( ASSOCIATED( grid%nmm_pctsno ) ) THEN 
  DEALLOCATE(grid%nmm_pctsno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pctsno. ')
 endif
  NULLIFY(grid%nmm_pctsno)
ENDIF
IF ( ASSOCIATED( grid%nmm_soiltb ) ) THEN 
  DEALLOCATE(grid%nmm_soiltb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_soiltb. ')
 endif
  NULLIFY(grid%nmm_soiltb)
ENDIF
IF ( ASSOCIATED( grid%nmm_vegfrc ) ) THEN 
  DEALLOCATE(grid%nmm_vegfrc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_vegfrc. ')
 endif
  NULLIFY(grid%nmm_vegfrc)
ENDIF
IF ( ASSOCIATED( grid%nmm_shdmin ) ) THEN 
  DEALLOCATE(grid%nmm_shdmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_shdmin. ')
 endif
  NULLIFY(grid%nmm_shdmin)
ENDIF
IF ( ASSOCIATED( grid%nmm_shdmax ) ) THEN 
  DEALLOCATE(grid%nmm_shdmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_shdmax. ')
 endif
  NULLIFY(grid%nmm_shdmax)
ENDIF
IF ( ASSOCIATED( grid%nmm_sh2o ) ) THEN 
  DEALLOCATE(grid%nmm_sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sh2o. ')
 endif
  NULLIFY(grid%nmm_sh2o)
ENDIF
IF ( ASSOCIATED( grid%nmm_smc ) ) THEN 
  DEALLOCATE(grid%nmm_smc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_smc. ')
 endif
  NULLIFY(grid%nmm_smc)
ENDIF
IF ( ASSOCIATED( grid%nmm_stc ) ) THEN 
  DEALLOCATE(grid%nmm_stc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_stc. ')
 endif
  NULLIFY(grid%nmm_stc)
ENDIF
IF ( ASSOCIATED( grid%nmm_dwdtmn ) ) THEN 
  DEALLOCATE(grid%nmm_dwdtmn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dwdtmn. ')
 endif
  NULLIFY(grid%nmm_dwdtmn)
ENDIF
IF ( ASSOCIATED( grid%nmm_dwdtmx ) ) THEN 
  DEALLOCATE(grid%nmm_dwdtmx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dwdtmx. ')
 endif
  NULLIFY(grid%nmm_dwdtmx)
ENDIF
IF ( ASSOCIATED( grid%nmm_dwdt ) ) THEN 
  DEALLOCATE(grid%nmm_dwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_dwdt. ')
 endif
  NULLIFY(grid%nmm_dwdt)
ENDIF
IF ( ASSOCIATED( grid%nmm_pdwdt ) ) THEN 
  DEALLOCATE(grid%nmm_pdwdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pdwdt. ')
 endif
  NULLIFY(grid%nmm_pdwdt)
ENDIF
IF ( ASSOCIATED( grid%nmm_pint ) ) THEN 
  DEALLOCATE(grid%nmm_pint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_pint. ')
 endif
  NULLIFY(grid%nmm_pint)
ENDIF
IF ( ASSOCIATED( grid%nmm_w ) ) THEN 
  DEALLOCATE(grid%nmm_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_w. ')
 endif
  NULLIFY(grid%nmm_w)
ENDIF
IF ( ASSOCIATED( grid%nmm_z ) ) THEN 
  DEALLOCATE(grid%nmm_z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_z. ')
 endif
  NULLIFY(grid%nmm_z)
ENDIF
IF ( ASSOCIATED( grid%nmm_acfrcv ) ) THEN 
  DEALLOCATE(grid%nmm_acfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_acfrcv. ')
 endif
  NULLIFY(grid%nmm_acfrcv)
ENDIF
IF ( ASSOCIATED( grid%nmm_acfrst ) ) THEN 
  DEALLOCATE(grid%nmm_acfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_acfrst. ')
 endif
  NULLIFY(grid%nmm_acfrst)
ENDIF
IF ( ASSOCIATED( grid%nmm_ssroff ) ) THEN 
  DEALLOCATE(grid%nmm_ssroff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ssroff. ')
 endif
  NULLIFY(grid%nmm_ssroff)
ENDIF
IF ( ASSOCIATED( grid%nmm_bgroff ) ) THEN 
  DEALLOCATE(grid%nmm_bgroff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_bgroff. ')
 endif
  NULLIFY(grid%nmm_bgroff)
ENDIF
IF ( ASSOCIATED( grid%nmm_rlwin ) ) THEN 
  DEALLOCATE(grid%nmm_rlwin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rlwin. ')
 endif
  NULLIFY(grid%nmm_rlwin)
ENDIF
IF ( ASSOCIATED( grid%nmm_rlwout ) ) THEN 
  DEALLOCATE(grid%nmm_rlwout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rlwout. ')
 endif
  NULLIFY(grid%nmm_rlwout)
ENDIF
IF ( ASSOCIATED( grid%nmm_rlwtoa ) ) THEN 
  DEALLOCATE(grid%nmm_rlwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rlwtoa. ')
 endif
  NULLIFY(grid%nmm_rlwtoa)
ENDIF
IF ( ASSOCIATED( grid%nmm_alwin ) ) THEN 
  DEALLOCATE(grid%nmm_alwin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_alwin. ')
 endif
  NULLIFY(grid%nmm_alwin)
ENDIF
IF ( ASSOCIATED( grid%nmm_alwout ) ) THEN 
  DEALLOCATE(grid%nmm_alwout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_alwout. ')
 endif
  NULLIFY(grid%nmm_alwout)
ENDIF
IF ( ASSOCIATED( grid%nmm_alwtoa ) ) THEN 
  DEALLOCATE(grid%nmm_alwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_alwtoa. ')
 endif
  NULLIFY(grid%nmm_alwtoa)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswin ) ) THEN 
  DEALLOCATE(grid%nmm_rswin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswin. ')
 endif
  NULLIFY(grid%nmm_rswin)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswinc ) ) THEN 
  DEALLOCATE(grid%nmm_rswinc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswinc. ')
 endif
  NULLIFY(grid%nmm_rswinc)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswout ) ) THEN 
  DEALLOCATE(grid%nmm_rswout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswout. ')
 endif
  NULLIFY(grid%nmm_rswout)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswtoa ) ) THEN 
  DEALLOCATE(grid%nmm_rswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswtoa. ')
 endif
  NULLIFY(grid%nmm_rswtoa)
ENDIF
IF ( ASSOCIATED( grid%nmm_aswin ) ) THEN 
  DEALLOCATE(grid%nmm_aswin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aswin. ')
 endif
  NULLIFY(grid%nmm_aswin)
ENDIF
IF ( ASSOCIATED( grid%nmm_aswout ) ) THEN 
  DEALLOCATE(grid%nmm_aswout,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aswout. ')
 endif
  NULLIFY(grid%nmm_aswout)
ENDIF
IF ( ASSOCIATED( grid%nmm_aswtoa ) ) THEN 
  DEALLOCATE(grid%nmm_aswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_aswtoa. ')
 endif
  NULLIFY(grid%nmm_aswtoa)
ENDIF
IF ( ASSOCIATED( grid%nmm_sfcshx ) ) THEN 
  DEALLOCATE(grid%nmm_sfcshx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sfcshx. ')
 endif
  NULLIFY(grid%nmm_sfcshx)
ENDIF
IF ( ASSOCIATED( grid%nmm_sfclhx ) ) THEN 
  DEALLOCATE(grid%nmm_sfclhx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sfclhx. ')
 endif
  NULLIFY(grid%nmm_sfclhx)
ENDIF
IF ( ASSOCIATED( grid%nmm_subshx ) ) THEN 
  DEALLOCATE(grid%nmm_subshx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_subshx. ')
 endif
  NULLIFY(grid%nmm_subshx)
ENDIF
IF ( ASSOCIATED( grid%nmm_snopcx ) ) THEN 
  DEALLOCATE(grid%nmm_snopcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_snopcx. ')
 endif
  NULLIFY(grid%nmm_snopcx)
ENDIF
IF ( ASSOCIATED( grid%nmm_sfcuvx ) ) THEN 
  DEALLOCATE(grid%nmm_sfcuvx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_sfcuvx. ')
 endif
  NULLIFY(grid%nmm_sfcuvx)
ENDIF
IF ( ASSOCIATED( grid%nmm_potevp ) ) THEN 
  DEALLOCATE(grid%nmm_potevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_potevp. ')
 endif
  NULLIFY(grid%nmm_potevp)
ENDIF
IF ( ASSOCIATED( grid%nmm_potflx ) ) THEN 
  DEALLOCATE(grid%nmm_potflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_potflx. ')
 endif
  NULLIFY(grid%nmm_potflx)
ENDIF
IF ( ASSOCIATED( grid%nmm_tlmin ) ) THEN 
  DEALLOCATE(grid%nmm_tlmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tlmin. ')
 endif
  NULLIFY(grid%nmm_tlmin)
ENDIF
IF ( ASSOCIATED( grid%nmm_tlmax ) ) THEN 
  DEALLOCATE(grid%nmm_tlmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tlmax. ')
 endif
  NULLIFY(grid%nmm_tlmax)
ENDIF
IF ( ASSOCIATED( grid%nmm_rlwtt ) ) THEN 
  DEALLOCATE(grid%nmm_rlwtt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rlwtt. ')
 endif
  NULLIFY(grid%nmm_rlwtt)
ENDIF
IF ( ASSOCIATED( grid%nmm_rswtt ) ) THEN 
  DEALLOCATE(grid%nmm_rswtt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_rswtt. ')
 endif
  NULLIFY(grid%nmm_rswtt)
ENDIF
IF ( ASSOCIATED( grid%nmm_tcucn ) ) THEN 
  DEALLOCATE(grid%nmm_tcucn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_tcucn. ')
 endif
  NULLIFY(grid%nmm_tcucn)
ENDIF
IF ( ASSOCIATED( grid%nmm_train ) ) THEN 
  DEALLOCATE(grid%nmm_train,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_train. ')
 endif
  NULLIFY(grid%nmm_train)
ENDIF
IF ( ASSOCIATED( grid%nmm_ncfrcv ) ) THEN 
  DEALLOCATE(grid%nmm_ncfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ncfrcv. ')
 endif
  NULLIFY(grid%nmm_ncfrcv)
ENDIF
IF ( ASSOCIATED( grid%nmm_ncfrst ) ) THEN 
  DEALLOCATE(grid%nmm_ncfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ncfrst. ')
 endif
  NULLIFY(grid%nmm_ncfrst)
ENDIF
IF ( ASSOCIATED( grid%nmm_ihe ) ) THEN 
  DEALLOCATE(grid%nmm_ihe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ihe. ')
 endif
  NULLIFY(grid%nmm_ihe)
ENDIF
IF ( ASSOCIATED( grid%nmm_ihw ) ) THEN 
  DEALLOCATE(grid%nmm_ihw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ihw. ')
 endif
  NULLIFY(grid%nmm_ihw)
ENDIF
IF ( ASSOCIATED( grid%nmm_ive ) ) THEN 
  DEALLOCATE(grid%nmm_ive,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ive. ')
 endif
  NULLIFY(grid%nmm_ive)
ENDIF
IF ( ASSOCIATED( grid%nmm_ivw ) ) THEN 
  DEALLOCATE(grid%nmm_ivw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ivw. ')
 endif
  NULLIFY(grid%nmm_ivw)
ENDIF
IF ( ASSOCIATED( grid%nmm_irad ) ) THEN 
  DEALLOCATE(grid%nmm_irad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_irad. ')
 endif
  NULLIFY(grid%nmm_irad)
ENDIF
IF ( ASSOCIATED( grid%nmm_iheg ) ) THEN 
  DEALLOCATE(grid%nmm_iheg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iheg. ')
 endif
  NULLIFY(grid%nmm_iheg)
ENDIF
IF ( ASSOCIATED( grid%nmm_ihwg ) ) THEN 
  DEALLOCATE(grid%nmm_ihwg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ihwg. ')
 endif
  NULLIFY(grid%nmm_ihwg)
ENDIF
IF ( ASSOCIATED( grid%nmm_iveg ) ) THEN 
  DEALLOCATE(grid%nmm_iveg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iveg. ')
 endif
  NULLIFY(grid%nmm_iveg)
ENDIF
IF ( ASSOCIATED( grid%nmm_ivwg ) ) THEN 
  DEALLOCATE(grid%nmm_ivwg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_ivwg. ')
 endif
  NULLIFY(grid%nmm_ivwg)
ENDIF
IF ( ASSOCIATED( grid%nmm_iradg ) ) THEN 
  DEALLOCATE(grid%nmm_iradg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iradg. ')
 endif
  NULLIFY(grid%nmm_iradg)
ENDIF
IF ( ASSOCIATED( grid%nmm_n_iup_h ) ) THEN 
  DEALLOCATE(grid%nmm_n_iup_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_n_iup_h. ')
 endif
  NULLIFY(grid%nmm_n_iup_h)
ENDIF
IF ( ASSOCIATED( grid%nmm_n_iup_v ) ) THEN 
  DEALLOCATE(grid%nmm_n_iup_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_n_iup_v. ')
 endif
  NULLIFY(grid%nmm_n_iup_v)
ENDIF
IF ( ASSOCIATED( grid%nmm_n_iup_adh ) ) THEN 
  DEALLOCATE(grid%nmm_n_iup_adh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_n_iup_adh. ')
 endif
  NULLIFY(grid%nmm_n_iup_adh)
ENDIF
IF ( ASSOCIATED( grid%nmm_n_iup_adv ) ) THEN 
  DEALLOCATE(grid%nmm_n_iup_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_n_iup_adv. ')
 endif
  NULLIFY(grid%nmm_n_iup_adv)
ENDIF
IF ( ASSOCIATED( grid%nmm_iup_h ) ) THEN 
  DEALLOCATE(grid%nmm_iup_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iup_h. ')
 endif
  NULLIFY(grid%nmm_iup_h)
ENDIF
IF ( ASSOCIATED( grid%nmm_iup_v ) ) THEN 
  DEALLOCATE(grid%nmm_iup_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iup_v. ')
 endif
  NULLIFY(grid%nmm_iup_v)
ENDIF
IF ( ASSOCIATED( grid%nmm_iup_adh ) ) THEN 
  DEALLOCATE(grid%nmm_iup_adh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iup_adh. ')
 endif
  NULLIFY(grid%nmm_iup_adh)
ENDIF
IF ( ASSOCIATED( grid%nmm_iup_adv ) ) THEN 
  DEALLOCATE(grid%nmm_iup_adv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_iup_adv. ')
 endif
  NULLIFY(grid%nmm_iup_adv)
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
IF ( ASSOCIATED( grid%moist ) ) THEN 
  DEALLOCATE(grid%moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%moist. ')
 endif
  NULLIFY(grid%moist)
ENDIF
IF ( ASSOCIATED( grid%scalar ) ) THEN 
  DEALLOCATE(grid%scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar. ')
 endif
  NULLIFY(grid%scalar)
ENDIF
IF ( ASSOCIATED( grid%scalar_b ) ) THEN 
  DEALLOCATE(grid%scalar_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_b. ')
 endif
  NULLIFY(grid%scalar_b)
ENDIF
IF ( ASSOCIATED( grid%scalar_bt ) ) THEN 
  DEALLOCATE(grid%scalar_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_bt. ')
 endif
  NULLIFY(grid%scalar_bt)
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
IF ( ASSOCIATED( grid%nmm_psfc_out ) ) THEN 
  DEALLOCATE(grid%nmm_psfc_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%nmm_psfc_out. ')
 endif
  NULLIFY(grid%nmm_psfc_out)
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
