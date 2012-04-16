IF ( SIZE(grid%x_1,1)*SIZE(grid%x_1,3) .GT. 1 ) THEN
grid%x_1 (ips:min(ide-1,ipe),:,jms:jme) = grid%x_1 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%x_2,1)*SIZE(grid%x_2,3) .GT. 1 ) THEN
grid%x_2 (ips:min(ide-1,ipe),:,jms:jme) = grid%x_2 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%lu_index,1)*SIZE(grid%lu_index,2) .GT. 1 ) THEN
grid%lu_index (ips:min(ide-1,ipe),jms:jme) = grid%lu_index (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lu_mask,1)*SIZE(grid%lu_mask,2) .GT. 1 ) THEN
grid%lu_mask (ips:min(ide-1,ipe),jms:jme) = grid%lu_mask (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p_gc,1)*SIZE(grid%p_gc,2) .GT. 1 ) THEN
grid%p_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%p_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%vegcat,1)*SIZE(grid%vegcat,2) .GT. 1 ) THEN
grid%vegcat (ips:min(ide-1,ipe),jms:jme) = grid%vegcat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilcat,1)*SIZE(grid%soilcat,2) .GT. 1 ) THEN
grid%soilcat (ips:min(ide-1,ipe),jms:jme) = grid%soilcat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%input_soil_cat,1)*SIZE(grid%input_soil_cat,2) .GT. 1 ) THEN
grid%input_soil_cat (ips:min(ide-1,ipe),jms:jme) = grid%input_soil_cat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tsk_gc,1)*SIZE(grid%tsk_gc,2) .GT. 1 ) THEN
grid%tsk_gc (ips:min(ide-1,ipe),jms:jme) = grid%tsk_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xice_gc,1)*SIZE(grid%xice_gc,2) .GT. 1 ) THEN
grid%xice_gc (ips:min(ide-1,ipe),jms:jme) = grid%xice_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ght_gc,1)*SIZE(grid%ght_gc,2) .GT. 1 ) THEN
grid%ght_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%ght_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rh_gc,1)*SIZE(grid%rh_gc,2) .GT. 1 ) THEN
grid%rh_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%rh_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%v_gc,1)*SIZE(grid%v_gc,2) .GT. 1 ) THEN
grid%v_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%v_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%u_gc,1)*SIZE(grid%u_gc,2) .GT. 1 ) THEN
grid%u_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%u_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%t_gc,1)*SIZE(grid%t_gc,2) .GT. 1 ) THEN
grid%t_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%t_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%snoalb,1)*SIZE(grid%snoalb,2) .GT. 1 ) THEN
grid%snoalb (ips:min(ide-1,ipe),jms:jme) = grid%snoalb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%greenfrac_gc,1)*SIZE(grid%greenfrac_gc,2) .GT. 1 ) THEN
grid%greenfrac_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%greenfrac_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%albedo12m_gc,1)*SIZE(grid%albedo12m_gc,2) .GT. 1 ) THEN
grid%albedo12m_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%albedo12m_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%soilcbot_gc,1)*SIZE(grid%soilcbot_gc,2) .GT. 1 ) THEN
grid%soilcbot_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%soilcbot_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%soilctop_gc,1)*SIZE(grid%soilctop_gc,2) .GT. 1 ) THEN
grid%soilctop_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%soilctop_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%tmn_gc,1)*SIZE(grid%tmn_gc,2) .GT. 1 ) THEN
grid%tmn_gc (ips:min(ide-1,ipe),jms:jme) = grid%tmn_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htv_gc,1)*SIZE(grid%htv_gc,2) .GT. 1 ) THEN
grid%htv_gc (ips:min(ide-1,ipe),jms:jme) = grid%htv_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ht_gc,1)*SIZE(grid%ht_gc,2) .GT. 1 ) THEN
grid%ht_gc (ips:min(ide-1,ipe),jms:jme) = grid%ht_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%landusef_gc,1)*SIZE(grid%landusef_gc,2) .GT. 1 ) THEN
grid%landusef_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%landusef_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%vlon_gc,1)*SIZE(grid%vlon_gc,2) .GT. 1 ) THEN
grid%vlon_gc (ips:min(ide-1,ipe),jms:jme) = grid%vlon_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vlat_gc,1)*SIZE(grid%vlat_gc,2) .GT. 1 ) THEN
grid%vlat_gc (ips:min(ide-1,ipe),jms:jme) = grid%vlat_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlon_gc,1)*SIZE(grid%hlon_gc,2) .GT. 1 ) THEN
grid%hlon_gc (ips:min(ide-1,ipe),jms:jme) = grid%hlon_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlat_gc,1)*SIZE(grid%hlat_gc,2) .GT. 1 ) THEN
grid%hlat_gc (ips:min(ide-1,ipe),jms:jme) = grid%hlat_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbm2,1)*SIZE(grid%hbm2,2) .GT. 1 ) THEN
grid%hbm2 (ips:min(ide-1,ipe),jms:jme) = grid%hbm2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbm3,1)*SIZE(grid%hbm3,2) .GT. 1 ) THEN
grid%hbm3 (ips:min(ide-1,ipe),jms:jme) = grid%hbm3 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbm2,1)*SIZE(grid%vbm2,2) .GT. 1 ) THEN
grid%vbm2 (ips:min(ide-1,ipe),jms:jme) = grid%vbm2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbm3,1)*SIZE(grid%vbm3,2) .GT. 1 ) THEN
grid%vbm3 (ips:min(ide-1,ipe),jms:jme) = grid%vbm3 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm,1)*SIZE(grid%sm,2) .GT. 1 ) THEN
grid%sm (ips:min(ide-1,ipe),jms:jme) = grid%sm (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sice,1)*SIZE(grid%sice,2) .GT. 1 ) THEN
grid%sice (ips:min(ide-1,ipe),jms:jme) = grid%sice (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pd,1)*SIZE(grid%pd,2) .GT. 1 ) THEN
grid%pd (ips:min(ide-1,ipe),jms:jme) = grid%pd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fis,1)*SIZE(grid%fis,2) .GT. 1 ) THEN
grid%fis (ips:min(ide-1,ipe),jms:jme) = grid%fis (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%res,1)*SIZE(grid%res,2) .GT. 1 ) THEN
grid%res (ips:min(ide-1,ipe),jms:jme) = grid%res (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t,1)*SIZE(grid%t,2) .GT. 1 ) THEN
grid%t (ips:min(ide-1,ipe),jms:jme,:) = grid%t (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%q,1)*SIZE(grid%q,2) .GT. 1 ) THEN
grid%q (ips:min(ide-1,ipe),jms:jme,:) = grid%q (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%u,1)*SIZE(grid%u,2) .GT. 1 ) THEN
grid%u (ips:min(ide-1,ipe),jms:jme,:) = grid%u (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%v,1)*SIZE(grid%v,2) .GT. 1 ) THEN
grid%v (ips:min(ide-1,ipe),jms:jme,:) = grid%v (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%told,1)*SIZE(grid%told,2) .GT. 1 ) THEN
grid%told (ips:min(ide-1,ipe),jms:jme,:) = grid%told (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%uold,1)*SIZE(grid%uold,2) .GT. 1 ) THEN
grid%uold (ips:min(ide-1,ipe),jms:jme,:) = grid%uold (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%vold,1)*SIZE(grid%vold,2) .GT. 1 ) THEN
grid%vold (ips:min(ide-1,ipe),jms:jme,:) = grid%vold (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_pd,1)*SIZE(grid%dfi_pd,2) .GT. 1 ) THEN
grid%dfi_pd (ips:min(ide-1,ipe),jms:jme) = grid%dfi_pd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_pint,1)*SIZE(grid%dfi_pint,2) .GT. 1 ) THEN
grid%dfi_pint (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_pint (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_dwdt,1)*SIZE(grid%dfi_dwdt,2) .GT. 1 ) THEN
grid%dfi_dwdt (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_dwdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_t,1)*SIZE(grid%dfi_t,2) .GT. 1 ) THEN
grid%dfi_t (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_t (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_q,1)*SIZE(grid%dfi_q,2) .GT. 1 ) THEN
grid%dfi_q (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_q (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_u,1)*SIZE(grid%dfi_u,2) .GT. 1 ) THEN
grid%dfi_u (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_u (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_v,1)*SIZE(grid%dfi_v,2) .GT. 1 ) THEN
grid%dfi_v (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_v (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_q2,1)*SIZE(grid%dfi_q2,2) .GT. 1 ) THEN
grid%dfi_q2 (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_q2 (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_cwm,1)*SIZE(grid%dfi_cwm,2) .GT. 1 ) THEN
grid%dfi_cwm (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_cwm (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_rrw,1)*SIZE(grid%dfi_rrw,2) .GT. 1 ) THEN
grid%dfi_rrw (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_rrw (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_stc,1)*SIZE(grid%dfi_stc,3) .GT. 1 ) THEN
grid%dfi_stc (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_stc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_smc,1)*SIZE(grid%dfi_smc,3) .GT. 1 ) THEN
grid%dfi_smc (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_smc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_sh2o,1)*SIZE(grid%dfi_sh2o,3) .GT. 1 ) THEN
grid%dfi_sh2o (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_sh2o (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_snow,1)*SIZE(grid%dfi_snow,2) .GT. 1 ) THEN
grid%dfi_snow (ips:min(ide-1,ipe),jms:jme) = grid%dfi_snow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_snowh,1)*SIZE(grid%dfi_snowh,2) .GT. 1 ) THEN
grid%dfi_snowh (ips:min(ide-1,ipe),jms:jme) = grid%dfi_snowh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_canwat,1)*SIZE(grid%dfi_canwat,2) .GT. 1 ) THEN
grid%dfi_canwat (ips:min(ide-1,ipe),jms:jme) = grid%dfi_canwat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_nmm_tsk,1)*SIZE(grid%dfi_nmm_tsk,2) .GT. 1 ) THEN
grid%dfi_nmm_tsk (ips:min(ide-1,ipe),jms:jme) = grid%dfi_nmm_tsk (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_snowc,1)*SIZE(grid%dfi_snowc,2) .GT. 1 ) THEN
grid%dfi_snowc (ips:min(ide-1,ipe),jms:jme) = grid%dfi_snowc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dx_nmm,1)*SIZE(grid%dx_nmm,2) .GT. 1 ) THEN
grid%dx_nmm (ips:min(ide-1,ipe),jms:jme) = grid%dx_nmm (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%wpdar,1)*SIZE(grid%wpdar,2) .GT. 1 ) THEN
grid%wpdar (ips:min(ide-1,ipe),jms:jme) = grid%wpdar (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cpgfu,1)*SIZE(grid%cpgfu,2) .GT. 1 ) THEN
grid%cpgfu (ips:min(ide-1,ipe),jms:jme) = grid%cpgfu (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%curv,1)*SIZE(grid%curv,2) .GT. 1 ) THEN
grid%curv (ips:min(ide-1,ipe),jms:jme) = grid%curv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fcp,1)*SIZE(grid%fcp,2) .GT. 1 ) THEN
grid%fcp (ips:min(ide-1,ipe),jms:jme) = grid%fcp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fdiv,1)*SIZE(grid%fdiv,2) .GT. 1 ) THEN
grid%fdiv (ips:min(ide-1,ipe),jms:jme) = grid%fdiv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%f,1)*SIZE(grid%f,2) .GT. 1 ) THEN
grid%f (ips:min(ide-1,ipe),jms:jme) = grid%f (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fad,1)*SIZE(grid%fad,2) .GT. 1 ) THEN
grid%fad (ips:min(ide-1,ipe),jms:jme) = grid%fad (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ddmpu,1)*SIZE(grid%ddmpu,2) .GT. 1 ) THEN
grid%ddmpu (ips:min(ide-1,ipe),jms:jme) = grid%ddmpu (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ddmpv,1)*SIZE(grid%ddmpv,2) .GT. 1 ) THEN
grid%ddmpv (ips:min(ide-1,ipe),jms:jme) = grid%ddmpv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%adt,1)*SIZE(grid%adt,2) .GT. 1 ) THEN
grid%adt (ips:min(ide-1,ipe),jms:jme) = grid%adt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%adu,1)*SIZE(grid%adu,2) .GT. 1 ) THEN
grid%adu (ips:min(ide-1,ipe),jms:jme) = grid%adu (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%adv,1)*SIZE(grid%adv,2) .GT. 1 ) THEN
grid%adv (ips:min(ide-1,ipe),jms:jme) = grid%adv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pdsl,1)*SIZE(grid%pdsl,2) .GT. 1 ) THEN
grid%pdsl (ips:min(ide-1,ipe),jms:jme) = grid%pdsl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pdslo,1)*SIZE(grid%pdslo,2) .GT. 1 ) THEN
grid%pdslo (ips:min(ide-1,ipe),jms:jme) = grid%pdslo (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psdt,1)*SIZE(grid%psdt,2) .GT. 1 ) THEN
grid%psdt (ips:min(ide-1,ipe),jms:jme) = grid%psdt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,2) .GT. 1 ) THEN
grid%div (ips:min(ide-1,ipe),jms:jme,:) = grid%div (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%few,1)*SIZE(grid%few,2) .GT. 1 ) THEN
grid%few (ips:min(ide-1,ipe),jms:jme,:) = grid%few (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%fne,1)*SIZE(grid%fne,2) .GT. 1 ) THEN
grid%fne (ips:min(ide-1,ipe),jms:jme,:) = grid%fne (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%fns,1)*SIZE(grid%fns,2) .GT. 1 ) THEN
grid%fns (ips:min(ide-1,ipe),jms:jme,:) = grid%fns (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%fse,1)*SIZE(grid%fse,2) .GT. 1 ) THEN
grid%fse (ips:min(ide-1,ipe),jms:jme,:) = grid%fse (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%omgalf,1)*SIZE(grid%omgalf,2) .GT. 1 ) THEN
grid%omgalf (ips:min(ide-1,ipe),jms:jme,:) = grid%omgalf (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%petdt,1)*SIZE(grid%petdt,2) .GT. 1 ) THEN
grid%petdt (ips:min(ide-1,ipe),jms:jme,:) = grid%petdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rtop,1)*SIZE(grid%rtop,2) .GT. 1 ) THEN
grid%rtop (ips:min(ide-1,ipe),jms:jme,:) = grid%rtop (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%pblh,1)*SIZE(grid%pblh,2) .GT. 1 ) THEN
grid%pblh (ips:min(ide-1,ipe),jms:jme) = grid%pblh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lpbl,1)*SIZE(grid%lpbl,2) .GT. 1 ) THEN
grid%lpbl (ips:min(ide-1,ipe),jms:jme) = grid%lpbl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mixht,1)*SIZE(grid%mixht,2) .GT. 1 ) THEN
grid%mixht (ips:min(ide-1,ipe),jms:jme) = grid%mixht (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ustar,1)*SIZE(grid%ustar,2) .GT. 1 ) THEN
grid%ustar (ips:min(ide-1,ipe),jms:jme) = grid%ustar (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%z0,1)*SIZE(grid%z0,2) .GT. 1 ) THEN
grid%z0 (ips:min(ide-1,ipe),jms:jme) = grid%z0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%z0base,1)*SIZE(grid%z0base,2) .GT. 1 ) THEN
grid%z0base (ips:min(ide-1,ipe),jms:jme) = grid%z0base (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ths,1)*SIZE(grid%ths,2) .GT. 1 ) THEN
grid%ths (ips:min(ide-1,ipe),jms:jme) = grid%ths (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mavail,1)*SIZE(grid%mavail,2) .GT. 1 ) THEN
grid%mavail (ips:min(ide-1,ipe),jms:jme) = grid%mavail (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qsh,1)*SIZE(grid%qsh,2) .GT. 1 ) THEN
grid%qsh (ips:min(ide-1,ipe),jms:jme) = grid%qsh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%twbs,1)*SIZE(grid%twbs,2) .GT. 1 ) THEN
grid%twbs (ips:min(ide-1,ipe),jms:jme) = grid%twbs (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qwbs,1)*SIZE(grid%qwbs,2) .GT. 1 ) THEN
grid%qwbs (ips:min(ide-1,ipe),jms:jme) = grid%qwbs (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%taux,1)*SIZE(grid%taux,2) .GT. 1 ) THEN
grid%taux (ips:min(ide-1,ipe),jms:jme) = grid%taux (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tauy,1)*SIZE(grid%tauy,2) .GT. 1 ) THEN
grid%tauy (ips:min(ide-1,ipe),jms:jme) = grid%tauy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%prec,1)*SIZE(grid%prec,2) .GT. 1 ) THEN
grid%prec (ips:min(ide-1,ipe),jms:jme) = grid%prec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aprec,1)*SIZE(grid%aprec,2) .GT. 1 ) THEN
grid%aprec (ips:min(ide-1,ipe),jms:jme) = grid%aprec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acprec,1)*SIZE(grid%acprec,2) .GT. 1 ) THEN
grid%acprec (ips:min(ide-1,ipe),jms:jme) = grid%acprec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cuprec,1)*SIZE(grid%cuprec,2) .GT. 1 ) THEN
grid%cuprec (ips:min(ide-1,ipe),jms:jme) = grid%cuprec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lspa,1)*SIZE(grid%lspa,2) .GT. 1 ) THEN
grid%lspa (ips:min(ide-1,ipe),jms:jme) = grid%lspa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ddata,1)*SIZE(grid%ddata,2) .GT. 1 ) THEN
grid%ddata (ips:min(ide-1,ipe),jms:jme) = grid%ddata (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%accliq,1)*SIZE(grid%accliq,2) .GT. 1 ) THEN
grid%accliq (ips:min(ide-1,ipe),jms:jme) = grid%accliq (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sno,1)*SIZE(grid%sno,2) .GT. 1 ) THEN
grid%sno (ips:min(ide-1,ipe),jms:jme) = grid%sno (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%si,1)*SIZE(grid%si,2) .GT. 1 ) THEN
grid%si (ips:min(ide-1,ipe),jms:jme) = grid%si (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cldefi,1)*SIZE(grid%cldefi,2) .GT. 1 ) THEN
grid%cldefi (ips:min(ide-1,ipe),jms:jme) = grid%cldefi (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%deep,1)*SIZE(grid%deep,2) .GT. 1 ) THEN
grid%deep (ips:min(ide-1,ipe),jms:jme) = grid%deep (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rf,1)*SIZE(grid%rf,2) .GT. 1 ) THEN
grid%rf (ips:min(ide-1,ipe),jms:jme) = grid%rf (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%th10,1)*SIZE(grid%th10,2) .GT. 1 ) THEN
grid%th10 (ips:min(ide-1,ipe),jms:jme) = grid%th10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%q10,1)*SIZE(grid%q10,2) .GT. 1 ) THEN
grid%q10 (ips:min(ide-1,ipe),jms:jme) = grid%q10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pshltr,1)*SIZE(grid%pshltr,2) .GT. 1 ) THEN
grid%pshltr (ips:min(ide-1,ipe),jms:jme) = grid%pshltr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tshltr,1)*SIZE(grid%tshltr,2) .GT. 1 ) THEN
grid%tshltr (ips:min(ide-1,ipe),jms:jme) = grid%tshltr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qshltr,1)*SIZE(grid%qshltr,2) .GT. 1 ) THEN
grid%qshltr (ips:min(ide-1,ipe),jms:jme) = grid%qshltr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%q2,1)*SIZE(grid%q2,2) .GT. 1 ) THEN
grid%q2 (ips:min(ide-1,ipe),jms:jme,:) = grid%q2 (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%t_adj,1)*SIZE(grid%t_adj,2) .GT. 1 ) THEN
grid%t_adj (ips:min(ide-1,ipe),jms:jme,:) = grid%t_adj (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%t_old,1)*SIZE(grid%t_old,2) .GT. 1 ) THEN
grid%t_old (ips:min(ide-1,ipe),jms:jme,:) = grid%t_old (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%zero_3d,1)*SIZE(grid%zero_3d,2) .GT. 1 ) THEN
grid%zero_3d (ips:min(ide-1,ipe),jms:jme,:) = grid%zero_3d (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%w0avg,1)*SIZE(grid%w0avg,3) .GT. 1 ) THEN
grid%w0avg (ips:min(ide-1,ipe),:,jms:jme) = grid%w0avg (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%akhs_out,1)*SIZE(grid%akhs_out,2) .GT. 1 ) THEN
grid%akhs_out (ips:min(ide-1,ipe),jms:jme) = grid%akhs_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%akms_out,1)*SIZE(grid%akms_out,2) .GT. 1 ) THEN
grid%akms_out (ips:min(ide-1,ipe),jms:jme) = grid%akms_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albase,1)*SIZE(grid%albase,2) .GT. 1 ) THEN
grid%albase (ips:min(ide-1,ipe),jms:jme) = grid%albase (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albedo,1)*SIZE(grid%albedo,2) .GT. 1 ) THEN
grid%albedo (ips:min(ide-1,ipe),jms:jme) = grid%albedo (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cnvbot,1)*SIZE(grid%cnvbot,2) .GT. 1 ) THEN
grid%cnvbot (ips:min(ide-1,ipe),jms:jme) = grid%cnvbot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cnvtop,1)*SIZE(grid%cnvtop,2) .GT. 1 ) THEN
grid%cnvtop (ips:min(ide-1,ipe),jms:jme) = grid%cnvtop (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%czen,1)*SIZE(grid%czen,2) .GT. 1 ) THEN
grid%czen (ips:min(ide-1,ipe),jms:jme) = grid%czen (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%czmean,1)*SIZE(grid%czmean,2) .GT. 1 ) THEN
grid%czmean (ips:min(ide-1,ipe),jms:jme) = grid%czmean (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%embck,1)*SIZE(grid%embck,2) .GT. 1 ) THEN
grid%embck (ips:min(ide-1,ipe),jms:jme) = grid%embck (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%epsr,1)*SIZE(grid%epsr,2) .GT. 1 ) THEN
grid%epsr (ips:min(ide-1,ipe),jms:jme) = grid%epsr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%gffc,1)*SIZE(grid%gffc,2) .GT. 1 ) THEN
grid%gffc (ips:min(ide-1,ipe),jms:jme) = grid%gffc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%glat,1)*SIZE(grid%glat,2) .GT. 1 ) THEN
grid%glat (ips:min(ide-1,ipe),jms:jme) = grid%glat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%glon,1)*SIZE(grid%glon,2) .GT. 1 ) THEN
grid%glon (ips:min(ide-1,ipe),jms:jme) = grid%glon (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%nmm_tsk,1)*SIZE(grid%nmm_tsk,2) .GT. 1 ) THEN
grid%nmm_tsk (ips:min(ide-1,ipe),jms:jme) = grid%nmm_tsk (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hdac,1)*SIZE(grid%hdac,2) .GT. 1 ) THEN
grid%hdac (ips:min(ide-1,ipe),jms:jme) = grid%hdac (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hdacv,1)*SIZE(grid%hdacv,2) .GT. 1 ) THEN
grid%hdacv (ips:min(ide-1,ipe),jms:jme) = grid%hdacv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mxsnal,1)*SIZE(grid%mxsnal,2) .GT. 1 ) THEN
grid%mxsnal (ips:min(ide-1,ipe),jms:jme) = grid%mxsnal (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%radin,1)*SIZE(grid%radin,2) .GT. 1 ) THEN
grid%radin (ips:min(ide-1,ipe),jms:jme) = grid%radin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%radot,1)*SIZE(grid%radot,2) .GT. 1 ) THEN
grid%radot (ips:min(ide-1,ipe),jms:jme) = grid%radot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sigt4,1)*SIZE(grid%sigt4,2) .GT. 1 ) THEN
grid%sigt4 (ips:min(ide-1,ipe),jms:jme) = grid%sigt4 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg,1)*SIZE(grid%tg,2) .GT. 1 ) THEN
grid%tg (ips:min(ide-1,ipe),jms:jme) = grid%tg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lvl,1)*SIZE(grid%lvl,2) .GT. 1 ) THEN
grid%lvl (ips:min(ide-1,ipe),jms:jme) = grid%lvl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cwm,1)*SIZE(grid%cwm,2) .GT. 1 ) THEN
grid%cwm (ips:min(ide-1,ipe),jms:jme,:) = grid%cwm (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rrw,1)*SIZE(grid%rrw,2) .GT. 1 ) THEN
grid%rrw (ips:min(ide-1,ipe),jms:jme,:) = grid%rrw (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%f_ice,1)*SIZE(grid%f_ice,3) .GT. 1 ) THEN
grid%f_ice (ips:min(ide-1,ipe),:,jms:jme) = grid%f_ice (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rain,1)*SIZE(grid%f_rain,3) .GT. 1 ) THEN
grid%f_rain (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rain (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rimef,1)*SIZE(grid%f_rimef,3) .GT. 1 ) THEN
grid%f_rimef (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rimef (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%cldfra,1)*SIZE(grid%cldfra,2) .GT. 1 ) THEN
grid%cldfra (ips:min(ide-1,ipe),jms:jme,:) = grid%cldfra (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%sr,1)*SIZE(grid%sr,2) .GT. 1 ) THEN
grid%sr (ips:min(ide-1,ipe),jms:jme) = grid%sr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cfrach,1)*SIZE(grid%cfrach,2) .GT. 1 ) THEN
grid%cfrach (ips:min(ide-1,ipe),jms:jme) = grid%cfrach (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cfracl,1)*SIZE(grid%cfracl,2) .GT. 1 ) THEN
grid%cfracl (ips:min(ide-1,ipe),jms:jme) = grid%cfracl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cfracm,1)*SIZE(grid%cfracm,2) .GT. 1 ) THEN
grid%cfracm (ips:min(ide-1,ipe),jms:jme) = grid%cfracm (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%islope,1)*SIZE(grid%islope,2) .GT. 1 ) THEN
grid%islope (ips:min(ide-1,ipe),jms:jme) = grid%islope (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cmc,1)*SIZE(grid%cmc,2) .GT. 1 ) THEN
grid%cmc (ips:min(ide-1,ipe),jms:jme) = grid%cmc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%grnflx,1)*SIZE(grid%grnflx,2) .GT. 1 ) THEN
grid%grnflx (ips:min(ide-1,ipe),jms:jme) = grid%grnflx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pctsno,1)*SIZE(grid%pctsno,2) .GT. 1 ) THEN
grid%pctsno (ips:min(ide-1,ipe),jms:jme) = grid%pctsno (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soiltb,1)*SIZE(grid%soiltb,2) .GT. 1 ) THEN
grid%soiltb (ips:min(ide-1,ipe),jms:jme) = grid%soiltb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vegfrc,1)*SIZE(grid%vegfrc,2) .GT. 1 ) THEN
grid%vegfrc (ips:min(ide-1,ipe),jms:jme) = grid%vegfrc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%shdmin,1)*SIZE(grid%shdmin,2) .GT. 1 ) THEN
grid%shdmin (ips:min(ide-1,ipe),jms:jme) = grid%shdmin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%shdmax,1)*SIZE(grid%shdmax,2) .GT. 1 ) THEN
grid%shdmax (ips:min(ide-1,ipe),jms:jme) = grid%shdmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sh2o,1)*SIZE(grid%sh2o,3) .GT. 1 ) THEN
grid%sh2o (ips:min(ide-1,ipe),:,jms:jme) = grid%sh2o (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%smc,1)*SIZE(grid%smc,3) .GT. 1 ) THEN
grid%smc (ips:min(ide-1,ipe),:,jms:jme) = grid%smc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%stc,1)*SIZE(grid%stc,3) .GT. 1 ) THEN
grid%stc (ips:min(ide-1,ipe),:,jms:jme) = grid%stc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%hstdv,1)*SIZE(grid%hstdv,2) .GT. 1 ) THEN
grid%hstdv (ips:min(ide-1,ipe),jms:jme) = grid%hstdv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hcnvx,1)*SIZE(grid%hcnvx,2) .GT. 1 ) THEN
grid%hcnvx (ips:min(ide-1,ipe),jms:jme) = grid%hcnvx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasyw,1)*SIZE(grid%hasyw,2) .GT. 1 ) THEN
grid%hasyw (ips:min(ide-1,ipe),jms:jme) = grid%hasyw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasys,1)*SIZE(grid%hasys,2) .GT. 1 ) THEN
grid%hasys (ips:min(ide-1,ipe),jms:jme) = grid%hasys (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasysw,1)*SIZE(grid%hasysw,2) .GT. 1 ) THEN
grid%hasysw (ips:min(ide-1,ipe),jms:jme) = grid%hasysw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasynw,1)*SIZE(grid%hasynw,2) .GT. 1 ) THEN
grid%hasynw (ips:min(ide-1,ipe),jms:jme) = grid%hasynw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlenw,1)*SIZE(grid%hlenw,2) .GT. 1 ) THEN
grid%hlenw (ips:min(ide-1,ipe),jms:jme) = grid%hlenw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlens,1)*SIZE(grid%hlens,2) .GT. 1 ) THEN
grid%hlens (ips:min(ide-1,ipe),jms:jme) = grid%hlens (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlensw,1)*SIZE(grid%hlensw,2) .GT. 1 ) THEN
grid%hlensw (ips:min(ide-1,ipe),jms:jme) = grid%hlensw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlennw,1)*SIZE(grid%hlennw,2) .GT. 1 ) THEN
grid%hlennw (ips:min(ide-1,ipe),jms:jme) = grid%hlennw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hangl,1)*SIZE(grid%hangl,2) .GT. 1 ) THEN
grid%hangl (ips:min(ide-1,ipe),jms:jme) = grid%hangl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hanis,1)*SIZE(grid%hanis,2) .GT. 1 ) THEN
grid%hanis (ips:min(ide-1,ipe),jms:jme) = grid%hanis (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hslop,1)*SIZE(grid%hslop,2) .GT. 1 ) THEN
grid%hslop (ips:min(ide-1,ipe),jms:jme) = grid%hslop (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hzmax,1)*SIZE(grid%hzmax,2) .GT. 1 ) THEN
grid%hzmax (ips:min(ide-1,ipe),jms:jme) = grid%hzmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%crot,1)*SIZE(grid%crot,2) .GT. 1 ) THEN
grid%crot (ips:min(ide-1,ipe),jms:jme) = grid%crot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%srot,1)*SIZE(grid%srot,2) .GT. 1 ) THEN
grid%srot (ips:min(ide-1,ipe),jms:jme) = grid%srot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ugwdsfc,1)*SIZE(grid%ugwdsfc,2) .GT. 1 ) THEN
grid%ugwdsfc (ips:min(ide-1,ipe),jms:jme) = grid%ugwdsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vgwdsfc,1)*SIZE(grid%vgwdsfc,2) .GT. 1 ) THEN
grid%vgwdsfc (ips:min(ide-1,ipe),jms:jme) = grid%vgwdsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dwdtmn,1)*SIZE(grid%dwdtmn,2) .GT. 1 ) THEN
grid%dwdtmn (ips:min(ide-1,ipe),jms:jme) = grid%dwdtmn (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dwdtmx,1)*SIZE(grid%dwdtmx,2) .GT. 1 ) THEN
grid%dwdtmx (ips:min(ide-1,ipe),jms:jme) = grid%dwdtmx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dwdt,1)*SIZE(grid%dwdt,2) .GT. 1 ) THEN
grid%dwdt (ips:min(ide-1,ipe),jms:jme,:) = grid%dwdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%pdwdt,1)*SIZE(grid%pdwdt,2) .GT. 1 ) THEN
grid%pdwdt (ips:min(ide-1,ipe),jms:jme,:) = grid%pdwdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%pint,1)*SIZE(grid%pint,2) .GT. 1 ) THEN
grid%pint (ips:min(ide-1,ipe),jms:jme,:) = grid%pint (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%w,1)*SIZE(grid%w,2) .GT. 1 ) THEN
grid%w (ips:min(ide-1,ipe),jms:jme,:) = grid%w (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%z,1)*SIZE(grid%z,2) .GT. 1 ) THEN
grid%z (ips:min(ide-1,ipe),jms:jme,:) = grid%z (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%acfrcv,1)*SIZE(grid%acfrcv,2) .GT. 1 ) THEN
grid%acfrcv (ips:min(ide-1,ipe),jms:jme) = grid%acfrcv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acfrst,1)*SIZE(grid%acfrst,2) .GT. 1 ) THEN
grid%acfrst (ips:min(ide-1,ipe),jms:jme) = grid%acfrst (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ssroff,1)*SIZE(grid%ssroff,2) .GT. 1 ) THEN
grid%ssroff (ips:min(ide-1,ipe),jms:jme) = grid%ssroff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%bgroff,1)*SIZE(grid%bgroff,2) .GT. 1 ) THEN
grid%bgroff (ips:min(ide-1,ipe),jms:jme) = grid%bgroff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwin,1)*SIZE(grid%rlwin,2) .GT. 1 ) THEN
grid%rlwin (ips:min(ide-1,ipe),jms:jme) = grid%rlwin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwout,1)*SIZE(grid%rlwout,2) .GT. 1 ) THEN
grid%rlwout (ips:min(ide-1,ipe),jms:jme) = grid%rlwout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwtoa,1)*SIZE(grid%rlwtoa,2) .GT. 1 ) THEN
grid%rlwtoa (ips:min(ide-1,ipe),jms:jme) = grid%rlwtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alwin,1)*SIZE(grid%alwin,2) .GT. 1 ) THEN
grid%alwin (ips:min(ide-1,ipe),jms:jme) = grid%alwin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alwout,1)*SIZE(grid%alwout,2) .GT. 1 ) THEN
grid%alwout (ips:min(ide-1,ipe),jms:jme) = grid%alwout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alwtoa,1)*SIZE(grid%alwtoa,2) .GT. 1 ) THEN
grid%alwtoa (ips:min(ide-1,ipe),jms:jme) = grid%alwtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswin,1)*SIZE(grid%rswin,2) .GT. 1 ) THEN
grid%rswin (ips:min(ide-1,ipe),jms:jme) = grid%rswin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswinc,1)*SIZE(grid%rswinc,2) .GT. 1 ) THEN
grid%rswinc (ips:min(ide-1,ipe),jms:jme) = grid%rswinc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswout,1)*SIZE(grid%rswout,2) .GT. 1 ) THEN
grid%rswout (ips:min(ide-1,ipe),jms:jme) = grid%rswout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswtoa,1)*SIZE(grid%rswtoa,2) .GT. 1 ) THEN
grid%rswtoa (ips:min(ide-1,ipe),jms:jme) = grid%rswtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aswin,1)*SIZE(grid%aswin,2) .GT. 1 ) THEN
grid%aswin (ips:min(ide-1,ipe),jms:jme) = grid%aswin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aswout,1)*SIZE(grid%aswout,2) .GT. 1 ) THEN
grid%aswout (ips:min(ide-1,ipe),jms:jme) = grid%aswout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aswtoa,1)*SIZE(grid%aswtoa,2) .GT. 1 ) THEN
grid%aswtoa (ips:min(ide-1,ipe),jms:jme) = grid%aswtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcshx,1)*SIZE(grid%sfcshx,2) .GT. 1 ) THEN
grid%sfcshx (ips:min(ide-1,ipe),jms:jme) = grid%sfcshx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfclhx,1)*SIZE(grid%sfclhx,2) .GT. 1 ) THEN
grid%sfclhx (ips:min(ide-1,ipe),jms:jme) = grid%sfclhx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%subshx,1)*SIZE(grid%subshx,2) .GT. 1 ) THEN
grid%subshx (ips:min(ide-1,ipe),jms:jme) = grid%subshx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snopcx,1)*SIZE(grid%snopcx,2) .GT. 1 ) THEN
grid%snopcx (ips:min(ide-1,ipe),jms:jme) = grid%snopcx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcuvx,1)*SIZE(grid%sfcuvx,2) .GT. 1 ) THEN
grid%sfcuvx (ips:min(ide-1,ipe),jms:jme) = grid%sfcuvx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%potevp,1)*SIZE(grid%potevp,2) .GT. 1 ) THEN
grid%potevp (ips:min(ide-1,ipe),jms:jme) = grid%potevp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%potflx,1)*SIZE(grid%potflx,2) .GT. 1 ) THEN
grid%potflx (ips:min(ide-1,ipe),jms:jme) = grid%potflx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tlmin,1)*SIZE(grid%tlmin,2) .GT. 1 ) THEN
grid%tlmin (ips:min(ide-1,ipe),jms:jme) = grid%tlmin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tlmax,1)*SIZE(grid%tlmax,2) .GT. 1 ) THEN
grid%tlmax (ips:min(ide-1,ipe),jms:jme) = grid%tlmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t02_min,1)*SIZE(grid%t02_min,2) .GT. 1 ) THEN
grid%t02_min (ips:min(ide-1,ipe),jms:jme) = grid%t02_min (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t02_max,1)*SIZE(grid%t02_max,2) .GT. 1 ) THEN
grid%t02_max (ips:min(ide-1,ipe),jms:jme) = grid%t02_max (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rh02_min,1)*SIZE(grid%rh02_min,2) .GT. 1 ) THEN
grid%rh02_min (ips:min(ide-1,ipe),jms:jme) = grid%rh02_min (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rh02_max,1)*SIZE(grid%rh02_max,2) .GT. 1 ) THEN
grid%rh02_max (ips:min(ide-1,ipe),jms:jme) = grid%rh02_max (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwtt,1)*SIZE(grid%rlwtt,2) .GT. 1 ) THEN
grid%rlwtt (ips:min(ide-1,ipe),jms:jme,:) = grid%rlwtt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rswtt,1)*SIZE(grid%rswtt,2) .GT. 1 ) THEN
grid%rswtt (ips:min(ide-1,ipe),jms:jme,:) = grid%rswtt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%tcucn,1)*SIZE(grid%tcucn,2) .GT. 1 ) THEN
grid%tcucn (ips:min(ide-1,ipe),jms:jme,:) = grid%tcucn (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%train,1)*SIZE(grid%train,2) .GT. 1 ) THEN
grid%train (ips:min(ide-1,ipe),jms:jme,:) = grid%train (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%ncfrcv,1)*SIZE(grid%ncfrcv,2) .GT. 1 ) THEN
grid%ncfrcv (ips:min(ide-1,ipe),jms:jme) = grid%ncfrcv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ncfrst,1)*SIZE(grid%ncfrst,2) .GT. 1 ) THEN
grid%ncfrst (ips:min(ide-1,ipe),jms:jme) = grid%ncfrst (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%max10mw,1)*SIZE(grid%max10mw,2) .GT. 1 ) THEN
grid%max10mw (ips:min(ide-1,ipe),jms:jme) = grid%max10mw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%max10u,1)*SIZE(grid%max10u,2) .GT. 1 ) THEN
grid%max10u (ips:min(ide-1,ipe),jms:jme) = grid%max10u (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%max10v,1)*SIZE(grid%max10v,2) .GT. 1 ) THEN
grid%max10v (ips:min(ide-1,ipe),jms:jme) = grid%max10v (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%maxupdr,1)*SIZE(grid%maxupdr,2) .GT. 1 ) THEN
grid%maxupdr (ips:min(ide-1,ipe),jms:jme) = grid%maxupdr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%maxdndr,1)*SIZE(grid%maxdndr,2) .GT. 1 ) THEN
grid%maxdndr (ips:min(ide-1,ipe),jms:jme) = grid%maxdndr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%maxhlcy,1)*SIZE(grid%maxhlcy,2) .GT. 1 ) THEN
grid%maxhlcy (ips:min(ide-1,ipe),jms:jme) = grid%maxhlcy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%maxdbz,1)*SIZE(grid%maxdbz,2) .GT. 1 ) THEN
grid%maxdbz (ips:min(ide-1,ipe),jms:jme) = grid%maxdbz (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_h,1)*SIZE(grid%iup_h,2) .GT. 1 ) THEN
grid%iup_h (ips:min(ide-1,ipe),jms:jme) = grid%iup_h (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_v,1)*SIZE(grid%iup_v,2) .GT. 1 ) THEN
grid%iup_v (ips:min(ide-1,ipe),jms:jme) = grid%iup_v (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_adh,1)*SIZE(grid%iup_adh,2) .GT. 1 ) THEN
grid%iup_adh (ips:min(ide-1,ipe),jms:jme) = grid%iup_adh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_adv,1)*SIZE(grid%iup_adv,2) .GT. 1 ) THEN
grid%iup_adv (ips:min(ide-1,ipe),jms:jme) = grid%iup_adv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_nostag,1)*SIZE(grid%imask_nostag,2) .GT. 1 ) THEN
grid%imask_nostag (ips:min(ide-1,ipe),jms:jme) = grid%imask_nostag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_xstag,1)*SIZE(grid%imask_xstag,2) .GT. 1 ) THEN
grid%imask_xstag (ips:min(ide-1,ipe),jms:jme) = grid%imask_xstag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_ystag,1)*SIZE(grid%imask_ystag,2) .GT. 1 ) THEN
grid%imask_ystag (ips:min(ide-1,ipe),jms:jme) = grid%imask_ystag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_xystag,1)*SIZE(grid%imask_xystag,2) .GT. 1 ) THEN
grid%imask_xystag (ips:min(ide-1,ipe),jms:jme) = grid%imask_xystag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm000007,1)*SIZE(grid%sm000007,2) .GT. 1 ) THEN
grid%sm000007 (ips:min(ide-1,ipe),jms:jme) = grid%sm000007 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm007028,1)*SIZE(grid%sm007028,2) .GT. 1 ) THEN
grid%sm007028 (ips:min(ide-1,ipe),jms:jme) = grid%sm007028 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm028100,1)*SIZE(grid%sm028100,2) .GT. 1 ) THEN
grid%sm028100 (ips:min(ide-1,ipe),jms:jme) = grid%sm028100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm100255,1)*SIZE(grid%sm100255,2) .GT. 1 ) THEN
grid%sm100255 (ips:min(ide-1,ipe),jms:jme) = grid%sm100255 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st000007,1)*SIZE(grid%st000007,2) .GT. 1 ) THEN
grid%st000007 (ips:min(ide-1,ipe),jms:jme) = grid%st000007 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st007028,1)*SIZE(grid%st007028,2) .GT. 1 ) THEN
grid%st007028 (ips:min(ide-1,ipe),jms:jme) = grid%st007028 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st028100,1)*SIZE(grid%st028100,2) .GT. 1 ) THEN
grid%st028100 (ips:min(ide-1,ipe),jms:jme) = grid%st028100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st100255,1)*SIZE(grid%st100255,2) .GT. 1 ) THEN
grid%st100255 (ips:min(ide-1,ipe),jms:jme) = grid%st100255 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm000010,1)*SIZE(grid%sm000010,2) .GT. 1 ) THEN
grid%sm000010 (ips:min(ide-1,ipe),jms:jme) = grid%sm000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm010040,1)*SIZE(grid%sm010040,2) .GT. 1 ) THEN
grid%sm010040 (ips:min(ide-1,ipe),jms:jme) = grid%sm010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm040100,1)*SIZE(grid%sm040100,2) .GT. 1 ) THEN
grid%sm040100 (ips:min(ide-1,ipe),jms:jme) = grid%sm040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm100200,1)*SIZE(grid%sm100200,2) .GT. 1 ) THEN
grid%sm100200 (ips:min(ide-1,ipe),jms:jme) = grid%sm100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm010200,1)*SIZE(grid%sm010200,2) .GT. 1 ) THEN
grid%sm010200 (ips:min(ide-1,ipe),jms:jme) = grid%sm010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm000,1)*SIZE(grid%soilm000,2) .GT. 1 ) THEN
grid%soilm000 (ips:min(ide-1,ipe),jms:jme) = grid%soilm000 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm005,1)*SIZE(grid%soilm005,2) .GT. 1 ) THEN
grid%soilm005 (ips:min(ide-1,ipe),jms:jme) = grid%soilm005 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm020,1)*SIZE(grid%soilm020,2) .GT. 1 ) THEN
grid%soilm020 (ips:min(ide-1,ipe),jms:jme) = grid%soilm020 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm040,1)*SIZE(grid%soilm040,2) .GT. 1 ) THEN
grid%soilm040 (ips:min(ide-1,ipe),jms:jme) = grid%soilm040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm160,1)*SIZE(grid%soilm160,2) .GT. 1 ) THEN
grid%soilm160 (ips:min(ide-1,ipe),jms:jme) = grid%soilm160 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm300,1)*SIZE(grid%soilm300,2) .GT. 1 ) THEN
grid%soilm300 (ips:min(ide-1,ipe),jms:jme) = grid%soilm300 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw000010,1)*SIZE(grid%sw000010,2) .GT. 1 ) THEN
grid%sw000010 (ips:min(ide-1,ipe),jms:jme) = grid%sw000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw010040,1)*SIZE(grid%sw010040,2) .GT. 1 ) THEN
grid%sw010040 (ips:min(ide-1,ipe),jms:jme) = grid%sw010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw040100,1)*SIZE(grid%sw040100,2) .GT. 1 ) THEN
grid%sw040100 (ips:min(ide-1,ipe),jms:jme) = grid%sw040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw100200,1)*SIZE(grid%sw100200,2) .GT. 1 ) THEN
grid%sw100200 (ips:min(ide-1,ipe),jms:jme) = grid%sw100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw010200,1)*SIZE(grid%sw010200,2) .GT. 1 ) THEN
grid%sw010200 (ips:min(ide-1,ipe),jms:jme) = grid%sw010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw000,1)*SIZE(grid%soilw000,2) .GT. 1 ) THEN
grid%soilw000 (ips:min(ide-1,ipe),jms:jme) = grid%soilw000 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw005,1)*SIZE(grid%soilw005,2) .GT. 1 ) THEN
grid%soilw005 (ips:min(ide-1,ipe),jms:jme) = grid%soilw005 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw020,1)*SIZE(grid%soilw020,2) .GT. 1 ) THEN
grid%soilw020 (ips:min(ide-1,ipe),jms:jme) = grid%soilw020 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw040,1)*SIZE(grid%soilw040,2) .GT. 1 ) THEN
grid%soilw040 (ips:min(ide-1,ipe),jms:jme) = grid%soilw040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw160,1)*SIZE(grid%soilw160,2) .GT. 1 ) THEN
grid%soilw160 (ips:min(ide-1,ipe),jms:jme) = grid%soilw160 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw300,1)*SIZE(grid%soilw300,2) .GT. 1 ) THEN
grid%soilw300 (ips:min(ide-1,ipe),jms:jme) = grid%soilw300 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st000010,1)*SIZE(grid%st000010,2) .GT. 1 ) THEN
grid%st000010 (ips:min(ide-1,ipe),jms:jme) = grid%st000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st010040,1)*SIZE(grid%st010040,2) .GT. 1 ) THEN
grid%st010040 (ips:min(ide-1,ipe),jms:jme) = grid%st010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st040100,1)*SIZE(grid%st040100,2) .GT. 1 ) THEN
grid%st040100 (ips:min(ide-1,ipe),jms:jme) = grid%st040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st100200,1)*SIZE(grid%st100200,2) .GT. 1 ) THEN
grid%st100200 (ips:min(ide-1,ipe),jms:jme) = grid%st100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st010200,1)*SIZE(grid%st010200,2) .GT. 1 ) THEN
grid%st010200 (ips:min(ide-1,ipe),jms:jme) = grid%st010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt000,1)*SIZE(grid%soilt000,2) .GT. 1 ) THEN
grid%soilt000 (ips:min(ide-1,ipe),jms:jme) = grid%soilt000 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt005,1)*SIZE(grid%soilt005,2) .GT. 1 ) THEN
grid%soilt005 (ips:min(ide-1,ipe),jms:jme) = grid%soilt005 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt020,1)*SIZE(grid%soilt020,2) .GT. 1 ) THEN
grid%soilt020 (ips:min(ide-1,ipe),jms:jme) = grid%soilt020 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt040,1)*SIZE(grid%soilt040,2) .GT. 1 ) THEN
grid%soilt040 (ips:min(ide-1,ipe),jms:jme) = grid%soilt040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt160,1)*SIZE(grid%soilt160,2) .GT. 1 ) THEN
grid%soilt160 (ips:min(ide-1,ipe),jms:jme) = grid%soilt160 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt300,1)*SIZE(grid%soilt300,2) .GT. 1 ) THEN
grid%soilt300 (ips:min(ide-1,ipe),jms:jme) = grid%soilt300 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%landmask,1)*SIZE(grid%landmask,2) .GT. 1 ) THEN
grid%landmask (ips:min(ide-1,ipe),jms:jme) = grid%landmask (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%topostdv,1)*SIZE(grid%topostdv,2) .GT. 1 ) THEN
grid%topostdv (ips:min(ide-1,ipe),jms:jme) = grid%topostdv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%toposlpx,1)*SIZE(grid%toposlpx,2) .GT. 1 ) THEN
grid%toposlpx (ips:min(ide-1,ipe),jms:jme) = grid%toposlpx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%toposlpy,1)*SIZE(grid%toposlpy,2) .GT. 1 ) THEN
grid%toposlpy (ips:min(ide-1,ipe),jms:jme) = grid%toposlpy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%greenmax,1)*SIZE(grid%greenmax,2) .GT. 1 ) THEN
grid%greenmax (ips:min(ide-1,ipe),jms:jme) = grid%greenmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%greenmin,1)*SIZE(grid%greenmin,2) .GT. 1 ) THEN
grid%greenmin (ips:min(ide-1,ipe),jms:jme) = grid%greenmin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albedomx,1)*SIZE(grid%albedomx,2) .GT. 1 ) THEN
grid%albedomx (ips:min(ide-1,ipe),jms:jme) = grid%albedomx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%slopecat,1)*SIZE(grid%slopecat,2) .GT. 1 ) THEN
grid%slopecat (ips:min(ide-1,ipe),jms:jme) = grid%slopecat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%toposoil,1)*SIZE(grid%toposoil,2) .GT. 1 ) THEN
grid%toposoil (ips:min(ide-1,ipe),jms:jme) = grid%toposoil (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%landusef,1)*SIZE(grid%landusef,3) .GT. 1 ) THEN
grid%landusef (ips:min(ide-1,ipe),:,jms:jme) = grid%landusef (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%soilctop,1)*SIZE(grid%soilctop,3) .GT. 1 ) THEN
grid%soilctop (ips:min(ide-1,ipe),:,jms:jme) = grid%soilctop (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%soilcbot,1)*SIZE(grid%soilcbot,3) .GT. 1 ) THEN
grid%soilcbot (ips:min(ide-1,ipe),:,jms:jme) = grid%soilcbot (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
  DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE(grid%moist,1)*SIZE(grid%moist,2) .GT. 1 ) THEN
grid%moist (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%moist (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE(grid%dfi_moist,1)*SIZE(grid%dfi_moist,2) .GT. 1 ) THEN
grid%dfi_moist (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%dfi_moist (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE(grid%scalar,1)*SIZE(grid%scalar,2) .GT. 1 ) THEN
grid%scalar (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%scalar (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE(grid%dfi_scalar,1)*SIZE(grid%dfi_scalar,3) .GT. 1 ) THEN
grid%dfi_scalar (ips:min(ide-1,ipe),:,jms:jme,itrace) = grid%dfi_scalar (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_chem
IF ( SIZE(grid%chem,1)*SIZE(grid%chem,3) .GT. 1 ) THEN
grid%chem (ips:min(ide-1,ipe),:,jms:jme,itrace) = grid%chem (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
ENDIF
  ENDDO
IF ( SIZE(grid%smois,1)*SIZE(grid%smois,3) .GT. 1 ) THEN
grid%smois (ips:min(ide-1,ipe),:,jms:jme) = grid%smois (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%tslb,1)*SIZE(grid%tslb,3) .GT. 1 ) THEN
grid%tslb (ips:min(ide-1,ipe),:,jms:jme) = grid%tslb (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%gsw,1)*SIZE(grid%gsw,2) .GT. 1 ) THEN
grid%gsw (ips:min(ide-1,ipe),jms:jme) = grid%gsw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xlat,1)*SIZE(grid%xlat,2) .GT. 1 ) THEN
grid%xlat (ips:min(ide-1,ipe),jms:jme) = grid%xlat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xlong,1)*SIZE(grid%xlong,2) .GT. 1 ) THEN
grid%xlong (ips:min(ide-1,ipe),jms:jme) = grid%xlong (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xland,1)*SIZE(grid%xland,2) .GT. 1 ) THEN
grid%xland (ips:min(ide-1,ipe),jms:jme) = grid%xland (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%raincv,1)*SIZE(grid%raincv,2) .GT. 1 ) THEN
grid%raincv (ips:min(ide-1,ipe),jms:jme) = grid%raincv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psfc,1)*SIZE(grid%psfc,2) .GT. 1 ) THEN
grid%psfc (ips:min(ide-1,ipe),jms:jme) = grid%psfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%th2,1)*SIZE(grid%th2,2) .GT. 1 ) THEN
grid%th2 (ips:min(ide-1,ipe),jms:jme) = grid%th2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t2,1)*SIZE(grid%t2,2) .GT. 1 ) THEN
grid%t2 (ips:min(ide-1,ipe),jms:jme) = grid%t2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%u10,1)*SIZE(grid%u10,2) .GT. 1 ) THEN
grid%u10 (ips:min(ide-1,ipe),jms:jme) = grid%u10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%v10,1)*SIZE(grid%v10,2) .GT. 1 ) THEN
grid%v10 (ips:min(ide-1,ipe),jms:jme) = grid%v10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xice,1)*SIZE(grid%xice,2) .GT. 1 ) THEN
grid%xice (ips:min(ide-1,ipe),jms:jme) = grid%xice (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lai,1)*SIZE(grid%lai,2) .GT. 1 ) THEN
grid%lai (ips:min(ide-1,ipe),jms:jme) = grid%lai (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smstav,1)*SIZE(grid%smstav,2) .GT. 1 ) THEN
grid%smstav (ips:min(ide-1,ipe),jms:jme) = grid%smstav (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smstot,1)*SIZE(grid%smstot,2) .GT. 1 ) THEN
grid%smstot (ips:min(ide-1,ipe),jms:jme) = grid%smstot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcrunoff,1)*SIZE(grid%sfcrunoff,2) .GT. 1 ) THEN
grid%sfcrunoff (ips:min(ide-1,ipe),jms:jme) = grid%sfcrunoff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%udrunoff,1)*SIZE(grid%udrunoff,2) .GT. 1 ) THEN
grid%udrunoff (ips:min(ide-1,ipe),jms:jme) = grid%udrunoff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ivgtyp,1)*SIZE(grid%ivgtyp,2) .GT. 1 ) THEN
grid%ivgtyp (ips:min(ide-1,ipe),jms:jme) = grid%ivgtyp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%isltyp,1)*SIZE(grid%isltyp,2) .GT. 1 ) THEN
grid%isltyp (ips:min(ide-1,ipe),jms:jme) = grid%isltyp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vegfra,1)*SIZE(grid%vegfra,2) .GT. 1 ) THEN
grid%vegfra (ips:min(ide-1,ipe),jms:jme) = grid%vegfra (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcevp,1)*SIZE(grid%sfcevp,2) .GT. 1 ) THEN
grid%sfcevp (ips:min(ide-1,ipe),jms:jme) = grid%sfcevp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%grdflx,1)*SIZE(grid%grdflx,2) .GT. 1 ) THEN
grid%grdflx (ips:min(ide-1,ipe),jms:jme) = grid%grdflx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albbck,1)*SIZE(grid%albbck,2) .GT. 1 ) THEN
grid%albbck (ips:min(ide-1,ipe),jms:jme) = grid%albbck (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcexc,1)*SIZE(grid%sfcexc,2) .GT. 1 ) THEN
grid%sfcexc (ips:min(ide-1,ipe),jms:jme) = grid%sfcexc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snotime,1)*SIZE(grid%snotime,2) .GT. 1 ) THEN
grid%snotime (ips:min(ide-1,ipe),jms:jme) = grid%snotime (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acsnow,1)*SIZE(grid%acsnow,2) .GT. 1 ) THEN
grid%acsnow (ips:min(ide-1,ipe),jms:jme) = grid%acsnow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acsnom,1)*SIZE(grid%acsnom,2) .GT. 1 ) THEN
grid%acsnom (ips:min(ide-1,ipe),jms:jme) = grid%acsnom (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rmol,1)*SIZE(grid%rmol,2) .GT. 1 ) THEN
grid%rmol (ips:min(ide-1,ipe),jms:jme) = grid%rmol (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snow,1)*SIZE(grid%snow,2) .GT. 1 ) THEN
grid%snow (ips:min(ide-1,ipe),jms:jme) = grid%snow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%canwat,1)*SIZE(grid%canwat,2) .GT. 1 ) THEN
grid%canwat (ips:min(ide-1,ipe),jms:jme) = grid%canwat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sst,1)*SIZE(grid%sst,2) .GT. 1 ) THEN
grid%sst (ips:min(ide-1,ipe),jms:jme) = grid%sst (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%weasd,1)*SIZE(grid%weasd,2) .GT. 1 ) THEN
grid%weasd (ips:min(ide-1,ipe),jms:jme) = grid%weasd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%znt,1)*SIZE(grid%znt,2) .GT. 1 ) THEN
grid%znt (ips:min(ide-1,ipe),jms:jme) = grid%znt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mol,1)*SIZE(grid%mol,2) .GT. 1 ) THEN
grid%mol (ips:min(ide-1,ipe),jms:jme) = grid%mol (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%noahres,1)*SIZE(grid%noahres,2) .GT. 1 ) THEN
grid%noahres (ips:min(ide-1,ipe),jms:jme) = grid%noahres (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tke_myj,1)*SIZE(grid%tke_myj,2) .GT. 1 ) THEN
grid%tke_myj (ips:min(ide-1,ipe),jms:jme,:) = grid%tke_myj (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%el_myj,1)*SIZE(grid%el_myj,3) .GT. 1 ) THEN
grid%el_myj (ips:min(ide-1,ipe),:,jms:jme) = grid%el_myj (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%exch_h,1)*SIZE(grid%exch_h,3) .GT. 1 ) THEN
grid%exch_h (ips:min(ide-1,ipe),:,jms:jme) = grid%exch_h (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%exch_m,1)*SIZE(grid%exch_m,3) .GT. 1 ) THEN
grid%exch_m (ips:min(ide-1,ipe),:,jms:jme) = grid%exch_m (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%thz0,1)*SIZE(grid%thz0,2) .GT. 1 ) THEN
grid%thz0 (ips:min(ide-1,ipe),jms:jme) = grid%thz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qz0,1)*SIZE(grid%qz0,2) .GT. 1 ) THEN
grid%qz0 (ips:min(ide-1,ipe),jms:jme) = grid%qz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%uz0,1)*SIZE(grid%uz0,2) .GT. 1 ) THEN
grid%uz0 (ips:min(ide-1,ipe),jms:jme) = grid%uz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vz0,1)*SIZE(grid%vz0,2) .GT. 1 ) THEN
grid%vz0 (ips:min(ide-1,ipe),jms:jme) = grid%vz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%flhc,1)*SIZE(grid%flhc,2) .GT. 1 ) THEN
grid%flhc (ips:min(ide-1,ipe),jms:jme) = grid%flhc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%flqc,1)*SIZE(grid%flqc,2) .GT. 1 ) THEN
grid%flqc (ips:min(ide-1,ipe),jms:jme) = grid%flqc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qsg,1)*SIZE(grid%qsg,2) .GT. 1 ) THEN
grid%qsg (ips:min(ide-1,ipe),jms:jme) = grid%qsg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qvg,1)*SIZE(grid%qvg,2) .GT. 1 ) THEN
grid%qvg (ips:min(ide-1,ipe),jms:jme) = grid%qvg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qcg,1)*SIZE(grid%qcg,2) .GT. 1 ) THEN
grid%qcg (ips:min(ide-1,ipe),jms:jme) = grid%qcg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt1,1)*SIZE(grid%soilt1,2) .GT. 1 ) THEN
grid%soilt1 (ips:min(ide-1,ipe),jms:jme) = grid%soilt1 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tsnav,1)*SIZE(grid%tsnav,2) .GT. 1 ) THEN
grid%tsnav (ips:min(ide-1,ipe),jms:jme) = grid%tsnav (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psfc_out,1)*SIZE(grid%psfc_out,2) .GT. 1 ) THEN
grid%psfc_out (ips:min(ide-1,ipe),jms:jme) = grid%psfc_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%uz0h,1)*SIZE(grid%uz0h,2) .GT. 1 ) THEN
grid%uz0h (ips:min(ide-1,ipe),jms:jme) = grid%uz0h (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vz0h,1)*SIZE(grid%vz0h,2) .GT. 1 ) THEN
grid%vz0h (ips:min(ide-1,ipe),jms:jme) = grid%vz0h (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dudt,1)*SIZE(grid%dudt,2) .GT. 1 ) THEN
grid%dudt (ips:min(ide-1,ipe),jms:jme,:) = grid%dudt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dvdt,1)*SIZE(grid%dvdt,2) .GT. 1 ) THEN
grid%dvdt (ips:min(ide-1,ipe),jms:jme,:) = grid%dvdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%qsfc,1)*SIZE(grid%qsfc,2) .GT. 1 ) THEN
grid%qsfc (ips:min(ide-1,ipe),jms:jme) = grid%qsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%akhs,1)*SIZE(grid%akhs,2) .GT. 1 ) THEN
grid%akhs (ips:min(ide-1,ipe),jms:jme) = grid%akhs (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%akms,1)*SIZE(grid%akms,2) .GT. 1 ) THEN
grid%akms (ips:min(ide-1,ipe),jms:jme) = grid%akms (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htop,1)*SIZE(grid%htop,2) .GT. 1 ) THEN
grid%htop (ips:min(ide-1,ipe),jms:jme) = grid%htop (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbot,1)*SIZE(grid%hbot,2) .GT. 1 ) THEN
grid%hbot (ips:min(ide-1,ipe),jms:jme) = grid%hbot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htopr,1)*SIZE(grid%htopr,2) .GT. 1 ) THEN
grid%htopr (ips:min(ide-1,ipe),jms:jme) = grid%htopr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbotr,1)*SIZE(grid%hbotr,2) .GT. 1 ) THEN
grid%hbotr (ips:min(ide-1,ipe),jms:jme) = grid%hbotr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htopd,1)*SIZE(grid%htopd,2) .GT. 1 ) THEN
grid%htopd (ips:min(ide-1,ipe),jms:jme) = grid%htopd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbotd,1)*SIZE(grid%hbotd,2) .GT. 1 ) THEN
grid%hbotd (ips:min(ide-1,ipe),jms:jme) = grid%hbotd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htops,1)*SIZE(grid%htops,2) .GT. 1 ) THEN
grid%htops (ips:min(ide-1,ipe),jms:jme) = grid%htops (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbots,1)*SIZE(grid%hbots,2) .GT. 1 ) THEN
grid%hbots (ips:min(ide-1,ipe),jms:jme) = grid%hbots (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cuppt,1)*SIZE(grid%cuppt,2) .GT. 1 ) THEN
grid%cuppt (ips:min(ide-1,ipe),jms:jme) = grid%cuppt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cprate,1)*SIZE(grid%cprate,2) .GT. 1 ) THEN
grid%cprate (ips:min(ide-1,ipe),jms:jme) = grid%cprate (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%f_ice_phy,1)*SIZE(grid%f_ice_phy,3) .GT. 1 ) THEN
grid%f_ice_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_ice_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rain_phy,1)*SIZE(grid%f_rain_phy,3) .GT. 1 ) THEN
grid%f_rain_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rain_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rimef_phy,1)*SIZE(grid%f_rimef_phy,3) .GT. 1 ) THEN
grid%f_rimef_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rimef_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%mass_flux,1)*SIZE(grid%mass_flux,2) .GT. 1 ) THEN
grid%mass_flux (ips:min(ide-1,ipe),jms:jme) = grid%mass_flux (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_gr,1)*SIZE(grid%apr_gr,2) .GT. 1 ) THEN
grid%apr_gr (ips:min(ide-1,ipe),jms:jme) = grid%apr_gr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_w,1)*SIZE(grid%apr_w,2) .GT. 1 ) THEN
grid%apr_w (ips:min(ide-1,ipe),jms:jme) = grid%apr_w (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_mc,1)*SIZE(grid%apr_mc,2) .GT. 1 ) THEN
grid%apr_mc (ips:min(ide-1,ipe),jms:jme) = grid%apr_mc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_st,1)*SIZE(grid%apr_st,2) .GT. 1 ) THEN
grid%apr_st (ips:min(ide-1,ipe),jms:jme) = grid%apr_st (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_as,1)*SIZE(grid%apr_as,2) .GT. 1 ) THEN
grid%apr_as (ips:min(ide-1,ipe),jms:jme) = grid%apr_as (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_capma,1)*SIZE(grid%apr_capma,2) .GT. 1 ) THEN
grid%apr_capma (ips:min(ide-1,ipe),jms:jme) = grid%apr_capma (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_capme,1)*SIZE(grid%apr_capme,2) .GT. 1 ) THEN
grid%apr_capme (ips:min(ide-1,ipe),jms:jme) = grid%apr_capme (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_capmi,1)*SIZE(grid%apr_capmi,2) .GT. 1 ) THEN
grid%apr_capmi (ips:min(ide-1,ipe),jms:jme) = grid%apr_capmi (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rthften,1)*SIZE(grid%rthften,3) .GT. 1 ) THEN
grid%rthften (ips:min(ide-1,ipe),:,jms:jme) = grid%rthften (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%rqvften,1)*SIZE(grid%rqvften,3) .GT. 1 ) THEN
grid%rqvften (ips:min(ide-1,ipe),:,jms:jme) = grid%rqvften (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%snowh,1)*SIZE(grid%snowh,2) .GT. 1 ) THEN
grid%snowh (ips:min(ide-1,ipe),jms:jme) = grid%snowh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rhosn,1)*SIZE(grid%rhosn,2) .GT. 1 ) THEN
grid%rhosn (ips:min(ide-1,ipe),jms:jme) = grid%rhosn (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smfr3d,1)*SIZE(grid%smfr3d,3) .GT. 1 ) THEN
grid%smfr3d (ips:min(ide-1,ipe),:,jms:jme) = grid%smfr3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%keepfr3dflag,1)*SIZE(grid%keepfr3dflag,3) .GT. 1 ) THEN
grid%keepfr3dflag (ips:min(ide-1,ipe),:,jms:jme) = grid%keepfr3dflag (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
