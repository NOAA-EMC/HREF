!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_nest_feedbackup_unpack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL rsl_lite_from_child_info(pig,pjg,retval)
DO while ( retval .eq. 1 )
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%lu_index(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%em_u_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%em_v_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,ckde
NEST_INFLUENCE(grid%em_w_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,ckde
NEST_INFLUENCE(grid%em_ph_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,ckde
NEST_INFLUENCE(grid%em_phb(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%em_t_2(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%em_mu_2(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%em_mub(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%em_mu0(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%em_muu(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%em_muv(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%em_mut(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%em_muts(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%nest_pos(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%nest_mask(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%em_alb(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%em_pb(pig,k,pjg),xv(k))
ENDDO
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_moist
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(moist(pig,k,pjg,itrace),xv(k))
ENDDO
ENDIF
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(scalar(pig,k,pjg,itrace),xv(k))
ENDDO
ENDIF
ENDDO
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%landmask(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%tslb(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%smois(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%sh2o(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%xice(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%ivgtyp(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%isltyp(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%vegfra(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%acsnow(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%acsnom(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%snow(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%snowh(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%rhosn(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%canwat(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%tr_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%tb_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%tg_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%tc_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%qc_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%uc_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%xxxr_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%xxxb_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%xxxg_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%xxxc_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%trl_urb3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%tbl_urb3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = 1,config_flags%num_soil_layers
NEST_INFLUENCE(grid%tgl_urb3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%frc_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%utype_urb2d(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%f_ice_phy(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%f_rain_phy(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%f_rimef_phy(pig,k,pjg),xv(k))
ENDDO
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%msft(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
grid%msfu(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
grid%msfv(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%f(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%e(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%sina(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%cosa(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%ht(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%tsk(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%rainc(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%rainnc(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%snownc(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%graupelnc(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%xlat(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%xlong(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
grid%em_xlat_u(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .TRUE., .FALSE. ) ) THEN
grid%em_xlong_u(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
grid%em_xlat_v(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .TRUE. ) ) THEN
grid%em_xlong_v(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%tmn(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%xland(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( em_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
grid%snowc(pig,pjg) = xv(1) ;
ENDIF
CALL rsl_lite_from_child_info(pig,pjg,retval)
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
