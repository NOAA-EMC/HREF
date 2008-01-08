!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_nest_feedbackup_pack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
msize = 25 * nlev + 55
CALL rsl_lite_to_parent_info( local_communicator, msize*RWORDSIZE                               &
                        ,cips,cipe,cjps,cjpe                               &
                        ,nids,nide,njds,njde                               &
                        ,ntasks_x,ntasks_y                                 &
                        ,icoord,jcoord                                     &
                        ,idim_cd,jdim_cd                                   &
                        ,pig,pjg,retval )
DO while ( retval .eq. 1 )
xv(1)= intermediate_grid%lu_index(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%em_u_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%em_v_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,ckde
xv(k)= intermediate_grid%em_w_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,ckde
xv(k)= intermediate_grid%em_ph_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,ckde
xv(k)= intermediate_grid%em_phb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%em_t_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
xv(1)= intermediate_grid%em_mu_2(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_mub(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_mu0(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_muu(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_muv(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_mut(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_muts(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%nest_pos(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%nest_mask(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%em_alb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%em_pb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO itrace =  PARAM_FIRST_SCALAR, num_moist
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%moist(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%scalar(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
xv(1)= intermediate_grid%landmask(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= intermediate_grid%tslb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= intermediate_grid%smois(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= intermediate_grid%sh2o(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
xv(1)= intermediate_grid%xice(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%ivgtyp(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%isltyp(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%vegfra(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%acsnow(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%acsnom(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%snow(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%snowh(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%rhosn(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%canwat(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%tr_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%tb_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%tg_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%tc_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%qc_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%uc_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%xxxr_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%xxxb_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%xxxg_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%xxxc_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= intermediate_grid%trl_urb3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= intermediate_grid%tbl_urb3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= intermediate_grid%tgl_urb3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
xv(1)= intermediate_grid%frc_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%utype_urb2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%f_ice_phy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%f_rain_phy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%f_rimef_phy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
xv(1)= intermediate_grid%msft(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%msfu(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%msfv(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%f(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%e(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%sina(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%cosa(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%ht(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%tsk(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%rainc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%rainnc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%snownc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%graupelnc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%xlat(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%xlong(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_xlat_u(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_xlong_u(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_xlat_v(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%em_xlong_v(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%tmn(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%xland(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
xv(1)= intermediate_grid%snowc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
CALL rsl_lite_to_parent_info( local_communicator, msize*RWORDSIZE                               &
                        ,cips,cipe,cjps,cjpe                               &
                        ,nids,nide,njds,njde                               &
                        ,ntasks_x,ntasks_y                                 &
                        ,icoord,jcoord                                     &
                        ,idim_cd,jdim_cd                                   &
                        ,pig,pjg,retval )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
