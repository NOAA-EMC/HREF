!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nodyn_nest_interpdown_pack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
msize = 18 * nlev + 54
CALL rsl_lite_to_child_info( local_communicator, msize*RWORDSIZE                               &
                        ,cips,cipe,cjps,cjpe                               &
                        ,iids,iide,ijds,ijde                               &
                        ,nids,nide,njds,njde                               &
                        ,pgr , sw                                          &
                        ,ntasks_x,ntasks_y                                 &
                        ,icoord,jcoord                                     &
                        ,idim_cd,jdim_cd                                   &
                        ,pig,pjg,retval )
DO while ( retval .eq. 1 )
xv(1)=grid%lu_index(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%q2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%t2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%th2(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%u10(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%v10(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
DO itrace =  PARAM_FIRST_SCALAR, num_moist
DO k = ckds,(ckde-1)
xv(k)= moist(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
DO k = ckds,(ckde-1)
xv(k)= scalar(pig,k,pjg,itrace)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
xv(1)=grid%landmask(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%tslb(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%smois(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%sh2o(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
xv(1)=grid%xice(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%smstav(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%sfcrunoff(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%udrunoff(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%ivgtyp(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%isltyp(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%vegfra(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%acsnow(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%acsnom(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%snow(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%snowh(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%rhosn(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%canwat(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%sst(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%tr_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%tb_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%tg_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%tc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%qc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%uc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%xxxr_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%xxxb_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%xxxg_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%xxxc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%trl_urb3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%tbl_urb3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
DO k = 1,config_flags%num_soil_layers
xv(k)= grid%tgl_urb3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((config_flags%num_soil_layers)-(1)+1)*RWORDSIZE,xv)
xv(1)=grid%frc_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%utype_urb2d(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= grid%f_ice_phy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= grid%f_rain_phy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= grid%f_rimef_phy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
xv(1)=grid%msft(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%msfu(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%msfv(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%f(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%e(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%sina(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%cosa(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%ht(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%tsk(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%rainc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%rainnc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%snownc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%graupelnc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= grid%rthraten(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
xv(1)=grid%swdown(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%gsw(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%glw(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%xlat(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%xlong(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%tmn(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%xland(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
xv(1)=grid%snowc(pig,pjg)
CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
CALL rsl_lite_to_child_info( local_communicator, msize*RWORDSIZE                               &
                        ,cips,cipe,cjps,cjpe                               &
                        ,iids,iide,ijds,ijde                               &
                        ,nids,nide,njds,njde                               &
                        ,pgr , sw                                          &
                        ,ntasks_x,ntasks_y                                 &
                        ,icoord,jcoord                                     &
                        ,idim_cd,jdim_cd                                   &
                        ,pig,pjg,retval )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
