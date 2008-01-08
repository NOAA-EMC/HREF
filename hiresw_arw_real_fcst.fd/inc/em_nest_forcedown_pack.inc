!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_nest_forcedown_pack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
msize = 13 * nlev + 1
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
DO k = ckds,(ckde-1)
xv(k)= grid%em_u_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= grid%em_v_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,ckde
xv(k)= grid%em_w_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,ckde
xv(k)= grid%em_ph_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
xv(k)= grid%em_t_2(pig,k,pjg)
ENDDO
CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
xv(1)=grid%em_mu_2(pig,pjg)
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
