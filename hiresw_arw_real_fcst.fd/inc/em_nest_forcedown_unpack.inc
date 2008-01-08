!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_nest_forcedown_unpack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL rsl_lite_from_parent_info(pig,pjg,retval)
DO while ( retval .eq. 1 )
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%em_u_2(pig,k,pjg) = xv(k)
ENDDO
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%em_v_2(pig,k,pjg) = xv(k)
ENDDO
CALL rsl_lite_from_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,ckde
grid%em_w_2(pig,k,pjg) = xv(k)
ENDDO
CALL rsl_lite_from_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,ckde
grid%em_ph_2(pig,k,pjg) = xv(k)
ENDDO
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
grid%em_t_2(pig,k,pjg) = xv(k)
ENDDO
CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
grid%em_mu_2(pig,pjg) = xv(1)
DO itrace =  PARAM_FIRST_SCALAR, num_moist
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
moist(pig,k,pjg,itrace) = xv(k)
ENDDO
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
DO k = ckds,(ckde-1)
scalar(pig,k,pjg,itrace) = xv(k)
ENDDO
ENDDO
CALL rsl_lite_from_parent_info(pig,pjg,retval)
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
