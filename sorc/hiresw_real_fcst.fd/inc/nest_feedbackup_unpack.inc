!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_feedbackup_unpack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL rsl_lite_from_child_info(pig,pjg,retval)
DO while ( retval .eq. 1 )
IF ( SIZE(grid%lu_index) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%lu_index(pig,pjg),xv(1))
ENDIF
ENDIF
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(scalar(pig,pjg,k,itrace),xv(k))
ENDDO
ENDIF
ENDDO
DO itrace =  PARAM_FIRST_SCALAR, num_dfi_scalar
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(dfi_scalar(pig,k,pjg,itrace),xv(k))
ENDDO
ENDIF
ENDDO
IF ( SIZE(grid%rhosn) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF ( cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. ) ) THEN
NEST_INFLUENCE(grid%rhosn(pig,pjg),xv(1))
ENDIF
ENDIF
CALL rsl_lite_from_child_info(pig,pjg,retval)
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
