!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nodyn_nest_feedbackup_pack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
msize = 1 * nlev + 2
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
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%scalar(pig,pjg,k,itrace)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
xv(1)= intermediate_grid%rhosn(pig,pjg)
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
