!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_SCALAR_OLD_E_5.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_SCALAR_OLD_E_5.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0  &
   + num_scalar   &
     , 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0  &
   + num_scalar   &
     , 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
))
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar_old ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
