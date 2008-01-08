!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_B.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_B.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     12, 3, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ru_tend, 2, RWORDSIZE, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rv_tend, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_save, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_save, 2, RWORDSIZE, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_save, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 2, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 2, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mudf, 2, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_pb, 2, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ru_tend, 2, RWORDSIZE, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_rv_tend, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_p, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_save, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_save, 2, RWORDSIZE, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_save, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 2, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 2, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mudf, 2, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_pb, 2, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
