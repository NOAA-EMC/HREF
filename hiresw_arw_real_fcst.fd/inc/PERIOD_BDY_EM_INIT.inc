!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_BDY_EM_INIT.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_BDY_EM_INIT.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 3 , &
     18, 12, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_1, 3, RWORDSIZE, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_2, 3, RWORDSIZE, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_1, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_2, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_1, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_2, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_2, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_1, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_init, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_phb, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph0, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_pb, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alb, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mub, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu0, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%ht, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%msft, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%msfu, 3, RWORDSIZE, 1, 0, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%msfv, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%sina, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%cosa, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%e, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%f, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_1, 3, RWORDSIZE, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_u_2, 3, RWORDSIZE, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_1, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_v_2, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_1, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_w_2, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_1, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_2, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_1, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph_2, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_t_init, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_phb, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_ph0, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_php, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_pb, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_al, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alt, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_alb, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_1, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu_2, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mub, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mu0, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%ht, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%msft, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%msfu, 3, RWORDSIZE, 1, 1, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%msfv, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%sina, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%cosa, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%e, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%f, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
