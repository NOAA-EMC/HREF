!STARTOFREGISTRYGENERATEDINCLUDE 'inc/PERIOD_EM_COUPLE_A.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/PERIOD_EM_COUPLE_A.inc')
IF ( config_flags%periodic_x ) THEN
CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, 2 , &
     0, 3, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, kpe    )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mub, 2, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
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
   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK_PERIOD_X ( local_communicator_periodic, grid%em_mub, 2, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
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
END IF
!ENDOFREGISTRYGENERATEDINCLUDE
