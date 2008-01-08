!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_VINTERP_UV_1.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_VINTERP_UV_1.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1, &
     2, 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,config_flags%num_metgrid_levels &
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_pd_gc, 1, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_metgrid_levels,             &
ims, ime, jms, jme, 1, config_flags%num_metgrid_levels,             &
ips, ipe, jps, jpe, 1, config_flags%num_metgrid_levels              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 1, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pd_gc, 1, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_metgrid_levels,             &
ims, ime, jms, jme, 1, config_flags%num_metgrid_levels,             &
ips, ipe, jps, jpe, 1, config_flags%num_metgrid_levels              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 1, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 1 , &
     2, 0, RWORDSIZE, &
     0, 0, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,config_flags%num_metgrid_levels &
,kpe &
))
CALL RSL_LITE_PACK ( local_communicator, grid%em_pd_gc, 1, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_metgrid_levels,             &
ims, ime, jms, jme, 1, config_flags%num_metgrid_levels,             &
ips, ipe, jps, jpe, 1, config_flags%num_metgrid_levels              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 1, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pd_gc, 1, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_metgrid_levels,             &
ims, ime, jms, jme, 1, config_flags%num_metgrid_levels,             &
ips, ipe, jps, jpe, 1, config_flags%num_metgrid_levels              )
CALL RSL_LITE_PACK ( local_communicator, grid%em_pb, 1, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
!ENDOFREGISTRYGENERATEDINCLUDE
