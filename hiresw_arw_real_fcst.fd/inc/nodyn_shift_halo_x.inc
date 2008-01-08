!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nodyn_shift_halo_x.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/nodyn_shift_halo_x.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     56  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
   + num_ozmixm   &
   + num_aerosolc   &
   + num_aerosolc   &
   + num_fdda3d   &
   + num_fdda2d   &
     , 198, RWORDSIZE, &
     0, 11, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
,config_flags%num_land_cat &
,config_flags%num_soil_cat &
,config_flags%num_soil_layers &
,config_flags%levsiz &
,config_flags%paerlev &
,1 &
))
CALL RSL_LITE_PACK ( local_communicator, grid%lu_index, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lu_mask, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nest_pos, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nest_mask, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_coarse, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%q2, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%t2, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%th2, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%psfc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%u10, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%v10, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uratx, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vratx, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tratx, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_nostag, 3, IWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_xstag, 3, IWORDSIZE, 0, 0, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_ystag, 3, IWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_xystag, 3, IWORDSIZE, 0, 0, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%sm000007, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm007028, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm028100, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm100255, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st000007, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st007028, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st028100, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st100255, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm000010, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm010040, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm040100, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm100200, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm010200, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm000, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm005, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm020, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm040, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm160, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm300, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw000010, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw010040, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw040100, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw100200, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw010200, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw000, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw005, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw020, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw040, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw160, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw300, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st000010, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st010040, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st040100, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st100200, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st010200, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt000, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt005, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt020, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt040, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt160, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt300, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%landmask, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%topostdv, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposlpx, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposlpy, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%shdmax, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%shdmin, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snoalb, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%slopecat, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposoil, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%landusef, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_land_cat,             &
ims, ime, jms, jme, 1, config_flags%num_land_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_land_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilctop, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_cat,             &
ims, ime, jms, jme, 1, config_flags%num_soil_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilcbot, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_cat,             &
ims, ime, jms, jme, 1, config_flags%num_soil_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilcat, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vegcat, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tslb, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%smois, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%sh2o, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%xice, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smstav, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smstot, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcrunoff, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%udrunoff, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ivgtyp, 3, IWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%isltyp, 3, IWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vegfra, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcevp, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%grdflx, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcexc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acsnow, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acsnom, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snow, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowh, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rhosn, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%canwat, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sst, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tr_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tb_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tg_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tc_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qc_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uc_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxr_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxb_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxg_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxc_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%trl_urb3d, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tbl_urb3d, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tgl_urb3d, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%sh_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lh_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%g_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rn_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ts_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%frc_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%utype_urb2d, 3, IWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cosz_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%omg_urb2d, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smfr3d, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%keepfr3dflag, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tke_myj, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%el_myj, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%exch_h, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%ct, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%thz0, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%z0, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qz0, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uz0, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vz0, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qsfc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%akhs, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%akms, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%kpbl, 3, IWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%htop, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hbot, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%htopr, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hbotr, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cutop, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cubot, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cuppt, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rswtoa, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rlwtoa, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%czmean, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfracl, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfracm, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfrach, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acfrst, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ncfrst, 3, IWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acfrcv, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ncfrcv, 3, IWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_ozmixm
 CALL RSL_LITE_PACK ( local_communicator,ozmixm ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%m_ps_1, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%m_ps_2, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
 CALL RSL_LITE_PACK ( local_communicator,aerosolc_1 ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
 CALL RSL_LITE_PACK ( local_communicator,aerosolc_2 ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%f_ice_phy, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%f_rain_phy, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%f_rimef_phy, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%h_diabatic, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%msft, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%msfu, 3, RWORDSIZE, 0, 0, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%msfv, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%f, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%e, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sina, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cosa, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_fine, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_int, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_input, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsk, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsk_save, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rthcuten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvcuten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqrcuten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqccuten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqscuten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqicuten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%w0avg, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rainc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainnc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%raincv, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainncv, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainbl, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snownc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%graupelnc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowncv, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%graupelncv, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nca, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lowlyr, 3, IWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mass_flux, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_gr, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_w, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_mc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_st, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_as, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capma, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capme, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capmi, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rthften, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvften, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthraten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthratenlw, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthratensw, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%cldfra, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%swdown, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%swdownc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%gsw, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%glw, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%swcf, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lwcf, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%olr, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xlat, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xlong, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%albedo, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%albbck, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%emiss, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cldefi, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rublten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvblten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthblten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvblten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqcblten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqiblten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%tmn, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xland, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%znt, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ust, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rmol, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mol, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%pblh, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%capg, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%thc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hfx, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qfx, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lh, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%flhc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%flqc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qsg, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qvg, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qcg, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt1, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsnav, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowc, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mavail, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tkesfcf, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%taucldi, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%taucldc, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor33, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%bn2, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rundgdten, 3, RWORDSIZE, 0, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvndgdten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthndgdten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvndgdten, 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rmundgdten, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_fdda3d
 CALL RSL_LITE_PACK ( local_communicator,fdda3d ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_fdda2d
 CALL RSL_LITE_PACK ( local_communicator,fdda2d ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%dpsdt, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%dmudt, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%pk1m, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mu_2m, 3, RWORDSIZE, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%lu_index, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lu_mask, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nest_pos, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nest_mask, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_coarse, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%q2, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%t2, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%th2, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%psfc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%u10, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%v10, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uratx, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vratx, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tratx, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_nostag, 3, IWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_xstag, 3, IWORDSIZE, 0, 1, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_ystag, 3, IWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_xystag, 3, IWORDSIZE, 0, 1, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%sm000007, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm007028, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm028100, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm100255, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st000007, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st007028, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st028100, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st100255, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm000010, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm010040, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm040100, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm100200, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm010200, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm000, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm005, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm020, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm040, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm160, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm300, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw000010, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw010040, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw040100, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw100200, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw010200, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw000, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw005, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw020, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw040, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw160, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw300, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st000010, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st010040, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st040100, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st100200, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st010200, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt000, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt005, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt020, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt040, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt160, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt300, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%landmask, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%topostdv, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposlpx, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposlpy, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%shdmax, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%shdmin, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snoalb, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%slopecat, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposoil, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%landusef, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_land_cat,             &
ims, ime, jms, jme, 1, config_flags%num_land_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_land_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilctop, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_cat,             &
ims, ime, jms, jme, 1, config_flags%num_soil_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilcbot, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_cat,             &
ims, ime, jms, jme, 1, config_flags%num_soil_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilcat, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vegcat, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tslb, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%smois, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%sh2o, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%xice, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smstav, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smstot, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcrunoff, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%udrunoff, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ivgtyp, 3, IWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%isltyp, 3, IWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vegfra, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcevp, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%grdflx, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcexc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acsnow, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acsnom, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snow, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowh, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rhosn, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%canwat, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sst, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tr_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tb_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tg_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tc_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qc_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uc_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxr_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxb_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxg_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxc_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%trl_urb3d, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tbl_urb3d, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tgl_urb3d, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%sh_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lh_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%g_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rn_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ts_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%frc_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%utype_urb2d, 3, IWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cosz_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%omg_urb2d, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smfr3d, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%keepfr3dflag, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tke_myj, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%el_myj, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%exch_h, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%ct, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%thz0, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%z0, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qz0, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uz0, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vz0, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qsfc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%akhs, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%akms, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%kpbl, 3, IWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%htop, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hbot, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%htopr, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hbotr, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cutop, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cubot, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cuppt, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rswtoa, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rlwtoa, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%czmean, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfracl, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfracm, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfrach, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acfrst, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ncfrst, 3, IWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acfrcv, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ncfrcv, 3, IWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_ozmixm
 CALL RSL_LITE_PACK ( local_communicator,ozmixm ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%m_ps_1, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%m_ps_2, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
 CALL RSL_LITE_PACK ( local_communicator,aerosolc_1 ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
 CALL RSL_LITE_PACK ( local_communicator,aerosolc_2 ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%f_ice_phy, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%f_rain_phy, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%f_rimef_phy, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%h_diabatic, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%msft, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%msfu, 3, RWORDSIZE, 0, 1, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%msfv, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%f, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%e, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sina, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cosa, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_fine, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_int, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_input, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsk, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsk_save, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rthcuten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvcuten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqrcuten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqccuten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqscuten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqicuten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%w0avg, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rainc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainnc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%raincv, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainncv, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainbl, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snownc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%graupelnc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowncv, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%graupelncv, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nca, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lowlyr, 3, IWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mass_flux, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_gr, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_w, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_mc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_st, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_as, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capma, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capme, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capmi, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rthften, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvften, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthraten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthratenlw, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthratensw, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%cldfra, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%swdown, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%swdownc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%gsw, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%glw, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%swcf, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lwcf, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%olr, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xlat, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xlong, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%albedo, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%albbck, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%emiss, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cldefi, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rublten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvblten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthblten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvblten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqcblten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqiblten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%tmn, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xland, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%znt, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ust, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rmol, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mol, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%pblh, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%capg, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%thc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hfx, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qfx, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lh, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%flhc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%flqc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qsg, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qvg, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qcg, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt1, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsnav, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowc, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mavail, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tkesfcf, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%taucldi, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%taucldc, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor33, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%bn2, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rundgdten, 3, RWORDSIZE, 0, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvndgdten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthndgdten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvndgdten, 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rmundgdten, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_fdda3d
 CALL RSL_LITE_PACK ( local_communicator,fdda3d ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_fdda2d
 CALL RSL_LITE_PACK ( local_communicator,fdda2d ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 0, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%dpsdt, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%dmudt, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%pk1m, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mu_2m, 3, RWORDSIZE, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     56  &
   + num_moist   &
   + num_chem   &
   + num_scalar   &
   + num_ozmixm   &
   + num_aerosolc   &
   + num_aerosolc   &
   + num_fdda3d   &
   + num_fdda2d   &
     , 198, RWORDSIZE, &
     0, 11, IWORDSIZE, &
     0, 0, DWORDSIZE, &
      0,  0, LWORDSIZE, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
,kpe &
,config_flags%num_land_cat &
,config_flags%num_soil_cat &
,config_flags%num_soil_layers &
,config_flags%levsiz &
,config_flags%paerlev &
,1 &
))
CALL RSL_LITE_PACK ( local_communicator, grid%lu_index, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lu_mask, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nest_pos, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nest_mask, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_coarse, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%q2, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%t2, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%th2, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%psfc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%u10, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%v10, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uratx, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vratx, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tratx, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_nostag, 3, IWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_xstag, 3, IWORDSIZE, 1, 0, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_ystag, 3, IWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_xystag, 3, IWORDSIZE, 1, 0, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%sm000007, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm007028, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm028100, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm100255, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st000007, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st007028, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st028100, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st100255, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm000010, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm010040, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm040100, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm100200, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm010200, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm000, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm005, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm020, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm040, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm160, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm300, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw000010, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw010040, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw040100, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw100200, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw010200, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw000, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw005, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw020, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw040, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw160, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw300, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st000010, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st010040, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st040100, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st100200, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st010200, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt000, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt005, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt020, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt040, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt160, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt300, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%landmask, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%topostdv, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposlpx, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposlpy, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%shdmax, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%shdmin, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snoalb, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%slopecat, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposoil, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%landusef, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_land_cat,             &
ims, ime, jms, jme, 1, config_flags%num_land_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_land_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilctop, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_cat,             &
ims, ime, jms, jme, 1, config_flags%num_soil_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilcbot, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_cat,             &
ims, ime, jms, jme, 1, config_flags%num_soil_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilcat, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vegcat, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tslb, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%smois, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%sh2o, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%xice, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smstav, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smstot, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcrunoff, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%udrunoff, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ivgtyp, 3, IWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%isltyp, 3, IWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vegfra, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcevp, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%grdflx, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcexc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acsnow, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acsnom, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snow, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowh, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rhosn, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%canwat, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sst, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tr_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tb_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tg_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tc_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qc_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uc_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxr_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxb_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxg_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxc_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%trl_urb3d, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tbl_urb3d, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tgl_urb3d, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%sh_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lh_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%g_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rn_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ts_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%frc_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%utype_urb2d, 3, IWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cosz_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%omg_urb2d, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smfr3d, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%keepfr3dflag, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tke_myj, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%el_myj, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%exch_h, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%ct, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%thz0, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%z0, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qz0, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uz0, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vz0, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qsfc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%akhs, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%akms, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%kpbl, 3, IWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%htop, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hbot, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%htopr, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hbotr, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cutop, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cubot, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cuppt, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rswtoa, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rlwtoa, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%czmean, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfracl, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfracm, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfrach, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acfrst, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ncfrst, 3, IWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acfrcv, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ncfrcv, 3, IWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_ozmixm
 CALL RSL_LITE_PACK ( local_communicator,ozmixm ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%m_ps_1, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%m_ps_2, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
 CALL RSL_LITE_PACK ( local_communicator,aerosolc_1 ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
 CALL RSL_LITE_PACK ( local_communicator,aerosolc_2 ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%f_ice_phy, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%f_rain_phy, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%f_rimef_phy, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%h_diabatic, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%msft, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%msfu, 3, RWORDSIZE, 1, 0, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%msfv, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%f, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%e, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sina, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cosa, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_fine, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_int, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_input, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsk, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsk_save, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rthcuten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvcuten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqrcuten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqccuten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqscuten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqicuten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%w0avg, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rainc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainnc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%raincv, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainncv, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainbl, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snownc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%graupelnc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowncv, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%graupelncv, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nca, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lowlyr, 3, IWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mass_flux, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_gr, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_w, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_mc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_st, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_as, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capma, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capme, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capmi, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rthften, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvften, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthraten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthratenlw, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthratensw, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%cldfra, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%swdown, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%swdownc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%gsw, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%glw, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%swcf, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lwcf, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%olr, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xlat, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xlong, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%albedo, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%albbck, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%emiss, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cldefi, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rublten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvblten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthblten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvblten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqcblten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqiblten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%tmn, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xland, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%znt, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ust, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rmol, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mol, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%pblh, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%capg, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%thc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hfx, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qfx, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lh, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%flhc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%flqc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qsg, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qvg, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qcg, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt1, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsnav, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowc, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mavail, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tkesfcf, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%taucldi, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%taucldc, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor33, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%bn2, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rundgdten, 3, RWORDSIZE, 1, 0, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvndgdten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthndgdten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvndgdten, 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rmundgdten, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_fdda3d
 CALL RSL_LITE_PACK ( local_communicator,fdda3d ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_fdda2d
 CALL RSL_LITE_PACK ( local_communicator,fdda2d ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 0, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%dpsdt, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%dmudt, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%pk1m, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mu_2m, 3, RWORDSIZE, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%lu_index, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lu_mask, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nest_pos, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nest_mask, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_coarse, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%q2, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%t2, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%th2, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%psfc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%u10, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%v10, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uratx, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vratx, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tratx, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_nostag, 3, IWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_xstag, 3, IWORDSIZE, 1, 1, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_ystag, 3, IWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%imask_xystag, 3, IWORDSIZE, 1, 1, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_moist
 CALL RSL_LITE_PACK ( local_communicator,moist ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_chem
 CALL RSL_LITE_PACK ( local_communicator,chem ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
 CALL RSL_LITE_PACK ( local_communicator,scalar ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%sm000007, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm007028, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm028100, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm100255, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st000007, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st007028, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st028100, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st100255, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm000010, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm010040, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm040100, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm100200, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sm010200, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm000, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm005, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm020, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm040, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm160, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilm300, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw000010, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw010040, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw040100, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw100200, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sw010200, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw000, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw005, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw020, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw040, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw160, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilw300, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st000010, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st010040, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st040100, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st100200, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%st010200, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt000, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt005, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt020, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt040, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt160, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt300, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%landmask, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%topostdv, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposlpx, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposlpy, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%shdmax, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%shdmin, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snoalb, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%slopecat, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%toposoil, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%landusef, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_land_cat,             &
ims, ime, jms, jme, 1, config_flags%num_land_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_land_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilctop, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_cat,             &
ims, ime, jms, jme, 1, config_flags%num_soil_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilcbot, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_cat,             &
ims, ime, jms, jme, 1, config_flags%num_soil_cat,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_cat              )
CALL RSL_LITE_PACK ( local_communicator, grid%soilcat, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vegcat, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tslb, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%smois, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%sh2o, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%xice, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smstav, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smstot, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcrunoff, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%udrunoff, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ivgtyp, 3, IWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%isltyp, 3, IWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vegfra, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcevp, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%grdflx, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sfcexc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acsnow, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acsnom, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snow, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowh, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rhosn, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%canwat, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sst, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tr_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tb_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tg_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tc_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qc_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uc_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxr_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxb_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxg_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xxxc_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%trl_urb3d, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tbl_urb3d, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tgl_urb3d, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%sh_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lh_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%g_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rn_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ts_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%frc_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%utype_urb2d, 3, IWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cosz_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%omg_urb2d, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%smfr3d, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%keepfr3dflag, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1, config_flags%num_soil_layers,             &
ims, ime, jms, jme, 1, config_flags%num_soil_layers,             &
ips, ipe, jps, jpe, 1, config_flags%num_soil_layers              )
CALL RSL_LITE_PACK ( local_communicator, grid%tke_myj, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%el_myj, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%exch_h, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%ct, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%thz0, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%z0, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qz0, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%uz0, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%vz0, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qsfc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%akhs, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%akms, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%kpbl, 3, IWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%htop, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hbot, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%htopr, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hbotr, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cutop, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cubot, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cuppt, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rswtoa, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rlwtoa, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%czmean, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfracl, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfracm, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cfrach, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acfrst, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ncfrst, 3, IWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%acfrcv, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ncfrcv, 3, IWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_ozmixm
 CALL RSL_LITE_PACK ( local_communicator,ozmixm ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%m_ps_1, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%m_ps_2, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
 CALL RSL_LITE_PACK ( local_communicator,aerosolc_1 ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
 CALL RSL_LITE_PACK ( local_communicator,aerosolc_2 ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%f_ice_phy, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%f_rain_phy, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%f_rimef_phy, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%h_diabatic, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%msft, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%msfu, 3, RWORDSIZE, 1, 1, 'XY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%msfv, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%f, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%e, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%sina, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cosa, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_fine, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_int, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ht_input, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsk, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsk_save, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rthcuten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvcuten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqrcuten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqccuten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqscuten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqicuten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%w0avg, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rainc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainnc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%raincv, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainncv, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rainbl, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snownc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%graupelnc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowncv, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%graupelncv, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%nca, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lowlyr, 3, IWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mass_flux, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_gr, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_w, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_mc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_st, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_as, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capma, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capme, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%apr_capmi, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rthften, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvften, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthraten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthratenlw, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthratensw, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%cldfra, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%swdown, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%swdownc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%gsw, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%glw, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%swcf, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lwcf, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%olr, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xlat, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xlong, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%albedo, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%albbck, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%emiss, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%cldefi, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rublten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvblten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthblten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvblten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqcblten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqiblten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%tmn, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%xland, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%znt, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%ust, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%rmol, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mol, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%pblh, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%capg, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%thc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%hfx, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qfx, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%lh, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%flhc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%flqc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qsg, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qvg, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%qcg, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%soilt1, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tsnav, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%snowc, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mavail, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%tkesfcf, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%taucldi, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%taucldc, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor11, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor22, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor12, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor33, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor13, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%defor23, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmv, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmh, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkmhd, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhv, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%xkhh, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%div, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%bn2, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rundgdten, 3, RWORDSIZE, 1, 1, 'XZY', 1, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rvndgdten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rthndgdten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rqvndgdten, 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
CALL RSL_LITE_PACK ( local_communicator, grid%rmundgdten, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
DO itrace = PARAM_FIRST_SCALAR, num_fdda3d
 CALL RSL_LITE_PACK ( local_communicator,fdda3d ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_fdda2d
 CALL RSL_LITE_PACK ( local_communicator,fdda2d ( grid%sm31,grid%sm32,grid%sm33,itrace), 3, RWORDSIZE, 1, 1, 'XZY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, kds, kde,             &
ims, ime, jms, jme, kms, kme,             &
ips, ipe, jps, jpe, kps, kpe              )
ENDDO
CALL RSL_LITE_PACK ( local_communicator, grid%dpsdt, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%dmudt, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%pk1m, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_PACK ( local_communicator, grid%mu_2m, 3, RWORDSIZE, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE
grid%lu_index (ips:min(ide-1,ipe),jms:jme) = grid%lu_index (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%lu_mask (ips:min(ide-1,ipe),jms:jme) = grid%lu_mask (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%nest_pos (ips:min(ide-1,ipe),jms:jme) = grid%nest_pos (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%nest_mask (ips:min(ide-1,ipe),jms:jme) = grid%nest_mask (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ht_coarse (ips:min(ide-1,ipe),jms:jme) = grid%ht_coarse (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%q2 (ips:min(ide-1,ipe),jms:jme) = grid%q2 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%t2 (ips:min(ide-1,ipe),jms:jme) = grid%t2 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%th2 (ips:min(ide-1,ipe),jms:jme) = grid%th2 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%psfc (ips:min(ide-1,ipe),jms:jme) = grid%psfc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%u10 (ips:min(ide-1,ipe),jms:jme) = grid%u10 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%v10 (ips:min(ide-1,ipe),jms:jme) = grid%v10 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%uratx (ips:min(ide-1,ipe),jms:jme) = grid%uratx (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%vratx (ips:min(ide-1,ipe),jms:jme) = grid%vratx (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tratx (ips:min(ide-1,ipe),jms:jme) = grid%tratx (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%imask_nostag (ips:min(ide-1,ipe),jms:jme) = grid%imask_nostag (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%imask_xstag (ips:min(ide,ipe),jms:jme) = grid%imask_xstag (ips+px:min(ide,ipe)+px,jms:jme)
grid%imask_ystag (ips:min(ide-1,ipe),jms:jme) = grid%imask_ystag (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%imask_xystag (ips:min(ide,ipe),jms:jme) = grid%imask_xystag (ips+px:min(ide,ipe)+px,jms:jme)
  DO itrace = PARAM_FIRST_SCALAR, num_moist
   moist ( ips:min(ide-1,ipe),:,jms:jme,itrace) = moist (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_chem
   chem ( ips:min(ide-1,ipe),:,jms:jme,itrace) = chem (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_scalar
   scalar ( ips:min(ide-1,ipe),:,jms:jme,itrace) = scalar (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
  ENDDO
grid%sm000007 (ips:min(ide-1,ipe),jms:jme) = grid%sm000007 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sm007028 (ips:min(ide-1,ipe),jms:jme) = grid%sm007028 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sm028100 (ips:min(ide-1,ipe),jms:jme) = grid%sm028100 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sm100255 (ips:min(ide-1,ipe),jms:jme) = grid%sm100255 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%st000007 (ips:min(ide-1,ipe),jms:jme) = grid%st000007 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%st007028 (ips:min(ide-1,ipe),jms:jme) = grid%st007028 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%st028100 (ips:min(ide-1,ipe),jms:jme) = grid%st028100 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%st100255 (ips:min(ide-1,ipe),jms:jme) = grid%st100255 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sm000010 (ips:min(ide-1,ipe),jms:jme) = grid%sm000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sm010040 (ips:min(ide-1,ipe),jms:jme) = grid%sm010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sm040100 (ips:min(ide-1,ipe),jms:jme) = grid%sm040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sm100200 (ips:min(ide-1,ipe),jms:jme) = grid%sm100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sm010200 (ips:min(ide-1,ipe),jms:jme) = grid%sm010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilm000 (ips:min(ide-1,ipe),jms:jme) = grid%soilm000 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilm005 (ips:min(ide-1,ipe),jms:jme) = grid%soilm005 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilm020 (ips:min(ide-1,ipe),jms:jme) = grid%soilm020 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilm040 (ips:min(ide-1,ipe),jms:jme) = grid%soilm040 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilm160 (ips:min(ide-1,ipe),jms:jme) = grid%soilm160 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilm300 (ips:min(ide-1,ipe),jms:jme) = grid%soilm300 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sw000010 (ips:min(ide-1,ipe),jms:jme) = grid%sw000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sw010040 (ips:min(ide-1,ipe),jms:jme) = grid%sw010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sw040100 (ips:min(ide-1,ipe),jms:jme) = grid%sw040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sw100200 (ips:min(ide-1,ipe),jms:jme) = grid%sw100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sw010200 (ips:min(ide-1,ipe),jms:jme) = grid%sw010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilw000 (ips:min(ide-1,ipe),jms:jme) = grid%soilw000 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilw005 (ips:min(ide-1,ipe),jms:jme) = grid%soilw005 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilw020 (ips:min(ide-1,ipe),jms:jme) = grid%soilw020 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilw040 (ips:min(ide-1,ipe),jms:jme) = grid%soilw040 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilw160 (ips:min(ide-1,ipe),jms:jme) = grid%soilw160 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilw300 (ips:min(ide-1,ipe),jms:jme) = grid%soilw300 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%st000010 (ips:min(ide-1,ipe),jms:jme) = grid%st000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%st010040 (ips:min(ide-1,ipe),jms:jme) = grid%st010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%st040100 (ips:min(ide-1,ipe),jms:jme) = grid%st040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%st100200 (ips:min(ide-1,ipe),jms:jme) = grid%st100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%st010200 (ips:min(ide-1,ipe),jms:jme) = grid%st010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilt000 (ips:min(ide-1,ipe),jms:jme) = grid%soilt000 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilt005 (ips:min(ide-1,ipe),jms:jme) = grid%soilt005 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilt020 (ips:min(ide-1,ipe),jms:jme) = grid%soilt020 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilt040 (ips:min(ide-1,ipe),jms:jme) = grid%soilt040 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilt160 (ips:min(ide-1,ipe),jms:jme) = grid%soilt160 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilt300 (ips:min(ide-1,ipe),jms:jme) = grid%soilt300 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%landmask (ips:min(ide-1,ipe),jms:jme) = grid%landmask (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%topostdv (ips:min(ide-1,ipe),jms:jme) = grid%topostdv (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%toposlpx (ips:min(ide-1,ipe),jms:jme) = grid%toposlpx (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%toposlpy (ips:min(ide-1,ipe),jms:jme) = grid%toposlpy (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%shdmax (ips:min(ide-1,ipe),jms:jme) = grid%shdmax (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%shdmin (ips:min(ide-1,ipe),jms:jme) = grid%shdmin (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%snoalb (ips:min(ide-1,ipe),jms:jme) = grid%snoalb (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%slopecat (ips:min(ide-1,ipe),jms:jme) = grid%slopecat (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%toposoil (ips:min(ide-1,ipe),jms:jme) = grid%toposoil (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%landusef (ips:min(ide-1,ipe),:,jms:jme) = grid%landusef (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%soilctop (ips:min(ide-1,ipe),:,jms:jme) = grid%soilctop (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%soilcbot (ips:min(ide-1,ipe),:,jms:jme) = grid%soilcbot (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%soilcat (ips:min(ide-1,ipe),jms:jme) = grid%soilcat (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%vegcat (ips:min(ide-1,ipe),jms:jme) = grid%vegcat (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tslb (ips:min(ide-1,ipe),:,jms:jme) = grid%tslb (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%smois (ips:min(ide-1,ipe),:,jms:jme) = grid%smois (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%sh2o (ips:min(ide-1,ipe),:,jms:jme) = grid%sh2o (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%xice (ips:min(ide-1,ipe),jms:jme) = grid%xice (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%smstav (ips:min(ide-1,ipe),jms:jme) = grid%smstav (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%smstot (ips:min(ide-1,ipe),jms:jme) = grid%smstot (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sfcrunoff (ips:min(ide-1,ipe),jms:jme) = grid%sfcrunoff (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%udrunoff (ips:min(ide-1,ipe),jms:jme) = grid%udrunoff (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ivgtyp (ips:min(ide-1,ipe),jms:jme) = grid%ivgtyp (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%isltyp (ips:min(ide-1,ipe),jms:jme) = grid%isltyp (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%vegfra (ips:min(ide-1,ipe),jms:jme) = grid%vegfra (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sfcevp (ips:min(ide-1,ipe),jms:jme) = grid%sfcevp (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%grdflx (ips:min(ide-1,ipe),jms:jme) = grid%grdflx (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sfcexc (ips:min(ide-1,ipe),jms:jme) = grid%sfcexc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%acsnow (ips:min(ide-1,ipe),jms:jme) = grid%acsnow (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%acsnom (ips:min(ide-1,ipe),jms:jme) = grid%acsnom (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%snow (ips:min(ide-1,ipe),jms:jme) = grid%snow (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%snowh (ips:min(ide-1,ipe),jms:jme) = grid%snowh (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rhosn (ips:min(ide-1,ipe),jms:jme) = grid%rhosn (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%canwat (ips:min(ide-1,ipe),jms:jme) = grid%canwat (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sst (ips:min(ide-1,ipe),jms:jme) = grid%sst (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tr_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%tr_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tb_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%tb_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tg_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%tg_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tc_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%tc_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%qc_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%qc_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%uc_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%uc_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%xxxr_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%xxxr_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%xxxb_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%xxxb_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%xxxg_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%xxxg_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%xxxc_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%xxxc_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%trl_urb3d (ips:min(ide-1,ipe),:,jms:jme) = grid%trl_urb3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%tbl_urb3d (ips:min(ide-1,ipe),:,jms:jme) = grid%tbl_urb3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%tgl_urb3d (ips:min(ide-1,ipe),:,jms:jme) = grid%tgl_urb3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%sh_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%sh_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%lh_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%lh_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%g_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%g_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rn_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%rn_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ts_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%ts_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%frc_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%frc_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%utype_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%utype_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%cosz_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%cosz_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%omg_urb2d (ips:min(ide-1,ipe),jms:jme) = grid%omg_urb2d (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%smfr3d (ips:min(ide-1,ipe),:,jms:jme) = grid%smfr3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%keepfr3dflag (ips:min(ide-1,ipe),:,jms:jme) = grid%keepfr3dflag (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%tke_myj (ips:min(ide-1,ipe),:,jms:jme) = grid%tke_myj (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%el_myj (ips:min(ide-1,ipe),:,jms:jme) = grid%el_myj (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%exch_h (ips:min(ide-1,ipe),:,jms:jme) = grid%exch_h (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%ct (ips:min(ide-1,ipe),jms:jme) = grid%ct (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%thz0 (ips:min(ide-1,ipe),jms:jme) = grid%thz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%z0 (ips:min(ide-1,ipe),jms:jme) = grid%z0 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%qz0 (ips:min(ide-1,ipe),jms:jme) = grid%qz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%uz0 (ips:min(ide-1,ipe),jms:jme) = grid%uz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%vz0 (ips:min(ide-1,ipe),jms:jme) = grid%vz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%qsfc (ips:min(ide-1,ipe),jms:jme) = grid%qsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%akhs (ips:min(ide-1,ipe),jms:jme) = grid%akhs (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%akms (ips:min(ide-1,ipe),jms:jme) = grid%akms (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%kpbl (ips:min(ide-1,ipe),jms:jme) = grid%kpbl (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%htop (ips:min(ide-1,ipe),jms:jme) = grid%htop (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%hbot (ips:min(ide-1,ipe),jms:jme) = grid%hbot (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%htopr (ips:min(ide-1,ipe),jms:jme) = grid%htopr (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%hbotr (ips:min(ide-1,ipe),jms:jme) = grid%hbotr (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%cutop (ips:min(ide-1,ipe),jms:jme) = grid%cutop (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%cubot (ips:min(ide-1,ipe),jms:jme) = grid%cubot (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%cuppt (ips:min(ide-1,ipe),jms:jme) = grid%cuppt (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rswtoa (ips:min(ide-1,ipe),jms:jme) = grid%rswtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rlwtoa (ips:min(ide-1,ipe),jms:jme) = grid%rlwtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%czmean (ips:min(ide-1,ipe),jms:jme) = grid%czmean (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%cfracl (ips:min(ide-1,ipe),jms:jme) = grid%cfracl (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%cfracm (ips:min(ide-1,ipe),jms:jme) = grid%cfracm (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%cfrach (ips:min(ide-1,ipe),jms:jme) = grid%cfrach (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%acfrst (ips:min(ide-1,ipe),jms:jme) = grid%acfrst (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ncfrst (ips:min(ide-1,ipe),jms:jme) = grid%ncfrst (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%acfrcv (ips:min(ide-1,ipe),jms:jme) = grid%acfrcv (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ncfrcv (ips:min(ide-1,ipe),jms:jme) = grid%ncfrcv (ips+px:min(ide-1,ipe)+px,jms:jme)
  DO itrace = PARAM_FIRST_SCALAR, num_ozmixm
   ozmixm ( ips:min(ide-1,ipe),:,jms:jme,itrace) = ozmixm (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
  ENDDO
grid%m_ps_1 (ips:min(ide-1,ipe),jms:jme) = grid%m_ps_1 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%m_ps_2 (ips:min(ide-1,ipe),jms:jme) = grid%m_ps_2 (ips+px:min(ide-1,ipe)+px,jms:jme)
  DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
   aerosolc_1 ( ips:min(ide-1,ipe),:,jms:jme,itrace) = aerosolc_1 (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_aerosolc
   aerosolc_2 ( ips:min(ide-1,ipe),:,jms:jme,itrace) = aerosolc_2 (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
  ENDDO
grid%f_ice_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_ice_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%f_rain_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rain_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%f_rimef_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rimef_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%h_diabatic (ips:min(ide-1,ipe),:,jms:jme) = grid%h_diabatic (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%msft (ips:min(ide-1,ipe),jms:jme) = grid%msft (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%msfu (ips:min(ide,ipe),jms:jme) = grid%msfu (ips+px:min(ide,ipe)+px,jms:jme)
grid%msfv (ips:min(ide-1,ipe),jms:jme) = grid%msfv (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%f (ips:min(ide-1,ipe),jms:jme) = grid%f (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%e (ips:min(ide-1,ipe),jms:jme) = grid%e (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%sina (ips:min(ide-1,ipe),jms:jme) = grid%sina (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%cosa (ips:min(ide-1,ipe),jms:jme) = grid%cosa (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ht (ips:min(ide-1,ipe),jms:jme) = grid%ht (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ht_fine (ips:min(ide-1,ipe),jms:jme) = grid%ht_fine (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ht_int (ips:min(ide-1,ipe),jms:jme) = grid%ht_int (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ht_input (ips:min(ide-1,ipe),jms:jme) = grid%ht_input (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tsk (ips:min(ide-1,ipe),jms:jme) = grid%tsk (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tsk_save (ips:min(ide-1,ipe),jms:jme) = grid%tsk_save (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rthcuten (ips:min(ide-1,ipe),:,jms:jme) = grid%rthcuten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqvcuten (ips:min(ide-1,ipe),:,jms:jme) = grid%rqvcuten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqrcuten (ips:min(ide-1,ipe),:,jms:jme) = grid%rqrcuten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqccuten (ips:min(ide-1,ipe),:,jms:jme) = grid%rqccuten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqscuten (ips:min(ide-1,ipe),:,jms:jme) = grid%rqscuten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqicuten (ips:min(ide-1,ipe),:,jms:jme) = grid%rqicuten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%w0avg (ips:min(ide-1,ipe),:,jms:jme) = grid%w0avg (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rainc (ips:min(ide-1,ipe),jms:jme) = grid%rainc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rainnc (ips:min(ide-1,ipe),jms:jme) = grid%rainnc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%raincv (ips:min(ide-1,ipe),jms:jme) = grid%raincv (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rainncv (ips:min(ide-1,ipe),jms:jme) = grid%rainncv (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rainbl (ips:min(ide-1,ipe),jms:jme) = grid%rainbl (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%snownc (ips:min(ide-1,ipe),jms:jme) = grid%snownc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%graupelnc (ips:min(ide-1,ipe),jms:jme) = grid%graupelnc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%snowncv (ips:min(ide-1,ipe),jms:jme) = grid%snowncv (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%graupelncv (ips:min(ide-1,ipe),jms:jme) = grid%graupelncv (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%nca (ips:min(ide-1,ipe),jms:jme) = grid%nca (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%lowlyr (ips:min(ide-1,ipe),jms:jme) = grid%lowlyr (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%mass_flux (ips:min(ide-1,ipe),jms:jme) = grid%mass_flux (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%apr_gr (ips:min(ide-1,ipe),jms:jme) = grid%apr_gr (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%apr_w (ips:min(ide-1,ipe),jms:jme) = grid%apr_w (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%apr_mc (ips:min(ide-1,ipe),jms:jme) = grid%apr_mc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%apr_st (ips:min(ide-1,ipe),jms:jme) = grid%apr_st (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%apr_as (ips:min(ide-1,ipe),jms:jme) = grid%apr_as (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%apr_capma (ips:min(ide-1,ipe),jms:jme) = grid%apr_capma (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%apr_capme (ips:min(ide-1,ipe),jms:jme) = grid%apr_capme (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%apr_capmi (ips:min(ide-1,ipe),jms:jme) = grid%apr_capmi (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rthften (ips:min(ide-1,ipe),:,jms:jme) = grid%rthften (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqvften (ips:min(ide-1,ipe),:,jms:jme) = grid%rqvften (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rthraten (ips:min(ide-1,ipe),:,jms:jme) = grid%rthraten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rthratenlw (ips:min(ide-1,ipe),:,jms:jme) = grid%rthratenlw (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rthratensw (ips:min(ide-1,ipe),:,jms:jme) = grid%rthratensw (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%cldfra (ips:min(ide-1,ipe),:,jms:jme) = grid%cldfra (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%swdown (ips:min(ide-1,ipe),jms:jme) = grid%swdown (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%swdownc (ips:min(ide-1,ipe),jms:jme) = grid%swdownc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%gsw (ips:min(ide-1,ipe),jms:jme) = grid%gsw (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%glw (ips:min(ide-1,ipe),jms:jme) = grid%glw (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%swcf (ips:min(ide-1,ipe),jms:jme) = grid%swcf (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%lwcf (ips:min(ide-1,ipe),jms:jme) = grid%lwcf (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%olr (ips:min(ide-1,ipe),jms:jme) = grid%olr (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%xlat (ips:min(ide-1,ipe),jms:jme) = grid%xlat (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%xlong (ips:min(ide-1,ipe),jms:jme) = grid%xlong (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%albedo (ips:min(ide-1,ipe),jms:jme) = grid%albedo (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%albbck (ips:min(ide-1,ipe),jms:jme) = grid%albbck (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%emiss (ips:min(ide-1,ipe),jms:jme) = grid%emiss (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%cldefi (ips:min(ide-1,ipe),jms:jme) = grid%cldefi (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rublten (ips:min(ide-1,ipe),:,jms:jme) = grid%rublten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rvblten (ips:min(ide-1,ipe),:,jms:jme) = grid%rvblten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rthblten (ips:min(ide-1,ipe),:,jms:jme) = grid%rthblten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqvblten (ips:min(ide-1,ipe),:,jms:jme) = grid%rqvblten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqcblten (ips:min(ide-1,ipe),:,jms:jme) = grid%rqcblten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqiblten (ips:min(ide-1,ipe),:,jms:jme) = grid%rqiblten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%tmn (ips:min(ide-1,ipe),jms:jme) = grid%tmn (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%xland (ips:min(ide-1,ipe),jms:jme) = grid%xland (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%znt (ips:min(ide-1,ipe),jms:jme) = grid%znt (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%ust (ips:min(ide-1,ipe),jms:jme) = grid%ust (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%rmol (ips:min(ide-1,ipe),jms:jme) = grid%rmol (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%mol (ips:min(ide-1,ipe),jms:jme) = grid%mol (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%pblh (ips:min(ide-1,ipe),jms:jme) = grid%pblh (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%capg (ips:min(ide-1,ipe),jms:jme) = grid%capg (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%thc (ips:min(ide-1,ipe),jms:jme) = grid%thc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%hfx (ips:min(ide-1,ipe),jms:jme) = grid%hfx (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%qfx (ips:min(ide-1,ipe),jms:jme) = grid%qfx (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%lh (ips:min(ide-1,ipe),jms:jme) = grid%lh (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%flhc (ips:min(ide-1,ipe),jms:jme) = grid%flhc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%flqc (ips:min(ide-1,ipe),jms:jme) = grid%flqc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%qsg (ips:min(ide-1,ipe),jms:jme) = grid%qsg (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%qvg (ips:min(ide-1,ipe),jms:jme) = grid%qvg (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%qcg (ips:min(ide-1,ipe),jms:jme) = grid%qcg (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%soilt1 (ips:min(ide-1,ipe),jms:jme) = grid%soilt1 (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tsnav (ips:min(ide-1,ipe),jms:jme) = grid%tsnav (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%snowc (ips:min(ide-1,ipe),jms:jme) = grid%snowc (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%mavail (ips:min(ide-1,ipe),jms:jme) = grid%mavail (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%tkesfcf (ips:min(ide-1,ipe),jms:jme) = grid%tkesfcf (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%taucldi (ips:min(ide-1,ipe),:,jms:jme) = grid%taucldi (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%taucldc (ips:min(ide-1,ipe),:,jms:jme) = grid%taucldc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%defor11 (ips:min(ide-1,ipe),:,jms:jme) = grid%defor11 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%defor22 (ips:min(ide-1,ipe),:,jms:jme) = grid%defor22 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%defor12 (ips:min(ide-1,ipe),:,jms:jme) = grid%defor12 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%defor33 (ips:min(ide-1,ipe),:,jms:jme) = grid%defor33 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%defor13 (ips:min(ide-1,ipe),:,jms:jme) = grid%defor13 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%defor23 (ips:min(ide-1,ipe),:,jms:jme) = grid%defor23 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%xkmv (ips:min(ide-1,ipe),:,jms:jme) = grid%xkmv (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%xkmh (ips:min(ide-1,ipe),:,jms:jme) = grid%xkmh (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%xkmhd (ips:min(ide-1,ipe),:,jms:jme) = grid%xkmhd (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%xkhv (ips:min(ide-1,ipe),:,jms:jme) = grid%xkhv (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%xkhh (ips:min(ide-1,ipe),:,jms:jme) = grid%xkhh (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%div (ips:min(ide-1,ipe),:,jms:jme) = grid%div (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%bn2 (ips:min(ide-1,ipe),:,jms:jme) = grid%bn2 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rundgdten (ips:min(ide,ipe),:,jms:jme) = grid%rundgdten (ips+px:min(ide,ipe)+px,:,jms:jme)
grid%rvndgdten (ips:min(ide-1,ipe),:,jms:jme) = grid%rvndgdten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rthndgdten (ips:min(ide-1,ipe),:,jms:jme) = grid%rthndgdten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rqvndgdten (ips:min(ide-1,ipe),:,jms:jme) = grid%rqvndgdten (ips+px:min(ide-1,ipe)+px,:,jms:jme)
grid%rmundgdten (ips:min(ide-1,ipe),jms:jme) = grid%rmundgdten (ips+px:min(ide-1,ipe)+px,jms:jme)
  DO itrace = PARAM_FIRST_SCALAR, num_fdda3d
   fdda3d ( ips:min(ide-1,ipe),:,jms:jme,itrace) = fdda3d (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_fdda2d
   fdda2d ( ips:min(ide-1,ipe),:,jms:jme,itrace) = fdda2d (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
  ENDDO
grid%dpsdt (ips:min(ide-1,ipe),jms:jme) = grid%dpsdt (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%dmudt (ips:min(ide-1,ipe),jms:jme) = grid%dmudt (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%pk1m (ips:min(ide-1,ipe),jms:jme) = grid%pk1m (ips+px:min(ide-1,ipe)+px,jms:jme)
grid%mu_2m (ips:min(ide-1,ipe),jms:jme) = grid%mu_2m (ips+px:min(ide-1,ipe)+px,jms:jme)
!ENDOFREGISTRYGENERATEDINCLUDE
