!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_metaput_restart.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF (BTEST(moist_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )), & !data name
          grid%moist(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_chem
  IF (BTEST(chem_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(chem_dname_table( grid%id, itrace )), & !data name
          grid%chem(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
          chem_desc_table( grid%id, itrace  ), & ! Desc
          chem_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )), & !data name
          grid%scalar(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_ozmixm
  IF (BTEST(ozmixm_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(ozmixm_dname_table( grid%id, itrace )), & !data name
          grid%ozmixm(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'levsiz'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
          ozmixm_desc_table( grid%id, itrace  ), & ! Desc
          ozmixm_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(ozmixm_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%levsiz , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%levsiz , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%levsiz , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_aerosolc
  IF (BTEST(aerosolc_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(aerosolc_dname_table( grid%id, itrace )), & !data name
          grid%aerosolc_2(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'paerlev'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
          aerosolc_desc_table( grid%id, itrace  ), & ! Desc
          aerosolc_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(aerosolc_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%paerlev , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%paerlev , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%paerlev , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_fdda3d
  IF (BTEST(fdda3d_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(fdda3d_dname_table( grid%id, itrace )), & !data name
          grid%fdda3d(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
          fdda3d_desc_table( grid%id, itrace  ), & ! Desc
          fdda3d_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(fdda3d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_fdda2d
  IF (BTEST(fdda2d_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(fdda2d_dname_table( grid%id, itrace )), & !data name
          grid%fdda2d(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XZY'               , &  ! MemoryOrder
          'Z'                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'one_stag'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
          fdda2d_desc_table( grid%id, itrace  ), & ! Desc
          fdda2d_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(fdda2d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , 1 , jds , (jde-1) ,  & 
ims , ime , 1 , 1 , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , 1 , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
CALL rconfig_get_run_days ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'RUN_DAYS', ibuf(1), 1, ierr )
CALL rconfig_get_run_hours ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'RUN_HOURS', ibuf(1), 1, ierr )
CALL rconfig_get_run_minutes ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'RUN_MINUTES', ibuf(1), 1, ierr )
CALL rconfig_get_run_seconds ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'RUN_SECONDS', ibuf(1), 1, ierr )
CALL rconfig_get_start_year ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'START_YEAR', ibuf(1), 1, ierr )
CALL rconfig_get_start_month ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'START_MONTH', ibuf(1), 1, ierr )
CALL rconfig_get_start_day ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'START_DAY', ibuf(1), 1, ierr )
CALL rconfig_get_start_hour ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'START_HOUR', ibuf(1), 1, ierr )
CALL rconfig_get_start_minute ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'START_MINUTE', ibuf(1), 1, ierr )
CALL rconfig_get_start_second ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'START_SECOND', ibuf(1), 1, ierr )
CALL rconfig_get_end_year ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'END_YEAR', ibuf(1), 1, ierr )
CALL rconfig_get_end_month ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'END_MONTH', ibuf(1), 1, ierr )
CALL rconfig_get_end_day ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'END_DAY', ibuf(1), 1, ierr )
CALL rconfig_get_end_hour ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'END_HOUR', ibuf(1), 1, ierr )
CALL rconfig_get_end_minute ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'END_MINUTE', ibuf(1), 1, ierr )
CALL rconfig_get_end_second ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'END_SECOND', ibuf(1), 1, ierr )
CALL rconfig_get_interval_seconds ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'INTERVAL_SECONDS', ibuf(1), 1, ierr )
CALL rconfig_get_input_from_file ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'INPUT_FROM_FILE', lbuf(1), 1, ierr )
CALL rconfig_get_fine_input_stream ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'FINE_INPUT_STREAM', ibuf(1), 1, ierr )
CALL rconfig_get_input_from_hires ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'INPUT_FROM_HIRES', lbuf(1), 1, ierr )
CALL rconfig_get_max_dom ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'MAX_DOM', ibuf(1), 1, ierr )
CALL rconfig_get_s_we ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'S_WE', ibuf(1), 1, ierr )
CALL rconfig_get_e_we ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'E_WE', ibuf(1), 1, ierr )
CALL rconfig_get_s_sn ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'S_SN', ibuf(1), 1, ierr )
CALL rconfig_get_e_sn ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'E_SN', ibuf(1), 1, ierr )
CALL rconfig_get_s_vert ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'S_VERT', ibuf(1), 1, ierr )
CALL rconfig_get_e_vert ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'E_VERT', ibuf(1), 1, ierr )
CALL rconfig_get_num_metgrid_levels ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NUM_METGRID_LEVELS', ibuf(1), 1, ierr )
CALL rconfig_get_p_top_requested ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'P_TOP_REQUESTED', rbuf(1), 1, ierr )
CALL rconfig_get_interp_type ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'INTERP_TYPE', ibuf(1), 1, ierr )
CALL rconfig_get_lowest_lev_from_sfc ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'LOWEST_LEV_FROM_SFC', lbuf(1), 1, ierr )
CALL rconfig_get_lagrange_order ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'LAGRANGE_ORDER', ibuf(1), 1, ierr )
CALL rconfig_get_force_sfc_in_vinterp ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'FORCE_SFC_IN_VINTERP', ibuf(1), 1, ierr )
CALL rconfig_get_zap_close_levels ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'ZAP_CLOSE_LEVELS', rbuf(1), 1, ierr )
CALL rconfig_get_sfcp_to_sfcp ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'AFCP_TO_SFCP', lbuf(1), 1, ierr )
CALL rconfig_get_adjust_heights ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'ADJUST_HEIGHTS', lbuf(1), 1, ierr )
CALL rconfig_get_grid_id ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'ID', ibuf(1), 1, ierr )
CALL rconfig_get_i_parent_start ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'I_PARENT_START', ibuf(1), 1, ierr )
CALL rconfig_get_j_parent_start ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'J_PARENT_START', ibuf(1), 1, ierr )
CALL rconfig_get_swap_x ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'SWAP_X', lbuf(1), 1, ierr )
CALL rconfig_get_swap_y ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'SWAP_Y', lbuf(1), 1, ierr )
CALL rconfig_get_cycle_x ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'CYCLE_X', lbuf(1), 1, ierr )
CALL rconfig_get_cycle_y ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'CYCLE_Y', lbuf(1), 1, ierr )
CALL rconfig_get_reorder_mesh ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'REORDER_MESH', lbuf(1), 1, ierr )
CALL rconfig_get_mp_physics ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'MP_PHYSICS', ibuf(1), 1, ierr )
CALL rconfig_get_ra_lw_physics ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'RA_LW_PHYSICS', ibuf(1), 1, ierr )
CALL rconfig_get_ra_sw_physics ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'RA_SW_PHYSICS', ibuf(1), 1, ierr )
CALL rconfig_get_sf_sfclay_physics ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'SF_SFCLAY_PHYSICS', ibuf(1), 1, ierr )
CALL rconfig_get_sf_surface_physics ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'SF_SURFACE_PHYSICS', ibuf(1), 1, ierr )
CALL rconfig_get_bl_pbl_physics ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'BL_PBL_PHYSICS', ibuf(1), 1, ierr )
CALL rconfig_get_cu_physics ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'CU_PHYSICS', ibuf(1), 1, ierr )
CALL rconfig_get_isfflx ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'ISFFLX', ibuf(1), 1, ierr )
CALL rconfig_get_ifsnow ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'IFSNOW', ibuf(1), 1, ierr )
CALL rconfig_get_icloud ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'ICLOUD', ibuf(1), 1, ierr )
CALL rconfig_get_swrad_scat ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'SWRAD_SCAT', rbuf(1), 1, ierr )
CALL rconfig_get_surface_input_source ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'SURFACE_INPUT_SOURCE', ibuf(1), 1, ierr )
CALL rconfig_get_num_soil_layers ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NUM_SOIL_LAYERS', ibuf(1), 1, ierr )
CALL rconfig_get_num_months ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NUM_MONTHS', ibuf(1), 1, ierr )
CALL rconfig_get_maxiens ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'MAXIENS', ibuf(1), 1, ierr )
CALL rconfig_get_maxens ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'MAXENS', ibuf(1), 1, ierr )
CALL rconfig_get_maxens2 ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'MAXENS2', ibuf(1), 1, ierr )
CALL rconfig_get_maxens3 ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'MAXENS3', ibuf(1), 1, ierr )
CALL rconfig_get_ensdim ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'ENSDIM', ibuf(1), 1, ierr )
CALL rconfig_get_chem_opt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'CHEM_OPT', ibuf(1), 1, ierr )
CALL rconfig_get_grid_fdda ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_if_no_pbl_nudging_uv ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_if_no_pbl_nudging_t ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_if_no_pbl_nudging_q ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_if_zfac_uv ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_k_zfac_uv ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_if_zfac_t ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_k_zfac_t ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_if_zfac_q ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_k_zfac_q ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
CALL rconfig_get_guv ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'GRID_FDDA', rbuf(1), 1, ierr )
CALL rconfig_get_gt ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'GRID_FDDA', rbuf(1), 1, ierr )
CALL rconfig_get_gq ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'GRID_FDDA', rbuf(1), 1, ierr )
CALL rconfig_get_obs_nudge_opt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'OBS_NUDGE_OPT', ibuf(1), 1, ierr )
CALL rconfig_get_fdda_start ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'FDDA_START', rbuf(1), 1, ierr )
CALL rconfig_get_fdda_end ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'FDDA_END', rbuf(1), 1, ierr )
CALL rconfig_get_obs_nudge_wind ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'OBS_NUDGE_WIND', ibuf(1), 1, ierr )
CALL rconfig_get_obs_coef_wind ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'OBS_COEF_WIND', rbuf(1), 1, ierr )
CALL rconfig_get_obs_nudge_temp ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'OBS_NUDGE_TEMP', ibuf(1), 1, ierr )
CALL rconfig_get_obs_coef_temp ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'OBS_COEF_TEMP', rbuf(1), 1, ierr )
CALL rconfig_get_obs_nudge_mois ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'OBS_NUDGE_MOIS', ibuf(1), 1, ierr )
CALL rconfig_get_obs_coef_mois ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'OBS_COEF_MOIS', rbuf(1), 1, ierr )
CALL rconfig_get_obs_nudge_pstr ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'OBS_NUDGE_PSTR', ibuf(1), 1, ierr )
CALL rconfig_get_obs_coef_pstr ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'OBS_COEF_PSTR', rbuf(1), 1, ierr )
CALL rconfig_get_obs_rinxy ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'OBS_RINXY', rbuf(1), 1, ierr )
CALL rconfig_get_dyn_opt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DYN_OPT', ibuf(1), 1, ierr )
CALL rconfig_get_rk_ord ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'RK_ORDER', ibuf(1), 1, ierr )
CALL rconfig_get_w_damping ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'W_DAMPING', ibuf(1), 1, ierr )
CALL rconfig_get_diff_opt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DIFF_OPT', ibuf(1), 1, ierr )
CALL rconfig_get_km_opt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'KM_OPT', ibuf(1), 1, ierr )
CALL rconfig_get_damp_opt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DAMP_OPT', ibuf(1), 1, ierr )
CALL rconfig_get_diff_6th_opt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DIFF_6TH_OPT', ibuf(1), 1, ierr )
CALL rconfig_get_non_hydrostatic ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'NON_HYDROSTATIC', lbuf(1), 1, ierr )
CALL rconfig_get_h_mom_adv_order ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'H_MOM_ADV_ORDER', ibuf(1), 1, ierr )
CALL rconfig_get_v_mom_adv_order ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'V_MOM_ADV_ORDER', ibuf(1), 1, ierr )
CALL rconfig_get_h_sca_adv_order ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'H_SCA_ADV_ORDER', ibuf(1), 1, ierr )
CALL rconfig_get_v_sca_adv_order ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'V_SCA_ADV_ORDER', ibuf(1), 1, ierr )
CALL rconfig_get_pd_moist ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'PD_MOIST', lbuf(1), 1, ierr )
CALL rconfig_get_pd_chem ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'PD_CHEM', lbuf(1), 1, ierr )
CALL rconfig_get_pd_scalar ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'PD_SCALAR', lbuf(1), 1, ierr )
CALL rconfig_get_pd_tke ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'PD_TKE', lbuf(1), 1, ierr )
CALL rconfig_get_top_radiation ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'TOP_RADIATION', lbuf(1), 1, ierr )
CALL rconfig_get_pert_coriolis ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'PERT_CORIOLIS', lbuf(1), 1, ierr )
CALL rconfig_get_mix_full_fields ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'MIX_FULL_FIELD', lbuf(1), 1, ierr )
CALL rconfig_get_spec_bdy_width ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'SPEC_BDY_WIDTH', ibuf(1), 1, ierr )
CALL rconfig_get_spec_zone ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'SPEC_ZONE', ibuf(1), 1, ierr )
CALL rconfig_get_relax_zone ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'RELAX_ZONE', ibuf(1), 1, ierr )
CALL rconfig_get_specified ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'SPECIFIED', lbuf(1), 1, ierr )
CALL rconfig_get_periodic_x ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'PERIODIC_X', lbuf(1), 1, ierr )
CALL rconfig_get_symmetric_xs ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'SYMMETRIC_XS', lbuf(1), 1, ierr )
CALL rconfig_get_symmetric_xe ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'SYMMETRIC_XE', lbuf(1), 1, ierr )
CALL rconfig_get_open_xs ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'OPEN_XS', lbuf(1), 1, ierr )
CALL rconfig_get_open_xe ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'OPEN_XE', lbuf(1), 1, ierr )
CALL rconfig_get_periodic_y ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'PERIODIC_Y', lbuf(1), 1, ierr )
CALL rconfig_get_symmetric_ys ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'SYMMETRIC_YS', lbuf(1), 1, ierr )
CALL rconfig_get_symmetric_ye ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'SYMMETRIC_YE', lbuf(1), 1, ierr )
CALL rconfig_get_open_ys ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'OPEN_YS', lbuf(1), 1, ierr )
CALL rconfig_get_open_ye ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'OPEN_YE', lbuf(1), 1, ierr )
CALL rconfig_get_nested ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'NESTED', lbuf(1), 1, ierr )
CALL rconfig_get_real_data_init_type ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'REAL_DATA_INIT_TYPE', ibuf(1), 1, ierr )
CALL rconfig_get_background_proc_id ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'BACKGROUND_PROC_ID', ibuf(1), 1, ierr )
CALL rconfig_get_forecast_proc_id ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'FORECAST_PROC_ID', ibuf(1), 1, ierr )
CALL rconfig_get_production_status ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'PRODUCTION_STATUS', ibuf(1), 1, ierr )
CALL rconfig_get_compression ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'COMPRESSION', ibuf(1), 1, ierr )
