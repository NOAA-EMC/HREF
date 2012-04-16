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
          grid%moist(ims,jms,kms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XYZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF (BTEST(dfi_moist_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )), & !data name
          grid%dfi_moist(ims,jms,kms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XYZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
          dfi_moist_desc_table( grid%id, itrace  ), & ! Desc
          dfi_moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )), & !data name
          grid%scalar(ims,jms,kms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XYZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF (BTEST(dfi_scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )), & !data name
          grid%dfi_scalar(ims,kms,jms,itrace)  , &  ! Field
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
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_metaput_restart.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
CALL rconfig_get_tstart ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'TSTART', rbuf(1), 1, ierr )
CALL rconfig_get_dfi_opt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_OPT', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_nfilter ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_NFILTER', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_write_filtered_input ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'DFI_WRITE_FILTERED_INPUT', lbuf(1), 1, ierr )
CALL rconfig_get_dfi_write_dfi_history ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'DFI_WRITE_DFI_HISTORY', lbuf(1), 1, ierr )
CALL rconfig_get_dfi_cutoff_seconds ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_CUTOFF_SECONDS', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_time_dim ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_TIME_DIM', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_fwdstop_year ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_FWDSTOP_YEAR', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_fwdstop_month ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_FWDSTOP_MONTH', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_fwdstop_day ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_FWDSTOP_DAY', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_fwdstop_hour ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_FWDSTOP_HOUR', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_fwdstop_minute ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_FWDSTOP_MINUTE', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_fwdstop_second ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_FWDSTOP_SECOND', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_bckstop_year ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_BCKSTOP_YEAR', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_bckstop_month ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_BCKSTOP_MONTH', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_bckstop_day ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_BCKSTOP_DAY', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_bckstop_hour ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_BCKSTOP_HOUR', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_bckstop_minute ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_BCKSTOP_MINUTE', ibuf(1), 1, ierr )
CALL rconfig_get_dfi_bckstop_second ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'DFI_BCKSTOP_SECOND', ibuf(1), 1, ierr )
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
CALL rconfig_get_grid_id ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'ID', ibuf(1), 1, ierr )
CALL rconfig_get_grid_allowed ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'ALLOWED', lbuf(1), 1, ierr )
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
CALL rconfig_get_num_metgrid_levels ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NUM_METGRID_LEVELS', ibuf(1), 1, ierr )
CALL rconfig_get_p_top_requested ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'P_TOP_REQUESTED', rbuf(1), 1, ierr )
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
CALL rconfig_get_sf_urban_physics ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'SF_URBAN_PHYSICS', ibuf(1), 1, ierr )
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
CALL rconfig_get_num_urban_layers ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NUM_URBAN_LAYERS', ibuf(1), 1, ierr )
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
CALL rconfig_get_gwd_opt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'GWD_OPT', ibuf(1), 1, ierr )
CALL rconfig_get_idtad ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'IDTAD', ibuf(1), 1, ierr )
CALL rconfig_get_nsoil ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NSOIL', ibuf(1), 1, ierr )
CALL rconfig_get_nphs ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NPHS', ibuf(1), 1, ierr )
CALL rconfig_get_ncnvc ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NCNVC', ibuf(1), 1, ierr )
CALL rconfig_get_nrads ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NRADS', ibuf(1), 1, ierr )
CALL rconfig_get_nradl ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'NRADL', ibuf(1), 1, ierr )
CALL rconfig_get_tprec ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'TPREC', rbuf(1), 1, ierr )
CALL rconfig_get_theat ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'THEAT', rbuf(1), 1, ierr )
CALL rconfig_get_tclod ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'TCLOD', rbuf(1), 1, ierr )
CALL rconfig_get_trdsw ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'TRDSW', rbuf(1), 1, ierr )
CALL rconfig_get_trdlw ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'TRDLW', rbuf(1), 1, ierr )
CALL rconfig_get_tsrfc ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'TSRFC', rbuf(1), 1, ierr )
CALL rconfig_get_pcpflg ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'PCPFLG', lbuf(1), 1, ierr )
CALL rconfig_get_sigma ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'SIGMA', ibuf(1), 1, ierr )
CALL rconfig_get_sfenth ( grid%id, rbuf(1) )
 CALL wrf_put_dom_ti_real ( fid , 'SEA SPRAY PARAMETER', rbuf(1), 1, ierr )
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
CALL rconfig_get_top_radiation ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'TOP_RADIATION', lbuf(1), 1, ierr )
CALL rconfig_get_pert_coriolis ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'PERT_CORIOLIS', lbuf(1), 1, ierr )
CALL rconfig_get_euler_adv ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'EULER_ADV', lbuf(1), 1, ierr )
CALL rconfig_get_idtadt ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'IDTADT', ibuf(1), 1, ierr )
CALL rconfig_get_idtadc ( grid%id, ibuf(1) )
 CALL wrf_put_dom_ti_integer ( fid , 'IDTADC', ibuf(1), 1, ierr )
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
CALL rconfig_get_polar ( grid%id, lbuf(1) )
 CALL wrf_put_dom_ti_logical ( fid , 'POLAR', lbuf(1), 1, ierr )
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
