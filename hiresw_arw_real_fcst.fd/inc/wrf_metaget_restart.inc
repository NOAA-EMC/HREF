!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_metaget_restart.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF (BTEST(moist_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )), & !data name
          grid%moist(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_restart.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_chem
  IF (BTEST(chem_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(chem_dname_table( grid%id, itrace )), & !data name
          grid%chem(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_restart.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )), & !data name
          grid%scalar(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_restart.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_ozmixm
  IF (BTEST(ozmixm_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(ozmixm_dname_table( grid%id, itrace )), & !data name
          grid%ozmixm(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_restart.inc ext_write_field '//TRIM(ozmixm_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%levsiz , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%levsiz , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%levsiz , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_aerosolc
  IF (BTEST(aerosolc_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(aerosolc_dname_table( grid%id, itrace )), & !data name
          grid%aerosolc_2(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_restart.inc ext_write_field '//TRIM(aerosolc_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%paerlev , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%paerlev , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%paerlev , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_fdda3d
  IF (BTEST(fdda3d_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(fdda3d_dname_table( grid%id, itrace )), & !data name
          grid%fdda3d(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_restart.inc ext_write_field '//TRIM(fdda3d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_fdda2d
  IF (BTEST(fdda2d_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(fdda2d_dname_table( grid%id, itrace )), & !data name
          grid%fdda2d(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XZY'               , &  ! MemoryOrder
          'Z'                , &  ! Stagger
'inc/wrf_metaget_restart.inc ext_write_field '//TRIM(fdda2d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , 1 , jds , (jde-1) ,  & 
ims , ime , 1 , 1 , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , 1 , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
CALL wrf_get_dom_ti_integer ( fid , 'RUN_DAYS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for RUN_DAYS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_run_days ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'RUN_HOURS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for RUN_HOURS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_run_hours ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'RUN_MINUTES', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for RUN_MINUTES returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_run_minutes ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'RUN_SECONDS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for RUN_SECONDS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_run_seconds ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'START_YEAR', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for START_YEAR returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_start_year ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'START_MONTH', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for START_MONTH returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_start_month ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'START_DAY', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for START_DAY returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_start_day ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'START_HOUR', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for START_HOUR returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_start_hour ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'START_MINUTE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for START_MINUTE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_start_minute ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'START_SECOND', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for START_SECOND returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_start_second ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'END_YEAR', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for END_YEAR returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_end_year ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'END_MONTH', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for END_MONTH returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_end_month ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'END_DAY', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for END_DAY returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_end_day ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'END_HOUR', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for END_HOUR returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_end_hour ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'END_MINUTE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for END_MINUTE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_end_minute ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'END_SECOND', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for END_SECOND returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_end_second ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'INTERVAL_SECONDS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for INTERVAL_SECONDS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_interval_seconds ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'INPUT_FROM_FILE', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for INPUT_FROM_FILE returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_input_from_file ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'FINE_INPUT_STREAM', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for FINE_INPUT_STREAM returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_fine_input_stream ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'INPUT_FROM_HIRES', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for INPUT_FROM_HIRES returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_input_from_hires ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'MAX_DOM', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAX_DOM returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_max_dom ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'S_WE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for S_WE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_s_we ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'E_WE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for E_WE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_e_we ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'S_SN', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for S_SN returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_s_sn ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'E_SN', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for E_SN returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_e_sn ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'S_VERT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for S_VERT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_s_vert ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'E_VERT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for E_VERT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_e_vert ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'NUM_METGRID_LEVELS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NUM_METGRID_LEVELS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_num_metgrid_levels ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'P_TOP_REQUESTED', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for P_TOP_REQUESTED returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_p_top_requested ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'INTERP_TYPE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for INTERP_TYPE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_interp_type ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'LOWEST_LEV_FROM_SFC', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for LOWEST_LEV_FROM_SFC returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_lowest_lev_from_sfc ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'LAGRANGE_ORDER', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for LAGRANGE_ORDER returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_lagrange_order ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'FORCE_SFC_IN_VINTERP', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for FORCE_SFC_IN_VINTERP returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_force_sfc_in_vinterp ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'ZAP_CLOSE_LEVELS', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for ZAP_CLOSE_LEVELS returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_zap_close_levels ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'AFCP_TO_SFCP', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for AFCP_TO_SFCP returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_sfcp_to_sfcp ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'ADJUST_HEIGHTS', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for ADJUST_HEIGHTS returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_adjust_heights ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'ID', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ID returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_grid_id ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'I_PARENT_START', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for I_PARENT_START returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_i_parent_start ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'J_PARENT_START', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for J_PARENT_START returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_j_parent_start ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'SWAP_X', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for SWAP_X returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_swap_x ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'SWAP_Y', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for SWAP_Y returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_swap_y ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'CYCLE_X', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for CYCLE_X returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_cycle_x ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'CYCLE_Y', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for CYCLE_Y returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_cycle_y ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'REORDER_MESH', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for REORDER_MESH returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_reorder_mesh ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'MP_PHYSICS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MP_PHYSICS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_mp_physics ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'RA_LW_PHYSICS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for RA_LW_PHYSICS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_ra_lw_physics ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'RA_SW_PHYSICS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for RA_SW_PHYSICS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_ra_sw_physics ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'SF_SFCLAY_PHYSICS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for SF_SFCLAY_PHYSICS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_sf_sfclay_physics ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'SF_SURFACE_PHYSICS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for SF_SURFACE_PHYSICS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_sf_surface_physics ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'BL_PBL_PHYSICS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for BL_PBL_PHYSICS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_bl_pbl_physics ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'CU_PHYSICS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for CU_PHYSICS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_cu_physics ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'ISFFLX', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISFFLX returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_isfflx ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'IFSNOW', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for IFSNOW returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_ifsnow ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'ICLOUD', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ICLOUD returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_icloud ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'SWRAD_SCAT', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for SWRAD_SCAT returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_swrad_scat ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'SURFACE_INPUT_SOURCE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for SURFACE_INPUT_SOURCE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_surface_input_source ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'NUM_SOIL_LAYERS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NUM_SOIL_LAYERS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_num_soil_layers ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'NUM_MONTHS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NUM_MONTHS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_num_months ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'MAXIENS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAXIENS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_maxiens ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'MAXENS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAXENS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_maxens ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'MAXENS2', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAXENS2 returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_maxens2 ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'MAXENS3', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAXENS3 returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_maxens3 ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'ENSDIM', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ENSDIM returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_ensdim ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'CHEM_OPT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for CHEM_OPT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_chem_opt ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_grid_fdda ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_if_no_pbl_nudging_uv ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_if_no_pbl_nudging_t ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_if_no_pbl_nudging_q ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_if_zfac_uv ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_k_zfac_uv ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_if_zfac_t ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_k_zfac_t ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_if_zfac_q ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'GRID_FDDA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GRID_FDDA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_k_zfac_q ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'GRID_FDDA', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for GRID_FDDA returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_guv ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'GRID_FDDA', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for GRID_FDDA returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_gt ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'GRID_FDDA', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for GRID_FDDA returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_gq ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'OBS_NUDGE_OPT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for OBS_NUDGE_OPT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_nudge_opt ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'FDDA_START', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for FDDA_START returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_fdda_start ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'FDDA_END', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for FDDA_END returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_fdda_end ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'OBS_NUDGE_WIND', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for OBS_NUDGE_WIND returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_nudge_wind ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'OBS_COEF_WIND', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for OBS_COEF_WIND returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_coef_wind ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'OBS_NUDGE_TEMP', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for OBS_NUDGE_TEMP returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_nudge_temp ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'OBS_COEF_TEMP', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for OBS_COEF_TEMP returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_coef_temp ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'OBS_NUDGE_MOIS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for OBS_NUDGE_MOIS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_nudge_mois ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'OBS_COEF_MOIS', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for OBS_COEF_MOIS returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_coef_mois ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'OBS_NUDGE_PSTR', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for OBS_NUDGE_PSTR returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_nudge_pstr ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'OBS_COEF_PSTR', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for OBS_COEF_PSTR returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_coef_pstr ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'OBS_RINXY', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for OBS_RINXY returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_obs_rinxy ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'DYN_OPT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for DYN_OPT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_dyn_opt ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'RK_ORDER', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for RK_ORDER returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_rk_ord ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'W_DAMPING', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for W_DAMPING returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_w_damping ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'DIFF_OPT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for DIFF_OPT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_diff_opt ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'KM_OPT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for KM_OPT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_km_opt ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'DAMP_OPT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for DAMP_OPT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_damp_opt ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'DIFF_6TH_OPT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for DIFF_6TH_OPT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_diff_6th_opt ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'NON_HYDROSTATIC', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for NON_HYDROSTATIC returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_non_hydrostatic ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'H_MOM_ADV_ORDER', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for H_MOM_ADV_ORDER returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_h_mom_adv_order ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'V_MOM_ADV_ORDER', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for V_MOM_ADV_ORDER returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_v_mom_adv_order ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'H_SCA_ADV_ORDER', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for H_SCA_ADV_ORDER returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_h_sca_adv_order ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'V_SCA_ADV_ORDER', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for V_SCA_ADV_ORDER returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_v_sca_adv_order ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'PD_MOIST', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for PD_MOIST returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_pd_moist ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'PD_CHEM', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for PD_CHEM returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_pd_chem ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'PD_SCALAR', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for PD_SCALAR returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_pd_scalar ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'PD_TKE', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for PD_TKE returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_pd_tke ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'TOP_RADIATION', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for TOP_RADIATION returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_top_radiation ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'PERT_CORIOLIS', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for PERT_CORIOLIS returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_pert_coriolis ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'MIX_FULL_FIELD', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for MIX_FULL_FIELD returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_mix_full_fields ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'SPEC_BDY_WIDTH', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for SPEC_BDY_WIDTH returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_spec_bdy_width ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'SPEC_ZONE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for SPEC_ZONE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_spec_zone ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'RELAX_ZONE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for RELAX_ZONE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_relax_zone ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'SPECIFIED', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for SPECIFIED returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_specified ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'PERIODIC_X', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for PERIODIC_X returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_periodic_x ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'SYMMETRIC_XS', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for SYMMETRIC_XS returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_symmetric_xs ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'SYMMETRIC_XE', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for SYMMETRIC_XE returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_symmetric_xe ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'OPEN_XS', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for OPEN_XS returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_open_xs ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'OPEN_XE', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for OPEN_XE returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_open_xe ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'PERIODIC_Y', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for PERIODIC_Y returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_periodic_y ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'SYMMETRIC_YS', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for SYMMETRIC_YS returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_symmetric_ys ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'SYMMETRIC_YE', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for SYMMETRIC_YE returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_symmetric_ye ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'OPEN_YS', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for OPEN_YS returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_open_ys ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'OPEN_YE', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for OPEN_YE returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_open_ye ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'NESTED', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for NESTED returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_nested ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'REAL_DATA_INIT_TYPE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for REAL_DATA_INIT_TYPE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_real_data_init_type ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'BACKGROUND_PROC_ID', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for BACKGROUND_PROC_ID returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_background_proc_id ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'FORECAST_PROC_ID', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for FORECAST_PROC_ID returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_forecast_proc_id ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'PRODUCTION_STATUS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for PRODUCTION_STATUS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_production_status ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'COMPRESSION', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for COMPRESSION returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_compression ( grid%id, ibuf(1) )
