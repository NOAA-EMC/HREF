!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_metaget_input.inc'
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
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(ozmixm_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(aerosolc_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(fdda3d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(fdda2d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
CALL wrf_get_dom_ti_integer ( fid , 'TIME_STEP', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for TIME_STEP returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_time_step ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'TIME_STEP_FRACT_NUM', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for TIME_STEP_FRACT_NUM returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_time_step_fract_num ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'TIME_STEP_FRACT_DEN', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for TIME_STEP_FRACT_DEN returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_time_step_fract_den ( grid%id, ibuf(1) )
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
CALL wrf_get_dom_ti_integer ( fid , 'REAL_DATA_INIT_TYPE', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for REAL_DATA_INIT_TYPE returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_real_data_init_type ( grid%id, ibuf(1) )
