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
          grid%moist(ims,jms,kms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XYZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF (BTEST(dfi_moist_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_moist_dname_table( grid%id, itrace )), & !data name
          grid%dfi_moist(ims,jms,kms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XYZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )), & !data name
          grid%scalar(ims,jms,kms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XYZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF (BTEST(dfi_scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )), & !data name
          grid%dfi_scalar(ims,kms,jms,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XZY'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_input.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
CALL wrf_get_dom_ti_real ( fid , 'TSTART', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TSTART returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_tstart ( grid%id, rbuf(1) )
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
CALL wrf_get_dom_ti_integer ( fid , 'ID', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ID returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_grid_id ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'ALLOWED', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for ALLOWED returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_grid_allowed ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'NUM_METGRID_LEVELS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NUM_METGRID_LEVELS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_num_metgrid_levels ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'P_TOP_REQUESTED', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for P_TOP_REQUESTED returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_p_top_requested ( grid%id, rbuf(1) )
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
CALL wrf_get_dom_ti_integer ( fid , 'NUM_URBAN_LAYERS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NUM_URBAN_LAYERS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_num_urban_layers ( grid%id, ibuf(1) )
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
CALL wrf_get_dom_ti_integer ( fid , 'GWD_OPT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for GWD_OPT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_gwd_opt ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'IDTAD', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for IDTAD returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_idtad ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'NSOIL', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NSOIL returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_nsoil ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'NPHS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NPHS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_nphs ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'NCNVC', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NCNVC returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_ncnvc ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'NRADS', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NRADS returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_nrads ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'NRADL', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for NRADL returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_nradl ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'TPREC', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TPREC returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_tprec ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'THEAT', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for THEAT returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_theat ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'TCLOD', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TCLOD returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_tclod ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'TRDSW', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRDSW returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_trdsw ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'TRDLW', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRDLW returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_trdlw ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'TSRFC', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TSRFC returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_tsrfc ( grid%id, rbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'PCPFLG', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for PCPFLG returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_pcpflg ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'SIGMA', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for SIGMA returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_sigma ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_real ( fid , 'SEA SPRAY PARAMETER', rbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for SEA SPRAY PARAMETER returns ',rbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_sfenth ( grid%id, rbuf(1) )
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
CALL wrf_get_dom_ti_logical ( fid , 'NON_HYDROSTATIC', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for NON_HYDROSTATIC returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_non_hydrostatic ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'PERT_CORIOLIS', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for PERT_CORIOLIS returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_pert_coriolis ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_logical ( fid , 'EULER_ADV', lbuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_logical for EULER_ADV returns ',lbuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_euler_adv ( grid%id, lbuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'IDTADT', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for IDTADT returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_idtadt ( grid%id, ibuf(1) )
CALL wrf_get_dom_ti_integer ( fid , 'IDTADC', ibuf(1), 1, ierr )
 WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for IDTADC returns ',ibuf(1)
 CALL wrf_debug ( 300 , wrf_err_message )
 CALL rconfig_set_idtadc ( grid%id, ibuf(1) )
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
