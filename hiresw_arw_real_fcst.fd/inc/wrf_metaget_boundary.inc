!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_metaget_boundary.inc'
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
          TRIM(moist_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%moist_B(1,kds,1,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%moist_B(1,kds,1,2,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%moist_B(1,kds,1,3,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%moist_B(1,kds,1,4,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF (BTEST(moist_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%moist_BT(1,kds,1,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%moist_BT(1,kds,1,2,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%moist_BT(1,kds,1,3,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(moist_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%moist_BT(1,kds,1,4,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%scalar_B(1,kds,1,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%scalar_B(1,kds,1,2,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%scalar_B(1,kds,1,3,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%scalar_B(1,kds,1,4,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%scalar_BT(1,kds,1,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%scalar_BT(1,kds,1,2,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%scalar_BT(1,kds,1,3,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_read_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%scalar_BT(1,kds,1,4,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
'inc/wrf_metaget_boundary.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
