!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_auxinput10in.inc'
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
'inc/wrf_auxinput10in.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput10in.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput10in.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput10in.inc ext_write_field '//TRIM(ozmixm_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput10in.inc ext_write_field '//TRIM(aerosolc_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput10in.inc ext_write_field '//TRIM(fdda3d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_auxinput10in.inc ext_write_field '//TRIM(fdda2d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , 1 , jds , (jde-1) ,  & 
ims , ime , 1 , 1 , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , 1 , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
