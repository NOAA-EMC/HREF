!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_bdyin.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BXS'               , &  ! Data Name 
                       grid%nmm_pd_b(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PD_BXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BXE'               , &  ! Data Name 
                       grid%nmm_pd_b(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PD_BXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BYS'               , &  ! Data Name 
                       grid%nmm_pd_b(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PD_BYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BYE'               , &  ! Data Name 
                       grid%nmm_pd_b(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PD_BYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BTXS'               , &  ! Data Name 
                       grid%nmm_pd_bt(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PD_BTXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BTXE'               , &  ! Data Name 
                       grid%nmm_pd_bt(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PD_BTXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BTYS'               , &  ! Data Name 
                       grid%nmm_pd_bt(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PD_BTYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BTYE'               , &  ! Data Name 
                       grid%nmm_pd_bt(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PD_BTYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXS'               , &  ! Data Name 
                       grid%nmm_t_b(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXE'               , &  ! Data Name 
                       grid%nmm_t_b(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYS'               , &  ! Data Name 
                       grid%nmm_t_b(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYE'               , &  ! Data Name 
                       grid%nmm_t_b(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXS'               , &  ! Data Name 
                       grid%nmm_t_bt(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXE'               , &  ! Data Name 
                       grid%nmm_t_bt(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYS'               , &  ! Data Name 
                       grid%nmm_t_bt(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYE'               , &  ! Data Name 
                       grid%nmm_t_bt(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BXS'               , &  ! Data Name 
                       grid%nmm_q_b(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BXE'               , &  ! Data Name 
                       grid%nmm_q_b(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BYS'               , &  ! Data Name 
                       grid%nmm_q_b(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BYE'               , &  ! Data Name 
                       grid%nmm_q_b(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BTXS'               , &  ! Data Name 
                       grid%nmm_q_bt(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BTXE'               , &  ! Data Name 
                       grid%nmm_q_bt(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BTYS'               , &  ! Data Name 
                       grid%nmm_q_bt(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BTYE'               , &  ! Data Name 
                       grid%nmm_q_bt(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BXS'               , &  ! Data Name 
                       grid%nmm_u_b(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BXE'               , &  ! Data Name 
                       grid%nmm_u_b(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BYS'               , &  ! Data Name 
                       grid%nmm_u_b(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BYE'               , &  ! Data Name 
                       grid%nmm_u_b(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTXS'               , &  ! Data Name 
                       grid%nmm_u_bt(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTXE'               , &  ! Data Name 
                       grid%nmm_u_bt(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTYS'               , &  ! Data Name 
                       grid%nmm_u_bt(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTYE'               , &  ! Data Name 
                       grid%nmm_u_bt(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field U_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BXS'               , &  ! Data Name 
                       grid%nmm_v_b(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BXE'               , &  ! Data Name 
                       grid%nmm_v_b(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BYS'               , &  ! Data Name 
                       grid%nmm_v_b(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BYE'               , &  ! Data Name 
                       grid%nmm_v_b(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTXS'               , &  ! Data Name 
                       grid%nmm_v_bt(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTXE'               , &  ! Data Name 
                       grid%nmm_v_bt(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTYS'               , &  ! Data Name 
                       grid%nmm_v_bt(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTYE'               , &  ! Data Name 
                       grid%nmm_v_bt(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field V_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BXS'               , &  ! Data Name 
                       grid%nmm_q2_b(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q2_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BXE'               , &  ! Data Name 
                       grid%nmm_q2_b(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q2_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BYS'               , &  ! Data Name 
                       grid%nmm_q2_b(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q2_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BYE'               , &  ! Data Name 
                       grid%nmm_q2_b(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q2_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BTXS'               , &  ! Data Name 
                       grid%nmm_q2_bt(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q2_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BTXE'               , &  ! Data Name 
                       grid%nmm_q2_bt(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q2_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BTYS'               , &  ! Data Name 
                       grid%nmm_q2_bt(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q2_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BTYE'               , &  ! Data Name 
                       grid%nmm_q2_bt(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field Q2_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BXS'               , &  ! Data Name 
                       grid%nmm_cwm_b(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field CWM_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BXE'               , &  ! Data Name 
                       grid%nmm_cwm_b(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field CWM_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BYS'               , &  ! Data Name 
                       grid%nmm_cwm_b(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field CWM_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BYE'               , &  ! Data Name 
                       grid%nmm_cwm_b(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field CWM_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BTXS'               , &  ! Data Name 
                       grid%nmm_cwm_bt(1,kds,1,1)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field CWM_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BTXE'               , &  ! Data Name 
                       grid%nmm_cwm_bt(1,kds,1,2)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field CWM_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BTYS'               , &  ! Data Name 
                       grid%nmm_cwm_bt(1,kds,1,3)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field CWM_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BTYE'               , &  ! Data Name 
                       grid%nmm_cwm_bt(1,kds,1,4)     , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field CWM_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
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
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_bdyin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
