!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_bdyout.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BXS'               , &  ! Data Name 
                       grid%pd_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy Mass at I,J in the sigma domain'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PD_BXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BXE'               , &  ! Data Name 
                       grid%pd_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy Mass at I,J in the sigma domain'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PD_BXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BYS'               , &  ! Data Name 
                       grid%pd_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy Mass at I,J in the sigma domain'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PD_BYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BYE'               , &  ! Data Name 
                       grid%pd_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy Mass at I,J in the sigma domain'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PD_BYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BTXS'               , &  ! Data Name 
                       grid%pd_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend Mass at I,J in the sigma domain'               , &  ! Desc  
                       '(Pa)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PD_BTXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BTXE'               , &  ! Data Name 
                       grid%pd_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend Mass at I,J in the sigma domain'               , &  ! Desc  
                       '(Pa)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PD_BTXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
jms, jme, 1, config_flags%spec_bdy_width, 1, 1, &
jps, MIN( (jde-1), jpe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BTYS'               , &  ! Data Name 
                       grid%pd_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend Mass at I,J in the sigma domain'               , &  ! Desc  
                       '(Pa)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PD_BTYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD_BTYE'               , &  ! Data Name 
                       grid%pd_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bdy_width'               , &  ! Dimname 2 
                       'one_element'               , &  ! Dimname 3 
                       'bdy tend Mass at I,J in the sigma domain'               , &  ! Desc  
                       '(Pa)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field PD_BTYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
ims, ime, 1, config_flags%spec_bdy_width, 1, 1, &
ips, MIN( (ide-1), ipe ), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXS'               , &  ! Data Name 
                       grid%t_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Sensible temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXE'               , &  ! Data Name 
                       grid%t_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Sensible temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYS'               , &  ! Data Name 
                       grid%t_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Sensible temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYE'               , &  ! Data Name 
                       grid%t_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Sensible temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXS'               , &  ! Data Name 
                       grid%t_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Sensible temperature'               , &  ! Desc  
                       '(K)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXE'               , &  ! Data Name 
                       grid%t_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Sensible temperature'               , &  ! Desc  
                       '(K)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYS'               , &  ! Data Name 
                       grid%t_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Sensible temperature'               , &  ! Desc  
                       '(K)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYE'               , &  ! Data Name 
                       grid%t_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Sensible temperature'               , &  ! Desc  
                       '(K)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field T_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BXS'               , &  ! Data Name 
                       grid%q_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Specific humidity'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BXE'               , &  ! Data Name 
                       grid%q_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Specific humidity'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BYS'               , &  ! Data Name 
                       grid%q_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Specific humidity'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BYE'               , &  ! Data Name 
                       grid%q_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Specific humidity'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BTXS'               , &  ! Data Name 
                       grid%q_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Specific humidity'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BTXE'               , &  ! Data Name 
                       grid%q_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Specific humidity'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BTYS'               , &  ! Data Name 
                       grid%q_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Specific humidity'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q_BTYE'               , &  ! Data Name 
                       grid%q_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Specific humidity'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BXS'               , &  ! Data Name 
                       grid%u_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy U component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BXE'               , &  ! Data Name 
                       grid%u_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy U component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BYS'               , &  ! Data Name 
                       grid%u_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy U component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BYE'               , &  ! Data Name 
                       grid%u_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy U component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTXS'               , &  ! Data Name 
                       grid%u_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend U component of wind'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTXE'               , &  ! Data Name 
                       grid%u_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend U component of wind'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTYS'               , &  ! Data Name 
                       grid%u_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend U component of wind'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BTYE'               , &  ! Data Name 
                       grid%u_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend U component of wind'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field U_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BXS'               , &  ! Data Name 
                       grid%v_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy V component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BXE'               , &  ! Data Name 
                       grid%v_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy V component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BYS'               , &  ! Data Name 
                       grid%v_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy V component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BYE'               , &  ! Data Name 
                       grid%v_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy V component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTXS'               , &  ! Data Name 
                       grid%v_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend V component of wind'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTXE'               , &  ! Data Name 
                       grid%v_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend V component of wind'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTYS'               , &  ! Data Name 
                       grid%v_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend V component of wind'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BTYE'               , &  ! Data Name 
                       grid%v_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend V component of wind'               , &  ! Desc  
                       '(m s-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field V_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BXS'               , &  ! Data Name 
                       grid%q2_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy 2 * Turbulence kinetic energy'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q2_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BXE'               , &  ! Data Name 
                       grid%q2_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy 2 * Turbulence kinetic energy'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q2_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BYS'               , &  ! Data Name 
                       grid%q2_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy 2 * Turbulence kinetic energy'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q2_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BYE'               , &  ! Data Name 
                       grid%q2_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy 2 * Turbulence kinetic energy'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q2_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BTXS'               , &  ! Data Name 
                       grid%q2_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend 2 * Turbulence kinetic energy'               , &  ! Desc  
                       '(m2 s-2)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q2_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BTXE'               , &  ! Data Name 
                       grid%q2_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend 2 * Turbulence kinetic energy'               , &  ! Desc  
                       '(m2 s-2)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q2_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BTYS'               , &  ! Data Name 
                       grid%q2_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend 2 * Turbulence kinetic energy'               , &  ! Desc  
                       '(m2 s-2)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q2_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2_BTYE'               , &  ! Data Name 
                       grid%q2_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend 2 * Turbulence kinetic energy'               , &  ! Desc  
                       '(m2 s-2)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field Q2_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BXS'               , &  ! Data Name 
                       grid%cwm_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Total condensate'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field CWM_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BXE'               , &  ! Data Name 
                       grid%cwm_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Total condensate'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field CWM_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BYS'               , &  ! Data Name 
                       grid%cwm_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Total condensate'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field CWM_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BYE'               , &  ! Data Name 
                       grid%cwm_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Total condensate'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field CWM_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BTXS'               , &  ! Data Name 
                       grid%cwm_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Total condensate'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field CWM_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BTXE'               , &  ! Data Name 
                       grid%cwm_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Total condensate'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field CWM_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BTYS'               , &  ! Data Name 
                       grid%cwm_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Total condensate'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field CWM_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM_BTYE'               , &  ! Data Name 
                       grid%cwm_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Total condensate'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field CWM_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RRW_BXS'               , &  ! Data Name 
                       grid%rrw_bxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Tracer'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field RRW_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RRW_BXE'               , &  ! Data Name 
                       grid%rrw_bxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Tracer'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field RRW_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RRW_BYS'               , &  ! Data Name 
                       grid%rrw_bys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Tracer'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field RRW_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RRW_BYE'               , &  ! Data Name 
                       grid%rrw_bye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy Tracer'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field RRW_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RRW_BTXS'               , &  ! Data Name 
                       grid%rrw_btxs(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Tracer'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field RRW_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RRW_BTXE'               , &  ! Data Name 
                       grid%rrw_btxe(jms,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Tracer'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field RRW_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RRW_BTYS'               , &  ! Data Name 
                       grid%rrw_btys(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Tracer'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field RRW_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RRW_BTYE'               , &  ! Data Name 
                       grid%rrw_btye(ims,kds,1)     , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
                       'bdy tend Tracer'               , &  ! Desc  
                       '(kg kg-1)/dt'               , &  ! Units 
'inc/wrf_bdyout.inc ext_write_field RRW_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%scalar_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%scalar_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%scalar_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%scalar_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%scalar_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%scalar_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%scalar_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(scalar_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%scalar_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF (BTEST(dfi_scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BXS', & !data name
          grid%dfi_scalar_BXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BXE', & !data name
          grid%dfi_scalar_BXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BYS', & !data name
          grid%dfi_scalar_BYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BYE', & !data name
          grid%dfi_scalar_BYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF (BTEST(dfi_scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTXS', & !data name
          grid%dfi_scalar_BTXS(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTXE', & !data name
          grid%dfi_scalar_BTXE(jms,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'XEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'south_north'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
jms, jme, kds, kde, 1, config_flags%spec_bdy_width, &
jps, MIN( (jde-1), jpe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTYS', & !data name
          grid%dfi_scalar_BTYS(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YSZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
    CALL wrf_ext_write_field (  &
          fid                             , &  ! DataHandle
          current_date(1:19)              , &  ! DateStr
          TRIM(dfi_scalar_dname_table( grid%id, itrace )) // '_BTYE', & !data name
          grid%dfi_scalar_BTYE(ims,kds,1,itrace)  , &  ! Field
                       WRF_FLOAT             , &  ! FieldType 
          grid%communicator  , &  ! Comm
          grid%iocommunicator  , &  ! Comm
          grid%domdesc       , &  ! Comm
          grid%bdy_mask       , &  ! bdy_mask
          dryrun             , &  ! flag
          'YEZ'               , &  ! MemoryOrder
          ''                , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'bdy_width'               , &  ! Dimname 3 
          dfi_scalar_desc_table( grid%id, itrace  ), & ! Desc
          dfi_scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_bdyout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
ims, ime, kds, kde, 1, config_flags%spec_bdy_width, &
ips, MIN( (ide-1), ipe ), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                         ierr )
  ENDIF
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
