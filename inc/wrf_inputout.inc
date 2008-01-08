!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_inputout.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( grid%dyn_opt .EQ. dyn_exp ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TOYVAR'               , &  ! Data Name 
                       grid%exp_x_2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field TOYVAR memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LU_INDEX'               , &  ! Data Name 
                       grid%lu_index               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'LAND USE CATEGORY'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field LU_INDEX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SEAICE'               , &  ! Data Name 
                       grid%xice_gc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SEA ICE'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SEAICE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOALB'               , &  ! Data Name 
                       grid%snoalb               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'ANNUAL MAX SNOW ALBEDO IN FRACTION'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SNOALB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBM2'               , &  ! Data Name 
                       grid%nmm_hbm2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Height boundary mask; =0 outer 2 rows on H points'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field HBM2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBM3'               , &  ! Data Name 
                       grid%nmm_hbm3               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Height boundary mask; =0 outer 3 rows on H points'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field HBM3 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VBM2'               , &  ! Data Name 
                       grid%nmm_vbm2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Velocity boundary mask; =0 outer 2 rows on V points'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field VBM2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VBM3'               , &  ! Data Name 
                       grid%nmm_vbm3               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Velocity boundary mask; =0 outer 3 rows on V points'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field VBM3 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM'               , &  ! Data Name 
                       grid%nmm_sm               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Sea mask; =1 for sea, =0 for land'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SICE'               , &  ! Data Name 
                       grid%nmm_sice               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Sea ice mask; =1 for sea ice, =0 for no sea ice'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SICE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD'               , &  ! Data Name 
                       grid%nmm_pd               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Mass at I,J in the sigma domain'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field PD memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FIS'               , &  ! Data Name 
                       grid%nmm_fis               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Surface geopotential'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field FIS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RES'               , &  ! Data Name 
                       grid%nmm_res               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Reciprocal of surface sigma'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field RES memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T'               , &  ! Data Name 
                       grid%nmm_t               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
                       'Sensible temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field T memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q'               , &  ! Data Name 
                       grid%nmm_q               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
                       'Specific humidity'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field Q memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U'               , &  ! Data Name 
                       grid%nmm_u               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
                       'U component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field U memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V'               , &  ! Data Name 
                       grid%nmm_v               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
                       'V component of wind'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field V memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DX_NMM'               , &  ! Data Name 
                       grid%nmm_dx_nmm               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'East-west distance H-to-V points'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DX_NMM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'WPDAR'               , &  ! Data Name 
                       grid%nmm_wpdar               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field WPDAR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CPGFU'               , &  ! Data Name 
                       grid%nmm_cpgfu               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CPGFU memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CURV'               , &  ! Data Name 
                       grid%nmm_curv               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Curvature term= .5*DT*TAN(phi)/RadEarth'               , &  ! Desc  
                       's m-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CURV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FCP'               , &  ! Data Name 
                       grid%nmm_fcp               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field FCP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FDIV'               , &  ! Data Name 
                       grid%nmm_fdiv               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field FDIV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F'               , &  ! Data Name 
                       grid%nmm_f               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Coriolis * DT/2'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field F memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FAD'               , &  ! Data Name 
                       grid%nmm_fad               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field FAD memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DDMPU'               , &  ! Data Name 
                       grid%nmm_ddmpu               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Divergence damping term for U'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DDMPU memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DDMPV'               , &  ! Data Name 
                       grid%nmm_ddmpv               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Divergence damping term for V'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DDMPV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DETA'               , &  ! Data Name 
                       grid%nmm_deta               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Delta sigma in sigma domain'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DETA memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RDETA'               , &  ! Data Name 
                       grid%nmm_rdeta               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Reciprocal of DETA'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field RDETA memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AETA'               , &  ! Data Name 
                       grid%nmm_aeta               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field AETA memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F4Q2'               , &  ! Data Name 
                       grid%nmm_f4q2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field F4Q2 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ETAX'               , &  ! Data Name 
                       grid%nmm_etax               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ETAX memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DFL'               , &  ! Data Name 
                       grid%nmm_dfl               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'bottom_top_stag'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Standard atmosphere geopotential'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DFL memorder Z' , & ! Debug message
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DETA1'               , &  ! Data Name 
                       grid%nmm_deta1               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Delta sigma in pressure domain'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DETA1 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AETA1'               , &  ! Data Name 
                       grid%nmm_aeta1               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Midlayer sigma value in pressure domain'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field AETA1 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ETA1'               , &  ! Data Name 
                       grid%nmm_eta1               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Interface sigma value in pressure domain'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ETA1 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DETA2'               , &  ! Data Name 
                       grid%nmm_deta2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Delta sigma in sigma domain'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DETA2 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AETA2'               , &  ! Data Name 
                       grid%nmm_aeta2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Midlayer sigma value in sigma domain'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field AETA2 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ETA2'               , &  ! Data Name 
                       grid%nmm_eta2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Interface sigma value in sigma domain'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ETA2 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'EM'               , &  ! Data Name 
                       grid%nmm_em               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'C'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field EM memorder C' , & ! Debug message
1 , 2600 , 1 , 1 , 1 , 1 ,  & 
1 , 2600 , 1 , 1 , 1 , 1 ,  & 
1 , 2600 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'EMT'               , &  ! Data Name 
                       grid%nmm_emt               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'C'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field EMT memorder C' , & ! Debug message
1 , 2600 , 1 , 1 , 1 , 1 ,  & 
1 , 2600 , 1 , 1 , 1 , 1 ,  & 
1 , 2600 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DY_NMM'               , &  ! Data Name 
                       grid%nmm_dy_nmm               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'North-south distance H-to-V points'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DY_NMM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CPGFV'               , &  ! Data Name 
                       grid%nmm_cpgfv               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CPGFV memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'EN'               , &  ! Data Name 
                       grid%nmm_en               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field EN memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ENT'               , &  ! Data Name 
                       grid%nmm_ent               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ENT memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F4D'               , &  ! Data Name 
                       grid%nmm_f4d               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field F4D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F4Q'               , &  ! Data Name 
                       grid%nmm_f4q               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field F4Q memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'EF4T'               , &  ! Data Name 
                       grid%nmm_ef4t               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field EF4T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DLMD'               , &  ! Data Name 
                       grid%nmm_dlmd               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'East-west angular distance H-to-V points'               , &  ! Desc  
                       'degrees'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DLMD memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DPHD'               , &  ! Data Name 
                       grid%nmm_dphd               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'North-south angular distance H-to-V points'               , &  ! Desc  
                       'degrees'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DPHD memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PDTOP'               , &  ! Data Name 
                       grid%nmm_pdtop               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Mass at I,J in pressure domain'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field PDTOP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PT'               , &  ! Data Name 
                       grid%nmm_pt               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Pressure at top of domain'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field PT memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LPBL'               , &  ! Data Name 
                       grid%nmm_lpbl               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Model layer of PBL top'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field LPBL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'USTAR'               , &  ! Data Name 
                       grid%nmm_ustar               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Friction velocity'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field USTAR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Z0'               , &  ! Data Name 
                       grid%nmm_z0               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Roughness height'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field Z0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Z0BASE'               , &  ! Data Name 
                       grid%nmm_z0base               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Base roughness height'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field Z0BASE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'THS'               , &  ! Data Name 
                       grid%nmm_ths               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Surface potential temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field THS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAVAIL'               , &  ! Data Name 
                       grid%nmm_mavail               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field MAVAIL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QS'               , &  ! Data Name 
                       grid%nmm_qsh               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Surface specific humidity'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field QS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TWBS'               , &  ! Data Name 
                       grid%nmm_twbs               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Instantaneous sensible heat flux'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field TWBS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QWBS'               , &  ! Data Name 
                       grid%nmm_qwbs               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Instantaneous latent heat flux'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field QWBS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNO'               , &  ! Data Name 
                       grid%nmm_sno               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Liquid water snow amount'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SNO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SI'               , &  ! Data Name 
                       grid%nmm_si               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Snow depth'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SI memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TH10'               , &  ! Data Name 
                       grid%nmm_th10               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '10-m potential temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field TH10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q10'               , &  ! Data Name 
                       grid%nmm_q10               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '10-m specific humidity'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field Q10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PSHLTR'               , &  ! Data Name 
                       grid%nmm_pshltr               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '2-m pressure'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field PSHLTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSHLTR'               , &  ! Data Name 
                       grid%nmm_tshltr               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '2-m sensible temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field TSHLTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSHLTR'               , &  ! Data Name 
                       grid%nmm_qshltr               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '2-m specific humidity'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field QSHLTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2'               , &  ! Data Name 
                       grid%nmm_q2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
                       '2 * Turbulence kinetic energy'               , &  ! Desc  
                       'm2 s-2'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field Q2 memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBASE'               , &  ! Data Name 
                       grid%nmm_albase               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Base albedo'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ALBASE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBEDO'               , &  ! Data Name 
                       grid%nmm_albedo               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Dynamic albedo'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ALBEDO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CNVBOT'               , &  ! Data Name 
                       grid%nmm_cnvbot               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Lowest convec cloud bottom lyr between outputs'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CNVBOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CNVTOP'               , &  ! Data Name 
                       grid%nmm_cnvtop               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Highest convec cloud top lyr between outputs'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CNVTOP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CZEN'               , &  ! Data Name 
                       grid%nmm_czen               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Cosine of solar zenith angle'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CZEN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CZMEAN'               , &  ! Data Name 
                       grid%nmm_czmean               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Mean CZEN between SW radiation calls'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CZMEAN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'EPSR'               , &  ! Data Name 
                       grid%nmm_epsr               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Radiative emissivity'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field EPSR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GFFC'               , &  ! Data Name 
                       grid%nmm_gffc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field GFFC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GLAT'               , &  ! Data Name 
                       grid%nmm_glat               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Geographic latitude, radians'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field GLAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GLON'               , &  ! Data Name 
                       grid%nmm_glon               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Geographic longitude, radians'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field GLON memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSK'               , &  ! Data Name 
                       grid%nmm_nmm_tsk               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Skin temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field TSK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HDAC'               , &  ! Data Name 
                       grid%nmm_hdac               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Composite diffusion coeff for mass points'               , &  ! Desc  
                       's m-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field HDAC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HDACV'               , &  ! Data Name 
                       grid%nmm_hdacv               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Composite diffusion coeff for velocity points'               , &  ! Desc  
                       's m-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field HDACV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MXSNAL'               , &  ! Data Name 
                       grid%nmm_mxsnal               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Maximum deep snow albedo'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field MXSNAL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TGROUND'               , &  ! Data Name 
                       grid%nmm_tg               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Deep ground soil temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field TGROUND memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DFRLG'               , &  ! Data Name 
                       grid%nmm_dfrlg               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'bottom_top_stag'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Std atmosphere height of model layer interfaces'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DFRLG memorder Z' , & ! Debug message
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LVL'               , &  ! Data Name 
                       grid%nmm_lvl               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field LVL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM'               , &  ! Data Name 
                       grid%nmm_cwm               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XYZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
                       'Total condensate'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CWM memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_ICE'               , &  ! Data Name 
                       grid%nmm_f_ice               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
                       'Frozen fraction of CWM'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field F_ICE memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_RAIN'               , &  ! Data Name 
                       grid%nmm_f_rain               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
                       'Rain fraction of liquid part of CWM'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field F_RAIN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_RIMEF'               , &  ! Data Name 
                       grid%nmm_f_rimef               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'bottom_top'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
                       'Rime factor'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field F_RIMEF memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SR'               , &  ! Data Name 
                       grid%nmm_sr               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Timestep mass ratio of snow:precip'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ISLOPE'               , &  ! Data Name 
                       grid%nmm_islope               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ISLOPE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DZSOIL'               , &  ! Data Name 
                       grid%nmm_dzsoil               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Thickness of soil layers'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DZSOIL memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RTDPTH'               , &  ! Data Name 
                       grid%nmm_rtdpth               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field RTDPTH memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SLDPTH'               , &  ! Data Name 
                       grid%nmm_sldpth               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'bottom_top'               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Thickness of soil layers'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SLDPTH memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CMC'               , &  ! Data Name 
                       grid%nmm_cmc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Canopy moisture'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CMC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GRNFLX'               , &  ! Data Name 
                       grid%nmm_grnflx               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Deep soil heat flux'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field GRNFLX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PCTSNO'               , &  ! Data Name 
                       grid%nmm_pctsno               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field PCTSNO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILTB'               , &  ! Data Name 
                       grid%nmm_soiltb               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Deep ground soil temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SOILTB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VEGFRC'               , &  ! Data Name 
                       grid%nmm_vegfrc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'Vegetation fraction'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field VEGFRC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SH2O'               , &  ! Data Name 
                       grid%nmm_sh2o               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'soil_layers_stag'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
                       'Unfrozen soil moisture volume fraction'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SH2O memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMC'               , &  ! Data Name 
                       grid%nmm_smc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'soil_layers_stag'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
                       'Soil moisture volume fraction'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SMC memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'STC'               , &  ! Data Name 
                       grid%nmm_stc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'soil_layers_stag'               , &  ! Dimname 2 
                       'south_north'               , &  ! Dimname 3 
                       'Soil temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field STC memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCFRCV'               , &  ! Data Name 
                       grid%nmm_ncfrcv               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  times convec cloud >0 between rad calls'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field NCFRCV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCFRST'               , &  ! Data Name 
                       grid%nmm_ncfrst               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  times stratiform cloud >0 between rad calls'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field NCFRST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NPREC'               , &  ! Data Name 
                       grid%nmm_nprec               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  timesteps between resetting precip bucket'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field NPREC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCLOD'               , &  ! Data Name 
                       grid%nmm_nclod               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  timesteps between resetting cloud frac accum'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field NCLOD memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NHEAT'               , &  ! Data Name 
                       grid%nmm_nheat               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  timesteps between resetting latent heat accum'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field NHEAT memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NRDLW'               , &  ! Data Name 
                       grid%nmm_nrdlw               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  timesteps between resetting longwave accums'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field NRDLW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NRDSW'               , &  ! Data Name 
                       grid%nmm_nrdsw               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  timesteps between resetting shortwave accums'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field NRDSW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NSRFC'               , &  ! Data Name 
                       grid%nmm_nsrfc               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  timesteps between resetting sfcflux accums'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field NSRFC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AVRAIN'               , &  ! Data Name 
                       grid%nmm_avrain               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  of times gridscale precip called in NHEAT steps'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field AVRAIN memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AVCNVC'               , &  ! Data Name 
                       grid%nmm_avcnvc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  of times convective precip called in NHEAT steps'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field AVCNVC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ARATIM'               , &  ! Data Name 
                       grid%nmm_aratim               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ARATIM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACUTIM'               , &  ! Data Name 
                       grid%nmm_acutim               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ACUTIM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ARDLW'               , &  ! Data Name 
                       grid%nmm_ardlw               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  of times LW fluxes summed before resetting'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ARDLW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ARDSW'               , &  ! Data Name 
                       grid%nmm_ardsw               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  of times SW fluxes summed before resetting'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ARDSW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ASRFC'               , &  ! Data Name 
                       grid%nmm_asrfc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '  of times sfc fluxes summed before resetting'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ASRFC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_nmm ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APHTIM'               , &  ! Data Name 
                       grid%nmm_aphtim               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field APHTIM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LANDMASK'               , &  ! Data Name 
                       grid%landmask               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'description'               , &  ! Desc  
                       'units'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field LANDMASK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
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
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
          moist_desc_table( grid%id, itrace  ), & ! Desc
          moist_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_inputout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top'               , &  ! Dimname 3 
          scalar_desc_table( grid%id, itrace  ), & ! Desc
          scalar_units_table( grid%id, itrace  ), & ! Units
'inc/wrf_inputout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
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
'inc/wrf_inputout.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DTBC'               , &  ! Data Name 
                       grid%dtbc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       ''               , &  ! Dimname 1 
                       ''               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'TIME SINCE BOUNDARY READ'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field DTBC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TH2'               , &  ! Data Name 
                       grid%th2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'POT TEMP at 2 M'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field TH2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T2'               , &  ! Data Name 
                       grid%t2               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'TEMP at 2 M'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field T2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U10'               , &  ! Data Name 
                       grid%u10               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'U at 10 M'               , &  ! Desc  
                       ' '               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field U10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V10'               , &  ! Data Name 
                       grid%v10               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'V at 10 M'               , &  ! Desc  
                       ' '               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field V10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XICE'               , &  ! Data Name 
                       grid%xice               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SEA ICE'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field XICE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMSTAV'               , &  ! Data Name 
                       grid%smstav               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'MOISTURE VARIBILITY'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SMSTAV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMSTOT'               , &  ! Data Name 
                       grid%smstot               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'TOTAL SOIL MOISTURE'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SMSTOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'IVGTYP'               , &  ! Data Name 
                       grid%ivgtyp               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'VEGETATION TYPE'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field IVGTYP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ISLTYP'               , &  ! Data Name 
                       grid%isltyp               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SOIL TYPE'               , &  ! Desc  
                       ' '               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ISLTYP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VEGFRA'               , &  ! Data Name 
                       grid%vegfra               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'VEGETATION FRACTION'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field VEGFRA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCEVP'               , &  ! Data Name 
                       grid%sfcevp               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SURFACE EVAPORATION'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SFCEVP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GRDFLX'               , &  ! Data Name 
                       grid%grdflx               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'GROUND HEAT FLUX'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field GRDFLX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBBCK'               , &  ! Data Name 
                       grid%albbck               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'BACKGROUND ALBEDO'               , &  ! Desc  
                       'NA'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ALBBCK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCEXC '               , &  ! Data Name 
                       grid%sfcexc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SURFACE EXCHANGE COEFFICIENT'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SFCEXC  memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACSNOW'               , &  ! Data Name 
                       grid%acsnow               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'ACCUMULATED SNOW'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ACSNOW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACSNOM'               , &  ! Data Name 
                       grid%acsnom               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'ACCUMULATED MELTED SNOW'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ACSNOM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RMOL'               , &  ! Data Name 
                       grid%rmol               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       ''               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field RMOL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOW'               , &  ! Data Name 
                       grid%snow               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SNOW WATER EQUIVALENT'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SNOW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CANWAT'               , &  ! Data Name 
                       grid%canwat               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'CANOPY WATER'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field CANWAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SST'               , &  ! Data Name 
                       grid%sst               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SEA SURFACE TEMPERATURE'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'WEASD'               , &  ! Data Name 
                       grid%weasd               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'WATER EQUIVALENT OF ACCUMULATED SNOW'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field WEASD memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZNT'               , &  ! Data Name 
                       grid%znt               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'TIME-VARYING ROUGHNESS LENGTH'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field ZNT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MOL'               , &  ! Data Name 
                       grid%mol               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'T* IN SIMILARITY THEORY'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field MOL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'THZ0'               , &  ! Data Name 
                       grid%thz0               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'POT. TEMPERATURE AT TOP OF VISC. SUBLYR'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field THZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QZ0'               , &  ! Data Name 
                       grid%qz0               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SPECIFIC HUMIDITY AT TOP OF VISC. SUBLYR'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field QZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'UZ0'               , &  ! Data Name 
                       grid%uz0               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'U WIND COMPONENT AT TOP OF VISC. SUBLYR'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field UZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VZ0'               , &  ! Data Name 
                       grid%vz0               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'V WIND COMPONENT AT TOP OF VISC. SUBLYR'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field VZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSFC'               , &  ! Data Name 
                       grid%qsfc               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SPECIFIC HUMIDITY AT LOWER BOUNDARY'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field QSFC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AKHS'               , &  ! Data Name 
                       grid%akhs               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SFC EXCH COEFF FOR HEAT /DELTA Z'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field AKHS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AKMS'               , &  ! Data Name 
                       grid%akms               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'SFC EXCH COEFF FOR MOMENTUM /DELTA Z'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field AKMS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HTOP'               , &  ! Data Name 
                       grid%htop               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'TOP OF CONVECTION LEVEL'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field HTOP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBOT'               , &  ! Data Name 
                       grid%hbot               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'BOT OF CONVECTION LEVEL'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field HBOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HTOPR'               , &  ! Data Name 
                       grid%htopr               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'TOP OF CONVECTION LEVEL FOR RADIATION'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field HTOPR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBOTR'               , &  ! Data Name 
                       grid%hbotr               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'BOT OF CONVECTION LEVEL FOR RADIATION'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field HBOTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOWH'               , &  ! Data Name 
                       grid%snowh               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       'PHYSICAL SNOW DEPTH'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field SNOWH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RHOSN'               , &  ! Data Name 
                       grid%rhosn               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       ''               , &  ! Dimname 3 
                       ' SNOW DENSITY'               , &  ! Desc  
                       'kg m-3'               , &  ! Units 
'inc/wrf_inputout.inc ext_write_field RHOSN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
!ENDOFREGISTRYGENERATEDINCLUDE
