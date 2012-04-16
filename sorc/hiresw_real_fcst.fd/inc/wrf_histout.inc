!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_histout.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( in_use_for_config(grid%id,'x_2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TOYVAR'               , &  ! Data Name 
                       grid%x_2               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field TOYVAR memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'lu_index') ) THEN
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
'inc/wrf_histout.inc ext_write_field LU_INDEX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hbm2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBM2'               , &  ! Data Name 
                       grid%hbm2               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field HBM2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SM'               , &  ! Data Name 
                       grid%sm               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field SM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sice') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SICE'               , &  ! Data Name 
                       grid%sice               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field SICE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pd') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PD'               , &  ! Data Name 
                       grid%pd               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field PD memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'fis') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FIS'               , &  ! Data Name 
                       grid%fis               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field FIS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'res') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RES'               , &  ! Data Name 
                       grid%res               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field RES memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'t') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T'               , &  ! Data Name 
                       grid%t               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field T memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'q') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q'               , &  ! Data Name 
                       grid%q               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field Q memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'u') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U'               , &  ! Data Name 
                       grid%u               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field U memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'v') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V'               , &  ! Data Name 
                       grid%v               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field V memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dx_nmm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DX_NMM'               , &  ! Data Name 
                       grid%dx_nmm               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field DX_NMM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'deta1') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DETA1'               , &  ! Data Name 
                       grid%deta1               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field DETA1 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aeta1') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AETA1'               , &  ! Data Name 
                       grid%aeta1               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field AETA1 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'eta1') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ETA1'               , &  ! Data Name 
                       grid%eta1               , &  ! Field 
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
                       'Interface sigma value in pressure domain'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ETA1 memorder Z' , & ! Debug message
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'deta2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DETA2'               , &  ! Data Name 
                       grid%deta2               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field DETA2 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aeta2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AETA2'               , &  ! Data Name 
                       grid%aeta2               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field AETA2 memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'eta2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ETA2'               , &  ! Data Name 
                       grid%eta2               , &  ! Field 
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
                       'Interface sigma value in sigma domain'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ETA2 memorder Z' , & ! Debug message
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dy_nmm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DY_NMM'               , &  ! Data Name 
                       grid%dy_nmm               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field DY_NMM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dlmd') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DLMD'               , &  ! Data Name 
                       grid%dlmd               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field DLMD memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dphd') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DPHD'               , &  ! Data Name 
                       grid%dphd               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field DPHD memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pdtop') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PDTOP'               , &  ! Data Name 
                       grid%pdtop               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field PDTOP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pt') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PT'               , &  ! Data Name 
                       grid%pt               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field PT memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pblh') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PBLH'               , &  ! Data Name 
                       grid%pblh               , &  ! Field 
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
                       'PBL Height'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field PBLH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'mixht') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MIXHT'               , &  ! Data Name 
                       grid%mixht               , &  ! Field 
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
                       'MXL HEIGHT'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field MIXHT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ustar') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'USTAR'               , &  ! Data Name 
                       grid%ustar               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field USTAR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'z0') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Z0'               , &  ! Data Name 
                       grid%z0               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field Z0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ths') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'THS'               , &  ! Data Name 
                       grid%ths               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field THS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qsh') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QS'               , &  ! Data Name 
                       grid%qsh               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field QS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'twbs') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TWBS'               , &  ! Data Name 
                       grid%twbs               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field TWBS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qwbs') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QWBS'               , &  ! Data Name 
                       grid%qwbs               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field QWBS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'prec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PREC'               , &  ! Data Name 
                       grid%prec               , &  ! Field 
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
                       'Precipitation in physics timestep'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field PREC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aprec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APREC'               , &  ! Data Name 
                       grid%aprec               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field APREC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acprec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACPREC'               , &  ! Data Name 
                       grid%acprec               , &  ! Field 
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
                       'Accumulated total precipitation'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ACPREC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cuprec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CUPREC'               , &  ! Data Name 
                       grid%cuprec               , &  ! Field 
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
                       'Accumulated convective precipitation'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field CUPREC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sno') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNO'               , &  ! Data Name 
                       grid%sno               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field SNO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'si') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SI'               , &  ! Data Name 
                       grid%si               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field SI memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cldefi') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CLDEFI'               , &  ! Data Name 
                       grid%cldefi               , &  ! Field 
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
                       'Convective cloud efficiency'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field CLDEFI memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'th10') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TH10'               , &  ! Data Name 
                       grid%th10               , &  ! Field 
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
                       '10-m potential temperature from MYJ'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field TH10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'q10') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q10'               , &  ! Data Name 
                       grid%q10               , &  ! Field 
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
                       '10-m specific humidity from MYJ'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field Q10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pshltr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PSHLTR'               , &  ! Data Name 
                       grid%pshltr               , &  ! Field 
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
                       '2-m pressure from MYJ'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field PSHLTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tshltr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSHLTR'               , &  ! Data Name 
                       grid%tshltr               , &  ! Field 
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
                       '2-m potential temperature from MYJ'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field TSHLTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qshltr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSHLTR'               , &  ! Data Name 
                       grid%qshltr               , &  ! Field 
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
                       '2-m specific humidity from MYJ'               , &  ! Desc  
                       'kg kg-1'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field QSHLTR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'q2') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2'               , &  ! Data Name 
                       grid%q2               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field Q2 memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'akhs_out') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AKHS_OUT'               , &  ! Data Name 
                       grid%akhs_out               , &  ! Field 
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
                       'Output sfc exch coeff for heat'               , &  ! Desc  
                       'm2 s-1'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field AKHS_OUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'akms_out') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AKMS_OUT'               , &  ! Data Name 
                       grid%akms_out               , &  ! Field 
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
                       'Output sfc exch coeff for momentum'               , &  ! Desc  
                       'm2 s-1'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field AKMS_OUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'albase') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBASE'               , &  ! Data Name 
                       grid%albase               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field ALBASE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'albedo') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBEDO'               , &  ! Data Name 
                       grid%albedo               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field ALBEDO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cnvbot') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CNVBOT'               , &  ! Data Name 
                       grid%cnvbot               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field CNVBOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cnvtop') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CNVTOP'               , &  ! Data Name 
                       grid%cnvtop               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field CNVTOP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'czen') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CZEN'               , &  ! Data Name 
                       grid%czen               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field CZEN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'czmean') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CZMEAN'               , &  ! Data Name 
                       grid%czmean               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field CZMEAN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'epsr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'EPSR'               , &  ! Data Name 
                       grid%epsr               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field EPSR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'glat') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GLAT'               , &  ! Data Name 
                       grid%glat               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field GLAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'glon') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GLON'               , &  ! Data Name 
                       grid%glon               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field GLON memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nmm_tsk') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSK'               , &  ! Data Name 
                       grid%nmm_tsk               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field TSK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'mxsnal') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MXSNAL'               , &  ! Data Name 
                       grid%mxsnal               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field MXSNAL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'radot') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RADOT'               , &  ! Data Name 
                       grid%radot               , &  ! Field 
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
                       'Radiative emission from surface'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RADOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sigt4') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SIGT4'               , &  ! Data Name 
                       grid%sigt4               , &  ! Field 
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
                       'Stefan-Boltzmann * T**4'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field SIGT4 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tg') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TGROUND'               , &  ! Data Name 
                       grid%tg               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field TGROUND memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cwm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CWM'               , &  ! Data Name 
                       grid%cwm               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field CWM memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'f_ice') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_ICE'               , &  ! Data Name 
                       grid%f_ice               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field F_ICE memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'f_rain') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_RAIN'               , &  ! Data Name 
                       grid%f_rain               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field F_RAIN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'f_rimef') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_RIMEF'               , &  ! Data Name 
                       grid%f_rimef               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field F_RIMEF memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SR'               , &  ! Data Name 
                       grid%sr               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field SR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cfrach') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFRACH'               , &  ! Data Name 
                       grid%cfrach               , &  ! Field 
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
                       'High cloud fraction'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field CFRACH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cfracl') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFRACL'               , &  ! Data Name 
                       grid%cfracl               , &  ! Field 
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
                       'Low cloud fraction'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field CFRACL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cfracm') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFRACM'               , &  ! Data Name 
                       grid%cfracm               , &  ! Field 
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
                       'Middle cloud fraction'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field CFRACM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'islope') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ISLOPE'               , &  ! Data Name 
                       grid%islope               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field ISLOPE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'dzsoil') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DZSOIL'               , &  ! Data Name 
                       grid%dzsoil               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field DZSOIL memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sldpth') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SLDPTH'               , &  ! Data Name 
                       grid%sldpth               , &  ! Field 
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
                       'Depths of centers of soil layers'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field SLDPTH memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'cmc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CMC'               , &  ! Data Name 
                       grid%cmc               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field CMC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'grnflx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GRNFLX'               , &  ! Data Name 
                       grid%grnflx               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field GRNFLX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pctsno') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PCTSNO'               , &  ! Data Name 
                       grid%pctsno               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field PCTSNO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'soiltb') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILTB'               , &  ! Data Name 
                       grid%soiltb               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field SOILTB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'vegfrc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VEGFRC'               , &  ! Data Name 
                       grid%vegfrc               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field VEGFRC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sh2o') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SH2O'               , &  ! Data Name 
                       grid%sh2o               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field SH2O memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'smc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMC'               , &  ! Data Name 
                       grid%smc               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field SMC memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'stc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'STC'               , &  ! Data Name 
                       grid%stc               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field STC memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'pint') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PINT'               , &  ! Data Name 
                       grid%pint               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XYZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top_stag'               , &  ! Dimname 3 
                       'Model layer interface pressure'               , &  ! Desc  
                       'Pa'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field PINT memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , kde ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( kde, kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'w') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W'               , &  ! Data Name 
                       grid%w               , &  ! Field 
                       WRF_FLOAT          , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask       , &  ! bdy_mask
                       dryrun             , &  ! flag
                       'XYZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
                       'west_east'               , &  ! Dimname 1 
                       'south_north'               , &  ! Dimname 2 
                       'bottom_top_stag'               , &  ! Dimname 3 
                       'Vertical velocity'               , &  ! Desc  
                       'm s-1'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field W memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , kde ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( kde, kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acfrcv') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACFRCV'               , &  ! Data Name 
                       grid%acfrcv               , &  ! Field 
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
                       'Accum convective cloud fraction'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ACFRCV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acfrst') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACFRST'               , &  ! Data Name 
                       grid%acfrst               , &  ! Field 
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
                       'Accum stratiform cloud fraction'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ACFRST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ssroff') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SSROFF'               , &  ! Data Name 
                       grid%ssroff               , &  ! Field 
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
                       'Surface runoff'               , &  ! Desc  
                       'mm'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field SSROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'bgroff') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'BGROFF'               , &  ! Data Name 
                       grid%bgroff               , &  ! Field 
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
                       'Subsurface runoff'               , &  ! Desc  
                       'mm'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field BGROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rlwin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RLWIN'               , &  ! Data Name 
                       grid%rlwin               , &  ! Field 
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
                       'Downward longwave at surface'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RLWIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rlwtoa') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RLWTOA'               , &  ! Data Name 
                       grid%rlwtoa               , &  ! Field 
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
                       'Outgoing LW flux at top of atmos'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RLWTOA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'alwin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALWIN'               , &  ! Data Name 
                       grid%alwin               , &  ! Field 
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
                       'Accum LW down at surface'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ALWIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'alwout') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALWOUT'               , &  ! Data Name 
                       grid%alwout               , &  ! Field 
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
                       'Accum RADOT (see above)'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ALWOUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'alwtoa') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALWTOA'               , &  ! Data Name 
                       grid%alwtoa               , &  ! Field 
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
                       'Accum RLWTOA'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ALWTOA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rswin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RSWIN'               , &  ! Data Name 
                       grid%rswin               , &  ! Field 
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
                       'Downward shortwave at surface'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RSWIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rswinc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RSWINC'               , &  ! Data Name 
                       grid%rswinc               , &  ! Field 
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
                       'Clear-sky equivalent of RSWIN'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RSWINC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rswout') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RSWOUT'               , &  ! Data Name 
                       grid%rswout               , &  ! Field 
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
                       'Upward shortwave at surface'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RSWOUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aswin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ASWIN'               , &  ! Data Name 
                       grid%aswin               , &  ! Field 
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
                       'Accum SW down at surface'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ASWIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aswout') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ASWOUT'               , &  ! Data Name 
                       grid%aswout               , &  ! Field 
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
                       'Accum RSWOUT'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ASWOUT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aswtoa') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ASWTOA'               , &  ! Data Name 
                       grid%aswtoa               , &  ! Field 
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
                       'Accum RSWTOA'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ASWTOA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcshx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCSHX'               , &  ! Data Name 
                       grid%sfcshx               , &  ! Field 
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
                       'Accum sfc sensible heat flux'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field SFCSHX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfclhx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCLHX'               , &  ! Data Name 
                       grid%sfclhx               , &  ! Field 
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
                       'Accum sfc latent heat flux'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field SFCLHX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'subshx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SUBSHX'               , &  ! Data Name 
                       grid%subshx               , &  ! Field 
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
                       'Accum deep soil heat flux'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field SUBSHX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'snopcx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOPCX'               , &  ! Data Name 
                       grid%snopcx               , &  ! Field 
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
                       'Snow phase change heat flux'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field SNOPCX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcuvx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCUVX'               , &  ! Data Name 
                       grid%sfcuvx               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field SFCUVX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'potevp') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'POTEVP'               , &  ! Data Name 
                       grid%potevp               , &  ! Field 
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
                       'Accum potential evaporation'               , &  ! Desc  
                       'm'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field POTEVP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'potflx') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'POTFLX'               , &  ! Data Name 
                       grid%potflx               , &  ! Field 
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
                       'Energy equivalent of POTEVP'               , &  ! Desc  
                       'W m-2'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field POTFLX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tlmin') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TLMIN'               , &  ! Data Name 
                       grid%tlmin               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field TLMIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'tlmax') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TLMAX'               , &  ! Data Name 
                       grid%tlmax               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field TLMAX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'t02_min') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T02_MIN'               , &  ! Data Name 
                       grid%t02_min               , &  ! Field 
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
                       'Hourly Min Shelter Temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field T02_MIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'t02_max') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T02_MAX'               , &  ! Data Name 
                       grid%t02_max               , &  ! Field 
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
                       'Hourly Max Shelter Temperature'               , &  ! Desc  
                       'K'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field T02_MAX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rh02_min') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RH02_MIN'               , &  ! Data Name 
                       grid%rh02_min               , &  ! Field 
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
                       'Hourly Min Relative Humidity'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RH02_MIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rh02_max') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RH02_MAX'               , &  ! Data Name 
                       grid%rh02_max               , &  ! Field 
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
                       'Hourly Max Relative Humidity'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RH02_MAX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rlwtt') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RLWTT'               , &  ! Data Name 
                       grid%rlwtt               , &  ! Field 
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
                       'Longwave temperature tendency'               , &  ! Desc  
                       'K s-1'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RLWTT memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'rswtt') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RSWTT'               , &  ! Data Name 
                       grid%rswtt               , &  ! Field 
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
                       'Shortwave temperature tendency'               , &  ! Desc  
                       'K s-1'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field RSWTT memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'train') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TRAIN'               , &  ! Data Name 
                       grid%train               , &  ! Field 
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
                       'Accum stratiform temp tendency'               , &  ! Desc  
                       'K s-1'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field TRAIN memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ncfrcv') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCFRCV'               , &  ! Data Name 
                       grid%ncfrcv               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field NCFRCV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ncfrst') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCFRST'               , &  ! Data Name 
                       grid%ncfrst               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field NCFRST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nphs0') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NPHS0'               , &  ! Data Name 
                       grid%nphs0               , &  ! Field 
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
                       '-'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field NPHS0 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nprec') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NPREC'               , &  ! Data Name 
                       grid%nprec               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field NPREC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nclod') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCLOD'               , &  ! Data Name 
                       grid%nclod               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field NCLOD memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nheat') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NHEAT'               , &  ! Data Name 
                       grid%nheat               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field NHEAT memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nrdlw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NRDLW'               , &  ! Data Name 
                       grid%nrdlw               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field NRDLW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nrdsw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NRDSW'               , &  ! Data Name 
                       grid%nrdsw               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field NRDSW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'nsrfc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NSRFC'               , &  ! Data Name 
                       grid%nsrfc               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field NSRFC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'avrain') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AVRAIN'               , &  ! Data Name 
                       grid%avrain               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field AVRAIN memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'avcnvc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AVCNVC'               , &  ! Data Name 
                       grid%avcnvc               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field AVCNVC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acutim') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACUTIM'               , &  ! Data Name 
                       grid%acutim               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field ACUTIM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ardlw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ARDLW'               , &  ! Data Name 
                       grid%ardlw               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field ARDLW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ardsw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ARDSW'               , &  ! Data Name 
                       grid%ardsw               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field ARDSW memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'asrfc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ASRFC'               , &  ! Data Name 
                       grid%asrfc               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field ASRFC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'aphtim') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APHTIM'               , &  ! Data Name 
                       grid%aphtim               , &  ! Field 
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
'inc/wrf_histout.inc ext_write_field APHTIM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'max10mw') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAX10MW'               , &  ! Data Name 
                       grid%max10mw               , &  ! Field 
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
                       'Maximum 10 m wind speed (m/s) in previous hour'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field MAX10MW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'max10u') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAX10U'               , &  ! Data Name 
                       grid%max10u               , &  ! Field 
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
                       'Maximum 10 m wind U comp (m/s) previous hour'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field MAX10U memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'max10v') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAX10V'               , &  ! Data Name 
                       grid%max10v               , &  ! Field 
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
                       'Maximum 10 m wind V comp (m/s) previous hour'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field MAX10V memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'maxupdr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAXUPDR'               , &  ! Data Name 
                       grid%maxupdr               , &  ! Field 
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
                       'Maximum updraft (m/s) in previous hour'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field MAXUPDR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'maxdndr') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAXDNDR'               , &  ! Data Name 
                       grid%maxdndr               , &  ! Field 
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
                       'Maximum downdraft (m/s) in previous hour'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field MAXDNDR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'maxhlcy') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAXHLCY'               , &  ! Data Name 
                       grid%maxhlcy               , &  ! Field 
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
                       'Maximum updraft helicity (m^2/s^2)'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field MAXHLCY memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'maxdbz') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAXDBZ'               , &  ! Data Name 
                       grid%maxdbz               , &  ! Field 
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
                       'Maximum ~1000m m simulated radar reflectivity'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field MAXDBZ memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'landmask') ) THEN
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
'inc/wrf_histout.inc ext_write_field LANDMASK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
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
'inc/wrf_histout.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & ! Debug message
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
'inc/wrf_histout.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & ! Debug message
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
'inc/wrf_histout.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XYZ' , & ! Debug message
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
'inc/wrf_histout.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_histout.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
IF ( in_use_for_config(grid%id,'psfc') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PSFC'               , &  ! Data Name 
                       grid%psfc               , &  ! Field 
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
                       'SFC PRESSURE'               , &  ! Desc  
                       '-'               , &  ! Units 
'inc/wrf_histout.inc ext_write_field PSFC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'th2') ) THEN
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
'inc/wrf_histout.inc ext_write_field TH2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'u10') ) THEN
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
'inc/wrf_histout.inc ext_write_field U10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'v10') ) THEN
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
'inc/wrf_histout.inc ext_write_field V10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'smstav') ) THEN
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
'inc/wrf_histout.inc ext_write_field SMSTAV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'smstot') ) THEN
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
'inc/wrf_histout.inc ext_write_field SMSTOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcrunoff') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFROFF'               , &  ! Data Name 
                       grid%sfcrunoff               , &  ! Field 
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
                       'SURFACE RUNOFF'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field SFROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'udrunoff') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'UDROFF'               , &  ! Data Name 
                       grid%udrunoff               , &  ! Field 
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
                       'UNDERGROUND RUNOFF'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field UDROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'ivgtyp') ) THEN
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
'inc/wrf_histout.inc ext_write_field IVGTYP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'isltyp') ) THEN
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
'inc/wrf_histout.inc ext_write_field ISLTYP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'vegfra') ) THEN
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
'inc/wrf_histout.inc ext_write_field VEGFRA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcevp') ) THEN
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
'inc/wrf_histout.inc ext_write_field SFCEVP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'grdflx') ) THEN
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
'inc/wrf_histout.inc ext_write_field GRDFLX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sfcexc') ) THEN
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
'inc/wrf_histout.inc ext_write_field SFCEXC  memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acsnow') ) THEN
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
'inc/wrf_histout.inc ext_write_field ACSNOW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'acsnom') ) THEN
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
'inc/wrf_histout.inc ext_write_field ACSNOM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'snow') ) THEN
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
'inc/wrf_histout.inc ext_write_field SNOW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'canwat') ) THEN
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
'inc/wrf_histout.inc ext_write_field CANWAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'sst') ) THEN
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
'inc/wrf_histout.inc ext_write_field SST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'weasd') ) THEN
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
'inc/wrf_histout.inc ext_write_field WEASD memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'thz0') ) THEN
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
'inc/wrf_histout.inc ext_write_field THZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qz0') ) THEN
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
'inc/wrf_histout.inc ext_write_field QZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'uz0') ) THEN
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
'inc/wrf_histout.inc ext_write_field UZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'vz0') ) THEN
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
'inc/wrf_histout.inc ext_write_field VZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'qsfc') ) THEN
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
'inc/wrf_histout.inc ext_write_field QSFC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'htop') ) THEN
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
'inc/wrf_histout.inc ext_write_field HTOP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'hbot') ) THEN
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
'inc/wrf_histout.inc ext_write_field HBOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'snowh') ) THEN
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
'inc/wrf_histout.inc ext_write_field SNOWH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'itimestep') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ITIMESTEP'               , &  ! Data Name 
                       grid%itimestep               , &  ! Field 
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
                       ''               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field ITIMESTEP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
IF ( in_use_for_config(grid%id,'xtime') ) THEN
CALL wrf_ext_write_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XTIME'               , &  ! Data Name 
                       grid%xtime               , &  ! Field 
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
                       'minutes since simulation start'               , &  ! Desc  
                       ''               , &  ! Units 
'inc/wrf_histout.inc ext_write_field XTIME memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
