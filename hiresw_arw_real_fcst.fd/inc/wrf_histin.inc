!STARTOFREGISTRYGENERATEDINCLUDE 'inc/wrf_histin.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LU_INDEX'               , &  ! Data Name 
                       grid%lu_index               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LU_INDEX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U'               , &  ! Data Name 
                       grid%em_u_2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field U memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V'               , &  ! Data Name 
                       grid%em_v_2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field V memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W'               , &  ! Data Name 
                       grid%em_w_2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field W memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH'               , &  ! Data Name 
                       grid%em_ph_2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PH memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PHB'               , &  ! Data Name 
                       grid%em_phb               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PHB memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T'               , &  ! Data Name 
                       grid%em_t_2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field T memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU'               , &  ! Data Name 
                       grid%em_mu_2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field MU memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MUB'               , &  ! Data Name 
                       grid%em_mub               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field MUB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NEST_POS'               , &  ! Data Name 
                       grid%nest_pos               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field NEST_POS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'P'               , &  ! Data Name 
                       grid%em_p               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field P memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PB'               , &  ! Data Name 
                       grid%em_pb               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PB memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SR'               , &  ! Data Name 
                       grid%em_sr               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'POTEVP'               , &  ! Data Name 
                       grid%em_potevp               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field POTEVP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOPCX'               , &  ! Data Name 
                       grid%em_snopcx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SNOPCX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILTB'               , &  ! Data Name 
                       grid%em_soiltb               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SOILTB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FNM'               , &  ! Data Name 
                       grid%em_fnm               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field FNM memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FNP'               , &  ! Data Name 
                       grid%em_fnp               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field FNP memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RDNW'               , &  ! Data Name 
                       grid%em_rdnw               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RDNW memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RDN'               , &  ! Data Name 
                       grid%em_rdn               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RDN memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DNW'               , &  ! Data Name 
                       grid%em_dnw               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field DNW memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DN '               , &  ! Data Name 
                       grid%em_dn               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field DN  memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZNU'               , &  ! Data Name 
                       grid%em_znu               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ZNU memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZNW'               , &  ! Data Name 
                       grid%em_znw               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ZNW memorder Z' , & ! Debug message
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFN'               , &  ! Data Name 
                       grid%cfn               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CFN memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFN1'               , &  ! Data Name 
                       grid%cfn1               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CFN1 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2'               , &  ! Data Name 
                       grid%q2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field Q2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T2'               , &  ! Data Name 
                       grid%t2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field T2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TH2'               , &  ! Data Name 
                       grid%th2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TH2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PSFC'               , &  ! Data Name 
                       grid%psfc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PSFC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U10'               , &  ! Data Name 
                       grid%u10               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field U10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V10'               , &  ! Data Name 
                       grid%v10               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field V10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RDX'               , &  ! Data Name 
                       grid%rdx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RDX memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RDY'               , &  ! Data Name 
                       grid%rdy               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RDY memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RESM'               , &  ! Data Name 
                       grid%resm               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RESM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZETATOP'               , &  ! Data Name 
                       grid%zetatop               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ZETATOP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CF1'               , &  ! Data Name 
                       grid%cf1               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CF1 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CF2'               , &  ! Data Name 
                       grid%cf2               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CF2 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CF3'               , &  ! Data Name 
                       grid%cf3               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CF3 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ITIMESTEP'               , &  ! Data Name 
                       grid%itimestep               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ITIMESTEP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XTIME'               , &  ! Data Name 
                       grid%xtime               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field XTIME memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
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
'inc/wrf_histin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_histin.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_histin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LANDMASK'               , &  ! Data Name 
                       grid%landmask               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LANDMASK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSLB'               , &  ! Data Name 
                       grid%tslb               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TSLB memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZS'               , &  ! Data Name 
                       grid%zs               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ZS memorder Z' , & ! Debug message
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DZS'               , &  ! Data Name 
                       grid%dzs               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field DZS memorder Z' , & ! Debug message
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMOIS'               , &  ! Data Name 
                       grid%smois               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SMOIS memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SH2O'               , &  ! Data Name 
                       grid%sh2o               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SH2O memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XICE'               , &  ! Data Name 
                       grid%xice               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field XICE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFROFF'               , &  ! Data Name 
                       grid%sfcrunoff               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SFROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'UDROFF'               , &  ! Data Name 
                       grid%udrunoff               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field UDROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'IVGTYP'               , &  ! Data Name 
                       grid%ivgtyp               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field IVGTYP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ISLTYP'               , &  ! Data Name 
                       grid%isltyp               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ISLTYP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VEGFRA'               , &  ! Data Name 
                       grid%vegfra               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field VEGFRA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GRDFLX'               , &  ! Data Name 
                       grid%grdflx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field GRDFLX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACSNOW'               , &  ! Data Name 
                       grid%acsnow               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ACSNOW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACSNOM'               , &  ! Data Name 
                       grid%acsnom               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ACSNOM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOW'               , &  ! Data Name 
                       grid%snow               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SNOW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOWH'               , &  ! Data Name 
                       grid%snowh               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SNOWH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RHOSN'               , &  ! Data Name 
                       grid%rhosn               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RHOSN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CANWAT'               , &  ! Data Name 
                       grid%canwat               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field CANWAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SST'               , &  ! Data Name 
                       grid%sst               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
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
'inc/wrf_histin.inc ext_write_field '//TRIM(ozmixm_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_histin.inc ext_write_field '//TRIM(aerosolc_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%paerlev , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%paerlev , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%paerlev , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAPFAC_M'               , &  ! Data Name 
                       grid%msft               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field MAPFAC_M memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAPFAC_U'               , &  ! Data Name 
                       grid%msfu               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field MAPFAC_U memorder XY' , & ! Debug message
ids , ide , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( ide, ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAPFAC_V'               , &  ! Data Name 
                       grid%msfv               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field MAPFAC_V memorder XY' , & ! Debug message
ids , (ide-1) , jds , jde , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( jde, jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F'               , &  ! Data Name 
                       grid%f               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field F memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'E'               , &  ! Data Name 
                       grid%e               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field E memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SINALPHA'               , &  ! Data Name 
                       grid%sina               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SINALPHA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'COSALPHA'               , &  ! Data Name 
                       grid%cosa               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field COSALPHA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HGT'               , &  ! Data Name 
                       grid%ht               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HGT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSK'               , &  ! Data Name 
                       grid%tsk               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TSK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'P_TOP'               , &  ! Data Name 
                       grid%p_top               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field P_TOP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LL_T'               , &  ! Data Name 
                       grid%em_lat_ll_t               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_LL_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UL_T'               , &  ! Data Name 
                       grid%em_lat_ul_t               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_UL_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UR_T'               , &  ! Data Name 
                       grid%em_lat_ur_t               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_UR_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LR_T'               , &  ! Data Name 
                       grid%em_lat_lr_t               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_LR_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LL_U'               , &  ! Data Name 
                       grid%em_lat_ll_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_LL_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UL_U'               , &  ! Data Name 
                       grid%em_lat_ul_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_UL_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UR_U'               , &  ! Data Name 
                       grid%em_lat_ur_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_UR_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LR_U'               , &  ! Data Name 
                       grid%em_lat_lr_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_LR_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LL_V'               , &  ! Data Name 
                       grid%em_lat_ll_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_LL_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UL_V'               , &  ! Data Name 
                       grid%em_lat_ul_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_UL_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UR_V'               , &  ! Data Name 
                       grid%em_lat_ur_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_UR_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LR_V'               , &  ! Data Name 
                       grid%em_lat_lr_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_LR_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LL_D'               , &  ! Data Name 
                       grid%em_lat_ll_d               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_LL_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UL_D'               , &  ! Data Name 
                       grid%em_lat_ul_d               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_UL_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UR_D'               , &  ! Data Name 
                       grid%em_lat_ur_d               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_UR_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LR_D'               , &  ! Data Name 
                       grid%em_lat_lr_d               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LAT_LR_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LL_T'               , &  ! Data Name 
                       grid%em_lon_ll_t               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_LL_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UL_T'               , &  ! Data Name 
                       grid%em_lon_ul_t               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_UL_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UR_T'               , &  ! Data Name 
                       grid%em_lon_ur_t               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_UR_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LR_T'               , &  ! Data Name 
                       grid%em_lon_lr_t               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_LR_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LL_U'               , &  ! Data Name 
                       grid%em_lon_ll_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_LL_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UL_U'               , &  ! Data Name 
                       grid%em_lon_ul_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_UL_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UR_U'               , &  ! Data Name 
                       grid%em_lon_ur_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_UR_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LR_U'               , &  ! Data Name 
                       grid%em_lon_lr_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_LR_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LL_V'               , &  ! Data Name 
                       grid%em_lon_ll_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_LL_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UL_V'               , &  ! Data Name 
                       grid%em_lon_ul_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_UL_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UR_V'               , &  ! Data Name 
                       grid%em_lon_ur_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_UR_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LR_V'               , &  ! Data Name 
                       grid%em_lon_lr_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_LR_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LL_D'               , &  ! Data Name 
                       grid%em_lon_ll_d               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_LL_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UL_D'               , &  ! Data Name 
                       grid%em_lon_ul_d               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_UL_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UR_D'               , &  ! Data Name 
                       grid%em_lon_ur_d               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_UR_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LR_D'               , &  ! Data Name 
                       grid%em_lon_lr_d               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LON_LR_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RAINC'               , &  ! Data Name 
                       grid%rainc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RAINC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RAINNC'               , &  ! Data Name 
                       grid%rainnc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field RAINNC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOWNC'               , &  ! Data Name 
                       grid%snownc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SNOWNC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GRAUPELNC'               , &  ! Data Name 
                       grid%graupelnc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field GRAUPELNC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SWDOWN'               , &  ! Data Name 
                       grid%swdown               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SWDOWN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GLW'               , &  ! Data Name 
                       grid%glw               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field GLW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'OLR'               , &  ! Data Name 
                       grid%olr               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field OLR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLAT'               , &  ! Data Name 
                       grid%xlat               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field XLAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLONG'               , &  ! Data Name 
                       grid%xlong               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field XLONG memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLAT_U'               , &  ! Data Name 
                       grid%em_xlat_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field XLAT_U memorder XY' , & ! Debug message
ids , ide , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( ide, ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLONG_U'               , &  ! Data Name 
                       grid%em_xlong_u               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field XLONG_U memorder XY' , & ! Debug message
ids , ide , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( ide, ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLAT_V'               , &  ! Data Name 
                       grid%em_xlat_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field XLAT_V memorder XY' , & ! Debug message
ids , (ide-1) , jds , jde , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( jde, jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLONG_V'               , &  ! Data Name 
                       grid%em_xlong_v               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field XLONG_V memorder XY' , & ! Debug message
ids , (ide-1) , jds , jde , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( jde, jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBEDO'               , &  ! Data Name 
                       grid%albedo               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field ALBEDO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TMN'               , &  ! Data Name 
                       grid%tmn               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field TMN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLAND'               , &  ! Data Name 
                       grid%xland               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field XLAND memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'UST'               , &  ! Data Name 
                       grid%ust               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field UST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PBLH'               , &  ! Data Name 
                       grid%pblh               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field PBLH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HFX'               , &  ! Data Name 
                       grid%hfx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field HFX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QFX'               , &  ! Data Name 
                       grid%qfx               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field QFX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LH'               , &  ! Data Name 
                       grid%lh               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field LH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOWC'               , &  ! Data Name 
                       grid%snowc               , &  ! Field 
                       WRF_FLOAT             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_histin.inc ext_read_field SNOWC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
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
'inc/wrf_histin.inc ext_write_field '//TRIM(fdda3d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
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
'inc/wrf_histin.inc ext_write_field '//TRIM(fdda2d_dname_table( grid%id, itrace ))//' memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , 1 , jds , (jde-1) ,  & 
ims , ime , 1 , 1 , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , 1 , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
