SUBROUTINE wrf_histin ( fid , grid , config_flags , switch , ierr )
    USE module_domain
    USE module_state_description
    USE module_configure
    USE module_scalar_tables
    USE module_io
    USE module_io_wrf
    USE module_date_time
    USE module_bc_time_utilities
    USE module_utility
    IMPLICIT NONE
      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110


      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102
  



  integer, parameter  :: WRF_NO_ERR                  =  0       
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       
  integer, parameter  :: WRF_WARN_MD_NF              = -2       
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      
  integer, parameter  :: WRF_WARN_NOOP               = -23      


  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 







  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    
  integer, parameter  :: WRF_WARN_FILE_OPEN_FOR_READ = -1009
  integer, parameter  :: WRF_IO_NOT_INITIALIZED      = -1010
  integer, parameter  :: WRF_WARN_MD_AFTER_OPEN      = -1011
  integer, parameter  :: WRF_WARN_TOO_MANY_VARIABLES = -1012
  integer, parameter  :: WRF_WARN_DRYRUN_CLOSE       = -1013
  integer, parameter  :: WRF_WARN_DATESTR_BAD_LENGTH = -1014
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_READ   = -1015
  integer, parameter  :: WRF_WARN_DATA_TYPE_NOT_FOUND = -1016
  integer, parameter  :: WRF_WARN_DATESTR_ERROR      = -1017
  integer, parameter  :: WRF_WARN_DRYRUN_READ        = -1018
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_GET    = -1019
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_PUT    = -1020
  integer, parameter  :: WRF_WARN_NETCDF             = -1021    
  integer, parameter  :: WRF_WARN_LENGTH_LESS_THAN_1 = -1022    
  integer, parameter  :: WRF_WARN_MORE_DATA_IN_FILE  = -1023    
  integer, parameter  :: WRF_WARN_DATE_LT_LAST_DATE  = -1024


  integer, parameter  :: WRF_HDF5_ERR_FILE                 = -200
  integer, parameter  :: WRF_HDF5_ERR_MD                   = -201
  integer, parameter  :: WRF_HDF5_ERR_TIME                 = -202
  integer, parameter  :: WRF_HDF5_ERR_TIME_EOF             = -203
  integer, parameter  :: WRF_HDF5_ERR_MORE_DATA_IN_FILE    = -204
  integer, parameter  :: WRF_HDF5_ERR_DATE_LT_LAST_DATE    = -205
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_FILES       = -206
  integer, parameter  :: WRF_HDF5_ERR_TYPE_MISMATCH        = -207
  integer, parameter  :: WRF_HDF5_ERR_LENGTH_LESS_THAN_1   = -208
  integer, parameter  :: WRF_HDF5_ERR_WRITE_RONLY_FILE     = -209
  integer, parameter  :: WRF_HDF5_ERR_READ_WONLY_FILE      = -210
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_OPENED      = -211
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_ERROR        = -212
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_READ          = -213
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_GET      = -214
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_PUT      = -215
  integer, parameter  :: WRF_HDF5_ERR_2DRYRUNS_1VARIABLE   = -216
  integer, parameter  :: WRF_HDF5_ERR_DATA_TYPE_NOTFOUND   = -217
  integer, parameter  :: WRF_HDF5_ERR_READ_PAST_EOF        = -218
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_HANDLE      = -219
  integer, parameter  :: WRF_HDF5_ERR_WRTLEN_NE_DRRUNLEN   = -220
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_CLOSE         = -221
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_BAD_LENGTH   = -222
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_READ     = -223
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_DIMS        = -224
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_VARIABLES   = -225
  integer, parameter  :: WRF_HDF5_ERR_COUNT_TOO_LONG       = -226
  integer, parameter  :: WRF_HDF5_ERR_DIMENSION_ERROR      = -227
  integer, parameter  :: WRF_HDF5_ERR_BAD_MEMORYORDER      = -228
  integer, parameter  :: WRF_HDF5_ERR_DIMNAME_REDEFINED    = -229
  integer, parameter  :: WRF_HDF5_ERR_MD_AFTER_OPEN        = -230
  integer, parameter  :: WRF_HDF5_ERR_CHARSTR_GT_LENDATA   = -231
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_TYPE        = -232
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_COMMITTED   = -233

  integer, parameter  :: WRF_HDF5_ERR_ALLOCATION        = -2001
  integer, parameter  :: WRF_HDF5_ERR_DEALLOCATION      = -2002
  integer, parameter  :: WRF_HDF5_ERR_BAD_FILE_STATUS   = -2003
  integer, parameter  :: WRF_HDF5_ERR_BAD_VARIABLE_DIM  = -2004
  integer, parameter  :: WRF_HDF5_ERR_MDVAR_DIM_NOT_1D  = -2005
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_TIMES    = -2006
  integer, parameter ::  WRF_HDF5_ERR_DATA_ID_NOTFOUND  = -2007

  integer, parameter ::  WRF_HDF5_ERR_DATASPACE         = -300
  integer, parameter ::  WRF_HDF5_ERR_DATATYPE          = -301
  integer, parameter :: WRF_HDF5_ERR_PROPERTY_LIST      = -302

  integer, parameter :: WRF_HDF5_ERR_DATASET_CREATE     = -303
  integer, parameter :: WRF_HDF5_ERR_DATASET_READ       = -304
  integer, parameter :: WRF_HDF5_ERR_DATASET_WRITE      = -305
  integer, parameter :: WRF_HDF5_ERR_DATASET_OPEN       = -306
  integer, parameter :: WRF_HDF5_ERR_DATASET_GENERAL    = -307
  integer, parameter :: WRF_HDF5_ERR_GROUP              = -308

  integer, parameter :: WRF_HDF5_ERR_FILE_OPEN          = -309
  integer, parameter :: WRF_HDF5_ERR_FILE_CREATE        = -310
  integer, parameter :: WRF_HDF5_ERR_DATASET_CLOSE      = -311
  integer, parameter :: WRF_HDF5_ERR_FILE_CLOSE         = -312
  integer, parameter :: WRF_HDF5_ERR_CLOSE_GENERAL      = -313

  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CREATE   = -314
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_READ     = -315
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_WRITE    = -316
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OPEN     = -317
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_GENERAL  = -318
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CLOSE    = -319

  integer, parameter :: WRF_HDF5_ERR_OTHERS             = -320
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OTHERS   = -321

    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(INOUT)    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(IN) :: switch
    INTEGER, INTENT(INOUT) :: ierr

    
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    INTEGER       itrace,idim1,idim2,idim3,idim4,idim5,idim6,idim7
    INTEGER       iname(9)
    INTEGER       iordering(3)
    INTEGER       icurrent_date(24)
    INTEGER       i,j,k
    INTEGER       icnt
    INTEGER       ndim
    INTEGER       ilen
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*256 errmess
    CHARACTER*40            :: this_datestr, next_datestr
    CHARACTER*9   NAMESTR
    INTEGER       IBDY, NAMELEN
    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    CHARACTER*19  new_date
    CHARACTER*24  base_date
    INTEGER idt
    INTEGER :: ide_compare , jde_compare , kde_compare
    ierr = 0

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )








IF ( in_use_for_config(grid%id,'x_2') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TOYVAR'               , &  
                       grid%x_2               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XZY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TOYVAR memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'lu_index') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'LU_INDEX'               , &  
                       grid%lu_index               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field LU_INDEX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'hbm2') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HBM2'               , &  
                       grid%hbm2               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field HBM2 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sm') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SM'               , &  
                       grid%sm               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SM memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sice') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SICE'               , &  
                       grid%sice               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SICE memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'pd') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PD'               , &  
                       grid%pd               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field PD memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'fis') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'FIS'               , &  
                       grid%fis               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field FIS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'res') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RES'               , &  
                       grid%res               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RES memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'t') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T'               , &  
                       grid%t               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field T memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'q') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'Q'               , &  
                       grid%q               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field Q memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'u') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U'               , &  
                       grid%u               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field U memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'v') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V'               , &  
                       grid%v               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field V memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'dx_nmm') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DX_NMM'               , &  
                       grid%dx_nmm               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field DX_NMM memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'deta1') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DETA1'               , &  
                       grid%deta1               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'Z'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field DETA1 memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'aeta1') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AETA1'               , &  
                       grid%aeta1               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'Z'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field AETA1 memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'eta1') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ETA1'               , &  
                       grid%eta1               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'Z'               , &  
                       'Z'               , &  
'inc/wrf_histin.inc ext_read_field ETA1 memorder Z' , & 
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'deta2') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DETA2'               , &  
                       grid%deta2               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'Z'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field DETA2 memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'aeta2') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AETA2'               , &  
                       grid%aeta2               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'Z'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field AETA2 memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'eta2') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ETA2'               , &  
                       grid%eta2               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'Z'               , &  
                       'Z'               , &  
'inc/wrf_histin.inc ext_read_field ETA2 memorder Z' , & 
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'dy_nmm') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DY_NMM'               , &  
                       grid%dy_nmm               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field DY_NMM memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'dlmd') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DLMD'               , &  
                       grid%dlmd               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field DLMD memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'dphd') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DPHD'               , &  
                       grid%dphd               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field DPHD memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'pdtop') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PDTOP'               , &  
                       grid%pdtop               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field PDTOP memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'pt') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PT'               , &  
                       grid%pt               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field PT memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'pblh') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PBLH'               , &  
                       grid%pblh               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field PBLH memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'mixht') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MIXHT'               , &  
                       grid%mixht               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field MIXHT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'ustar') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'USTAR'               , &  
                       grid%ustar               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field USTAR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'z0') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'Z0'               , &  
                       grid%z0               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field Z0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'ths') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'THS'               , &  
                       grid%ths               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field THS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'qsh') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QS'               , &  
                       grid%qsh               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field QS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'twbs') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TWBS'               , &  
                       grid%twbs               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TWBS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'qwbs') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QWBS'               , &  
                       grid%qwbs               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field QWBS memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'prec') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PREC'               , &  
                       grid%prec               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field PREC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'aprec') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'APREC'               , &  
                       grid%aprec               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field APREC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'acprec') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACPREC'               , &  
                       grid%acprec               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ACPREC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'cuprec') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CUPREC'               , &  
                       grid%cuprec               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CUPREC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sno') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SNO'               , &  
                       grid%sno               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SNO memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'si') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SI'               , &  
                       grid%si               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SI memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'cldefi') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CLDEFI'               , &  
                       grid%cldefi               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CLDEFI memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'th10') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TH10'               , &  
                       grid%th10               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TH10 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'q10') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'Q10'               , &  
                       grid%q10               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field Q10 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'pshltr') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PSHLTR'               , &  
                       grid%pshltr               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field PSHLTR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'tshltr') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TSHLTR'               , &  
                       grid%tshltr               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TSHLTR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'qshltr') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QSHLTR'               , &  
                       grid%qshltr               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field QSHLTR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'q2') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'Q2'               , &  
                       grid%q2               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field Q2 memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'akhs_out') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AKHS_OUT'               , &  
                       grid%akhs_out               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field AKHS_OUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'akms_out') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AKMS_OUT'               , &  
                       grid%akms_out               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field AKMS_OUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'albase') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALBASE'               , &  
                       grid%albase               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ALBASE memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'albedo') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALBEDO'               , &  
                       grid%albedo               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ALBEDO memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'cnvbot') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CNVBOT'               , &  
                       grid%cnvbot               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CNVBOT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'cnvtop') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CNVTOP'               , &  
                       grid%cnvtop               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CNVTOP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'czen') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CZEN'               , &  
                       grid%czen               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CZEN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'czmean') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CZMEAN'               , &  
                       grid%czmean               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CZMEAN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'epsr') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'EPSR'               , &  
                       grid%epsr               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field EPSR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'glat') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GLAT'               , &  
                       grid%glat               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field GLAT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'glon') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GLON'               , &  
                       grid%glon               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field GLON memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'nmm_tsk') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TSK'               , &  
                       grid%nmm_tsk               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TSK memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'mxsnal') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MXSNAL'               , &  
                       grid%mxsnal               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field MXSNAL memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'radot') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RADOT'               , &  
                       grid%radot               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RADOT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sigt4') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SIGT4'               , &  
                       grid%sigt4               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SIGT4 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'tg') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TGROUND'               , &  
                       grid%tg               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TGROUND memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'cwm') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CWM'               , &  
                       grid%cwm               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CWM memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'f_ice') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'F_ICE'               , &  
                       grid%f_ice               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XZY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field F_ICE memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'f_rain') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'F_RAIN'               , &  
                       grid%f_rain               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XZY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field F_RAIN memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'f_rimef') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'F_RIMEF'               , &  
                       grid%f_rimef               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XZY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field F_RIMEF memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sr') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SR'               , &  
                       grid%sr               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'cfrach') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CFRACH'               , &  
                       grid%cfrach               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CFRACH memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'cfracl') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CFRACL'               , &  
                       grid%cfracl               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CFRACL memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'cfracm') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CFRACM'               , &  
                       grid%cfracm               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CFRACM memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'islope') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ISLOPE'               , &  
                       grid%islope               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ISLOPE memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'dzsoil') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'DZSOIL'               , &  
                       grid%dzsoil               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'Z'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field DZSOIL memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sldpth') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SLDPTH'               , &  
                       grid%sldpth               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'Z'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SLDPTH memorder Z' , & 
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'cmc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CMC'               , &  
                       grid%cmc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CMC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'grnflx') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GRNFLX'               , &  
                       grid%grnflx               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field GRNFLX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'pctsno') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PCTSNO'               , &  
                       grid%pctsno               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field PCTSNO memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'soiltb') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SOILTB'               , &  
                       grid%soiltb               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SOILTB memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'vegfrc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'VEGFRC'               , &  
                       grid%vegfrc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field VEGFRC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sh2o') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SH2O'               , &  
                       grid%sh2o               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XZY'               , &  
                       'Z'               , &  
'inc/wrf_histin.inc ext_read_field SH2O memorder XZY' , & 
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'smc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SMC'               , &  
                       grid%smc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XZY'               , &  
                       'Z'               , &  
'inc/wrf_histin.inc ext_read_field SMC memorder XZY' , & 
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'stc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'STC'               , &  
                       grid%stc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XZY'               , &  
                       'Z'               , &  
'inc/wrf_histin.inc ext_read_field STC memorder XZY' , & 
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'pint') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PINT'               , &  
                       grid%pint               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       'Z'               , &  
'inc/wrf_histin.inc ext_read_field PINT memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , kde ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( kde, kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'w') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'W'               , &  
                       grid%w               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       'Z'               , &  
'inc/wrf_histin.inc ext_read_field W memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , kde ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( kde, kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'acfrcv') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACFRCV'               , &  
                       grid%acfrcv               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ACFRCV memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'acfrst') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACFRST'               , &  
                       grid%acfrst               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ACFRST memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'ssroff') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SSROFF'               , &  
                       grid%ssroff               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SSROFF memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'bgroff') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'BGROFF'               , &  
                       grid%bgroff               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field BGROFF memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'rlwin') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RLWIN'               , &  
                       grid%rlwin               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RLWIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'rlwtoa') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RLWTOA'               , &  
                       grid%rlwtoa               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RLWTOA memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'alwin') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALWIN'               , &  
                       grid%alwin               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ALWIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'alwout') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALWOUT'               , &  
                       grid%alwout               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ALWOUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'alwtoa') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ALWTOA'               , &  
                       grid%alwtoa               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ALWTOA memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'rswin') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RSWIN'               , &  
                       grid%rswin               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RSWIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'rswinc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RSWINC'               , &  
                       grid%rswinc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RSWINC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'rswout') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RSWOUT'               , &  
                       grid%rswout               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RSWOUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'aswin') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ASWIN'               , &  
                       grid%aswin               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ASWIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'aswout') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ASWOUT'               , &  
                       grid%aswout               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ASWOUT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'aswtoa') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ASWTOA'               , &  
                       grid%aswtoa               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ASWTOA memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sfcshx') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCSHX'               , &  
                       grid%sfcshx               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SFCSHX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sfclhx') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCLHX'               , &  
                       grid%sfclhx               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SFCLHX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'subshx') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SUBSHX'               , &  
                       grid%subshx               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SUBSHX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'snopcx') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SNOPCX'               , &  
                       grid%snopcx               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SNOPCX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sfcuvx') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCUVX'               , &  
                       grid%sfcuvx               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SFCUVX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'potevp') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'POTEVP'               , &  
                       grid%potevp               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field POTEVP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'potflx') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'POTFLX'               , &  
                       grid%potflx               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field POTFLX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'tlmin') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TLMIN'               , &  
                       grid%tlmin               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TLMIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'tlmax') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TLMAX'               , &  
                       grid%tlmax               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TLMAX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'t02_min') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T02_MIN'               , &  
                       grid%t02_min               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field T02_MIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'t02_max') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'T02_MAX'               , &  
                       grid%t02_max               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field T02_MAX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'rh02_min') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RH02_MIN'               , &  
                       grid%rh02_min               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RH02_MIN memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'rh02_max') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RH02_MAX'               , &  
                       grid%rh02_max               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RH02_MAX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'rlwtt') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RLWTT'               , &  
                       grid%rlwtt               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RLWTT memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'rswtt') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'RSWTT'               , &  
                       grid%rswtt               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field RSWTT memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'train') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TRAIN'               , &  
                       grid%train               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XYZ'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TRAIN memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'ncfrcv') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NCFRCV'               , &  
                       grid%ncfrcv               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field NCFRCV memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'ncfrst') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NCFRST'               , &  
                       grid%ncfrst               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field NCFRST memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'nphs0') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NPHS0'               , &  
                       grid%nphs0               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field NPHS0 memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'nprec') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NPREC'               , &  
                       grid%nprec               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field NPREC memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'nclod') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NCLOD'               , &  
                       grid%nclod               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field NCLOD memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'nheat') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NHEAT'               , &  
                       grid%nheat               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field NHEAT memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'nrdlw') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NRDLW'               , &  
                       grid%nrdlw               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field NRDLW memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'nrdsw') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NRDSW'               , &  
                       grid%nrdsw               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field NRDSW memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'nsrfc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'NSRFC'               , &  
                       grid%nsrfc               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field NSRFC memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'avrain') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AVRAIN'               , &  
                       grid%avrain               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field AVRAIN memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'avcnvc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'AVCNVC'               , &  
                       grid%avcnvc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field AVCNVC memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'acutim') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACUTIM'               , &  
                       grid%acutim               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ACUTIM memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'ardlw') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ARDLW'               , &  
                       grid%ardlw               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ARDLW memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'ardsw') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ARDSW'               , &  
                       grid%ardsw               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ARDSW memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'asrfc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ASRFC'               , &  
                       grid%asrfc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ASRFC memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'aphtim') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'APHTIM'               , &  
                       grid%aphtim               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field APHTIM memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'max10mw') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAX10MW'               , &  
                       grid%max10mw               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field MAX10MW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'max10u') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAX10U'               , &  
                       grid%max10u               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field MAX10U memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'max10v') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAX10V'               , &  
                       grid%max10v               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field MAX10V memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'maxupdr') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAXUPDR'               , &  
                       grid%maxupdr               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field MAXUPDR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'maxdndr') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAXDNDR'               , &  
                       grid%maxdndr               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field MAXDNDR memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'maxhlcy') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAXHLCY'               , &  
                       grid%maxhlcy               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field MAXHLCY memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'maxdbz') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'MAXDBZ'               , &  
                       grid%maxdbz               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field MAXDBZ memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'landmask') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'LANDMASK'               , &  
                       grid%landmask               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field LANDMASK memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
DO itrace = PARAM_FIRST_SCALAR , num_moist
  IF (BTEST(moist_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(moist_dname_table( grid%id, itrace )), & 
          grid%moist(ims,jms,kms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          'XYZ'               , &  
          ''                , &  
'inc/wrf_histin.inc ext_write_field '//TRIM(moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_moist
  IF (BTEST(dfi_moist_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_moist_dname_table( grid%id, itrace )), & 
          grid%dfi_moist(ims,jms,kms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          'XYZ'               , &  
          ''                , &  
'inc/wrf_histin.inc ext_write_field '//TRIM(dfi_moist_dname_table( grid%id, itrace ))//' memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_scalar
  IF (BTEST(scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(scalar_dname_table( grid%id, itrace )), & 
          grid%scalar(ims,jms,kms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          'XYZ'               , &  
          ''                , &  
'inc/wrf_histin.inc ext_write_field '//TRIM(scalar_dname_table( grid%id, itrace ))//' memorder XYZ' , & 
ids , (ide-1) , jds , (jde-1) , kds , (kde-1) ,  & 
ims , ime , jms , jme , kms , kme ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , kps , MIN( (kde-1), kpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_dfi_scalar
  IF (BTEST(dfi_scalar_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(dfi_scalar_dname_table( grid%id, itrace )), & 
          grid%dfi_scalar(ims,kms,jms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          'XZY'               , &  
          ''                , &  
'inc/wrf_histin.inc ext_write_field '//TRIM(dfi_scalar_dname_table( grid%id, itrace ))//' memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR , num_chem
  IF (BTEST(chem_stream_table(grid%id, itrace ) , switch )) THEN
    CALL wrf_ext_read_field (  &
          fid                             , &  
          current_date(1:19)              , &  
          TRIM(chem_dname_table( grid%id, itrace )), & 
          grid%chem(ims,kms,jms,itrace)  , &  
                       WRF_FLOAT             , &  
          grid%communicator  , &  
          grid%iocommunicator  , &  
          grid%domdesc       , &  
          grid%bdy_mask       , &  
          'XZY'               , &  
          ''                , &  
'inc/wrf_histin.inc ext_write_field '//TRIM(chem_dname_table( grid%id, itrace ))//' memorder XZY' , & 
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                         ierr )
  ENDIF
ENDDO
IF ( in_use_for_config(grid%id,'psfc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'PSFC'               , &  
                       grid%psfc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field PSFC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'th2') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'TH2'               , &  
                       grid%th2               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field TH2 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'u10') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'U10'               , &  
                       grid%u10               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field U10 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'v10') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'V10'               , &  
                       grid%v10               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field V10 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'smstav') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SMSTAV'               , &  
                       grid%smstav               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SMSTAV memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'smstot') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SMSTOT'               , &  
                       grid%smstot               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SMSTOT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sfcrunoff') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFROFF'               , &  
                       grid%sfcrunoff               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SFROFF memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'udrunoff') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'UDROFF'               , &  
                       grid%udrunoff               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field UDROFF memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'ivgtyp') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'IVGTYP'               , &  
                       grid%ivgtyp               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field IVGTYP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'isltyp') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ISLTYP'               , &  
                       grid%isltyp               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ISLTYP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'vegfra') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'VEGFRA'               , &  
                       grid%vegfra               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field VEGFRA memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sfcevp') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCEVP'               , &  
                       grid%sfcevp               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SFCEVP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'grdflx') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'GRDFLX'               , &  
                       grid%grdflx               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field GRDFLX memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sfcexc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SFCEXC '               , &  
                       grid%sfcexc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SFCEXC  memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'acsnow') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACSNOW'               , &  
                       grid%acsnow               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ACSNOW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'acsnom') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ACSNOM'               , &  
                       grid%acsnom               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ACSNOM memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'snow') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SNOW'               , &  
                       grid%snow               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SNOW memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'canwat') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'CANWAT'               , &  
                       grid%canwat               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field CANWAT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'sst') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SST'               , &  
                       grid%sst               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SST memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'weasd') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'WEASD'               , &  
                       grid%weasd               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field WEASD memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'thz0') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'THZ0'               , &  
                       grid%thz0               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field THZ0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'qz0') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QZ0'               , &  
                       grid%qz0               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field QZ0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'uz0') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'UZ0'               , &  
                       grid%uz0               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field UZ0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'vz0') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'VZ0'               , &  
                       grid%vz0               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field VZ0 memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'qsfc') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'QSFC'               , &  
                       grid%qsfc               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field QSFC memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'htop') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HTOP'               , &  
                       grid%htop               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field HTOP memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'hbot') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'HBOT'               , &  
                       grid%hbot               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field HBOT memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'snowh') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'SNOWH'               , &  
                       grid%snowh               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       'XY'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field SNOWH memorder XY' , & 
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'itimestep') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'ITIMESTEP'               , &  
                       grid%itimestep               , &  
                       WRF_integer             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field ITIMESTEP memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( in_use_for_config(grid%id,'xtime') ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  
                       current_date(1:19) , &  
                       'XTIME'               , &  
                       grid%xtime               , &  
                       WRF_FLOAT             , &  
                       grid%communicator  , &  
                       grid%iocommunicator  , &  
                       grid%domdesc       , &  
                       grid%bdy_mask     , &  
                       '0'               , &  
                       ''               , &  
'inc/wrf_histin.inc ext_read_field XTIME memorder 0' , & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF


    RETURN
    END
